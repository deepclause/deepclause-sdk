/**
 * The main-thread side of L2: a `ProcessBackend` that routes to a process worker.
 *
 * It keeps a synchronous cache of the worker's process table (refreshed by
 * events and a short poll) so the status bar and `/ps` stay synchronous, and it
 * services the worker's capability requests (`need`) through the session's
 * `CapabilityBroker` — model, host tools, human input.
 */

import { Worker } from 'node:worker_threads';
import { existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

import type { LLMBackendRequest, LLMBackendResponse } from '../types.js';
import type { ProcessRequestPayload, ProcessRequestResult } from '../runner.js';
import type { ProcessBackend } from './process-backend.js';
import type { DMLSession } from './session.js';
import { pidKey, type ProcessEvent, type ProcessInfo, type Pid, type SpawnOptions } from './processes.js';
import type { MainToWorker, WorkerInitData, WorkerToMain } from './worker-protocol.js';

/** The slice of `worker_threads.Worker` this host uses (injectable for tests). */
export interface WorkerLike {
  postMessage(message: unknown): void;
  on(event: 'message', handler: (message: WorkerToMain) => void): unknown;
  on(event: 'error', handler: (error: Error) => void): unknown;
  on(event: 'exit', handler: (code: number) => void): unknown;
  terminate(): unknown;
}

export type WorkerFactory = (init: WorkerInitData) => WorkerLike;

export interface RemoteProcessHostOptions {
  onEvent?: (event: ProcessEvent) => void;
  workerFactory?: WorkerFactory;
  refreshMs?: number;
}

interface PendingRpc {
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
}

function defaultWorkerFactory(init: WorkerInitData): WorkerLike {
  const here = import.meta.url;
  const isTypeScript = /\.ts(\?|$)/.test(here) || here.includes('/src/');
  if (isTypeScript) {
    // Running from source: prefer the built worker, whose relative `.js`
    // imports resolve under plain Node. The TypeScript worker needs tsx inside
    // the worker, which is not always resolvable; fall back to it only if the
    // build is absent. Run `npm run build` after runtime changes.
    const built = new URL('../../dist/repl/process-worker.js', here);
    if (existsSync(fileURLToPath(built))) {
      return new Worker(built, { workerData: init }) as unknown as WorkerLike;
    }
    const url = new URL('./process-worker.ts', here);
    return new Worker(url, { workerData: init, execArgv: ['--import', 'tsx'] }) as unknown as WorkerLike;
  }
  const url = new URL('./process-worker.js', here);
  return new Worker(url, { workerData: init }) as unknown as WorkerLike;
}

export class RemoteProcessHost implements ProcessBackend {
  private readonly onEvent?: (event: ProcessEvent) => void;
  private readonly workerFactory: WorkerFactory;
  private readonly refreshMs: number;

  private worker?: WorkerLike;
  private ready = false;
  private readyPromise?: Promise<void>;
  private disposed = false;
  private requestCounter = 0;
  private readonly pending = new Map<number, PendingRpc>();
  private cache: ProcessInfo[] = [];
  private nameIndex = new Map<string, Pid>();
  private refreshTimer?: ReturnType<typeof setInterval>;
  /** Every harness snapshot sent to the worker, keyed by version (for inspection). */
  private readonly snapshotTexts = new Map<string, string>();
  private lastSnapshotVersion?: string;

  constructor(private readonly session: DMLSession, options: RemoteProcessHostOptions = {}) {
    this.onEvent = options.onEvent;
    this.workerFactory = options.workerFactory ?? defaultWorkerFactory;
    this.refreshMs = options.refreshMs ?? 400;
  }

  async start(): Promise<void> {
    if (this.worker) return;
    const snapshot = this.session.processSnapshotFor();
    this.snapshotTexts.set(snapshot.version, snapshot.text);
    this.lastSnapshotVersion = snapshot.version;
    const init: WorkerInitData = {
      workspacePath: this.session.workspacePathValue(),
      sessionId: `${this.session.getSessionId()}__w`,
      harnessText: snapshot.text,
      harnessVersion: snapshot.version,
      model: this.session.modelId(),
      provider: this.session.providerId(),
      temperature: this.session.temperatureValue(),
      maxTokens: this.session.maxTokensValue(),
      toolPolicy: this.session.toolPolicyValue(),
      tools: this.session.toolDescriptors(),
    };
    this.readyPromise = new Promise<void>((resolve, reject) => {
      this.onReady = resolve;
      this.onFatal = reject;
    });
    const worker = this.workerFactory(init);
    this.worker = worker;
    worker.on('message', (message) => this.handleMessage(message));
    worker.on('error', (error) => {
      this.onFatal?.(error);
      this.failAll(error);
    });
    worker.on('exit', () => {
      this.failAll(new Error('process worker exited'));
    });

    await this.readyPromise;
    this.refreshTimer = setInterval(() => void this.refresh(), this.refreshMs);
    await this.refresh();
  }

  private onReady?: () => void;
  private onFatal?: (error: Error) => void;

  /**
   * The harness update to attach to an outbound spawn/foreground request. The
   * worker's children must run the code the user committed, not the module the
   * worker happened to start with — otherwise a long-lived worker serves every
   * later spawn from a stale snapshot.
   */
  private snapshotUpdate(): { harnessText?: string; harnessVersion: string } {
    const snapshot = this.session.processSnapshotFor();
    this.snapshotTexts.set(snapshot.version, snapshot.text);
    if (snapshot.version === this.lastSnapshotVersion) {
      return { harnessVersion: snapshot.version };
    }
    this.lastSnapshotVersion = snapshot.version;
    return { harnessText: snapshot.text, harnessVersion: snapshot.version };
  }

  // ---- ProcessBackend -----------------------------------------------------

  async spawn(goal: string, options: SpawnOptions = {}): Promise<Pid> {
    const result = (await this.request({
      type: 'spawn',
      requestId: 0,
      goal,
      options,
      ...this.snapshotUpdate(),
    })) as {
      pid?: Pid;
      error?: string;
    };
    if (!result.pid) throw new Error(result.error ?? 'worker spawn failed');
    // Refresh before returning so `processes()` already includes the new
    // process (otherwise the /spawn confirmation reads "no subagents").
    await this.refresh();
    return result.pid;
  }

  send(to: Pid | string, messageTerm: unknown): void {
    this.post({ type: 'send', to, message: messageTerm });
  }

  async stop(target: Pid | string, reason: unknown = 'shutdown'): Promise<void> {
    this.post({ type: 'stop', target, reason });
    await delay(0);
    await this.refresh();
  }

  register(name: string, pid: Pid): void {
    this.post({ type: 'register', name, pid });
    this.nameIndex.set(name, pid);
  }

  whereis(name: string): Pid | undefined {
    if (this.nameIndex.has(name)) return this.nameIndex.get(name);
    return undefined;
  }

  info(pid: Pid): ProcessInfo | undefined {
    return this.cache.find((process) => pidKey(process.pid) === pidKey(pid));
  }

  processes(): ProcessInfo[] {
    return this.cache;
  }

  async callTerm(to: Pid | string, messageTerm: string, timeoutMs: number): Promise<string> {
    const result = (await this.request({ type: 'call', requestId: 0, to, message: messageTerm, timeoutMs })) as {
      value?: string;
      error?: string;
    };
    if (result.error) throw new Error(result.error);
    return result.value ?? '';
  }

  async foregroundHandle(request: ProcessRequestPayload, _signal?: AbortSignal): Promise<ProcessRequestResult> {
    const result = (await this.request({
      type: 'foreground',
      requestId: 0,
      request,
      ...this.snapshotUpdate(),
    })) as {
      ok: boolean;
      result?: ProcessRequestResult;
      error?: string;
    };
    if (!result.ok || !result.result) throw new Error(result.error ?? 'foreground request failed');
    return result.result;
  }

  /**
   * The harness source a process is running. The main keeps every snapshot it
   * sent (keyed by version), so this stays synchronous and needs no worker hop.
   */
  codeOf(pid: Pid | string): string | undefined {
    const version = this.info(pid as Pid)?.harnessVersion;
    return version ? this.snapshotTexts.get(version) : undefined;
  }

  async shutdown(): Promise<void> {
    if (this.disposed) return;
    if (this.refreshTimer) clearInterval(this.refreshTimer);
    // Post the dispose request before marking disposed, or `post` drops it.
    try {
      await this.request({ type: 'dispose', requestId: 0 });
    } catch {
      // best effort
    }
    this.disposed = true;
    await this.worker?.terminate();
  }

  // ---- worker message handling -------------------------------------------

  private handleMessage(message: WorkerToMain): void {
    switch (message.type) {
      case 'ready':
        this.ready = true;
        this.onReady?.();
        break;
      case 'fatal':
        this.onFatal?.(new Error(message.error));
        break;
      case 'disposed':
        this.resolveRpc(message.requestId, undefined);
        break;
      case 'event':
        this.onEvent?.(message.event);
        if (message.event.type === 'started' || message.event.type === 'exited') void this.refresh();
        break;
      case 'spawn_result':
        this.resolveRpc(message.requestId, message);
        break;
      case 'processes_result':
        this.cache = message.processes;
        this.rebuildNameIndex();
        this.resolveRpc(message.requestId, message);
        break;
      case 'foreground_result':
        this.resolveRpc(message.requestId, message);
        break;
      case 'call_result':
        this.resolveRpc(message.requestId, message);
        break;
      case 'need':
        void this.serviceNeed(message.requestId, message.kind, message.payload);
        break;
    }
  }

  private async serviceNeed(
    requestId: number,
    kind: 'model' | 'tool' | 'input',
    payload: unknown,
  ): Promise<void> {
    try {
      if (kind === 'model') {
        const data = payload as {
          messages: LLMBackendRequest['messages'];
          tools?: LLMBackendRequest['tools'];
          temperature?: number;
          maxTokens?: number;
          target?: LLMBackendRequest['target'];
        };
        const response: LLMBackendResponse = await this.session.completeModelRequest({
          messages: data.messages,
          tools: data.tools,
          temperature: data.temperature,
          maxTokens: data.maxTokens,
          target: data.target,
          onText: (chunk) => this.post({ type: 'stream', requestId, kind: 'text', chunk }),
          onReasoning: (chunk) => this.post({ type: 'stream', requestId, kind: 'reasoning', chunk }),
        });
        this.post({ type: 'resolve', requestId, ok: true, result: response });
        return;
      }
      if (kind === 'tool') {
        const data = payload as { name: string; args: Record<string, unknown> };
        const result = await this.session.runHostTool(data.name, data.args);
        this.post({ type: 'resolve', requestId, ok: true, result });
        return;
      }
      const data = payload as { prompt: string };
      const answer = await this.session.requestHumanInput(data.prompt);
      this.post({ type: 'resolve', requestId, ok: true, result: answer });
    } catch (error) {
      this.post({ type: 'resolve', requestId, ok: false, error: error instanceof Error ? error.message : String(error) });
    }
  }

  private request(message: MainToWorker): Promise<unknown> {
    const requestId = ++this.requestCounter;
    const withId = { ...message, requestId } as MainToWorker;
    return new Promise((resolve, reject) => {
      this.pending.set(requestId, { resolve, reject });
      this.post(withId);
    });
  }

  private resolveRpc(requestId: number, value: unknown): void {
    const rpc = this.pending.get(requestId);
    if (!rpc) return;
    this.pending.delete(requestId);
    rpc.resolve(value);
  }

  private async refresh(): Promise<void> {
    if (!this.ready || this.disposed) return;
    try {
      await this.request({ type: 'processes', requestId: 0 });
    } catch {
      // the poll is best-effort
    }
  }

  private rebuildNameIndex(): void {
    const next = new Map<string, Pid>();
    for (const process of this.cache) {
      if (process.state !== 'done' && process.state !== 'crashed' && process.state !== 'killed') {
        next.set(process.name, process.pid);
      }
    }
    // Preserve explicitly registered names.
    for (const [name, pid] of this.nameIndex) if (!next.has(name)) next.set(name, pid);
    this.nameIndex = next;
  }

  private post(message: MainToWorker): void {
    if (!this.worker || this.disposed) return;
    this.worker.postMessage(message);
  }

  private failAll(error: Error): void {
    for (const rpc of this.pending.values()) rpc.reject(error);
    this.pending.clear();
  }
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
