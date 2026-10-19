/**
 * Process worker (L2).
 *
 * Runs a session-scoped process host and its subagents in a dedicated worker
 * with its own `swipl-wasm` instance. All side effects (model, tools, human
 * input) are proxied to the session that owns the worker, so a worker never
 * holds credentials or policy (spec §2.3).
 *
 * The worker body is host-agnostic (`startProcessWorker`) so a browser embedding
 * can run it in a Web Worker; the Node entrypoint at the bottom drives it from
 * `worker_threads`.
 */

import { parentPort, workerData } from 'node:worker_threads';

import type { LLMBackend, LLMBackendRequest, LLMBackendResponse, ToolDefinition } from '../types.js';
import { DMLSession } from './session.js';
import type { ProcessEvent } from './processes.js';
import { WORKER_PROTOCOL_VERSION, type MainToWorker, type WorkerInitData } from './worker-protocol.js';

/** The minimal message channel a worker needs. */
export interface ProcessWorkerPort {
  postMessage(message: unknown): void;
  on(handler: (message: MainToWorker) => void): void;
}

interface PendingRpc {
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
  onText?: (chunk: string) => void;
  onReasoning?: (chunk: string) => void;
}

/** Options for the worker body. */
export interface ProcessWorkerOptions {
  /**
   * Tools implemented inside the worker (e.g. a worker-local Python host). A
   * local tool replaces the proxy for the same name, so its calls never cross
   * the worker boundary. The session's `toolPolicy` still applies because the
   * tool is registered on the worker's `DMLSession` and its children.
   */
  localTools?: Map<string, ToolDefinition>;
  /**
   * Awaited before a spawn/foreground request runs inside the worker. The
   * embedding uses it to pull the parent's latest workspace, so a child starts
   * from the tree as it is at spawn time.
   */
  beforeSpawn?: () => Promise<void>;
}

/**
 * Run the L2 process worker. Resolves when the worker is ready; the session then
 * lives until a `dispose` message arrives.
 */
export async function startProcessWorker(
  port: ProcessWorkerPort,
  init: WorkerInitData,
  options: ProcessWorkerOptions = {},
): Promise<void> {
  const pending = new Map<number, PendingRpc>();
  let requestCounter = 0;

  if (init.protocolVersion !== WORKER_PROTOCOL_VERSION) {
    // The worker bundle is built (`dist`), so a `src` edit without a rebuild runs
    // old code against a new main. Say so instead of failing somewhere unrelated
    // — an empty child session used to look like "spawn lost the harness".
    port.postMessage({
      type: 'fatal',
      error: `stale L2 process worker: protocol ${String(init.protocolVersion)} vs ${WORKER_PROTOCOL_VERSION} — run 'npm run build'`,
    });
    return;
  }

  const rpc = (
    kind: 'model' | 'tool' | 'input',
    payload: unknown,
    onText?: (chunk: string) => void,
    onReasoning?: (chunk: string) => void,
  ): Promise<unknown> => {
    const requestId = ++requestCounter;
    return new Promise((resolve, reject) => {
      pending.set(requestId, { resolve, reject, onText, onReasoning });
      port.postMessage({ type: 'need', requestId, kind, payload });
    });
  };

  /** A backend that forwards every model call to the owning session. */
  const proxyBackend = (): LLMBackend => ({
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      const payload = {
        messages: request.messages,
        tools: request.tools,
        temperature: request.temperature,
        maxTokens: request.maxTokens,
        target: request.target,
      };
      return rpc('model', payload, request.onText, request.onReasoning) as Promise<LLMBackendResponse>;
    },
  });

  /**
   * The worker's tool map: host tools are stubs that forward to the session,
   * unless the embedding supplied a local implementation for that name.
   */
  const proxyTools = (descriptors: WorkerInitData['tools']): Map<string, ToolDefinition> => {
    const tools = new Map<string, ToolDefinition>();
    for (const descriptor of descriptors) {
      if (options.localTools?.has(descriptor.name)) continue;
      tools.set(descriptor.name, {
        description: descriptor.description,
        parameters: descriptor.parameters as unknown as ToolDefinition['parameters'],
        execute: (args: Record<string, unknown>) => rpc('tool', { name: descriptor.name, args }),
      });
    }
    for (const [name, tool] of options.localTools ?? []) tools.set(name, tool);
    return tools;
  };

  const session = await DMLSession.open({
    sessionId: init.sessionId,
    workspacePath: init.workspacePath,
    execution: 'inline',
    runnerOptions: {
      model: init.model,
      provider: init.provider,
      temperature: init.temperature ?? 0.7,
      maxTokens: init.maxTokens ?? 65536,
      streaming: true,
      llmBackend: proxyBackend(),
    },
    tools: proxyTools(init.tools),
    toolPolicy: init.toolPolicy,
    autoLoadHarness: false,
    onInput: (prompt) => rpc('input', { prompt }) as Promise<string | undefined>,
  });
  await session.loadCode(init.harnessText);
  session.setProcessSnapshot(init.harnessText, init.harnessVersion);
  session.setProcessEventSink(createEventForwarder(port));

  port.on((message: MainToWorker) => {
    void handle(session, port, pending, message, options);
  });

  port.postMessage({ type: 'ready' });
}

/**
 * Adopt the harness snapshot the session pushed with a spawn/foreground request.
 * Without this a long-lived worker would snapshot every later child from the
 * module it started with, so committed edits would never reach new processes.
 */
function adoptSnapshot(
  session: DMLSession,
  message: { harnessText?: string; harnessVersion?: string },
): void {
  if (message.harnessText) {
    session.setProcessSnapshot(message.harnessText, message.harnessVersion);
  }
}

async function handle(
  session: DMLSession,
  port: ProcessWorkerPort,
  pending: Map<number, PendingRpc>,
  message: MainToWorker,
  options: ProcessWorkerOptions,
): Promise<void> {
  switch (message.type) {
    case 'spawn':
      try {
        await options.beforeSpawn?.();
        adoptSnapshot(session, message);
        const pid = await session.spawnProcess(message.goal, message.options);
        port.postMessage({ type: 'spawn_result', requestId: message.requestId, pid });
      } catch (error) {
        port.postMessage({ type: 'spawn_result', requestId: message.requestId, error: messageOf(error) });
      }
      break;

    case 'send':
      session.sendProcess(message.to, message.message as string);
      break;

    case 'stop':
      await session.stopProcess(message.target, message.reason);
      // Reply, so the caller can refresh `ps` knowing the state has changed: a
      // fire-and-forget stop reported `ok` while `/ps` still said "running".
      port.postMessage({ type: 'stop_result', requestId: message.requestId });
      break;

    case 'register':
      session.registerProcess(message.name, message.pid);
      break;

    case 'processes':
      port.postMessage({ type: 'processes_result', requestId: message.requestId, processes: session.processes() });
      break;

    case 'foreground':
      try {
        await options.beforeSpawn?.();
        adoptSnapshot(session, message);
        const result = await session.handleForeground(message.request);
        port.postMessage({ type: 'foreground_result', requestId: message.requestId, ok: true, result });
      } catch (error) {
        port.postMessage({ type: 'foreground_result', requestId: message.requestId, ok: false, error: messageOf(error) });
      }
      break;

    case 'call':
      try {
        const value = await session.callProcess(message.to, message.message, message.timeoutMs);
        port.postMessage({ type: 'call_result', requestId: message.requestId, value });
      } catch (error) {
        port.postMessage({ type: 'call_result', requestId: message.requestId, error: messageOf(error) });
      }
      break;

    case 'resolve': {
      const rpcState = pending.get(message.requestId);
      if (!rpcState) break;
      pending.delete(message.requestId);
      if (message.ok) rpcState.resolve(message.result);
      else rpcState.reject(new Error(message.error));
      break;
    }

    case 'stream': {
      const rpcState = pending.get(message.requestId);
      if (!rpcState) break;
      if (message.kind === 'text') rpcState.onText?.(message.chunk);
      else rpcState.onReasoning?.(message.chunk);
      break;
    }

    case 'dispose':
      await session.dispose();
      port.postMessage({ type: 'disposed', requestId: message.requestId });
      break;
  }
}

function messageOf(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

/**
 * Coalesce streamed `stream`/`reasoning` chunks before they cross the worker
 * boundary: a long model response would otherwise be hundreds of IPC messages.
 * A non-stream event flushes the pending buffers first, to keep rough order.
 */
function createEventForwarder(target: { postMessage(message: unknown): void }): (event: ProcessEvent) => void {
  const buffers = new Map<
    string,
    { event: ProcessEvent; text: string; done: boolean; timer: ReturnType<typeof setTimeout> | null }
  >();

  const flush = (key: string): void => {
    const buffer = buffers.get(key);
    if (!buffer) return;
    if (buffer.timer) clearTimeout(buffer.timer);
    buffers.delete(key);
    if (buffer.text.length === 0) return;
    target.postMessage({ type: 'event', event: { ...buffer.event, content: buffer.text, done: buffer.done } });
  };

  const flushAll = (): void => {
    for (const key of [...buffers.keys()]) flush(key);
  };

  return (event: ProcessEvent): void => {
    if (event.type === 'stream' || event.type === 'reasoning') {
      const key = `${event.pid.id}:${event.type}`;
      const chunk = event.content ?? '';
      const buffer = buffers.get(key);
      if (!buffer) {
        buffers.set(key, {
          event,
          text: chunk,
          done: event.done === true,
          timer: setTimeout(() => flush(key), 120),
        });
      } else {
        buffer.event = event;
        buffer.text += chunk;
        buffer.done = event.done === true;
      }
      const current = buffers.get(key);
      if (current && (current.done || current.text.length >= 500)) flush(key);
      return;
    }
    flushAll();
    target.postMessage({ type: 'event', event });
  };
}

// ---------------------------------------------------------------------------
// Node entrypoint (a real `worker_threads` worker). In the browser this module
// is imported for `startProcessWorker` only, and this block does not run.
// ---------------------------------------------------------------------------

const nodePort =
  typeof parentPort !== 'undefined' &&
  parentPort &&
  typeof (parentPort as unknown as { postMessage?: unknown }).postMessage === 'function'
    ? parentPort
    : null;

if (nodePort) {
  const adapter: ProcessWorkerPort = {
    postMessage: (message) => nodePort.postMessage(message),
    on: (handler) => nodePort.on('message', (message) => handler(message as MainToWorker)),
  };
  startProcessWorker(adapter, workerData as WorkerInitData).catch((error) =>
    adapter.postMessage({ type: 'fatal', error: messageOf(error) }),
  );
}
