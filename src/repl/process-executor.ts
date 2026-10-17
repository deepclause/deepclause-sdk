/**
 * The DML process executor: turns a spawned goal into a `ProcessBody`.
 *
 * Each process runs in a **private child session** carrying a snapshot of the
 * living module (invariant C), and resolves its primitives (spawn/send/receive/…)
 * through the shared `LocalProcessHost`. Goals and messages cross the boundary as
 * canonical Prolog term strings, so no variable or term is shared (invariant A).
 *
 * This is the L1 executor. At L2 the host is replaced by a worker-backed one and
 * this file's role moves into the worker; nothing above it changes.
 */

import type { DMLEvent } from '../types.js';
import type { ProcessRequestPayload, ProcessRequestResult } from '../runner.js';
import type { ProcessBackend } from './process-backend.js';
import type { DMLSession } from './session.js';
import {
  LocalProcessHost,
  parsePidTerm,
  pidKey,
  pidTermText,
  prologAtom,
  type Pid,
  type ProcessBody,
  type ProcessContext,
  type ProcessEvent,
  type ProcessInfo,
  type SpawnOptions,
} from './processes.js';

export interface DmlProcessExecutorOptions {
  onEvent?: (event: ProcessEvent) => void;
  maxProcesses?: number;
  maxMailbox?: number;
}

/** A minimal parser for the `spawn_opt/3` options dict (keys are lowercase atoms). */
function parseSpawnOptions(text: string | undefined): SpawnOptions {
  const options: SpawnOptions = {};
  if (!text) return options;
  const grab = (key: string): string | undefined => {
    const match = new RegExp(`(?:^|[,\\s{])${key}\\s*:\\s*([^,}]+)`).exec(text);
    return match?.[1]?.trim();
  };
  const name = grab('name');
  if (name && name !== '_') options.name = name.replace(/^'(.*)'$/s, '$1');
  const priority = grab('priority');
  if (priority === 'foreground' || priority === 'interactive' || priority === 'background') {
    options.priority = priority;
  }
  const timeout = grab('timeout');
  if (timeout && /^\d+$/.test(timeout)) options.timeoutMs = Number(timeout);
  const code = grab('code');
  if (code) options.codeVersion = code;
  return options;
}

/** Render a process's info as a Prolog dict for `process_info/2`. */
function infoTerm(info: ProcessInfo | undefined): string {
  if (!info) return 'info{state:dead}';
  const usage = info.usage;
  return 'info{'
    + `pid:${pidTermText(info.pid)},`
    + `name:${prologAtom(info.name)},`
    + `registered:[${(info.registeredAs ?? []).map((name) => prologAtom(name)).join(',')}],`
    + `state:${info.state},`
    + `priority:${info.priority},`
    + `mailbox:${info.mailboxDepth},`
    + `restarts:${info.restarts},`
    + `usage:usage{input:${usage.inputTokens},output:${usage.outputTokens},total:${usage.totalTokens}}`
    + '}';
}

/** Quote text as a Prolog double-quoted string. */
function prologString(text: string): string {
  return `"${text.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/\r?\n/g, '\\n')}"`;
}

/** A one-line, log-friendly rendering of a tool-call event. */
function formatToolEvent(event: DMLEvent): string {
  const name = event.toolName ?? 'tool';
  if (event.toolState === 'completed') {
    return `✓ ${name}${event.toolSummary ? ` → ${event.toolSummary}` : ''}`;
  }
  if (event.toolState === 'failed') {
    return `✗ ${name}${event.toolError ? ` → ${event.toolError}` : ''}`;
  }
  return `▸ ${name}`;
}

function delay(ms: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) {
      reject(new Error('aborted'));
      return;
    }
    const timer = setTimeout(resolve, ms);
    signal?.addEventListener('abort', () => {
      clearTimeout(timer);
      reject(new Error('aborted'));
    }, { once: true });
  });
}

export class DmlProcessExecutor implements ProcessBackend {
  readonly host: LocalProcessHost;
  private readonly pidTerms = new Map<string, Pid>();
  private readonly pendingReplies = new Map<string, (value: string) => void>();
  private readonly hostMailbox: string[] = [];
  private readonly hostWaiters: Array<(message: string | undefined) => void> = [];
  private replyCounter = 0;

  constructor(private readonly parent: DMLSession, options: DmlProcessExecutorOptions = {}) {
    this.host = new LocalProcessHost({
      sessionId: parent.getSessionId(),
      resolveBody: (goal) => this.resolveBody(goal),
      resolveSnapshot: () => parent.processSnapshotFor(),
      onEvent: options.onEvent,
      onHostMessage: (message) => {
        if (typeof message === 'string') this.handleHostMessage(message);
      },
      maxProcesses: options.maxProcesses,
      maxMailbox: options.maxMailbox,
    });
  }

  /** The body for a spawned goal: run it in a private child session. */
  resolveBody(goal: string): ProcessBody {
    return (ctx) => this.run(goal, ctx);
  }

  /** The harness source a process is running (its spawn-time snapshot). */
  codeOf(pid: Pid): string | undefined {
    return this.host.codeOf(pid);
  }

  /**
   * A binding for the foreground (non-process) run, using the synthetic host
   * pid. It lets a `/goal` or an authoring turn spawn, send, and call services.
   */
  foregroundBinding(): { pid: Pid; handle: (request: ProcessRequestPayload, signal?: AbortSignal) => Promise<ProcessRequestResult> } {
    return {
      pid: this.host.hostPid,
      handle: (request, signal) => this.handleRequest(this.host.hostPid, request, signal),
    };
  }

  private async run(goal: string, ctx: ProcessContext): Promise<unknown> {
    const { session: child, loadError } = await this.parent.createProcessSession();
    if (loadError) {
      // Surface a broken snapshot instead of letting every call fail with a
      // puzzling existence error.
      ctx.emit({ type: 'output', content: `harness snapshot failed to load: ${loadError}` });
    }
    const process = {
      pid: ctx.pid,
      handle: (request: ProcessRequestPayload, signal?: AbortSignal) =>
        this.handleRequest(ctx.pid, request, signal),
    };
    try {
      const result = await child.executeGoal(
        goal,
        1,
        (event) => this.forwardEvent(ctx, event),
        ctx.signal,
        'full',
        process,
      );
      if (result.error) throw new Error(result.error);
      const output = result.output.trim();
      if (output) return output;
      if (result.bindings.length > 0) {
        return result.bindings.map((binding) => `${binding.name} = ${binding.value}`).join(', ');
      }
      return result.solutions.length > 0 ? 'ok' : 'ok';
    } finally {
      await child.dispose();
    }
  }

  private forwardEvent(ctx: ProcessContext, event: DMLEvent): void {
    switch (event.type) {
      case 'stream':
        if (event.content) ctx.emit({ type: 'stream', content: event.content, done: event.done });
        break;
      case 'reasoning':
        if (event.content) ctx.emit({ type: 'reasoning', content: event.content, done: event.done });
        break;
      case 'tool_call':
        ctx.emit({ type: 'tool', content: formatToolEvent(event) });
        break;
      case 'output':
      case 'answer':
        if (event.content) ctx.emit({ type: 'output', content: event.content });
        break;
      default:
        break;
    }
  }

  /** Resolve one process primitive against the shared host (spec §4). */
  async handleRequest(
    pid: Pid,
    request: ProcessRequestPayload,
    signal?: AbortSignal,
  ): Promise<ProcessRequestResult> {
    switch (request.op) {
      case 'self':
        return { key: 'self', term: pidTermText(pid) };

      case 'spawn': {
        const options = parseSpawnOptions(request.options);
        const isHost = pidKey(pid) === pidKey(this.host.hostPid);
        const childPid = this.host.spawn(request.goal ?? 'true', isHost ? options : { ...options, parent: pid });
        this.pidTerms.set(pidTermText(childPid), childPid);
        return { key: 'spawn', term: `ok(${pidTermText(childPid)})` };
      }

      case 'send': {
        const target = this.resolveTarget(request.to, pid);
        this.host.send(target, request.message ?? 'true');
        return { key: 'send', term: 'ok' };
      }

      case 'receive': {
        const timeout = Number(request.timeout ?? 0);
        if (pidKey(pid) === pidKey(this.host.hostPid)) {
          const message = await this.hostReceive(timeout > 0 ? timeout : undefined);
          return message === undefined
            ? { key: 'receive', term: 'timeout' }
            : { key: 'receive', term: `ok(${message})` };
        }
        const result = await this.host.receive<string>(pid, timeout > 0 ? timeout : undefined);
        return result.ok
          ? { key: 'receive', term: `ok(${result.message})` }
          : { key: 'receive', term: 'timeout' };
      }

      case 'requeue': {
        if (pidKey(pid) === pidKey(this.host.hostPid)) {
          this.hostMailbox.unshift(String(request.message ?? 'true'));
        } else {
          this.host.requeue(pid, request.message ?? 'true');
        }
        return { key: 'requeue', term: 'ok' };
      }

      case 'wait': {
        const target = this.resolveTarget(request.pid, pid);
        const resolved = typeof target === 'string' ? this.host.whereis(target) : target;
        if (!resolved) return { key: 'wait', term: 'failed(noproc)' };
        const info = await this.host.wait(resolved);
        if (!info) return { key: 'wait', term: 'failed(noproc)' };
        if (info.state === 'done') {
          const text = typeof info.result === 'string'
            ? info.result
            : info.result === undefined ? '' : JSON.stringify(info.result);
          return { key: 'wait', term: `ok(${prologString(text)})` };
        }
        if (info.state === 'crashed') {
          return { key: 'wait', term: `failed(${prologString(info.error ?? 'crashed')})` };
        }
        return { key: 'wait', term: `killed(${prologString(String(info.exitReason ?? info.state))})` };
      }

      case 'exit': {
        // `exit/1` targets this process; `exit/2` targets another.
        const target = request.pid ? this.resolveTarget(request.pid, pid) : pid;
        const resolved = typeof target === 'string' ? this.host.whereis(target) : target;
        if (!resolved) return { key: 'exit', term: 'failure(noproc)' };
        this.host.stop(resolved, request.reason ?? 'exit');
        return { key: 'exit', term: 'ok' };
      }

      case 'process_info': {
        const target = this.resolveTarget(request.pid, pid);
        const resolved = typeof target === 'string' ? this.host.whereis(target) : target;
        return { key: 'process_info', term: infoTerm(resolved ? this.host.info(resolved) : undefined) };
      }

      case 'register': {
        const target = this.resolveTarget(request.pid, pid);
        const resolved = typeof target === 'string' ? this.host.whereis(target) : target;
        if (!resolved) throw new Error(`cannot resolve pid for register: ${request.pid ?? '(self)'}`);
        this.host.register(String(request.name ?? ''), resolved);
        return { key: 'register', term: 'ok' };
      }

      case 'whereis': {
        const found = this.host.whereis(String(request.name ?? ''));
        return { key: 'whereis', term: found ? `ok(${pidTermText(found)})` : 'none' };
      }

      case 'sleep': {
        await delay(Math.max(0, Number(request.ms ?? 0)), signal);
        return { key: 'sleep', term: 'ok' };
      }
    }
  }

  /** Resolve a pid term, a registered name, or fall back to `fallback`. */
  private resolveTarget(text: string | undefined, fallback: Pid): Pid | string {
    if (!text) return fallback;
    const trimmed = text.trim();
    const mapped = this.pidTerms.get(trimmed);
    if (mapped) return mapped;
    const parsed = parsePidTerm(trimmed);
    if (parsed) return parsed;
    // A registered name arrives as a (possibly quoted) atom.
    return trimmed.replace(/^'(.*)'$/s, '$1');
  }

  /**
   * Host-side request/reply used by `/call` and the `call` tool. Sends
   * `request(HostPid, Ref, Message)` and waits for `reply(Ref, Value)`.
   */
  async callTerm(to: Pid | string, messageTerm: string, timeoutMs: number, signal?: AbortSignal): Promise<string> {
    const ref = `hc${++this.replyCounter}`;
    const request = `request(${pidTermText(this.host.hostPid)},${ref},${messageTerm})`;
    return new Promise<string>((resolve, reject) => {
      let settled = false;
      let timer: ReturnType<typeof setTimeout> | null = null;
      const onAbort = (): void => finish(() => reject(new Error('call aborted')));
      const finish = (settle: () => void): void => {
        if (settled) return;
        settled = true;
        if (timer) clearTimeout(timer);
        this.pendingReplies.delete(ref);
        signal?.removeEventListener('abort', onAbort);
        settle();
      };
      if (signal?.aborted) {
        reject(new Error('call aborted'));
        return;
      }
      timer = setTimeout(() => {
        finish(() => reject(new Error(`call timed out after ${timeoutMs} ms`)));
      }, timeoutMs);
      this.pendingReplies.set(ref, (value) => finish(() => resolve(value)));
      signal?.addEventListener('abort', onAbort, { once: true });
      let watched: Pid | undefined;
      try {
        const resolved = this.resolveTarget(typeof to === 'string' ? to : pidTermText(to), this.host.hostPid);
        this.host.send(resolved, request);
        // Only a pid or registered name can be watched by `wait`; a bare label sends
        // fine but has no record to watch, so the exit check is best-effort.
        watched = typeof resolved === 'string' ? this.host.whereis(resolved) : resolved;
      } catch (error) {
        finish(() => reject(error));
        return;
      }
      if (watched) {
        void this.host.wait(watched).then((info) => {
          finish(() => reject(new Error(`service ${pidTermText(watched)} exited (${info?.state ?? 'gone'}) before replying`)));
        }).catch(() => {});
      }
    });
  }

  /** Pop a message addressed to the synthetic host pid, or wait. */
  private hostReceive(timeoutMs?: number): Promise<string | undefined> {
    if (this.hostMailbox.length > 0) return Promise.resolve(this.hostMailbox.shift());
    return new Promise<string | undefined>((resolve) => {
      let timer: ReturnType<typeof setTimeout> | undefined;
      const waiter = (message: string | undefined): void => {
        if (timer) clearTimeout(timer);
        resolve(message);
      };
      if (timeoutMs && timeoutMs > 0) {
        timer = setTimeout(() => {
          const index = this.hostWaiters.indexOf(waiter);
          if (index !== -1) this.hostWaiters.splice(index, 1);
          resolve(undefined);
        }, timeoutMs);
      }
      this.hostWaiters.push(waiter);
    });
  }

  /** Resolve a `reply(Ref, Value)` addressed to the host, or queue the message. */
  private handleHostMessage(message: string): void {
    const match = /^reply\(\s*([^,]+?)\s*,\s*([\s\S]*)\)$/.exec(message.trim());
    if (match) {
      const resolver = this.pendingReplies.get(match[1]);
      if (resolver) {
        this.pendingReplies.delete(match[1]);
        resolver(match[2].trim());
        return;
      }
    }
    const waiter = this.hostWaiters.shift();
    if (waiter) waiter(message);
    else this.hostMailbox.push(message);
  }

  // ---- ProcessBackend -----------------------------------------------------

  spawn(goal: string, options: SpawnOptions = {}): Promise<Pid> {
    return Promise.resolve(this.host.spawn(goal, options));
  }

  send(to: Pid | string, messageTerm: unknown): void {
    this.host.send(to, messageTerm);
  }

  async stop(target: Pid | string, reason: unknown = 'shutdown'): Promise<void> {
    const pid = typeof target === 'string' ? this.host.whereis(target) : target;
    if (pid) this.host.stop(pid, reason);
  }

  whereis(name: string): Pid | undefined {
    return this.host.whereis(name);
  }

  info(pid: Pid): ProcessInfo | undefined {
    return this.host.info(pid);
  }

  processes(): ProcessInfo[] {
    return this.host.processes();
  }

  register(name: string, pid: Pid): void {
    this.host.register(name, pid);
  }

  foregroundHandle(request: ProcessRequestPayload, signal?: AbortSignal): Promise<ProcessRequestResult> {
    return this.handleRequest(this.host.hostPid, request, signal);
  }

  shutdown(): Promise<void> {
    return this.host.shutdown();
  }
}
