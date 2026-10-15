/**
 * Session-scoped processes for the REPL (L1 core).
 *
 * This module is the host bookkeeping and the L1↔L2 seam from
 * `docs/DML_REPL_PROCESSES_SPEC.md` §2. It owns process identity, mailboxes,
 * links, monitors, naming, limits, and lifecycle events. It does **not** drive
 * Prolog; a `ProcessBody` is an opaque async function, and the DML layer supplies
 * bodies that step a runner frame.
 *
 * Keeping the bodies opaque is what makes L2 a mode switch: a worker-backed host
 * implements the same interface with RPC bodies and node-qualified pids, and
 * nothing above this module changes.
 *
 * Invariants enforced here (spec §6.2):
 *  - pids are node-qualified (`node: 'main'` in v1);
 *  - messages are copied (value semantics), never shared by reference;
 *  - process info and events are plain data;
 *  - there is no UI dependency.
 */

import type { LLMUsage } from '../types.js';

// ---------------------------------------------------------------------------
// Identity
// ---------------------------------------------------------------------------

export type NodeId = string;

/** A node-qualified, location-transparent process identifier. */
export interface Pid {
  node: NodeId;
  session: string;
  id: string;
}

export function pidKey(pid: Pid): string {
  return `${pid.node}:${pid.session}:${pid.id}`;
}

/** Quote a string as a Prolog atom when it is not a plain lowercase atom. */
export function prologAtom(text: string): string {
  return /^[a-z][A-Za-z0-9_]*$/.test(text) ? text : `'${text.replace(/'/g, "''")}'`;
}

/** The canonical Prolog term for a pid: `proc(Node,Session,Id)`. */
export function pidTermText(pid: Pid): string {
  return `proc(${prologAtom(pid.node)},${prologAtom(pid.session)},${prologAtom(pid.id)})`;
}

/** Parse `proc(Node,Session,Id)` back to a pid, or undefined. */
export function parsePidTerm(text: string): Pid | undefined {
  const match = /^proc\(([^,]+),([^,]+),([^)]+)\)$/.exec(text.trim());
  if (!match) return undefined;
  const unquote = (part: string): string => {
    const trimmed = part.trim();
    const quoted = /^'(.*)'$/s.exec(trimmed);
    return quoted ? quoted[1].replace(/''/g, "'") : trimmed;
  };
  return { node: unquote(match[1]), session: unquote(match[2]), id: unquote(match[3]) };
}

export type ProcessPriority = 'foreground' | 'interactive' | 'background';

export type ProcessState =
  | 'runnable'
  | 'running'
  | 'waiting'
  | 'input'
  | 'done'
  | 'crashed'
  | 'killed';

export interface ProcessInfo {
  pid: Pid;
  name: string;
  goal: string;
  state: ProcessState;
  priority: ProcessPriority;
  mailboxDepth: number;
  links: Pid[];
  monitors: Pid[];
  usage: LLMUsage;
  startedAt: number;
  finishedAt?: number;
  restarts: number;
  currentActivity?: string;
  parent?: Pid;
  codeVersion?: string;
  result?: unknown;
  error?: string;
  exitReason?: unknown;
}

export interface SpawnOptions {
  /** Human/model-readable label. Defaults to the goal's predicate name. */
  name?: string;
  priority?: ProcessPriority;
  memory?: 'fresh' | 'parent';
  /** Wall-clock budget in ms, enforced by the body's runtime. */
  timeoutMs?: number;
  /** Inference budget, enforced by the body's runtime. */
  maxGas?: number;
  /** Module version snapshot the process runs. */
  codeVersion?: string;
  /** Link at spawn time (exit propagation). */
  link?: Pid;
  parent?: Pid;
}

export type ReceiveResult<T = unknown> =
  | { ok: true; message: T }
  | { ok: false; reason: 'timeout' | 'stopped' };

/** Thrown by `ProcessContext.exit` to terminate the current process. */
export class ProcessExit extends Error {
  constructor(readonly reason: unknown) {
    super(typeof reason === 'string' ? reason : 'process exited');
    this.name = 'ProcessExit';
  }
}

/** Raised when sending to a process that is gone. */
export class NoProcError extends Error {
  readonly code = 'noproc';
  constructor(readonly pid: Pid) {
    super(`no process: ${pidKey(pid)}`);
    this.name = 'NoProcError';
  }
}

/** Raised when a mailbox is full (spec §13.3). */
export class MailboxFullError extends Error {
  readonly code = 'mailbox_full';
  constructor(readonly pid: Pid) {
    super(`mailbox full: ${pidKey(pid)}`);
    this.name = 'MailboxFullError';
  }
}

// ---------------------------------------------------------------------------
// Events
// ---------------------------------------------------------------------------

/** Plain-data lifecycle event for the service sink (spec §8.4). */
export interface ProcessEvent {
  type: 'started' | 'state' | 'activity' | 'output' | 'answer' | 'exited'
    | 'stream' | 'reasoning' | 'tool' | 'model';
  at: number;
  pid: Pid;
  name: string;
  state?: ProcessState;
  activity?: string;
  content?: string;
  /** For 'stream'/'reasoning': the final chunk of a run. */
  done?: boolean;
  answer?: unknown;
  reason?: unknown;
  error?: string;
}

/** One line in the subagent log view (`/ps --watch --logs`). */
export interface ProcessLogEntry {
  at: number;
  pid: Pid;
  name: string;
  kind: ProcessEvent['type'];
  text: string;
  /** True once a streamed entry is complete (stops coalescing). */
  done?: boolean;
}

// ---------------------------------------------------------------------------
// Bodies and context
// ---------------------------------------------------------------------------

export interface ProcessContext {
  readonly pid: Pid;
  readonly goal: string;
  readonly options: SpawnOptions;
  readonly signal: AbortSignal;

  /** Pop the oldest message, or wait for one (spec §4.4). */
  receive<T = unknown>(timeoutMs?: number): Promise<ReceiveResult<T>>;
  /** Copy a message into another process's mailbox. */
  send(to: Pid | string, message: unknown): void;
  /** Start a child process running `goal`. */
  spawn(goal: string, options?: SpawnOptions): Pid;
  /** Terminate this process with `reason` (never returns). */
  exit(reason?: unknown): never;
  /** Snapshot this process's info. */
  info(): ProcessInfo;
  /** Update the activity string shown in the process panel. */
  setActivity(activity?: string): void;
  /** Emit a plain-data event to the service sink. */
  emit(event: Omit<ProcessEvent, 'at' | 'pid' | 'name'>): void;
  /** Record model usage against this process. */
  recordUsage(usage: LLMUsage): void;
}

/** An opaque, async unit of work. The DML layer supplies bodies that step a runner. */
export type ProcessBody = (ctx: ProcessContext) => Promise<unknown>;

// ---------------------------------------------------------------------------
// Host
// ---------------------------------------------------------------------------

export interface ProcessHostOptions {
  sessionId: string;
  node?: NodeId;
  /** Resolve a spawned goal to a body. */
  resolveBody?: (goal: string) => ProcessBody | undefined;
  onEvent?: (event: ProcessEvent) => void;
  /** Called when a message is addressed to the synthetic host pid. */
  onHostMessage?: (message: unknown) => void;
  maxProcesses?: number;
  maxMailbox?: number;
  /** Terminal process records kept for `ps`/`wait` before the oldest are reaped. */
  maxTerminalRecords?: number;
  now?: () => number;
}

interface Waiter {
  resolve: (result: ReceiveResult) => void;
  timer?: ReturnType<typeof setTimeout>;
}

interface Watch {
  ref: string;
  watcher: string;
  resolve: (reason: unknown) => void;
}

interface ProcRecord {
  pid: Pid;
  name: string;
  goal: string;
  options: SpawnOptions;
  body?: ProcessBody;
  state: ProcessState;
  priority: ProcessPriority;
  mailbox: unknown[];
  waiters: Waiter[];
  links: Set<string>;
  /** refs this process owns when monitoring others */
  watching: Set<string>;
  /** monitors on this process */
  watchedBy: Map<string, Watch>;
  usage: LLMUsage;
  startedAt: number;
  finishedAt?: number;
  restarts: number;
  currentActivity?: string;
  controller: AbortController;
  result?: unknown;
  error?: string;
  exitReason?: unknown;
  terminal: boolean;
  terminalWaiters: Array<() => void>;
}

function emptyUsage(): LLMUsage {
  return { inputTokens: 0, outputTokens: 0, totalTokens: 0 };
}

function addUsage(a: LLMUsage, b: LLMUsage): LLMUsage {
  return {
    inputTokens: a.inputTokens + b.inputTokens,
    outputTokens: a.outputTokens + b.outputTokens,
    totalTokens: a.totalTokens + b.totalTokens,
    cacheReadTokens: (a.cacheReadTokens ?? 0) + (b.cacheReadTokens ?? 0) || undefined,
    cacheWriteTokens: (a.cacheWriteTokens ?? 0) + (b.cacheWriteTokens ?? 0) || undefined,
    reasoningTokens: (a.reasoningTokens ?? 0) + (b.reasoningTokens ?? 0) || undefined,
  };
}

/** Default label for a goal: the predicate's functor, or the first token. */
export function labelForGoal(goal: string): string {
  const match = /^\s*([a-z_][A-Za-z0-9_]*)/.exec(goal);
  return match ? match[1] : 'process';
}

/**
 * Copy a message so processes never share mutable terms (spec §4.2 / invariant A).
 *
 * `structuredClone` is the right tool for plain data. Values it cannot clone
 * (functions, symbols) are rejected, which is the intended behavior: such a value
 * could not cross an L2 worker boundary either.
 */
export function copyMessage<T>(value: T): T {
  if (value === null || typeof value !== 'object') return value;
  try {
    return structuredClone(value);
  } catch {
    throw new Error('message is not portable between processes (must be plain data)');
  }
}

/**
 * The L1 process host: a process table, mailboxes, links, monitors, and naming,
 * all in one JS realm.
 *
 * The DML runner drives process progress; this host owns identity and messaging.
 */
export class LocalProcessHost {
  private readonly sessionId: string;
  private readonly node: NodeId;
  private readonly resolveBody?: (goal: string) => ProcessBody | undefined;
  private readonly onEvent?: (event: ProcessEvent) => void;
  private readonly onHostMessage?: (message: unknown) => void;
  private readonly maxProcesses: number;
  private readonly maxMailbox: number;
  private readonly maxTerminalRecords: number;
  private readonly now: () => number;

  private readonly records = new Map<string, ProcRecord>();
  private readonly names = new Map<string, Pid>();
  private readonly watchersByRef = new Map<string, { watcher: string; resolve: (reason: unknown) => void }>();
  private readonly replyInboxes = new Map<string, (value: unknown) => void>();
  private counter = 0;
  private refCounter = 0;
  private shutdownRequested = false;

  /** Synthetic pid representing the host itself, for `call`/`request` replies. */
  readonly hostPid: Pid;

  constructor(options: ProcessHostOptions) {
    this.sessionId = options.sessionId;
    this.node = options.node ?? 'main';
    this.resolveBody = options.resolveBody;
    this.onEvent = options.onEvent;
    this.onHostMessage = options.onHostMessage;
    this.maxProcesses = options.maxProcesses ?? 16;
    this.maxMailbox = options.maxMailbox ?? 1000;
    this.maxTerminalRecords = options.maxTerminalRecords ?? 50;
    this.now = options.now ?? Date.now;
    this.hostPid = { node: this.node, session: this.sessionId, id: 'host' };
  }

  // ---- spawning -----------------------------------------------------------

  spawn(goal: string, options: SpawnOptions = {}, body?: ProcessBody): Pid {
    if (this.shutdownRequested) throw new Error('process host is shut down');
    // The limit bounds *live* processes, not cumulative spawns: a finished
    // process must not consume a slot forever (a session that spawned 16 things
    // could otherwise never fan out again).
    if (this.liveCount() >= this.maxProcesses) {
      throw new Error(`process limit reached (${this.maxProcesses} running); stop a process first`);
    }
    this.reapTerminalRecords();
    const resolved = body ?? this.resolveBody?.(goal);
    if (!resolved) throw new Error(`no body registered for goal: ${goal}`);

    const pid: Pid = { node: this.node, session: this.sessionId, id: `p${++this.counter}` };
    const record: ProcRecord = {
      pid,
      name: options.name ?? labelForGoal(goal),
      goal,
      options,
      body: resolved,
      state: 'runnable',
      priority: options.priority ?? 'background',
      mailbox: [],
      waiters: [],
      links: new Set(),
      watching: new Set(),
      watchedBy: new Map(),
      usage: emptyUsage(),
      startedAt: this.now(),
      restarts: 0,
      controller: new AbortController(),
      terminal: false,
      terminalWaiters: [],
    };
    this.records.set(pidKey(pid), record);
    if (options.parent) this.link(options.parent, pid);
    if (options.link) this.link(options.link, pid);

    this.emit({ type: 'started', pid, name: record.name, state: 'runnable' });
    // Start on the next microtask so `spawn` behaves as fire-and-forget.
    void Promise.resolve().then(() => this.run(record));
    return pid;
  }

  private async run(record: ProcRecord): Promise<void> {    if (record.terminal) return;
    record.state = 'running';
    this.emit({ type: 'state', pid: record.pid, name: record.name, state: 'running' });
    try {
      const result = await record.body!(this.contextFor(record));
      this.finish(record, 'done', 'normal', result);
    } catch (error) {
      if (error instanceof ProcessExit) {
        this.finish(record, 'killed', error.reason);
      } else {
        const message = error instanceof Error ? error.message : String(error);
        this.finish(record, 'crashed', { error: message }, undefined, message);
      }
    }
  }

  private finish(
    record: ProcRecord,
    state: ProcessState,
    reason: unknown,
    result?: unknown,
    error?: string,
  ): void {
    if (record.terminal) return;
    record.terminal = true;
    record.state = state;
    record.finishedAt = this.now();
    record.result = result;
    record.error = error;
    record.exitReason = reason;

    // Wake anyone blocked on receive.
    for (const waiter of record.waiters.splice(0)) {
      if (waiter.timer) clearTimeout(waiter.timer);
      waiter.resolve({ ok: false, reason: 'stopped' });
    }
    this.names.forEach((pid, name) => {
      if (pidKey(pid) === pidKey(record.pid)) this.names.delete(name);
    });

    // Monitors.
    for (const watch of record.watchedBy.values()) {
      this.watchersByRef.delete(watch.ref);
      const watcher = this.records.get(watch.watcher);
      watcher?.watching.delete(watch.ref);
      watch.resolve(reason);
    }
    record.watchedBy.clear();

    for (const waiter of record.terminalWaiters.splice(0)) waiter();

    if (state === 'done' && result !== undefined) {
      this.emit({ type: 'answer', pid: record.pid, name: record.name, answer: result });
    }
    this.emit({
      type: 'exited',
      pid: record.pid,
      name: record.name,
      state,
      reason,
      error,
    });

    // Links: an abnormal exit propagates as a stop to linked processes.
    if (state !== 'done' || reason !== 'normal') {
      for (const linkKey of record.links) {
        const linked = this.records.get(linkKey);
        if (linked && !linked.terminal) this.stop(linked.pid, reason);
      }
    }
  }

  // ---- context ------------------------------------------------------------

  private contextFor(record: ProcRecord): ProcessContext {
    const host = this;
    return {
      pid: record.pid,
      goal: record.goal,
      options: record.options,
      signal: record.controller.signal,
      receive<T>(timeoutMs?: number): Promise<ReceiveResult<T>> {
        return host.receive<T>(record.pid, timeoutMs);
      },
      send(to, message): void {
        host.send(to, message);
      },
      spawn(goal, options): Pid {
        return host.spawn(goal, { ...options, parent: record.pid });
      },
      exit(reason?: unknown): never {
        throw new ProcessExit(reason);
      },
      info(): ProcessInfo {
        return host.info(record.pid)!;
      },
      setActivity(activity?: string): void {
        record.currentActivity = activity;
        host.emit({ type: 'activity', pid: record.pid, name: record.name, activity });
      },
      emit(event): void {
        host.emit({ ...event, pid: record.pid, name: record.name });
      },
      recordUsage(usage): void {
        record.usage = addUsage(record.usage, usage);
      },
    };
  }

  // ---- messaging ----------------------------------------------------------

  send(to: Pid | string, message: unknown): void {
    const pid = typeof to === 'string' ? (this.names.get(to) ?? this.pidByLabel(to)) : to;
    if (!pid) throw new NoProcError(typeof to === 'string' ? { node: this.node, session: this.sessionId, id: to } : to);

    // The host is a synthetic endpoint for request/reply.
    if (pidKey(pid) === pidKey(this.hostPid)) {
      const copied = copyMessage(message);
      this.onHostMessage?.(copied);
      const ref = typeof copied === 'object' && copied !== null && 'ref' in copied
        ? String((copied as { ref: unknown }).ref)
        : undefined;
      const kind = typeof copied === 'object' && copied !== null && 'kind' in copied
        ? String((copied as { kind: unknown }).kind)
        : undefined;
      if (kind === 'reply' && ref) {
        const resolver = this.replyInboxes.get(ref);
        if (resolver) {
          this.replyInboxes.delete(ref);
          resolver((copied as { value: unknown }).value);
        }
        return;
      }
      return;
    }

    const record = this.records.get(pidKey(pid));
    if (!record || record.terminal) throw new NoProcError(pid);
    if (record.mailbox.length >= this.maxMailbox) throw new MailboxFullError(pid);

    const copied = copyMessage(message);
    const waiter = record.waiters.shift();
    if (waiter) {
      if (waiter.timer) clearTimeout(waiter.timer);
      waiter.resolve({ ok: true, message: copied });
    } else {
      record.mailbox.push(copied);
    }
  }

  /**
   * Pop the oldest message, or wait. `timeoutMs` of 0 or undefined waits forever
   * (spec §4.4); the DML layer chooses a default.
   */
  receive<T = unknown>(pid: Pid, timeoutMs?: number): Promise<ReceiveResult<T>> {
    const record = this.records.get(pidKey(pid));
    if (!record || record.terminal) return Promise.resolve({ ok: false, reason: 'stopped' });

    if (record.mailbox.length > 0) {
      return Promise.resolve({ ok: true, message: record.mailbox.shift() as T });
    }
    return new Promise<ReceiveResult<T>>((resolve) => {
      const waiter: Waiter = { resolve: (result) => resolve(result as ReceiveResult<T>) };
      if (timeoutMs && timeoutMs > 0) {
        waiter.timer = setTimeout(() => {
          const index = record.waiters.indexOf(waiter);
          if (index !== -1) record.waiters.splice(index, 1);
          resolve({ ok: false, reason: 'timeout' });
        }, timeoutMs);
      }
      record.waiters.push(waiter);
    });
  }

  /**
   * Put a message back at the front of the mailbox (selective receive).
   */
  requeue(pid: Pid, message: unknown): void {
    const record = this.records.get(pidKey(pid));
    if (!record || record.terminal) return;
    record.mailbox.unshift(copyMessage(message));
  }

  /**
   * Send `{kind:'request', from, ref, value: message}` and await a
   * `{kind:'reply', ref, value}` from the target (the `library(service)`
   * protocol).
   */
  call(to: Pid | string, message: unknown, timeoutMs = 300_000): Promise<unknown> {
    const ref = `r${++this.refCounter}`;
    return new Promise<unknown>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.replyInboxes.delete(ref);
        reject(new Error(`call timed out after ${timeoutMs} ms`));
      }, timeoutMs);
      this.replyInboxes.set(ref, (value) => {
        clearTimeout(timer);
        resolve(value);
      });
      try {
        this.send(to, { kind: 'request', from: this.hostPid, ref, value: copyMessage(message) });
      } catch (error) {
        clearTimeout(timer);
        this.replyInboxes.delete(ref);
        reject(error);
      }
    });
  }

  // ---- links and monitors -------------------------------------------------

  link(a: Pid, b: Pid): void {
    const ra = this.records.get(pidKey(a));
    const rb = this.records.get(pidKey(b));
    if (!ra || !rb) throw new NoProcError(ra ? b : a);
    ra.links.add(pidKey(b));
    rb.links.add(pidKey(a));
  }

  unlink(a: Pid, b: Pid): void {
    this.records.get(pidKey(a))?.links.delete(pidKey(b));
    this.records.get(pidKey(b))?.links.delete(pidKey(a));
  }

  monitor(pid: Pid): { ref: string; down: Promise<unknown> } {
    const record = this.records.get(pidKey(pid));
    if (!record || record.terminal) {
      return { ref: `m${++this.refCounter}`, down: Promise.resolve(record?.exitReason ?? 'noproc') };
    }
    const ref = `m${++this.refCounter}`;
    const down = new Promise<unknown>((resolve) => {
      record.watchedBy.set(ref, { ref, watcher: pidKey(this.hostPid), resolve });
      this.watchersByRef.set(ref, { watcher: pidKey(this.hostPid), resolve });
    });
    return { ref, down };
  }

  // ---- lifecycle ----------------------------------------------------------

  /** Stop a process. Idempotent. `reason: 'kill'` is untrappable (same as any stop in v1). */
  stop(pid: Pid, reason: unknown = 'shutdown'): void {
    const record = this.records.get(pidKey(pid));
    if (!record || record.terminal) return;
    record.controller.abort(reason);
    const state: ProcessState = reason === 'normal' ? 'done' : 'killed';
    this.finish(record, state, reason);
  }

  async shutdown(): Promise<void> {
    this.shutdownRequested = true;
    const records = [...this.records.values()];
    for (const record of records) this.stop(record.pid, 'shutdown');
    for (const resolver of this.replyInboxes.values()) resolver(undefined);
    this.replyInboxes.clear();
  }

  // ---- naming -------------------------------------------------------------

  register(name: string, pid: Pid): void {
    if (this.names.has(name)) throw new Error(`name already registered: ${name}`);
    if (!this.records.has(pidKey(pid))) throw new NoProcError(pid);
    this.names.set(name, pid);
  }

  whereis(name: string): Pid | undefined {
    const pid = this.names.get(name) ?? this.pidByLabel(name);
    if (!pid) return undefined;
    const record = this.records.get(pidKey(pid));
    if (!record || record.terminal) {
      this.names.delete(name);
      return undefined;
    }
    return pid;
  }

  /** Resolve a process label (the goal functor) to a live pid. */
  private pidByLabel(name: string): Pid | undefined {
    for (const record of this.records.values()) {
      if (!record.terminal && record.name === name) return record.pid;
    }
    return undefined;
  }

  // ---- inspection ---------------------------------------------------------

  info(pid: Pid): ProcessInfo | undefined {
    const record = this.records.get(pidKey(pid));
    return record ? this.toInfo(record) : undefined;
  }

  processes(): ProcessInfo[] {
    const order: Record<ProcessState, number> = {
      running: 0,
      runnable: 1,
      waiting: 2,
      input: 3,
      done: 4,
      crashed: 5,
      killed: 6,
    };
    return [...this.records.values()]
      .sort((a, b) => order[a.state] - order[b.state] || a.startedAt - b.startedAt)
      .map((record) => this.toInfo(record));
  }

  get size(): number {
    return this.records.size;
  }

  /** Processes that have not reached a terminal state. */
  private liveCount(): number {
    let live = 0;
    for (const record of this.records.values()) if (!record.terminal) live += 1;
    return live;
  }

  /** Drop the oldest terminal records beyond `maxTerminalRecords`. */
  private reapTerminalRecords(): void {
    const terminal = [...this.records.entries()].filter(([, record]) => record.terminal);
    const excess = terminal.length - this.maxTerminalRecords;
    for (let index = 0; index < excess; index += 1) {
      this.records.delete(terminal[index][0]);
    }
  }

  /** Resolve when `pid` reaches a terminal state (already terminal resolves now). */
  wait(pid: Pid): Promise<ProcessInfo | undefined> {
    const record = this.records.get(pidKey(pid));
    if (!record) return Promise.resolve(undefined);
    if (record.terminal) return Promise.resolve(this.toInfo(record));
    return new Promise((resolve) => {
      record.terminalWaiters.push(() => resolve(this.toInfo(record)));
    });
  }

  private toInfo(record: ProcRecord): ProcessInfo {
    return {
      pid: record.pid,
      name: record.name,
      goal: record.goal,
      state: record.state,
      priority: record.priority,
      mailboxDepth: record.mailbox.length,
      links: [...record.links].map((key) => this.pidFromKey(key)).filter((pid): pid is Pid => Boolean(pid)),
      monitors: [...record.watching].map((ref) => this.watchersByRef.get(ref)?.watcher).filter(Boolean).map((key) => this.pidFromKey(key!)).filter((pid): pid is Pid => Boolean(pid)),
      usage: record.usage,
      startedAt: record.startedAt,
      finishedAt: record.finishedAt,
      restarts: record.restarts,
      currentActivity: record.currentActivity,
      parent: record.options.parent,
      codeVersion: record.options.codeVersion,
      result: record.result,
      error: record.error,
      exitReason: record.exitReason,
    };
  }

  private pidFromKey(key: string): Pid | undefined {
    const record = this.records.get(key);
    return record?.pid;
  }

  private emit(event: Omit<ProcessEvent, 'at'>): void {
    this.onEvent?.({ ...event, at: this.now() });
  }
}

// ---------------------------------------------------------------------------
// The broker seam (all side effects, always on the main thread)
// ---------------------------------------------------------------------------

export interface CapabilityContext {
  pid: Pid;
  signal: AbortSignal;
}

/**
 * Everything a process can do to the outside world (spec §2.3). At L1 the REPL
 * implements this directly; at L2 a worker sends `need` messages and the main
 * thread services them. A worker never holds credentials or policy.
 */
export interface CapabilityBroker {
  requestInput(prompt: string, ctx: CapabilityContext): Promise<string | undefined>;
  emit(event: ProcessEvent, ctx: { pid: Pid }): void;
}
