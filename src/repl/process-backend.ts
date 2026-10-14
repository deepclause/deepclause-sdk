/**
 * The process backend seam.
 *
 * `DMLSession` talks to processes only through this interface. The L1 backend is
 * the in-process `DmlProcessExecutor`; the L2 backend is a `RemoteProcessHost`
 * that routes to a worker thread. Nothing above this interface branches on the
 * mode (spec §2, §6).
 */

import type { ProcessRequestPayload, ProcessRequestResult } from '../runner.js';
import type { Pid, ProcessInfo, SpawnOptions } from './processes.js';

export interface ProcessBackend {
  /** Optional async setup (starting a worker). */
  start?(): Promise<void>;

  spawn(goal: string, options?: SpawnOptions): Promise<Pid>;
  /** Copy a message term into a process's mailbox. */
  send(to: Pid | string, messageTerm: unknown): void;
  stop(target: Pid | string, reason?: unknown): Promise<void>;
  whereis(name: string): Pid | undefined;
  info(pid: Pid): ProcessInfo | undefined;
  /** Synchronous snapshot (L2 serves it from a cache). */
  processes(): ProcessInfo[];
  register(name: string, pid: Pid): void;
  /** Host-side request/reply used by `/call` and the `call` tool. */
  callTerm(to: Pid | string, messageTerm: string, timeoutMs: number): Promise<string>;
  /** Resolve a process primitive for the non-process foreground run. */
  foregroundHandle(request: ProcessRequestPayload, signal?: AbortSignal): Promise<ProcessRequestResult>;
  shutdown(): Promise<void>;
}
