/**
 * Message protocol between the main thread and a process worker (L2).
 *
 * See `docs/DML_REPL_PROCESSES_SPEC.md` §6.3. Capability requests (`need`) always
 * originate in the worker and are serviced on the main thread through the
 * `CapabilityBroker`, so a worker never holds credentials or policy.
 */

import type { ProcessEvent, ProcessInfo, Pid, SpawnOptions } from './processes.js';
import type { ProcessRequestPayload, ProcessRequestResult } from '../runner.js';
import type { ToolPolicy } from '../types.js';

/** One tool descriptor sent to the worker so it can build a proxy. */
export interface WorkerToolDescriptor {
  name: string;
  description: string;
  parameters: Record<string, unknown>;
}

export interface WorkerInitData {
  workspacePath?: string;
  sessionId: string;
  harnessText: string;
  harnessVersion?: string;
  model: string;
  provider: string;
  temperature?: number;
  maxTokens?: number;
  toolPolicy: ToolPolicy | null;
  tools: WorkerToolDescriptor[];
}

export type MainToWorker =
  | { type: 'spawn'; requestId: number; goal: string; options: SpawnOptions; harnessText?: string; harnessVersion?: string }
  | { type: 'send'; to: Pid | string; message: unknown }
  | { type: 'stop'; target: Pid | string; reason: unknown }
  | { type: 'register'; name: string; pid: Pid }
  | { type: 'processes'; requestId: number }
  | { type: 'foreground'; requestId: number; request: ProcessRequestPayload; harnessText?: string; harnessVersion?: string }
  | { type: 'call'; requestId: number; to: Pid | string; message: string; timeoutMs: number }
  | { type: 'resolve'; requestId: number; ok: true; result: unknown }
  | { type: 'resolve'; requestId: number; ok: false; error: string }
  | { type: 'stream'; requestId: number; kind: 'text' | 'reasoning'; chunk: string }
  | { type: 'dispose'; requestId: number };

export type WorkerToMain =
  | { type: 'ready' }
  | { type: 'fatal'; error: string }
  | { type: 'event'; event: ProcessEvent }
  | { type: 'spawn_result'; requestId: number; pid?: Pid; error?: string }
  | { type: 'processes_result'; requestId: number; processes: ProcessInfo[] }
  | { type: 'foreground_result'; requestId: number; ok: true; result: ProcessRequestResult }
  | { type: 'foreground_result'; requestId: number; ok: false; error: string }
  | { type: 'call_result'; requestId: number; value?: string; error?: string }
  | { type: 'need'; requestId: number; kind: 'model' | 'tool' | 'input'; payload: unknown }
  | { type: 'disposed'; requestId: number };
