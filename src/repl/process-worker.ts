/**
 * Process worker (L2).
 *
 * Runs a session-scoped process host and its subagents in a dedicated
 * `worker_threads` thread with its own `swipl-wasm` instance. All side effects
 * (model, tools, human input) are proxied to the main thread, so a worker never
 * holds credentials or policy (spec §2.3).
 */

import { parentPort, workerData } from 'node:worker_threads';

import type { LLMBackend, LLMBackendRequest, LLMBackendResponse, ToolDefinition } from '../types.js';
import { DMLSession } from './session.js';
import type { MainToWorker, WorkerInitData } from './worker-protocol.js';

if (!parentPort) throw new Error('process-worker must run inside a worker thread');
const port = parentPort;

interface PendingRpc {
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
  onText?: (chunk: string) => void;
  onReasoning?: (chunk: string) => void;
}

const pending = new Map<number, PendingRpc>();
let requestCounter = 0;

function rpc(
  kind: 'model' | 'tool' | 'input',
  payload: unknown,
  onText?: (chunk: string) => void,
  onReasoning?: (chunk: string) => void,
): Promise<unknown> {
  const requestId = ++requestCounter;
  return new Promise((resolve, reject) => {
    pending.set(requestId, { resolve, reject, onText, onReasoning });
    port.postMessage({ type: 'need', requestId, kind, payload });
  });
}

/** A backend that forwards every model call to the main thread. */
function proxyBackend(): LLMBackend {
  return {
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
  };
}

/** Stub tools with the host's schemas whose `execute` forwards to the main thread. */
function proxyTools(descriptors: WorkerInitData['tools']): Map<string, ToolDefinition> {
  const tools = new Map<string, ToolDefinition>();
  for (const descriptor of descriptors) {
    tools.set(descriptor.name, {
      description: descriptor.description,
      parameters: descriptor.parameters as unknown as ToolDefinition['parameters'],
      execute: (args: Record<string, unknown>) => rpc('tool', { name: descriptor.name, args }),
    });
  }
  return tools;
}

async function main(): Promise<void> {
  const init = workerData as WorkerInitData;
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
  session.setProcessEventSink((event) => port.postMessage({ type: 'event', event }));

  port.on('message', (message: MainToWorker) => {
    void handle(session, message);
  });

  port.postMessage({ type: 'ready' });
}

async function handle(session: DMLSession, message: MainToWorker): Promise<void> {
  switch (message.type) {
    case 'spawn':
      try {
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
      break;

    case 'register':
      session.registerProcess(message.name, message.pid);
      break;

    case 'processes':
      port.postMessage({ type: 'processes_result', requestId: message.requestId, processes: session.processes() });
      break;

    case 'foreground':
      try {
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

main().catch((error) => port.postMessage({ type: 'fatal', error: messageOf(error) }));
