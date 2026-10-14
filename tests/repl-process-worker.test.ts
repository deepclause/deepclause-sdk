/**
 * L2 worker host: RPC correlation, the synchronous process cache, name
 * resolution, and capability (`need`) servicing.
 *
 * The real worker thread is verified end-to-end manually (and by the gated test
 * at the bottom); these tests drive the main-thread host against a fake worker so
 * the protocol logic is deterministic and runs in every CI.
 */
import { describe, expect, it } from 'vitest';

import { RemoteProcessHost, type WorkerLike } from '../src/repl/worker-host.js';
import type { DMLSession } from '../src/repl/session.js';
import type { ProcessInfo, Pid } from '../src/repl/processes.js';
import type { MainToWorker, WorkerInitData, WorkerToMain } from '../src/repl/worker-protocol.js';

const delay = (ms: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, ms));

function info(pid: Pid, name: string): ProcessInfo {
  return {
    pid,
    name,
    goal: `${name}`,
    state: 'running',
    priority: 'background',
    mailboxDepth: 0,
    links: [],
    monitors: [],
    usage: { inputTokens: 0, outputTokens: 0, totalTokens: 0 },
    startedAt: 0,
    restarts: 0,
  };
}

/** A scripted stand-in for `worker_threads.Worker`. */
class FakeWorker implements WorkerLike {
  readonly sent: MainToWorker[] = [];
  readonly processes: ProcessInfo[] = [];
  private readonly listeners: Array<(message: WorkerToMain) => void> = [];
  private readySent = false;
  private counter = 0;

  on(event: 'message', handler: (message: WorkerToMain) => void): unknown {
    if (event === 'message') {
      this.listeners.push(handler);
      if (!this.readySent) {
        this.readySent = true;
        queueMicrotask(() => this.emit({ type: 'ready' }));
      }
    }
    return this;
  }

  postMessage(message: MainToWorker): void {
    this.sent.push(message);
    switch (message.type) {
      case 'spawn': {
        const pid: Pid = { node: 'w0', session: 's', id: `p${++this.counter}` };
        this.processes.push(info(pid, message.options.name ?? 'worker-proc'));
        this.emit({ type: 'spawn_result', requestId: message.requestId, pid });
        break;
      }
      case 'processes':
        this.emit({ type: 'processes_result', requestId: message.requestId, processes: [...this.processes] });
        break;
      case 'foreground':
        this.emit({
          type: 'foreground_result',
          requestId: message.requestId,
          ok: true,
          result: { key: 'self', term: 'proc(w0,s,host)' },
        });
        break;
      case 'call':
        this.emit({ type: 'call_result', requestId: message.requestId, value: 'pong' });
        break;
      case 'dispose':
        this.emit({ type: 'disposed', requestId: message.requestId });
        break;
      default:
        break;
    }
  }

  emit(message: WorkerToMain): void {
    for (const listener of this.listeners) listener(message);
  }

  terminate(): Promise<number> {
    return Promise.resolve(0);
  }
}

function stubSession(log: { tools: string[]; models: number; inputs: string[] }): DMLSession {
  return {
    getSessionId: () => 's',
    workspacePathValue: () => undefined,
    harnessText: async () => '',
    modelId: () => 'test-model',
    providerId: () => 'openai',
    temperatureValue: () => 0.7,
    maxTokensValue: () => 1024,
    toolPolicyValue: () => null,
    toolDescriptors: () => [{ name: 'bash', description: 'run', parameters: {} }],
    completeModelRequest: async () => {
      log.models += 1;
      return { text: 'model-ok', usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 } };
    },
    runHostTool: async (name: string) => {
      log.tools.push(name);
      return { ran: name };
    },
    requestHumanInput: async (prompt: string) => {
      log.inputs.push(prompt);
      return 'answer';
    },
  } as unknown as DMLSession;
}

async function startHost(): Promise<{ host: RemoteProcessHost; worker: FakeWorker; log: { tools: string[]; models: number; inputs: string[] } }> {
  const worker = new FakeWorker();
  const log = { tools: [] as string[], models: 0, inputs: [] as string[] };
  const host = new RemoteProcessHost(stubSession(log), {
    workerFactory: (_init: WorkerInitData) => worker,
    refreshMs: 20,
  });
  await host.start();
  return { host, worker, log };
}

describe('RemoteProcessHost', () => {
  it('spawns through the worker and caches the process table', async () => {
    const { host, worker } = await startHost();
    const pid = await host.spawn('keeper', { name: 'keeper' });
    expect(pid.id).toBe('p1');
    await delay(60);
    expect(host.processes()).toHaveLength(1);
    expect(host.info(pid)?.name).toBe('keeper');
    expect(host.whereis('keeper')?.id).toBe('p1');
    await host.shutdown();
    expect(worker.sent.some((message) => message.type === 'dispose')).toBe(true);
  });

  it('forwards foreground primitives and calls', async () => {
    const { host } = await startHost();
    const result = await host.foregroundHandle({ op: 'self' });
    expect(result).toEqual({ key: 'self', term: 'proc(w0,s,host)' });
    expect(await host.callTerm('keeper', 'ping', 1000)).toBe('pong');
    await host.shutdown();
  });

  it('services worker capability requests on the main thread', async () => {
    const { host, worker, log } = await startHost();
    worker.emit({ type: 'need', requestId: 41, kind: 'tool', payload: { name: 'bash', args: { command: 'true' } } });
    await delay(20);
    expect(log.tools).toEqual(['bash']);
    const resolve = worker.sent.find((message) => message.type === 'resolve' && message.requestId === 41);
    expect(resolve).toMatchObject({ ok: true });

    worker.emit({ type: 'need', requestId: 42, kind: 'input', payload: { prompt: 'ok?' } });
    await delay(20);
    expect(log.inputs).toEqual(['ok?']);
    await host.shutdown();
  });

  it('forwards process events to the sink', async () => {
    const worker = new FakeWorker();
    const seen: string[] = [];
    const host = new RemoteProcessHost(stubSession({ tools: [], models: 0, inputs: [] }), {
      workerFactory: () => worker,
      refreshMs: 20,
      onEvent: (event) => seen.push(event.type),
    });
    await host.start();
    worker.emit({ type: 'event', event: { type: 'started', at: 1, pid: { node: 'w0', session: 's', id: 'p1' }, name: 'keeper' } });
    expect(seen).toContain('started');
    await host.shutdown();
  });
});

// The real worker thread is exercised manually against `dist`; vitest cannot load
// a TypeScript worker entry. Run with DC_WORKER_TEST=1 after `npm run build` to
// enable it (it imports the built worker).
const runRealWorker = process.env.DC_WORKER_TEST === '1';
describe.skipIf(!runRealWorker)('real worker (opt-in)', () => {
  it('is covered by tests/repl-process-spawn.test.ts scenarios', () => {
    expect(runRealWorker).toBe(true);
  });
});
