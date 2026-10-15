/**
 * Process core: identity, mailboxes, links, monitors, naming, limits.
 *
 * The DML surface is layered on this in a later phase; these tests exercise the
 * L1 host directly with plain async bodies.
 */
import { describe, expect, it } from 'vitest';

import {
  LocalProcessHost,
  MailboxFullError,
  NoProcError,
  ProcessExit,
  labelForGoal,
  type ProcessBody,
  type ProcessEvent,
} from '../src/repl/processes.js';

const flush = (): Promise<void> => new Promise((resolve) => setImmediate(resolve));

function makeHost(options: Partial<ConstructorParameters<typeof LocalProcessHost>[0]> = {}): LocalProcessHost {
  return new LocalProcessHost({ sessionId: 'test-session', ...options });
}

/** A host plus a goal→body registry, the shape the DML layer will use. */
function makeRegistryHost(options: Partial<ConstructorParameters<typeof LocalProcessHost>[0]> = {}) {
  const bodies = new Map<string, ProcessBody>();
  const host = new LocalProcessHost({
    sessionId: 'test-session',
    resolveBody: (goal) => bodies.get(goal),
    ...options,
  });
  return { host, bodies };
}

describe('labelForGoal', () => {
  it('uses the goal functor, falling back to "process"', () => {
    expect(labelForGoal('summarise("x")')).toBe('summarise');
    expect(labelForGoal('  note_keeper([])')).toBe('note_keeper');
    expect(labelForGoal('("x")')).toBe('process');
  });
});

describe('LocalProcessHost lifecycle', () => {
  it('runs a one-shot body and records the result', async () => {
    const host = makeHost();
    const events: ProcessEvent[] = [];
    const withEvents = new LocalProcessHost({ sessionId: 's', onEvent: (e) => events.push(e) });

    const pid = withEvents.spawn('one_shot', {}, async (ctx) => {
      ctx.setActivity('working');
      return 42;
    });
    const info = await withEvents.wait(pid);

    expect(info?.state).toBe('done');
    expect(info?.result).toBe(42);
    expect(info?.name).toBe('one_shot');
    expect(events.some((e) => e.type === 'started')).toBe(true);
    expect(events.some((e) => e.type === 'answer' && e.answer === 42)).toBe(true);
    expect(events.some((e) => e.type === 'exited' && e.state === 'done')).toBe(true);

    await host.shutdown();
  });

  it('marks an uncaught throw as crashed', async () => {
    const host = makeHost();
    const pid = host.spawn('boom', {}, async () => {
      throw new Error('kaboom');
    });
    const info = await host.wait(pid);
    expect(info?.state).toBe('crashed');
    expect(info?.error).toContain('kaboom');
  });

  it('marks ProcessExit as killed with the reason', async () => {
    const host = makeHost();
    const pid = host.spawn('bye', {}, async (ctx) => {
      ctx.exit('because');
      return 'unreachable';
    });
    const info = await host.wait(pid);
    expect(info?.state).toBe('killed');
    expect(info?.exitReason).toBe('because');
  });
});

describe('messaging', () => {
  it('delivers a message to a waiting receiver', async () => {
    const host = makeHost();
    const pid = host.spawn('waiter', {}, async (ctx) => {
      const r = await ctx.receive<string>(1000);
      return r.ok ? r.message : 'none';
    });
    await flush();
    host.send(pid, 'hello');
    expect((await host.wait(pid))?.result).toBe('hello');
  });

  it('times out an empty receive', async () => {
    const host = makeHost();
    const pid = host.spawn('waiter', {}, async (ctx) => {
      const r = await ctx.receive(10);
      return r.ok ? 'message' : r.reason;
    });
    expect((await host.wait(pid))?.result).toBe('timeout');
  });

  it('copies messages so the sender and receiver never share a term', async () => {
    const host = makeHost();
    const message = { list: [1, 2, 3] };
    const pid = host.spawn('mutator', {}, async (ctx) => {
      const r = await ctx.receive<{ list: number[] }>(1000);
      if (r.ok) r.message.list.push(999);
      return r.ok ? r.message.list.length : 0;
    });
    await flush();
    host.send(pid, message);
    expect((await host.wait(pid))?.result).toBe(4);
    expect(message.list).toEqual([1, 2, 3]);
  });

  it('rejects sends to a dead process and an out-of-range mailbox', async () => {
    const host = makeHost({ maxMailbox: 1 });
    const slow = host.spawn('slow', {}, async () => new Promise(() => undefined));
    host.send(slow, 'a');
    expect(() => host.send(slow, 'b')).toThrow(MailboxFullError);
    host.stop(slow, 'shutdown');
    expect(() => host.send(slow, 'c')).toThrow(NoProcError);
  });

  it('answers a request/reply call', async () => {
    const host = makeHost();
    const pid = host.spawn('echo', {}, async (ctx) => {
      const r = await ctx.receive<{ kind: string; from: unknown; ref: string; value: string }>(1000);
      if (r.ok && r.message.kind === 'request') {
        ctx.send(r.message.from as never, { kind: 'reply', ref: r.message.ref, value: r.message.value });
      }
      return 'served';
    });
    await flush();
    expect(await host.call(pid, 'ping', 1000)).toBe('ping');
  });
});

describe('spawning children', () => {
  it('lets a process spawn and await a child', async () => {
    const { host, bodies } = makeRegistryHost();
    bodies.set('child', async () => 'child-result');
    bodies.set('parent', async (ctx) => {
      const child = ctx.spawn('child');
      const info = await host.wait(child);
      return info?.result;
    });
    const pid = host.spawn('parent');
    expect((await host.wait(pid))?.result).toBe('child-result');
  });

  it('enforces the process limit', () => {
    const host = makeHost({ maxProcesses: 1 });
    host.spawn('first', {}, async () => new Promise(() => undefined));
    expect(() => host.spawn('second', {}, async () => 1)).toThrow(/process limit/);
  });

  it('bounds live processes, not cumulative spawns', async () => {
    // A finished process must not consume a slot forever: a session that spawned
    // 16 things could otherwise never fan out again.
    const host = makeHost({ maxProcesses: 2 });
    const a = host.spawn('a', {}, async () => 'a');
    const b = host.spawn('b', {}, async () => 'b');
    await host.wait(a);
    await host.wait(b);
    expect(() => host.spawn('c', {}, async () => new Promise(() => undefined))).not.toThrow();
    expect(() => host.spawn('d', {}, async () => new Promise(() => undefined))).not.toThrow();
    // Now two are live again, so the limit applies.
    expect(() => host.spawn('e', {}, async () => 1)).toThrow(/process limit/);
  });
});

describe('links and monitors', () => {
  it('propagates an abnormal exit to a linked process', async () => {
    const host = makeHost();
    const linked = host.spawn('linked', {}, async (ctx) => {
      await ctx.receive(5000);
      return 'unreachable';
    });
    host.spawn('crasher', { link: linked }, async () => {
      throw new Error('boom');
    });
    const info = await host.wait(linked);
    expect(info?.state).toBe('killed');
  });

  it('resolves a monitor with the exit reason', async () => {
    const host = makeHost();
    const good = host.spawn('good', {}, async () => 'ok');
    expect(await host.monitor(good).down).toBe('normal');

    const bad = host.spawn('bad', {}, async () => {
      throw new Error('nope');
    });
    const reason = await host.monitor(bad).down;
    expect(reason).toMatchObject({ error: 'nope' });
  });

  it('resolves a monitor immediately for an unknown pid', async () => {
    const host = makeHost();
    expect(await host.monitor({ node: 'main', session: 'x', id: 'gone' }).down).toBe('noproc');
  });
});

describe('naming', () => {
  it('registers, resolves, and releases a name on exit', async () => {
    const host = makeHost();
    const pid = host.spawn('named', {}, async () => 'done');
    host.register('svc', pid);
    expect(host.whereis('svc')?.id).toBe(pid.id);
    await host.wait(pid);
    expect(host.whereis('svc')).toBeUndefined();
  });

  it('refuses a duplicate name', () => {
    const host = makeHost();
    const pid = host.spawn('a', {}, async () => 1);
    host.register('svc', pid);
    expect(() => host.register('svc', pid)).toThrow(/already registered/);
  });
});

describe('inspection and shutdown', () => {
  it('reports the whole system state', async () => {
    const host = makeHost();
    const pid = host.spawn('reporter', { priority: 'background' }, async () => 'r');
    const infos = host.processes();
    expect(infos).toHaveLength(1);
    expect(infos[0]?.pid.id).toBe(pid.id);
    expect(infos[0]?.priority).toBe('background');
    await host.wait(pid);
    expect(host.info(pid)?.state).toBe('done');
  });

  it('stops every process and refuses new spawns after shutdown', async () => {
    const host = makeHost();
    const pid = host.spawn('long', {}, async () => new Promise(() => undefined));
    await host.shutdown();
    expect((await host.wait(pid))?.state).toBe('killed');
    expect(() => host.spawn('late', {}, async () => 1)).toThrow(/shut down/);
  });
});
