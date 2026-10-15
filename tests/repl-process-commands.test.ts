/**
 * REPL command classification and the subagent harness tools.
 */
import { describe, expect, it } from 'vitest';

import { classifyReplInput, isSlashCommand } from '../src/cli/repl.js';
import { createHarnessTools, formatProcessTable, type HarnessToolController } from '../src/repl/harness-tools.js';
import type { Pid, ProcessInfo, SpawnOptions } from '../src/repl/processes.js';

describe('classifyReplInput subagent commands', () => {
  it('treats slash lines as commands, never steering', () => {
    expect(isSlashCommand('/send keeper stop')).toBe(true);
    expect(isSlashCommand('  /ps')).toBe(true);
    expect(isSlashCommand('/call keeper ping')).toBe(true);
    expect(isSlashCommand('please stop the keeper')).toBe(false);
  });
  it('classifies /spawn and its alias', () => {
    expect(classifyReplInput('/spawn keeper([])')).toEqual({ kind: 'spawn', goal: 'keeper([])' });
    expect(classifyReplInput('/serve summarise(topic)')).toEqual({ kind: 'spawn', goal: 'summarise(topic)' });
    expect(classifyReplInput('/spawn')).toMatchObject({ kind: 'unknown-command' });
  });

  it('classifies /ps and its alias', () => {
    expect(classifyReplInput('/ps')).toEqual({ kind: 'ps', watch: false, logs: false });
    expect(classifyReplInput('/processes')).toEqual({ kind: 'ps', watch: false, logs: false });
    expect(classifyReplInput('/ps --watch')).toEqual({ kind: 'ps', watch: true, logs: false });
    expect(classifyReplInput('/ps --watch --logs')).toEqual({ kind: 'ps', watch: true, logs: true });
    expect(classifyReplInput('/ps --logs')).toEqual({ kind: 'ps', watch: false, logs: true });
  });

  it('classifies /send and /call with a target and message', () => {
    expect(classifyReplInput('/send note_keeper hello there')).toEqual({
      kind: 'send',
      target: 'note_keeper',
      message: 'hello there',
    });
    expect(classifyReplInput('/call keeper list')).toEqual({
      kind: 'call',
      target: 'keeper',
      message: 'list',
    });
    expect(classifyReplInput('/send onlytarget')).toMatchObject({ kind: 'unknown-command' });
  });

  it('classifies /stop', () => {
    expect(classifyReplInput('/stop keeper')).toEqual({ kind: 'stop', target: 'keeper' });
    expect(classifyReplInput('/stop')).toMatchObject({ kind: 'unknown-command' });
  });

  it('classifies /login', () => {
    expect(classifyReplInput('/login')).toEqual({ kind: 'login' });
  });
});

describe('formatProcessTable', () => {
  const info = (overrides: Partial<ProcessInfo>): ProcessInfo => ({
    pid: { node: 'main', session: 's', id: 'p1' },
    name: 'keeper',
    goal: 'keeper([])',
    state: 'waiting',
    priority: 'background',
    mailboxDepth: 0,
    links: [],
    monitors: [],
    usage: { inputTokens: 0, outputTokens: 0, totalTokens: 0 },
    startedAt: 0,
    restarts: 0,
    ...overrides,
  });

  it('says so when there is nothing running', () => {
    expect(formatProcessTable([])).toBe('no subagents');
  });

  it('renders one line per process', () => {
    const table = formatProcessTable([
      info({}),
      info({ pid: { node: 'main', session: 's', id: 'p2' }, name: 'quick', state: 'done', usage: { inputTokens: 1, outputTokens: 2, totalTokens: 3 } }),
    ]);
    expect(table.split('\n')).toHaveLength(2);
    expect(table).toContain('keeper');
    expect(table).toContain('waiting');
    expect(table).toContain('3 tok');
  });
});

describe('subagent harness tools', () => {
  function controller(): { controller: HarnessToolController; spawned: SpawnOptions[]; sent: string[]; stopped: string[] } {
    const spawned: SpawnOptions[] = [];
    const sent: string[] = [];
    const stopped: string[] = [];
    const processes: ProcessInfo[] = [];
    const pid: Pid = { node: 'main', session: 's', id: 'p1' };
    const stub = {
      isFrozen: () => false,
      spawnProcess: async (_goal: string, options?: SpawnOptions) => {
        spawned.push(options ?? {});
        processes.push({
          pid,
          name: options?.name ?? 'keeper',
          goal: 'keeper([])',
          state: 'running',
          priority: 'background',
          mailboxDepth: 0,
          links: [],
          monitors: [],
          usage: { inputTokens: 0, outputTokens: 0, totalTokens: 0 },
          startedAt: 0,
          restarts: 0,
        });
        return pid;
      },
      processes: () => processes,
      whereisProcess: () => undefined,
      processInfo: (pid: Pid) => processes.find((p) => p.pid.id === pid.id),
      processCode: () => undefined,
      currentHarnessVersion: () => 'testversion',
      sendProcess: (_to: Pid | string, message: string) => {
        sent.push(message);
      },
      callProcess: async () => 'pong',
      stopProcess: async (target: Pid | string) => {
        stopped.push(typeof target === 'string' ? target : target.id);
      },
      execute: async () => ({
        bindings: [
          { name: 'DcSwarmResults', value: '[ok(1), ok(2)]' },
          { name: 'DcWaitOutcome', value: 'ok("done")' },
        ],
        solutions: [],
        output: '',
        elapsedMs: 0,
      }),
    } as unknown as HarnessToolController;
    return { controller: stub, spawned, sent, stopped };
  }

  it('exposes spawn, ps, send, call, stop, swarm and wait', () => {
    const { controller: c } = controller();
    const tools = createHarnessTools(c);
    for (const name of ['spawn', 'ps', 'send', 'call', 'stop', 'swarm', 'wait']) {
      expect(tools.has(name)).toBe(true);
    }
  });

  it('swarm and wait run through the execute tool', async () => {
    const { controller: c } = controller();
    const tools = createHarnessTools(c);
    const swarm = await tools.get('swarm')!.execute({ worker: 'mark', items: ['a', 'b'] });
    expect(swarm).toMatchObject({ results: expect.stringContaining('ok(1)') });
    const wait = await tools.get('wait')!.execute({ pid: 'proc(main,s,p1)' });
    expect(wait).toMatchObject({ outcome: expect.stringContaining('ok(') });
  });

  it('spawn returns a pid and ps lists it', async () => {
    const { controller: c, spawned } = controller();
    const tools = createHarnessTools(c);
    const result = await tools.get('spawn')!.execute({ goal: 'keeper([])', name: 'keeper' });
    expect(result).toMatchObject({ name: 'keeper' });
    expect(spawned[0]?.name).toBe('keeper');
    expect(await tools.get('ps')!.execute({})).toMatchObject({ text: expect.stringContaining('keeper') });
  });

  it('send and stop forward to the controller', async () => {
    const { controller: c, sent, stopped } = controller();
    const tools = createHarnessTools(c);
    await tools.get('send')!.execute({ to: 'keeper', message: 'stop' });
    await tools.get('stop')!.execute({ target: 'keeper' });
    expect(sent).toEqual(['stop']);
    expect(stopped).toEqual(['keeper']);
  });
});
