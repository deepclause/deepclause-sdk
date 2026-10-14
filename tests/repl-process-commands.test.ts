/**
 * REPL command classification and the subagent harness tools.
 */
import { describe, expect, it } from 'vitest';

import { classifyReplInput } from '../src/cli/repl.js';
import { createHarnessTools, formatProcessTable, type HarnessToolController } from '../src/repl/harness-tools.js';
import type { Pid, ProcessInfo, SpawnOptions } from '../src/repl/processes.js';

describe('classifyReplInput subagent commands', () => {
  it('classifies /spawn and its alias', () => {
    expect(classifyReplInput('/spawn keeper([])')).toEqual({ kind: 'spawn', goal: 'keeper([])' });
    expect(classifyReplInput('/serve summarise(topic)')).toEqual({ kind: 'spawn', goal: 'summarise(topic)' });
    expect(classifyReplInput('/spawn')).toMatchObject({ kind: 'unknown-command' });
  });

  it('classifies /ps and its alias', () => {
    expect(classifyReplInput('/ps')).toEqual({ kind: 'ps', watch: false });
    expect(classifyReplInput('/processes')).toEqual({ kind: 'ps', watch: false });
    expect(classifyReplInput('/ps --watch')).toEqual({ kind: 'ps', watch: true });
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
      sendProcess: (_to: Pid | string, message: string) => {
        sent.push(message);
      },
      callProcess: async () => 'pong',
      stopProcess: async (target: Pid | string) => {
        stopped.push(typeof target === 'string' ? target : target.id);
      },
    } as unknown as HarnessToolController;
    return { controller: stub, spawned, sent, stopped };
  }

  it('exposes spawn, ps, send, call and stop', () => {
    const { controller: c } = controller();
    const tools = createHarnessTools(c);
    for (const name of ['spawn', 'ps', 'send', 'call', 'stop']) {
      expect(tools.has(name)).toBe(true);
    }
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
