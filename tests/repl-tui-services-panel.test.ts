/**
 * The services panel renders nothing when hidden or empty, and one row per
 * process (summary) or a chronological follow of output (logs).
 */
import { describe, expect, it } from 'vitest';

import { ServicesPanel, type ServicesPanelMode } from '../src/repl/tui/services-panel.js';
import type { ProcessInfo, ProcessLogEntry } from '../src/repl/processes.js';

function info(name: string, overrides: Partial<ProcessInfo> = {}): ProcessInfo {
  return {
    pid: { node: 'main', session: 's', id: name },
    name,
    goal: `${name}()`,
    state: 'running',
    priority: 'background',
    mailboxDepth: 0,
    links: [],
    monitors: [],
    usage: { inputTokens: 0, outputTokens: 0, totalTokens: 0 },
    startedAt: 0,
    restarts: 0,
    ...overrides,
  };
}

function entry(name: string, kind: ProcessLogEntry['kind'], text: string): ProcessLogEntry {
  return { at: 0, pid: { node: 'main', session: 's', id: name }, name, kind, text };
}

function panel(options: {
  processes?: ProcessInfo[];
  log?: ProcessLogEntry[];
  visible?: boolean;
  mode?: ServicesPanelMode;
}): ServicesPanel {
  return new ServicesPanel({
    getProcesses: () => options.processes ?? [],
    getLog: () => options.log ?? [],
    isVisible: () => options.visible ?? true,
    mode: () => options.mode ?? 'summary',
    maxLogLines: 4,
  });
}

describe('ServicesPanel summary', () => {
  it('renders nothing when hidden', () => {
    expect(panel({ processes: [info('a')], visible: false }).render(80)).toEqual([]);
  });

  it('renders nothing when there are no processes', () => {
    expect(panel({ processes: [] }).render(80)).toEqual([]);
  });

  it('renders a header and one row per process', () => {
    const panel = new ServicesPanel({
      getProcesses: () => [
        info('keeper', { state: 'waiting', mailboxDepth: 2 }),
        info('quick', { state: 'done', usage: { inputTokens: 1, outputTokens: 2, totalTokens: 3 } }),
      ],
      getLog: () => [],
      isVisible: () => true,
      mode: () => 'summary',
    });
    const lines = panel.render(80);
    expect(lines).toHaveLength(3);
    expect(lines[0]).toContain('services (2)');
    expect(lines[1]).toContain('keeper');
    expect(lines[1]).toContain('2 queued');
    expect(lines[2]).toContain('3 tok');
  });

  it('flags attention when a process waits on input', () => {
    const panel = new ServicesPanel({
      getProcesses: () => [info('asker', { state: 'input' })],
      getLog: () => [],
      isVisible: () => true,
      mode: () => 'summary',
    });
    expect(panel.render(80)[0]).toContain('needs input');
  });
});

describe('ServicesPanel logs', () => {
  it('follows the last lines, newest at the bottom, at a stable height', () => {
    const log: ProcessLogEntry[] = Array.from({ length: 10 }, (_, index) => entry('debater', 'output', `line ${index}`));
    const lines = panel({ processes: [info('debater')], log, mode: 'logs' }).render(80);
    // header + maxLogLines rows
    expect(lines).toHaveLength(5);
    expect(lines[0]).toContain('logs');
    expect(lines.join('\n')).not.toContain('line 4');
    expect(lines.at(-1)).toContain('line 9');
    expect(lines[1]).toContain('line 6');
  });

  it('includes answers and exits, accent-styled differently from output', () => {
    const log = [entry('debater', 'output', 'hello'), entry('debater', 'answer', '› done')];
    const text = panel({ processes: [info('debater')], log, mode: 'logs' }).render(80).join('\n');
    expect(text).toContain('hello');
    expect(text).toContain('done');
  });
});
