/**
 * The services panel: hidden renders nothing; visible always renders, including
 * the "nothing is running" state; logs mode adds a rule-separated follow of
 * subagent output.
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
  maxLogLines?: number;
}): ServicesPanel {
  return new ServicesPanel({
    getProcesses: () => options.processes ?? [],
    getLog: () => options.log ?? [],
    isVisible: () => options.visible ?? true,
    mode: () => options.mode ?? 'summary',
    maxLogLines: options.maxLogLines ?? 4,
  });
}

describe('ServicesPanel summary', () => {
  it('renders nothing when hidden', () => {
    expect(panel({ processes: [info('a')], visible: false }).render(80)).toEqual([]);
  });

  it('says nothing is running when visible and empty', () => {
    const lines = panel({ processes: [] }).render(80);
    expect(lines).toHaveLength(3);
    expect(lines[0]).toContain('─'); // rule separating it from the transcript
    expect(lines[1]).toContain('nothing is running');
    expect(lines[2]).toContain('/spawn');
  });

  it('renders a rule, a header and one row per process', () => {
    const lines = panel({
      processes: [
        info('keeper', { state: 'waiting', mailboxDepth: 2 }),
        info('quick', { state: 'done', usage: { inputTokens: 1, outputTokens: 2, totalTokens: 3 } }),
      ],
    }).render(80);
    expect(lines).toHaveLength(4);
    expect(lines[0]).toContain('─');
    expect(lines[1]).toContain('2 processes');
    expect(lines[1]).toContain('keeper waiting');
    expect(lines[2]).toContain('keeper');
    expect(lines[2]).toContain('2 queued');
    expect(lines[3]).toContain('3 tok');
  });

  it('flags attention when a process waits on input', () => {
    const lines = panel({ processes: [info('asker', { state: 'input' })] }).render(80);
    expect(lines[1]).toContain('needs input');
  });
});

describe('ServicesPanel logs', () => {
  it('separates the process header from the log with a rule', () => {
    const log = [entry('debater', 'stream', 'the evidence')];
    const lines = panel({ processes: [info('debater')], log, mode: 'logs' }).render(80);
    // rule, header, rule, then maxLogLines rows
    expect(lines).toHaveLength(3 + 4);
    expect(lines[0]).toContain('─');
    expect(lines[1]).toContain('debater running');
    expect(lines[2]).toContain('─');
    expect(lines[3]).toContain('the evidence');
  });

  it('follows the last lines at a stable height', () => {
    const log: ProcessLogEntry[] = Array.from({ length: 10 }, (_, index) => entry('debater', 'output', `line ${index}`));
    const lines = panel({ processes: [info('debater')], log, mode: 'logs' }).render(80);
    expect(lines).toHaveLength(7); // rule + header + rule + 4
    expect(lines.join('\n')).not.toContain('line 4');
    expect(lines.at(-1)).toContain('line 9');
    expect(lines[3]).toContain('line 6');
  });

  it('shows the nothing-running state in logs mode too', () => {
    const lines = panel({ processes: [], log: [], mode: 'logs' }).render(80);
    expect(lines[1]).toContain('nothing is running');
    expect(lines.join('\n')).toContain('/spawn');
  });

  it('never emits a line containing a newline', () => {
    const log = [entry('debater', 'answer', '› first line\nsecond line')];
    const lines = panel({ processes: [info('debater')], log, mode: 'logs' }).render(80);
    expect(lines.every((line) => !line.includes('\n'))).toBe(true);
    expect(lines.join(' ')).toContain('first line');
  });

  it('marks reasoning lines distinctly', () => {
    const log = [entry('debater', 'reasoning', 'considering the options')];
    const lines = panel({ processes: [info('debater')], log, mode: 'logs' }).render(80);
    expect(lines.join('\n')).toContain('~ considering the options');
  });
});
