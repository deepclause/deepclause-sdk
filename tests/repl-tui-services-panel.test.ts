/**
 * The services panel renders nothing when hidden or empty, and one row per
 * process when visible.
 */
import { describe, expect, it } from 'vitest';

import { ServicesPanel } from '../src/repl/tui/services-panel.js';
import type { ProcessInfo } from '../src/repl/processes.js';

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

describe('ServicesPanel', () => {
  it('renders nothing when hidden', () => {
    const panel = new ServicesPanel({ getProcesses: () => [info('a')], isVisible: () => false });
    expect(panel.render(80)).toEqual([]);
  });

  it('renders nothing when there are no processes', () => {
    const panel = new ServicesPanel({ getProcesses: () => [], isVisible: () => true });
    expect(panel.render(80)).toEqual([]);
  });

  it('renders a header and one row per process', () => {
    const panel = new ServicesPanel({
      getProcesses: () => [
        info('keeper', { state: 'waiting', mailboxDepth: 2 }),
        info('quick', { state: 'done', usage: { inputTokens: 1, outputTokens: 2, totalTokens: 3 } }),
      ],
      isVisible: () => true,
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
      isVisible: () => true,
    });
    expect(panel.render(80)[0]).toContain('needs input');
  });
});
