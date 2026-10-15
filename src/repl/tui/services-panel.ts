/**
 * Live services panel: a dock row for the session's subagents.
 *
 * Renders nothing when hidden. When visible it always renders, so
 * `/ps --watch` with no subagents says so instead of leaving a blank row.
 *
 * Two modes, toggled with `/ps --watch` and `/ps --watch --logs`:
 *  - `summary` — one row per process with state, activity, queue and usage;
 *  - `logs`    — the process list over a full-width rule, then a chronological
 *                follow of each subagent's streamed output, reasoning, tool
 *                calls and answers.
 */

import { truncateToWidth, visibleWidth, type Component } from '@earendil-works/pi-tui';

import type { ProcessInfo, ProcessLogEntry, ProcessState } from '../processes.js';
import { dim, tuiStyle } from './theme.js';

export type ServicesPanelMode = 'summary' | 'logs';

export interface ServicesPanelOptions {
  getProcesses: () => ProcessInfo[];
  getLog: () => ProcessLogEntry[];
  isVisible: () => boolean;
  mode: () => ServicesPanelMode;
  /** Rows of log shown in logs mode (default 16). */
  maxLogLines?: number;
}

const ACTIVE: ProcessState[] = ['running', 'runnable', 'waiting', 'input'];
const NAME_WIDTH = 12;

function stateLabel(state: ProcessState): string {
  return state === 'input' ? 'input' : state;
}

function processSummaryLine(processes: ProcessInfo[]): string {
  if (processes.length === 0) return 'nothing is running';
  const active = processes.filter((process) => ACTIVE.includes(process.state)).length;
  const attention = processes.some((process) => process.state === 'input');
  const names = processes
    .slice(0, 6)
    .map((process) => `${process.name} ${stateLabel(process.state)}`)
    .join(' · ');
  const more = processes.length > 6 ? ` · +${processes.length - 6}` : '';
  return `${processes.length} process${processes.length === 1 ? '' : 'es'}`
    + `${active > 0 ? ` · ${active} active` : ''}`
    + `${attention ? ' · needs input' : ''}`
    + ` · ${names}${more}`;
}

export class ServicesPanel implements Component {
  constructor(private readonly options: ServicesPanelOptions) {}

  render(width: number): string[] {
    if (!this.options.isVisible()) return [];
    const columns = Math.max(1, width);
    const rule = dim('─'.repeat(columns));
    const processes = this.options.getProcesses();
    const header = dim(`⚙ services — ${processSummaryLine(processes)}`);

    if (this.options.mode() === 'logs') return this.renderLogs(columns, processes, rule, header);
    return [rule, header, ...this.renderSummary(columns, processes)];
  }

  private renderSummary(columns: number, processes: ProcessInfo[]): string[] {
    if (processes.length === 0) {
      return [dim('  /spawn <goal> to start a subagent')];
    }
    return processes.map((process) => {
      const usage = process.usage.totalTokens > 0 ? ` · ${process.usage.totalTokens} tok` : '';
      const activity = process.currentActivity ? ` · ${process.currentActivity}` : '';
      const error = process.error ? ` (${process.error})` : '';
      const mail = process.mailboxDepth > 0 ? ` · ${process.mailboxDepth} queued` : '';
      const text = `  ${process.name}  ${stateLabel(process.state)}${error}${activity}${mail}${usage}`;
      const painted = ACTIVE.includes(process.state) ? text : dim(text);
      return truncateToWidth(painted, columns, '…');
    });
  }

  private renderLogs(columns: number, processes: ProcessInfo[], rule: string, header: string): string[] {
    const log = this.options.getLog();
    if (processes.length === 0 && log.length === 0) {
      return [rule, header, dim('  /spawn <goal> to start a subagent')];
    }

    const max = this.options.maxLogLines ?? 16;
    const slice = log.slice(-max);

    // Entries first (top-down), then pad to a stable height so the panel does
    // not jump as output arrives.
    const rows: string[] = [];
    for (const entry of slice) {
      rows.push(truncateToWidth(this.formatLogLine(entry), columns, '…'));
    }
    for (let i = slice.length; i < max; i += 1) rows.push('');

    // Rule above (separates from the transcript), process header, rule, log.
    return [rule, header, rule, ...rows];
  }

  private formatLogLine(entry: ProcessLogEntry): string {
    const raw = entry.name.length > NAME_WIDTH ? `${entry.name.slice(0, NAME_WIDTH - 1)}…` : entry.name;
    const padding = ' '.repeat(Math.max(0, NAME_WIDTH - visibleWidth(raw)));
    const label = dim(`${raw}${padding}`);
    // A component line must not contain a newline: a multi-line answer/result
    // would otherwise break the layout.
    const body = entry.text.replace(/\s*\n\s*/g, ' ↵ ');
    const prefix = entry.kind === 'reasoning' ? '~ ' : '';
    const text = `${prefix}${body}`;
    const styled = entry.kind === 'output' || entry.kind === 'stream'
      ? text
      : entry.kind === 'reasoning'
        ? dim(text)
        : tuiStyle.accent(text);
    return `  ${label}  ${styled}`;
  }

  invalidate(): void {
    // Stateless: reads the host on each render.
  }
}
