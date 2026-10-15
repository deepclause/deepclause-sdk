/**
 * Live services panel: a dock row listing the session's subagents.
 *
 * Renders nothing when hidden or when no process exists, so the dock layout
 * does not reserve a row for it (the same trick `WorkingLine` uses).
 *
 * Two modes, toggled with `/ps --watch` and `/ps --watch --logs`:
 *  - `summary` — one row per process with state, activity, queue and usage;
 *  - `logs`    — a larger, chronological follow of every subagent's output.
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

export class ServicesPanel implements Component {
  constructor(private readonly options: ServicesPanelOptions) {}

  render(width: number): string[] {
    if (!this.options.isVisible()) return [];
    const processes = this.options.getProcesses();
    if (this.options.mode() === 'logs') return this.renderLogs(width, processes);
    if (processes.length === 0) return [];
    return this.renderSummary(width, processes);
  }

  private renderSummary(width: number, processes: ProcessInfo[]): string[] {
    const active = processes.filter((process) => ACTIVE.includes(process.state));
    const attention = active.some((process) => process.state === 'input');
    const header = dim(`⚙ services (${processes.length})${attention ? ' — needs input' : ''}`);

    const rows = processes.map((process) => {
      const usage = process.usage.totalTokens > 0 ? ` · ${process.usage.totalTokens} tok` : '';
      const activity = process.currentActivity ? ` · ${process.currentActivity}` : '';
      const error = process.error ? ` (${process.error})` : '';
      const mail = process.mailboxDepth > 0 ? ` · ${process.mailboxDepth} queued` : '';
      const text = `  ${process.name}  ${stateLabel(process.state)}${error}${activity}${mail}${usage}`;
      const painted = ACTIVE.includes(process.state) ? text : dim(text);
      return truncateToWidth(painted, Math.max(1, width), '…');
    });

    return [header, ...rows];
  }

  private renderLogs(width: number, processes: ProcessInfo[]): string[] {
    const log = this.options.getLog();
    if (processes.length === 0 && log.length === 0) return [];

    const max = this.options.maxLogLines ?? 16;
    const slice = log.slice(-max);
    const active = processes.filter((process) => ACTIVE.includes(process.state)).length;
    const header = dim(
      `⚙ services — logs · ${processes.length} process${processes.length === 1 ? '' : 'es'}`
      + `${active > 0 ? ` · ${active} active` : ''}`,
    );

    // Pad to a stable height so the panel does not jump as output arrives.
    const rows: string[] = [];
    for (let i = slice.length; i < max; i += 1) rows.push('');
    for (const entry of slice) {
      rows.push(truncateToWidth(this.formatLogLine(entry), Math.max(1, width), '…'));
    }
    return [header, ...rows];
  }

  private formatLogLine(entry: ProcessLogEntry): string {
    const raw = entry.name.length > NAME_WIDTH
      ? `${entry.name.slice(0, NAME_WIDTH - 1)}…`
      : entry.name;
    const padding = ' '.repeat(Math.max(0, NAME_WIDTH - visibleWidth(raw)));
    const label = dim(`${raw}${padding}`);
    // A component line must not contain a newline: a multi-line answer/result
    // would otherwise break the layout.
    const text = entry.text.replace(/\s*\n\s*/g, ' ↵ ');
    const styled = entry.kind === 'output' ? text : tuiStyle.accent(text);
    return `  ${label}  ${styled}`;
  }

  invalidate(): void {
    // Stateless: reads the host on each render.
  }
}
