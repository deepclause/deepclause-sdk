/**
 * Live services panel: a dock row listing the session's subagents.
 *
 * Renders nothing when hidden or when no process exists, so the dock layout
 * does not reserve a row for it (the same trick `WorkingLine` uses). Toggled
 * with `/ps --watch`; `/ps` alone prints a snapshot into the transcript.
 */

import { truncateToWidth, visibleWidth, type Component } from '@earendil-works/pi-tui';

import type { ProcessInfo, ProcessState } from '../processes.js';
import { dim, tuiStyle } from './theme.js';

export interface ServicesPanelOptions {
  getProcesses: () => ProcessInfo[];
  isVisible: () => boolean;
}

const ACTIVE: ProcessState[] = ['running', 'runnable', 'waiting', 'input'];

function stateLabel(state: ProcessState): string {
  return state === 'input' ? 'input' : state;
}

export class ServicesPanel implements Component {
  constructor(private readonly options: ServicesPanelOptions) {}

  render(width: number): string[] {
    if (!this.options.isVisible()) return [];
    const processes = this.options.getProcesses();
    if (processes.length === 0) return [];

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

    return [header, ...rows].map((line) => (visibleWidth(line) > width ? truncateToWidth(line, width, '…') : line));
  }

  invalidate(): void {
    // Stateless: reads the host on each render.
  }
}

/** The accent style is exported for callers that want to highlight the panel. */
export const servicesAccent = tuiStyle.accent;
