/**
 * Fixed status bar for the REPL TUI (pi-style footer).
 *
 * One row: where the harness lives and what it is doing on the left, the model on
 * the right, and the token/context counters in between. Anything that does not fit
 * is dropped whole, in priority order (see `render`), so a narrow terminal loses
 * detail instead of showing truncated fragments.
 */

import { truncateToWidth, visibleWidth, type Component } from '@earendil-works/pi-tui';

import {
  formatContextSegment,
  formatModelSegment,
  formatUsageSegment,
  type ReplStatusSnapshot,
} from '../status.js';
import { dim } from './theme.js';

export interface StatusBarOptions {
  getSnapshot: () => ReplStatusSnapshot;
  workspace: string;
  sessionId: string;
  /** Harness this session edits. */
  harness?: string;
  backend: string;
  isFrozen?: () => boolean;
  /** True while a turn is running (so the user knows they can steer). */
  isRunning?: () => boolean;
  /** Compact subagent status, e.g. `⚙ desk 2/3`; omitted when none run. */
  processLabel?: () => string | undefined;
}

/**
 * The workspace, as short as it can be while still identifying it.
 *
 * A single row cannot afford `/home/me/git/org/project` — it takes half the line
 * and pushes the model and counters off it — so this falls back to the parent and
 * name (`…/org/project`) and then to the name alone.
 */
function shortenPath(path: string, maxWidth = 24): string {
  const home = process.env.HOME;
  const shown = home && path.startsWith(home) ? `~${path.slice(home.length)}` : path;
  if (visibleWidth(shown) <= maxWidth) return shown;
  const parts = shown.split('/').filter(Boolean);
  const parentAndName = `…/${parts.slice(-2).join('/')}`;
  if (visibleWidth(parentAndName) <= maxWidth) return parentAndName;
  return `…/${parts[parts.length - 1] ?? shown}`;
}

/**
 * Join the segments that fit, in the order given, as whole segments.
 *
 * The old code truncated the last segment to fit, which is how a footer ended up
 * reading `deepseek-flash · ` — a separator with nothing after it, and no way to
 * tell a short value from a cut one. Skipping a long segment (a full path) also
 * lets the short, more useful ones after it through.
 */
function joinFitting(segments: Array<string | undefined>, width: number, separator = ' · '): string {
  const present = segments.filter((segment): segment is string => Boolean(segment));
  const kept: string[] = [];
  let used = 0;
  for (const segment of present) {
    const cost = (kept.length > 0 ? visibleWidth(separator) : 0) + visibleWidth(segment);
    if (used + cost > width) continue;
    kept.push(segment);
    used += cost;
  }
  // Nothing fit: say so with an empty string. The caller decides what to truncate —
  // this never cuts a segment in half, which is what produced `deepseek-flash · `.
  return kept.join(separator);
}

/**
 * Left content padded, with `right` against the right edge.
 *
 * The counters are dropped whole before the model is touched: a run is identified
 * by its model, and a half-printed counter is worse than a missing one.
 */
function spread(left: Array<string | undefined>, right: string, width: number): string {
  const gap = 2;
  const rightWidth = visibleWidth(right);
  if (rightWidth >= width) return truncateToWidth(right, width, '…');
  const joined = joinFitting(left, Math.max(0, width - rightWidth - gap), '  ');
  if (joined) {
    return joined + ' '.repeat(width - visibleWidth(joined) - rightWidth) + right;
  }
  return ' '.repeat(width - rightWidth) + right;
}

export class StatusBarComponent implements Component {
  constructor(private readonly options: StatusBarOptions) {}

  render(width: number): string[] {
    const snapshot = this.options.getSnapshot();
    // The long steering hint only on a wide terminal; `running` alone still says
    // what matters, and the dock's loader shows that work is happening.
    const running = this.options.isRunning?.()
      ? (width >= 100 ? 'running (type to steer)' : 'running')
      : undefined;
    const frozen = this.options.isFrozen?.() ? 'FROZEN' : undefined;
    const harness = this.options.harness ? `harness ${this.options.harness}` : undefined;
    const processes = this.options.processLabel?.();

    // Priority order, most useful first: where we are, what state we are in, what
    // it has cost, what we are pointed at. The session id is printed by the exit
    // footer, and the backend is only interesting when debugging, so they go last.
    const line = spread(
      [
        shortenPath(this.options.workspace),
        running,
        processes,
        frozen,
        formatUsageSegment(snapshot),
        formatContextSegment(snapshot),
        harness,
        `backend ${this.options.backend}`,
        `session ${this.options.sessionId}`,
      ],
      formatModelSegment(snapshot),
      width,
    );

    return [dim(line)];
  }

  invalidate(): void {
    // Stateless rendering: reads the snapshot on each frame.
  }
}
