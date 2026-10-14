/**
 * Fixed status bar for the REPL TUI (pi-style footer).
 *
 * Two lines: workspace/session/backend, then token totals, context usage for
 * the active frame slot, and the current model (with reasoning level).
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
}

/** `~/src/project` instead of `/home/me/src/project`, and `…/gh/project` when long. */
function shortenPath(path: string, maxWidth = 28): string {
  const home = process.env.HOME;
  let shown = home && path.startsWith(home) ? `~${path.slice(home.length)}` : path;
  if (visibleWidth(shown) > maxWidth) {
    const parts = shown.split('/').filter(Boolean);
    shown = `…/${parts.slice(-2).join('/')}`;
  }
  return shown;
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
function spread(left: string[], right: string, width: number): string {
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
    const running = this.options.isRunning?.() ? 'running (type to steer)' : undefined;
    const frozen = this.options.isFrozen?.() ? 'FROZEN' : undefined;
    const harness = this.options.harness ? `harness ${this.options.harness}` : undefined;

    // Ordered by what identifies the session: the workspace basename first (short,
    // and where the harness lives), then what it is running. A long path used to
    // crowd out everything after it, so the footer said where but not what.
    const workspace = shortenPath(this.options.workspace);
    const line1 = joinFitting([
      workspace,
      running,
      frozen,
      harness,
      `backend ${this.options.backend}`,
      `session ${this.options.sessionId}`,
    ], width) || truncateToWidth(workspace, width, '…');

    const line2 = spread(
      [formatUsageSegment(snapshot), formatContextSegment(snapshot)],
      formatModelSegment(snapshot),
      width,
    );

    return [dim(line1), dim(line2)];
  }

  invalidate(): void {
    // Stateless rendering: reads the snapshot on each frame.
  }
}
