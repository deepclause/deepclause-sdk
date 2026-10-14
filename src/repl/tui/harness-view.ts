/**
 * The harness side view: watch the module change while the agent builds it.
 *
 * A second column beside the transcript, showing what the harness *is right now* —
 * including uncommitted edits, because it reads the session module rather than the
 * last commit. Each predicate gets a row with its size and what it is (entry point,
 * tool, documented), and anything that changed since the previous frame is marked and
 * flashes for a moment, so an edit is visible as it happens rather than only in the
 * transcript.
 *
 * It also keeps a short history of the module's size and draws it as a sparkline: a
 * harness taking shape has a shape of its own, and it is the one thing here that
 * rewards watching over time.
 */

import {
  matchesKey,
  truncateToWidth,
  visibleWidth,
  wrapTextWithAnsi,
  type Component,
  type TuiMouseEvent,
  type TuiMouseEventResult,
} from '@earendil-works/pi-tui';

import { paintToken, tuiStyle } from './theme.js';

/** One predicate as the side view shows it. */
export interface HarnessPredicate {
  indicator: string;
  name: string;
  arity: number;
  clauses: number;
  /** `entry` (agent_main), `tool`, `doc`, `touched` (changed in the open transaction). */
  flags: string[];
}

export interface HarnessSnapshot {
  predicates: HarnessPredicate[];
  /** Last committed version, when history is enabled. */
  version?: number;
  /** True while an edit transaction is open (uncommitted edits are shown). */
  transactionOpen: boolean;
  /** Predicates the open transaction has touched. */
  touched: string[];
  lastCommit?: boolean;
}

/** How long a change keeps its marker. */
const FLASH_MS = 4000;
/** Rows of history in the sparkline. */
const SPARK_POINTS = 28;
const SPARK_CHARS = ['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];

type Change = 'added' | 'changed' | 'removed';

export interface HarnessViewOptions {
  /** Injectable clock, so the flash window is testable without fake timers. */
  now?: () => number;
  /** Ask the host for a predicate's source; answered with `setSource`. */
  requestSource?: (indicator: string) => void;
  /** Called when the user leaves the view, so focus can go back to the editor. */
  onExit?: () => void;
}

export class HarnessView implements Component {
  private readonly getSnapshot: () => HarnessSnapshot | undefined;
  private readonly clock: () => number;
  /** Which predicates changed last refresh, and when, so the marker can fade. */
  private changes = new Map<string, { kind: Change; at: number }>();
  private previous = new Map<string, number>();
  private history: number[] = [];
  private rows: HarnessSnapshot['predicates'] = [];
  private removed: string[] = [];
  private lastRefresh = 0;
  private lastNote = '';
  /** Cursor and the predicate being drilled into. */
  private selected = 0;
  private openIndicator?: string;
  private sourceLines: string[] = [];
  private sourceLoading = false;
  /** Where the predicate rows start in the rendered output, for hit-testing clicks. */
  private rowsStart = 0;

  constructor(getSnapshot: () => HarnessSnapshot | undefined, options: HarnessViewOptions = {}) {
    this.getSnapshot = getSnapshot;
    this.clock = options.now ?? (() => Date.now());
    this.requestSource = options.requestSource;
    this.onExit = options.onExit;
  }

  private readonly requestSource?: (indicator: string) => void;
  private readonly onExit?: () => void;

  /** The predicate under the cursor. */
  get selectedIndicator(): string | undefined {
    return this.rows[this.selected]?.indicator;
  }

  /** The predicate currently drilled into, if any. */
  get open(): string | undefined {
    return this.openIndicator;
  }

  /** The host's answer to `requestSource`. */
  setSource(indicator: string, lines: string[]): void {
    if (indicator !== this.openIndicator) return;
    this.sourceLines = lines.map((line) => line.replace(/\s+$/, ''));
    this.sourceLoading = false;
  }

  private openPredicate(indicator: string | undefined): void {
    if (!indicator) return;
    this.openIndicator = indicator;
    this.sourceLines = [];
    this.sourceLoading = true;
    this.requestSource?.(indicator);
  }

  private closeDrilldown(): void {
    this.openIndicator = undefined;
    this.sourceLines = [];
    this.sourceLoading = false;
  }

  /** Keyboard: move the cursor, open a predicate, go back. */
  handleInput(data: string): void {
    if (matchesKey(data, 'escape') || matchesKey(data, 'left')) {
      if (this.openIndicator) {
        this.closeDrilldown();
        return;
      }
      this.onExit?.();
      return;
    }
    if (matchesKey(data, 'up') || matchesKey(data, 'k')) {
      this.moveCursor(-1);
      return;
    }
    if (matchesKey(data, 'down') || matchesKey(data, 'j')) {
      this.moveCursor(1);
      return;
    }
    if (matchesKey(data, 'enter') || matchesKey(data, 'right') || matchesKey(data, 'space')) {
      const indicator = this.selectedIndicator;
      if (this.openIndicator === indicator) this.closeDrilldown();
      else this.openPredicate(indicator);
    }
  }

  /**
   * Mouse: the alternate screen routes a click to the box under the pointer and makes
   * `y` relative to this component, so the rendered line index is the hit test.
   */
  handleMouse(event: TuiMouseEvent): TuiMouseEventResult | undefined {
    if (event.type === 'wheel') {
      this.moveCursor((event.wheelDelta ?? 0) > 0 ? 1 : -1);
      return { handled: true, render: true };
    }
    if (event.type !== 'press' && event.type !== 'click') return undefined;
    const index = event.y - this.rowsStart;
    if (index < 0 || index >= this.rows.length) {
      // Clicking the header or the sparkline drills back out.
      if (this.openIndicator) {
        this.closeDrilldown();
        return { handled: true, render: true };
      }
      return undefined;
    }
    this.selected = index;
    const indicator = this.selectedIndicator;
    if (this.openIndicator === indicator) this.closeDrilldown();
    else this.openPredicate(indicator);
    return { handled: true, focus: true, render: true };
  }

  private moveCursor(delta: number): void {
    this.selected = Math.min(Math.max(0, this.rows.length - 1), Math.max(0, this.selected + delta));
    this.clampSelectedToCursor();
  }

  private clampSelectedToCursor(): void {
    const indicator = this.selectedIndicator;
    // Moving the cursor while drilled in follows it, so the panel always describes the
    // highlighted predicate rather than the one you opened.
    if (this.openIndicator && indicator !== this.openIndicator) this.openPredicate(indicator);
  }

  /** Refresh from the session. Cheap enough to call after every tool call. */
  refresh(): void {
    const snapshot = this.getSnapshot();
    if (!snapshot) return;
    const now = this.clock();
    const next = new Map(snapshot.predicates.map((predicate) => [predicate.indicator, predicate.clauses]));

    // Keep recent markers visible across refreshes (a refresh happens per tool call,
    // so an edit that flashed for 20ms would otherwise never be seen) and record the
    // new ones.
    this.changes = new Map(
      [...this.changes].filter(([, change]) => now - change.at <= FLASH_MS),
    );
    for (const [indicator, clauses] of next) {
      const before = this.previous.get(indicator);
      if (before === undefined) this.changes.set(indicator, { kind: 'added', at: now });
      else if (before !== clauses) this.changes.set(indicator, { kind: 'changed', at: now });
    }

    this.removed = [];
    for (const indicator of this.previous.keys()) {
      if (!next.has(indicator)) {
        this.removed.push(indicator);
        this.changes.set(indicator, { kind: 'removed', at: now });
      }
    }

    const total = [...next.values()].reduce((sum, clauses) => sum + clauses, 0);
    if (this.history.at(-1) !== total) this.history.push(total);
    if (this.history.length > SPARK_POINTS) this.history.splice(0, this.history.length - SPARK_POINTS);

    if (this.changes.size > 0) {
      const [indicator, change] = [...this.changes.entries()][0] ?? [];
      if (indicator && change) {
        const verb = change.kind === 'added' ? 'added' : change.kind === 'changed' ? 'updated' : 'removed';
        this.lastNote = `${verb} ${indicator}`;
      }
    }

    this.previous = next;
    this.rows = snapshot.predicates;
    this.lastRefresh = now;
  }

  /** True while a change marker is still within its flash window. */
  private marker(indicator: string): Change | undefined {
    const change = this.changes.get(indicator);
    if (!change) return undefined;
    return this.clock() - change.at <= FLASH_MS ? change.kind : undefined;
  }

  /**
   * Rendered fresh every frame, deliberately: the output depends on the clock (a
   * marker fades), so a width-keyed cache holds stale lines exactly when the change
   * should be visible. A width-keyed cache is what froze the flash on screen.
   */
  render(width: number): string[] {
    return this.renderLines(Math.max(12, width));
  }

  private renderLines(width: number): string[] {
    const out: string[] = [];
    const snapshot = this.getSnapshot();
    const total = this.rows.reduce((sum, predicate) => sum + predicate.clauses, 0);

    // Header: the harness, its version, and whether what you see is committed.
    const version = snapshot?.version !== undefined ? ` v${snapshot.version}` : '';
    const state = snapshot?.transactionOpen ? tuiStyle.warning(' ● editing') : '';
    out.push(truncateToWidth(`${tuiStyle.title(`harness${version}`)}${state}`, width, '…'));
    out.push(tuiStyle.dim(truncateToWidth(
      `${this.rows.length} predicate${this.rows.length === 1 ? '' : 's'} · ${total} clause${total === 1 ? '' : 's'}`,
      width,
      '…',
    )));

    // The size over time: the interesting part to watch.
    out.push(this.sparkline(width));

    const rule = tuiStyle.borderMuted('─'.repeat(Math.max(0, width)));
    out.push(rule);

    if (this.openIndicator) {
      // The rows are not on screen while drilled in, so no click can land on one; a
      // click anywhere is "go back".
      this.rowsStart = Number.POSITIVE_INFINITY;
      out.push(...this.drilldown(width));
      return out;
    }

    this.rowsStart = out.length;
    if (this.rows.length === 0) {
      out.push(tuiStyle.dim('(empty — nothing defined yet)'));
    }
    this.rows.forEach((predicate, index) => {
      out.push(this.row(predicate, width, index === this.selected));
    });
    for (const indicator of this.removed) {
      const change = this.marker(indicator);
      if (!change) continue;
      out.push(truncateToWidth(`${tuiStyle.error('−')} ${tuiStyle.dim(strikethrough(indicator))}`, width, '…'));
    }
    out.push('');
    out.push(tuiStyle.dim(truncateToWidth('↑↓ move · enter open · click a row', width, '…')));

    // The most recent change, so the view says what just happened.
    if (this.lastNote && this.clock() - this.lastRefresh < FLASH_MS) {
      out.push('');
      out.push(truncateToWidth(tuiStyle.dim(this.lastNote), width, '…'));
    }
    return out;
  }

  /** The clauses of the predicate under the cursor, indented under the view's rule. */
  private drilldown(width: number): string[] {
    const out: string[] = [];
    out.push(tuiStyle.title(truncateToWidth(this.openIndicator ?? '', width, '…')));
    out.push(tuiStyle.dim(truncateToWidth('esc to go back', width, '…')));
    out.push(tuiStyle.borderMuted('─'.repeat(Math.max(0, width))));
    if (this.sourceLoading) {
      out.push(tuiStyle.dim('loading…'));
      return out;
    }
    const lines = this.sourceLines.filter((line) => line.trim() !== '');
    const budget = 40;
    for (const line of lines.slice(0, budget)) {
      // Indent the source so it reads as belonging to the drilled-in predicate, and
      // wrap rather than cut: a truncated clause body is worse than a wrapped one.
      const wrapped = wrapTextWithAnsi(line, Math.max(8, width - 2));
      for (const piece of wrapped) out.push(tuiStyle.output(` ${piece}`));
    }
    if (lines.length > budget) {
      out.push(tuiStyle.dim(` … ${lines.length - budget} more lines`));
    }
    return out;
  }

  private row(predicate: HarnessPredicate, width: number, cursor: boolean): string {
    const change = this.marker(predicate.indicator);
    const marker = change === 'added'
      ? tuiStyle.success('+')
      : change === 'changed'
        ? tuiStyle.warning('~')
        : change === 'removed'
          ? tuiStyle.error('−')
          : ' ';
    const touchable = this.touchedIndicators().has(predicate.indicator);
    const name = change ? paintToken('title', predicate.indicator, { bold: true })
      : touchable ? tuiStyle.accent(predicate.indicator)
        : predicate.indicator;
    const badges: string[] = [];
    if (predicate.flags.includes('entry')) badges.push(tuiStyle.accent('entry'));
    if (predicate.flags.includes('tool')) badges.push(tuiStyle.success('tool'));
    if (predicate.flags.includes('doc')) badges.push(tuiStyle.dim('doc'));
    badges.push(tuiStyle.dim(`${predicate.clauses}c`));

    const pointer = cursor ? tuiStyle.accent('❯') : ' ';
    const left = `${pointer}${marker} ${name}`;
    const right = badges.join(' ');
    const gap = Math.max(1, width - visibleWidth(left) - visibleWidth(right));
    return truncateToWidth(`${left}${' '.repeat(gap)}${right}`, width, '…');
  }

  private touchedIndicators(): Set<string> {
    const snapshot = this.getSnapshot();
    return new Set(snapshot?.touched ?? []);
  }

  /** A compact sparkline of the module's size, oldest to newest. */
  private sparkline(width: number): string {
    // With a single sample there is no shape to draw yet, but a row of dots reads as
    // "broken". Draw the level instead, so the row is meaningful from the first frame.
    const values = this.history.slice(-Math.max(2, Math.min(SPARK_POINTS, width)));
    const padded = this.history.length === 1 ? [values[0] ?? 0, values[0] ?? 0] : values;
    const min = Math.min(...padded);
    const max = Math.max(...padded);
    const span = max - min || 1;
    const text = padded
      .map((value) => {
        const level = Math.round(((value - min) / span) * (SPARK_CHARS.length - 1));
        return SPARK_CHARS[level] ?? '▁';
      })
      .join('');
    return truncateToWidth(paintToken('output', text), width, '…');
  }

  invalidate(): void {
    // Nothing cached by choice; see `render`.
  }
}

/** Strike-through without pulling a colour helper in for one glyph. */
function strikethrough(text: string): string {
  return `\u001b[9m${text}\u001b[0m`;
}

/** Wrap a long indicator for a narrow column, so nothing is silently cut. */
export function wrapIndicator(indicator: string, width: number): string[] {
  return visibleWidth(indicator) <= width ? [indicator] : wrapTextWithAnsi(indicator, width);
}
