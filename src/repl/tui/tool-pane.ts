/**
 * A tool-call pane for the fullscreen TUI.
 *
 * Modelled on Pi's tool panes, but built for our tools rather than Pi's tool
 * model: `execute` (a goal and its bindings), `bash` (command, captured output,
 * exit code, duration), `listing`, `visualize`, and the DML runtime's own tools.
 *
 * Collapsed by default — a tool call is usually a one-line event, and the detail
 * is only wanted when something looks wrong. The last pane can be expanded with a
 * keybinding (see `toggleLastPane` in the renderer).
 */

import {
  Markdown,
  Text,
  TruncatedText,
  truncateToWidth,
  visibleWidth,
  wrapTextWithAnsi,
  type Component,
} from '@earendil-works/pi-tui';

import { markdownTheme, tuiStyle } from './theme.js';

export type ToolPaneState = 'running' | 'ok' | 'failed';

export interface DmlToolPaneOptions {
  toolName: string;
  args: Record<string, unknown>;
  /** Inline `execute`: show the goal, and keep the pane tight. */
  compact?: boolean;
}

/** Lines of body shown while collapsed. */
const COLLAPSED_BODY_LINES = 4;

export class DmlToolPane implements Component {
  private readonly toolName: string;
  private args: Record<string, unknown>;
  private state: ToolPaneState = 'running';
  private failure?: string;
  private result?: unknown;
  private expanded = false;
  private elapsedMs?: number;
  private readonly compact: boolean;
  private cachedWidth?: number;
  private cachedLines?: string[];

  constructor(options: DmlToolPaneOptions) {
    this.toolName = options.toolName;
    this.args = options.args;
    this.compact = options.compact ?? false;
  }

  setArgs(args: Record<string, unknown>): void {
    this.args = args;
    this.invalidateCache();
  }

  setResult(result: unknown, success: boolean, elapsedMs?: number): void {
    this.result = result;
    this.state = success ? 'ok' : 'failed';
    this.elapsedMs = elapsedMs;
    if (!success) {
      this.failure = typeof result === 'string'
        ? result
        : String((result as { error?: unknown })?.error ?? 'failed');
    }
    this.invalidateCache();
  }

  setExpanded(expanded: boolean): void {
    this.expanded = expanded;
    this.invalidateCache();
  }

  toggle(): void {
    this.setExpanded(!this.expanded);
  }

  get isExpanded(): boolean {
    return this.expanded;
  }

  /** One-line summary for a bounded list, e.g. `bash — printf hello`. */
  get summary(): string {
    const first = this.bodyLines()[0] ?? '';
    return first ? `${this.toolName} — ${first}` : this.toolName;
  }

  private invalidateCache(): void {
    this.cachedWidth = undefined;
    this.cachedLines = undefined;
  }

  invalidate(): void {
    this.invalidateCache();
  }

  /** The interesting argument, rendered as one short line. */
  private bodyLines(): string[] {
    const { args, compact } = this;
    const lines: string[] = [];
    const command = typeof args.command === 'string' ? args.command : undefined;
    const argv = Array.isArray(args.argv) ? args.argv.map(String) : undefined;
    const goal = typeof args.goal === 'string' ? args.goal : undefined;

    if (command) lines.push(command);
    else if (argv && argv.length > 0) lines.push(argv.join(' '));
    else if (goal) {
      for (const line of goal.split('\n')) lines.push(line.trim());
    } else {
      const entries = Object.entries(args).filter(([, value]) => value !== undefined && value !== null);
      if (entries.length > 0) {
        lines.push(entries.map(([key, value]) => `${key}=${shortValue(value)}`).join(' '));
      }
    }
    // A `execute` on its own is not interesting enough for a body.
    if (compact && lines.length === 0) return [];
    return lines;
  }

  /** Result text, split into lines: stdout/stderr, bindings, or an error. */
  private resultLines(): string[] {
    const out: string[] = [];
    if (this.failure) {
      out.push(this.failure);
    }
    const result = this.result as Record<string, unknown> | undefined;
    if (result && typeof result === 'object') {
      const stdout = typeof result.stdout === 'string' ? result.stdout : undefined;
      const stderr = typeof result.stderr === 'string' ? result.stderr : undefined;
      const output = typeof result.output === 'string' ? result.output : undefined;
      const error = typeof result.error === 'string' ? result.error : undefined;
      if (stdout && stdout.trim()) out.push(...stdout.trimEnd().split('\n'));
      if (stderr && stderr.trim()) out.push(...stderr.trimEnd().split('\n'));
      if (output && output.trim()) out.push(...output.trimEnd().split('\n'));
      if (error && error !== this.failure) out.push(error);
      const bindings = Array.isArray(result.bindings) ? result.bindings : undefined;
      if (bindings && bindings.length > 0) {
        out.push(bindings
          .map((binding) => {
            const { name, value } = binding as { name?: unknown; value?: unknown };
            return `${String(name ?? '?')} = ${String(value ?? '')}`;
          })
          .join('\n'));
      }
    } else if (typeof this.result === 'string' && this.result.trim()) {
      out.push(...this.result.trimEnd().split('\n'));
    }
    return out;
  }

  render(width: number): string[] {
    if (this.cachedLines && this.cachedWidth === width) return this.cachedLines;
    const lines = this.renderLines(width);
    this.cachedWidth = width;
    this.cachedLines = lines;
    return lines;
  }

  private renderLines(width: number): string[] {
    const gutter = tuiStyle.borderMuted('│ ');
    const gutterWidth = 2;
    const contentWidth = Math.max(1, width - gutterWidth);
    const glyph = this.state === 'ok' ? '✔' : this.state === 'failed' ? '✖' : '⏺';
    const glyphStyled = this.state === 'failed'
      ? tuiStyle.error(glyph)
      : this.state === 'ok'
        ? tuiStyle.success(glyph)
        : tuiStyle.accent(glyph);
    const meta: string[] = [];
    if (this.elapsedMs !== undefined) meta.push(`${this.elapsedMs}ms`);
    if (!this.expanded) meta.push('ctrl+o to expand');

    const header = `${glyphStyled} ${tuiStyle.title(this.toolName)}${meta.length ? ` ${tuiStyle.dim(`· ${meta.join(' · ')}`)}` : ''}`;
    const out: string[] = [truncateToWidth(header, width, '…')];

    // Body: the call itself.
    const body = this.bodyLines();
    const bodyBudget = this.expanded ? body.length : Math.min(body.length, COLLAPSED_BODY_LINES);
    for (const line of body.slice(0, bodyBudget)) {
      for (const wrapped of this.wrap(line, contentWidth, body.length > 1 ? '  ' : '')) {
        out.push(`${gutter}${truncateToWidth(wrapped, contentWidth, '…')}`);
      }
    }

    // Result: markdown when expanded (a model answer or a report), bounded otherwise.
    const result = this.resultLines();
    if (result.length > 0) {
      const shown = this.expanded ? result : result.slice(0, COLLAPSED_BODY_LINES);
      const omitted = result.length - shown.length;
      if (this.expanded) {
        const markdown = new Markdown(shown.join('\n'), 0, 0, markdownTheme());
        for (const line of markdown.render(contentWidth)) {
          out.push(line === '' ? gutter.trimEnd() : `${gutter}${line}`);
        }
      } else {
        for (const line of shown) {
          const styled = this.state === 'failed' ? tuiStyle.error(line) : line;
          for (const wrapped of this.wrap(styled, contentWidth, '')) {
            out.push(`${gutter}${truncateToWidth(wrapped, contentWidth, '…')}`);
          }
        }
      }
      if (omitted > 0) {
        out.push(`${gutter}${tuiStyle.dim(`… ${omitted} more line${omitted === 1 ? '' : 's'}`)}`);
      }
    }

    // Every line must fit the viewport; a pane is a child of the transcript.
    return out.map((line) => (visibleWidth(line) <= width ? line : truncateToWidth(line, width, '…')));
  }

  /** Wrap a styled string to `width`, prefixing continuation lines. */
  private wrap(text: string, width: number, continuationPrefix: string): string[] {
    if (visibleWidth(text) <= width) return [text];
    const wrapped = wrapTextWithAnsi(text, Math.max(1, width - visibleWidth(continuationPrefix)));
    return wrapped.map((line, index) => (index === 0 ? line : `${continuationPrefix}${line}`));
  }
}

/** Bound a value for a header line. */
function shortValue(value: unknown): string {
  const text = typeof value === 'string'
    ? value
    : (() => {
      try {
        return JSON.stringify(value);
      } catch {
        return String(value);
      }
    })();
  return text.length > 60 ? `${text.slice(0, 57)}…` : text;
}

/** A plain text block component, so callers can append text as a component. */
export class TextBlock implements Component {
  private readonly text: Text;

  constructor(value: string, paddingX = 0) {
    this.text = new Text(value, paddingX, 0);
  }

  setText(value: string): void {
    this.text.setText(value);
  }

  render(width: number): string[] {
    return this.text.render(width);
  }

  invalidate(): void {
    this.text.invalidate();
  }
}

/** A bounded single-line text component (for long binding values). */
export function truncated(value: string): Component {
  return new TruncatedText(value, 0, 0);
}
