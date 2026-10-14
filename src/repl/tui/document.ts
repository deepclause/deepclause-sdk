/**
 * Component transcript for the fullscreen TUI.
 *
 * The plain/readline path keeps the line-based `TranscriptComponent`; the TUI
 * needs a tree, because a tool call, a streamed answer and a code fence have to
 * be *components* (so markdown can be re-rendered as text arrives, a tool pane can
 * expand, and `ScrollView` can scroll a bounded document).
 *
 * Entries are either styled text lines (rendered at the current width, so a
 * resize re-wraps them) or components rendered as-is.
 */

import {
  visibleWidth,
  wrapTextWithAnsi,
  type Component,
} from '@earendil-works/pi-tui';

import { renderSteerLine, renderUserLine, STEER_LABEL } from './theme.js';

export type DocumentLineStyle = 'user' | 'steer';

interface TextEntry {
  kind: 'text';
  text: string;
  style?: DocumentLineStyle;
}

interface ComponentEntry {
  kind: 'component';
  component: Component;
}

interface ImageEntry {
  kind: 'image';
  line: string;
}

type Entry = TextEntry | ComponentEntry | ImageEntry;

/** A bounded component document: append-only, with one live (unterminated) line. */
export class TranscriptDocument implements Component {
  private entries: Entry[] = [];
  private pending = '';
  private pendingStyle?: DocumentLineStyle;
  private readonly maxEntries: number;
  /** Called whenever content changes, so the TUI can schedule a render. */
  onChange?: () => void;

  constructor(maxEntries = 2000) {
    this.maxEntries = maxEntries;
  }

  private trim(): void {
    if (this.entries.length > this.maxEntries) {
      this.entries.splice(0, this.entries.length - this.maxEntries);
    }
  }

  /** Append one styled line. Consecutive blank lines collapse. */
  appendText(line: string, style?: DocumentLineStyle): void {
    const last = this.entries[this.entries.length - 1];
    if (line === '' && last?.kind === 'text' && last.text === '') {
      return;
    }
    this.entries.push({ kind: 'text', text: line, style });
    this.trim();
    this.onChange?.();
  }

  /** Alias for `appendText`, matching the line-based transcript's API. */
  append(line: string, style?: DocumentLineStyle): void {
    this.appendText(line, style);
  }

  /** Append a component (a tool pane, markdown block, status notice). */
  appendComponent(component: Component): void {
    this.entries.push({ kind: 'component', component });
    this.trim();
    this.onChange?.();
  }

  /** Append terminal-image lines verbatim: they must not be wrapped or measured. */
  appendImageLines(lines: string[]): void {
    for (const line of lines) {
      this.entries.push({ kind: 'image', line });
    }
    this.trim();
    this.onChange?.();
  }

  /** The live (not yet newline-terminated) line, as the line sink writes it. */
  setPending(line: string, style?: DocumentLineStyle): void {
    this.pending = line;
    this.pendingStyle = style;
    this.onChange?.();
  }

  /** Whether anything has been written (used to separate user turns). */
  isEmpty(): boolean {
    return this.entries.length === 0 && this.pending === '';
  }

  /**
   * One string per entry. Kept for callers that only ask "is there output yet";
   * the rendered text of a component is not available without a width.
   */
  getLines(): string[] {
    return this.entries.map((entry) =>
      entry.kind === 'text' ? entry.text : entry.kind === 'image' ? entry.line : '');
  }

  clear(): void {
    this.entries = [];
    this.pending = '';
    this.onChange?.();
  }

  render(width: number): string[] {
    const source: Entry[] = this.pending
      ? [...this.entries, { kind: 'text', text: this.pending, style: this.pendingStyle }]
      : this.entries;

    const out: string[] = [];
    for (const entry of source) {
      if (entry.kind === 'image') {
        out.push(entry.line);
        continue;
      }
      if (entry.kind === 'component') {
        out.push(...entry.component.render(width));
        continue;
      }
      if (width <= 0) {
        out.push(entry.text);
        continue;
      }
      // Styled blocks reserve room for their label/padding; everything else is
      // wrapped to the full width so no line exceeds it.
      const wrapWidth = entry.style === 'user'
        ? Math.max(1, width - 2)
        : entry.style === 'steer'
          ? Math.max(1, width - STEER_LABEL.length)
          : width;
      const wrapped = visibleWidth(entry.text) <= wrapWidth
        ? [entry.text]
        : wrapTextWithAnsi(entry.text, wrapWidth);
      wrapped.forEach((line, index) => {
        if (entry.style === 'user') out.push(renderUserLine(line, width));
        else if (entry.style === 'steer') out.push(renderSteerLine(line, width, index === 0));
        else out.push(line);
      });
    }
    return out;
  }

  invalidate(): void {
    for (const entry of this.entries) {
      if (entry.kind === 'component') entry.component.invalidate?.();
    }
  }
}
