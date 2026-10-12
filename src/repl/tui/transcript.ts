/**
 * TUI transcript: a bounded, append-only view of rendered REPL output.
 *
 * The existing `ReplRenderer` (rich mode) writes its output through the
 * `LineSink` into this component, so all of its formatting — tool panels,
 * wrapped bash bodies, streamed text, compaction notices — is reused unchanged.
 * User messages are stored with a style and rendered as pi-style blue blocks.
 */

import { visibleWidth, wrapTextWithAnsi, type Component } from '@earendil-works/pi-tui';

import { renderSteerLine, renderUserLine, STEER_LABEL } from './theme.js';

export type TranscriptLineStyle = 'user' | 'steer';

interface TranscriptEntry {
  text: string;
  style?: TranscriptLineStyle;
  /** Raw terminal-image line: never wrapped or styled. */
  image?: boolean;
}

/** A `Component` holding committed entries plus one live (unterminated) line. */
export class TranscriptComponent implements Component {
  private entries: TranscriptEntry[] = [];
  private pending = '';
  private readonly maxLines: number;
  /** Called whenever content changes so the TUI can schedule a render. */
  onChange?: () => void;

  constructor(maxLines = 2000) {
    this.maxLines = maxLines;
  }

  append(line: string, style?: TranscriptLineStyle): void {
    // Collapse runs of blank lines: block separators and user-message spacing
    // can otherwise stack up.
    if (line === '' && this.entries.length > 0 && this.entries[this.entries.length - 1].text === '') {
      return;
    }
    this.entries.push({ text: line, style });
    if (this.entries.length > this.maxLines) {
      this.entries.splice(0, this.entries.length - this.maxLines);
    }
    this.onChange?.();
  }

  setPending(line: string): void {
    this.pending = line;
    this.onChange?.();
  }

  /** Append a terminal-image block (escape sequence plus reserved blank rows). */
  appendImageLines(lines: string[]): void {
    for (const line of lines) {
      this.entries.push({ text: line, image: true });
    }
    if (this.entries.length > this.maxLines) {
      this.entries.splice(0, this.entries.length - this.maxLines);
    }
    this.onChange?.();
  }

  getLines(): string[] {
    return this.entries.map((entry) => entry.text);
  }

  clear(): void {
    this.entries = [];
    this.pending = '';
    this.onChange?.();
  }

  render(width: number): string[] {
    const source: TranscriptEntry[] = this.pending
      ? [...this.entries, { text: this.pending }]
      : this.entries;
    if (width <= 0) {
      return source.map((entry) => entry.text);
    }
    // The renderer wraps to the width captured when it was built; the terminal
    // may resize, and streamed text is not pre-wrapped. Wrapping here guarantees
    // every line fits, which `TuiMainScreen` requires.
    const out: string[] = [];
    for (const entry of source) {
      if (entry.image) {
        // Image lines carry an escape sequence (or a cursor-move prefix) and
        // must not be wrapped or measured.
        out.push(entry.text);
        continue;
      }
      // Styled blocks reserve room for their label/padding.
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
    // Stateless rendering: nothing cached.
  }
}

/**
 * A write sink that translates the renderer's stream (which uses `\r` and
 * `\n` for in-place updates) into committed transcript lines plus one live line.
 *
 * ANSI color codes are preserved; the `ESC[K` "clear to end of line" sequence
 * is dropped because the pending-line model already replaces the line.
 */
export class LineSink {
  private current = '';

  constructor(private readonly transcript: TranscriptComponent) {}

  write(chunk: string): void {
    let index = 0;
    while (index < chunk.length) {
      const char = chunk[index];
      if (char === '\r') {
        this.current = '';
        index += 1;
        continue;
      }
      if (char === '\n') {
        this.transcript.append(this.current);
        this.current = '';
        index += 1;
        continue;
      }
      // Drop "erase in line" (ESC[K) and "erase in display" (ESC[J) requests.
      if (char === '\u001b' && chunk[index + 1] === '[') {
        const match = /^\u001b\[[0-9;]*[JK]/.exec(chunk.slice(index));
        if (match) {
          index += match[0].length;
          continue;
        }
      }
      this.current += char;
      index += 1;
    }
    this.transcript.setPending(this.current);
  }
}
