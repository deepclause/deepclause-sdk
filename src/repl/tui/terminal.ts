/**
 * A terminal that knows how tall it really is.
 *
 * pi-tui asks the stream: `rows` is `process.stdout.rows || LINES || 24`. When the
 * stream does not report a size — an unsized pty, a multiplexer or embedded
 * terminal that never sends SIGWINCH, a wrapper that resizes the pty after the
 * child starts — `rows` is 0 and the fallback is a **hard-coded 24**. The TUI then
 * draws a 24-row frame inside a taller terminal, so the UI occupies the top of the
 * screen and leaves the rest untouched, which reads as "it does not use the full
 * height".
 *
 * `refreshTerminalDimensions()` (which pi-tui calls on start) only re-raises
 * SIGWINCH at itself, so it cannot fix a stream that has no size to report.
 * `stty size` asks the tty directly, which is the same source `tput`/`$LINES`
 * use, and is the fallback here.
 */

import { execFileSync } from 'child_process';

import { ProcessTerminal } from '@earendil-works/pi-tui';

interface Size {
  rows: number;
  columns: number;
}

/**
 * A usable dimension, or undefined.
 *
 * `Number(process.env.LINES)` is NaN when the variable is unset, and `??` does not
 * skip NaN — chaining it directly produced a NaN height, and a NaN viewport renders
 * nothing at all.
 */
function positive(value: number | undefined): number | undefined {
  return typeof value === 'number' && Number.isFinite(value) && value > 0
    ? Math.floor(value)
    : undefined;
}

/** Ask the controlling tty for its size. Returns undefined when it has none. */
function queryTtySize(): Size | undefined {
  if (process.platform === 'win32') return undefined;
  try {
    // stdin must be the tty: `inherit` gives stty the real one, and stderr is
    // dropped so a shell that cannot report a size is silent.
    const output = execFileSync('stty', ['size'], {
      stdio: ['inherit', 'pipe', 'ignore'],
      encoding: 'utf8',
    });
    const match = /^\s*(\d+)\s+(\d+)\s*$/.exec(output);
    if (!match) return undefined;
    const rows = Number(match[1]);
    const columns = Number(match[2]);
    if (rows > 0 && columns > 0) return { rows, columns };
  } catch {
    // No tty, no stty, or not permitted: the parent's answer is the best available.
  }
  return undefined;
}

export class SizedTerminal extends ProcessTerminal {
  private fallback?: Size;
  private queried = false;

  /** Re-read the size (call on resize, where the stream may still not report one). */
  refreshSize(): void {
    this.queried = false;
    this.fallback = undefined;
  }

  private streamSize(): Partial<Size> {
    // The stream is authoritative when it answers: it reflects SIGWINCH.
    return {
      rows: process.stdout.rows || undefined,
      columns: process.stdout.columns || undefined,
    };
  }

  private ttySize(): Size | undefined {
    if (!this.queried) {
      this.queried = true;
      const stream = this.streamSize();
      // Only bother when the stream cannot be trusted for either dimension.
      this.fallback = stream.rows && stream.columns ? undefined : queryTtySize();
    }
    return this.fallback;
  }

  override get rows(): number {
    const stream = this.streamSize();
    return positive(stream.rows)
      ?? positive(this.ttySize()?.rows)
      ?? positive(Number(process.env.LINES))
      ?? 24;
  }

  override get columns(): number {
    const stream = this.streamSize();
    return positive(stream.columns)
      ?? positive(this.ttySize()?.columns)
      ?? positive(Number(process.env.COLUMNS))
      ?? 80;
  }
}
