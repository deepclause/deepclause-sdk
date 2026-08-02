/**
 * Header component — top bar showing session title, status, and spinner.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, padRight } from '../util/ansi.js';

const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
const SPINNER_INTERVAL_MS = 80;

export class Header implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private title = 'DeepClause';
  private busy = false;
  private spinnerFrame = 0;
  private spinnerTimer: ReturnType<typeof setInterval> | null = null;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  setTitle(title: string): void {
    if (this.title === title) return;
    this.title = title;
    this.invalidate();
  }

  setBusy(busy: boolean): void {
    if (this.busy === busy) return;
    this.busy = busy;
    if (busy && !this.spinnerTimer) {
      this.spinnerTimer = setInterval(() => {
        this.spinnerFrame = (this.spinnerFrame + 1) % SPINNER_FRAMES.length;
        this.invalidate();
      }, SPINNER_INTERVAL_MS);
    } else if (!busy && this.spinnerTimer) {
      clearInterval(this.spinnerTimer);
      this.spinnerTimer = null;
    }
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const spinner = this.busy ? SPINNER_FRAMES[this.spinnerFrame] + ' ' : '';
    const titleText = `${spinner}${this.title}`;
    const right = 'DeepClause TUI';
    const left = style(titleText, ANSI.bold);
    const rightStyled = style(right, ANSI.dim);

    // Compose: [left ... right]
    const leftLen = spinner.length + this.title.length;
    const rightLen = right.length;
    const gap = Math.max(1, width - leftLen - rightLen);

    const line = left + ' '.repeat(gap) + rightStyled;
    this.dirty = false;
    return [padRight(line, width)];
  }

  dispose(): void {
    if (this.spinnerTimer) {
      clearInterval(this.spinnerTimer);
      this.spinnerTimer = null;
    }
  }
}
