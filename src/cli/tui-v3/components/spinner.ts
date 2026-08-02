/**
 * Spinner component — animated loading indicator.
 * Uses invalidation-based rendering with a timer only when active.
 */

import type { Component, RequestRender } from '../types.js';

const FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
const FRAME_INTERVAL_MS = 80;

export class Spinner implements Component {
  dirty = true;
  minHeight = 0;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private frameIndex = 0;
  private timer: ReturnType<typeof setInterval> | null = null;
  private _active = false;
  private label = '';

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Start the spinner animation. */
  start(label = ''): void {
    if (this._active) return;
    this._active = true;
    this.label = label;
    this.frameIndex = 0;
    this.timer = setInterval(() => {
      this.frameIndex = (this.frameIndex + 1) % FRAMES.length;
      this.invalidate();
    }, FRAME_INTERVAL_MS);
    this.invalidate();
  }

  /** Stop the spinner animation. */
  stop(): void {
    if (!this._active) return;
    this._active = false;
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = null;
    }
    this.invalidate();
  }

  /** Update the label text. */
  setLabel(label: string): void {
    this.label = label;
    this.invalidate();
  }

  get active(): boolean {
    return this._active;
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(_width: number): string[] {
    if (!this._active) {
      this.dirty = false;
      return [];
    }
    const frame = FRAMES[this.frameIndex];
    this.dirty = false;
    return [this.label ? `${frame} ${this.label}` : frame];
  }

  /** Cleanup: stop any timers. */
  dispose(): void {
    this.stop();
  }
}
