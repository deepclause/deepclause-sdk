/**
 * StatusBar component — bottom bar showing mode, focused pane, and shortcuts.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, padRight } from '../util/ansi.js';

export class StatusBar implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private mode = 'normal';
  private focusedPane = 'messages';
  private busy = false;
  private followMode = true;
  private statusRight = '';

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  setMode(mode: string): void {
    if (this.mode === mode) return;
    this.mode = mode;
    this.invalidate();
  }

  setFocusedPane(pane: string): void {
    if (this.focusedPane === pane) return;
    this.focusedPane = pane;
    this.invalidate();
  }

  setBusy(busy: boolean): void {
    if (this.busy === busy) return;
    this.busy = busy;
    this.invalidate();
  }

  setFollowMode(follow: boolean): void {
    if (this.followMode === follow) return;
    this.followMode = follow;
    this.invalidate();
  }

  setStatusRight(text: string): void {
    if (this.statusRight === text) return;
    this.statusRight = text;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const parts: string[] = [];

    // Mode indicator
    const modeLabel = this.mode.toUpperCase();
    parts.push(style(` ${modeLabel} `, ANSI.bold, ANSI.inverse));

    // Focused pane
    parts.push(style(` ${this.focusedPane} `, ANSI.dim));

    // Follow indicator
    if (this.followMode) {
      parts.push(style(' ↓FOLLOW ', ANSI.cyan));
    }

    // Busy indicator
    if (this.busy) {
      parts.push(style(' RUNNING ', ANSI.yellow));
    }

    // Shortcuts hint
    const shortcuts = 'Tab:pane  Ctrl+C:quit  ?:help';
    const left = parts.join('');

    // Right side
    const right = this.statusRight || shortcuts;
    const rightStyled = style(right, ANSI.dim);

    // Compose
    const leftVisLen = modeLabel.length + 2 + this.focusedPane.length + 2
      + (this.followMode ? 9 : 0) + (this.busy ? 9 : 0);
    const rightLen = right.length;
    const gap = Math.max(1, width - leftVisLen - rightLen);

    const line = left + ' '.repeat(gap) + rightStyled;
    this.dirty = false;
    return [padRight(line, width)];
  }
}
