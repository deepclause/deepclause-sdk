/**
 * StatusBar component — Borland-style bottom bar with pane shortcuts and status.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, padRight } from '../util/ansi.js';
import type { PaneVisibility } from '../state/app-state.js';

export class StatusBar implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private mode = 'normal';
  private focusedPane = 'messages';
  private busy = false;
  private followMode = true;
  private paneVisibility: PaneVisibility = {
    sessions: true,
    messages: true,
    context: true,
  };

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

  setPaneVisibility(vis: PaneVisibility): void {
    this.paneVisibility = vis;
    this.invalidate();
  }

  setStatusRight(_text: string): void {
    // Kept for API compat
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    // Borland-style: function key shortcuts on the bottom
    const shortcuts: Array<{ key: string; label: string; pane?: keyof PaneVisibility }> = [
      { key: 'F1', label: 'Help' },
      { key: 'F2', label: 'Sess', pane: 'sessions' },
      { key: 'F3', label: 'Ctx', pane: 'context' },
      { key: '^E', label: 'Inspect' },
      { key: 'Tab', label: 'Next' },
      { key: '^C', label: this.busy ? 'Cancel' : 'Quit' },
    ];

    let line = '';
    let visLen = 0;

    for (const sc of shortcuts) {
      const hidden = sc.pane && !this.paneVisibility[sc.pane];
      const keyPart = hidden
        ? style(sc.key, ANSI.dim)
        : style(sc.key, ANSI.brightWhite, ANSI.bgBlue);
      const labelPart = hidden
        ? style(sc.label, ANSI.dim)
        : style(sc.label, ANSI.black, ANSI.bgCyan);
      line += keyPart + labelPart + ' ';
      visLen += sc.key.length + sc.label.length + 1;
    }

    // Right side: focused pane + status
    const rightParts: string[] = [];
    if (this.busy) {
      rightParts.push(style(' RUNNING ', ANSI.yellow, ANSI.bold));
    }
    if (this.followMode) {
      rightParts.push(style('↓', ANSI.cyan));
    }
    rightParts.push(style(` ${this.focusedPane} `, ANSI.inverse));

    const right = rightParts.join('');
    const rightVisLen = (this.busy ? 9 : 0) + (this.followMode ? 1 : 0) + this.focusedPane.length + 2;

    const gap = Math.max(0, width - visLen - rightVisLen);
    const fullLine = line + ' '.repeat(gap) + right;

    this.dirty = false;
    return [padRight(fullLine, width)];
  }
}
