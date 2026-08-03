/**
 * Sessions pane component — collapsible sidebar showing session list.
 * Default collapsed to icon-width (3 chars), expand to show full titles.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, truncate } from '../util/ansi.js';

export interface SessionEntry {
  id: string;
  title: string;
}

export class Sessions implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private requestRenderFn: RequestRender;
  private sessions: SessionEntry[] = [];
  private activeSessionId: string | null = null;
  private collapsed = true;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the list of sessions. */
  setSessions(sessions: SessionEntry[]): void {
    this.sessions = sessions;
    this.invalidate();
  }

  /** Set the active session. */
  setActiveSession(id: string | null): void {
    if (this.activeSessionId === id) return;
    this.activeSessionId = id;
    this.invalidate();
  }

  /** Toggle collapsed state. */
  toggleCollapsed(): void {
    this.collapsed = !this.collapsed;
    this.invalidate();
  }

  /** Get collapsed state. */
  get isCollapsed(): boolean {
    return this.collapsed;
  }

  /** Get the display width. */
  get displayWidth(): number {
    return this.collapsed ? 3 : 24;
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const rows: string[] = [];

    if (this.collapsed) {
      // Collapsed: just show "S" header and numbered indicators
      rows.push(style('S', ANSI.cyan, ANSI.bold));
      for (let i = 0; i < this.sessions.length; i++) {
        const s = this.sessions[i];
        const color = s.id === this.activeSessionId ? ANSI.green : ANSI.dim;
        rows.push(style(String(i + 1), color));
      }
    } else {
      // Expanded: show full session list
      rows.push(style('Sessions', ANSI.bold));
      rows.push(style('─'.repeat(Math.min(width, 22)), ANSI.dim));

      if (this.sessions.length === 0) {
        rows.push(style('  No sessions', ANSI.dim));
      } else {
        for (const s of this.sessions) {
          const isActive = s.id === this.activeSessionId;
          const prefix = isActive ? '▸ ' : '  ';
          const label = s.title || s.id.slice(0, 8);
          const line = prefix + label;

          if (isActive) {
            rows.push(style(truncate(line, width), ANSI.green, ANSI.bold));
          } else {
            rows.push(truncate(line, width));
          }
        }
      }
    }

    this.dirty = false;
    return rows;
  }
}
