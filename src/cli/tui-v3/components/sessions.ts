/**
 * Sessions pane component — collapsible sidebar showing session list.
 * Default collapsed to icon-width (3 chars), expand to show full titles.
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';
import { style, ANSI, truncate } from '../util/ansi.js';

export interface SessionEntry {
  id: string;
  title: string;
  updatedAt?: string;
}

export class Sessions implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private requestRenderFn: RequestRender;
  private sessions: SessionEntry[] = [];
  private activeSessionId: string | null = null;
  private selectedIndex = 0;
  private collapsed = false;
  private onSelect: ((id: string) => void) | null = null;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the list of sessions. */
  setSessions(sessions: SessionEntry[]): void {
    const selectedId = this.sessions[this.selectedIndex]?.id;
    this.sessions = sessions;
    const selectedIndex = sessions.findIndex((session) => session.id === selectedId);
    const activeIndex = sessions.findIndex((session) => session.id === this.activeSessionId);
    if (selectedIndex >= 0) {
      this.selectedIndex = selectedIndex;
    } else if (activeIndex >= 0) {
      this.selectedIndex = activeIndex;
    } else {
      this.selectedIndex = Math.min(this.selectedIndex, Math.max(0, sessions.length - 1));
    }
    this.invalidate();
  }

  /** Set the active session. */
  setActiveSession(id: string | null): void {
    if (this.activeSessionId === id) return;
    this.activeSessionId = id;
    const activeIndex = this.sessions.findIndex((session) => session.id === id);
    if (activeIndex >= 0) this.selectedIndex = activeIndex;
    this.invalidate();
  }

  setOnSelect(fn: (id: string) => void): void {
    this.onSelect = fn;
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
    return this.collapsed ? 3 : 30;
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    return this.renderWithHeight(width, Number.MAX_SAFE_INTEGER);
  }

  renderWithHeight(width: number, height: number): string[] {
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
        const entryRows = this.sessions.map((session, index) => this.renderSession(session, index, width));
        const availableRows = Math.max(1, height - rows.length);
        let startIndex = 0;
        let rowsThroughSelection = entryRows
          .slice(0, this.selectedIndex + 1)
          .reduce((total, entry) => total + entry.length, 0);
        while (rowsThroughSelection > availableRows && startIndex < this.selectedIndex) {
          rowsThroughSelection -= entryRows[startIndex].length;
          startIndex++;
        }

        let usedRows = 0;
        for (let i = startIndex; i < entryRows.length; i++) {
          if (usedRows + entryRows[i].length > availableRows) break;
          rows.push(...entryRows[i]);
          usedRows += entryRows[i].length;
        }
      }
    }

    this.dirty = false;
    return rows;
  }

  handleInput(key: KeyEvent): boolean {
    if (this.sessions.length === 0) return false;

    if (key.name === 'up') {
      this.selectedIndex = Math.max(0, this.selectedIndex - 1);
      this.invalidate();
      return true;
    }
    if (key.name === 'down') {
      this.selectedIndex = Math.min(this.sessions.length - 1, this.selectedIndex + 1);
      this.invalidate();
      return true;
    }
    if (key.name === 'home') {
      this.selectedIndex = 0;
      this.invalidate();
      return true;
    }
    if (key.name === 'end') {
      this.selectedIndex = this.sessions.length - 1;
      this.invalidate();
      return true;
    }
    if (key.name === 'return') {
      const selected = this.sessions[this.selectedIndex];
      if (selected && selected.id !== this.activeSessionId) this.onSelect?.(selected.id);
      return true;
    }

    return false;
  }

  private renderSession(session: SessionEntry, index: number, width: number): string[] {
    const rows: string[] = [];
    const isActive = session.id === this.activeSessionId;
    const isSelected = index === this.selectedIndex;
    const prefix = isSelected ? '▸ ' : isActive ? '● ' : '  ';
    const line = prefix + (session.title || session.id.slice(0, 8));

    if (isSelected) {
      rows.push(style(truncate(line, width), ANSI.cyan, ANSI.bold));
    } else if (isActive) {
      rows.push(style(truncate(line, width), ANSI.green, ANSI.bold));
    } else {
      rows.push(truncate(line, width));
    }
    if (session.updatedAt) {
      rows.push(style(truncate(`  ${formatSessionDate(session.updatedAt)}`, width), ANSI.dim));
    }
    return rows;
  }
}

function formatSessionDate(value: string): string {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value.replace('T', ' ').slice(0, 16);
  return date.toLocaleString(undefined, {
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  });
}
