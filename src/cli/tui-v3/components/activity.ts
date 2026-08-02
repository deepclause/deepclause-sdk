/**
 * Activity component — tool execution activity feed with bounded line buffer.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, truncate } from '../util/ansi.js';

const MAX_LINES = 400;

export interface ActiveTool {
  name: string;
  scope: string;
  state: 'starting' | 'running' | 'completed' | 'failed';
}

export class Activity implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private requestRenderFn: RequestRender;
  private lines: string[] = [];
  private activeTools: ActiveTool[] = [];
  private running = false;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Push a new activity line. */
  pushLine(line: string): void {
    this.lines.push(line);
    if (this.lines.length > MAX_LINES) {
      this.lines.splice(0, this.lines.length - MAX_LINES);
    }
    this.invalidate();
  }

  /** Clear all activity. */
  clear(): void {
    this.lines = [];
    this.activeTools = [];
    this.invalidate();
  }

  /** Update the list of active tools. */
  setActiveTools(tools: ActiveTool[]): void {
    this.activeTools = tools;
    this.invalidate();
  }

  /** Set running state. */
  setRunning(running: boolean): void {
    if (this.running === running) return;
    this.running = running;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const rows: string[] = [];

    // Active tools header
    if (this.activeTools.length > 0) {
      for (const tool of this.activeTools) {
        const stateIcon = tool.state === 'running' ? '⚡' : tool.state === 'starting' ? '…' : '✓';
        const line = `${stateIcon} ${tool.scope}:${tool.name}`;
        rows.push(style(truncate(line, width), ANSI.cyan));
      }
      rows.push('');
    }

    // Activity log
    for (const line of this.lines) {
      rows.push(truncate(style(line, ANSI.dim), width));
    }

    // If empty and not running, show placeholder
    if (rows.length === 0) {
      rows.push(style('  No activity yet.', ANSI.dim));
    }

    this.dirty = false;
    return rows;
  }
}
