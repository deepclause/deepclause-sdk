/**
 * Tasks pane component — displays a tree of execution steps with status markers.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, truncate } from '../util/ansi.js';

export interface TaskEntry {
  id: string;
  description: string;
  state: 'started' | 'completed' | 'failed';
  depth: number;
}

const STATE_MARKERS: Record<string, { char: string; color: string }> = {
  started: { char: '▸', color: ANSI.yellow },
  completed: { char: '✓', color: ANSI.green },
  failed: { char: '✗', color: ANSI.red },
};

export class Tasks implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private requestRenderFn: RequestRender;
  private tasks: TaskEntry[] = [];

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the task list. */
  setTasks(tasks: TaskEntry[]): void {
    this.tasks = tasks;
    this.invalidate();
  }

  /** Add a task. */
  addTask(task: TaskEntry): void {
    this.tasks.push(task);
    this.invalidate();
  }

  /** Update a task state. */
  updateTask(id: string, state: 'completed' | 'failed'): void {
    const t = this.tasks.find((x) => x.id === id);
    if (t) {
      t.state = state;
      this.invalidate();
    }
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const rows: string[] = [];

    rows.push(style('Steps', ANSI.bold));

    if (this.tasks.length === 0) {
      rows.push(style('  No steps yet.', ANSI.dim));
    } else {
      for (const task of this.tasks) {
        const marker = STATE_MARKERS[task.state] ?? STATE_MARKERS.started;
        const indent = '  '.repeat(task.depth);
        const markerStr = style(marker.char, marker.color);
        const desc = task.state === 'completed'
          ? style(task.description, ANSI.dim)
          : task.description;
        const line = `${indent}${markerStr} ${desc}`;
        rows.push(truncate(line, width));
      }
    }

    this.dirty = false;
    return rows;
  }
}
