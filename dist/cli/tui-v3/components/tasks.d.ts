/**
 * Tasks pane component — displays a tree of execution steps with status markers.
 */
import type { Component, RequestRender } from '../types.js';
export interface TaskEntry {
    id: string;
    description: string;
    state: 'started' | 'completed' | 'failed';
    depth: number;
}
export declare class Tasks implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private tasks;
    constructor(requestRender: RequestRender);
    /** Set the task list. */
    setTasks(tasks: TaskEntry[]): void;
    /** Add a task. */
    addTask(task: TaskEntry): void;
    /** Update a task state. */
    updateTask(id: string, state: 'completed' | 'failed'): void;
    invalidate(): void;
    render(width: number): string[];
}
//# sourceMappingURL=tasks.d.ts.map