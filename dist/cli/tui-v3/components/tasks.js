/**
 * Tasks pane component — displays a tree of execution steps with status markers.
 */
import { style, ANSI, truncate } from '../util/ansi.js';
const STATE_MARKERS = {
    started: { char: '▸', color: ANSI.yellow },
    completed: { char: '✓', color: ANSI.green },
    failed: { char: '✗', color: ANSI.red },
};
export class Tasks {
    dirty = true;
    minHeight = 1;
    flexGrow = 1;
    requestRenderFn;
    tasks = [];
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    /** Set the task list. */
    setTasks(tasks) {
        this.tasks = tasks;
        this.invalidate();
    }
    /** Add a task. */
    addTask(task) {
        this.tasks.push(task);
        this.invalidate();
    }
    /** Update a task state. */
    updateTask(id, state) {
        const t = this.tasks.find((x) => x.id === id);
        if (t) {
            t.state = state;
            this.invalidate();
        }
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        const rows = [];
        rows.push(style('Steps', ANSI.bold));
        if (this.tasks.length === 0) {
            rows.push(style('  No steps yet.', ANSI.dim));
        }
        else {
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
//# sourceMappingURL=tasks.js.map