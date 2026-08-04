/**
 * Activity component — tool execution activity feed with bounded line buffer.
 */
import { style, ANSI, truncate } from '../util/ansi.js';
const MAX_LINES = 400;
export class Activity {
    dirty = true;
    minHeight = 1;
    flexGrow = 1;
    requestRenderFn;
    lines = [];
    activeTools = [];
    running = false;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    /** Push a new activity line. */
    pushLine(line) {
        this.lines.push(line);
        if (this.lines.length > MAX_LINES) {
            this.lines.splice(0, this.lines.length - MAX_LINES);
        }
        this.invalidate();
    }
    /** Replace the activity log with state-managed lines. */
    setLines(lines) {
        this.lines = lines.slice(-MAX_LINES);
        this.invalidate();
    }
    /** Clear all activity. */
    clear() {
        this.lines = [];
        this.activeTools = [];
        this.invalidate();
    }
    /** Update the list of active tools. */
    setActiveTools(tools) {
        this.activeTools = tools;
        this.invalidate();
    }
    /** Set running state. */
    setRunning(running) {
        if (this.running === running)
            return;
        this.running = running;
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        const rows = [];
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
        // If empty, distinguish an idle session from a run awaiting its first event.
        if (rows.length === 0) {
            rows.push(style(this.running ? '  Waiting for activity…' : '  No activity yet.', ANSI.dim));
        }
        this.dirty = false;
        return rows;
    }
}
//# sourceMappingURL=activity.js.map