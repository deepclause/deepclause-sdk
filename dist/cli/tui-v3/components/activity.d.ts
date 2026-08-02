/**
 * Activity component — tool execution activity feed with bounded line buffer.
 */
import type { Component, RequestRender } from '../types.js';
export interface ActiveTool {
    name: string;
    scope: string;
    state: 'starting' | 'running' | 'completed' | 'failed';
}
export declare class Activity implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private lines;
    private activeTools;
    private running;
    constructor(requestRender: RequestRender);
    /** Push a new activity line. */
    pushLine(line: string): void;
    /** Clear all activity. */
    clear(): void;
    /** Update the list of active tools. */
    setActiveTools(tools: ActiveTool[]): void;
    /** Set running state. */
    setRunning(running: boolean): void;
    invalidate(): void;
    render(width: number): string[];
}
//# sourceMappingURL=activity.d.ts.map