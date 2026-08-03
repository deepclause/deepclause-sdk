/**
 * Sessions pane component — collapsible sidebar showing session list.
 * Default collapsed to icon-width (3 chars), expand to show full titles.
 */
import type { Component, RequestRender } from '../types.js';
export interface SessionEntry {
    id: string;
    title: string;
}
export declare class Sessions implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private sessions;
    private activeSessionId;
    private collapsed;
    constructor(requestRender: RequestRender);
    /** Set the list of sessions. */
    setSessions(sessions: SessionEntry[]): void;
    /** Set the active session. */
    setActiveSession(id: string | null): void;
    /** Toggle collapsed state. */
    toggleCollapsed(): void;
    /** Get collapsed state. */
    get isCollapsed(): boolean;
    /** Get the display width. */
    get displayWidth(): number;
    invalidate(): void;
    render(width: number): string[];
}
//# sourceMappingURL=sessions.d.ts.map