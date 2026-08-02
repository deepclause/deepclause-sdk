/**
 * StatusBar component — bottom bar showing mode, focused pane, and shortcuts.
 */
import type { Component, RequestRender } from '../types.js';
export declare class StatusBar implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private mode;
    private focusedPane;
    private busy;
    private followMode;
    private statusRight;
    constructor(requestRender: RequestRender);
    setMode(mode: string): void;
    setFocusedPane(pane: string): void;
    setBusy(busy: boolean): void;
    setFollowMode(follow: boolean): void;
    setStatusRight(text: string): void;
    invalidate(): void;
    render(width: number): string[];
}
//# sourceMappingURL=status-bar.d.ts.map