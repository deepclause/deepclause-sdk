/**
 * StatusBar component — Borland-style bottom bar with pane shortcuts and status.
 */
import type { Component, RequestRender } from '../types.js';
import type { PaneVisibility } from '../state/app-state.js';
export declare class StatusBar implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private mode;
    private focusedPane;
    private busy;
    private followMode;
    private paneVisibility;
    constructor(requestRender: RequestRender);
    setMode(mode: string): void;
    setFocusedPane(pane: string): void;
    setBusy(busy: boolean): void;
    setFollowMode(follow: boolean): void;
    setPaneVisibility(vis: PaneVisibility): void;
    setStatusRight(_text: string): void;
    invalidate(): void;
    render(width: number): string[];
}
//# sourceMappingURL=status-bar.d.ts.map