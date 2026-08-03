/**
 * Application state machine for TUI v3.
 *
 * Simple reducer-based state management, decoupled from React.
 * State changes trigger invalidation of relevant components.
 */
export type PaneKind = 'sessions' | 'messages' | 'context';
export type UiMode = 'normal' | 'command' | 'picker' | 'help';
export type OverlayKind = 'none' | 'picker' | 'help' | 'confirm';
export interface PaneVisibility {
    sessions: boolean;
    messages: boolean;
    context: boolean;
}
export interface AppState {
    focusedPane: PaneKind;
    mode: UiMode;
    sessionPaneCollapsed: boolean;
    paneVisibility: PaneVisibility;
    overlay: OverlayKind;
    columns: number;
    rows: number;
    busy: boolean;
    autoScroll: boolean;
}
export type AppAction = {
    type: 'SET_FOCUSED_PANE';
    pane: PaneKind;
} | {
    type: 'CYCLE_PANE';
} | {
    type: 'SET_MODE';
    mode: UiMode;
} | {
    type: 'TOGGLE_SESSION_PANE';
} | {
    type: 'TOGGLE_PANE_VISIBILITY';
    pane: PaneKind;
} | {
    type: 'SET_OVERLAY';
    overlay: OverlayKind;
} | {
    type: 'RESIZE';
    columns: number;
    rows: number;
} | {
    type: 'SET_BUSY';
    busy: boolean;
} | {
    type: 'TOGGLE_AUTO_SCROLL';
};
export declare function createInitialAppState(): AppState;
export declare function appReducer(state: AppState, action: AppAction): AppState;
//# sourceMappingURL=app-state.d.ts.map