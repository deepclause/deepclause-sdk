/**
 * Global application state for the Ink TUI.
 * Uses a simple event-emitter pattern for state updates,
 * consumed via React hooks.
 */
export type PaneKind = 'sessions' | 'messages' | 'process' | 'tasks' | 'context';
export type UiMode = 'normal' | 'command' | 'menu' | 'picker' | 'viewer' | 'editor';
export type OverlayKind = 'none' | 'picker' | 'viewer' | 'editor' | 'help';
export interface AppState {
    /** Currently focused pane */
    focusedPane: PaneKind;
    /** Current UI mode */
    mode: UiMode;
    /** Whether session pane is collapsed */
    sessionPaneCollapsed: boolean;
    /** Active overlay */
    overlay: OverlayKind;
    /** Terminal dimensions */
    columns: number;
    rows: number;
    /** Whether an execution is running */
    busy: boolean;
    /** Command input text */
    inputValue: string;
    /** Auto-scroll enabled */
    autoScroll: boolean;
}
export type AppAction = {
    type: 'SET_FOCUSED_PANE';
    pane: PaneKind;
} | {
    type: 'SET_MODE';
    mode: UiMode;
} | {
    type: 'TOGGLE_SESSION_PANE';
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
    type: 'SET_INPUT';
    value: string;
} | {
    type: 'TOGGLE_AUTO_SCROLL';
};
export declare function createInitialAppState(): AppState;
export declare function appReducer(state: AppState, action: AppAction): AppState;
//# sourceMappingURL=app-state.d.ts.map