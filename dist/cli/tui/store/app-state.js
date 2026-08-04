/**
 * Global application state for the Ink TUI.
 * Uses a simple event-emitter pattern for state updates,
 * consumed via React hooks.
 */
export function createInitialAppState() {
    return {
        focusedPane: 'messages',
        mode: 'normal',
        sessionPaneCollapsed: true,
        overlay: 'none',
        columns: process.stdout.columns || 80,
        rows: process.stdout.rows || 24,
        busy: false,
        inputValue: '',
        autoScroll: true,
    };
}
const PANE_ORDER = ['sessions', 'messages', 'process', 'tasks', 'context'];
export function appReducer(state, action) {
    switch (action.type) {
        case 'SET_FOCUSED_PANE':
            return { ...state, focusedPane: action.pane };
        case 'CYCLE_PANE': {
            const idx = PANE_ORDER.indexOf(state.focusedPane);
            const next = PANE_ORDER[(idx + 1) % PANE_ORDER.length];
            return { ...state, focusedPane: next };
        }
        case 'SET_MODE':
            return { ...state, mode: action.mode };
        case 'TOGGLE_SESSION_PANE':
            return { ...state, sessionPaneCollapsed: !state.sessionPaneCollapsed };
        case 'SET_OVERLAY':
            return { ...state, overlay: action.overlay };
        case 'RESIZE':
            return { ...state, columns: action.columns, rows: action.rows };
        case 'SET_BUSY':
            return { ...state, busy: action.busy };
        case 'SET_INPUT':
            return { ...state, inputValue: action.value };
        case 'TOGGLE_AUTO_SCROLL':
            return { ...state, autoScroll: !state.autoScroll };
        default:
            return state;
    }
}
//# sourceMappingURL=app-state.js.map