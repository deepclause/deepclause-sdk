/**
 * Session state management for TUI v3.
 * Tracks sessions, messages, and streaming content.
 */
export function createInitialSessionState() {
    return {
        sessions: [],
        activeSessionId: null,
        activeTitle: 'No session',
        messages: [],
        executionPreview: null,
        loading: false,
    };
}
export function sessionReducer(state, action) {
    switch (action.type) {
        case 'SET_SESSIONS':
            return { ...state, sessions: action.sessions };
        case 'SET_ACTIVE_SESSION':
            return { ...state, activeSessionId: action.id, activeTitle: action.title };
        case 'SET_MESSAGES':
            return { ...state, messages: action.messages };
        case 'APPEND_MESSAGE':
            return { ...state, messages: [...state.messages, action.message] };
        case 'START_EXECUTION_PREVIEW':
            return {
                ...state,
                executionPreview: { label: action.label, content: '', complete: false, expanded: true },
            };
        case 'UPDATE_EXECUTION_PREVIEW':
            return state.executionPreview
                ? {
                    ...state,
                    executionPreview: {
                        ...state.executionPreview,
                        content: action.content,
                        label: action.label ?? state.executionPreview.label,
                    },
                }
                : state;
        case 'COMPLETE_EXECUTION_PREVIEW':
            return state.executionPreview
                ? { ...state, executionPreview: { ...state.executionPreview, complete: true, expanded: false } }
                : state;
        case 'TOGGLE_EXECUTION_PREVIEW':
            return state.executionPreview?.complete
                ? { ...state, executionPreview: { ...state.executionPreview, expanded: !state.executionPreview.expanded } }
                : state;
        case 'CLEAR_EXECUTION_PREVIEW':
            return { ...state, executionPreview: null };
        case 'SET_LOADING':
            return { ...state, loading: action.loading };
        default:
            return state;
    }
}
//# sourceMappingURL=session-state.js.map