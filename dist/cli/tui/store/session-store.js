/**
 * Session state management for the Ink TUI.
 */
export function createInitialSessionState() {
    return {
        sessions: [],
        activeSessionId: null,
        activeDetail: null,
        messages: [],
        streamingContent: null,
        loading: false,
    };
}
export function sessionReducer(state, action) {
    switch (action.type) {
        case 'SET_SESSIONS':
            return { ...state, sessions: action.sessions };
        case 'SET_ACTIVE_SESSION':
            return {
                ...state,
                activeSessionId: action.id,
                activeDetail: action.detail,
            };
        case 'SET_MESSAGES':
            return { ...state, messages: action.messages };
        case 'APPEND_MESSAGE':
            return { ...state, messages: [...state.messages, action.message] };
        case 'SET_STREAMING':
            return { ...state, streamingContent: action.content };
        case 'SET_LOADING':
            return { ...state, loading: action.loading };
        default:
            return state;
    }
}
//# sourceMappingURL=session-store.js.map