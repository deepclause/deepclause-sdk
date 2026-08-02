/**
 * Session state management for TUI v3.
 * Tracks sessions, messages, and streaming content.
 */
export interface DisplayMessage {
    role: 'user' | 'assistant' | 'system';
    content: string;
    pending?: boolean;
    error?: boolean;
}
export interface SessionSummary {
    id: string;
    title: string;
    updatedAt?: string;
}
export interface SessionState {
    sessions: SessionSummary[];
    activeSessionId: string | null;
    activeTitle: string;
    messages: DisplayMessage[];
    streamingContent: string | null;
    loading: boolean;
}
export type SessionAction = {
    type: 'SET_SESSIONS';
    sessions: SessionSummary[];
} | {
    type: 'SET_ACTIVE_SESSION';
    id: string;
    title: string;
} | {
    type: 'SET_MESSAGES';
    messages: DisplayMessage[];
} | {
    type: 'APPEND_MESSAGE';
    message: DisplayMessage;
} | {
    type: 'SET_STREAMING';
    content: string | null;
} | {
    type: 'SET_LOADING';
    loading: boolean;
};
export declare function createInitialSessionState(): SessionState;
export declare function sessionReducer(state: SessionState, action: SessionAction): SessionState;
//# sourceMappingURL=session-state.d.ts.map