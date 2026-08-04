/**
 * Session state management for the Ink TUI.
 */
import type { ConductorSessionDetail, ConductorSessionSummary } from '../../../system/runtime/conductor.js';
export interface DisplayMessage {
    role: 'user' | 'assistant' | 'system';
    content: string;
    pending?: boolean;
    error?: boolean;
    tag?: string;
    kind?: 'output' | 'question';
}
export interface SessionState {
    /** List of available sessions */
    sessions: ConductorSessionSummary[];
    /** Currently active session ID */
    activeSessionId: string | null;
    /** Active session detail (if loaded) */
    activeDetail: ConductorSessionDetail | null;
    /** Messages for the active session */
    messages: DisplayMessage[];
    /** Streaming message currently being received */
    streamingContent: string | null;
    /** Whether sessions list is loading */
    loading: boolean;
}
export type SessionAction = {
    type: 'SET_SESSIONS';
    sessions: ConductorSessionSummary[];
} | {
    type: 'SET_ACTIVE_SESSION';
    id: string;
    detail: ConductorSessionDetail;
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
//# sourceMappingURL=session-store.d.ts.map