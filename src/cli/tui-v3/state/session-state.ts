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

export type SessionAction =
  | { type: 'SET_SESSIONS'; sessions: SessionSummary[] }
  | { type: 'SET_ACTIVE_SESSION'; id: string; title: string }
  | { type: 'SET_MESSAGES'; messages: DisplayMessage[] }
  | { type: 'APPEND_MESSAGE'; message: DisplayMessage }
  | { type: 'SET_STREAMING'; content: string | null }
  | { type: 'SET_LOADING'; loading: boolean };

export function createInitialSessionState(): SessionState {
  return {
    sessions: [],
    activeSessionId: null,
    activeTitle: 'No session',
    messages: [],
    streamingContent: null,
    loading: false,
  };
}

export function sessionReducer(state: SessionState, action: SessionAction): SessionState {
  switch (action.type) {
    case 'SET_SESSIONS':
      return { ...state, sessions: action.sessions };
    case 'SET_ACTIVE_SESSION':
      return { ...state, activeSessionId: action.id, activeTitle: action.title };
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
