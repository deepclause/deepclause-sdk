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

export type SessionAction =
  | { type: 'SET_SESSIONS'; sessions: ConductorSessionSummary[] }
  | { type: 'SET_ACTIVE_SESSION'; id: string; detail: ConductorSessionDetail }
  | { type: 'SET_MESSAGES'; messages: DisplayMessage[] }
  | { type: 'APPEND_MESSAGE'; message: DisplayMessage }
  | { type: 'SET_STREAMING'; content: string | null }
  | { type: 'SET_LOADING'; loading: boolean };

export function createInitialSessionState(): SessionState {
  return {
    sessions: [],
    activeSessionId: null,
    activeDetail: null,
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
