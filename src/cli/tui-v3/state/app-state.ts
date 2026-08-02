/**
 * Application state machine for TUI v3.
 *
 * Simple reducer-based state management, decoupled from React.
 * State changes trigger invalidation of relevant components.
 */

export type PaneKind = 'sessions' | 'messages' | 'process' | 'tasks' | 'context';
export type UiMode = 'normal' | 'command' | 'picker' | 'help';
export type OverlayKind = 'none' | 'picker' | 'help' | 'confirm';

export interface AppState {
  focusedPane: PaneKind;
  mode: UiMode;
  sessionPaneCollapsed: boolean;
  overlay: OverlayKind;
  columns: number;
  rows: number;
  busy: boolean;
  autoScroll: boolean;
}

export type AppAction =
  | { type: 'SET_FOCUSED_PANE'; pane: PaneKind }
  | { type: 'CYCLE_PANE' }
  | { type: 'SET_MODE'; mode: UiMode }
  | { type: 'TOGGLE_SESSION_PANE' }
  | { type: 'SET_OVERLAY'; overlay: OverlayKind }
  | { type: 'RESIZE'; columns: number; rows: number }
  | { type: 'SET_BUSY'; busy: boolean }
  | { type: 'TOGGLE_AUTO_SCROLL' };

const PANE_ORDER: PaneKind[] = ['sessions', 'messages', 'process', 'tasks', 'context'];

export function createInitialAppState(): AppState {
  return {
    focusedPane: 'messages',
    mode: 'command',
    sessionPaneCollapsed: true,
    overlay: 'none',
    columns: process.stdout.columns || 80,
    rows: process.stdout.rows || 24,
    busy: false,
    autoScroll: true,
  };
}

export function appReducer(state: AppState, action: AppAction): AppState {
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
    case 'TOGGLE_AUTO_SCROLL':
      return { ...state, autoScroll: !state.autoScroll };
    default:
      return state;
  }
}
