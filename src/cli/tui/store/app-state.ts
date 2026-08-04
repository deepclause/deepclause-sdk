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

export type AppAction =
  | { type: 'SET_FOCUSED_PANE'; pane: PaneKind }
  | { type: 'CYCLE_PANE' }
  | { type: 'SET_MODE'; mode: UiMode }
  | { type: 'TOGGLE_SESSION_PANE' }
  | { type: 'SET_OVERLAY'; overlay: OverlayKind }
  | { type: 'RESIZE'; columns: number; rows: number }
  | { type: 'SET_BUSY'; busy: boolean }
  | { type: 'SET_INPUT'; value: string }
  | { type: 'TOGGLE_AUTO_SCROLL' };

export function createInitialAppState(): AppState {
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

const PANE_ORDER: PaneKind[] = ['sessions', 'messages', 'process', 'tasks', 'context'];

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
    case 'SET_INPUT':
      return { ...state, inputValue: action.value };
    case 'TOGGLE_AUTO_SCROLL':
      return { ...state, autoScroll: !state.autoScroll };
    default:
      return state;
  }
}
