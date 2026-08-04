/**
 * Application state machine for TUI v3.
 *
 * Simple reducer-based state management, decoupled from React.
 * State changes trigger invalidation of relevant components.
 */

export type PaneKind = 'sessions' | 'messages' | 'context';
export type UiMode = 'normal' | 'command' | 'picker' | 'help';
export type OverlayKind = 'none' | 'picker' | 'help' | 'confirm';

export interface PaneVisibility {
  sessions: boolean;
  messages: boolean;
  context: boolean;
}

export interface AppState {
  focusedPane: PaneKind;
  mode: UiMode;
  sessionPaneCollapsed: boolean;
  paneVisibility: PaneVisibility;
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
  | { type: 'TOGGLE_PANE_VISIBILITY'; pane: PaneKind }
  | { type: 'SET_OVERLAY'; overlay: OverlayKind }
  | { type: 'RESIZE'; columns: number; rows: number }
  | { type: 'SET_BUSY'; busy: boolean }
  | { type: 'TOGGLE_AUTO_SCROLL' };

const PANE_ORDER: PaneKind[] = ['sessions', 'messages', 'context'];

export function createInitialAppState(): AppState {
  return {
    focusedPane: 'messages',
    mode: 'command',
    sessionPaneCollapsed: true,
    paneVisibility: {
      sessions: true,
      messages: true,
      context: true,
    },
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
      // Only cycle through visible panes
      const visiblePanes = PANE_ORDER.filter((p) => state.paneVisibility[p]);
      if (visiblePanes.length === 0) return state;
      const idx = visiblePanes.indexOf(state.focusedPane);
      const next = visiblePanes[(idx + 1) % visiblePanes.length];
      return { ...state, focusedPane: next };
    }
    case 'SET_MODE':
      return { ...state, mode: action.mode };
    case 'TOGGLE_SESSION_PANE':
      return { ...state, sessionPaneCollapsed: !state.sessionPaneCollapsed };
    case 'TOGGLE_PANE_VISIBILITY': {
      const newVis = { ...state.paneVisibility, [action.pane]: !state.paneVisibility[action.pane] };
      // Don't allow hiding all panes — messages must stay visible
      if (!newVis.messages && action.pane === 'messages') return state;
      // If focused pane is now hidden, move focus to messages
      let newFocused = state.focusedPane;
      if (!newVis[newFocused]) {
        const visiblePanes = PANE_ORDER.filter((p) => newVis[p]);
        newFocused = visiblePanes[0] ?? 'messages';
      }
      return { ...state, paneVisibility: newVis, focusedPane: newFocused };
    }
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
