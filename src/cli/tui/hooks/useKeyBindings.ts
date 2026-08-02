/**
 * Hook for managing keyboard shortcuts in the Ink TUI.
 */

import { useInput } from 'ink';
import type { AppAction, PaneKind } from '../store/app-state.js';

export interface KeyBindingOptions {
  dispatch: (action: AppAction) => void;
  onSubmit: (text: string) => void;
  onCancel: () => void;
  inputActive: boolean;
}

const PANE_KEYS: Record<string, PaneKind> = {
  '1': 'sessions',
  '2': 'messages',
  '3': 'process',
  '4': 'tasks',
  '5': 'context',
};

const PANE_ORDER: PaneKind[] = ['sessions', 'messages', 'process', 'tasks', 'context'];

export function useKeyBindings({ dispatch, onSubmit, onCancel, inputActive }: KeyBindingOptions): void {
  useInput((input, key) => {
    // Global shortcuts (always active)
    if (key.ctrl && input === 'c') {
      onCancel();
      return;
    }

    if (key.ctrl && input === 'l') {
      // Refresh / redraw
      return;
    }

    // When input is active, don't intercept normal keys
    if (inputActive) {
      if (key.return) {
        onSubmit(input);
      }
      return;
    }

    // Pane focus shortcuts (Alt+1..5)
    if (key.meta && PANE_KEYS[input]) {
      dispatch({ type: 'SET_FOCUSED_PANE', pane: PANE_KEYS[input] });
      return;
    }

    // Tab to cycle panes
    if (key.tab) {
      // nextPane requires current state; dispatch will be enhanced later
      dispatch({ type: 'SET_FOCUSED_PANE', pane: nextPane('messages') });
      return;
    }

    // Toggle session pane
    if (key.ctrl && input === 'b') {
      dispatch({ type: 'TOGGLE_SESSION_PANE' });
      return;
    }

    // Toggle auto-scroll
    if (key.ctrl && input === 'f') {
      dispatch({ type: 'TOGGLE_AUTO_SCROLL' });
      return;
    }

    // Enter command mode
    if (input === ':' || input === '/') {
      dispatch({ type: 'SET_MODE', mode: 'command' });
      return;
    }

    // Help overlay
    if (input === '?') {
      dispatch({ type: 'SET_OVERLAY', overlay: 'help' });
      return;
    }

    // Escape closes overlays
    if (key.escape) {
      dispatch({ type: 'SET_OVERLAY', overlay: 'none' });
      dispatch({ type: 'SET_MODE', mode: 'normal' });
    }
  });
}

function nextPane(current: PaneKind): PaneKind {
  const idx = PANE_ORDER.indexOf(current);
  return PANE_ORDER[(idx + 1) % PANE_ORDER.length];
}
