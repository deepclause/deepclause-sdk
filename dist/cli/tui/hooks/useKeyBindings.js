/**
 * Hook for managing keyboard shortcuts in the Ink TUI.
 * Only intercepts modifier-key combos and special keys when input is not active.
 */
import { useInput } from 'ink';
const PANE_KEYS = {
    '1': 'sessions',
    '2': 'messages',
    '3': 'process',
    '4': 'tasks',
    '5': 'context',
};
export function useKeyBindings({ dispatch, onCancel, inputActive }) {
    useInput((input, key) => {
        // Global shortcuts (always active)
        if (key.ctrl && input === 'c') {
            onCancel();
            return;
        }
        // When input is active, don't intercept anything else
        if (inputActive) {
            return;
        }
        // Pane focus shortcuts (Alt+1..5)
        if (key.meta && PANE_KEYS[input]) {
            dispatch({ type: 'SET_FOCUSED_PANE', pane: PANE_KEYS[input] });
            return;
        }
        // Tab to cycle panes
        if (key.tab) {
            dispatch({ type: 'CYCLE_PANE' });
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
//# sourceMappingURL=useKeyBindings.js.map