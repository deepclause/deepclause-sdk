/**
 * Hook for managing keyboard shortcuts in the Ink TUI.
 * Only intercepts modifier-key combos and special keys when input is not active.
 */
import type { AppAction } from '../store/app-state.js';
export interface KeyBindingOptions {
    dispatch: (action: AppAction) => void;
    onCancel: () => void;
    inputActive: boolean;
}
export declare function useKeyBindings({ dispatch, onCancel, inputActive }: KeyBindingOptions): void;
//# sourceMappingURL=useKeyBindings.d.ts.map