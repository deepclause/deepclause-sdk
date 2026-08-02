/**
 * Hook for managing keyboard shortcuts in the Ink TUI.
 */
import type { AppAction } from '../store/app-state.js';
export interface KeyBindingOptions {
    dispatch: (action: AppAction) => void;
    onSubmit: (text: string) => void;
    onCancel: () => void;
    inputActive: boolean;
}
export declare function useKeyBindings({ dispatch, onSubmit, onCancel, inputActive }: KeyBindingOptions): void;
//# sourceMappingURL=useKeyBindings.d.ts.map