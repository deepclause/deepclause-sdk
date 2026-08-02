/**
 * TUI v3 Entry Point — startTuiV3()
 *
 * Wires together the event loop, renderer, state management, and components
 * into a fully functioning terminal UI with zero-flicker differential rendering.
 */
export interface TuiV3Options {
    sandbox?: boolean;
}
/**
 * Start the TUI v3 (differential renderer).
 * Returns when the user exits.
 */
export declare function startTuiV3(workspaceRoot?: any, options?: TuiV3Options): Promise<void>;
//# sourceMappingURL=index.d.ts.map