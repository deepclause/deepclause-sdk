/**
 * DeepClause CLI - Terminal UI Module (Ink-based v2)
 *
 * This is the Ink (React for CLIs) rewrite of the TUI.
 * Activated via the `--tui=v2` flag; the original monolithic TUI
 * remains the default in `../tui.ts`.
 */
export interface TUIOptions {
    headless?: boolean;
    verbose?: boolean;
}
/**
 * Start the Ink-based TUI (v2).
 */
export declare function startTuiV2(workspaceRoot?: any, options?: {
    sandbox?: boolean;
}): Promise<void>;
/**
 * Render execution progress in TUI
 */
export declare function renderExecution(_options: TUIOptions): void;
/**
 * Render compilation progress in TUI
 */
export declare function renderCompilation(_options: TUIOptions): void;
/**
 * Check if TUI should be used
 */
export declare function shouldUseTUI(options: TUIOptions): boolean;
//# sourceMappingURL=index.d.ts.map