/**
 * DeepClause CLI - Terminal UI Module (Stub)
 *
 * TODO: Full implementation in Phase 5
 */
export interface TUIOptions {
    headless?: boolean;
    verbose?: boolean;
}
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