/**
 * TUI v3 Entry Point — startTuiV3()
 *
 * Wires together the event loop, renderer, state management, and components
 * into a fully functioning terminal UI with zero-flicker differential rendering.
 *
 * Layout: Borland 90s IDE style with multi-pane layout.
 * ┌─────────────────────────────────────────────────────────┐
 * │ ≡ DeepClause                              session title │  <- Logo bar
 * ├───┬──────────────────────────┬──────────────────────────┤
 * │ S │                          │                          │
 * │ e │      Messages            │  Context (Tokens)        │
 * │ s │      (main content)      │                          │
 * │ s │                          │                          │
 * │   │                          │                          │
 * ├───┴──────────────────────────┴──────────────────────────┤
 * │┌─────────────────────────────────────────────────────┐  │
 * ││› multiline input                                    │  │  <- Editor
 * │└─────────────────────────────────────────────────────┘  │
 * ├─────────────────────────────────────────────────────────┤
 * │ F1 Help F2 Sess F3 Ctx Tab Next ^C Quit               │  <- Status bar
 * └─────────────────────────────────────────────────────────┘
 */
export interface TuiV3Options {
    sandbox?: boolean;
}
/**
 * Start the TUI v3 (differential renderer).
 * Returns when the user exits.
 */
export declare function startTuiV3(workspaceRoot?: string, options?: TuiV3Options): Promise<void>;
//# sourceMappingURL=index.d.ts.map