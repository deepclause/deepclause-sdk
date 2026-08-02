/**
 * Minimal ANSI helpers for the Ink-based TUI.
 * Most styling is handled by Ink's <Text> color props, but some edge cases
 * (e.g. measuring display width) still need raw escape-code awareness.
 */
/** Strip all ANSI escape sequences from a string. */
export declare function stripAnsi(text: string): string;
//# sourceMappingURL=ansi.d.ts.map