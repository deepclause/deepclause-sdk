/**
 * Minimal ANSI helpers for the Ink-based TUI.
 * Most styling is handled by Ink's <Text> color props, but some edge cases
 * (e.g. measuring display width) still need raw escape-code awareness.
 */
/** Strip all ANSI escape sequences from a string. */
export function stripAnsi(text) {
    // eslint-disable-next-line no-control-regex
    return text.replace(/\u001b\[[0-9;]*[A-Za-z]/g, '');
}
//# sourceMappingURL=ansi.js.map