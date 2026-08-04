/**
 * ANSI string manipulation utilities.
 *
 * Provides functions for stripping, measuring, and slicing strings
 * that contain ANSI escape sequences.
 */
/** Strip all ANSI escape sequences from a string. */
export declare function stripAnsi(text: string): string;
/** Measure the visible display width of a string (ignoring ANSI codes). */
export declare function visibleLength(text: string): number;
/**
 * Clip a string to a maximum visible width without splitting ANSI sequences.
 * Unlike truncate(), this does not add an ellipsis.
 */
export declare function clipAnsi(text: string, maxWidth: number): string;
/**
 * Truncate a string to a maximum visible width, preserving ANSI codes.
 * Appends '…' if truncated.
 */
export declare function truncate(text: string, maxWidth: number): string;
/**
 * Pad a string with spaces to reach the desired visible width.
 * Accounts for ANSI codes not counting as visible characters.
 */
export declare function padRight(text: string, width: number): string;
/**
 * Center a string within a given width.
 */
export declare function center(text: string, width: number): string;
/** ANSI color/style codes. */
export declare const ANSI: {
    readonly reset: "\u001B[0m";
    readonly bold: "\u001B[1m";
    readonly dim: "\u001B[2m";
    readonly italic: "\u001B[3m";
    readonly underline: "\u001B[4m";
    readonly inverse: "\u001B[7m";
    readonly black: "\u001B[30m";
    readonly red: "\u001B[31m";
    readonly green: "\u001B[32m";
    readonly yellow: "\u001B[33m";
    readonly blue: "\u001B[34m";
    readonly magenta: "\u001B[35m";
    readonly cyan: "\u001B[36m";
    readonly white: "\u001B[37m";
    readonly brightBlack: "\u001B[90m";
    readonly brightRed: "\u001B[91m";
    readonly brightGreen: "\u001B[92m";
    readonly brightYellow: "\u001B[93m";
    readonly brightBlue: "\u001B[94m";
    readonly brightMagenta: "\u001B[95m";
    readonly brightCyan: "\u001B[96m";
    readonly brightWhite: "\u001B[97m";
    readonly bgBlue: "\u001B[44m";
    readonly bgCyan: "\u001B[46m";
    readonly bgWhite: "\u001B[47m";
    readonly bgBrightBlue: "\u001B[104m";
};
/** Apply a style to text and auto-reset at the end. */
export declare function style(text: string, ...styles: string[]): string;
//# sourceMappingURL=ansi.d.ts.map