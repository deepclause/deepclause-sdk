/**
 * Grapheme-aware string width measurement utilities.
 */
/**
 * Measure the display width of a string, accounting for wide characters
 * (CJK, emoji) and ignoring ANSI escape codes.
 */
export declare function measureDisplayWidth(text: string): number;
/** Truncate text to fit within a given display width, appending '…' if needed. */
export declare function ellipsize(text: string, maxWidth: number): string;
//# sourceMappingURL=text-measure.d.ts.map