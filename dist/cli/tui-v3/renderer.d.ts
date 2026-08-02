/**
 * Differential terminal renderer for TUI v3.
 *
 * Key design:
 * - Uses alternate screen buffer to avoid scroll artifacts
 * - Row-level diffing: only writes rows that have changed
 * - Cursor is hidden during rendering for flicker-free output
 * - All output goes through a single write buffer to minimize syscalls
 */
import type { ScreenBuffer } from './types.js';
export interface RendererOptions {
    /** Stream to write to (defaults to process.stdout) */
    stdout?: NodeJS.WriteStream;
    /** Whether to use alternate screen buffer (default true) */
    altScreen?: boolean;
    /** Whether to enable mouse events (default false) */
    mouse?: boolean;
}
/**
 * The differential renderer maintains the previous screen state
 * and only writes changed rows to the terminal.
 */
export declare class Renderer {
    private stdout;
    private prevScreen;
    private useAltScreen;
    private useMouse;
    private active;
    private _rows;
    private _cols;
    constructor(options?: RendererOptions);
    /** Current terminal rows. */
    get rows(): number;
    /** Current terminal columns. */
    get cols(): number;
    /** Enter the TUI: alt-screen, hide cursor, enable mouse. */
    enter(): void;
    /** Exit the TUI: restore screen, show cursor, disable mouse. */
    exit(): void;
    /**
     * Render a new screen buffer to the terminal.
     * Only rows that differ from the previous frame are written.
     */
    render(screen: ScreenBuffer): void;
    /**
     * Show the cursor at a specific position (e.g., for text input).
     */
    showCursor(row: number, col: number): void;
    /** Hide the cursor. */
    hideCursor(): void;
    /** Handle terminal resize. */
    resize(rows: number, cols: number): void;
    /** Force a full screen clear and redraw on next render. */
    invalidateAll(): void;
    /** Whether the renderer is currently active. */
    get isActive(): boolean;
}
//# sourceMappingURL=renderer.d.ts.map