/**
 * Differential terminal renderer for TUI v3.
 *
 * Key design:
 * - Uses alternate screen buffer to avoid scroll artifacts
 * - Row-level diffing: only writes rows that have changed
 * - Cursor is hidden during rendering for flicker-free output
 * - All output goes through a single write buffer to minimize syscalls
 */
/** ANSI escape sequences for terminal control. */
const ESC = '\x1b[';
const ALT_SCREEN_ON = '\x1b[?1049h';
const ALT_SCREEN_OFF = '\x1b[?1049l';
const CURSOR_HIDE = `${ESC}?25l`;
const CURSOR_SHOW = `${ESC}?25h`;
const CLEAR_LINE = `${ESC}2K`;
const MOUSE_ON = '\x1b[?1003h\x1b[?1006h';
const MOUSE_OFF = '\x1b[?1003l\x1b[?1006l';
function moveCursor(row, col) {
    return `${ESC}${row + 1};${col + 1}H`;
}
/**
 * The differential renderer maintains the previous screen state
 * and only writes changed rows to the terminal.
 */
export class Renderer {
    stdout;
    prevScreen = [];
    useAltScreen;
    useMouse;
    active = false;
    _rows = 0;
    _cols = 0;
    constructor(options = {}) {
        this.stdout = options.stdout ?? process.stdout;
        this.useAltScreen = options.altScreen !== false;
        this.useMouse = options.mouse ?? false;
    }
    /** Current terminal rows. */
    get rows() {
        return this._rows || this.stdout.rows || 24;
    }
    /** Current terminal columns. */
    get cols() {
        return this._cols || this.stdout.columns || 80;
    }
    /** Enter the TUI: alt-screen, hide cursor, enable mouse. */
    enter() {
        if (this.active)
            return;
        this.active = true;
        this._rows = this.stdout.rows || 24;
        this._cols = this.stdout.columns || 80;
        let init = '';
        if (this.useAltScreen)
            init += ALT_SCREEN_ON;
        init += CURSOR_HIDE;
        if (this.useMouse)
            init += MOUSE_ON;
        this.stdout.write(init);
        this.prevScreen = [];
    }
    /** Exit the TUI: restore screen, show cursor, disable mouse. */
    exit() {
        if (!this.active)
            return;
        this.active = false;
        let cleanup = '';
        if (this.useMouse)
            cleanup += MOUSE_OFF;
        cleanup += CURSOR_SHOW;
        if (this.useAltScreen)
            cleanup += ALT_SCREEN_OFF;
        this.stdout.write(cleanup);
        this.prevScreen = [];
    }
    /**
     * Render a new screen buffer to the terminal.
     * Only rows that differ from the previous frame are written.
     */
    render(screen) {
        if (!this.active)
            return;
        const rows = this.rows;
        const cols = this.cols;
        let output = '';
        // Hide cursor during render
        output += CURSOR_HIDE;
        for (let i = 0; i < rows; i++) {
            const newRow = (screen[i] ?? '').slice(0, cols);
            const prevRow = this.prevScreen[i] ?? '';
            if (newRow !== prevRow) {
                output += moveCursor(i, 0);
                output += CLEAR_LINE;
                output += newRow;
            }
        }
        // Single write for the entire frame
        if (output.length > CURSOR_HIDE.length) {
            this.stdout.write(output);
        }
        this.prevScreen = screen.slice(0, rows);
    }
    /**
     * Show the cursor at a specific position (e.g., for text input).
     */
    showCursor(row, col) {
        if (!this.active)
            return;
        this.stdout.write(moveCursor(row, col) + CURSOR_SHOW);
    }
    /** Hide the cursor. */
    hideCursor() {
        if (!this.active)
            return;
        this.stdout.write(CURSOR_HIDE);
    }
    /** Handle terminal resize. */
    resize(rows, cols) {
        this._rows = rows;
        this._cols = cols;
        // Force full redraw on next render by clearing prev screen
        this.prevScreen = [];
    }
    /** Force a full screen clear and redraw on next render. */
    invalidateAll() {
        this.prevScreen = [];
    }
    /** Whether the renderer is currently active. */
    get isActive() {
        return this.active;
    }
}
//# sourceMappingURL=renderer.js.map