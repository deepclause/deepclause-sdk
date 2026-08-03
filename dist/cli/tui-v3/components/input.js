/**
 * Input component — multiline text editor with cursor navigation.
 * Supports Enter for newline, Ctrl+Enter / Ctrl+D to submit.
 */
import { style, ANSI } from '../util/ansi.js';
export class Input {
    dirty = true;
    minHeight = 3;
    flexGrow = 0;
    requestRenderFn;
    lines = [''];
    cursorRow = 0;
    cursorCol = 0;
    _active = true;
    prompt = '│ ';
    maxVisibleLines = 5;
    scrollOffset = 0;
    onSubmit = null;
    onEscape = null;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    /** Set the submit callback. */
    setOnSubmit(fn) {
        this.onSubmit = fn;
    }
    /** Set the escape callback. */
    setOnEscape(fn) {
        this.onEscape = fn;
    }
    /** Set the prompt prefix. */
    setPrompt(prompt) {
        this.prompt = prompt;
        this.invalidate();
    }
    /** Set whether the input is active (accepting input). */
    setActive(active) {
        this._active = active;
        this.invalidate();
    }
    /** Get the current multiline value. */
    getValue() {
        return this.lines.join('\n');
    }
    /** Set the value programmatically. */
    setValue(value) {
        this.lines = value.split('\n');
        this.cursorRow = this.lines.length - 1;
        this.cursorCol = this.lines[this.cursorRow].length;
        this.invalidate();
    }
    /** Clear the input. */
    clear() {
        this.lines = [''];
        this.cursorRow = 0;
        this.cursorCol = 0;
        this.scrollOffset = 0;
        this.invalidate();
    }
    get active() {
        return this._active;
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        const rows = [];
        const contentWidth = Math.max(10, width - this.prompt.length);
        // Top border
        rows.push(style('┌' + '─'.repeat(width - 2) + '┐', ANSI.cyan));
        // Ensure cursor is visible within scroll window
        if (this.cursorRow < this.scrollOffset) {
            this.scrollOffset = this.cursorRow;
        }
        else if (this.cursorRow >= this.scrollOffset + this.maxVisibleLines) {
            this.scrollOffset = this.cursorRow - this.maxVisibleLines + 1;
        }
        // Render visible lines
        const visibleEnd = Math.min(this.lines.length, this.scrollOffset + this.maxVisibleLines);
        for (let i = this.scrollOffset; i < visibleEnd; i++) {
            const lineText = this.lines[i];
            const prefix = i === this.scrollOffset && i === 0
                ? style('│› ', ANSI.cyan)
                : style('│  ', ANSI.cyan);
            const suffix = style('│', ANSI.cyan);
            if (this._active && i === this.cursorRow) {
                // Render with cursor
                const col = Math.min(this.cursorCol, lineText.length);
                const before = lineText.slice(0, col);
                const cursorChar = lineText[col] ?? ' ';
                const after = lineText.slice(col + 1);
                const textPart = before + style(cursorChar, ANSI.inverse) + after;
                const pad = Math.max(0, contentWidth - Math.max(lineText.length, col + 1));
                rows.push(prefix + textPart + ' '.repeat(pad) + suffix);
            }
            else {
                const pad = Math.max(0, contentWidth - lineText.length);
                const textPart = this._active ? lineText : style(lineText, ANSI.dim);
                rows.push(prefix + textPart + ' '.repeat(pad) + suffix);
            }
        }
        // Pad remaining visible lines if fewer than maxVisibleLines
        for (let i = visibleEnd - this.scrollOffset; i < this.maxVisibleLines; i++) {
            const prefix = style('│  ', ANSI.cyan);
            const suffix = style('│', ANSI.cyan);
            rows.push(prefix + ' '.repeat(contentWidth) + suffix);
        }
        // Bottom border with hints
        const hint = this.lines.length > 1
            ? ' Ctrl+Enter:send  Esc:cancel '
            : ' Enter:send  Shift+Enter:newline ';
        const hintLen = hint.length;
        const bottomBorder = '└' + '─'.repeat(Math.max(0, width - 2 - hintLen)) + style(hint, ANSI.dim) + '┘';
        rows.push(style(bottomBorder, ANSI.cyan));
        this.dirty = false;
        return rows;
    }
    handleInput(key) {
        if (!this._active)
            return false;
        // Submit: Ctrl+Enter or Ctrl+D (for multiline), or plain Enter on single line
        if (key.name === 'return' && key.ctrl) {
            this.submit();
            return true;
        }
        if (key.ctrl && key.name === 'd') {
            this.submit();
            return true;
        }
        // Plain Enter: submit if single line, otherwise newline
        if (key.name === 'return' && !key.shift && !key.ctrl && !key.meta) {
            if (this.lines.length === 1) {
                this.submit();
            }
            else {
                this.insertNewline();
            }
            return true;
        }
        // Shift+Enter: always newline
        if (key.name === 'return' && key.shift) {
            this.insertNewline();
            return true;
        }
        // Escape
        if (key.name === 'escape') {
            if (this.lines.length > 1 || this.lines[0].length > 0) {
                this.clear();
                return true;
            }
            if (this.onEscape)
                this.onEscape();
            return true;
        }
        // Backspace
        if (key.name === 'backspace') {
            if (this.cursorCol > 0) {
                const line = this.lines[this.cursorRow];
                this.lines[this.cursorRow] = line.slice(0, this.cursorCol - 1) + line.slice(this.cursorCol);
                this.cursorCol--;
            }
            else if (this.cursorRow > 0) {
                // Merge with previous line
                const prevLine = this.lines[this.cursorRow - 1];
                const curLine = this.lines[this.cursorRow];
                this.lines.splice(this.cursorRow, 1);
                this.cursorRow--;
                this.cursorCol = prevLine.length;
                this.lines[this.cursorRow] = prevLine + curLine;
            }
            this.invalidate();
            return true;
        }
        // Delete
        if (key.name === 'delete') {
            const line = this.lines[this.cursorRow];
            if (this.cursorCol < line.length) {
                this.lines[this.cursorRow] = line.slice(0, this.cursorCol) + line.slice(this.cursorCol + 1);
            }
            else if (this.cursorRow < this.lines.length - 1) {
                // Merge with next line
                this.lines[this.cursorRow] = line + this.lines[this.cursorRow + 1];
                this.lines.splice(this.cursorRow + 1, 1);
            }
            this.invalidate();
            return true;
        }
        // Cursor movement
        if (key.name === 'left') {
            if (this.cursorCol > 0) {
                this.cursorCol--;
            }
            else if (this.cursorRow > 0) {
                this.cursorRow--;
                this.cursorCol = this.lines[this.cursorRow].length;
            }
            this.invalidate();
            return true;
        }
        if (key.name === 'right') {
            const line = this.lines[this.cursorRow];
            if (this.cursorCol < line.length) {
                this.cursorCol++;
            }
            else if (this.cursorRow < this.lines.length - 1) {
                this.cursorRow++;
                this.cursorCol = 0;
            }
            this.invalidate();
            return true;
        }
        if (key.name === 'up') {
            if (this.cursorRow > 0) {
                this.cursorRow--;
                this.cursorCol = Math.min(this.cursorCol, this.lines[this.cursorRow].length);
                this.invalidate();
            }
            return true;
        }
        if (key.name === 'down') {
            if (this.cursorRow < this.lines.length - 1) {
                this.cursorRow++;
                this.cursorCol = Math.min(this.cursorCol, this.lines[this.cursorRow].length);
                this.invalidate();
            }
            return true;
        }
        if (key.name === 'home' || (key.ctrl && key.name === 'a')) {
            this.cursorCol = 0;
            this.invalidate();
            return true;
        }
        if (key.name === 'end' || (key.ctrl && key.name === 'e')) {
            this.cursorCol = this.lines[this.cursorRow].length;
            this.invalidate();
            return true;
        }
        // Ctrl+U — clear current line
        if (key.ctrl && key.name === 'u') {
            this.lines[this.cursorRow] = '';
            this.cursorCol = 0;
            this.invalidate();
            return true;
        }
        // Ctrl+K — kill to end of line
        if (key.ctrl && key.name === 'k') {
            this.lines[this.cursorRow] = this.lines[this.cursorRow].slice(0, this.cursorCol);
            this.invalidate();
            return true;
        }
        // Regular character input
        if (key.sequence && key.sequence.length === 1 && !key.ctrl && !key.meta) {
            const ch = key.sequence;
            if (ch.charCodeAt(0) >= 32) { // Printable
                const line = this.lines[this.cursorRow];
                this.lines[this.cursorRow] = line.slice(0, this.cursorCol) + ch + line.slice(this.cursorCol);
                this.cursorCol++;
                this.invalidate();
                return true;
            }
        }
        return false;
    }
    submit() {
        const val = this.getValue().trim();
        if (!val)
            return;
        this.clear();
        if (this.onSubmit)
            this.onSubmit(val);
    }
    insertNewline() {
        const line = this.lines[this.cursorRow];
        const before = line.slice(0, this.cursorCol);
        const after = line.slice(this.cursorCol);
        this.lines[this.cursorRow] = before;
        this.lines.splice(this.cursorRow + 1, 0, after);
        this.cursorRow++;
        this.cursorCol = 0;
        this.invalidate();
    }
}
//# sourceMappingURL=input.js.map