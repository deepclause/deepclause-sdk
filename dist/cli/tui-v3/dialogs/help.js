/**
 * Help overlay dialog — shows keyboard shortcuts and commands.
 */
import { style, ANSI, center, clipAnsi, padRight } from '../util/ansi.js';
export class HelpDialog {
    dirty = true;
    minHeight = 0;
    flexGrow = 0;
    requestRenderFn;
    visible = false;
    onClose = null;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    show() {
        this.visible = true;
        this.invalidate();
    }
    hide() {
        this.visible = false;
        this.invalidate();
    }
    setOnClose(fn) {
        this.onClose = fn;
    }
    get isVisible() {
        return this.visible;
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        if (!this.visible)
            return [];
        const maxWidth = Math.max(4, Math.min(64, width - 4));
        const rows = [];
        const border = '─'.repeat(maxWidth - 2);
        rows.push(`┌${border}┐`);
        rows.push(`│${center(style('Help', ANSI.bold), maxWidth - 2)}│`);
        rows.push(`├${border}┤`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`│${padLine(style(' Keyboard Shortcuts:', ANSI.bold), maxWidth - 2)}│`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`│${padLine('  Tab          Cycle panes', maxWidth - 2)}│`);
        rows.push(`│${padLine('  F2/F3       Focus; press again to hide', maxWidth - 2)}│`);
        rows.push(`│${padLine('  ↑/↓ + Enter Select focused session', maxWidth - 2)}│`);
        rows.push(`│${padLine('  Ctrl+C       Quit / Cancel', maxWidth - 2)}│`);
        rows.push(`│${padLine('  Ctrl+U       Clear input', maxWidth - 2)}│`);
        rows.push(`│${padLine('  Ctrl+E       Inspect last DML execution', maxWidth - 2)}│`);
        rows.push(`│${padLine('  Shift+↑/↓    Scroll messages', maxWidth - 2)}│`);
        rows.push(`│${padLine('  PgUp/PgDn    Page scroll', maxWidth - 2)}│`);
        rows.push(`│${padLine('  Home/End     Top/Bottom of messages', maxWidth - 2)}│`);
        rows.push(`│${padLine('  ?            Toggle this help', maxWidth - 2)}│`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`│${padLine(style(' Commands:', ANSI.bold), maxWidth - 2)}│`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`│${padLine('  /new [title] Create new session', maxWidth - 2)}│`);
        rows.push(`│${padLine('  /sessions    Toggle session pane', maxWidth - 2)}│`);
        rows.push(`│${padLine('  /help        Show this help', maxWidth - 2)}│`);
        rows.push(`│${padLine('  /exit        Quit the TUI', maxWidth - 2)}│`);
        rows.push(`│${padLine('', maxWidth - 2)}│`);
        rows.push(`│${center(style('Press Esc or ? to close', ANSI.dim), maxWidth - 2)}│`);
        rows.push(`└${border}┘`);
        this.dirty = false;
        return rows;
    }
    handleInput(key) {
        if (!this.visible)
            return false;
        if (key.name === 'escape' || key.name === 'esc' || key.sequence === '\x1b' || key.sequence === '?') {
            this.hide();
            if (this.onClose)
                this.onClose();
            return true;
        }
        // Consume all keys when help is shown
        return true;
    }
}
function padLine(text, width) {
    return padRight(clipAnsi(text, width), width);
}
//# sourceMappingURL=help.js.map