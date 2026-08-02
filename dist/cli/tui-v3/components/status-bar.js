/**
 * StatusBar component — bottom bar showing mode, focused pane, and shortcuts.
 */
import { style, ANSI, padRight } from '../util/ansi.js';
export class StatusBar {
    dirty = true;
    minHeight = 1;
    flexGrow = 0;
    requestRenderFn;
    mode = 'normal';
    focusedPane = 'messages';
    busy = false;
    followMode = true;
    statusRight = '';
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    setMode(mode) {
        if (this.mode === mode)
            return;
        this.mode = mode;
        this.invalidate();
    }
    setFocusedPane(pane) {
        if (this.focusedPane === pane)
            return;
        this.focusedPane = pane;
        this.invalidate();
    }
    setBusy(busy) {
        if (this.busy === busy)
            return;
        this.busy = busy;
        this.invalidate();
    }
    setFollowMode(follow) {
        if (this.followMode === follow)
            return;
        this.followMode = follow;
        this.invalidate();
    }
    setStatusRight(text) {
        if (this.statusRight === text)
            return;
        this.statusRight = text;
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        const parts = [];
        // Mode indicator
        const modeLabel = this.mode.toUpperCase();
        parts.push(style(` ${modeLabel} `, ANSI.bold, ANSI.inverse));
        // Focused pane
        parts.push(style(` ${this.focusedPane} `, ANSI.dim));
        // Follow indicator
        if (this.followMode) {
            parts.push(style(' ↓FOLLOW ', ANSI.cyan));
        }
        // Busy indicator
        if (this.busy) {
            parts.push(style(' RUNNING ', ANSI.yellow));
        }
        // Shortcuts hint
        const shortcuts = 'Tab:pane  Ctrl+C:quit  ?:help';
        const left = parts.join('');
        // Right side
        const right = this.statusRight || shortcuts;
        const rightStyled = style(right, ANSI.dim);
        // Compose
        const leftVisLen = modeLabel.length + 2 + this.focusedPane.length + 2
            + (this.followMode ? 9 : 0) + (this.busy ? 9 : 0);
        const rightLen = right.length;
        const gap = Math.max(1, width - leftVisLen - rightLen);
        const line = left + ' '.repeat(gap) + rightStyled;
        this.dirty = false;
        return [padRight(line, width)];
    }
}
//# sourceMappingURL=status-bar.js.map