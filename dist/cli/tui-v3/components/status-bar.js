/**
 * StatusBar component — Borland-style bottom bar with pane shortcuts and status.
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
    paneVisibility = {
        sessions: true,
        messages: true,
        context: true,
    };
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
    setPaneVisibility(vis) {
        this.paneVisibility = vis;
        this.invalidate();
    }
    setStatusRight(_text) {
        // Kept for API compat
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        // Borland-style: function key shortcuts on the bottom
        const shortcuts = [
            { key: 'F1', label: 'Help' },
            { key: 'F2', label: 'Sess', pane: 'sessions' },
            { key: 'F3', label: 'Ctx', pane: 'context' },
            { key: 'Tab', label: 'Next' },
            { key: '^C', label: 'Quit' },
        ];
        let line = '';
        let visLen = 0;
        for (const sc of shortcuts) {
            const hidden = sc.pane && !this.paneVisibility[sc.pane];
            const keyPart = hidden
                ? style(sc.key, ANSI.dim)
                : style(sc.key, ANSI.brightWhite, ANSI.bgBlue);
            const labelPart = hidden
                ? style(sc.label, ANSI.dim)
                : style(sc.label, ANSI.black, ANSI.bgCyan);
            line += keyPart + labelPart + ' ';
            visLen += sc.key.length + sc.label.length + 1;
        }
        // Right side: focused pane + status
        const rightParts = [];
        if (this.busy) {
            rightParts.push(style(' RUNNING ', ANSI.yellow, ANSI.bold));
        }
        if (this.followMode) {
            rightParts.push(style('↓', ANSI.cyan));
        }
        rightParts.push(style(` ${this.focusedPane} `, ANSI.inverse));
        const right = rightParts.join('');
        const rightVisLen = (this.busy ? 9 : 0) + (this.followMode ? 1 : 0) + this.focusedPane.length + 2;
        const gap = Math.max(0, width - visLen - rightVisLen);
        const fullLine = line + ' '.repeat(gap) + right;
        this.dirty = false;
        return [padRight(fullLine, width)];
    }
}
//# sourceMappingURL=status-bar.js.map