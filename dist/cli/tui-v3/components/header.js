/**
 * Header component — Borland-style logo bar (no menu).
 * Shows DeepClause logo on the left, session title on the right.
 */
import { style, ANSI, padRight } from '../util/ansi.js';
const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
const SPINNER_INTERVAL_MS = 80;
export class Header {
    dirty = true;
    minHeight = 1;
    flexGrow = 0;
    requestRenderFn;
    title = '';
    busy = false;
    spinnerFrame = 0;
    spinnerTimer = null;
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    setTitle(title) {
        if (this.title === title)
            return;
        this.title = title;
        this.invalidate();
    }
    setBusy(busy) {
        if (this.busy === busy)
            return;
        this.busy = busy;
        if (busy && !this.spinnerTimer) {
            this.spinnerTimer = setInterval(() => {
                this.spinnerFrame = (this.spinnerFrame + 1) % SPINNER_FRAMES.length;
                this.invalidate();
            }, SPINNER_INTERVAL_MS);
        }
        else if (!busy && this.spinnerTimer) {
            clearInterval(this.spinnerTimer);
            this.spinnerTimer = null;
        }
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        // Borland-style: bright white on blue background for logo bar
        const logo = ' ≡ DeepClause ';
        const spinner = this.busy ? ' ' + SPINNER_FRAMES[this.spinnerFrame] : '';
        const sessionInfo = this.title ? ` ${this.title}` : '';
        const right = sessionInfo + spinner + ' ';
        const leftLen = logo.length;
        const rightLen = right.length;
        const gap = Math.max(0, width - leftLen - rightLen);
        const line = style(logo, ANSI.bold, ANSI.brightWhite, ANSI.bgBlue)
            + style(' '.repeat(gap), ANSI.bgBlue)
            + style(right, ANSI.brightWhite, ANSI.bgBlue);
        this.dirty = false;
        return [padRight(line, width)];
    }
    dispose() {
        if (this.spinnerTimer) {
            clearInterval(this.spinnerTimer);
            this.spinnerTimer = null;
        }
    }
}
//# sourceMappingURL=header.js.map