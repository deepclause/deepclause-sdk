/**
 * Header component — top bar showing session title, status, and spinner.
 */
import { style, ANSI, padRight } from '../util/ansi.js';
const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
const SPINNER_INTERVAL_MS = 80;
export class Header {
    dirty = true;
    minHeight = 1;
    flexGrow = 0;
    requestRenderFn;
    title = 'DeepClause';
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
        const spinner = this.busy ? SPINNER_FRAMES[this.spinnerFrame] + ' ' : '';
        const titleText = `${spinner}${this.title}`;
        const right = 'DeepClause TUI';
        const left = style(titleText, ANSI.bold);
        const rightStyled = style(right, ANSI.dim);
        // Compose: [left ... right]
        const leftLen = spinner.length + this.title.length;
        const rightLen = right.length;
        const gap = Math.max(1, width - leftLen - rightLen);
        const line = left + ' '.repeat(gap) + rightStyled;
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