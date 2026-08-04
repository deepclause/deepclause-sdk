/**
 * Spinner component — animated loading indicator.
 * Uses invalidation-based rendering with a timer only when active.
 */
const FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
const FRAME_INTERVAL_MS = 80;
export class Spinner {
    dirty = true;
    minHeight = 0;
    flexGrow = 0;
    requestRenderFn;
    frameIndex = 0;
    timer = null;
    _active = false;
    label = '';
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    /** Start the spinner animation. */
    start(label = '') {
        if (this._active)
            return;
        this._active = true;
        this.label = label;
        this.frameIndex = 0;
        this.timer = setInterval(() => {
            this.frameIndex = (this.frameIndex + 1) % FRAMES.length;
            this.invalidate();
        }, FRAME_INTERVAL_MS);
        this.invalidate();
    }
    /** Stop the spinner animation. */
    stop() {
        if (!this._active)
            return;
        this._active = false;
        if (this.timer) {
            clearInterval(this.timer);
            this.timer = null;
        }
        this.invalidate();
    }
    /** Update the label text. */
    setLabel(label) {
        this.label = label;
        this.invalidate();
    }
    get active() {
        return this._active;
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(_width) {
        if (!this._active) {
            this.dirty = false;
            return [];
        }
        const frame = FRAMES[this.frameIndex];
        this.dirty = false;
        return [this.label ? `${frame} ${this.label}` : frame];
    }
    /** Cleanup: stop any timers. */
    dispose() {
        this.stop();
    }
}
//# sourceMappingURL=spinner.js.map