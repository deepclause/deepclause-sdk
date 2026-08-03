/**
 * Event loop for TUI v3.
 *
 * Single event loop architecture:
 * - Input events (keypresses) are parsed and dispatched
 * - State mutations happen synchronously in response to events
 * - Render is debounced: multiple invalidations in the same tick produce one render
 * - No interval timers — only renders when something changes
 */
import { emitKeypressEvents } from 'readline';
import { Renderer } from './renderer.js';
/**
 * The event loop manages:
 * 1. Terminal input → keypress parsing → dispatch to root component
 * 2. Debounced rendering: requestRender() coalesces multiple invalidations
 * 3. Resize handling
 * 4. Clean shutdown
 */
export class EventLoop {
    renderer;
    root;
    stdin;
    stdout;
    onExit;
    running = false;
    renderScheduled = false;
    exitPromise = null;
    exitResolve = null;
    constructor(options) {
        this.root = options.root;
        this.stdin = (options.stdin ?? process.stdin);
        this.stdout = options.stdout ?? process.stdout;
        this.onExit = options.onExit ?? (() => this.stop());
        this.renderer = new Renderer(options);
    }
    /** Start the event loop. Returns a promise that resolves when the loop stops. */
    start() {
        if (this.running)
            return this.exitPromise;
        this.running = true;
        this.exitPromise = new Promise((resolve) => {
            this.exitResolve = resolve;
        });
        // Enter alternate screen
        this.renderer.enter();
        // Setup raw mode for keypress events
        if (this.stdin.isTTY && this.stdin.setRawMode) {
            this.stdin.setRawMode(true);
        }
        this.stdin.resume();
        emitKeypressEvents(this.stdin);
        // Listen for keypresses
        this.stdin.on('keypress', this.handleKeypress);
        // Listen for resize
        this.stdout.on('resize', this.handleResize);
        // Initial render
        this.requestRender();
        return this.exitPromise;
    }
    /** Stop the event loop and restore the terminal. */
    stop() {
        if (!this.running)
            return;
        this.running = false;
        // Remove listeners
        this.stdin.off('keypress', this.handleKeypress);
        this.stdout.off('resize', this.handleResize);
        // Restore terminal
        if (this.stdin.isTTY && this.stdin.setRawMode) {
            this.stdin.setRawMode(false);
        }
        this.stdin.pause();
        this.renderer.exit();
        if (this.exitResolve) {
            this.exitResolve();
            this.exitResolve = null;
        }
    }
    /**
     * Schedule a render on the next microtask.
     * Multiple calls in the same tick are coalesced into a single render.
     * This is the "requestAnimationFrame" equivalent for the terminal.
     */
    requestRender = () => {
        if (!this.running)
            return;
        if (this.renderScheduled)
            return;
        this.renderScheduled = true;
        // Use setImmediate for minimal latency while still coalescing
        setImmediate(() => {
            this.renderScheduled = false;
            if (this.running) {
                this.doRender();
            }
        });
    };
    /** Get the renderer instance (for cursor control). */
    getRenderer() {
        return this.renderer;
    }
    /** Perform a synchronous render. */
    doRender() {
        const width = this.renderer.cols;
        const height = this.renderer.rows;
        // Get rows from root component
        const rows = this.root.render(width);
        // Pad or truncate to fill the terminal height
        const screen = [];
        for (let i = 0; i < height; i++) {
            screen.push(rows[i] ?? '');
        }
        this.renderer.render(screen);
        this.root.dirty = false;
    }
    /** Handle a keypress from stdin. */
    handleKeypress = (_ch, key) => {
        if (!key)
            return;
        const event = {
            name: key.name ?? '',
            sequence: key.sequence ?? '',
            ctrl: key.ctrl ?? false,
            meta: key.meta ?? false,
            shift: key.shift ?? false,
        };
        // Ctrl+C fallback — if no component handles it, exit
        if (event.ctrl && event.name === 'c') {
            if (this.root.handleInput && this.root.handleInput(event)) {
                this.requestRender();
                return;
            }
            this.onExit();
            return;
        }
        // Dispatch to root component
        if (this.root.handleInput) {
            const consumed = this.root.handleInput(event);
            if (consumed) {
                this.requestRender();
            }
        }
    };
    /** Handle terminal resize. */
    handleResize = () => {
        const rows = this.stdout.rows || 24;
        const cols = this.stdout.columns || 80;
        this.renderer.resize(rows, cols);
        this.requestRender();
    };
}
//# sourceMappingURL=event-loop.js.map