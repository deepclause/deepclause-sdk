/**
 * Event loop for TUI v3.
 *
 * Single event loop architecture:
 * - Input events (keypresses) are parsed and dispatched
 * - State mutations happen synchronously in response to events
 * - Render is debounced: multiple invalidations in the same tick produce one render
 * - No interval timers — only renders when something changes
 */
import { Renderer, type RendererOptions } from './renderer.js';
import type { Component } from './types.js';
export interface EventLoopOptions extends RendererOptions {
    /** The root component to render */
    root: Component;
    /** stdin stream (defaults to process.stdin) */
    stdin?: NodeJS.ReadStream & {
        setRawMode?(mode: boolean): NodeJS.ReadStream;
    };
    /** Called when the user presses Ctrl+C and no component handles it */
    onExit?: () => void;
}
/**
 * The event loop manages:
 * 1. Terminal input → keypress parsing → dispatch to root component
 * 2. Debounced rendering: requestRender() coalesces multiple invalidations
 * 3. Resize handling
 * 4. Clean shutdown
 */
export declare class EventLoop {
    private renderer;
    private root;
    private stdin;
    private stdout;
    private onExit;
    private running;
    private renderScheduled;
    private exitPromise;
    private exitResolve;
    constructor(options: EventLoopOptions);
    /** Start the event loop. Returns a promise that resolves when the loop stops. */
    start(): Promise<void>;
    /** Stop the event loop and restore the terminal. */
    stop(): void;
    /**
     * Schedule a render on the next microtask.
     * Multiple calls in the same tick are coalesced into a single render.
     * This is the "requestAnimationFrame" equivalent for the terminal.
     */
    requestRender: () => void;
    /** Get the renderer instance (for cursor control). */
    getRenderer(): Renderer;
    /** Perform a synchronous render. */
    private doRender;
    /** Handle a keypress from stdin. */
    private handleKeypress;
    /** Handle terminal resize. */
    private handleResize;
}
//# sourceMappingURL=event-loop.d.ts.map