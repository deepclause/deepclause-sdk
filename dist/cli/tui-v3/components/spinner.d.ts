/**
 * Spinner component — animated loading indicator.
 * Uses invalidation-based rendering with a timer only when active.
 */
import type { Component, RequestRender } from '../types.js';
export declare class Spinner implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private frameIndex;
    private timer;
    private _active;
    private label;
    constructor(requestRender: RequestRender);
    /** Start the spinner animation. */
    start(label?: string): void;
    /** Stop the spinner animation. */
    stop(): void;
    /** Update the label text. */
    setLabel(label: string): void;
    get active(): boolean;
    invalidate(): void;
    render(_width: number): string[];
    /** Cleanup: stop any timers. */
    dispose(): void;
}
//# sourceMappingURL=spinner.d.ts.map