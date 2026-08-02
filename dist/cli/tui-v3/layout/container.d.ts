/**
 * Container — wraps a child with optional padding and border.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export interface ContainerOptions {
    /** Padding (top, right, bottom, left) */
    padding?: [number, number, number, number];
    /** Whether to draw a box border */
    border?: boolean;
    /** Border style characters */
    borderChars?: BorderChars;
    /** Title shown in the top border */
    title?: string;
}
export interface BorderChars {
    topLeft: string;
    topRight: string;
    bottomLeft: string;
    bottomRight: string;
    horizontal: string;
    vertical: string;
}
export declare class Container implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private child;
    private requestRenderFn;
    private options;
    constructor(child: Component, requestRender: RequestRender, options?: ContainerOptions);
    /** Update container options (e.g. title). */
    setOptions(options: Partial<ContainerOptions>): void;
    invalidate(): void;
    render(width: number): string[];
    handleInput(key: KeyEvent): boolean;
}
//# sourceMappingURL=container.d.ts.map