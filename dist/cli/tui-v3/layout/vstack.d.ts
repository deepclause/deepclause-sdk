/**
 * VStack layout — arranges child components vertically.
 *
 * Allocates space based on minHeight and flexGrow:
 * 1. Each child gets its minHeight first
 * 2. Remaining space is distributed proportionally by flexGrow
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export interface VStackChild {
    component: Component;
    /** Minimum lines this child needs (default: component.minHeight ?? 1) */
    minHeight?: number;
    /** Flex grow factor (default: component.flexGrow ?? 0) */
    flexGrow?: number;
}
export declare class VStack implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private children;
    private requestRenderFn;
    private cachedRows;
    private cachedWidth;
    constructor(requestRender: RequestRender);
    /** Set the children of this stack. */
    setChildren(children: VStackChild[]): void;
    invalidate(): void;
    render(width: number): string[];
    /** Render with an explicit height constraint. */
    renderWithHeight(width: number, height: number): string[];
    handleInput(key: KeyEvent): boolean;
    private getTotalHeight;
    private allocateHeights;
    private hasAnyDirtyChild;
}
//# sourceMappingURL=vstack.d.ts.map