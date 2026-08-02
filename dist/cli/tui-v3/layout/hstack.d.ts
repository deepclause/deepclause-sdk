/**
 * HStack layout — arranges child components horizontally.
 *
 * Each child gets a width allocation based on its flex properties.
 * Renders each child within its column allocation and joins them side-by-side.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export interface HStackChild {
    component: Component;
    /** Fixed width (overrides flex) */
    width?: number;
    /** Flex grow factor (default: 1) */
    flexGrow?: number;
}
export declare class HStack implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private children;
    private requestRenderFn;
    constructor(requestRender: RequestRender);
    setChildren(children: HStackChild[]): void;
    invalidate(): void;
    render(width: number): string[];
    /** Render with an explicit height constraint (returns first `height` rows). */
    renderWithHeight(width: number, height: number): string[];
    handleInput(key: KeyEvent): boolean;
    private allocateWidths;
}
//# sourceMappingURL=hstack.d.ts.map