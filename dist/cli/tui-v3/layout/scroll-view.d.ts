/**
 * ScrollView — a vertically scrolling container that shows a window
 * into its child's content. Supports follow-end mode for streaming content.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export declare class ScrollView implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private child;
    private requestRenderFn;
    private scrollOffset;
    private viewHeight;
    private contentHeight;
    private followEnd;
    constructor(child: Component, requestRender: RequestRender);
    /** Whether auto-scrolling to the end is active. */
    get isFollowing(): boolean;
    /** Set follow-end mode. */
    setFollow(follow: boolean): void;
    /** Toggle follow-end mode. */
    toggleFollow(): void;
    /** Scroll to the bottom. */
    scrollToEnd(): void;
    /** Scroll up by n lines. */
    scrollUp(n?: number): void;
    /** Scroll down by n lines. */
    scrollDown(n?: number): void;
    /** Page up. */
    pageUp(): void;
    /** Page down. */
    pageDown(): void;
    invalidate(): void;
    render(width: number): string[];
    /** Render with explicit height (used by layout). */
    renderWithHeight(width: number, height: number): string[];
    handleInput(key: KeyEvent): boolean;
    private getVisibleRows;
}
//# sourceMappingURL=scroll-view.d.ts.map