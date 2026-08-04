/**
 * Markdown component — streaming-safe markdown renderer.
 *
 * Renders markdown content as styled terminal text.
 * Designed to work with streaming content where the text is progressively appended.
 */
import type { Component, RequestRender } from '../types.js';
export declare class Markdown implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private content;
    private cachedRows;
    private cachedWidth;
    constructor(requestRender: RequestRender);
    /** Set the markdown content. */
    setContent(content: string): void;
    /** Append text (for streaming). */
    append(text: string): void;
    invalidate(): void;
    render(width: number): string[];
    private renderMarkdown;
}
//# sourceMappingURL=markdown.d.ts.map