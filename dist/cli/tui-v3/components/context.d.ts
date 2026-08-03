/**
 * Context pane component — displays token usage and cost information per model.
 */
import type { Component, RequestRender } from '../types.js';
export interface TokenUsage {
    input: number;
    output: number;
}
export declare class Context implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private tokenUsage;
    private contextTokens;
    constructor(requestRender: RequestRender);
    /** Set token usage data. */
    setTokenUsage(usage: Record<string, TokenUsage>): void;
    /** Set the approximate number of tokens currently held in session context. */
    setContextTokens(tokens: number): void;
    invalidate(): void;
    render(width: number): string[];
}
//# sourceMappingURL=context.d.ts.map