/**
 * Header component — Borland-style logo bar (no menu).
 * Shows DeepClause logo on the left, session title on the right.
 */
import type { Component, RequestRender } from '../types.js';
export declare class Header implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private title;
    private busy;
    private spinnerFrame;
    private spinnerTimer;
    constructor(requestRender: RequestRender);
    setTitle(title: string): void;
    setBusy(busy: boolean): void;
    invalidate(): void;
    render(width: number): string[];
    dispose(): void;
}
//# sourceMappingURL=header.d.ts.map