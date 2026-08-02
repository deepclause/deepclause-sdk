/**
 * Confirm dialog — yes/no confirmation overlay.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export declare class ConfirmDialog implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private visible;
    private message;
    private selectedYes;
    private onConfirm;
    constructor(requestRender: RequestRender);
    show(message: string): void;
    hide(): void;
    setOnConfirm(fn: (confirmed: boolean) => void): void;
    get isVisible(): boolean;
    invalidate(): void;
    render(width: number): string[];
    handleInput(key: KeyEvent): boolean;
}
//# sourceMappingURL=confirm.d.ts.map