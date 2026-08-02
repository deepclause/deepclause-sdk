/**
 * Help overlay dialog — shows keyboard shortcuts and commands.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export declare class HelpDialog implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private visible;
    private onClose;
    constructor(requestRender: RequestRender);
    show(): void;
    hide(): void;
    setOnClose(fn: () => void): void;
    get isVisible(): boolean;
    invalidate(): void;
    render(width: number): string[];
    handleInput(key: KeyEvent): boolean;
}
//# sourceMappingURL=help.d.ts.map