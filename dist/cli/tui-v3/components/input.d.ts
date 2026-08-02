/**
 * Input component — single-line text input with cursor.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export declare class Input implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private value;
    private cursorPos;
    private _active;
    private prompt;
    private onSubmit;
    private onEscape;
    constructor(requestRender: RequestRender);
    /** Set the submit callback. */
    setOnSubmit(fn: (text: string) => void): void;
    /** Set the escape callback. */
    setOnEscape(fn: () => void): void;
    /** Set the prompt prefix. */
    setPrompt(prompt: string): void;
    /** Set whether the input is active (accepting input). */
    setActive(active: boolean): void;
    /** Get the current value. */
    getValue(): string;
    /** Set the value programmatically. */
    setValue(value: string): void;
    /** Clear the input. */
    clear(): void;
    /** Get cursor position for the renderer to show the cursor. */
    getCursorCol(): number;
    get active(): boolean;
    invalidate(): void;
    render(width: number): string[];
    handleInput(key: KeyEvent): boolean;
}
//# sourceMappingURL=input.d.ts.map