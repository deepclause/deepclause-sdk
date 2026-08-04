/**
 * Input component — multiline text editor with cursor navigation.
 * Supports Enter for newline, Ctrl+Enter / Ctrl+D to submit.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export declare class Input implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private lines;
    private cursorRow;
    private cursorCol;
    private _active;
    private prompt;
    private maxVisibleLines;
    private scrollOffset;
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
    /** Get the current multiline value. */
    getValue(): string;
    /** Set the value programmatically. */
    setValue(value: string): void;
    /** Clear the input. */
    clear(): void;
    get active(): boolean;
    /** Current rendered height: borders plus the visible editor lines. */
    get height(): number;
    invalidate(): void;
    render(width: number): string[];
    handleInput(key: KeyEvent): boolean;
    private submit;
    private insertNewline;
}
//# sourceMappingURL=input.d.ts.map