/**
 * Input component — single-line text input with cursor.
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';
import { style, ANSI, padRight } from '../util/ansi.js';

export class Input implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private value = '';
  private cursorPos = 0;
  private _active = true;
  private prompt = '> ';
  private onSubmit: ((text: string) => void) | null = null;
  private onEscape: (() => void) | null = null;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the submit callback. */
  setOnSubmit(fn: (text: string) => void): void {
    this.onSubmit = fn;
  }

  /** Set the escape callback. */
  setOnEscape(fn: () => void): void {
    this.onEscape = fn;
  }

  /** Set the prompt prefix. */
  setPrompt(prompt: string): void {
    this.prompt = prompt;
    this.invalidate();
  }

  /** Set whether the input is active (accepting input). */
  setActive(active: boolean): void {
    this._active = active;
    this.invalidate();
  }

  /** Get the current value. */
  getValue(): string {
    return this.value;
  }

  /** Set the value programmatically. */
  setValue(value: string): void {
    this.value = value;
    this.cursorPos = value.length;
    this.invalidate();
  }

  /** Clear the input. */
  clear(): void {
    this.value = '';
    this.cursorPos = 0;
    this.invalidate();
  }

  /** Get cursor position for the renderer to show the cursor. */
  getCursorCol(): number {
    return this.prompt.length + this.cursorPos;
  }

  get active(): boolean {
    return this._active;
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const prefix = style(this.prompt, ANSI.cyan);
    const text = this.value;

    // Show visible portion of text if it exceeds width
    const maxTextWidth = width - this.prompt.length;
    let visibleText: string;
    let visibleCursor = this.cursorPos;

    if (text.length > maxTextWidth) {
      // Scroll text to keep cursor visible
      const scrollOffset = Math.max(0, this.cursorPos - maxTextWidth + 1);
      visibleText = text.slice(scrollOffset, scrollOffset + maxTextWidth);
      visibleCursor = this.cursorPos - scrollOffset;
    } else {
      visibleText = text;
    }

    // Insert a cursor character marker for rendering
    const beforeCursor = visibleText.slice(0, visibleCursor);
    const cursorChar = visibleText[visibleCursor] ?? ' ';
    const afterCursor = visibleText.slice(visibleCursor + 1);

    const line = this._active
      ? prefix + beforeCursor + style(cursorChar, ANSI.inverse) + afterCursor
      : prefix + style(visibleText, ANSI.dim);

    this.dirty = false;
    return [padRight(line, width)];
  }

  handleInput(key: KeyEvent): boolean {
    if (!this._active) return false;

    // Submit
    if (key.name === 'return') {
      const val = this.value;
      this.clear();
      if (this.onSubmit) this.onSubmit(val);
      return true;
    }

    // Escape
    if (key.name === 'escape') {
      if (this.onEscape) this.onEscape();
      return true;
    }

    // Backspace
    if (key.name === 'backspace') {
      if (this.cursorPos > 0) {
        this.value = this.value.slice(0, this.cursorPos - 1) + this.value.slice(this.cursorPos);
        this.cursorPos--;
        this.invalidate();
      }
      return true;
    }

    // Delete
    if (key.name === 'delete') {
      if (this.cursorPos < this.value.length) {
        this.value = this.value.slice(0, this.cursorPos) + this.value.slice(this.cursorPos + 1);
        this.invalidate();
      }
      return true;
    }

    // Cursor movement
    if (key.name === 'left') {
      if (this.cursorPos > 0) {
        this.cursorPos--;
        this.invalidate();
      }
      return true;
    }
    if (key.name === 'right') {
      if (this.cursorPos < this.value.length) {
        this.cursorPos++;
        this.invalidate();
      }
      return true;
    }
    if (key.name === 'home' || (key.ctrl && key.name === 'a')) {
      this.cursorPos = 0;
      this.invalidate();
      return true;
    }
    if (key.name === 'end' || (key.ctrl && key.name === 'e')) {
      this.cursorPos = this.value.length;
      this.invalidate();
      return true;
    }

    // Ctrl+U — clear line
    if (key.ctrl && key.name === 'u') {
      this.clear();
      return true;
    }

    // Ctrl+K — kill to end of line
    if (key.ctrl && key.name === 'k') {
      this.value = this.value.slice(0, this.cursorPos);
      this.invalidate();
      return true;
    }

    // Regular character input
    if (key.sequence && key.sequence.length === 1 && !key.ctrl && !key.meta) {
      const ch = key.sequence;
      if (ch.charCodeAt(0) >= 32) { // Printable
        this.value = this.value.slice(0, this.cursorPos) + ch + this.value.slice(this.cursorPos);
        this.cursorPos++;
        this.invalidate();
        return true;
      }
    }

    return false;
  }
}
