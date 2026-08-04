/**
 * Help overlay dialog — shows keyboard shortcuts and commands.
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';
import { style, ANSI, center, clipAnsi, padRight } from '../util/ansi.js';

export class HelpDialog implements Component {
  dirty = true;
  minHeight = 0;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private visible = false;
  private onClose: (() => void) | null = null;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  show(): void {
    this.visible = true;
    this.invalidate();
  }

  hide(): void {
    this.visible = false;
    this.invalidate();
  }

  setOnClose(fn: () => void): void {
    this.onClose = fn;
  }

  get isVisible(): boolean {
    return this.visible;
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    if (!this.visible) return [];

    const maxWidth = Math.max(4, Math.min(64, width - 4));
    const rows: string[] = [];
    const border = '─'.repeat(maxWidth - 2);

    rows.push(`┌${border}┐`);
    rows.push(`│${center(style('Help', ANSI.bold), maxWidth - 2)}│`);
    rows.push(`├${border}┤`);
    rows.push(`│${padLine('', maxWidth - 2)}│`);
    rows.push(`│${padLine(style(' Keyboard Shortcuts:', ANSI.bold), maxWidth - 2)}│`);
    rows.push(`│${padLine('', maxWidth - 2)}│`);
    rows.push(`│${padLine('  Tab          Cycle panes', maxWidth - 2)}│`);
    rows.push(`│${padLine('  F2/F3       Focus; press again to hide', maxWidth - 2)}│`);
    rows.push(`│${padLine('  ↑/↓ + Enter Select focused session', maxWidth - 2)}│`);
    rows.push(`│${padLine('  Ctrl+C       Cancel; press again to quit', maxWidth - 2)}│`);
    rows.push(`│${padLine('  Ctrl+U       Clear input', maxWidth - 2)}│`);
    rows.push(`│${padLine('  Ctrl+E       Inspect last DML execution', maxWidth - 2)}│`);
    rows.push(`│${padLine('  Shift+↑/↓    Scroll messages', maxWidth - 2)}│`);
    rows.push(`│${padLine('  PgUp/PgDn    Page scroll', maxWidth - 2)}│`);
    rows.push(`│${padLine('  Home/End     Top/Bottom of messages', maxWidth - 2)}│`);
    rows.push(`│${padLine('  ?            Toggle this help', maxWidth - 2)}│`);
    rows.push(`│${padLine('', maxWidth - 2)}│`);
    rows.push(`│${padLine(style(' Commands:', ANSI.bold), maxWidth - 2)}│`);
    rows.push(`│${padLine('', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /new [title] Create new session', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /sessions    Toggle session pane', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /help        Show this help', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /run FILE …  Run a DML file', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /compile SPEC Create a skill', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /set-model … Change model settings', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /SKILL …     Run a skill or plan', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /cancel      Cancel current execution', maxWidth - 2)}│`);
    rows.push(`│${padLine('  /exit        Quit the TUI', maxWidth - 2)}│`);
    rows.push(`│${padLine('', maxWidth - 2)}│`);
    rows.push(`│${center(style('Press Esc or ? to close', ANSI.dim), maxWidth - 2)}│`);
    rows.push(`└${border}┘`);

    this.dirty = false;
    return rows;
  }

  handleInput(key: KeyEvent): boolean {
    if (!this.visible) return false;

    if (key.name === 'escape' || key.name === 'esc' || key.sequence === '\x1b' || key.sequence === '?') {
      this.hide();
      if (this.onClose) this.onClose();
      return true;
    }

    // Consume all keys when help is shown
    return true;
  }
}

function padLine(text: string, width: number): string {
  return padRight(clipAnsi(text, width), width);
}
