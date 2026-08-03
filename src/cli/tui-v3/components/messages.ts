/**
 * Messages component — chat message list with per-message render cache.
 *
 * Key optimization: historical messages are cached and never re-rendered
 * during streaming. Only the streaming (latest) message re-renders.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, padRight, truncate } from '../util/ansi.js';
import type { ExecutionPreview } from '../state/session-state.js';

export interface ChatMessage {
  role: 'user' | 'assistant' | 'system';
  content: string;
  pending?: boolean;
  error?: boolean;
}

interface CachedMessage {
  message: ChatMessage;
  rows: string[];
  width: number;
}

function sanitizeTerminalText(text: string): string {
  return text
    .replace(/\x1b\][^\x07]*(?:\x07|\x1b\\)/g, '')
    .replace(/\x1b\[[0-?]*[ -/]*[@-~]/g, '')
    .replace(/[\x00-\x08\x0b-\x1f\x7f]/g, '');
}

export class Messages implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private requestRenderFn: RequestRender;
  private messages: ChatMessage[] = [];
  private executionPreview: ExecutionPreview | null = null;
  private cache: CachedMessage[] = [];

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the message list. */
  setMessages(messages: ChatMessage[]): void {
    this.messages = messages;
    // Invalidate cache for any changed messages
    this.cache = this.cache.slice(0, Math.min(this.cache.length, messages.length));
    this.invalidate();
  }

  /** Append a message. */
  appendMessage(message: ChatMessage): void {
    this.messages.push(message);
    this.invalidate();
  }

  /** Set the current or most recently completed DML execution preview. */
  setExecutionPreview(preview: ExecutionPreview | null): void {
    this.executionPreview = preview;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const allRows: string[] = [];

    // Render each message (with caching)
    const previewBefore = this.executionPreview?.complete
      && this.messages.at(-1)?.role === 'assistant'
      ? this.messages.length - 1
      : this.messages.length;
    for (let i = 0; i < this.messages.length; i++) {
      if (i === previewBefore && this.executionPreview) {
        allRows.push(...this.renderExecution(this.executionPreview, width));
      }
      const msg = this.messages[i];
      const cached = this.cache[i];

      if (cached && cached.message === msg && cached.width === width) {
        // Use cached render
        allRows.push(...cached.rows);
      } else {
        // Render and cache
        const rows = this.renderMessage(msg, width);
        this.cache[i] = { message: msg, rows, width };
        allRows.push(...rows);
      }
    }

    if (previewBefore === this.messages.length && this.executionPreview) {
      allRows.push(...this.renderExecution(this.executionPreview, width));
    }

    // If no messages, show a placeholder
    if (allRows.length === 0) {
      allRows.push('');
      allRows.push(style('  No messages yet. Type a message below to start.', ANSI.dim));
      allRows.push('');
    }

    this.dirty = false;
    return allRows;
  }

  private renderExecution(preview: ExecutionPreview, width: number): string[] {
    const boxWidth = Math.max(12, width - 2);
    const innerWidth = Math.max(8, boxWidth - 2);
    const state = preview.complete ? 'complete' : 'running';
    const marker = preview.complete && !preview.expanded ? '▸' : '▾';
    const title = truncate(` ${marker} ${sanitizeTerminalText(preview.label)} · ${state} `, innerWidth);
    const rows = [
      style(`  ┌${title}${'─'.repeat(Math.max(0, innerWidth - title.length))}┐`, ANSI.cyan),
    ];
    if (preview.complete && !preview.expanded) {
      rows.push(
        style('  │', ANSI.cyan)
        + style(padRight(' Ctrl+E to inspect', innerWidth), ANSI.dim)
        + style('│', ANSI.cyan),
      );
      rows.push(style(`  └${'─'.repeat(innerWidth)}┘`, ANSI.cyan));
      rows.push('');
      return rows;
    }
    const content = sanitizeTerminalText(preview.content || 'Waiting for model output…');
    const contentWidth = Math.max(1, innerWidth - 2);
    const lines = preview.complete
      ? this.wrapText(content, contentWidth)
      : content.split('\n').filter(Boolean).slice(-3).map((line) => truncate(line, contentWidth));
    for (const line of lines) {
      rows.push(
        style('  │', ANSI.cyan)
        + style(padRight(` ${line}`, innerWidth), ANSI.dim)
        + style('│', ANSI.cyan),
      );
    }
    rows.push(style(`  └${'─'.repeat(innerWidth)}┘`, ANSI.cyan));
    rows.push('');
    return rows;
  }

  private renderMessage(msg: ChatMessage, width: number): string[] {
    const rows: string[] = [];
    const contentWidth = Math.max(10, width - 4); // 2 chars indent + 2 margin

    // Role header
    const roleLabel = this.getRoleLabel(msg);
    rows.push(roleLabel);

    // Wrap content into lines
    const lines = this.wrapText(sanitizeTerminalText(msg.content), contentWidth);
    for (const line of lines) {
      const styled = msg.error
        ? style(`  ${line}`, ANSI.red)
        : msg.pending
          ? style(`  ${line}`, ANSI.dim)
          : `  ${line}`;
      rows.push(styled);
    }

    // Blank separator
    rows.push('');

    return rows;
  }

  private getRoleLabel(msg: ChatMessage): string {
    switch (msg.role) {
      case 'user':
        return style('  You:', ANSI.bold, ANSI.green);
      case 'assistant':
        return msg.pending
          ? style('  Assistant: (streaming…)', ANSI.bold, ANSI.blue)
          : style('  Assistant:', ANSI.bold, ANSI.blue);
      case 'system':
        return msg.error
          ? style('  System (error):', ANSI.bold, ANSI.red)
          : style('  System:', ANSI.bold, ANSI.yellow);
    }
  }

  private wrapText(text: string, maxWidth: number): string[] {
    const lines: string[] = [];
    const rawLines = text.split('\n');

    for (const rawLine of rawLines) {
      if (rawLine.length <= maxWidth) {
        lines.push(rawLine);
      } else {
        // Simple word wrapping
        let remaining = rawLine;
        while (remaining.length > maxWidth) {
          let breakAt = remaining.lastIndexOf(' ', maxWidth);
          if (breakAt <= 0) breakAt = maxWidth;
          lines.push(remaining.slice(0, breakAt));
          remaining = remaining.slice(breakAt).trimStart();
        }
        if (remaining) lines.push(remaining);
      }
    }

    return lines;
  }
}
