/**
 * Markdown component — streaming-safe markdown renderer.
 *
 * Renders markdown content as styled terminal text.
 * Designed to work with streaming content where the text is progressively appended.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI } from '../util/ansi.js';

export class Markdown implements Component {
  dirty = true;
  minHeight = 0;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private content = '';
  private cachedRows: string[] | null = null;
  private cachedWidth = 0;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the markdown content. */
  setContent(content: string): void {
    if (this.content === content) return;
    this.content = content;
    this.cachedRows = null;
    this.invalidate();
  }

  /** Append text (for streaming). */
  append(text: string): void {
    this.content += text;
    this.cachedRows = null;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    if (this.cachedRows && this.cachedWidth === width) {
      this.dirty = false;
      return this.cachedRows;
    }

    const rows = this.renderMarkdown(this.content, width);
    this.cachedRows = rows;
    this.cachedWidth = width;
    this.dirty = false;
    return rows;
  }

  private renderMarkdown(text: string, width: number): string[] {
    const lines = text.split('\n');
    const rows: string[] = [];
    let inCodeBlock = false;

    for (const line of lines) {
      // Code block fences
      if (line.startsWith('```')) {
        inCodeBlock = !inCodeBlock;
        if (inCodeBlock) {
          const lang = line.slice(3).trim();
          rows.push(style(`┌─${lang ? ` ${lang} ` : ''}${'─'.repeat(Math.max(0, width - 4 - (lang?.length ?? 0)))}`, ANSI.dim));
        } else {
          rows.push(style(`└${'─'.repeat(Math.max(0, width - 2))}`, ANSI.dim));
        }
        continue;
      }

      if (inCodeBlock) {
        // Code content — dim styling
        rows.push(style(`│ ${line}`, ANSI.dim));
        continue;
      }

      // Headers
      const headerMatch = line.match(/^(#{1,6})\s+(.*)/);
      if (headerMatch) {
        const level = headerMatch[1].length;
        const text = headerMatch[2];
        if (level <= 2) {
          rows.push(style(text, ANSI.bold, ANSI.brightWhite));
        } else {
          rows.push(style(text, ANSI.bold));
        }
        continue;
      }

      // Horizontal rule
      if (/^---+$/.test(line) || /^\*\*\*+$/.test(line)) {
        rows.push(style('─'.repeat(width), ANSI.dim));
        continue;
      }

      // Bold
      let processed = line.replace(/\*\*(.*?)\*\*/g, (_, text) => style(text, ANSI.bold));

      // Italic
      processed = processed.replace(/\*(.*?)\*/g, (_, text) => style(text, ANSI.italic));

      // Inline code
      processed = processed.replace(/`([^`]+)`/g, (_, text) => style(text, ANSI.dim, ANSI.cyan));

      // Bullet points
      if (processed.match(/^\s*[-*]\s/)) {
        processed = processed.replace(/^(\s*)[-*]\s/, '$1• ');
      }

      // Wrap and push
      if (processed.length <= width) {
        rows.push(processed);
      } else {
        // Simple wrap
        let remaining = processed;
        while (remaining.length > width) {
          rows.push(remaining.slice(0, width));
          remaining = remaining.slice(width);
        }
        if (remaining) rows.push(remaining);
      }
    }

    return rows;
  }
}
