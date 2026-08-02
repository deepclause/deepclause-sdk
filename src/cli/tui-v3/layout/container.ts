/**
 * Container — wraps a child with optional padding and border.
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';
import { stripAnsi } from '../util/ansi.js';

export interface ContainerOptions {
  /** Padding (top, right, bottom, left) */
  padding?: [number, number, number, number];
  /** Whether to draw a box border */
  border?: boolean;
  /** Border style characters */
  borderChars?: BorderChars;
  /** Title shown in the top border */
  title?: string;
}

export interface BorderChars {
  topLeft: string;
  topRight: string;
  bottomLeft: string;
  bottomRight: string;
  horizontal: string;
  vertical: string;
}

const DEFAULT_BORDER: BorderChars = {
  topLeft: '┌',
  topRight: '┐',
  bottomLeft: '└',
  bottomRight: '┘',
  horizontal: '─',
  vertical: '│',
};

export class Container implements Component {
  dirty = true;
  minHeight = 0;
  flexGrow = 0;

  private child: Component;
  private requestRenderFn: RequestRender;
  private options: ContainerOptions;

  constructor(child: Component, requestRender: RequestRender, options: ContainerOptions = {}) {
    this.child = child;
    this.requestRenderFn = requestRender;
    this.options = options;
  }

  /** Update container options (e.g. title). */
  setOptions(options: Partial<ContainerOptions>): void {
    Object.assign(this.options, options);
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const { padding = [0, 0, 0, 0], border, borderChars = DEFAULT_BORDER, title } = this.options;
    const [pt, pr, pb, pl] = padding;

    const borderWidth = border ? 2 : 0; // left + right
    const innerWidth = Math.max(0, width - pl - pr - borderWidth);

    // Render child
    const childRows = this.child.render(innerWidth);
    this.child.dirty = false;

    const rows: string[] = [];

    // Top border
    if (border) {
      let topLine = borderChars.topLeft;
      if (title) {
        const titleStr = ` ${title} `;
        topLine += titleStr;
        topLine += borderChars.horizontal.repeat(Math.max(0, width - 2 - stripAnsi(titleStr).length));
      } else {
        topLine += borderChars.horizontal.repeat(Math.max(0, width - 2));
      }
      topLine += borderChars.topRight;
      rows.push(topLine);
    }

    // Top padding
    for (let i = 0; i < pt; i++) {
      rows.push(border
        ? borderChars.vertical + ' '.repeat(width - 2) + borderChars.vertical
        : ' '.repeat(width));
    }

    // Content rows
    for (const row of childRows) {
      const leftPad = ' '.repeat(pl);
      const rightPad = ' '.repeat(pr);
      if (border) {
        rows.push(borderChars.vertical + leftPad + padToWidth(row, innerWidth) + rightPad + borderChars.vertical);
      } else {
        rows.push(leftPad + row + rightPad);
      }
    }

    // Bottom padding
    for (let i = 0; i < pb; i++) {
      rows.push(border
        ? borderChars.vertical + ' '.repeat(width - 2) + borderChars.vertical
        : ' '.repeat(width));
    }

    // Bottom border
    if (border) {
      rows.push(
        borderChars.bottomLeft +
        borderChars.horizontal.repeat(Math.max(0, width - 2)) +
        borderChars.bottomRight,
      );
    }

    this.dirty = false;
    return rows;
  }

  handleInput(key: KeyEvent): boolean {
    if (this.child.handleInput) {
      return this.child.handleInput(key);
    }
    return false;
  }
}

function padToWidth(text: string, width: number): string {
  const visible = stripAnsi(text).length;
  if (visible >= width) return text;
  return text + ' '.repeat(width - visible);
}
