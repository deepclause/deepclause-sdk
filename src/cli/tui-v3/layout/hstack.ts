/**
 * HStack layout — arranges child components horizontally.
 *
 * Each child gets a width allocation based on its flex properties.
 * Renders each child within its column allocation and joins them side-by-side.
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';
import { stripAnsi } from '../util/ansi.js';

export interface HStackChild {
  component: Component;
  /** Fixed width (overrides flex) */
  width?: number;
  /** Flex grow factor (default: 1) */
  flexGrow?: number;
}

export class HStack implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private children: HStackChild[] = [];
  private requestRenderFn: RequestRender;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  setChildren(children: HStackChild[]): void {
    this.children = children;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    if (this.children.length === 0) return [''];

    const widths = this.allocateWidths(width);
    const columns: string[][] = [];
    let maxHeight = 0;

    // Render each child with its allocated width
    for (let i = 0; i < this.children.length; i++) {
      const child = this.children[i];
      const childWidth = widths[i];
      const rows = child.component.render(childWidth);
      columns.push(rows);
      if (rows.length > maxHeight) maxHeight = rows.length;
      child.component.dirty = false;
    }

    // Join columns side-by-side
    const result: string[] = [];
    for (let row = 0; row < maxHeight; row++) {
      let line = '';
      for (let col = 0; col < columns.length; col++) {
        const cellContent = columns[col][row] ?? '';
        const cellWidth = widths[col];
        line += padRight(cellContent, cellWidth);
      }
      result.push(line);
    }

    this.dirty = false;
    return result;
  }

  /** Render with an explicit height constraint (returns first `height` rows). */
  renderWithHeight(width: number, height: number): string[] {
    const rows = this.render(width);
    const result: string[] = [];
    for (let i = 0; i < height; i++) {
      result.push(rows[i] ?? '');
    }
    return result;
  }

  handleInput(key: KeyEvent): boolean {
    for (const child of this.children) {
      if (child.component.handleInput && child.component.handleInput(key)) {
        return true;
      }
    }
    return false;
  }

  private allocateWidths(totalWidth: number): number[] {
    const allocations: number[] = [];
    let fixedUsed = 0;
    let totalFlex = 0;

    for (const child of this.children) {
      if (child.width !== undefined) {
        allocations.push(child.width);
        fixedUsed += child.width;
      } else {
        allocations.push(0);
        totalFlex += child.flexGrow ?? 1;
      }
    }

    const remaining = Math.max(0, totalWidth - fixedUsed);
    if (remaining > 0 && totalFlex > 0) {
      let distributed = 0;
      for (let i = 0; i < this.children.length; i++) {
        if (this.children[i].width === undefined) {
          const flex = this.children[i].flexGrow ?? 1;
          const share = Math.floor((remaining * flex) / totalFlex);
          allocations[i] = share;
          distributed += share;
        }
      }
      // Give leftover to last flex child
      if (distributed < remaining) {
        for (let i = this.children.length - 1; i >= 0; i--) {
          if (this.children[i].width === undefined) {
            allocations[i] += remaining - distributed;
            break;
          }
        }
      }
    }

    return allocations;
  }
}

/** Pad a string with spaces to reach the desired visible width. */
function padRight(text: string, width: number): string {
  const visible = stripAnsi(text).length;
  if (visible >= width) return text;
  return text + ' '.repeat(width - visible);
}
