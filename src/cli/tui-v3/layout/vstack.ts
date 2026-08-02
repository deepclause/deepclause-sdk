/**
 * VStack layout — arranges child components vertically.
 *
 * Allocates space based on minHeight and flexGrow:
 * 1. Each child gets its minHeight first
 * 2. Remaining space is distributed proportionally by flexGrow
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';

export interface VStackChild {
  component: Component;
  /** Minimum lines this child needs (default: component.minHeight ?? 1) */
  minHeight?: number;
  /** Flex grow factor (default: component.flexGrow ?? 0) */
  flexGrow?: number;
}

export class VStack implements Component {
  dirty = true;
  minHeight = 0;
  flexGrow = 1;

  private children: VStackChild[] = [];
  private requestRenderFn: RequestRender;
  private cachedRows: string[] | null = null;
  private cachedWidth = 0;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set the children of this stack. */
  setChildren(children: VStackChild[]): void {
    this.children = children;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.cachedRows = null;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    if (this.cachedRows && this.cachedWidth === width && !this.dirty && !this.hasAnyDirtyChild()) {
      return this.cachedRows;
    }

    this.cachedWidth = width;
    const rows: string[] = [];

    // This is called from the parent with the total available height implicit in the render call
    // We compute allocation here based on all children
    const allocations = this.allocateHeights(this.getTotalHeight());

    for (let i = 0; i < this.children.length; i++) {
      const child = this.children[i];
      const height = allocations[i];
      const childRows = child.component.render(width);

      // Take up to `height` rows from child, pad if needed
      for (let r = 0; r < height; r++) {
        rows.push(childRows[r] ?? '');
      }
      child.component.dirty = false;
    }

    this.cachedRows = rows;
    this.dirty = false;
    return rows;
  }

  /** Render with an explicit height constraint. */
  renderWithHeight(width: number, height: number): string[] {
    const allocations = this.allocateHeights(height);
    const rows: string[] = [];

    for (let i = 0; i < this.children.length; i++) {
      const child = this.children[i];
      const h = allocations[i];
      const childRows = child.component.render(width);
      for (let r = 0; r < h; r++) {
        rows.push(childRows[r] ?? '');
      }
      child.component.dirty = false;
    }

    this.cachedRows = rows;
    this.cachedWidth = width;
    this.dirty = false;
    return rows;
  }

  handleInput(key: KeyEvent): boolean {
    // Dispatch to children (focused child first if applicable)
    for (const child of this.children) {
      if (child.component.handleInput && child.component.handleInput(key)) {
        return true;
      }
    }
    return false;
  }

  private getTotalHeight(): number {
    let total = 0;
    for (const child of this.children) {
      total += child.minHeight ?? child.component.minHeight ?? 1;
    }
    return total;
  }

  private allocateHeights(totalHeight: number): number[] {
    const allocations: number[] = [];
    let usedHeight = 0;
    let totalFlex = 0;

    // First pass: allocate minimum heights
    for (const child of this.children) {
      const min = child.minHeight ?? child.component.minHeight ?? 1;
      allocations.push(min);
      usedHeight += min;
      totalFlex += child.flexGrow ?? child.component.flexGrow ?? 0;
    }

    // Second pass: distribute remaining space by flex
    const remaining = Math.max(0, totalHeight - usedHeight);
    if (remaining > 0 && totalFlex > 0) {
      let distributed = 0;
      for (let i = 0; i < this.children.length; i++) {
        const flex = this.children[i].flexGrow ?? this.children[i].component.flexGrow ?? 0;
        if (flex > 0) {
          const share = Math.floor((remaining * flex) / totalFlex);
          allocations[i] += share;
          distributed += share;
        }
      }
      // Give any leftover pixel to the last flex child
      if (distributed < remaining) {
        for (let i = this.children.length - 1; i >= 0; i--) {
          const flex = this.children[i].flexGrow ?? this.children[i].component.flexGrow ?? 0;
          if (flex > 0) {
            allocations[i] += remaining - distributed;
            break;
          }
        }
      }
    }

    return allocations;
  }

  private hasAnyDirtyChild(): boolean {
    for (const child of this.children) {
      if (child.component.dirty) return true;
    }
    return false;
  }
}
