/**
 * Fuzzy picker dialog — item selection overlay.
 */

import type { Component, KeyEvent, RequestRender } from '../types.js';
import { style, ANSI, truncate, center } from '../util/ansi.js';

export interface PickerItem {
  id: string;
  label: string;
  description?: string;
}

export class PickerDialog implements Component {
  dirty = true;
  minHeight = 0;
  flexGrow = 0;

  private requestRenderFn: RequestRender;
  private visible = false;
  private items: PickerItem[] = [];
  private filteredItems: PickerItem[] = [];
  private query = '';
  private selectedIndex = 0;
  private title = 'Select';
  private onSelect: ((item: PickerItem) => void) | null = null;
  private onCancel: (() => void) | null = null;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  show(items: PickerItem[], title = 'Select'): void {
    this.items = items;
    this.filteredItems = items;
    this.title = title;
    this.query = '';
    this.selectedIndex = 0;
    this.visible = true;
    this.invalidate();
  }

  hide(): void {
    this.visible = false;
    this.invalidate();
  }

  setOnSelect(fn: (item: PickerItem) => void): void {
    this.onSelect = fn;
  }

  setOnCancel(fn: () => void): void {
    this.onCancel = fn;
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

    const maxWidth = Math.min(60, width - 4);
    const maxItems = 10;
    const rows: string[] = [];
    const border = '─'.repeat(maxWidth - 2);

    rows.push(`┌${border}┐`);
    rows.push(`│${center(style(this.title, ANSI.bold), maxWidth - 2)}│`);
    rows.push(`├${border}┤`);

    // Search input
    const queryLine = `  > ${this.query}█`;
    rows.push(`│${padLine(queryLine, maxWidth - 2)}│`);
    rows.push(`├${border}┤`);

    // Items
    const visibleItems = this.filteredItems.slice(0, maxItems);
    if (visibleItems.length === 0) {
      rows.push(`│${padLine(style('  No matches', ANSI.dim), maxWidth - 2)}│`);
    } else {
      for (let i = 0; i < visibleItems.length; i++) {
        const item = visibleItems[i];
        const isSelected = i === this.selectedIndex;
        const prefix = isSelected ? '▸ ' : '  ';
        const label = truncate(item.label, maxWidth - 6);
        const line = isSelected
          ? style(`${prefix}${label}`, ANSI.bold, ANSI.cyan)
          : `${prefix}${label}`;
        rows.push(`│${padLine(line, maxWidth - 2)}│`);
      }
    }

    rows.push(`└${border}┘`);

    this.dirty = false;
    return rows;
  }

  handleInput(key: KeyEvent): boolean {
    if (!this.visible) return false;

    // Navigation
    if (key.name === 'up') {
      this.selectedIndex = Math.max(0, this.selectedIndex - 1);
      this.invalidate();
      return true;
    }
    if (key.name === 'down') {
      this.selectedIndex = Math.min(this.filteredItems.length - 1, this.selectedIndex + 1);
      this.invalidate();
      return true;
    }

    // Select
    if (key.name === 'return') {
      const item = this.filteredItems[this.selectedIndex];
      if (item && this.onSelect) {
        this.onSelect(item);
      }
      this.hide();
      return true;
    }

    // Cancel
    if (key.name === 'escape') {
      this.hide();
      if (this.onCancel) this.onCancel();
      return true;
    }

    // Backspace
    if (key.name === 'backspace') {
      if (this.query.length > 0) {
        this.query = this.query.slice(0, -1);
        this.filterItems();
        this.invalidate();
      }
      return true;
    }

    // Character input
    if (key.sequence && key.sequence.length === 1 && !key.ctrl && !key.meta) {
      if (key.sequence.charCodeAt(0) >= 32) {
        this.query += key.sequence;
        this.filterItems();
        this.invalidate();
        return true;
      }
    }

    return true; // Consume all input when picker is active
  }

  private filterItems(): void {
    const q = this.query.toLowerCase();
    if (!q) {
      this.filteredItems = this.items;
    } else {
      this.filteredItems = this.items.filter(
        (item) => item.label.toLowerCase().includes(q) ||
          (item.description && item.description.toLowerCase().includes(q)),
      );
    }
    this.selectedIndex = Math.min(this.selectedIndex, Math.max(0, this.filteredItems.length - 1));
  }
}

function padLine(text: string, width: number): string {
  const stripped = text.replace(/\u001b\[[0-9;]*[A-Za-z]/g, '');
  const padding = Math.max(0, width - stripped.length);
  return text + ' '.repeat(padding);
}
