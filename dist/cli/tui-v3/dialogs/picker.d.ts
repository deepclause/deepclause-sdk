/**
 * Fuzzy picker dialog — item selection overlay.
 */
import type { Component, KeyEvent, RequestRender } from '../types.js';
export interface PickerItem {
    id: string;
    label: string;
    description?: string;
}
export declare class PickerDialog implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private visible;
    private items;
    private filteredItems;
    private query;
    private selectedIndex;
    private title;
    private onSelect;
    private onCancel;
    constructor(requestRender: RequestRender);
    show(items: PickerItem[], title?: string): void;
    hide(): void;
    setOnSelect(fn: (item: PickerItem) => void): void;
    setOnCancel(fn: () => void): void;
    get isVisible(): boolean;
    invalidate(): void;
    render(width: number): string[];
    handleInput(key: KeyEvent): boolean;
    private filterItems;
}
//# sourceMappingURL=picker.d.ts.map