/**
 * A chooser overlay: a titled list the user picks from.
 *
 * Used for the model and the reasoning-display setting. It is a plain component so
 * the host can place it with `tui.showOverlay`; `onSelect`/`onCancel` tell the host
 * when to take it down. Keys are the list's own (up/down, enter, escape), which is
 * what pi's pickers use, so nothing has to be re-taught.
 */

import { SelectList, type Component, type SelectItem } from '@earendil-works/pi-tui';

import { selectListTheme, tuiStyle } from './theme.js';

export interface ChoicePickerOptions {
  /** Heading, e.g. "Model" or "Reasoning". */
  title: string;
  items: SelectItem[];
  /** `value` of the item to start on. */
  current?: string;
  /** Shown under the title, e.g. what the choice affects. */
  hint?: string;
  onSelect: (item: SelectItem) => void;
  onCancel: () => void;
}

export class ChoicePicker implements Component {
  private readonly options: ChoicePickerOptions;
  private readonly list: SelectList;

  constructor(options: ChoicePickerOptions) {
    this.options = options;
    this.list = new SelectList(options.items, 12, selectListTheme());
    if (options.current) {
      const index = options.items.findIndex((item) => item.value === options.current);
      if (index >= 0) this.list.setSelectedIndex(index);
    }
    this.list.onSelect = (item) => options.onSelect(item);
    this.list.onCancel = () => options.onCancel();
  }

  /** The visible lines, so a host can size the overlay to its content. */
  get contentHeight(): number {
    return 2 + (this.options.hint ? 1 : 0) + Math.min(this.options.items.length, 12);
  }

  render(width: number): string[] {
    const out = [tuiStyle.title(this.options.title)];
    if (this.options.hint) out.push(tuiStyle.dim(this.options.hint));
    out.push(...this.list.render(width));
    return out;
  }

  handleInput(data: string): void {
    this.list.handleInput(data);
  }

  invalidate(): void {
    this.list.invalidate();
  }
}
