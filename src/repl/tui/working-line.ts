/**
 * The working indicator, as a dock component.
 *
 * It used to be a `\r`-based line inside the transcript, which meant unrelated
 * output could replace it and leave no sign that anything was happening — most
 * visible with reasoning off and streaming quiet, where the transcript went
 * silent until the first token. As a component in the dock it animates on its own
 * and is only absent when nothing is running.
 */

import { Loader, type Component, type TUI } from '@earendil-works/pi-tui';

import { tuiStyle } from './theme.js';

/** Frames for the indicator (a braille spinner, like the rest of the TUI). */
const FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

export class WorkingLine implements Component {
  private readonly loader: Loader;
  private label: string | null = null;

  constructor(tui: TUI) {
    this.loader = new Loader(tui, tuiStyle.accent, tuiStyle.muted, '', {
      frames: FRAMES,
      intervalMs: 90,
    });
    // `Loader`'s constructor calls `setIndicator()`, which calls `start()`: it
    // begins animating (and holding a timer) the moment it is constructed. Left
    // alone that keeps the process alive after the TUI stops and re-renders while
    // nothing is running, so park it until there is work to show.
    this.loader.stop();
  }

  /** Stop the animation (called on exit, so no timer outlives the UI). */
  stop(): void {
    this.label = null;
    this.loader.stop();
  }

  /** Show the indicator with `label`, or hide it when `label` is null. */
  set(label: string | null): void {
    if (label === this.label) return;
    this.label = label;
    if (label === null) {
      this.loader.stop();
      return;
    }
    this.loader.setMessage(label);
    this.loader.start();
  }

  get isWorking(): boolean {
    return this.label !== null;
  }

  render(width: number): string[] {
    // Nothing to show: no row at all, so the dock does not reserve a blank line.
    if (this.label === null) return [];
    return this.loader.render(width);
  }

  invalidate(): void {
    this.loader.invalidate();
  }
}
