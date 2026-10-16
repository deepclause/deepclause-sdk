/**
 * A docked panel showing the active form: title, fields, values and issues.
 *
 * In the TUI the fields are edited through the editor (one prompt per field);
 * this panel is the live read-out, rendered from the same widget state the
 * plain-TTY widget uses.
 */
import { truncateToWidth, type Component } from '@earendil-works/pi-tui';

import { renderForm, type FormWidgetState } from '../form-widget.js';

export class FormView implements Component {
  private state?: FormWidgetState;

  /** Set the form to show, or `undefined` to hide the panel. */
  setState(state?: FormWidgetState): void {
    this.state = state;
  }

  render(width: number): string[] {
    if (!this.state) return [];
    return renderForm(this.state).map((line) => truncateToWidth(line, width, '…'));
  }

  invalidate(): void {
    // Stateless: renders from the current state each time.
  }
}
