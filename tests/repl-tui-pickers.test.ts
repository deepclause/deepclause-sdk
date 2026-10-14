/**
 * The chooser overlay (model, reasoning) and the live reasoning switch.
 *
 * The picker is a component the host places with `tui.showOverlay`; what matters
 * here is that it renders its items, marks the current one, and reports a selection
 * or a cancel. The host's overlay wiring is exercised by hand in a pty, since arrow
 * keys through a scripted pty depend on escape-sequence timing.
 */
import { describe, expect, it, vi } from 'vitest';
import { visibleWidth } from '@earendil-works/pi-tui';

import { ChoicePicker } from '../src/repl/tui/picker.js';
import { ReplRenderer } from '../src/repl/render.js';

const plain = (line: string): string => line.replace(/\u001b\[[0-9;]*[a-zA-Z]/g, '');

const ITEMS = [
  { value: 'off', label: 'off', description: 'hide reasoning entirely' },
  { value: 'summary', label: 'summary', description: 'a bounded preview' },
  { value: 'full', label: 'full', description: 'everything the model emits' },
];

describe('ChoicePicker', () => {
  it('renders the title, hint and every item within the width', () => {
    const picker = new ChoicePicker({
      title: 'Reasoning',
      hint: 'How much of the model thinking to show',
      items: ITEMS,
      current: 'summary',
      onSelect: () => {},
      onCancel: () => {},
    });
    const lines = picker.render(50).map(plain);
    expect(lines[0]).toContain('Reasoning');
    expect(lines[1]).toContain('How much of the model');
    const body = lines.slice(2).join('\n');
    for (const item of ITEMS) expect(body).toContain(item.label);
    for (const line of picker.render(50)) {
      expect(visibleWidth(line)).toBeLessThanOrEqual(50);
    }
  });

  it('reports the selection', () => {
    const onSelect = vi.fn();
    const onCancel = vi.fn();
    const picker = new ChoicePicker({
      title: 'Reasoning', items: ITEMS, current: 'summary', onSelect, onCancel,
    });
    picker.handleInput('\r');
    expect(onCancel).not.toHaveBeenCalled();
    expect(onSelect).toHaveBeenCalledTimes(1);
    expect(onSelect.mock.calls[0]?.[0]).toMatchObject({ value: 'summary' });
  });

  it('reports a cancel', () => {
    const onSelect = vi.fn();
    const onCancel = vi.fn();
    const picker = new ChoicePicker({ title: 'Reasoning', items: ITEMS, onSelect, onCancel });
    picker.handleInput('\u001b');
    expect(onSelect).not.toHaveBeenCalled();
    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  it('sizes itself to its content, so the host can bound the overlay', () => {
    const picker = new ChoicePicker({
      title: 'Model', hint: 'hint', items: ITEMS, onSelect: () => {}, onCancel: () => {},
    });
    expect(picker.contentHeight).toBe(2 + 1 + ITEMS.length);
  });
});

describe('the reasoning display mode can change while running', () => {
  it('shows reasoning after switching from off to full', () => {
    const written: string[] = [];
    const renderer = new ReplRenderer({ mode: 'rich', reasoning: 'off' });
    renderer.setSink((text) => written.push(text));

    const reasoning = { type: 'reasoning' as const, content: 'weighing the options', frameKind: 'run' as const, depth: 0 };
    renderer.render(reasoning);
    expect(written.join('')).not.toContain('weighing the options');

    renderer.setReasoningMode('full');
    expect(renderer.getReasoningMode()).toBe('full');
    renderer.render(reasoning);
    expect(written.join('')).toContain('weighing the options');
  });
});
