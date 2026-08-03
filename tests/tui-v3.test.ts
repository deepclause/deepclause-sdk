import { describe, expect, it, vi } from 'vitest';
import { Input } from '../src/cli/tui-v3/components/input.js';
import { Sessions } from '../src/cli/tui-v3/components/sessions.js';
import { normalizeKeyEvent } from '../src/cli/tui-v3/event-loop.js';
import { composeOverlay } from '../src/cli/tui-v3/layout/overlay.js';
import { clipAnsi, stripAnsi, style, ANSI, visibleLength } from '../src/cli/tui-v3/util/ansi.js';

describe('TUI v3', () => {
  it('clips styled rows by visible width without breaking ANSI sequences', () => {
    const clipped = clipAnsi(style('F1Help F2Sessions', ANSI.bgBlue), 8);

    expect(stripAnsi(clipped)).toBe('F1Help F');
    expect(visibleLength(clipped)).toBe(8);
    expect(clipped.endsWith(ANSI.reset)).toBe(true);
  });

  it('keeps the input compact and expands it for Shift+Enter', () => {
    const input = new Input(vi.fn());
    input.setValue('first line');

    expect(input.height).toBe(3);
    expect(input.handleInput({
      name: 'return',
      sequence: '\x1b[13;2u',
      ctrl: false,
      meta: false,
      shift: true,
    })).toBe(true);
    expect(input.getValue()).toBe('first line\n');
    expect(input.height).toBe(4);
    const rows = input.render(40);
    expect(rows).toHaveLength(4);
    expect(rows.map(visibleLength)).toEqual([40, 40, 40, 40]);
  });

  it('normalizes function keys and enhanced modified Enter sequences', () => {
    expect(normalizeKeyEvent(undefined, { sequence: '\x1bOP' })?.name).toBe('f1');
    expect(normalizeKeyEvent(undefined, { sequence: '\x1b[17~' })?.name).toBe('f6');
    expect(normalizeKeyEvent(undefined, { sequence: '\x1b[13;2u' })).toMatchObject({
      name: 'return',
      shift: true,
      ctrl: false,
      meta: false,
    });
    expect(normalizeKeyEvent(undefined, { sequence: '\x1b[27;5;13~' })).toMatchObject({
      name: 'return',
      ctrl: true,
    });
  });

  it('renders session titles and dates and selects them with the keyboard', () => {
    const onSelect = vi.fn();
    const sessions = new Sessions(vi.fn());
    sessions.setSessions([
      { id: 'one', title: 'First session', updatedAt: '2026-08-03T00:45:00.000Z' },
      { id: 'two', title: 'Second session', updatedAt: '2026-08-03T01:45:00.000Z' },
    ]);
    sessions.setActiveSession('one');
    sessions.setOnSelect(onSelect);

    const rendered = sessions.render(30).map(stripAnsi).join('\n');
    expect(sessions.isCollapsed).toBe(false);
    expect(rendered).toContain('First session');
    expect(rendered).toContain('Second session');
    expect(rendered).toMatch(/Aug 3/);

    sessions.handleInput({ name: 'down', sequence: '', ctrl: false, meta: false, shift: false });
    sessions.handleInput({ name: 'return', sequence: '\r', ctrl: false, meta: false, shift: false });
    expect(onSelect).toHaveBeenCalledWith('two');
  });

  it('keeps the selected session visible in a short pane', () => {
    const sessions = new Sessions(vi.fn());
    sessions.setSessions([
      { id: 'one', title: 'First', updatedAt: '2026-08-01T00:00:00.000Z' },
      { id: 'two', title: 'Second', updatedAt: '2026-08-02T00:00:00.000Z' },
      { id: 'three', title: 'Third', updatedAt: '2026-08-03T00:00:00.000Z' },
    ]);
    sessions.handleInput({ name: 'end', sequence: '', ctrl: false, meta: false, shift: false });
    sessions.setSessions([
      { id: 'one', title: 'First', updatedAt: '2026-08-01T00:00:00.000Z' },
      { id: 'two', title: 'Second', updatedAt: '2026-08-02T00:00:00.000Z' },
      { id: 'three', title: 'Third', updatedAt: '2026-08-03T00:01:00.000Z' },
    ]);

    const rendered = sessions.renderWithHeight(24, 6).map(stripAnsi).join('\n');
    expect(rendered).toContain('Third');
    expect(rendered).not.toContain('First');
  });

  it('composes dialogs over ANSI-styled rows without corrupting their width', () => {
    const background = [style('abcdefghij', ANSI.bgBlue)];
    const composed = composeOverlay(background, ['[ok]'], 4, 10, 1, { x: 3, y: 0 });

    expect(stripAnsi(composed[0])).toBe('abc[ok]hij');
    expect(visibleLength(composed[0])).toBe(10);
  });
});
