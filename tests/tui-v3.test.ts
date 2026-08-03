import { describe, expect, it, vi } from 'vitest';
import { Input } from '../src/cli/tui-v3/components/input.js';
import { normalizeKeyEvent } from '../src/cli/tui-v3/event-loop.js';
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
    expect(input.render(40)).toHaveLength(4);
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
});
