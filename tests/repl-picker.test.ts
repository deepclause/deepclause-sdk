/**
 * The readline picker: bounded pages + filter, so long model catalogs do not
 * render as endless lists.
 */
import { describe, expect, it } from 'vitest';

import { classifyKey, filterItems, renderPick, type PickItem } from '../src/repl/picker.js';

const ITEMS: PickItem[] = [
  { id: 'anthropic/claude', label: 'anthropic/claude', hint: '[reasoning]' },
  { id: 'openai/gpt', label: 'openai/gpt' },
  { id: 'openai/o3', label: 'openai/o3' },
];

describe('classifyKey', () => {
  it('recognizes navigation, select and cancel keys', () => {
    expect(classifyKey('\x1b[A')).toBe('up');
    expect(classifyKey('\x1b[B')).toBe('down');
    expect(classifyKey('\x1b[5~')).toBe('pageup');
    expect(classifyKey('\x1b[6~')).toBe('pagedown');
    expect(classifyKey('\r')).toBe('enter');
    expect(classifyKey('\n')).toBe('enter');
    expect(classifyKey('\x1b')).toBe('cancel');
    expect(classifyKey('\x03')).toBe('cancel');
    expect(classifyKey('\x7f')).toBe('backspace');
  });

  it('treats other printables as filter text', () => {
    expect(classifyKey('g')).toBe('text');
    expect(classifyKey('4')).toBe('text');
    expect(classifyKey('\x1b[Z')).toBe('ignore');
  });
});

describe('filterItems', () => {
  it('returns everything for an empty query', () => {
    expect(filterItems(ITEMS, '')).toHaveLength(3);
  });

  it('matches label/id substrings, case-insensitively', () => {
    expect(filterItems(ITEMS, 'OPENAI').map((item) => item.id)).toEqual(['openai/gpt', 'openai/o3']);
    expect(filterItems(ITEMS, 'claude').map((item) => item.id)).toEqual(['anthropic/claude']);
  });

  it('accepts a custom matcher', () => {
    expect(filterItems(ITEMS, 'gpt', (item, query) => item.id.endsWith(query))).toHaveLength(1);
  });
});

describe('renderPick', () => {
  it('pages long lists and reports the total', () => {
    const many = Array.from({ length: 30 }, (_, index) => ({ id: `m${index}`, label: `model-${index}` }));
    const { lines, page } = renderPick({ message: 'pick', items: many, pageSize: 5 }, '');
    expect(page).toHaveLength(5);
    expect(lines.join('\n')).toContain('… 25 more');
    expect(lines.join('\n')).toContain('5 of 30');
  });

  it('filters and reports no matches', () => {
    const { lines, page } = renderPick({ message: 'pick', items: ITEMS, pageSize: 5 }, 'zzz');
    expect(page).toHaveLength(0);
    expect(lines.join('\n')).toContain('(no matches)');
  });

  it('marks the current selection', () => {
    const { lines } = renderPick(
      { message: 'pick', items: ITEMS, pageSize: 5, currentId: 'openai/gpt' },
      '',
    );
    expect(lines.join('\n')).toContain('current: openai/gpt');
  });
});
