/**
 * The form widget's pure layer: row order, navigation, editing, toggling, and
 * submit/cancel.
 */
import { describe, expect, it } from 'vitest';

import { applyFormKey, initialState, renderForm, rows } from '../src/repl/form-widget.js';
import type { FormSpec } from '../src/forms/spec.js';

const spec: FormSpec = {
  version: 1,
  title: 'Claim',
  fields: [
    { name: 'vendor', type: 'string', title: 'Vendor', required: true },
    { name: 'amount', type: 'integer', title: 'Amount' },
    { name: 'urgent', type: 'boolean', title: 'Urgent' },
    { name: 'category', type: 'string', title: 'Category', enum: ['travel', 'meals'] },
    { name: 'secret', type: 'string', widget: 'hidden' },
  ],
  actions: [
    { name: 'submit', title: 'Save', kind: 'primary' },
    { name: 'cancel', title: 'Cancel', kind: 'link' },
  ],
};

const KEY = (state: ReturnType<typeof initialState>, key: Parameters<typeof applyFormKey>[1], text = '') =>
  applyFormKey(state, key, text);

describe('form widget', () => {
  it('lists fields in order, skipping hidden ones, then the actions', () => {
    expect(rows(spec).map((row) => (row.kind === 'field' ? row.path : `@${row.action.name}`))).toEqual([
      'vendor',
      'amount',
      'urgent',
      'category',
      '@submit',
      '@cancel',
    ]);
  });

  it('renders the title, values and issues', () => {
    const state = initialState(spec, { vendor: 'acme' }, [{ path: 'amount', level: 'error', message: 'is required' }]);
    const text = renderForm(state).join('\n');
    expect(text).toContain('Claim');
    expect(text).toContain('Vendor *: acme');
    expect(text).toContain('✗ is required');
    expect(renderForm(state).join('\n')).toContain('^s submit');
  });

  it('moves focus with up/down and wraps', () => {
    let state = initialState(spec);
    state = KEY(state, 'down').state;
    expect(state.focus).toBe(1);
    state = KEY(state, 'up').state;
    expect(state.focus).toBe(0);
    state = KEY(state, 'up').state;
    expect(state.focus).toBe(rows(spec).length - 1);
  });

  it('edits a text field', () => {
    let state = initialState(spec);
    state = KEY(state, 'enter').state;          // start editing vendor
    expect(state.editing).toBe(true);
    state = KEY(state, 'text', 'g').state;
    state = KEY(state, 'text', 'l').state;
    state = KEY(state, 'text', 'o').state;
    state = KEY(state, 'text', 'b').state;
    state = KEY(state, 'text', 'e').state;
    state = KEY(state, 'text', 'x').state;
    state = KEY(state, 'text', 'x').state;
    state = KEY(state, 'backspace').state;       // drop the typo
    const result = KEY(state, 'enter');         // commit
    expect(result.state.editing).toBe(false);
    expect(result.state.values).toEqual({ vendor: 'globex' });
  });

  it('coerces a number and toggles a boolean and cycles a select', () => {
    let state = initialState(spec);
    state = { ...state, focus: 1 };
    state = KEY(state, 'enter').state;
    state = KEY(state, 'text', '4').state;
    state = KEY(state, 'text', '2').state;
    state = KEY(state, 'enter').state;
    expect(state.values).toEqual({ amount: 42 });

    state = { ...state, focus: 2 };
    state = KEY(state, 'space').state;
    expect(state.values).toEqual({ amount: 42, urgent: true });

    state = { ...state, focus: 3 };
    state = KEY(state, 'space').state;
    expect(state.values).toEqual({ amount: 42, urgent: true, category: 'travel' });
    state = KEY(state, 'space').state;
    expect(state.values).toEqual({ amount: 42, urgent: true, category: 'meals' });
  });

  it('submits from the action row', () => {
    let state = initialState(spec, { vendor: 'acme' });
    state = { ...state, focus: rows(spec).length - 2 };
    const result = KEY(state, 'enter');
    expect(result.action).toEqual({ type: 'submit', action: 'submit', values: { vendor: 'acme' } });
  });

  it('submits on ctrl-s from anywhere', () => {
    const result = KEY(initialState(spec, { vendor: 'acme' }), 'submit');
    expect(result.action).toEqual({ type: 'submit', action: 'submit', values: { vendor: 'acme' } });
  });

  it('cancels on escape from anywhere', () => {
    expect(KEY(initialState(spec), 'escape').action?.type).toBe('cancel');
  });
});

describe('FormView (TUI panel)', () => {
  it('renders nothing until a state is set, then the form lines', async () => {
    const { FormView } = await import('../src/repl/tui/form-view.js');
    const view = new FormView();
    expect(view.render(80)).toEqual([]);
    view.setState(initialState(spec, { vendor: 'acme' }));
    expect(view.render(80).join('\n')).toContain('Vendor *: acme');
    expect(() => view.invalidate()).not.toThrow();
    view.setState(undefined);
    expect(view.render(80)).toEqual([]);
  });
});
