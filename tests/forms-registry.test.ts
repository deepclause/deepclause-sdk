/**
 * The form registry: revisions, waiters, and the rule that a *UI action* wakes a
 * waiter while an *agent edit* does not.
 */
import { describe, expect, it } from 'vitest';

import { FormRegistry, diffPaths, mergePatch } from '../src/forms/registry.js';
import type { FormSpec } from '../src/forms/spec.js';

const spec: FormSpec = {
  version: 1,
  title: 'Claim',
  fields: [
    { name: 'vendor', type: 'string', required: true },
    { name: 'amount', type: 'number', default: 0 },
  ],
  actions: [{ name: 'submit', title: 'Submit', kind: 'primary' }, { name: 'cancel', title: 'Cancel' }],
};

describe('FormRegistry', () => {
  it('opens a form with defaults and reports its revision', () => {
    const registry = new FormRegistry();
    const { formId, revision, values } = registry.open(spec);
    expect(formId).toBeTruthy();
    expect(revision).toBe(1);
    expect(values).toEqual({ amount: 0 });
    expect(registry.get(formId)?.state).toBe('open');
  });

  it('emits an event on open, update and close', () => {
    const events: string[] = [];
    const registry = new FormRegistry({ onEvent: (event) => events.push(`${event.op}:${event.revision}`) });
    const { formId } = registry.open(spec);
    registry.update(formId, { amount: 5 });
    registry.setIssues(formId, [{ path: 'vendor', level: 'error', message: 'is required' }]);
    registry.close(formId, 'submitted');
    expect(events).toEqual(['open:1', 'update:2', 'update:3', 'close:4']);
  });

  it('delivers a submit to a waiting caller', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    const pending = registry.wait(formId, 0);
    registry.submit(formId, 'submit', { vendor: 'acme', amount: 5 });
    const change = await pending;
    expect(change).toMatchObject({ formId, action: 'submit', values: { vendor: 'acme', amount: 5 } });
    expect(change.changed).toEqual(['amount', 'vendor']);
  });

  it('does not lose a submit that lands before the wait starts', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    registry.submit(formId, 'submit', { vendor: 'acme', amount: 1 });
    const change = await registry.wait(formId, 0);
    expect(change.action).toBe('submit');
    expect(change.revision).toBe(2);
  });

  it('does not re-deliver a change the caller has already seen', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    const first = registry.submit(formId, 'submit', { vendor: 'acme' });
    expect(first).toBeDefined();
    // Waiting "after" the change it already handled must block, not return it.
    const race = await Promise.race([
      registry.wait(formId, first!.revision).then(() => 'returned'),
      new Promise((resolve) => setTimeout(() => resolve('blocked'), 20)),
    ]);
    expect(race).toBe('blocked');
  });

  it('does NOT wake a waiter on an agent edit', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    let resolved = false;
    const pending = registry.wait(formId, 1).then((change) => {
      resolved = true;
      return change;
    });
    registry.update(formId, { amount: 9 });          // agent edit
    registry.setIssues(formId, [{ path: 'vendor', level: 'error', message: 'is required' }]);
    await new Promise((resolve) => setTimeout(resolve, 10));
    expect(resolved).toBe(false);

    registry.submit(formId, 'submit', { vendor: 'acme' });   // UI action
    expect((await pending).action).toBe('submit');
  });

  it('resolves `timeout` when the UI never acts', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    const change = await registry.wait(formId, 0, { timeoutMs: 15 });
    expect(change.action).toBe('timeout');
  });

  it('resolves `cancelled` when the turn aborts', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    const controller = new AbortController();
    const pending = registry.wait(formId, 0, { signal: controller.signal });
    controller.abort();
    expect((await pending).action).toBe('cancelled');
  });

  it('cancelPending resolves every waiter', async () => {
    const registry = new FormRegistry();
    const a = registry.open(spec).formId;
    const b = registry.open(spec, { formId: 'other' }).formId;
    const pending = Promise.all([registry.wait(a, 0), registry.wait(b, 0)]);
    registry.cancelPending();
    const changes = await pending;
    expect(changes.map((change) => change.action)).toEqual(['cancelled', 'cancelled']);
  });

  it('reports `unavailable` for an unknown form', async () => {
    const registry = new FormRegistry();
    expect((await registry.wait('missing', 0)).action).toBe('unavailable');
  });

  it('closes pending waiters with `closed`', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    const pending = registry.wait(formId, 0);
    registry.close(formId, 'cancelled');
    expect((await pending).action).toBe('closed');
    expect(registry.get(formId)?.state).toBe('cancelled');
  });
});

describe('awaitingForms', () => {
  // A conversational turn must not end while a form it opened still owes the
  // person an answer — or while it has asked them for one and nobody is waiting.
  it('reports a freshly rendered form as awaiting', () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    expect(registry.awaitingForms().map((record) => record.formId)).toEqual([formId]);
  });

  it('stops reporting it once a change has been delivered', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    const wait = registry.wait(formId, -1, {});
    registry.submit(formId, 'submit', { vendor: 'acme', amount: 5 });
    await wait;
    expect(registry.awaitingForms()).toEqual([]);
  });

  it('reports it again when the agent changes the form after a delivery', () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    registry.submit(formId, 'submit', { vendor: '', amount: 0 });
    registry.setIssues(formId, [{ path: 'vendor', level: 'error', message: 'is required' }]);
    // The person is being asked to fix something nobody is waiting for.
    expect(registry.awaitingForms().map((record) => record.formId)).toEqual([formId]);
  });

  it('does not report a closed form', async () => {
    const registry = new FormRegistry();
    const { formId } = registry.open(spec);
    registry.close(formId, 'cancelled');
    expect(registry.awaitingForms()).toEqual([]);
    expect(registry.get(formId)?.state).toBe('cancelled');
  });
});

describe('merge patch', () => {
  it('merges nested objects and deletes on null', () => {
    expect(mergePatch({ a: { b: 1, c: 2 } }, { a: { c: null, d: 3 } })).toEqual({ a: { b: 1, d: 3 } });
  });

  it('replaces arrays wholesale', () => {
    expect(mergePatch({ items: [1, 2, 3] }, { items: [9] })).toEqual({ items: [9] });
  });

  it('diffs changed paths, recursing into groups', () => {
    expect(diffPaths({ a: 1, g: { x: 1, y: 2 } }, { a: 1, g: { x: 1, y: 5 } })).toEqual(['g.y']);
  });
});
