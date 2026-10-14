/**
 * The harness side view (experiment): watching the module change.
 *
 * It reads the session module, so it shows uncommitted edits too, and it diffs each
 * refresh against the previous one so an edit is marked as it happens. The visual
 * claims worth testing are those two: that a change is marked, and that the marker
 * fades rather than sticking forever.
 */
import { describe, expect, it } from 'vitest';
import { visibleWidth } from '@earendil-works/pi-tui';

import { HarnessView, type HarnessSnapshot } from '../src/repl/tui/harness-view.js';

const plain = (line: string): string => line.replace(/\u001b\[[0-9;]*[a-zA-Z]/g, '');

function snapshot(predicates: HarnessSnapshot['predicates'], extra: Partial<HarnessSnapshot> = {}): HarnessSnapshot {
  return { predicates, transactionOpen: false, touched: [], ...extra };
}

const alpha = { indicator: 'alpha/1', name: 'alpha', arity: 1, clauses: 2, flags: [] as string[] };
const entry = { indicator: 'agent_main/1', name: 'agent_main', arity: 1, clauses: 3, flags: ['entry'] };
const tool = { indicator: 'count/2', name: 'count', arity: 2, clauses: 1, flags: ['tool', 'doc'] };

describe('HarnessView', () => {
  it('shows the harness, its size and each predicate', () => {
    const view = new HarnessView(() => snapshot([entry, tool], { version: 3 }));
    view.refresh();
    const text = view.render(40).map(plain).join('\n');
    expect(text).toContain('harness v3');
    expect(text).toContain('2 predicates · 4 clauses');
    expect(text).toContain('agent_main/1');
    expect(text).toContain('entry');
    expect(text).toContain('tool');
  });

  it('says when it is empty, rather than showing nothing', () => {
    const view = new HarnessView(() => snapshot([]));
    view.refresh();
    expect(view.render(40).map(plain).join('\n')).toContain('(empty');
  });

  it('marks what changed since the previous refresh', () => {
    let current = snapshot([alpha]);
    const view = new HarnessView(() => current);
    view.refresh();

    current = snapshot([{ ...alpha, clauses: 5 }]);
    view.refresh();
    expect(view.render(40).map(plain).join('\n')).toContain('~'); // updated

    current = snapshot([{ ...alpha, clauses: 5 }, tool]);
    view.refresh();
    const text = view.render(40).map(plain).join('\n');
    expect(text).toContain('+'); // added
    expect(text).toContain('count/2');
  });

  it('marks a removal and lets the marker fade', () => {
    // An injected clock rather than fake timers: the flash window is the component's
    // own concern, so it should not depend on the test runner faking `Date`.
    let now = 1_000;
    let current = snapshot([alpha, tool]);
    const view = new HarnessView(() => current, { now: () => now });
    view.refresh();
    current = snapshot([alpha]);
    view.refresh();
    expect(view.render(40).map(plain).join('\n')).toContain('−');

    // The marker is temporary: it exists to draw the eye, not to annotate history.
    now += 5_000;
    expect(view.render(40).map(plain).join('\n')).not.toContain('−');
  });

  it('says when an edit transaction is open', () => {
    const view = new HarnessView(() => snapshot([alpha], { transactionOpen: true, touched: ['alpha/1'] }));
    view.refresh();
    expect(view.render(40).map(plain).join('\n')).toContain('editing');
  });

  it('keeps every line within its column', () => {
    const view = new HarnessView(() => snapshot(
      [{ indicator: 'a_very_long_predicate_name_indeed/12', name: 'x', arity: 12, clauses: 9, flags: ['tool', 'doc', 'entry'] }],
    ));
    view.refresh();
    for (const width of [24, 32, 40]) {
      for (const line of view.render(width)) {
        expect(visibleWidth(line)).toBeLessThanOrEqual(width);
      }
    }
  });
});

describe('drill-down', () => {
  const predicates = [entry, alpha, tool];
  function viewWithSource() {
    const requested: string[] = [];
    const view = new HarnessView(() => snapshot(predicates), {
      requestSource: (indicator) => requested.push(indicator),
    });
    view.refresh();
    return { view, requested };
  }

  it('opens the predicate under the cursor and shows its clauses', () => {
    const { view, requested } = viewWithSource();
    view.handleInput('\r');
    const indicator = view.selectedIndicator!;
    expect(view.open).toBe(indicator);
    expect(requested).toEqual([indicator]);

    view.setSource(indicator, [`${indicator} :- true.`, '']);
    const text = view.render(40).map(plain).join('\n');
    expect(text).toContain('esc to go back');
    expect(text).toContain(`${indicator} :- true.`);
  });

  it('moves the cursor with the keyboard and follows it while open', () => {
    const { view, requested } = viewWithSource();
    const first = view.selectedIndicator;
    view.handleInput('\u001b[B'); // down
    expect(view.selectedIndicator).not.toBe(first);

    view.handleInput('\r');
    const second = view.selectedIndicator;
    view.handleInput('\u001b[B'); // down again, still drilled in
    expect(requested).toContain(second);
    expect(view.open).toBe(view.selectedIndicator);
  });

  it('goes back with escape, and reports leaving when nothing is open', () => {
    const exits: number[] = [];
    const view = new HarnessView(() => snapshot(predicates), { onExit: () => exits.push(1) });
    view.refresh();

    view.handleInput('\r');
    expect(view.open).toBeDefined();
    view.handleInput('\u001b');
    expect(view.open).toBeUndefined();
    expect(exits).toHaveLength(0);

    view.handleInput('\u001b');
    expect(exits).toHaveLength(1);
  });

  it('opens the row that was clicked', () => {
    const { view } = viewWithSource();
    // The mouse y is relative to this component, so the rendered line index is the
    // hit test — find the row for a known predicate and click it.
    const lines = view.render(40).map(plain);
    const rowIndex = lines.findIndex((line) => line.includes('alpha/1'));
    expect(rowIndex).toBeGreaterThan(0);
    view.handleMouse({ type: 'press', y: rowIndex, x: 3, button: 'left' } as never);
    expect(view.open).toBe('alpha/1');
  });

  it('hides nothing when a click lands on the sparkline or header', () => {
    const { view } = viewWithSource();
    view.handleInput('\r');
    expect(view.open).toBeDefined();
    view.render(40); // draw it, so the hit map matches what is on screen
    view.handleMouse({ type: 'press', y: 1, x: 3, button: 'left' } as never);
    expect(view.open).toBeUndefined();
  });
});
