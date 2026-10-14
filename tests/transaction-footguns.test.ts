/**
 * Edit-transaction footguns.
 *
 * Every case here was reproduced against the running runtime before it was
 * fixed, and each one is a way an agent could end up with "the old predicate
 * clashing with the uncommitted one" — or with no predicate at all.
 *
 * The theme is that a failure used to be able to *damage* the harness, and that
 * silent successes left both versions of a predicate live.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { plainValue } from '../src/repl/frames.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-txn-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
  repl.session.takePrologMessages();
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

/** Read one binding from a host query. */
function val(goal: string, name: string): unknown {
  const query = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  return plainValue(query.call(repl.session, goal)[name]);
}

/** Install a predicate with `define` and commit it, returning the commit result. */
async function install(head: string, clauses: string) {
  await repl.session.beginTurn();
  const edit = await repl.session.define(head, clauses);
  const commit = await repl.session.endTurn();
  return { edit, commit };
}

const clausesOf = (name: string): unknown => val(`findall(X, ${name}, L)`, 'L');

/** Does `goal` hold? */
function holds(goal: string): boolean {
  const query = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  return query.call(repl.session, goal).success === true;
}

describe('a failed edit does not damage the harness', () => {
  it('keeps the predicate when the block is refused part-way', async () => {
    await install('alpha/1', 'alpha(1). alpha(2).');

    await repl.session.beginTurn();
    // The reserved name is only discovered while the terms are committed, which
    // is *after* the replacement used to have abolished `alpha/1`.
    const failed = await repl.session.define('alpha/1', 'alpha(9). repl_listing(2).');
    expect(failed.ok).toBe(false);
    expect(String(failed.error)).toMatch(/reserved/i);

    // The old predicate is intact and still callable — not merely restorable.
    expect(clausesOf('alpha(X)')).toEqual([1, 2]);
    await repl.session.rollbackTurn();
  });

  it('does not let a following end_turn commit the damage', async () => {
    await install('eps/1', 'eps(1). eps(2).');

    await repl.session.beginTurn();
    expect((await repl.session.define('eps/1', 'eps(9). repl_listing(2).')).ok).toBe(false);
    // The agent does the wrong thing and commits anyway.
    const commit = await repl.session.endTurn();

    expect(commit.changed).toEqual([]);
    expect(clausesOf('eps(X)')).toEqual([1, 2]);
  });

  it('restores the predicate even if the commit itself throws', async () => {
    // A tool/2 block whose schema cannot be extracted throws inside the commit,
    // after the target has been abolished.
    await install('toolish/1', 'toolish(1).');
    await repl.session.beginTurn();
    const failed = await repl.session.define('toolish/1', 'tool(toolish(1), missing_schema).');
    await repl.session.rollbackTurn();

    if (!failed.ok) {
      expect(clausesOf('toolish(X)')).toEqual([1]);
    }
  });
});

describe('assert does not silently mix versions', () => {
  it('refuses assert_many onto a predicate that has clauses', async () => {
    await install('beta/1', 'beta(1).');

    await repl.session.beginTurn();
    const refused = await repl.session.assertMany('beta(2).');
    expect(refused.ok).toBe(false);
    expect(String(refused.error)).toMatch(/already has 1 clause/);
    // Names the remedy, so the agent does not just retry.
    expect(String(refused.error)).toMatch(/define/);

    expect(clausesOf('beta(X)')).toEqual([1]);
    await repl.session.rollbackTurn();
  });

  it('refuses assert onto a predicate that has clauses', async () => {
    await install('kappa/1', 'kappa(1).');

    await repl.session.beginTurn();
    expect((await repl.session.assertClause('kappa(3).')).ok).toBe(false);
    expect(clausesOf('kappa(X)')).toEqual([1]);
    await repl.session.rollbackTurn();
  });

  it('still allows defining a predicate that does not exist', async () => {
    await repl.session.beginTurn();
    expect((await repl.session.assertMany('fresh(1).')).ok).toBe(true);
    expect((await repl.session.endTurn()).committed).toBe(true);
    expect(clausesOf('fresh(X)')).toEqual([1]);
  });
});

describe('retract removes what it was told to', () => {
  it('removes only the matching clauses and reports the count', async () => {
    await install('g/1', 'g(1). g(2).');

    await repl.session.beginTurn();
    const removed = await repl.session.retractClause('g(1)');
    expect(removed.ok).toBe(true);
    // It used to report plain `ok` while removing everything; the arguments of
    // the pattern were discarded entirely.
    expect(String(removed.note)).toBe('removed 1 clause');
    expect(clausesOf('g(X)')).toEqual([2]);
    await repl.session.endTurn();
  });

  it('errors when nothing matches, instead of reporting success', async () => {
    await install('g/1', 'g(1).');

    await repl.session.beginTurn();
    const removed = await repl.session.retractClause('g(999)');
    expect(removed.ok).toBe(false);
    expect(String(removed.error)).toMatch(/nothing was removed/);
    expect(clausesOf('g(X)')).toEqual([1]);
    await repl.session.rollbackTurn();
  });

  it('honours a rule pattern', async () => {
    await install('r/1', 'r(X) :- X > 1.\nr(X) :- X =:= 0.');

    await repl.session.beginTurn();
    const removed = await repl.session.retractClause('r(X) :- X > 1');
    expect(removed.ok).toBe(true);
    expect(String(removed.note)).toBe('removed 1 clause');
    // The remaining clause needs a bound argument, so ask it directly.
    expect(holds('r(0)')).toBe(true);
    expect(holds('r(2)')).toBe(false);
    await repl.session.endTurn();
  });
});

describe('one transaction at a time', () => {
  it('refuses a nested begin_turn, so rollback still means the last commit', async () => {
    await install('delta/1', 'delta(1).');

    expect((await repl.session.beginTurn()).ok).toBe(true);
    await repl.session.define('delta/1', 'delta(2).');
    const nested = await repl.session.beginTurn();
    expect(nested.ok).toBe(false);
    expect(String(nested.error)).toMatch(/already open/);

    // Without the refusal this re-snapshot restored delta(2) — the state the
    // agent was trying to discard.
    await repl.session.rollbackTurn();
    expect(clausesOf('delta(X)')).toEqual([1]);
  });
});

describe('define replaces the predicate it names', () => {
  it('refuses a target the block does not define', async () => {
    await install('h/1', 'h(1).');

    await repl.session.beginTurn();
    const refused = await repl.session.define('h/9', 'h(2).');
    expect(refused.ok).toBe(false);
    expect(String(refused.error)).toMatch(/h\/9/);
    expect(String(refused.error)).toMatch(/h\/1/);

    expect(clausesOf('h(X)')).toEqual([1]);
    await repl.session.rollbackTurn();
  });

  it('treats the head as an indicator, not a division term', async () => {
    // `demo/1` reads as the operator term `/(demo, 1)`; treating it as a clause
    // head yields the functor `//2` and the target was silently ignored.
    await install('demo/1', 'demo(1).');
    await repl.session.beginTurn();
    expect((await repl.session.define('demo/1', 'demo(2).')).ok).toBe(true);
    await repl.session.endTurn();
    expect(clausesOf('demo(X)')).toEqual([2]);
  });
});
