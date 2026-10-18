/**
 * What `end_turn` says happened.
 *
 * Both fields lied in a way that cost four edit attempts: `committed` was
 * hard-coded `true` (so a transaction whose every edit was *refused* reported
 * `{committed: true, changed: []}`), and `changed` was a set diff over predicate
 * indicators, so *replacing* a predicate — the most common edit — reported `[]`
 * while the clause block had been swapped. Reading `{committed: true,
 * changed: []}` as "committed, nothing changed" is the natural conclusion.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-commit-report-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('end_turn reports what actually happened', () => {
  it('marks a replaced predicate as rewritten, not as nothing', async () => {
    await repl.session.beginTurn();
    await repl.session.define('greet/1', 'greet(ok).');
    await repl.session.endTurn();

    await repl.session.beginTurn();
    await repl.session.define('greet/1', 'greet(fine).');
    const result = await repl.session.endTurn();

    expect(result.committed).toBe(true);
    expect(result.changed).toEqual(['~greet/1']);
  });

  it('says nothing was committed when every edit was refused', async () => {
    await repl.session.beginTurn();
    await repl.session.define('web_sources/3', 'web_sources(Q, C, S) :- C = 3, S = [].');
    await repl.session.endTurn();

    await repl.session.beginTurn();
    // The head says /2, the block defines /3: refused (correctly).
    const refused = await repl.session.define('web_sources/2', 'web_sources(Q, C, S) :- C = 3, S = [].');
    expect(refused.ok).toBe(false);
    const result = await repl.session.endTurn();

    // Not `{committed: true, changed: []}` — nothing was recorded.
    expect(result.committed).toBe(false);
    expect(result.changed).toEqual([]);
    expect(repl.session.harnessView().predicates.map((predicate) => predicate.indicator))
      .toContain('web_sources/3');
  });

  it('says nothing was committed for an empty transaction', async () => {
    await repl.session.beginTurn();
    const result = await repl.session.endTurn();
    expect(result.committed).toBe(false);
    expect(result.changed).toEqual([]);
  });

  it('still reports added and removed predicates', async () => {
    await repl.session.beginTurn();
    await repl.session.define('old/1', 'old(1).');
    await repl.session.endTurn();

    await repl.session.beginTurn();
    await repl.session.assertMany('fresh(1).');
    await repl.session.deletePredicate('old/1');
    const result = await repl.session.endTurn();

    expect(result.changed).toEqual(['+fresh/1', '-old/1']);
  });
});

describe('changing a predicate’s arity', () => {
  it('needs both a delete and a define: define only replaces the arities its block defines', async () => {
    await repl.session.beginTurn();
    await repl.session.define('web_sources/3', 'web_sources(Q, C, S) :- C = 3, S = [].');
    await repl.session.endTurn();

    // Define alone adds the new arity and leaves the old one behind — a second
    // predicate, which is why a harness can look "stuck" at the old arity.
    await repl.session.beginTurn();
    await repl.session.define('web_sources/2', 'web_sources(Q, S) :- S = [].');
    let result = await repl.session.endTurn();
    expect(result.changed).toEqual(['+web_sources/2']);
    expect(repl.session.harnessView().predicates.map((predicate) => predicate.indicator))
      .toEqual(expect.arrayContaining(['web_sources/2', 'web_sources/3']));

    // The recipe: delete the old arity in the same transaction.
    await repl.session.beginTurn();
    await repl.session.deletePredicate('web_sources/3');
    result = await repl.session.endTurn();
    expect(result.changed).toEqual(['-web_sources/3']);
    expect(repl.session.harnessView().predicates.map((predicate) => predicate.indicator))
      .not.toContain('web_sources/3');
  });
});
