/**
 * `listing` filters: a predicate, a prefix, or everything — and nothing else.
 *
 * A catch-all clause used to make any *unmatched* filter mean "the whole module",
 * silently. `listing('alpha/1')` listed every predicate, which is how the harness side
 * view's drill-down ended up showing the module instead of the predicate clicked.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-listing-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
  await repl.session.beginTurn();
  await repl.session.define('alpha/1', 'alpha(1). alpha(2).');
  await repl.session.define('gamma/1', 'gamma(9).');
  await repl.session.define('alphabet/2', 'alphabet(x, y).');
  await repl.session.endTurn();
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

/** The predicate indicators a listing contains. */
function indicators(text: string): string[] {
  return text
    .split('\n')
    .map((line) => line.trim())
    .filter((line) => /^[a-z][A-Za-z0-9_]*\/\d+$/.test(line));
}

describe('listing filters', () => {
  it('lists one predicate for a name/arity filter', async () => {
    expect(indicators(await repl.session.listing('alpha/1'))).toEqual(['alpha/1']);
    expect(indicators(await repl.session.listing('gamma/1'))).toEqual(['gamma/1']);
  });

  it('does not confuse same-named predicates of different arity', async () => {
    await repl.session.beginTurn();
    await repl.session.define('alpha/2', 'alpha(a, b).');
    await repl.session.endTurn();
    expect(indicators(await repl.session.listing('alpha/1'))).toEqual(['alpha/1']);
    expect(indicators(await repl.session.listing('alpha/2'))).toEqual(['alpha/2']);
  });

  it('lists a prefix, and everything for `all`', async () => {
    expect(indicators(await repl.session.listing('alpha')).sort()).toEqual(['alpha/1', 'alphabet/2']);
    expect(indicators(await repl.session.listing('all')).sort()).toEqual(['alpha/1', 'alphabet/2', 'gamma/1']);
  });

  it('lists nothing for a filter that matches no predicate', async () => {
    // The important half: an unrecognised filter must not mean "everything".
    expect(indicators(await repl.session.listing('nothing_matches_this'))).toEqual([]);
  });
});
