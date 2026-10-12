/**
 * `end_turn` advisory probe: committed pure predicates that yield many solutions
 * on fresh variables are flagged as probably nondeterministic. Side-effecting and
 * fact-table predicates are skipped.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-nondet-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

async function commit(clauses: string): Promise<string[]> {
  await repl.session.beginTurn();
  await repl.session.assertMany(clauses);
  const result = await repl.session.endTurn();
  return result.warnings ?? [];
}

describe('end_turn nondeterminism probe', () => {
  it('warns about a predicate that enumerates on fresh variables', async () => {
    const warnings = await commit('numbers(X) :- between(1, 500, X).');
    expect(warnings.join(' ')).toContain('numbers/1');
    expect(warnings.join(' ')).toMatch(/once\/1|!/);
  });

  it('does not warn for a deterministic predicate', async () => {
    expect(await commit('double(X, Y) :- Y is X * 2.')).toEqual([]);
  });

  it('does not warn for a pure fact table', async () => {
    expect(await commit('color(red).\ncolor(green).\ncolor(blue).')).toEqual([]);
  });

  it('does not probe side-effecting predicates (no model call)', async () => {
    // `greet/2` calls task/2; the probe must skip it rather than run the model.
    expect(await commit('greet(N) :- task("say hi", N).')).toEqual([]);
  });
});
