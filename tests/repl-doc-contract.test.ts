/**
 * `doc/2` documents a predicate; it does not create one. A doc/2 whose predicate
 * has no clauses names a goal that fails silently, which is how a harness ends up
 * returning empty results with no error and a healthy-looking listing.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-doc-contract-'));
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
  return (result.warnings ?? []).filter((w) => w.startsWith('doc('));
}

describe('end_turn documentation validation', () => {
  it('warns about a documented predicate with no clauses', async () => {
    const warnings = await commit('doc(author_of/3, "Author name, affiliation, email.").');
    expect(warnings.join(' ')).toContain('author_of/3');
    expect(warnings.join(' ')).toContain('no clauses');
  });

  it('accepts a documented predicate that has clauses', async () => {
    const warnings = await commit([
      'author_of(Name, Affiliation, Email) :- Name = "x", Affiliation = "y", Email = "z".',
      'doc(author_of/3, "Author name, affiliation, email.").',
    ].join('\n'));
    expect(warnings).toEqual([]);
  });

  it('accepts a doc for a predicate defined in another clause of the same commit', async () => {
    const warnings = await commit([
      'doc(f/1, "doc first, clause second.").',
      'f(ok).',
    ].join('\n'));
    expect(warnings).toEqual([]);
  });

  it('does not warn about module_doc/1 or doc-less predicates', async () => {
    const warnings = await commit([
      'module_doc("A harness with no documented predicates.").',
      'plain(1).',
    ].join('\n'));
    expect(warnings).toEqual([]);
  });
});
