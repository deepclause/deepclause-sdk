/**
 * `tool/2` is only an annotation: the nested agent inside `task/N` calls it, and
 * a call runs the annotation's body. A body that calls a different arity, or
 * the tool head itself, fails only when a nested agent calls the tool — so
 * `end_turn` reports it as a contract warning.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-tool-contract-'));
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
  return (result.warnings ?? []).filter((w) => w.includes('tool '));
}

describe('end_turn tool contract validation', () => {
  it('accepts a correctly delegating tool', async () => {
    const warnings = await commit([
      'add(A, B, C) :- C is A + B.',
      'tool(add(A, B, C), "Add two numbers.") :- add(A, B, C).',
    ].join('\n'));
    expect(warnings).toEqual([]);
  });

  it('flags a tool whose delegate has a different arity', async () => {
    const warnings = await commit([
      'bibliography(Query, Max, Entries) :- findall(Query, member(Query, [a, b]), Entries), Max >= 0.',
      'tool(bibliography(A, B), "Search and read.") :- bibliography(A, B, C).',
    ].join('\n'));
    expect(warnings.join(' ')).toContain('bibliography/2');
    expect(warnings.join(' ')).toContain('bibliography/3');
  });

  it('flags a tool head whose body calls the same head with the same arity', async () => {
    // Self-delegation: the tool body calls itself, so a nested agent recurses.
    const warnings = await commit([
      'tool(loop(A, B), "Reads a file.") :- loop(A, B).',
    ].join('\n'));
    expect(warnings.join(' ')).toContain('loop/2');
    expect(warnings.join(' ')).toMatch(/itself|recursion/);
  });

  it('flags a tool whose predicate does not exist', async () => {
    const warnings = await commit('tool(ghost(A, B), "Missing.") :- ghost(A, B).');
    expect(warnings.join(' ')).toContain('ghost/2');
  });

  it('flags a tool that delegates to a module-qualified skill', async () => {
    // Known limitation: a cross-module delegate is not a session predicate, so
    // it is reported. with_tools/2 is the supported route for those.
    const warnings = await commit('tool(saved(A, B), "Saves.") :- other_mod:save(A, B).');
    expect(warnings.join(' ')).toContain('saved/2');
  });
});

describe('the research_assistant turn', () => {
  it('catches every broken tool from that session, and none of the correct ones', async () => {
    const warnings = await commit([
      // real implementations (as that turn wrote them)
      'bib_search(A, B, C) :- C = A-B.',
      'bibliography(A, B, C) :- C = A-B.',
      'citations(A, B) :- B = A.',
      'citations_in(A, B, C) :- C = A-B.',
      'fetch_paper(A, B) :- B = A.',
      'fetch_pdf(A, B) :- B = A.',
      'get_paper(A, B) :- B = A.',
      'paper_profile(A, B) :- B = A.',
      'read_pdf(A, B) :- B = A.',
      // the annotations, verbatim in shape from that session
      'tool(bib_search(A,B,C), "Search arXiv.") :- bib_search(A,B,C).',
      'tool(bibliography(A,B), "Search and read.") :- bibliography(A,B,C).',
      'tool(citations(A,B), "Extract citations.") :- citations(A,B).',
      'tool(citations_in(A,B,C), "In a section.") :- citations_in(A,B,C).',
      'tool(fetch_paper(A,B), "Download URL.") :- fetch_paper(A,B).',
      'tool(fetch_pdf(A,B), "Download arXiv.") :- fetch_pdf(A,B).',
      'tool(get_paper(A,B), "One step.") :- get_paper(A,B).',
      'tool(paper_profile(A), "Profile.") :- paper_profile(A,B).',
      'tool(read_pdf(A,B), "PDF to text.") :- read_pdf(A,B).',
    ].join('\n'));
    const toolWarnings = warnings.filter((w) => w.startsWith('tool '));
    const flagged = toolWarnings.map((w) => w.split(' ')[1]);
    // bibliography/2 and paper_profile/1: arity mismatch with the body.
    expect(flagged).toContain('bibliography/2');
    expect(flagged).toContain('paper_profile/1');
    // the rest are correct delegations to real predicates.
    expect(flagged).not.toContain('bib_search/3');
    expect(flagged).not.toContain('citations_in/3');
    expect(flagged).not.toContain('read_pdf/2');
    expect(flagged).not.toContain('fetch_pdf/2');
  });
});
