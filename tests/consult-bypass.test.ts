/**
 * Why the authoring prompt forbids editing the module by loading a file.
 *
 * `:- consult("…")` (and a file-list directive, or `use_module` on a workspace
 * `.dml`) asserts straight into the live module instead of going through
 * `define`/`assert`/`assert_many`. Measured consequences, pinned here so the rule in
 * AGENT_PROMPT.md keeps its reason:
 *
 *  - the predicates reach the module but not the transaction's *tracked* set, so the
 *    `end_turn` nondeterminism probe skips them;
 *  - they still appear in the commit diff, so the change lands with no account of
 *    where it came from.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-consult-'));
  await fs.writeFile(path.join(workspace, 'extra.dml'), 'consulted(1).\n');
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

interface SessionView {
  sessionId: string;
  prologQuery: (goal: string) => Record<string, unknown>;
  predicateList: () => string[];
  editTouched: () => string[];
}

const view = () => repl.session as unknown as SessionView;

describe('loading a file is not an edit', () => {
  it('adds the predicates but leaves them out of the tracked set', async () => {
    await repl.session.beginTurn();
    await repl.session.assertMany('asserted(1).');
    const load = await repl.session.loadCode(':- consult("extra.dml").');
    expect(load.error).toBeUndefined();

    // Both reached the module...
    expect(view().predicateList()).toEqual(expect.arrayContaining(['asserted/1', 'consulted/1']));
    // ...but only the asserted one is tracked, which is what the probe reads.
    expect(view().editTouched()).toContain('asserted/1');
    expect(view().editTouched()).not.toContain('consulted/1');
  });

  it('still commits the consulted predicate, with no record of its origin', async () => {
    await repl.session.beginTurn();
    await repl.session.assertMany('asserted(1).');
    await repl.session.loadCode(':- consult("extra.dml").');
    const result = await repl.session.endTurn();

    expect(result.committed).toBe(true);
    // In the diff, so recorded — and indistinguishable from an edit the agent made.
    expect(result.changed).toEqual(expect.arrayContaining(['+consulted/1']));
    expect(view().predicateList()).toContain('consulted/1');
  });
});
