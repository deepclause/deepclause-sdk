/**
 * Two silent-failure bugs in the loader.
 *
 * `:- repl_delete(a/1).` fell through the directive catch-all and was discarded,
 * so a predicate the author asked to remove stayed in the module. Any other
 * unrecognised directive (`:- dynamic(f/1).`) also did nothing without saying so.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-directives-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('loader directives', () => {
  it('removes a predicate through the repl_delete directive', async () => {
    await repl.session.loadCode('temp(1).');
    expect((repl.session as unknown as { predicateList: () => string[] }).predicateList())
      .toContain('temp/1');

    const result = await repl.session.loadCode(':- repl_delete(temp/1).');
    expect(result.error).toBeUndefined();
    expect((repl.session as unknown as { predicateList: () => string[] }).predicateList())
      .not.toContain('temp/1');
  });

  it('still supports the bare repl_delete fact form', async () => {
    await repl.session.loadCode('temp(1).');
    const result = await repl.session.loadCode('repl_delete(temp/1).');
    expect(result.error).toBeUndefined();
    expect((repl.session as unknown as { predicateList: () => string[] }).predicateList())
      .not.toContain('temp/1');
  });

  it('reports an unrecognised directive instead of discarding it', async () => {
    const result = await repl.session.loadCode(':- repl_del(ghost/1).');
    expect(result.error).toBeUndefined();
    expect((result.warnings ?? []).join(' ')).toContain('repl_del');
  });

  it('tolerates the redundant dynamic/1 declaration silently', async () => {
    // Session predicates are asserted, so `dynamic` is already true and warning
    // on it would be noise in every harness that writes it.
    const result = await repl.session.loadCode(':- dynamic(ghost/1).');
    expect(result.error).toBeUndefined();
    expect(result.warnings ?? []).toEqual([]);
  });

  it('accepts the benign directives SWI manages itself', async () => {
    const result = await repl.session.loadCode(':- discontiguous(a/1).\n:- multifile(b/1).');
    expect(result.error).toBeUndefined();
    expect(result.warnings ?? []).toEqual([]);
  });
});
