/**
 * A child spawned inside a real L2 worker must run the harness.
 *
 * Nothing checked this: `repl-process-spawn.test.ts` pins `execution: 'inline'`
 * ("for determinism") and `repl-process-worker.test.ts` drives a scripted worker,
 * so neither looks at a *worker-spawned child's module*. It was broken once — a
 * stale `dist/repl/process-worker.js` ran old worker code against a new main — and
 * the only symptom was `existence_error` inside the child, which reads like the
 * harness vanished. `WORKER_PROTOCOL_VERSION` now turns a stale build into a clear
 * failure; this is the positive case.
 *
 * The worker loads from `dist`, so this test needs `npm run build`. When the build
 * is missing it is skipped rather than failing for the wrong reason.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';
import { existsSync } from 'fs';
import { fileURLToPath } from 'url';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

const here = path.dirname(fileURLToPath(import.meta.url));
const workerEntry = path.resolve(here, '../dist/repl/process-worker.js');

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-worker-child-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe.skipIf(!existsSync(workerEntry))('a child spawned in an L2 worker', () => {
  it('calls harness predicates from its own module', async () => {
    repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });

    await repl.session.beginTurn();
    await repl.session.define('triple/2', 'triple(X, Y) :- Y is X * 3.');
    await repl.session.define('child/1', 'child(To) :- triple(4, R), send(To, got(R)).');
    await repl.session.define(
      'parent/1',
      [
        'parent(M) :-',
        '    self(Me),',
        '    spawn_opt(child(Me), P, name(c)),',
        '    (   receive(M, 15000) -> true ; M = no_message ),',
        '    (   process_info(P, I) -> true ; I = info{state: no_info} ),',
        '    format(string(S), "~w from ~w", [M, I.state]),',
        '    answer(S).',
      ].join('\n'),
    );
    await repl.session.endTurn();

    const answers: string[] = [];
    const errors: string[] = [];
    for await (const event of repl.query('parent(M)')) {
      if (event.type === 'answer' && event.content) answers.push(event.content);
      if (event.type === 'error') errors.push(event.content ?? '');
    }

    // The child ran `triple/2` from the harness snapshot: 4 * 3.
    expect(answers.join('\n')).toContain('got(12)');
    expect(answers.join('\n')).toContain('done');
    // And it did not crash on the way (the old symptom was existence_error).
    expect(errors).toEqual([]);
    expect(repl.session.processes().filter((process) => process.state === 'crashed')).toEqual([]);
  }, 40_000);
});
