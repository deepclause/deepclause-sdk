/**
 * `write/1` used to fall through to SWI's `current_output` with no handler, so its
 * text vanished in the REPL: only `format/2` and `writeln/1` were captured. A
 * debugging `write(step)` then looks exactly like a goal that produced nothing —
 * the worst possible signal, and it cost a debugging session.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-write-output-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

async function outputs(goal: string): Promise<string[]> {
  const text: string[] = [];
  for await (const event of repl.query(goal)) {
    if (event.type === 'output' && event.content) text.push(event.content);
  }
  return text;
}

describe('write/1 in the REPL', () => {
  it('reaches the output channel, like format/2 and writeln/1', async () => {
    await repl.session.beginTurn();
    await repl.session.loadCode([
      'probe :- write(first), output(" | "), writeln(second).',
      'format_probe :- format("~w-~w", [a, b]), output("").',
    ].join('\n'));
    await repl.session.endTurn();

    expect((await outputs('probe')).join('')).toContain('first');
    expect((await outputs('probe')).join('')).toContain('second');
    // format/2 was already captured; keep it that way.
    expect((await outputs('format_probe')).join('')).toContain('a-b');
  });

  it('does not invent an output event for an empty write', async () => {
    await repl.session.beginTurn();
    await repl.session.loadCode('quiet :- write("").');
    await repl.session.endTurn();
    expect(await outputs('quiet')).toEqual([]);
  });
});
