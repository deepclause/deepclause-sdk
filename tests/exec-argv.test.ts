/**
 * `exec(bash(argv: [...]), R)` — the runtime quotes, so the author never does.
 *
 * The docs' loudest warning is "never interpolate untrusted data into a command".
 * Passing a list removes the opportunity: every element is single-quoted by the
 * runtime, so a value containing a space, a quote, `$`, backticks or `;` is
 * inert. It still runs through the same launch plan, so the sandbox is unchanged.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { existsSync } from 'fs';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { createReplToolset, type ReplToolset } from '../src/repl/toolset.js';
import { plainValue } from '../src/repl/frames.js';

let workspace: string;
let repl: HarnessRepl;
let toolset: ReplToolset;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-argv-'));
  // The capability toolset is not registered implicitly; pass it explicitly.
  toolset = await createReplToolset({ workspacePath: workspace });
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    tools: toolset.tools,
    hostToolsCatalog: toolset.catalog,
  });
});

afterEach(async () => {
  await repl.dispose();
  await toolset.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

/**
 * Run a goal and read one binding as text. The event carries Prolog's *rendered*
 * form, so a string arrives with its quotes: `"hello"`, not `hello`.
 */
async function out(goal: string, name = 'O'): Promise<string> {
  const events: Array<{ type: string; solution?: { bindings: Array<{ name: string; value: unknown }> } }> = [];
  for await (const e of repl.query(goal)) events.push(e as never);
  const binding = events
    .find((e) => e.type === 'solution')
    ?.solution?.bindings.find((b) => b.name === name);
  return String(plainValue(binding?.value) ?? '');
}

describe('exec(bash(argv: …))', () => {
  it('passes an argument through verbatim, including shell metacharacters', async () => {
    const marker = path.join(workspace, 'pwned');
    const payload = `a b;$(echo pwned > ${marker}) && echo x`;
    const goal = `exec(bash(argv: ["printf", "%s", ${JSON.stringify(payload)}]), R), get_dict(stdout, R, O)`;
    expect(await out(goal)).toContain(payload);
    // The `$(…)` was never executed: it is data, not code.
    expect(existsSync(marker)).toBe(false);
  });

  it('handles a single quote in an argument', async () => {
    const payload = `it's a 'test'`;
    const goal = `exec(bash(argv: ["printf", "%s", ${JSON.stringify(payload)}]), R), get_dict(stdout, R, O)`;
    expect(await out(goal)).toContain(payload);
  });

  it('matches the equivalent quoted command for a benign case', async () => {
    const viaArgv = await out('exec(bash(argv: ["printf", "%s", "hello"]), R), get_dict(stdout, R, O)');
    const viaCommand = await out(`exec(bash(command: ${JSON.stringify('printf %s hello')}), R), get_dict(stdout, R, O)`);
    expect(viaArgv).toContain('hello');
    expect(viaArgv).toBe(viaCommand);
  });

  it('reports a failing binary as a failed tool call', async () => {
    const events: Array<{ type: string; toolState?: string }> = [];
    for await (const e of repl.query('exec(bash(argv: ["definitely-not-a-binary-xyz"]), R)')) {
      events.push(e as never);
    }
    // Either the exec fails (no solution) or the tool call is reported failed;
    // what must not happen is a silent success.
    const succeeded = events.some((e) => e.type === 'solution');
    const failed = events.some((e) => e.toolState === 'failed');
    expect(succeeded && !failed).toBe(false);
  });
});
