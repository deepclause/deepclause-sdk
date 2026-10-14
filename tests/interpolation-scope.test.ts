/**
 * `{var}` interpolation applies in *goal* position only.
 *
 * It used to rewrite every string anywhere in a term, nested ones included, so a
 * string of data containing braces was replaced by a format goal. In a live session
 * that truncated a `curl` argument list at `-w`, because the next element was
 * `%{http_code}`: the argv list came back as `["curl", …, "-w"]`.
 *
 * Braces are everywhere in data — JSON, `awk` programs, `jq` filters, shell
 * parameter expansion — so the rule has to be "a string is literal unless it is a
 * goal argument that interpolates".
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-interp-'));
  toolset = await createReplToolset({ workspacePath: workspace });
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'mock-model',
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

/** Rendered bindings escape `"` as `\"`; unescape for comparison. */
function unescape(value: string): string {
  return value.replace(/\\(.)/g, '$1');
}

async function out(goal: string, name = 'O'): Promise<string> {
  const events: Array<{ type: string; solution?: { bindings: Array<{ name: string; value: unknown }> } }> = [];
  for await (const e of repl.query(goal)) events.push(e as never);
  const binding = events
    .find((e) => e.type === 'solution')
    ?.solution?.bindings.find((b) => b.name === name);
  return unescape(String(plainValue(binding?.value) ?? ''));
}

/** `printf %s` each argument, so the values that reached the shell are visible. */
function argvGoal(values: string[]): string {
  const args = values.map((v) => JSON.stringify(v)).join(', ');
  return `exec(bash(argv: ["printf", "%s\\\\n", ${args}]), R), get_dict(stdout, R, O)`;
}

describe('braces in data are literal', () => {
  it('passes a shell parameter expansion through intact', async () => {
    expect(await out(argvGoal(['%{http_code}']))).toContain('%{http_code}');
  });

  it('passes JSON through intact', async () => {
    expect(await out(argvGoal(['{"a":1}']))).toContain('{"a":1}');
  });

  it('passes an awk program and a jq filter through intact', async () => {
    const result = await out(argvGoal(['{print $1}', '.items[] | {n: .name}']));
    expect(result).toContain('{print $1}');
    expect(result).toContain('.items[] | {n: .name}');
  });

  it('keeps a curl argument list complete', async () => {
    // The shape that failed: every element must reach the shell, in order.
    const goal = 'exec(bash(argv: ["printf", "%s\\\\n", '
      + '"curl", "-s", "-m", "10", "-o", "/dev/null", "-w", "%{http_code}", "https://example.test/x"]), R), '
      + 'get_dict(stdout, R, O)';
    const result = await out(goal);
    for (const part of ['curl', '-w', '%{http_code}', 'https://example.test/x']) {
      expect(result).toContain(part);
    }
  });

  it('keeps braces literal in a loaded clause body too', async () => {
    await repl.session.loadCode([
      'braces(X) :- X = "%{code} and {not_a_var}", output(X).',
      '',
    ].join('\n'));
    const events: Array<{ type: string; content?: string }> = [];
    for await (const e of repl.query('braces(_)')) events.push(e as never);
    const rendered = events.filter((e) => e.type === 'output').map((e) => e.content ?? '').join('\n');
    expect(rendered).toContain('%{code}');
    expect(rendered).toContain('{not_a_var}');
  });
});

describe('interpolation still works where it is documented', () => {
  it('interpolates a bound variable in a goal', async () => {
    const events: Array<{ type: string; content?: string }> = [];
    for await (const e of repl.query('X = "Ada", output("val {X}")')) events.push(e as never);
    const rendered = events.filter((e) => e.type === 'output').map((e) => e.content ?? '').join('\n');
    expect(rendered).toContain('val Ada');
  });

  it('interpolates in a clause body', async () => {
    // `output/1` is a documented interpolation site; `format/2` and `=` are not.
    await repl.session.loadCode('greet(Name) :- output("hi {Name}").');
    const events: Array<{ type: string; content?: string }> = [];
    for await (const e of repl.query('greet(world)')) events.push(e as never);
    const rendered = events.filter((e) => e.type === 'output').map((e) => e.content ?? '').join('\n');
    expect(rendered).toContain('hi world');
  });
});
