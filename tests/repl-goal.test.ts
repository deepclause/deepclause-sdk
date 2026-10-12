/**
 * M1 — session + goal mode.
 *
 * Covers goal enumeration, loading DML, `[file].` consult, reset, and error
 * handling through the public HarnessRepl facade.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { ReplRenderer } from '../src/repl/render.js';
import { runInteractiveGoal } from '../src/cli/repl.js';
import type { DMLEvent } from '../src/types.js';

let workspace: string;
let repl: HarnessRepl;

async function openRepl(): Promise<HarnessRepl> {
  return createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
  });
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

function solutionValues(events: DMLEvent[], name: string): string[] {
  return events
    .filter((event) => event.type === 'solution')
    .map((event) => event.solution?.bindings.find((binding) => binding.name === name)?.value ?? '');
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-repl-'));
  repl = await openRepl();
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('goal mode', () => {
  it('enumerates every solution of member/2', async () => {
    const events = await collect(repl.query('member(X, [a, b, c])'));
    expect(events.filter((event) => event.type === 'solution')).toHaveLength(3);
    expect(solutionValues(events, 'X')).toEqual(['a', 'b', 'c']);
    expect(events.at(-1)?.type).toBe('finished');
  });

  it('stops `execute` enumeration at the solution limit', async () => {
    let solutionEvents = 0;
    const result = await repl.session.execute(
      'member(X, [1,2,3,4,5,6,7,8,9,10])',
      2,
      undefined,
      (event) => { if (event.type === 'solution') solutionEvents += 1; },
    );
    expect(result.solutions).toHaveLength(2);
    expect(solutionEvents).toBe(2);
    expect(result.bindings.find((binding) => binding.name === 'X')?.value).toBe('1');
  });

  it('binds variables after loading DML code', async () => {
    const loaded = await repl.session.loadCode(
      'fact(0, 1).\nfact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.\n',
    );
    expect(loaded.error).toBeUndefined();

    const events = await collect(repl.query('fact(5, F)'));
    expect(solutionValues(events, 'F')).toEqual(['120']);
  });

  it('counts the user predicates in the harness', async () => {
    expect(repl.session.predicateCount()).toBe(0);
    await repl.session.loadCode('a(1).\nb(2).\nc(X) :- X = 1.');
    expect(repl.session.predicateCount()).toBe(3);
  });

  it('keeps a predicate annotated with tool/2 callable from a goal', async () => {
    const loaded = await repl.session.loadCode(
      'strlen(Text, N) :- atom_length(Text, N).\n'
      + 'tool(strlen(Text, N), "Length of an atom (test).") :- strlen(Text, N).\n',
    );
    expect(loaded.error).toBeUndefined();

    // The capability is reachable from execute/goal: ...
    const events = await collect(repl.query('strlen(abcd, N)'));
    expect(solutionValues(events, 'N')).toEqual(['4']);
    // ... and the annotation registers it for the nested agent.
    expect(await repl.list('tools')).toContain('strlen');
  });

  it('does not expose a predicate defined only inside tool/2', async () => {
    const loaded = await repl.session.loadCode(
      'tool(hidden(Value), "Only a tool.") :- Value = 7.\n',
    );
    expect(loaded.error).toBeUndefined();

    const events = await collect(repl.query('hidden(V)'));
    expect(events.some((event) => event.type === 'error')).toBe(true);
    expect(events.some((event) => event.type === 'solution')).toBe(false);
  });

  it('renders output events in the interactive goal pager (not only solutions)', async () => {
    const sink: string[] = [];
    const renderer = new ReplRenderer({ mode: 'plain', color: false, sink: (text) => sink.push(text) });
    // The pager stops after the first solution ('.'); output events must still render.
    await runInteractiveGoal(repl, renderer, 'X = 1, output("val {X}")', {}, async () => '.');
    const rendered = sink.join('');
    expect(rendered).toContain('val 1');
    expect(rendered).toContain('X = 1');
  });

  it('interpolates {Var} inside a raw goal', async () => {
    const events = await collect(repl.query('X = "hi", output("got {X}")'));
    expect(events.filter((event) => event.type === 'output').map((event) => event.content)).toContain('got hi');
  });

  it('reports undefined predicates as an error event instead of hanging', async () => {
    const events = await collect(repl.query('no_such_predicate(X)'));
    expect(events.some((event) => event.type === 'error')).toBe(true);
    expect(events.at(-1)?.type).toBe('error');
  });

  it('reports a syntax error without treating the input as a prompt', async () => {
    const events = await collect(repl.query('member(X, [a,b'));
    const error = events.find((event) => event.type === 'error');
    expect(error?.content).toContain('syntax_error');
  });

  it('captures format/2 output as an output event', async () => {
    const events = await collect(repl.query('format("value=~w~n", [42])'));
    expect(events.some((event) => event.type === 'output' && event.content === 'value=42\n')).toBe(true);
  });

  it('commits at answer/1 and stops enumeration', async () => {
    const events = await collect(repl.query('answer("done")'));
    expect(events.some((event) => event.type === 'answer' && event.content === 'done')).toBe(true);
    expect(events.at(-1)?.type).toBe('finished');
  });

  it('merges multiple loaded files in order', async () => {
    await fs.writeFile(path.join(workspace, 'a.dml'), 'from_a(1).\n');
    await fs.writeFile(path.join(workspace, 'b.dml'), 'from_b(2).\n');
    expect((await repl.load('a.dml')).error).toBeUndefined();
    expect((await repl.load('b.dml')).error).toBeUndefined();

    const events = await collect(repl.query('from_a(A), from_b(B)'));
    const solution = events.find((event) => event.type === 'solution');
    expect(solution?.solution?.bindings).toEqual([
      { name: 'A', value: '1' },
      { name: 'B', value: '2' },
    ]);
  });

  it('loads a file through [file]. consult', async () => {
    await fs.writeFile(path.join(workspace, 'extra.dml'), 'consulted(7).\n');
    const events = await collect(repl.query('["extra.dml"]'));
    expect(events.some((event) => event.type === 'error')).toBe(false);

    const result = await collect(repl.query('consulted(X)'));
    expect(solutionValues(result, 'X')).toEqual(['7']);
  });

  it('empties the living module on reset', async () => {
    await repl.session.loadCode('temp(1).\n');
    expect(solutionValues(await collect(repl.query('temp(X)')), 'X')).toEqual(['1']);

    await repl.reset();
    const after = await collect(repl.query('temp(X)'));
    expect(after.some((event) => event.type === 'error')).toBe(true);
    expect(after.some((event) => event.type === 'solution')).toBe(false);
  });
});
