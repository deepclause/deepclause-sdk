import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-trace-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

async function run(goal: string, trace = true): Promise<void> {
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    execution: 'inline',
    trace,
  });
  await repl.session.loadCode([
    'leaf(1).',
    'leaf(2).',
    'parent(X) :- leaf(X), write(X).',
  ].join('\n'));
  for await (const event of repl.session.query(goal)) {
    if (event.type === 'error') throw new Error(event.content ?? 'query failed');
  }
}

describe('full MI execution trace', () => {
  it('records call, exit and output entries with JSON-safe text', async () => {
    await run('parent(X)');
    const entries = await repl!.session.trace();
    expect(entries.length).toBeGreaterThan(0);
    const parentCall = entries.find((entry) => entry.predicate === 'parent' && entry.type === 'call');
    const parentExit = entries.find((entry) => entry.predicate === 'parent' && entry.type === 'exit');
    const leafCall = entries.find((entry) => entry.predicate === 'leaf' && entry.type === 'call');
    expect(parentCall?.argsText).toContain('_');
    expect(parentExit?.resultText).toContain('parent(1)');
    expect(leafCall).toBeDefined();
    expect(entries.some((entry) => entry.type === 'output')).toBe(true);
  });

  it('returns an empty trace when tracing is disabled', async () => {
    await run('parent(X)', false);
    await expect(repl!.session.trace()).resolves.toEqual([]);
  });

  it('keeps trace entries across turns', async () => {
    await run('leaf(1)');
    const first = await repl!.session.trace();
    for await (const event of repl!.session.query('leaf(2)')) {
      if (event.type === 'error') throw new Error(event.content ?? 'query failed');
    }
    const second = await repl!.session.trace();
    expect(second.length).toBeGreaterThan(first.length);
  });

  it('keeps entry order and depth for nested calls', async () => {
    await run('parent(X)');
    const entries = await repl!.session.trace();
    const parent = entries.findIndex((entry) => entry.predicate === 'parent' && entry.type === 'call');
    const leaf = entries.findIndex((entry) => entry.predicate === 'leaf' && entry.type === 'call');
    expect(leaf).toBeGreaterThan(parent);
    expect(entries[leaf]?.depth).toBeGreaterThan(entries[parent]?.depth ?? 0);
  });
});
