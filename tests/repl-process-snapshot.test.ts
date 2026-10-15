/**
 * A process runs the harness **as of its spawn**, and its snapshot is
 * inspectable.
 *
 * The danger this guards: the L2 worker host loaded the harness once at worker
 * start, so every later spawn in a long REPL run ran the code from the worker's
 * first spawn — committing an edit changed nothing for new subagents. The fix
 * pushes the current snapshot with each spawn and stamps every process with a
 * version, so "is this worker stale?" is a comparison rather than a guess.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl | undefined;

async function define(nameArity: string, clauses: string): Promise<void> {
  await repl!.session.beginTurn();
  await repl!.session.define(nameArity, clauses);
  await repl!.session.endTurn();
}

async function waitDone(id: string, timeoutMs = 15_000): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const info = repl!.session.processes().find((p) => p.pid.id === id);
    if (info && ['done', 'crashed', 'killed'].includes(info.state)) return;
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  throw new Error('process did not finish');
}

function collector(): string[] {
  const outputs: string[] = [];
  repl!.session.setProcessEventSink((event) => {
    if (event.type === 'output' && event.content) outputs.push(event.content);
  });
  return outputs;
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-snapshot-'));
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    execution: 'inline',
  });
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('process harness snapshots', () => {
  it('runs the module as of each spawn, and versions it', async () => {
    const outputs = collector();
    await define('ver/0', 'ver :- output("v1").');
    const one = await repl!.session.spawnProcess('ver', { name: 'one' });
    await waitDone(one.id);

    await define('ver/0', 'ver :- output("v2").');
    const two = await repl!.session.spawnProcess('ver', { name: 'two' });
    await waitDone(two.id);

    // The second spawn must pick up the committed edit.
    expect(outputs).toEqual(['v1', 'v2']);

    const v1 = repl!.session.processInfo(one)?.harnessVersion;
    const v2 = repl!.session.processInfo(two)?.harnessVersion;
    expect(v1).toBeTruthy();
    expect(v2).toBeTruthy();
    expect(v1).not.toBe(v2);
    // Exactly one of them is the current harness.
    const current = repl!.session.currentHarnessVersion();
    expect(current).toBe(v2);
    expect(current).not.toBe(v1);

    // And the snapshot source is inspectable.
    expect(repl!.session.processCode(two)).toContain('v2');
    expect(repl!.session.processCode(one)).toContain('v1');
  });

  it('lets a worker host run an adopted snapshot even when its module differs', async () => {
    const outputs = collector();
    await define('ver/0', 'ver :- output("live").');
    // What the main pushes to a worker: not the worker's own module.
    repl!.session.setProcessSnapshot('ver :- output("pushed").', 'adoptedVersion');

    const pid = await repl!.session.spawnProcess('ver');
    await waitDone(pid.id);

    expect(outputs).toEqual(['pushed']);
    expect(repl!.session.processInfo(pid)?.harnessVersion).toBe('adoptedVersion');
    expect(repl!.session.processCode(pid)).toContain('pushed');
  });

  it('reports the current version through /ps --code with no target', () => {
    expect(repl!.session.currentHarnessVersion()).toMatch(/^[0-9a-f]{12}$/);
  });
});
