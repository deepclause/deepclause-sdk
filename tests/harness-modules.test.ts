import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-modules-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

async function open(modules: Array<{ id: string; name: string; version?: string; hash?: string; source: string }>) {
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    execution: 'inline',
    trace: false,
    harnessModules: modules,
  });
  return repl;
}

describe('harness modules', () => {
  it('loads an exported predicate without exposing its private helpers', async () => {
    const support = [
      ':- export([answer_for/1]).',
      'answer_for(X) :- private_helper(X).',
      'private_helper(42).',
    ].join('\n');
    await open([{ id: 'dcmod_support_1', name: 'support', version: '1', hash: 'support-hash', source: support }]);
    const loaded = await repl!.session.loadCode([
      ':- export([main/1]).',
      ':- use_harness(support, version(1), [answer_for/1]).',
      'main(X) :- answer_for(X).',
    ].join('\n'));
    expect(loaded.error).toBeUndefined();
    let answer: unknown;
    for await (const event of repl!.session.query('main(X)')) {
      if (event.type === 'solution') answer = event.solution?.bindings?.find((binding) => binding.name === 'X')?.value;
      if (event.type === 'error') throw new Error(event.content ?? 'query failed');
    }
    expect(answer).toBe('42');
    const privateQuery = [];
    for await (const event of repl!.session.query('private_helper(X)')) privateQuery.push(event);
    expect(privateQuery.some((event) => event.type === 'error')).toBe(true);

    const info = repl!.session.harnessModuleInfo();
    expect(info.exports).toContain('main/1');
    expect(info.modules.find((module) => module.name === 'support')?.exports).toContain('answer_for/1');
    const moduleId = info.modules.find((module) => module.name === 'support')?.module;
    expect(moduleId).toBeTruthy();
    const qualifiedPrivate = [];
    for await (const event of repl!.session.query(`${moduleId}:private_helper(X)`)) qualifiedPrivate.push(event);
    expect(qualifiedPrivate.some((event) => event.type === 'error')).toBe(true);
  });

  it('resolves imports transitively and keeps the version pin', async () => {
    const base = [
      ':- export([value/1]).',
      'value(7).',
    ].join('\n');
    const middle = [
      ':- export([middle_value/1]).',
      ':- use_harness(base, version(2), [value/1]).',
      'middle_value(X) :- value(X).',
    ].join('\n');
    await open([
      { id: 'dcmod_base_2', name: 'base', version: '2', hash: 'base-hash', source: base },
      { id: 'dcmod_middle_1', name: 'middle', version: '1', hash: 'middle-hash', source: middle },
    ]);
    const loaded = await repl!.session.loadCode([
      ':- use_harness(middle, version(1), [middle_value/1]).',
      'main(X) :- middle_value(X).',
    ].join('\n'));
    expect(loaded.error).toBeUndefined();
    let answer: unknown;
    for await (const event of repl!.session.query('main(X)')) {
      if (event.type === 'solution') answer = event.solution?.bindings?.find((binding) => binding.name === 'X')?.value;
    }
    expect(answer).toBe('7');
  });

  it('rejects invalid exports and imports and prevents redefinition', async () => {
    await open([{ id: 'dcmod_support_1', name: 'support', version: '1', hash: 'h', source: ':- export([f/1]).\nf(1).' }]);
    const badExport = await repl!.session.loadCode(':- export([missing/1]).');
    expect(badExport.error).toContain('exported predicate is not defined');

    const imported = await repl!.session.loadCode([
      ':- use_harness(support, [f/1]).',
      'main(X) :- f(X).',
    ].join('\n'));
    expect(imported.error).toBeUndefined();
    const redefined = await repl!.session.loadCode([
      ':- use_harness(support, [f/1]).',
      'f(X) :- X = 2.',
    ].join('\n'));
    expect(redefined.error).toContain('read-only');
    const wrongExport = await repl!.session.loadCode([
      ':- use_harness(support, [missing/1]).',
    ].join('\n'));
    expect(wrongExport.error).toContain('does not export');
  });
});

describe('harness module bundles', () => {
  it('carries imported module sources into a zip bundle', async () => {
    const support = ':- export([value/1]).\nvalue(9).';
    await open([{ id: 'dcmod_bundle_1', name: 'bundle', version: '1', hash: 'h', source: support }]);
    const loaded = await repl!.session.loadCode([
      ':- use_harness(bundle, [value/1]).',
      'main(X) :- value(X).',
    ].join('\n'));
    expect(loaded.error).toBeUndefined();
    const bundle = path.join(workspace, 'export.zip');
    await repl!.session.export(bundle);
    await repl!.dispose();
    repl = undefined;

    const imported = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai', execution: 'inline' });
    repl = imported;
    const result = await imported.session.load(bundle);
    expect(result.error).toBeUndefined();
    let answer: unknown;
    for await (const event of imported.session.query('main(X)')) {
      if (event.type === 'solution') answer = event.solution?.bindings?.find((binding) => binding.name === 'X')?.value;
    }
    expect(answer).toBe('9');
  });
});
