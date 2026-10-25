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

  it('clears the trace on request', async () => {
    await run('leaf(1)');
    expect((await repl!.session.trace()).length).toBeGreaterThan(0);
    await repl!.session.clearTrace();
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

describe('tool-scoped MI trace', () => {
  it('nests MI calls under the Work tool invocation that caused them', async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-tool-trace-'));
    let requestIndex = 0;
    const work = await createHarnessRepl({
      workspacePath: dir,
      model: 'test-model',
      provider: 'openai',
      mode: 'work',
      execution: 'inline',
      trace: true,
      llmBackend: {
        complete: async (request) => {
          if (!request.tools?.some((tool) => tool.name === 'inner')) throw new Error('inner tool missing');
          const index = requestIndex++;
          if (index === 0) return { text: '', toolCalls: [{ id: 'call-1', name: 'inner', arguments: {} }] };
          return { text: 'done' };
        },
      },
    });
    try {
      await work.session.loadCode([
        'inner(Value) :- Value = "ok".',
        'shell(_{kind:agent,system:"test",host_tools:[],tools:[',
        '  _{name:"inner",goal:"inner",description:"Inner tool",inputs:[],outputs:[_{name:"Value",type:string}]}',
        '],workflows:[],welcome:""}).',
      ].join('\n'));
      for await (const event of work.prompt('run the inner tool')) {
        if (event.type === 'error') throw new Error(event.content ?? 'prompt failed');
      }
      const entries = await work.session.trace();
      const root = entries.find((entry) => entry.type === 'tool' && entry.predicate === 'inner');
      expect(root?.id).toBeTruthy();
      const child = entries.find((entry) => entry.type === 'call' && entry.predicate === 'inner' && entry.scope === root?.id);
      expect(child).toBeDefined();
      const exit = entries.find((entry) => entry.type === 'exit' && entry.predicate === 'tool' && entry.scope === root?.id);
      expect(exit).toBeDefined();
    } finally {
      await work.dispose();
      await fs.rm(dir, { recursive: true, force: true });
    }
  });
});

describe('workflow-scoped MI trace', () => {
  it('nests a workflow MI tree under the agent run_workflow call', async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-workflow-trace-'));
    let calls = 0;
    const work = await createHarnessRepl({
      workspacePath: dir,
      model: 'test-model',
      provider: 'openai',
      mode: 'work',
      execution: 'inline',
      trace: true,
      llmBackend: {
        complete: async (request) => {
          if (!request.tools?.some((tool) => tool.name === 'run_workflow')) throw new Error('run_workflow tool missing');
          return calls++ === 0
            ? { text: '', toolCalls: [{ id: 'workflow-1', name: 'run_workflow', arguments: { name: 'Do' } }] }
            : { text: 'done' };
        },
      },
    });
    try {
      await work.session.loadCode([
        'wf(Value) :- Value = "wf".',
        'shell(_{kind:agent,system:"test",host_tools:[],tools:[],workflows:[',
        '  _{name:"Do",goal:"wf",inputs:[],outputs:[_{name:"Value",type:string}]}',
        '],welcome:""}).',
      ].join('\n'));
      for await (const event of work.prompt('run the workflow')) {
        if (event.type === 'error') throw new Error(event.content ?? 'prompt failed');
      }
      const entries = await work.session.trace();
      const root = entries.find((entry) => entry.type === 'tool' && entry.predicate === 'run_workflow');
      expect(root?.id).toBeTruthy();
      expect(entries.some((entry) => entry.type === 'call' && entry.predicate === 'wf' && entry.scope === root?.id)).toBe(true);
    } finally {
      await work.dispose();
      await fs.rm(dir, { recursive: true, force: true });
    }
  });
});
