/**
 * Authoring fixes that make small composable skills cheaper to write:
 * `run_script/3`, dict dot-notation and CLP in `execute`, an explicit error for
 * an unknown host tool, the nested-task retry cap, JSON interpolation, and the
 * complexity advisories.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-authoring-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

async function open(options: { backend?: LLMBackend; tools?: Map<string, unknown> } = {}): Promise<HarnessRepl> {
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    ...(options.backend ? { llmBackend: options.backend } : {}),
    ...(options.tools ? { tools: options.tools as never } : {}),
    maxModelCalls: 40,
  });
  return repl;
}

async function events(replInstance: HarnessRepl, goal: string): Promise<DMLEvent[]> {
  const out: DMLEvent[] = [];
  for await (const event of replInstance.query(goal)) out.push(event);
  return out;
}

const binding = (list: DMLEvent[], name: string): string =>
  String(list.find((event) => event.type === 'solution')?.solution?.bindings.find((b) => b.name === name)?.value ?? '');
const errors = (list: DMLEvent[]): string =>
  list.filter((event) => event.type === 'error').map((event) => String(event.content ?? '')).join('\n');

/** A fake `python` host tool that records its arguments and returns fixed JSON. */
function fakePython(result: { exitCode?: number; stdout?: string; stderr?: string } = {}) {
  const calls: Array<Record<string, unknown>> = [];
  const tool = {
    description: 'Run Python.',
    parameters: {
      type: 'object',
      properties: {
        code: { type: 'string' },
        script: { type: 'string' },
        argv: { type: 'array' },
      },
    },
    async execute(args: Record<string, unknown>) {
      calls.push(args);
      return { stdout: '', stderr: '', exitCode: 0, ...result };
    },
  };
  return { tool, calls };
}

describe('run_script/3', () => {
  it('resolves script_file, stringifies numeric args, and parses JSON stdout', async () => {
    const python = fakePython({ stdout: '{"words": 3, "lines": 1}\n' });
    const r = await open({ tools: new Map([['python', python.tool]]) });
    await r.session.beginTurn();
    await r.session.assertMany('script_file(count, "scripts/count.py").');
    await r.session.endTurn();
    const out = await events(r, 'run_script(count, [42, "a b"], Value).');
    expect(python.calls[0]?.script).toBe('scripts/count.py');
    expect(python.calls[0]?.argv).toEqual(['42', 'a b']);
    expect(binding(out, 'Value')).toContain('words:3');
  });

  it('throws the script stderr when the exit code is non-zero', async () => {
    const python = fakePython({ exitCode: 2, stderr: 'boom: bad input' });
    const r = await open({ tools: new Map([['python', python.tool]]) });
    await r.session.beginTurn();
    await r.session.assertMany('script_file(bad, "scripts/bad.py").');
    await r.session.endTurn();
    const out = await events(r, 'run_script(bad, [], Value).');
    expect(errors(out)).toContain('boom: bad input');
  });

  it('explains an unlinked script name', async () => {
    const python = fakePython();
    const r = await open({ tools: new Map([['python', python.tool]]) });
    const out = await events(r, 'run_script(missing, [], Value).');
    expect(errors(out)).toContain('no script_file');
  });
});

describe('harness loading', () => {
  it('loads an empty or comment-only harness without a lone full stop', async () => {
    const r = await open();
    expect((await r.session.loadCode('')).error).toBeUndefined();
    expect((await r.session.loadCode('% DML harness — exported\n')).error).toBeUndefined();
    const out = await events(r, 'X is 6*7, output(X).');
    expect(binding(out, 'X')).toBe('42');
  });
});

describe('execute goals', () => {
  it('expands Dict.key instead of returning the term', async () => {
    const r = await open();
    const out = await events(r, 'D = _{a: 1}, X = D.a.');
    expect(binding(out, 'X')).toBe('1');
  });

  it('accepts CLP(FD) operators', async () => {
    const r = await open();
    const out = await events(r, 'X #> 3, X #< 6, label([X]).');
    expect(binding(out, 'X')).toBe('4');
    expect(errors(out)).toBe('');
  });

  it('reports an unknown host tool with its name instead of failing silently', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('run/1', 'run(R) :- exec(bash(command: "ls"), R).');
    await r.session.endTurn();
    const out = await events(r, 'run(R).');
    expect(errors(out)).toContain('bash');
  });
});

describe('host value mistakes', () => {
  it('explains output/1 on a dict instead of crashing inside media handling', async () => {
    const python = fakePython({ stdout: 'ok\n' });
    const r = await open({ tools: new Map([['python', python.tool]]) });
    await r.session.beginTurn();
    await r.session.define('show/1', 'show(R) :- exec(python(code: "print(1)"), R), output(R).');
    await r.session.endTurn();
    const out = await events(r, 'show(R).');
    expect(errors(out)).toContain('output/1 expects text or media');
    expect(errors(out)).not.toContain('sub_atom');
  });

  it('explains script_file/2 used as a goal', async () => {
    const python = fakePython();
    const r = await open({ tools: new Map([['python', python.tool]]) });
    await r.session.beginTurn();
    await r.session.define('render/1', 'render(P) :- script_file(render, P).');
    await r.session.endTurn();
    const out = await events(r, 'render(P).');
    expect(errors(out)).toContain('script_file/2 is an annotation');
    expect(errors(out)).toContain('run_script');
  });
});

describe('nested task protocol', () => {
  it('stops retrying a rejected result instead of exhausting the turn budget', async () => {
    let calls = 0;
    const backend: LLMBackend = {
      async complete(): Promise<LLMBackendResponse> {
        calls += 1;
        return {
          text: '',
          finishReason: 'tool_use',
          toolCalls: [{ id: `s${calls}`, name: 'set_result', arguments: { variable: 'N', value: 'not a number' } }],
        };
      },
    };
    const r = await open({ backend });
    await r.session.beginTurn();
    await r.session.define('go/1', 'go(N) :- task("give me a number", integer(N)).');
    await r.session.endTurn();
    const out = await events(r, 'go(N).');
    expect(errors(out)).toContain('task protocol failed');
    expect(calls).toBeLessThan(10);
  });
});

describe('interpolation', () => {
  it('renders lists and dicts as JSON in a task description', async () => {
    const prompts: string[] = [];
    const backend: LLMBackend = {
      async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
        prompts.push(request.messages.map((message) => String(message.content ?? '')).join('\n'));
        return {
          text: '',
          finishReason: 'tool_use',
          toolCalls: [
            { id: 's', name: 'set_result', arguments: { variable: 'R', value: 'ok' } },
            { id: 'f', name: 'finish', arguments: { success: true } },
          ],
        };
      },
    };
    const r = await open({ backend });
    await r.session.beginTurn();
    await r.session.define(
      'render/2',
      'render(Items, Out) :- task("These items: {Items}", string(Out)).',
    );
    await r.session.endTurn();
    await events(r, 'render([_{title: "A", score: 3}, _{title: "B", score: 5}], R).');
    expect(prompts.join('\n')).toContain('"title":"A"');
    expect(prompts.join('\n')).not.toContain('_42{');
  });
});

describe('complexity advisories', () => {
  it('warns about an agent_main that returns a value instead of answering', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('agent_main/1', 'agent_main(Out) :- format(string(Out), "hi", []).');
    const committed = await r.session.endTurn();
    expect((committed.warnings ?? []).join(' ')).toContain('answer/1');
  });

  it('warns about inline Python that belongs in a script', async () => {
    const python = fakePython();
    const r = await open({ tools: new Map([['python', python.tool]]) });
    await r.session.beginTurn();
    await r.session.define('go/0', 'go :- exec(python(code: "a\\nb\\nc\\nd"), _R).');
    const committed = await r.session.endTurn();
    expect((committed.warnings ?? []).join(' ')).toContain('run_script');
  });

  it('warns about a clause that is too long to review', async () => {
    const r = await open();
    await r.session.beginTurn();
    const goals = Array.from({ length: 13 }, (_, index) => `X${index} = ${index}`).join(',\n    ');
    await r.session.define('long/0', `long :-\n    ${goals}.`);
    const committed = await r.session.endTurn();
    expect((committed.warnings ?? []).join(' ')).toContain('goals in one clause');
  });
});
