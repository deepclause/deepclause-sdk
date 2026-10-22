/**
 * M3 — prompt mode: English -> meta task -> DML -> commit -> entry run.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type {
  DMLEvent,
  LLMBackend,
  LLMBackendRequest,
  LLMBackendResponse,
  ToolDefinition,
} from '../src/types.js';

interface CannedResponse {
  text?: string;
  reasoning?: string;
  toolCalls?: Array<{ name: string; arguments: Record<string, unknown> }>;
}

class CannedBackend implements LLMBackend {
  readonly calls: LLMBackendRequest[] = [];

  constructor(private readonly script: CannedResponse[] | ((request: LLMBackendRequest, index: number) => CannedResponse)) {}

  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls.push(request);
    const index = this.calls.length - 1;
    const response = typeof this.script === 'function'
      ? this.script(request, index)
      : this.script[Math.min(index, this.script.length - 1)];
    if (response.reasoning) request.onReasoning?.(response.reasoning);
    if (response.text) request.onText?.(response.text);
    return {
      text: response.text ?? '',
      reasoning: response.reasoning,
      toolCalls: (response.toolCalls ?? []).map((call, callIndex) => ({
        id: `${index}-${callIndex}`,
        name: call.name,
        arguments: call.arguments,
      })),
    };
  }
}

function defineCall(clauses: string): { name: string; arguments: Record<string, unknown> } {
  const first = clauses
    .split(/\r?\n/)
    .map((line) => line.trim())
    .find((line) => line && !line.startsWith(':-') && !line.startsWith('%'))
    ?.replace(/\s*\.\s*$/, '')
    .replace(/\s*:-.*$/, '') ?? 'defined_predicate';
  return { name: 'define', arguments: { head: first, clauses } };
}

function finalResponse(code: string, run?: string): CannedResponse {
  return {
    text: 'Here is the harness.',
    toolCalls: [
      ...(code.trim()
        ? [
            { name: 'begin_turn', arguments: {} },
            defineCall(code),
            { name: 'end_turn', arguments: {} },
          ]
        : []),
      ...(run ? [{ name: 'execute', arguments: { goal: run } }] : []),
      { name: 'finish', arguments: { success: true } },
    ],
  };
}

let workspace: string;
let repl: HarnessRepl;
let backend: CannedBackend;

const STUB_BASH: ToolDefinition = {
  description: 'Run a shell command (stub).',
  parameters: {
    type: 'object',
    properties: { command: { type: 'string' } },
    required: ['command'],
  },
  execute: async () => ({ success: true, stdout: '', stderr: '', exitCode: 0 }),
};

async function openRepl(): Promise<HarnessRepl> {
  return createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    streaming: true,
    llmBackend: backend,
    tools: new Map([['bash', STUB_BASH]]),
  });
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

function values(events: DMLEvent[], name: string): string[] {
  return events
    .filter((event) => event.type === 'solution')
    .map((event) => event.solution?.bindings.find((binding) => binding.name === name)?.value ?? '');
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-repl-prompt-'));
});

afterEach(async () => {
  await repl?.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

const FACTORIAL = [
  'fact(0, 1).',
  'fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.',
  'demo :- fact(5, F), output(F).',
].join('\n');

describe('prompt mode', () => {
  it('commits generated DML and runs the declared entry', async () => {
    backend = new CannedBackend([finalResponse(FACTORIAL, 'demo')]);
    repl = await openRepl();

    const events = await collect(repl.prompt('add a factorial rule'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(events.filter((event) => event.type === 'output').map((event) => event.content)).toContain('120');

    const after = await collect(repl.query('fact(5, F)'));
    expect(values(after, 'F')).toEqual(['120']);
  });

  it('commits CLP(FD) source that uses a top-level use_module directive', async () => {
    // Regression: commit validation used to read the source with an empty
    // operator table, so library operators (#=/ins) were rejected even though
    // the same source loads fine.
    const code = ':- use_module(library(clpfd)).\nsolve(X) :- X #> 3, X #< 7, label([X]).';
    backend = new CannedBackend([finalResponse(code, '')]);
    repl = await openRepl();

    const events = await collect(repl.prompt('add a clpfd solver'));
    expect(events.some((event) => event.type === 'error')).toBe(false);

    const solutions = await collect(repl.query('solve(X)'));
    expect(values(solutions, 'X')).toEqual(['4', '5', '6']);

    // The directive is recorded so an exported harness reloads with clpfd.
    const exported = await repl.export('harness.dml');
    const exportedPath = path.isAbsolute(exported.path) ? exported.path : path.join(workspace, exported.path);
    expect(await fs.readFile(exportedPath, 'utf8')).toContain('use_module(library(clpfd))');
  });

  it('shows task/N and prompt/N in listing/export (no task_named/typed_var leak)', async () => {
    backend = new CannedBackend([{ text: '' }]);
    repl = await openRepl();
    const loaded = await repl.session.loadCode([
      'research(Question, Answer) :- task("Answer: {Question}", Answer).',
      'analyse(Topic, Report, Count) :-',
      '    prompt("Summarise: {Topic}", Summary),',
      '    integer(Count),',
      '    task("Expand {Summary}", Report).',
    ].join('\n'));
    expect(loaded.error).toBeUndefined();

    // The load-time rewrite is reversed for anything the meta task reads.
    const listing = await repl.list('all');
    expect(listing).not.toContain('task_named');
    expect(listing).not.toContain('prompt_named');
    expect(listing).not.toContain('typed_var');
    // Listing now prints the block as written, so the author's names survive.
    expect(listing).toContain('task("Answer: {Question}"');
    expect(listing).toContain('prompt("Summarise: {Topic}"');
    expect(listing).toContain('integer(Count)');

    const exported = await repl.export('clauses.dml');
    const exportedPath = path.isAbsolute(exported.path) ? exported.path : path.join(workspace, exported.path);
    const text = await fs.readFile(exportedPath, 'utf8');
    expect(text).not.toContain('task_named');
    expect(text).not.toContain('prompt_named');
    expect(text).not.toContain('typed_var');
    expect(text).toContain('task("Answer: {A}"');
  });

  it('does not synthesize agent_main', async () => {
    backend = new CannedBackend([finalResponse(FACTORIAL, 'demo')]);
    repl = await openRepl();
    await collect(repl.prompt('add a factorial rule'));

    const listing = await repl.list('all');
    expect(listing).not.toContain('agent_main');
    expect(listing).toContain('demo/0');
  });

  

  

  

  it('materializes a script file via a DML goal (execute)', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        // The authoring agent has no file tool; it writes files through DML.
        // DML file I/O reads the workspace, so the write is immediate.
        { name: 'execute', arguments: { goal: 'make_directory_path("scripts"), write_file("scripts/hello.txt", "hi-from-meta"), output("written")' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    const events = await collect(repl.prompt('write a script file'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(await fs.readFile(path.join(workspace, 'scripts/hello.txt'), 'utf8')).toBe('hi-from-meta');
  });

  it('rejects DML file writes that escape the workspace', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'execute', arguments: { goal: 'write_file("../escape.txt", "nope")' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    await collect(repl.prompt('try to escape'));
    const escaped = path.join(path.dirname(workspace), 'escape.txt');
    expect(await fs.access(escaped).then(() => true).catch(() => false)).toBe(false);
  });

  it('emits model_call started/completed around the meta model call', async () => {
    backend = new CannedBackend([finalResponse('demo :- output(ok).', '')]);
    repl = await openRepl();

    const events = await collect(repl.prompt('make a demo'));
    const phases = events
      .filter((event) => event.type === 'model_call')
      .map((event) => event.modelCallState);
    expect(phases).toContain('started');
    expect(phases).toContain('completed');
    expect(phases).not.toContain('failed');
  });

  it('define replaces a predicate instead of duplicating its clauses', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'begin_turn', arguments: {} },
        { name: 'define', arguments: { head: 'greet(Name, Msg)', clauses: 'greet(Name, Msg) :- format(atom(Msg), "hi ~w", [Name]).' } },
        { name: 'define', arguments: { head: 'greet(Name, Msg)', clauses: 'greet(Name, Msg) :- format(atom(Msg), "yo ~w", [Name]).' } },
        { name: 'execute', arguments: { goal: 'greet(bob, M), output(M)' } },
        { name: 'end_turn', arguments: {} },
        { name: 'set_result', arguments: { variable: 'DMLCode', value: 'greet(Name, Msg) :- format(atom(Msg), "yo ~w", [Name]).' } },
        { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    const events = await collect(repl.prompt('define greet'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    const outputs = events.filter((event) => event.type === 'output').map((event) => event.content);
    // The second define replaced the first, so only the new clause runs once.
    expect(outputs.filter((text) => text === 'yo bob')).toHaveLength(1);
    expect(outputs).not.toContain('hi bob');
  });

  it('runs a skill that wraps task/2 in once/1 (the MI intercepts once)', async () => {
    // Regression: `once/1` was a raw builtin, so a wrapped task/prompt/exec ran
    // in plain Prolog and failed with `task_named/3` undefined.
    backend = new CannedBackend([
      {
        text: 'ok',
        toolCalls: [
          { name: 'begin_turn', arguments: {} },
          {
            name: 'define',
            arguments: {
              head: 'research(Question, Answer)',
              clauses: 'research(Question, Answer) :- once(task("Answer this: {Question}", Answer)).',
            },
          },
          { name: 'execute', arguments: { goal: 'research("capital of Australia?", A), output(A)' } },
          { name: 'end_turn', arguments: {} },
          {
            name: 'set_result',
            arguments: {
              variable: 'DMLCode',
              value: 'research(Question, Answer) :- once(task("Answer this: {Question}", Answer)).',
            },
          },
          { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
          { name: 'finish', arguments: { success: true } },
        ],
      },
      {
        text: '',
        toolCalls: [
          { name: 'set_result', arguments: { variable: 'Answer', value: 'Canberra' } },
          { name: 'finish', arguments: { success: true } },
        ],
      },
    ]);
    repl = await openRepl();

    const events = await collect(repl.prompt('add a research skill'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    const outputs = events.filter((event) => event.type === 'output').map((event) => event.content);
    expect(outputs).toContain('Canberra');
  });

  

  

  it('rewrites task/N inside scratch-asserted clauses (same as loaded code)', async () => {
    backend = new CannedBackend([
      {
        text: '',
        toolCalls: [
          { name: 'begin_turn', arguments: {} },
          defineCall('foo :- task("hi", X), output(X).\n'),
          { name: 'execute', arguments: { goal: 'foo' } },
          { name: 'end_turn', arguments: {} },
          { name: 'set_result', arguments: { variable: 'DMLCode', value: 'kept(1).' } },
          { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
          { name: 'finish', arguments: { success: true } },
        ],
      },
      {
        text: '',
        toolCalls: [
          { name: 'set_result', arguments: { variable: 'X', value: 'nested-ok' } },
          { name: 'finish', arguments: { success: true } },
        ],
      },
    ]);
    repl = await openRepl();

    const events = await collect(repl.prompt('assert a clause that calls task'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    // The nested task ran through the scratch-asserted clause and produced its output.
    expect(events.some((event) => event.type === 'output' && event.content === 'nested-ok')).toBe(true);
  });

  it('fails loudly on an unrewritten task/N at the runtime', async () => {
    const { loadProlog } = await import('../src/prolog/loader.js');
    const swipl = await loadProlog();
    const result = swipl.prolog.query(
      `catch(deepclause_mi:mi_call(task("x", _), state{memory: [], params: params{}, context_stack: [], depth: 0}, _), E, true), term_string(E, S)`,
    ).once() as Record<string, unknown> | null;
    const term = result?.S as { v?: string } | string | undefined;
    expect(String(typeof term === 'object' ? term?.v : term)).toContain('unrewritten_task_form');
  });

  it('define accepts blocks that end with a newline', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'begin_turn', arguments: {} },
        defineCall('a(1).\nb(2).\nc(3) :- true.\n'),
        { name: 'execute', arguments: { goal: 'a(X), b(Y), c(Z)' } },
        { name: 'end_turn', arguments: {} },
        { name: 'set_result', arguments: { variable: 'DMLCode', value: 'kept(1).' } },
        { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    const events = await collect(repl.prompt('assert a multi-clause block'));
    expect(events.some((event) => (
      event.type === 'tool_call' && event.toolName === 'define' && event.toolState === 'completed'
    ))).toBe(true);
    expect(events.some((event) => (
      event.type === 'tool_call' && event.toolName === 'define' && event.toolState === 'failed'
    ))).toBe(false);
  });

  it('keeps the turn alive when an execute child goal fails', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'execute', arguments: { goal: 'no_such_predicate(1)' } },
        { name: 'begin_turn', arguments: {} },
        defineCall('kept(1).'),
        { name: 'end_turn', arguments: {} },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    const events = await collect(repl.prompt('explore with a failing execute'));
    // The nested error is surfaced for rendering, but the turn still commits.
    expect(events.some((event) => event.type === 'error')).toBe(true);
    expect(values(await collect(repl.query('kept(X)')), 'X')).toEqual(['1']);
  });

  it('define keeps clause order so recursion terminates', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'begin_turn', arguments: {} },
        defineCall('fact(0, 1).\nfact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.'),
        { name: 'execute', arguments: { goal: 'fact(5, F)' } },
        { name: 'end_turn', arguments: {} },
        { name: 'set_result', arguments: { variable: 'DMLCode', value: 'kept(1).' } },
        { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    const events = await collect(repl.prompt('define fact with define'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(events.some((event) => (
      event.type === 'tool_call'
      && event.toolName === 'execute'
      && event.toolState === 'completed'
      && event.toolSummary?.includes('solution')
    ))).toBe(true);
  });

  it('answers a simple question directly without changing the harness', async () => {
    backend = new CannedBackend([{ text: 'We added a factorial rule.' }]);
    repl = await openRepl();
    await repl.session.loadCode('existing(1).\n');
    const before = await repl.list('all');

    const events = await collect(repl.prompt('what did we just do?'));
    expect(events.some((event) => event.type === 'stream' && event.content === 'We added a factorial rule.')).toBe(true);
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(await repl.list('all')).toBe(before);
  });

  

  it('replaces by predicate on redefinition', async () => {
    const first = [
      'fact(0, 1).',
      'fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.',
    ].join('\n');
    const second = 'fact(0, 1).';
    backend = new CannedBackend([
      finalResponse(first),
      {
        text: '',
        toolCalls: [
          { name: 'begin_turn', arguments: {} },
          { name: 'define', arguments: { head: 'fact(N, F)', clauses: second } },
          { name: 'end_turn', arguments: {} },
          { name: 'finish', arguments: { success: true } },
        ],
      },
    ]);
    repl = await openRepl();

    await collect(repl.prompt('primeira'));
    await collect(repl.prompt('segunda'));

    const clauses = await collect(repl.query('findall(N, fact(N, _), L)'));
    // Exactly one clause remains, so the clause list has one element.
    expect(clauses.filter((event) => event.type === 'solution').length).toBe(1);
    const listing = await repl.list('fact/2');
    expect(listing.match(/fact\(/g)?.length).toBe(1);
  });

  it('removes a predicate via repl_delete', async () => {
    backend = new CannedBackend([
      finalResponse('temp(1).\nkeep(2).'),
      finalResponse('repl_delete(temp/1).'),
    ]);
    repl = await openRepl();

    await collect(repl.prompt('add temp and keep'));
    expect(values(await collect(repl.query('temp(X)')), 'X')).toEqual(['1']);

    await collect(repl.prompt('remove temp'));
    const removed = await collect(repl.query('temp(X)'));
    expect(removed.some((event) => event.type === 'solution')).toBe(false);
    expect(values(await collect(repl.query('keep(X)')), 'X')).toEqual(['2']);
  });

  

  it('leaves the harness untouched when the edit is invalid', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        defineCall('fact(0, 1) :- .'),
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();
    await repl.session.loadCode('existing(1).\n');
    const before = await repl.list('all');

    const events = await collect(repl.prompt('write broken code'));
    expect(events.some((event) => event.type === 'tool_call' && event.toolName === 'define' && event.toolState === 'failed')).toBe(true);
    expect(await repl.list('all')).toBe(before);
    expect(values(await collect(repl.query('existing(X)')), 'X')).toEqual(['1']);
  });

  

  it('replaces clauses through the agent define tool', async () => {
    backend = new CannedBackend([
      {
        text: '',
        toolCalls: [
          { name: 'begin_turn', arguments: {} },
          defineCall('foo(1).\nbar(1).\n'),
          { name: 'execute', arguments: { goal: 'foo(X)' } },
          { name: 'execute', arguments: { goal: 'bar(1)' } },
          { name: 'end_turn', arguments: {} },
          { name: 'finish', arguments: { success: true } },
        ],
      },
    ]);
    repl = await openRepl();

    const events = await collect(repl.prompt('replace foo clauses'));
    expect(events.some((event) => event.type === 'error')).toBe(false);

    expect(events.some((event) => event.type === 'tool_call' && event.toolName === 'define' && event.toolState === 'completed')).toBe(true);

    const executeSolutions = (goal: string) => {
      const event = events.find(
        (candidate) => candidate.type === 'tool_call'
          && candidate.toolName === 'execute'
          && candidate.toolState === 'completed'
          && (candidate.toolArgs as Record<string, unknown> | undefined)?.goal === goal,
      );
      return (JSON.parse(event?.toolSummary ?? '{}') as { solutions?: unknown[] }).solutions?.length ?? -1;
    };
    expect(executeSolutions('foo(X)')).toBe(1);
    expect(executeSolutions('bar(1)')).toBe(1);
  });
});
