/**
 * Typed outputs of `task/N` and `prompt/N`.
 *
 * The nested agent sets outputs with `set_result`. It must be told the variable
 * name *and* the shape (a declared `schema/2` is otherwise just "object"), and a
 * call that omits the name should not abort a one-output task.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent, LLMBackend, LLMBackendResponse } from '../src/types.js';

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-typed-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

function finishing(variable: string | undefined, value: unknown): LLMBackendResponse {
  const setArgs: Record<string, unknown> = { value };
  if (variable !== undefined) setArgs.variable = variable;
  return {
    text: '',
    finishReason: 'tool_use',
    toolCalls: [
      { id: 's', name: 'set_result', arguments: setArgs },
      { id: 'f', name: 'finish', arguments: { success: true } },
    ],
  };
}

async function runWith(
  code: string,
  reply: (description: string) => LLMBackendResponse,
  seen: { description?: string } = {},
): Promise<DMLEvent[]> {
  const backend: LLMBackend = {
    async complete(request) {
      if ((request.tools ?? []).some((tool) => tool.name === 'begin_turn')) {
        return { text: 'done', finishReason: 'stop' };
      }
      const setResult = request.tools?.find((tool) => tool.name === 'set_result');
      seen.description = setResult?.description;
      return reply(setResult?.description ?? '');
    },
  };
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    llmBackend: backend,
    maxModelCalls: 20,
  });
  await repl.session.loadCode(code);
  const events: DMLEvent[] = [];
  for await (const event of repl.query('go(X).')) events.push(event);
  return events;
}

const binding = (events: DMLEvent[], name: string): string =>
  String(events.find((event) => event.type === 'solution')?.solution?.bindings.find((b) => b.name === name)?.value ?? '');

describe('typed outputs', () => {
  it('describes a declared schema and its fields to the nested agent', async () => {
    const seen: { description?: string } = {};
    await runWith(
      'schema(person, [field(name, string, required), field(age, integer, default(0))]).\n'
      + 'go(X) :- task("t", schema(person, X)).',
      () => finishing('X', { name: 'Ada' }),
      seen,
    );
    expect(seen.description).toContain('X: object');
    expect(seen.description).toContain('name (string, required)');
    expect(seen.description).toContain('age (integer, default 0)');
  });

  it('describes a list of schemas as array<object> with the fields', async () => {
    const seen: { description?: string } = {};
    await runWith(
      'schema(person, [field(name, string, required)]).\n'
      + 'go(X) :- task("t", list(schema(person, X))).',
      () => finishing('X', [{ name: 'Ada' }]),
      seen,
    );
    expect(seen.description).toContain('X: array<object>');
    expect(seen.description).toContain('name (string, required)');
  });

  it('infers the only output when the model omits "variable"', async () => {
    const events = await runWith('go(X) :- task("t", string(X)).', () => finishing(undefined, 'hello'));
    expect(binding(events, 'X')).toContain('hello');
    expect(events.some((event) => event.type === 'error')).toBe(false);
  });

  const NESTED_SCHEMAS = [
    'schema(note, [field(pitch, string, required), field(beats, number, required)]).',
    'schema(part, [field(name, string, required), field(notes, list(schema(note, _)), required)]).',
    'schema(score, [field(title, string, required), field(parts, list(schema(part, _)), required), field(bpm, integer, default(90))]).',
  ].join('\n');

  it('describes nested schema fields to the nested agent', async () => {
    const seen: { description?: string } = {};
    await runWith(
      `${NESTED_SCHEMAS}\ngo(X) :- task("t", schema(score, X)).`,
      () => finishing('X', { title: 'T', parts: [{ name: 'Lead', notes: [{ pitch: 'C', beats: 1 }] }] }),
      seen,
    );
    expect(seen.description).toContain('parts (array<object>, required)');
    expect(seen.description).toContain('notes (array<object>, required)');
    expect(seen.description).toContain('pitch (string, required)');
  });

  it('validates nested schema fields and fills nested defaults', async () => {
    const events = await runWith(
      `${NESTED_SCHEMAS}\ngo(X) :- task("t", schema(score, X)).`,
      () => finishing('X', { title: 'T', parts: [{ name: 'Lead', notes: [{ pitch: 'C', beats: 1 }] }] }),
    );
    expect(binding(events, 'X')).toContain('bpm:90');
    expect(events.some((event) => event.type === 'error')).toBe(false);
  });

  it('rejects an invalid value inside a nested schema', async () => {
    const events = await runWith(
      `${NESTED_SCHEMAS}\ngo(X) :- task("t", schema(score, X)).`,
      () => finishing('X', { title: 'T', parts: [{ name: 'Lead', notes: [{ pitch: 'C', beats: 'one' }] }] }),
    );
    const error = events.filter((event) => event.type === 'error').map((event) => String(event.content)).join(' ');
    expect(error).toContain('parts[0].notes[0].beats');
  });

  it('keeps schema(name, X) in listing and export', async () => {
    const backend: LLMBackend = {
      async complete(request) {
        if ((request.tools ?? []).some((tool) => tool.name === 'begin_turn')) {
          return { text: 'done', finishReason: 'stop' };
        }
        return finishing('Score', { name: 'Ada' })(request);
      },
    };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
      maxModelCalls: 10,
    });
    await repl.session.beginTurn();
    await repl.session.define(
      'go/1',
      'schema(person, [field(name, string, required)]).\ngo(Score) :- task("t", schema(person, Score)).',
    );
    await repl.session.endTurn();
    const listing = await repl.session.listing('go/1');
    expect(listing).toContain('schema(person');
    expect(listing).not.toContain('object(');
    const source = await repl.session.harnessSource();
    expect(source).toContain('schema(person');
  });

  it('reports a set_result with no arguments as a missing value', async () => {
    const events = await runWith('go(X) :- task("t", string(X)).', () => ({
      text: '',
      finishReason: 'tool_use',
      toolCalls: [
        { id: 's', name: 'set_result', arguments: {} },
        { id: 'f', name: 'finish', arguments: { success: true } },
      ],
    }));
    const error = events.filter((event) => event.type === 'error').map((event) => String(event.content)).join(' ');
    expect(error).toContain('received no');
    expect(error).toContain('too large');
  });

  it('names the expected outputs when the variable is unknown', async () => {
    const events = await runWith('go(X) :- task("t", string(X)).', () => finishing('Wrong', 'hello'));
    const error = events.filter((event) => event.type === 'error').map((event) => String(event.content)).join(' ');
    expect(error).toContain('Unknown result variable');
    expect(error).toContain('expected one of: X');
    expect(error).toContain('Expected outputs: X');
  });
});
