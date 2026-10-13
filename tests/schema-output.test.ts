/**
 * `schema/2`: declare the shape of a model's structured output, and have it
 * applied.
 *
 * `list(object(B))` says "a list of dicts" but not what is in them, so every
 * harness re-derives defaults and tolerates missing fields by hand. A schema
 * declares once: absent fields with a default are filled, unknown fields are kept
 * (so a model mistake stays visible), and a missing `required` or a field of the
 * wrong type **fails** rather than yielding an empty result.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { plainValue } from '../src/repl/frames.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

/** Answers a `task/N` with a `set_result` carrying the given value. */
class Scripted implements LLMBackend {
  constructor(private readonly value: unknown) {}
  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    const text = request.messages.map((m) => m.content).join('\n');
    // A task ends only when the agent calls finish; returning text just makes the
    // host nudge it to continue.
    if (text.includes('"success":true')) {
      return {
        text: '',
        toolCalls: [{ id: 'f', name: 'finish', arguments: { success: true } }],
        finishReason: 'tool_use',
      };
    }
    return {
      text: '',
      toolCalls: [{ id: 'r', name: 'set_result', arguments: { variable: 'Authors', value: this.value } }],
      finishReason: 'tool_use',
    };
  }
}

let workspace: string;
let repl: HarnessRepl;

const SCHEMA = [
  'schema(author, [',
  '    field(name,        string, required),',
  '    field(affiliation, string, default("not stated")),',
  '    field(email,       string, default("")),',
  '    field(country,     string, default(""))',
  ']).',
].join('\n');

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-schema-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

function host(goal: string): Record<string, unknown> {
  const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  return q.call(repl.session, goal) ?? {};
}

/** Unwrap Prolog bridge wrappers all the way down (dict values are wrapped too). */
function deep(value: unknown): unknown {
  const unwrapped = plainValue(value);
  if (Array.isArray(unwrapped)) return unwrapped.map(deep);
  if (unwrapped && typeof unwrapped === 'object') {
    const out: Record<string, unknown> = {};
    for (const [key, item] of Object.entries(unwrapped as Record<string, unknown>)) {
      out[key] = deep(item);
    }
    return out;
  }
  return unwrapped;
}

function val(goal: string, name: string): unknown {
  const raw = host(goal)[name];
  const unwrapped = deep(raw);
  return unwrapped === undefined ? raw : unwrapped;
}

describe('schema/2', () => {
  beforeEach(async () => {
    await repl.session.loadCode(SCHEMA);
  });

  it('fills declared defaults for absent fields', () => {
    const r = val('schema_fill(author, x{name:"Ada"}, F)', 'F') as Record<string, unknown>;
    expect(r).toMatchObject({ name: 'Ada', affiliation: 'not stated', email: '', country: '' });
  });

  it('keeps a value that is present', () => {
    const r = val('schema_fill(author, x{name:"Ada",country:"UK"}, F)', 'F') as Record<string, unknown>;
    expect(r).toMatchObject({ name: 'Ada', country: 'UK' });
  });

  it('keeps an undeclared field so a model mistake stays visible', () => {
    const r = val('schema_fill(author, x{name:"Ada",nikname:"A"}, F)', 'F') as Record<string, unknown>;
    expect(r).toMatchObject({ name: 'Ada', nikname: 'A' });
  });

  it('fails when a required field is missing, rather than yielding empty', () => {
    const r = host('catch(schema_fill(author, x{affiliation:"MIT"}, F), error(schema_error(M), _), F = M)');
    expect(String(r.F)).toContain('required field "name" is missing');
  });

  it('fails when a field has the wrong type', () => {
    const r = host('catch(schema_fill(author, x{name:42}, F), error(schema_error(M), _), F = M)');
    expect(String(r.F)).toContain('not string');
  });

  it('reports a schema that was never declared', () => {
    const r = host('catch(schema_fill(missing_schema, x{}, F), error(schema_error(M), _), F = M)');
    expect(String(r.F)).toContain('not declared');
  });

  it('normalises every record of a list', () => {
    const list = val(
      'findall(F, (member(N, ["A","B"]), schema_fill(author, x{name:N}, F)), L)',
      'L',
    ) as Array<Record<string, unknown>>;
    expect(list.map((record) => record.name)).toEqual(['A', 'B']);
    expect(list.map((record) => record.affiliation)).toEqual(['not stated', 'not stated']);
  });

describe('schema applied to a real task result', () => {
  it('fills defaults on what the model returned', async () => {
    const backend = new Scripted([{ name: 'Ada' }, { name: 'Bob', affiliation: 'MIT' }]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'm',
      provider: 'openai',
      llmBackend: backend,
      maxModelCalls: 4,
    });
    await repl.session.loadCode([
      SCHEMA,
      'extract(Text, Authors) :-',
      '    task("Extract every author from: {Text}", list(schema(author, Authors))).',
    ].join('\n'));

    const events: Array<{ type: string; solution?: { bindings: Array<{ name: string; value: unknown }> } }> = [];
    for await (const e of repl.query('extract("some text", Authors)')) events.push(e as never);
    const bound = String(
      events.find((e) => e.type === 'solution')
        ?.solution?.bindings.find((b) => b.name === 'Authors')?.value ?? '',
    );

    // A solution binding arrives as Prolog's rendered text, so assert on that.
    // The point is that the *task result* was normalised: `Ada` had no
    // affiliation and gets the declared default, `Bob` keeps the one supplied.
    expect(bound).toContain('name:"Ada"');
    expect(bound).toContain('name:"Bob"');
    expect(bound).toContain('affiliation:"not stated"');
    expect(bound).toContain('affiliation:"MIT"');
    expect(bound).toContain('country:""');
  });
});
});

describe('schema fields reach the agent-prompt builder', () => {
  beforeEach(async () => {
    await repl.session.loadCode(SCHEMA);
  });

  it('attaches the declared fields to an output descriptor', () => {
    const sid = (repl.session as unknown as { sessionId: string }).sessionId;
    const rendered = String(
      val(
        `deepclause_mi:resolve_schema_fields('${sid}', `
        + '[typed_var{name:"Authors", type:object, schema:author}], R), '
        + 'format(string(S), "~w", [R])',
        'S',
      ),
    );
    // The descriptor now carries the field list, which is what the agent-prompt
    // builder turns into the `set_result` description.
    expect(rendered).toContain('name:name');
    expect(rendered).toContain('required:1');
    expect(rendered).toContain('affiliation');
    expect(rendered).toContain('not stated');
  });

  it('leaves a descriptor without a schema untouched', () => {
    const sid = (repl.session as unknown as { sessionId: string }).sessionId;
    const rendered = String(
      val(
        `deepclause_mi:resolve_schema_fields('${sid}', `
        + '[typed_var{name:"Text", type:string}], R), format(string(S), "~w", [R])',
        'S',
      ),
    );
    expect(rendered).toContain('name:Text');
    expect(rendered).not.toContain('fields');
  });
});
