/**
 * Tool annotations and keyed annotations: the lifecycle the authoring tools own.
 *
 * A tool is a capability the nested agent can call. Its bookkeeping (schema,
 * source, implementation) lives outside the clause database, and the edit tools
 * used to handle it inconsistently: `listing` hid it, redefining the predicate
 * dropped it, redefining the tool left the old body live, duplicates were
 * accepted, and the only warning was printed to stderr where the agent never
 * saw it. These tests pin the behaviour the prompt now teaches.
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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-tool-lifecycle-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

function resultAndFinish(variable: string, value: unknown): LLMBackendResponse {
  return {
    text: '',
    finishReason: 'tool_use',
    toolCalls: [
      { id: `set-${variable}`, name: 'set_result', arguments: { variable, value } },
      { id: 'finish', name: 'finish', arguments: { success: true } },
    ],
  };
}

/** A backend that calls `toolName` once with `arg1`, then returns `value`. */
function callingBackend(toolName: string, value: unknown, seen: { offered?: string[]; schema?: unknown }): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      const tools = request.tools ?? [];
      if (tools.some((tool) => tool.name === 'begin_turn')) return { text: 'done', finishReason: 'stop' };
      seen.offered = tools.map((tool) => tool.name).filter((name) => !['finish', 'set_result'].includes(name));
      seen.schema = tools.find((tool) => tool.name === toolName);
      const toolMessages = request.messages.filter((message) => message.role === 'tool');
      if (toolMessages.length === 0) {
        return { text: '', finishReason: 'tool_use', toolCalls: [{ id: 'c1', name: toolName, arguments: { arg1: 'in' } }] };
      }
      return resultAndFinish('R', value);
    },
  };
}

async function open(backend?: LLMBackend): Promise<HarnessRepl> {
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    ...(backend ? { llmBackend: backend } : {}),
    maxModelCalls: 20,
  });
  return repl;
}

async function run(replInstance: HarnessRepl, goal: string): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of replInstance.query(goal)) events.push(event);
  return events;
}

const binding = (events: DMLEvent[], name: string): string => {
  const value = events.find((event) => event.type === 'solution')?.solution?.bindings.find((b) => b.name === name)?.value;
  return String(value ?? '');
};

describe('tool annotation lifecycle', () => {
  it('refuses a second registration of the same tool name', async () => {
    const r = await open();
    await r.session.beginTurn();
    expect((await r.session.assertClause('tool(up(A, B), "One.") :- string_upper(A, B)')).ok).toBe(true);
    const second = await r.session.assertClause('tool(up(A, B), "Two.") :- string_lower(A, B)');
    expect(second.ok).toBe(false);
    expect(second.error).toContain('already registered');
  });

  it('keeps the annotation when the predicate is redefined without its tool clause', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('up/2', 'up(A, B) :- string_upper(A, B).\ntool(up(A, B), "Uppercase.") :- up(A, B).');
    await r.session.define('up/2', 'up(A, B) :- string_concat(A, "-v2", B).');
    await r.session.endTurn();
    // The annotation still exists and delegates to the *new* predicate body.
    expect(await r.session.listing('tools')).toContain('up/2');
    const events = await run(r, 'tool(up("in", Y)).');
    expect(binding(events, 'Y')).toBe('"in-v2"');
  });

  it('replaces the implementation, leaving one live body (no stale orphan)', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('up/2', 'up(A, B) :- string_upper(A, B).\ntool(up(A, B), "One.") :- up(A, B).');
    await r.session.define('up/2', 'up(A, B) :- string_concat(A, "?", B).\ntool(up(A, B), "Two.") :- string_concat(A, "-v2", B).');
    await r.session.endTurn();
    // Exactly one implementation clause, and it is the new one.
    const clauses = await run(r, 'findall(B, clause(tool(up(_, _)), B), Bs).');
    expect(binding(clauses, 'Bs')).toContain('-v2');
    expect(binding(clauses, 'Bs').match(/string_concat/g)?.length).toBe(1);
    const direct = await run(r, 'tool(up("x", Y)).');
    expect(binding(direct, 'Y')).toBe('"x-v2"');
    const delegate = await run(r, 'up("x", Y).');
    expect(binding(delegate, 'Y')).toBe('"x?"');
  });

  it('refuses a tool written as a fact', async () => {
    const r = await open();
    await r.session.beginTurn();
    const result = await r.session.define('shout/2', 'shout(A, B) :- string_upper(A, B).\ntool(shout(A, B), "Shout.").');
    expect(result.ok).toBe(false);
    expect(result.error).toContain('a tool needs a body');
  });

  it('refuses define with the tool annotation as the target', async () => {
    const r = await open();
    await r.session.beginTurn();
    const result = await r.session.define('tool/2', 'tool(up(A, B), "Up.") :- up(A, B).');
    expect(result.ok).toBe(false);
    expect(result.error).toContain('define the predicate the tool delegates to');
  });

  it('retracts an annotation by pattern, leaving the delegate in place', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('cnt/2', 'cnt(A, B) :- string_length(A, B).\ntool(cnt(A, B), "Length.") :- cnt(A, B).');
    const removed = await r.session.retractClause('tool(cnt(_, _), _)');
    expect(removed.ok).toBe(true);
    const tools = await r.session.listing('tools');
    expect(tools).not.toContain('cnt/2');
    const kept = await run(r, 'cnt("abc", N).');
    expect(binding(kept, 'N')).toBe('3');
  });

  it('deletes one tool with delete("tool:name/arity") and keeps the delegate', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('cnt/2', 'cnt(A, B) :- string_length(A, B).\ntool(cnt(A, B), "Length.") :- cnt(A, B).');
    expect((await r.session.deletePredicate('tool:cnt/2')).ok).toBe(true);
    expect(await r.session.listing('tools')).not.toContain('cnt/2');
    const kept = await run(r, 'cnt("abc", N).');
    expect(binding(kept, 'N')).toBe('3');
  });

  it('removes the annotation and its implementation when the predicate is deleted', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('cnt/2', 'cnt(A, B) :- string_length(A, B).\ntool(cnt(A, B), "Length.") :- cnt(A, B).');
    await r.session.deletePredicate('cnt/2');
    expect(await r.session.listing('tools')).not.toContain('cnt/2');
    const clauses = await run(r, 'findall(H, clause(tool(H), _), Hs).');
    expect(binding(clauses, 'Hs')).not.toContain('cnt(');
  });

  it('drops a tool whose name was redefined as an unrelated predicate', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('search/2', 'search(Q, R) :- format(string(R), "results ~w", [Q]).\ntool(search(Q, R), "Search.") :- search(Q, R).');
    await r.session.deletePredicate('search/2');
    await r.session.define('search/1', 'search(x).');
    expect(await r.session.listing('tools')).not.toContain('search/2');
  });
});

describe('tool visibility and schema', () => {
  it('shows the tool annotation under its predicate and summarises tools', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('up/2', 'up(A, B) :- string_upper(A, B).\ntool(up(A, B), "Uppercase.", [string]) :- up(A, B).');
    const listing = await r.session.listing('all');
    expect(listing).toContain('up/2');
    expect(listing).toContain('% tool up(_');
    expect(listing).toContain('tools: [up/2]');
  });

  it('lists user tools without the built-in form tools, and all with tools:all', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('up/2', 'up(A, B) :- string_upper(A, B).\ntool(up(A, B), "Uppercase.") :- up(A, B).');
    const userTools = await r.session.listing('tools');
    expect(userTools).toContain('up/2');
    expect(userTools).not.toContain('render_form');
    expect(await r.session.listing('tools:all')).toContain('render_form');
  });

  it('names arguments after the head variables in the schema description', async () => {
    const seen: { schema?: unknown } = {};
    const r = await open(callingBackend('inspect', 'ok', seen));
    await r.session.beginTurn();
    await r.session.define(
      'inspect/2',
      'inspect(Spec, Name) :- get_dict(name, Spec, Name).\ntool(inspect(Spec, Name), "Read a name.", [object]) :- inspect(Spec, Name).',
    );
    await r.session.endTurn();
    await run(r, 'with_tools([inspect], task("read", string(R))).');
    expect(JSON.stringify(seen.schema)).toContain('"Spec"');
    expect(JSON.stringify(seen.schema)).toContain('Returns Name');
  });

  it('reports a type list that does not match the inputs at end_turn', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('wr/2', 'wr(A, B) :- write_file(A, B).\ntool(wr(A, B), "Write.", [string, string]) :- wr(A, B).');
    const committed = await r.session.endTurn();
    expect((committed.warnings ?? []).join(' ')).toContain('input argument');
  });
});

describe('keyed annotation facts', () => {
  it('keeps other predicates documented when one doc is defined', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('a/1', 'doc(a/1, "A doc.").\na(1).');
    await r.session.define('b/1', 'doc(b/1, "B doc.").\nb(1).');
    const docs = await run(r, 'findall(P, doc(P, _), Ps).');
    expect(binding(docs, 'Ps')).toContain('a/1');
    expect(binding(docs, 'Ps')).toContain('b/1');
  });

  it('merges a doc/2 block by key instead of replacing every doc', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.assertMany('doc(a/1, "A doc.").\ndoc(b/1, "B doc.").');
    await r.session.endTurn();
    await r.session.beginTurn();
    // The old behaviour removed every doc/2 clause and re-added only this one.
    await r.session.define('c/1', 'doc(c/1, "C doc.").\nc(1).');
    await r.session.endTurn();
    const docs = await run(r, 'findall(P, doc(P, _), Ps).');
    for (const key of ['a/1', 'b/1', 'c/1']) expect(binding(docs, 'Ps')).toContain(key);
  });

  it('hides annotation predicates from listing and refuses deleting them whole', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define(
      'a/1',
      'doc(a/1, "A doc.").\nschema(thing, [field(name, string, required)]).\nscript_file(a_script, "scripts/a.py").\na(1).',
    );
    const listing = await r.session.listing('all');
    expect(listing).toContain('a/1');
    expect(listing).not.toContain('doc/2');
    expect(listing).not.toContain('schema/2');
    expect(listing).not.toContain('script_file/2');
    const scripts = await r.session.listing('scripts');
    expect(scripts).toContain('a_script -> scripts/a.py');
    const deleted = await r.session.deletePredicate('doc/2');
    expect(deleted.ok).toBe(false);
    expect(deleted.error).toContain('keyed annotations');
  });

  it('refuses delete("tool/2") and points at the tool form', async () => {
    const r = await open();
    await r.session.beginTurn();
    const result = await r.session.deletePredicate('tool/2');
    expect(result.ok).toBe(false);
    expect(result.error).toContain('tool:Name/Arity');
  });

  it('keeps other script links when one is replaced', async () => {
    const r = await open();
    await r.session.beginTurn();
    await r.session.define('x/0', 'script_file(one, "scripts/one.py").\nscript_file(two, "scripts/two.py").\nx.' );
    const links = await run(r, 'findall(N, script_file(N, _), Ns).');
    expect(binding(links, 'Ns')).toContain('one');
    expect(binding(links, 'Ns')).toContain('two');
  });
});
