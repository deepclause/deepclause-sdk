/**
 * Harness history: sessions, traces, and harness versions persisted under
 * `.deepclause/repl/` and queryable from DML as `harness_*` facts.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { HistoryStore } from '../src/repl/history.js';
import type { DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

interface CannedResponse {
  text?: string;
  toolCalls?: Array<{ name: string; arguments: Record<string, unknown> }>;
}

class CannedBackend implements LLMBackend {
  readonly calls: LLMBackendRequest[] = [];

  constructor(private readonly script: CannedResponse[]) {}

  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls.push(request);
    const index = this.calls.length - 1;
    const response = this.script[Math.min(index, this.script.length - 1)] ?? { text: '' };
    return {
      text: response.text ?? '',
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

function commitResponse(code: string): CannedResponse {
  return {
    text: '',
    toolCalls: [
      { name: 'begin_turn', arguments: {} },
      defineCall(code),
      { name: 'end_turn', arguments: {} },
      { name: 'finish', arguments: { success: true } },
    ],
  };
}

/** Replace a predicate, then commit (so re-running yields identical content). */
function defineResponse(head: string, clauses: string): CannedResponse {
  return {
    text: '',
    toolCalls: [
      { name: 'begin_turn', arguments: {} },
      { name: 'define', arguments: { head, clauses } },
      { name: 'end_turn', arguments: {} },
      { name: 'finish', arguments: { success: true } },
    ],
  };
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

function outputs(events: DMLEvent[]): string[] {
  return events.filter((event) => event.type === 'output').map((event) => event.content);
}

const HISTORY = { dir: '.deepclause/repl', trace: 'summary' as const };

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-repl-history-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('harness history', () => {
  it('records sessions, turns, traces, and versions on disk', async () => {
    const backend = new CannedBackend([commitResponse('demo :- output(hi).')]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      streaming: true,
      llmBackend: backend,
      tools: new Map(),
      history: HISTORY,
    });

    const events = await collect(repl.prompt('make a demo'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    const sessionId = repl.session.getSessionId();
    await repl.dispose();
    repl = undefined;

    const history = await fs.readFile(path.join(workspace, '.deepclause/repl/history.dml'), 'utf8');
    expect(history).toContain('harness_session(');
    expect(history).toContain('harness_session_end(');
    expect(history).toContain('harness_turn(');
    const harnessHistory = await fs.readFile(
      path.join(workspace, '.deepclause/repl/harnesses/main/history.dml'),
      'utf8',
    );
    expect(harnessHistory).toContain('harness_change(1,');
    expect(harnessHistory).toContain('harness_changed(1,');
    expect(harnessHistory).toContain('harness_head(1,');

    const sessionDir = path.join(workspace, '.deepclause/repl/sessions', sessionId);
    expect(await fs.readFile(path.join(sessionDir, 'turns.jsonl'), 'utf8')).toContain('"outcome":"committed"');
    expect(await fs.readFile(path.join(sessionDir, 'events.jsonl'), 'utf8')).toContain('"type":"tool_call"');

    const head = JSON.parse(await fs.readFile(path.join(workspace, '.deepclause/repl/harnesses/main/HEAD.json'), 'utf8'));
    expect(head.version).toBe(1);
    expect(head.sessionId).toBe(sessionId);
  });

  it('does not create a new version when the committed content is unchanged', async () => {
    const backend = new CannedBackend([
      defineResponse('demo', 'demo :- output(hi).'),
      defineResponse('demo', 'demo :- output(hi).'),
    ]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      streaming: true,
      llmBackend: backend,
      tools: new Map(),
      history: HISTORY,
    });

    await collect(repl.prompt('make a demo'));
    await collect(repl.prompt('commit the same thing again'));

    const head = JSON.parse(await fs.readFile(path.join(workspace, '.deepclause/repl/harnesses/main/HEAD.json'), 'utf8'));
    expect(head.version).toBe(1);
    const changes = (await fs.readFile(path.join(workspace, '.deepclause/repl/harnesses/main/changes.jsonl'), 'utf8'))
      .trim()
      .split('\n')
      .filter((line) => JSON.parse(line).kind === 'commit');
    expect(changes).toHaveLength(1);
  });

  it('a fresh session starts from the harness content file', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      streaming: true,
      llmBackend: new CannedBackend([defineResponse('demo', 'demo :- output(hi).')]),
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.prompt('make a demo'));
    await repl.dispose();
    repl = undefined;

    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      streaming: true,
      llmBackend: new CannedBackend([{ text: 'ok' }]),
      tools: new Map(),
      history: HISTORY,
    });
    expect(repl.session.isEmpty()).toBe(false);
    const events = await collect(repl.query('demo'));
    expect(events.some((event) => event.type === 'output' && event.content === 'hi')).toBe(true);
  });

  it('keeps separate histories per named harness', async () => {
    const make = async (harness: string, head: string, clauses: string): Promise<void> => {
      const session = await createHarnessRepl({
        workspacePath: workspace,
        model: 'test-model',
        provider: 'openai',
        llmBackend: new CannedBackend([defineResponse(head, clauses)]),
        tools: new Map(),
        history: HISTORY,
        harness,
      });
      await collect(session.prompt(`make ${harness}`));
      await session.dispose();
    };
    await make('alpha', 'demoA', 'demoA :- output(a).');
    await make('beta', 'demoB', 'demoB :- output(b).');

    const dir = path.join(workspace, '.deepclause/repl/harnesses');
    const alpha = await fs.readFile(path.join(dir, 'alpha/harness.dml'), 'utf8');
    const beta = await fs.readFile(path.join(dir, 'beta/harness.dml'), 'utf8');
    expect(alpha).toContain('demoA');
    expect(alpha).not.toContain('demoB');
    expect(beta).toContain('demoB');
    expect(beta).not.toContain('demoA');
    const headA = JSON.parse(await fs.readFile(path.join(dir, 'alpha/HEAD.json'), 'utf8'));
    const headB = JSON.parse(await fs.readFile(path.join(dir, 'beta/HEAD.json'), 'utf8'));
    expect(headA.version).toBe(1);
    expect(headB.version).toBe(1);
  });

  it('a harness file loaded into a new harness becomes its baseline', async () => {
    await fs.writeFile(path.join(workspace, 'external.dml'), 'external :- output(hello).\n', 'utf8');
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([{ text: 'ok' }]),
      tools: new Map(),
      history: HISTORY,
      harness: 'external',
      autoLoadHarness: false,
    });
    const loaded = await repl.session.load('external.dml');
    expect(loaded.error).toBeUndefined();
    await repl.session.finalizeHarnessBaseline();

    // No committed versions: rollback floors at the loaded file.
    expect(await repl.session.rollback()).toMatchObject({ ok: false });
    const baseline = await fs.readFile(
      path.join(workspace, '.deepclause/repl/harnesses/external/baseline.dml'),
      'utf8',
    );
    expect(baseline).toContain('external');
    expect(baseline).toContain('output(hello)');
    await repl.dispose();
    repl = undefined;

    // A later session for that harness loads the imported content.
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([{ text: 'ok' }]),
      tools: new Map(),
      history: HISTORY,
      harness: 'external',
    });
    const events = await collect(repl.query('external'));
    expect(events.some((event) => event.type === 'output' && event.content === 'hello')).toBe(true);
  });

  it('migrates the legacy single harness store to harnesses/main', async () => {
    const legacy = path.join(workspace, '.deepclause/repl/harness');
    await fs.mkdir(path.join(legacy, 'versions'), { recursive: true });
    await fs.writeFile(path.join(legacy, 'HEAD.json'), JSON.stringify({ version: 1, maxVersion: 1 }), 'utf8');
    await fs.writeFile(
      path.join(legacy, 'versions/0001_2026-01-01T00-00-00-000Z.json'),
      JSON.stringify({ version: 1, parentVersion: null, module: 'legacy :- output(1).' }),
      'utf8',
    );

    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([{ text: 'ok' }]),
      tools: new Map(),
      history: HISTORY,
    });
    expect(repl.session.isEmpty()).toBe(false);
    const migrated = await fs.readFile(
      path.join(workspace, '.deepclause/repl/harnesses/main/harness.dml'),
      'utf8',
    );
    expect(migrated).toContain('legacy');
    const events = await collect(repl.query('legacy'));
    expect(events.some((event) => event.type === 'output' && event.content === '1')).toBe(true);
  });

  it('loads prior history into a new session so DML can query it', async () => {
    const first = new CannedBackend([commitResponse('demo :- output(hi).')]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: first,
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.prompt('make a demo'));
    await repl.dispose();
    repl = undefined;

    // Second session: history facts are consulted into the living module.
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([{ text: '' }]),
      tools: new Map(),
      history: HISTORY,
    });

    const changes = await collect(repl.query('harness_change(V, Ts, K, S, T), output(V-K)'));
    expect(outputs(changes)).toContain('1-commit');

    const sessions = await collect(repl.query('harness_session(S, Started, Model, Backend, Dir)'));
    expect(sessions.filter((event) => event.type === 'solution')).toHaveLength(2);

    // History facts are system-owned: not listed as harness predicates.
    const listing = await repl.list('all');
    expect(listing).not.toContain('harness_');
  });

  it('migrates pre-/5 history (old harness_change/6) on load', async () => {
    const dir = path.join(workspace, '.deepclause/repl');
    await fs.mkdir(dir, { recursive: true });
    await fs.writeFile(
      path.join(dir, 'history.dml'),
      "harness_change(1, '2026-01-01T00:00:00Z', commit, 'sess_old', 1, demo).\n"
        + "harness_head(1, '2026-01-01T00:00:00Z', 'sess_old', 1).\n",
    );

    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'm',
      provider: 'openai',
      llmBackend: new CannedBackend([{ text: '' }]),
      tools: new Map(),
      history: HISTORY,
    });

    const changes = await collect(repl.query('harness_change(V, Ts, K, S, T), output(V-K)'));
    expect(outputs(changes)).toContain('1-commit');
    expect(await repl.list('history')).toContain('harness_change(1');
  });

  it('rolls back the last harness transaction, step by step', async () => {
    const backend = new CannedBackend([
      commitResponse('demo :- output(one).'),
      defineResponse('demo', 'demo :- output(two).'),
    ]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      streaming: true,
      llmBackend: backend,
      tools: new Map(),
      history: HISTORY,
    });

    await collect(repl.prompt('first version'));
    const second = await collect(repl.prompt('second version'));
    expect(second.some((event) => event.type === 'error')).toBe(false);
    expect(outputs(await collect(repl.query('demo')))).toContain('two');

    // Undo the second commit -> back to version 1.
    expect(await repl.rollback()).toEqual({ ok: true, from: 2, to: 1 });
    expect(outputs(await collect(repl.query('demo')))).toContain('one');

    // Undo the first commit -> empty harness.
    expect(await repl.rollback()).toEqual({ ok: true, from: 1, to: 0 });
    expect((await collect(repl.query('demo'))).some((event) => event.type === 'error')).toBe(true);

    // Nothing left to undo.
    const nothing = await repl.rollback();
    expect(nothing.ok).toBe(false);
    expect(nothing.error).toMatch(/no harness changes/);

    const history = await fs.readFile(
      path.join(workspace, '.deepclause/repl/harnesses/main/history.dml'),
      'utf8',
    );
    expect(history).toContain('harness_rollback(');
    const head = JSON.parse(
      await fs.readFile(path.join(workspace, '.deepclause/repl/harnesses/main/HEAD.json'), 'utf8'),
    );
    expect(head.version).toBe(0);
    expect(head.maxVersion).toBe(2);
  });

  it('resumes a previous session: restores the harness and continues turns', async () => {
    const id = 'sess_resume';
    repl = await createHarnessRepl({
      sessionId: id,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([commitResponse('demo :- output(one).')]),
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.prompt('make demo'));
    await repl.dispose();
    repl = undefined;

    // The durable snapshot lands beside the session.
    const snapshot = JSON.parse(
      await fs.readFile(path.join(workspace, '.deepclause/repl/sessions', id, 'harness.json'), 'utf8'),
    );
    expect(snapshot.module).toContain('demo');
    expect(snapshot.version).toBe(1);
    expect(snapshot.hash).toBeTruthy();

    // Resume the same session: the harness comes back and turns continue at 2.
    repl = await createHarnessRepl({
      sessionId: id,
      resume: true,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([defineResponse('demo', 'demo :- output(two).')]),
      tools: new Map(),
      history: HISTORY,
    });

    expect(outputs(await collect(repl.query('demo')))).toContain('one');
    const changed = await collect(repl.prompt('change demo'));
    expect(changed.some((event) => event.type === 'error')).toBe(false);
    expect(outputs(await collect(repl.query('demo')))).toContain('two');

    const history = await fs.readFile(path.join(workspace, '.deepclause/repl/history.dml'), 'utf8');
    expect(history).toContain('harness_session_resume(');
    expect(history).toMatch(new RegExp(`harness_turn\\('${id}', 2,`));
  });

  it('notes in the model context that a resumed session was reloaded', async () => {
    const id = 'sess_resume_note';
    repl = await createHarnessRepl({
      sessionId: id,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([commitResponse('demo :- output(one).')]),
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.prompt('make demo'));
    await repl.dispose();
    repl = undefined;

    // Resume, then run a task so we see the memory the model is handed.
    const backend = new CannedBackend([{
      toolCalls: [
        { name: 'set_result', arguments: { variable: 'R', value: 'ok' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await createHarnessRepl({
      sessionId: id,
      resume: true,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.query('task("just reply", string(R))'));

    const seen = backend.calls
      .flatMap((call) => call.messages)
      .map((message) => `${message.role}: ${message.content}`)
      .join('\n');
    expect(seen).toContain('This session was reloaded');
    expect(seen).toContain('transient state was reset');
    expect(seen).toContain('subagents/services');

    // Resuming again replaces the note instead of stacking it.
    await repl.dispose();
    const backend2 = new CannedBackend([{
      toolCalls: [
        { name: 'set_result', arguments: { variable: 'R', value: 'ok' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await createHarnessRepl({
      sessionId: id,
      resume: true,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend2,
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.query('task("reply again", string(R))'));
    // Count within one model call: the memory is repeated across calls.
    const firstCall = backend2.calls[0]?.messages.map((message) => message.content).join('\n') ?? '';
    expect(firstCall.match(/This session was reloaded/g)).toHaveLength(1);
  });

  it('replays a resumed session transcript on load', async () => {
    const id = 'sess_replay';
    repl = await createHarnessRepl({
      sessionId: id,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([commitResponse('demo :- output(one).')]),
      tools: new Map(),
      history: HISTORY,
    });
    await collect(repl.prompt('make demo'));
    // Run the skill so an `output` event is recorded for the session.
    expect(outputs(await collect(repl.query('demo')))).toContain('one');
    await repl.dispose();
    repl = undefined;

    repl = await createHarnessRepl({
      sessionId: id,
      resume: true,
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new CannedBackend([{ text: '' }]),
      tools: new Map(),
      history: HISTORY,
    });

    const prior = await repl.resumedTranscript();
    expect(prior.length).toBeGreaterThan(0);
    expect(prior.some((event) => event.type === 'output' && event.content?.includes('one'))).toBe(true);
  });

  it('indexes sessions and resolves the latest for --session last', async () => {
    const first = new HistoryStore(workspace, 'sess_a', HISTORY);
    await first.init();
    await first.startSession({ model: 'm', backend: 'openai' });
    await new Promise((resolve) => setTimeout(resolve, 5));
    const second = new HistoryStore(workspace, 'sess_b', HISTORY);
    await second.init();
    await second.startSession({ model: 'm', backend: 'openai' });

    expect(await second.sessionExists('sess_a')).toBe(true);
    expect(await second.sessionExists('nope')).toBe(false);
    expect(await second.latestSessionId()).toBe('sess_b');
  });

  it('reads a session turn list for input history', async () => {
    const store = new HistoryStore(workspace, 'sess_a', HISTORY);
    await store.init();
    await store.startSession({ model: 'm', backend: 'openai' });
    await store.recordTurn({ turn: 1, mode: 'prompt', request: 'add a fact', outcome: 'committed', durationMs: 1, modelCalls: 1 });
    await store.recordTurn({ turn: 2, mode: 'goal', request: 'member(X, [1,2])', outcome: 'query', durationMs: 1, modelCalls: 0 });

    const turns = await store.readTurns('sess_a');
    expect(turns.map((turn) => turn.request)).toEqual(['add a fact', 'member(X, [1,2])']);
    expect(await store.readTurns('missing')).toEqual([]);
  });
});
