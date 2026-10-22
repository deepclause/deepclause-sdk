/**
 * M4 — two-level memory, execute scratch, reserved guards, and budgets.
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
} from '../src/types.js';

interface CannedResponse {
  text?: string;
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

function finalResponse(code: string, run?: string): CannedResponse {
  return {
    text: 'Here is the harness.',
    toolCalls: [
      ...(run
        ? [
            { name: 'begin_turn', arguments: {} },
            defineCall(code),
            { name: 'execute', arguments: { goal: run } },
            { name: 'end_turn', arguments: {} },
          ]
        : []),
      { name: 'set_result', arguments: { variable: 'DMLCode', value: code } },
      { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
      { name: 'finish', arguments: { success: true } },
    ],
  };
}

function taskResponse(value: string, request: LLMBackendRequest): CannedResponse {
  const setResult = request.tools?.find((tool) => tool.name === 'set_result');
  const variable = (setResult?.parameters as { properties?: { variable?: { enum?: string[] } } })
    ?.properties?.variable?.enum?.[0];
  const toolCalls: CannedResponse['toolCalls'] = [];
  if (variable) {
    toolCalls.push({ name: 'set_result', arguments: { variable, value } });
  }
  toolCalls.push({ name: 'finish', arguments: { success: true } });
  return { text: 'task done', toolCalls };
}

const NOTE_HELPER = [
  'has_note(Note) :-',
  '    get_memory(Memory),',
  '    member(Message, Memory),',
  '    get_dict(content, Message, Note).',
].join('\n');

let workspace: string;
let repl: HarnessRepl;
let backend: CannedBackend;

async function openRepl(extra: Record<string, unknown> = {}): Promise<HarnessRepl> {
  return createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    streaming: true,
    llmBackend: backend,
    ...extra,
  });
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

function hasSolution(events: DMLEvent[]): boolean {
  return events.some((event) => event.type === 'solution');
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-repl-context-'));
});

afterEach(async () => {
  await repl?.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('two-level memory', () => {
  it('keeps harness memory out of the meta task and meta memory out of the harness', async () => {
    backend = new CannedBackend((request, index) => {
      if (index === 0) {
        return finalResponse('demo :- output("ok").', 'demo');
      }
      return taskResponse('harness answer', request);
    });
    repl = await openRepl();
    await repl.session.loadCode(NOTE_HELPER);

    await collect(repl.query('system("harness-secret")'));
    await collect(repl.prompt('build something'));

    // The meta task must not see the harness's runtime memory.
    const metaMessages = backend.calls[0]?.messages.map((message) => message.content).join('\n') ?? '';
    expect(metaMessages).not.toContain('harness-secret');

    // A harness task must not see the code-writing conversation.
    await collect(repl.query('task("check the harness", R)'));
    const harnessCall = backend.calls[backend.calls.length - 1];
    const harnessMessages = harnessCall.messages.map((message) => message.content).join('\n');
    expect(harnessMessages).not.toContain('You are the DeepClause REPL harness author');
  });

  it('discards execute and commit memory writes (nothing runs the harness)', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'execute', arguments: { goal: 'system("scratch-note")' } },
        { name: 'set_result', arguments: { variable: 'DMLCode', value: 'demo :- system("entry-note"), output("done").' } },
        { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();
    await repl.session.loadCode(NOTE_HELPER);

    await collect(repl.prompt('write a note'));

    // Neither the scratch run nor the commit advances harness memory now.
    expect(hasSolution(await collect(repl.query('has_note("entry-note")')))).toBe(false);
    expect(hasSolution(await collect(repl.query('has_note("scratch-note")')))).toBe(false);
  });
});

describe('execute recursion', () => {
  it('supports a nested task inside execute', async () => {
    backend = new CannedBackend((request, index) => {
      if (index === 0) {
        return {
          text: '',
          toolCalls: [
            { name: 'execute', arguments: { goal: 'task("nested ask", R), output(R)' } },
            { name: 'begin_turn', arguments: {} },
            defineCall('kept(1).'),
            { name: 'end_turn', arguments: {} },
            { name: 'finish', arguments: { success: true } },
          ],
        };
      }
      return taskResponse('nested answer', request);
    });
    repl = await openRepl();

    const events = await collect(repl.prompt('run a nested task'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(backend.calls.length).toBeGreaterThanOrEqual(2);
    expect(hasSolution(await collect(repl.query('kept(_)')))).toBe(true);
  });

  it('rejects a reserved predicate in execute', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'execute', arguments: { goal: 'call(repl_freeze)' } },
        { name: 'set_result', arguments: { variable: 'DMLCode', value: 'kept(1).' } },
      { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();
    await collect(repl.prompt('try to freeze'));

    // The harness stays mutable because the reserved goal was refused.
    expect(repl.isFrozen()).toBe(false);
    await repl.freeze();
  });

  it('serializes concurrent execute calls', async () => {
    backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'execute', arguments: { goal: 'output("one")' } },
        { name: 'execute', arguments: { goal: 'output("two")' } },
        { name: 'set_result', arguments: { variable: 'DMLCode', value: 'kept(1).' } },
      { name: 'set_result', arguments: { variable: 'Answer', value: '' } },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await openRepl();

    const original = repl.session.executeGoal.bind(repl.session);
    let inFlight = 0;
    let maxInFlight = 0;
    (repl.session as unknown as { executeGoal: typeof original }).executeGoal = async (...args) => {
      inFlight += 1;
      maxInFlight = Math.max(maxInFlight, inFlight);
      await new Promise((resolve) => setTimeout(resolve, 5));
      try {
        return await original(...args);
      } finally {
        inFlight -= 1;
      }
    };

    await collect(repl.prompt('two executes'));
    expect(maxInFlight).toBe(1);
  });
});

describe('reserved guards', () => {
  it('refuses call(repl_freeze), asserted repl_*, and consulted repl_*', async () => {
    backend = new CannedBackend([finalResponse('kept(1).', '')]);
    repl = await openRepl();

    expect(hasSolution(await collect(repl.query('call(repl_freeze)')))).toBe(false);
    expect((await collect(repl.query('call(repl_freeze)'))).some((event) => event.type === 'error')).toBe(true);

    expect((await collect(repl.query('assertz(repl_evil(1))'))).some((event) => event.type === 'error')).toBe(true);

    await fs.writeFile(path.join(workspace, 'evil.dml'), 'repl_evil(1).\n');
    expect((await collect(repl.query('["evil.dml"]'))).some((event) => event.type === 'error')).toBe(true);
  });
});

describe('budgets', () => {
  it('enforces the frame depth budget on execute', async () => {
    backend = new CannedBackend([finalResponse('kept(1).', '')]);
    repl = await openRepl({ maxDepth: 0 });
    const result = await repl.session.executeGoal('true');
    expect(result.error).toContain('maximum frame depth');
  });

  it('aborts a turn when the model-call budget is exhausted', async () => {
    let calls = 0;
    backend = new CannedBackend((request) => {
      calls += 1;
      if (calls === 1) {
        // First call: request a listing, forcing a second model call.
        return { text: '', toolCalls: [{ name: 'listing', arguments: { filter: 'all' } }] };
      }
      return finalResponse('kept(1).', '');
    });
    repl = await openRepl({ maxModelCalls: 1 });

    const events = await collect(repl.prompt('spend the budget'));
    expect(events.some((event) => event.type === 'error' && event.content?.includes('budget'))).toBe(true);
    expect(hasSolution(await collect(repl.query('kept(_)')))).toBe(false);
  });

  it('aborts when the user declines to extend the budget', async () => {
    let calls = 0;
    backend = new CannedBackend(() => {
      calls += 1;
      if (calls === 1) {
        return { text: '', toolCalls: [{ name: 'listing', arguments: { filter: 'all' } }] };
      }
      return finalResponse('kept(1).', '');
    });
    repl = await openRepl({ maxModelCalls: 1, onInput: async () => 'no' });

    const events = await collect(repl.prompt('decline the budget'));
    expect(events.some((event) => event.type === 'error' && event.content?.includes('budget'))).toBe(true);
  });
});
