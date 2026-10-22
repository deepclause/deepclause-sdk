/**
 * The TS authoring agent: harness tools and the agent loop.
 */
import { describe, expect, it, vi } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { HarnessAgent, buildAgentSystemPrompt } from '../src/repl/agent.js';
import { createHarnessTools, type HarnessToolController } from '../src/repl/harness-tools.js';
import type { DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

function stubController(overrides: Partial<HarnessToolController> = {}): {
  controller: HarnessToolController;
  calls: Array<{ method: string; args: unknown[] }>;
} {
  const calls: Array<{ method: string; args: unknown[] }> = [];
  const record = <T>(method: string, value: T): T => {
    calls.push({ method, args: [] });
    return value;
  };
  const controller: HarnessToolController = {
    isFrozen: () => record('isFrozen', false),
    listing: async (filter) => record('listing', `${filter}: a/1`),
    manifest: async () => record('manifest', 'a/1'),
    beginTurn: async () => { record('beginTurn', undefined); },
    endTurn: async () => record('endTurn', { committed: true, changed: ['+a/1'] }),
    rollbackTurn: async () => { record('rollbackTurn', undefined); },
    define: async (head, clauses) => { record('define', undefined); return { ok: true, head, clauses } as { ok: boolean }; },
    assertClause: async () => record('assertClause', { ok: true }),
    assertMany: async () => record('assertMany', { ok: true }),
    retractClause: async () => record('retractClause', { ok: true }),
    deletePredicate: async (predicate) => { record('deletePredicate', undefined); return { ok: true, predicate } as { ok: boolean }; },
    execute: async (_goal, _limit, _signal, onEvent) => {
      onEvent?.({
        type: 'output',
        content: 'child-output',
        frameKind: 'nested',
        depth: 1,
      } as DMLEvent);
      record('execute', undefined);
      return { bindings: [], solutions: [], output: 'child-output' };
    },
    reference: async () => record('reference', 'reference text'),
    listModels: async () => record('listModels', 'current model: a:b\n\n  a:b'),
    ...overrides,
  };
  return { controller, calls };
}

class ScriptedBackend implements LLMBackend {
  readonly calls: LLMBackendRequest[] = [];
  constructor(private readonly responses: LLMBackendResponse[]) {}
  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls.push(request);
    return this.responses.shift() ?? { text: 'done', finishReason: 'stop' };
  }
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) events.push(event);
  return events;
}

describe('createHarnessTools', () => {
  it('exposes the harness editing tools', () => {
    const { controller } = stubController();
    const names = [...createHarnessTools(controller).keys()];
    for (const expected of [
      'listing', 'begin_turn', 'end_turn', 'rollback', 'define', 'delete', 'execute', 'reference',
    ]) {
      expect(names).toContain(expected);
    }
    // `ask_user` is a DML host tool (input/2, exec(ask_user)), not an authoring tool.
    expect(names).not.toContain('ask_user');
    // `finish` is not exposed to the model either.
    expect(names).not.toContain('finish');
  });

  it('routes each tool to the controller', async () => {
    const { controller, calls } = stubController();
    const tools = createHarnessTools(controller);
    await tools.get('define')!.execute({ head: 'a(1)', clauses: 'a(1).' });
    await tools.get('delete')!.execute({ predicate: 'c/1' });
    await tools.get('execute')!.execute({ goal: 'a(X)' });
    await tools.get('listing')!.execute({ filter: 'all' });
    expect(calls.map((call) => call.method)).toEqual(
      expect.arrayContaining(['define', 'deletePredicate', 'execute', 'listing']),
    );
  });

  it('refuses edits when the harness is frozen', async () => {
    const { controller } = stubController({ isFrozen: () => true });
    const tools = createHarnessTools(controller);
    expect(await tools.get('define')!.execute({ head: 'a(1)', clauses: 'a(1).' })).toMatchObject({ ok: false });
    expect(await tools.get('delete')!.execute({ predicate: 'a/1' })).toMatchObject({ ok: false });
  });
});

describe('HarnessAgent', () => {
  it('runs a tool, then finishes', async () => {
    const { controller, calls } = stubController();
    const backend = new ScriptedBackend([
      { text: 'editing', toolCalls: [{ id: '1', name: 'define', arguments: { head: 'a(1)', clauses: 'a(1).' } }], finishReason: 'tool_use' },
      { text: 'done', toolCalls: [{ id: '2', name: 'finish', arguments: { success: true } }], finishReason: 'tool_use' },
    ]);
    const agent = new HarnessAgent({
      tools: createHarnessTools(controller),
      llmBackend: backend,
      model: 'test-model',
      streaming: true,
    });
    const events = await collect(agent.run('add a fact'));
    expect(calls.map((call) => call.method)).toEqual(expect.arrayContaining(['define']));
    expect(events.some((event) => event.type === 'tool_call' && event.toolName === 'define')).toBe(true);
    const completed = events.find((event) => (
      event.type === 'tool_call' && event.toolName === 'define' && event.toolState === 'completed'
    ));
    expect(typeof completed?.toolDurationMs).toBe('number');
    expect(backend.calls).toHaveLength(2);
    expect(agent.getMessages().some((message) => message.role === 'user' && message.content === 'add a fact')).toBe(true);
  });

  it('streams text and reasoning deltas', async () => {
    const { controller } = stubController();
    const backend: LLMBackend = {
      complete: async (request) => {
        request.onText?.('hel');
        request.onText?.('lo');
        request.onReasoning?.('think');
        return {
          text: 'hello',
          toolCalls: [{ id: '1', name: 'finish', arguments: { success: true } }],
          finishReason: 'tool_use',
        };
      },
    };
    const agent = new HarnessAgent({ tools: createHarnessTools(controller), llmBackend: backend, model: 'm', streaming: true });
    const events = await collect(agent.run('hi'));
    expect(events.filter((event) => event.type === 'stream').map((event) => event.content)).toEqual(['hel', 'lo']);
    expect(events.some((event) => event.type === 'reasoning' && event.content === 'think')).toBe(true);
  });

  it('reports a backend error', async () => {
    const { controller } = stubController();
    const backend: LLMBackend = { complete: vi.fn(async () => { throw new Error('boom'); }) };
    const agent = new HarnessAgent({ tools: createHarnessTools(controller), llmBackend: backend, model: 'm', streaming: true });
    const events = await collect(agent.run('hi'));
    expect(events.some((event) => event.type === 'error' && (event.content ?? '').includes('boom'))).toBe(true);
  });

  it('surfaces a missing tool', async () => {
    const { controller } = stubController();
    const backend = new ScriptedBackend([
      { text: '', toolCalls: [{ id: '1', name: 'nope', arguments: {} }], finishReason: 'tool_use' },
      { text: 'done', toolCalls: [{ id: '2', name: 'finish', arguments: { success: true } }], finishReason: 'tool_use' },
    ]);
    const agent = new HarnessAgent({ tools: createHarnessTools(controller), llmBackend: backend, model: 'm', streaming: true });
    const events = await collect(agent.run('hi'));
    expect(events.some((event) => event.type === 'tool_call' && event.toolState === 'failed')).toBe(true);
  });

  it('streams events an execute tool produces', async () => {
    const { controller } = stubController();
    const backend = new ScriptedBackend([
      { text: '', toolCalls: [{ id: '1', name: 'execute', arguments: { goal: 'x' } }], finishReason: 'tool_use' },
      { text: 'done', toolCalls: [{ id: '2', name: 'finish', arguments: { success: true } }], finishReason: 'tool_use' },
    ]);
    const agent = new HarnessAgent({ tools: createHarnessTools(controller), llmBackend: backend, model: 'm', streaming: true });
    const events = await collect(agent.run('run it'));
    const output = events.find((event) => event.type === 'output' && event.content === 'child-output');
    expect(output).toBeDefined();
    expect(output?.frameKind).toBe('nested');
  });

  it('never persists host nudges in the conversation history', async () => {
    const { controller } = stubController();
    const backend = new ScriptedBackend([
      { text: 'answer one', finishReason: 'stop' },
      { text: 'answer two', finishReason: 'stop' },
    ]);
    const agent = new HarnessAgent({ tools: createHarnessTools(controller), llmBackend: backend, model: 'm', streaming: true });
    await collect(agent.run('a direct question'));
    const messages = agent.getMessages();
    expect(messages.some((message) => message.content.includes('Continue: call a tool'))).toBe(false);
    expect(messages.filter((message) => message.role === 'user')).toHaveLength(1);
  });

  it('allows a workspace override of the system prompt (used verbatim)', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-agent-prompt-'));
    try {
      await fs.mkdir(path.join(workspace, '.deepclause', 'system'), { recursive: true });
      await fs.writeFile(
        path.join(workspace, '.deepclause', 'system', 'AGENT_PROMPT.md'),
        'CUSTOM PROMPT — no template substitution.',
        'utf8',
      );
      const prompt = await buildAgentSystemPrompt({ workspacePath: workspace });
      expect(prompt).toContain('CUSTOM PROMPT — no template substitution.');
    } finally {
      await fs.rm(workspace, { recursive: true, force: true });
    }
  });

  it('uses a replacement profile without appending the project contract', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-agent-profile-'));
    try {
      await fs.writeFile(path.join(workspace, 'AGENTS.md'), '# House rules\nAlways cite sources.', 'utf8');
      const prompt = await buildAgentSystemPrompt({
        workspacePath: workspace,
        systemPrompt: 'LOCAL PROFILE',
        modelCatalog: [{ provider: 'p', id: 'm', input: 1, output: 2 } as never],
      });
      expect(prompt).toContain('LOCAL PROFILE');
      expect(prompt).not.toContain('PROJECT INSTRUCTIONS (AGENTS.md)');
      expect(prompt).not.toContain('AVAILABLE MODELS');
    } finally {
      await fs.rm(workspace, { recursive: true, force: true });
    }
  });

  it('appends a workspace AGENTS.md to the system prompt', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-agent-agents-'));
    try {
      await fs.writeFile(path.join(workspace, 'AGENTS.md'), '# House rules\nAlways cite sources.', 'utf8');
      const prompt = await buildAgentSystemPrompt({ workspacePath: workspace });
      expect(prompt).toContain('PROJECT INSTRUCTIONS (AGENTS.md)');
      expect(prompt).toContain('Always cite sources.');
    } finally {
      await fs.rm(workspace, { recursive: true, force: true });
    }
  });

  it('injects a steering message into the next model call', async () => {
    const { controller } = stubController();
    const calls: LLMBackendRequest[] = [];
    let agentRef: HarnessAgent;
    const backend: LLMBackend = {
      complete: async (request) => {
        calls.push(request);
        if (calls.length === 1) {
          agentRef.steer('actually, focus on X');
          return {
            text: 'working',
            toolCalls: [{ id: '1', name: 'execute', arguments: { goal: 'x' } }],
            finishReason: 'tool_use',
          };
        }
        return {
          text: 'done',
          toolCalls: [{ id: '2', name: 'finish', arguments: { success: true } }],
          finishReason: 'tool_use',
        };
      },
    };
    agentRef = new HarnessAgent({ tools: createHarnessTools(controller), llmBackend: backend, model: 'm', streaming: false });
    await collect(agentRef.run('do it'));
    expect(calls.length).toBeGreaterThanOrEqual(2);
    expect(calls[1].messages.some((message) => message.role === 'user' && message.content.includes('focus on X'))).toBe(true);
  });

  it('uses the single bundled prompt with no template placeholders', async () => {
    const prompt = await buildAgentSystemPrompt();
    expect(prompt).toContain('DML REPL');
    expect(prompt).toContain('`tool/2`');
    expect(prompt).not.toContain('{{');
  });

  it('asks the human to extend the model-call budget, then continues', async () => {
    const { controller } = stubController();
    let backendCalls = 0;
    const backend: LLMBackend = {
      complete: async () => {
        backendCalls += 1;
        return {
          text: '',
          toolCalls: [{ id: String(backendCalls), name: 'listing', arguments: { filter: 'all' } }],
          finishReason: 'tool_use',
        };
      },
    };
    const asks: string[] = [];
    let asked = 0;
    const agent = new HarnessAgent({
      tools: createHarnessTools(controller),
      llmBackend: backend,
      model: 'm',
      streaming: false,
      maxModelCalls: 2,
      askInput: async (prompt) => {
        asks.push(prompt);
        asked += 1;
        return asked === 1 ? 'yes' : 'no';
      },
    });

    const events = await collect(agent.run('keep going'));

    expect(asks.length).toBeGreaterThanOrEqual(1);
    expect(asks[0]).toContain('model-call budget');
    expect(asks[0]).toContain('repl.maxModelCalls'); // reminds the user of the setting
    expect(backendCalls).toBeGreaterThan(2); // continued past the first budget
    const error = events.find((event) => event.type === 'error');
    expect(error?.content).toContain('budget exhausted');
    expect(error?.content).toContain('repl.maxModelCalls');
  });

  it('aborts with a helpful message when there is no input channel', async () => {
    const { controller } = stubController();
    const backend: LLMBackend = {
      complete: async () => ({
        text: '',
        toolCalls: [{ id: '1', name: 'listing', arguments: { filter: 'all' } }],
        finishReason: 'tool_use',
      }),
    };
    const agent = new HarnessAgent({
      tools: createHarnessTools(controller),
      llmBackend: backend,
      model: 'm',
      streaming: false,
      maxModelCalls: 1,
    });

    const events = await collect(agent.run('loop'));
    const error = events.find((event) => event.type === 'error');
    expect(error?.content).toContain('budget exhausted');
    expect(error?.content).toContain('repl.maxModelCalls');
  });
});
