/**
 * Nested task/prompt calls inside a DML tool.
 *
 * The tool engine has its own request_agent_loop handler. It previously lacked
 * the 8-argument task and 6-argument prompt patterns, so a tool body calling
 * task/N produced "Unknown tool engine result" instead of running the nested
 * agent. These tests exercise the real tool-engine path.
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
  ResolvedModel,
} from '../src/types.js';

interface ScriptContext {
  calls: LLMBackendRequest[];
  /** Target requested by the nested with_model scope, if any. */
  nestedTarget?: ResolvedModel;
}

function toolCall(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return {
    text: '',
    toolCalls: [{ id: `call-${name}`, name, arguments: args }],
    finishReason: 'tool_use',
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  };
}

function resultAndFinish(variable: string, value: string): LLMBackendResponse {
  return {
    text: '',
    toolCalls: [
      { id: `set-${variable}`, name: 'set_result', arguments: { variable, value } },
      { id: 'finish', name: 'finish', arguments: { success: true } },
    ],
    finishReason: 'tool_use',
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  };
}

function isInner(request: LLMBackendRequest): boolean {
  return request.messages.some((message) => String(message.content).includes('Subtask: inner'));
}

function nestedBackend(context: ScriptContext): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      context.calls.push(request);
      if (request.target) context.nestedTarget = request.target;
      if (isInner(request)) return resultAndFinish('Out', 'inner-result');
      if (request.messages.some((message) => message.role === 'tool')) {
        return resultAndFinish('R', 'outer-result');
      }
      return toolCall('echo', { arg1: 'x' });
    },
  };
}

function textOnlyBackend(context: ScriptContext): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      context.calls.push(request);
      return { text: 'plain prompt answer', finishReason: 'stop' };
    },
  };
}

const resolver = {
  async resolve(spec: any): Promise<ResolvedModel> {
    if (spec === 'default') throw new Error('default');
    return {
      provider: 'alt',
      id: 'm',
      reasoning: false,
      temperature: spec.temperature,
      maxTokens: spec.maxTokens,
    };
  },
  describe: () => 'alt:m',
  catalog: () => [{ model: 'alt:m' }],
};

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) events.push(event);
  return events;
}

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-nested-tool-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('task and prompt inside a DML tool', () => {
  it('runs a nested task inside a tool', async () => {
    const context: ScriptContext = { calls: [] };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: nestedBackend(context),
      maxModelCalls: 8,
    });
    await repl.session.loadCode([
      'go(R) :- with_tools([echo], task("outer", string(R))).',
      'tool(echo(Text, Out), "echo", [string]) :- task("inner {Text}", Out).',
    ].join('\n'));

    const events = await collect(repl.query('go(R)'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(JSON.stringify(events)).not.toContain('Unknown tool engine result');
    expect(context.calls.length).toBe(3);
  });

  it('runs a nested prompt inside a tool', async () => {
    const context: ScriptContext = { calls: [] };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: nestedBackend(context),
      maxModelCalls: 8,
    });
    await repl.session.loadCode([
      'go(R) :- with_tools([echo], task("outer", string(R))).',
      'tool(echo(Text, Out), "echo", [string]) :- prompt("inner {Text}", Out).',
    ].join('\n'));

    const events = await collect(repl.query('go(R)'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(JSON.stringify(events)).not.toContain('Unknown tool engine result');
    expect(context.calls.length).toBe(3);
  });

  it('resolves with_model for a task nested inside a tool', async () => {
    const context: ScriptContext = { calls: [] };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: nestedBackend(context),
      modelResolver: resolver as never,
      maxModelCalls: 8,
    });
    await repl.session.loadCode([
      'go(R) :- with_tools([echo], task("outer", string(R))).',
      'tool(echo(Text, Out), "echo", [string]) :- with_model("alt:m", task("inner {Text}", Out)).',
    ].join('\n'));

    const events = await collect(repl.query('go(R)'));
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(context.nestedTarget).toMatchObject({ provider: 'alt', id: 'm' });
  });

  // The one-shot prompt fix is deliberately not part of this change. This
  // expected-failure test records the remaining defect and will start failing
  // (alerting us) once prompt/N is corrected.
  it.fails('uses one model call for a text-only prompt', async () => {
    const context: ScriptContext = { calls: [] };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: textOnlyBackend(context),
      maxModelCalls: 2,
    });
    await repl.session.loadCode('go(A) :- prompt("Say hi", A).');
    await collect(repl.query('go(A)'));
    expect(context.calls.length).toBe(1);
  });
});
