/**
 * Typed tool inputs: `tool(Head, Desc, [Type, …])` lets a tool take an object,
 * an array, a number or a boolean — not just a string.
 */
import { afterEach, describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, DMLEvent, LLMBackend, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

function toolCall(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return { text: '', toolCalls: [{ id: name, name, arguments: args }], usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 } };
}

/** Call one tool with the given args, then finish. */
function backend(name: string, args: Record<string, unknown>): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      const answered = request.messages.some((message) => message.role === 'tool');
      if (!answered) return toolCall(name, args);
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'R';
      return {
        text: '',
        toolCalls: [
          { id: 'set', name: 'set_result', arguments: { variable, value: 'done' } },
          { id: 'finish', name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
      };
    },
  };
}

async function run(code: string, name: string, args: Record<string, unknown>): Promise<DMLEvent[]> {
  sdk = await createDeepClause({ model: 'test-model', llmBackend: backend(name, args) });
  const events: DMLEvent[] = [];
  for await (const event of sdk.runDML(code)) events.push(event);
  return events;
}

const toolResult = (events: DMLEvent[]): string =>
  JSON.stringify(events.filter((event) => event.type === 'tool_call').map((event) => event.toolResult));

describe('typed tool inputs', () => {
  it('accepts an integer argument when declared', async () => {
    const events = await run(
      'agent_main :- with_tools([scale], task("Scale it.", string(R))), answer(R).\ntool(scale(N, Result), "Multiply N by 2.", [integer]) :- Result is N * 2.',
      'scale',
      { arg1: 3 },
    );
    expect(toolResult(events)).toContain('6');
  });

  it('accepts an object argument when declared', async () => {
    const events = await run(
      'agent_main :- with_tools([inspect], task("Inspect it.", string(R))), answer(R).\ntool(inspect(Spec, Name), "Read a name.", [object]) :- get_dict(name, Spec, Name).',
      'inspect',
      { arg1: { name: 'kit' } },
    );
    expect(toolResult(events)).toContain('kit');
  });

  it('still rejects the wrong type', async () => {
    const events = await run(
      'agent_main :- with_tools([scale], task("Scale it.", string(R))), answer(R).\ntool(scale(N, Result), "Multiply N by 2.", [integer]) :- Result is N * 2.',
      'scale',
      { arg1: 'three' },
    );
    expect(JSON.stringify(events)).toContain('should be integer');
  });

  it('defaults an undeclared input to string', async () => {
    const events = await run(
      'agent_main :- with_tools([echo], task("Echo it.", string(R))), answer(R).\ntool(echo(Text, Out), "Echo a string.") :- Out = Text.',
      'echo',
      { arg1: 'hi' },
    );
    expect(toolResult(events)).toContain('hi');
  });
});
