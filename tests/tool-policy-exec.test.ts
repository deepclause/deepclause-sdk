/**
 * The tool policy governs host capabilities — and `exec/2` inside a DML tool
 * body is one. The top-level path checked the policy; the tool-engine path did
 * not, so a `tool/2` body could reach a blocked host tool.
 */
import { describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/index.js';

/** A backend that calls one DML tool, then reports whatever result it got. */
function scripted(toolName: string): { backend: LLMBackend; requests: LLMBackendRequest[] } {
  const requests: LLMBackendRequest[] = [];
  const backend: LLMBackend = {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      requests.push(request);
      const usage = { inputTokens: 1, outputTokens: 1, totalTokens: 2 };
      const answered = request.messages.some((message) => message.role === 'tool');
      if (!answered) {
        return { text: '', toolCalls: [{ id: 'c', name: toolName, arguments: {} }], usage, finishReason: 'tool_use' };
      }
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'R';
      return {
        text: '',
        toolCalls: [
          { id: 'set', name: 'set_result', arguments: { variable, value: 'done' } },
          { id: 'finish', name: 'finish', arguments: { success: true } },
        ],
        usage,
        finishReason: 'tool_use',
      };
    },
  };
  return { backend, requests };
}

async function run(
  code: string,
  toolName: string,
): Promise<{ dangerRan: boolean; requests: LLMBackendRequest[]; events: DMLEvent[] }> {
  let dangerRan = false;
  const { backend, requests } = scripted(toolName);
  const dc = await createDeepClause({ model: 'test-model', llmBackend: backend });
  dc.registerTool('danger', {
    description: 'A host tool that should be blocked.',
    parameters: { type: 'object', properties: { n: { type: 'number' } } },
    execute: async () => {
      dangerRan = true;
      return { ok: true };
    },
  });
  dc.registerTool('safe', {
    description: 'An allowed host tool.',
    parameters: { type: 'object', properties: {} },
    execute: async () => ({ ok: true }),
  });
  dc.setToolPolicy({ mode: 'whitelist', tools: ['safe'] });
  const events: DMLEvent[] = [];
  try {
    for await (const event of dc.runDML(code)) events.push(event);
  } finally {
    await dc.dispose();
  }
  return { dangerRan, requests, events };
}

/** Everything the operator can see: log lines and tool-call errors. */
const eventText = (events: DMLEvent[]): string =>
  events.map((event) => event.content ?? event.toolError ?? '').join('\n');

describe('tool policy and exec', () => {
  it('blocks a top-level exec of a tool outside the whitelist', async () => {
    const { dangerRan } = await run(
      'agent_main :- ( exec(danger(1), _) -> answer("ran") ; answer("blocked") ).',
      'set_result',
    );
    expect(dangerRan).toBe(false);
  });

  it('blocks exec/2 inside a DML tool body', async () => {
    const code = [
      'tool(proxy(R), "Calls a host tool.") :- exec(danger(1), R).',
      'agent_main :- task("Use the proxy tool.", string(_)).',
    ].join('\n');
    const { dangerRan, events } = await run(code, 'proxy');

    // The blocked tool never runs, and the operator sees why; the model gets a
    // plain failure, exactly as a top-level `exec` of a blocked tool does.
    expect(dangerRan).toBe(false);
    expect(eventText(events)).toContain('whitelist');
  });
});
