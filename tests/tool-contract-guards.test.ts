/**
 * Two guards for the `fs_write(Path, Content)` mistake: a tool whose output
 * argument is never bound must fail, and a `tool/3` type list that does not match
 * the input count must warn at load time.
 */
import { afterEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createDeepClause, createHarnessRepl, type DeepClauseSDK, type DMLEvent, type HarnessRepl } from '../src/index.js';
import type { LLMBackend, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;
let repl: HarnessRepl | undefined;
let workspace: string | undefined;

afterEach(async () => {
  await sdk?.dispose();
  await repl?.dispose();
  if (workspace) await fs.rm(workspace, { recursive: true, force: true });
  sdk = undefined;
  repl = undefined;
  workspace = undefined;
});

function toolCall(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return { text: '', toolCalls: [{ id: name, name, arguments: args }], usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 } };
}

/** Call one tool, then finish. */
function backend(name: string, args: Record<string, unknown>): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      const answered = request.messages.some((message) => message.role === 'tool');
      if (!answered) return toolCall(name, args);
      return {
        text: '',
        toolCalls: [
          { id: 'set', name: 'set_result', arguments: { variable: 'R', value: 'done' } },
          { id: 'finish', name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
      };
    },
  };
}

const DML = `
agent_main :- with_tools([bad], task("Write it.", string(R))), answer(R).
tool(bad(Path, Content), "Write a file (content taken for the output: the bug).") :- write_file(Path, Content).
`;

describe('a tool that returns no result', () => {
  it('fails with an explanation instead of reporting success', async () => {
    workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-no-result-'));
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend('bad', { arg1: 'out.txt' }) });
    const events: DMLEvent[] = [];
    for await (const event of sdk.runDML(DML, { workspacePath: workspace })) events.push(event);

    const call = events.filter((event) => event.type === 'tool_call' && event.toolName === 'bad');
    expect(call.some((event) => event.toolState === 'failed')).toBe(true);
    expect(JSON.stringify(call)).toContain('returned no result');
    expect(JSON.stringify(call)).toContain('last argument');
  });
});

describe('a tool/3 type list that does not match its inputs', () => {
  it('warns at load time', async () => {
    workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-tool-types-'));
    repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
    // Two types declared, but the head has one input and one output.
    const loaded = await repl.session.loadCode('tool(bad(A, B), "Two inputs, no output.", [string, string]) :- A = B.');
    expect(loaded.error).toBeUndefined();
    expect((loaded.warnings ?? []).join('\n')).toContain('input argument');
  });
});
