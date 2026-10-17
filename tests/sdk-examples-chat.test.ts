/**
 * `sdk-examples/chat.dml` is the public conversational example. Load it and run
 * one turn with a scripted model, so the file cannot rot: a renamed primitive or
 * a changed chat semantic fails here rather than in a reader's session.
 */
import { afterEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/index.js';

const examplePath = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  '../sdk-examples/chat.dml',
);

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

/** Turn 1 calls `add`, turn 2 replies with the sum. */
function scriptedBackend(requests: LLMBackendRequest[]): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      requests.push(request);
      if (requests.length === 1) {
        return {
          text: '',
          toolCalls: [{ id: 't1', name: 'add', arguments: { arg1: 2, arg2: 3 } }],
          usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
        };
      }
      return {
        text: 'The answer is 5.',
        toolCalls: [],
        usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
      };
    },
  };
}

describe('sdk-examples/chat.dml', () => {
  it('runs a turn with a tool, then waits for the next message', async () => {
    const code = await fs.readFile(examplePath, 'utf8');
    const requests: LLMBackendRequest[] = [];
    sdk = await createDeepClause({ model: 'test-model', llmBackend: scriptedBackend(requests) });

    const outputs: string[] = [];
    const errors: string[] = [];
    let inputs = 0;
    for await (const event of sdk.runDML(code, {
      onUserInput: async () => {
        inputs += 1;
        return (inputs === 1 ? 'what is 2 + 3?' : undefined) as unknown as string;
      },
    })) {
      if (event.type === 'output' && event.content) outputs.push(event.content);
      if (event.type === 'error') errors.push(event.content ?? '');
    }

    expect(errors).toEqual([]);
    // The tool ran (two model calls in one turn), the reply is shown, and the
    // example then asked for another message — which ends the loop when none comes.
    expect(requests).toHaveLength(2);
    expect(JSON.stringify(requests[1].messages)).toContain('5');
    expect(outputs.join('\n')).toContain('The answer is 5.');
    expect(inputs).toBe(2);
    // A chat turn offers no finish/set_result.
    expect((requests[0].tools ?? []).map((tool) => tool.name)).not.toContain('finish');
  });
});
