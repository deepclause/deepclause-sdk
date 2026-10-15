/**
 * Two LLM-backed subagents that talk to each other, end to end.
 *
 * Runs `sdk-examples/debate.dml` with a deterministic mock model (the injected
 * backend returns `set_result`/`finish`, which is how an injected backend answers
 * `task/N`). Asserts the full exchange: Pro opens, Con rebuts, both close, and a
 * judge call returns a verdict.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';
import { fileURLToPath } from 'url';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

const here = path.dirname(fileURLToPath(import.meta.url));

function mockBackend(): LLMBackend {
  let turn = 0;
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      turn += 1;
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const user = request.messages.filter((m) => m.role === 'user').map((m) => m.content).join('\n');
      const prompt = `${system}\n${user}`;
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'Result';
      const judge = /judging a debate/i.test(prompt);
      const side = /the pro side/i.test(prompt) ? 'PRO' : /the con side/i.test(prompt) ? 'CON' : 'SIDE';
      const value = judge ? `${side === 'SIDE' ? '' : side} verdict ${turn}` : `${side} turn ${turn}`;
      return {
        text: '',
        toolCalls: [
          { id: `set-${turn}`, name: 'set_result', arguments: { variable, value } },
          { id: `finish-${turn}`, name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 8, outputTokens: 3, totalTokens: 11 },
      };
    },
  };
}

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-debate-'));
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    llmBackend: mockBackend(),
  });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('debate example', () => {
  it('runs two LLM-backed subagents that exchange arguments and are judged', async () => {
    const dml = await fs.readFile(path.join(here, '..', 'sdk-examples', 'debate.dml'), 'utf8');
    const load = await repl.session.loadCode(dml);
    expect(load.error).toBeUndefined();

    let closings = 0;
    repl.session.setProcessEventSink((event) => {
      if (event.type === 'output' && event.content?.includes('closing')) closings += 1;
    });

    let verdict = '';
    const errors: string[] = [];
    for await (const event of repl.query('debate("Test topic", 2, Verdict)')) {
      if (event.type === 'solution') {
        verdict = event.solution?.bindings.find((binding) => binding.name === 'Verdict')?.value ?? '';
      }
      if (event.type === 'error' && event.content) errors.push(event.content);
    }

    expect(errors).toEqual([]);
    expect(verdict).toContain('verdict'); // the judge call ran and returned
    expect(closings).toBe(2); // both debaters produced a closing
    const states = repl.session.processes().map((process) => process.state);
    expect(states).toHaveLength(2);
    expect(states.every((state) => state === 'done')).toBe(true);
  });
});
