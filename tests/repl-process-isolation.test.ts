/**
 * Subagent LLM-context isolation.
 *
 * Each process owns its conversation memory (its `system/1`/`user/1`/`task`
 * history). This asserts that a worker sees only its own, and never another
 * worker's or the parent's.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

/** Records the full prompt of every model call. */
function recordingBackend(calls: string[][]): LLMBackend {
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      calls.push(request.messages.map((message) => `${message.role}:${message.content}`));
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'Result';
      return {
        text: '',
        toolCalls: [
          { id: 'set', name: 'set_result', arguments: { variable, value: 'ok' } },
          { id: 'finish', name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
      };
    },
  };
}

let repl: HarnessRepl;
let calls: string[][];

async function waitForTerminal(id: string, timeoutMs = 15_000): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const info = repl.session.processes().find((process) => process.pid.id === id);
    if (info && ['done', 'crashed', 'killed'].includes(info.state)) return;
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  throw new Error('process did not finish');
}

beforeEach(async () => {
  calls = [];
  repl = await createHarnessRepl({ model: 'test-model', provider: 'openai', execution: 'inline', llmBackend: recordingBackend(calls) });
  await repl.session.beginTurn();
  await repl.session.define(
    'worker/1',
    'worker(Tag) :- system("sys-{Tag}"), user("marker-{Tag}"), task("Reply.", string(R)), output(R).',
  );
  await repl.session.endTurn();
});

afterEach(async () => {
  await repl.dispose();
});

describe('subagent LLM context', () => {
  it('gives each worker only its own conversation, never a sibling\'s', async () => {
    const a = await repl.session.spawnProcess('worker(a)', { name: 'a' });
    const b = await repl.session.spawnProcess('worker(b)', { name: 'b' });
    await waitForTerminal(a.id);
    await waitForTerminal(b.id);

    expect(calls.length).toBeGreaterThanOrEqual(2);
    for (const messages of calls) {
      const text = messages.join('\n');
      // No single model call may see both markers — that would be cross-pollution.
      expect(text.includes('marker-a') && text.includes('marker-b')).toBe(false);
      expect(text.includes('sys-a') && text.includes('sys-b')).toBe(false);
    }
    // Each marker was actually sent, to its own worker.
    expect(calls.some((messages) => messages.join('\n').includes('marker-a'))).toBe(true);
    expect(calls.some((messages) => messages.join('\n').includes('marker-b'))).toBe(true);
  });

  it('does not inherit the parent session\'s live memory', async () => {
    // Seed the parent's harness memory, then spawn a worker: the worker's prompt
    // must not contain the parent's `user/1` line.
    await repl.session.executeGoal('system("parent-only-system"), user("parent-only-user")', 1);
    const worker = await repl.session.spawnProcess('worker(child)', { name: 'child' });
    await waitForTerminal(worker.id);

    const workerCalls = calls.filter((messages) => messages.join('\n').includes('marker-child'));
    expect(workerCalls.length).toBeGreaterThan(0);
    for (const messages of workerCalls) {
      const text = messages.join('\n');
      expect(text).not.toContain('parent-only-system');
      expect(text).not.toContain('parent-only-user');
    }
  });
});
