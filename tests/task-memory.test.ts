/**
 * What a `task/N` / `prompt/N` sends as its conversation.
 *
 * `task/N` shares and *accumulates* the memory; `prompt/N` is a fresh, isolated
 * call with no memory and no tools, and does not modify the memory. The docs
 * described neither, and the distinction decides whether a step can see (and
 * grow) the conversation.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { plainValue } from '../src/repl/frames.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

/** Answers a `task/N` with a result and a finish, and a `prompt/N` with text. */
class Scripted implements LLMBackend {
  readonly seen: Array<Array<{ role: string; content: string }>> = [];
  /** Responses for task agent loops, consumed in order. */
  constructor(private readonly taskValues: string[]) {}
  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.seen.push(request.messages.map((m) => ({ role: m.role, content: String(m.content) })));
    const wantsResult = request.messages.some((m) => m.content.includes('set_result'));
    const hasResult = request.messages.some((m) => m.content.includes('"success":true'));
    if (!wantsResult || hasResult) return { text: 'plain text reply', finishReason: 'stop' };
    const value = this.taskValues.shift() ?? 'value';
    return {
      text: '',
      toolCalls: [{ id: `t${this.seen.length}`, name: 'set_result', arguments: { variable: 'A', value } }],
      finishReason: 'tool_use',
    };
  }
}

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-task-memory-'));
});

afterEach(async () => {
  await repl?.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

async function run(code: string, goal: string, values: string[]): Promise<{
  backend: Scripted;
  bindings: Record<string, unknown>;
}> {
  const backend = new Scripted(values);
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'm', provider: 'openai', llmBackend: backend, maxModelCalls: 8 });
  await repl.session.loadCode(code);
  const events: Array<{ type: string; bindings?: unknown }> = [];
  for await (const e of repl.query(goal)) events.push(e as never);
  const bindings = (events.find((e) => e.type === 'solution') as { bindings?: unknown })?.bindings;
  return { backend, bindings: (bindings ?? {}) as Record<string, unknown> };
}

describe('task/N memory', () => {
  it('grows the memory: a second task sees the first', async () => {
    const { backend } = await run(
      'go(N1, N2) :- get_memory(M1), length(M1, N1), task("FIRST", A), get_memory(M2), length(M2, N2).',
      'go(N1, N2)',
      ['one', 'two'],
    );
    // Every call after the first carries the earlier turns.
    expect(backend.seen.length).toBeGreaterThan(1);
    expect(backend.seen[1]!.length).toBeGreaterThan(backend.seen[0]!.length);
    expect(backend.seen[1]!.some((m) => m.content.includes('FIRST'))).toBe(true);
  });

  it('gives prompt/N no accumulated memory', async () => {
    // `system/1` and `user/1` land in the memory a task/N would see, and prompt/N
    // must not see them: it is a fresh, isolated call.
    const { backend } = await run(
      'go(A) :- system("SYS-MARKER"), user("USR-MARKER"), prompt("SECOND-PROMPT", A).',
      'go(_)',
      ['one'],
    );
    const promptCall = backend.seen.find((call) => call.some((m) => m.content.includes('SECOND-PROMPT')));
    expect(promptCall).toBeDefined();
    expect(promptCall!.some((m) => m.content.includes('SYS-MARKER'))).toBe(false);
    expect(promptCall!.some((m) => m.content.includes('USR-MARKER'))).toBe(false);
  });

  it('sends system/1 and user/1 to the next task', async () => {
    const { backend } = await run(
      'go(A) :- system("SYS-MARKER"), user("USR-MARKER"), task("FIRST", A).',
      'go(_)',
      ['one'],
    );
    const first = backend.seen[0]!;
    expect(first.some((m) => m.content.includes('SYS-MARKER'))).toBe(true);
    expect(first.some((m) => m.content.includes('USR-MARKER'))).toBe(true);
  });

  it('with_fresh_memory isolates a whole Goal, and does not leak back', async () => {
    const { backend } = await run(
      'go(A) :- system("SYS-MARKER"), with_fresh_memory(task("FRESH-TASK", A)).',
      'go(_)',
      ['one'],
    );
    const fresh = backend.seen.find((call) => call.some((m) => m.content.includes('FRESH-TASK')));
    expect(fresh).toBeDefined();
    expect(fresh!.some((m) => m.content.includes('SYS-MARKER'))).toBe(false);
  });

  it('discards what the fresh scope accumulates', async () => {
    const { backend } = await run(
      'go(N1, N2) :- system("SYS-MARKER"), get_memory(M1), length(M1, N1),'
      + ' with_fresh_memory((system("INNER"), task("FRESH-TASK", A))),'
      + ' get_memory(M2), length(M2, N2).',
      'go(N1, N2)',
      ['one'],
    );
    // The inner system/1 and the task's turns are discarded: the caller's memory
    // is exactly what it was before the scope.
    const fresh = backend.seen.find((call) => call.some((m) => m.content.includes('FRESH-TASK')));
    expect(fresh!.some((m) => m.content.includes('INNER'))).toBe(true);
    expect(fresh!.some((m) => m.content.includes('SYS-MARKER'))).toBe(false);
  });
});

describe('prompt/N output binding', () => {
  it('binds an output variable', async () => {
    // A regression guard: the descriptor list sent to the agent was left unbound on
    // the prompt path (only a Prolog singleton warning hinted at it), so the model
    // was told about no outputs. Binding the variable is the observable effect.
    //
    // Its own backend rather than the shared one: that one keys off the
    // "use set_result" nudge, which never appears for a prompt/N (it has no tools).
    class PromptMock implements LLMBackend {
      async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
        const answered = request.messages.some((m) => m.content.includes('"success":true'));
        if (answered) {
          return { text: '', toolCalls: [{ id: 'f', name: 'finish', arguments: { success: true } }], finishReason: 'tool_use' };
        }
        return {
          text: '',
          toolCalls: [{ id: 'r', name: 'set_result', arguments: { variable: 'A', value: 'hello-from-prompt' } }],
          finishReason: 'tool_use',
        };
      }
    }

    const backend = new PromptMock();
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'm',
      provider: 'openai',
      llmBackend: backend,
      maxModelCalls: 6,
    });
    await repl.session.loadCode('go(A) :- prompt("Say hi", A).');

    const events: Array<{ type: string; solution?: { bindings: Array<{ name: string; value: unknown }> } }> = [];
    for await (const e of repl.query('go(A)')) events.push(e as never);
    const bound = events
      .find((e) => e.type === 'solution')
      ?.solution?.bindings.find((b) => b.name === 'A')?.value;
    expect(String(plainValue(bound) ?? '')).toContain('hello-from-prompt');
  });
});
