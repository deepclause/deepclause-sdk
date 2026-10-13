/**
 * A tool whose body calls `task/N` must not be offered to the agent that `task/N`
 * spawns: that agent would call it again and recurse.
 *
 * The nested agent is still *told* about it (so a call can be answered with the
 * reason) but its implementation refuses. This drives the real path: a DML tool
 * whose body calls task/N, with a scripted model that tries to call the tool.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

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

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-cycle-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

const HARNESS = [
  'extract_authors(A, B) :- task("Extract the authors from this text: {A}", B).',
  'tool(extract_authors(Text, Authors), "Extract authors and affiliations.") :- extract_authors(Text, Authors).',
  'get_result(_) :- extract_authors("Attention Is All You Need", X), output(X).',
  '',
].join('\n');

describe('a tool that calls task/N', () => {
  // The list handed to the agent inside `task/N` still contains the tool (that is
  // by design: a withheld tool is declared so a call can be answered with the
  // reason). What must not happen is the tool *running*, which the dispatch-time
  // guard prevents — covered directly in tests/agent-withheld-tools.test.ts.
  it('does not run the tool when the agent inside task/N calls it', async () => {
    const backend = new ScriptedBackend([
      // The agent inside task/N tries the very tool that is running.
      {
        text: 'using the tool',
        toolCalls: [{ id: '1', name: 'extract_authors', arguments: { arg1: 'Attention Is All You Need' } }],
        finishReason: 'tool_use',
      },
      { text: 'done', finishReason: 'stop' },
    ]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      streaming: true,
      llmBackend: backend,
      authoring: 'agent',
    });

    const loaded = await repl.session.loadCode(HARNESS);
    expect(loaded.error).toBeUndefined();
    const events = await collect(repl.query('get_result(done)'));


    // The running tool must not have been re-entered: no second nested agent loop
    // for it. A recursion would show up as another task_activity 'started'.
    const tasks = events.filter((e) => e.type === 'task_activity' && e.taskState === 'started');
    expect(tasks.length).toBe(1);

    // It is still declared to the agent inside task/N, so a call can be answered
    // with the reason rather than looking like a missing tool.
    const nestedRequest = backend.calls[1];
    const declared = (nestedRequest?.tools ?? []).find((tool) => tool.name === 'extract_authors');
    expect(declared).toBeDefined();
  });
});
