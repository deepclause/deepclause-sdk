/**
 * The log view streams everything from a subagent: model text (`stream`),
 * reasoning, and tool calls, not just `output/1`. Streamed chunks coalesce into
 * one line per run.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

function streamingBackend(): LLMBackend {
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'Result';
      request.onText?.('Hello ');
      request.onText?.('from ');
      request.onText?.('the model');
      request.onReasoning?.('weighing ');
      request.onReasoning?.('the options');
      return {
        text: 'Hello from the model',
        toolCalls: [
          { id: 's', name: 'set_result', arguments: { variable, value: 'Hello from the model' } },
          { id: 'f', name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 3, outputTokens: 4, totalTokens: 7 },
      };
    },
  };
}

let workspace: string;
let repl: HarnessRepl;

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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-log-'));
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    execution: 'inline',
    llmBackend: streamingBackend(),
  });
  await repl.session.beginTurn();
  await repl.session.define('talk/0', 'talk :- task("Say hi in a few words.", string(X)), output(X).');
  await repl.session.endTurn();
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('subagent log streaming', () => {
  it('coalesces streamed model text into one line', async () => {
    const pid = await repl.session.spawnProcess('talk', { name: 'talker' });
    await waitForTerminal(pid.id);
    const streamed = repl.session.processLog().filter((entry) => entry.kind === 'stream');
    expect(streamed).toHaveLength(1);
    expect(streamed[0]?.text).toContain('Hello from the model');
  });

  it('records reasoning and tool calls', async () => {
    const pid = await repl.session.spawnProcess('talk', { name: 'talker' });
    await waitForTerminal(pid.id);
    const log = repl.session.processLog();
    expect(log.some((entry) => entry.kind === 'reasoning' && entry.text.includes('weighing'))).toBe(true);
    expect(log.some((entry) => entry.kind === 'tool' && entry.text.includes('set_result'))).toBe(true);
  });
});
