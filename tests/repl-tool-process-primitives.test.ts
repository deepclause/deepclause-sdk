/**
 * A DML tool that uses a process primitive (`self/1`, `send/2`, `receive/2`, …).
 *
 * Tools run in an isolated tool engine, and that engine's result handler did not
 * know `request_proc`: a tool calling `self/1` came back as
 * "Unknown tool engine result: request_proc(self)" — the failure mode seen when a
 * worker's tool tried to route work to another worker.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

let workspace: string;
let repl: HarnessRepl | undefined;
let calls: LLMBackendRequest[];

/** Answers a `task/N` by calling `toolName`, then finishing once it has a result. */
function toolCallingBackend(toolName: string): LLMBackend {
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      calls.push(request);
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'R';
      const sawToolResult = request.messages.some((m) => m.role === 'tool');
      if (!sawToolResult) {
        return {
          text: '',
          toolCalls: [{ id: `call-${toolName}`, name: toolName, arguments: {} }],
          usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
        };
      }
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

function toolResults(): string {
  return calls
    .flatMap((call) => call.messages)
    .filter((message) => message.role === 'tool')
    .map((message) => message.content)
    .join('\n');
}

async function waitForDone(id: string, timeoutMs = 15_000): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const info = repl?.session.processes().find((process) => process.pid.id === id);
    if (info && ['done', 'crashed', 'killed'].includes(info.state)) return;
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  throw new Error('process did not finish');
}

/** A REPL whose model calls exactly one DML tool, plus the harness that offers it. */
async function open(toolName: string): Promise<HarnessRepl> {
  calls = [];
  const harness = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    execution: 'inline',
    llmBackend: toolCallingBackend(toolName),
  });
  await harness.session.beginTurn();
  await harness.session.assertMany([
    // A tool that reads its own process identity.
    'whoami(Pid) :- self(Pid).',
    'tool(whoami(Pid), "Report the pid of the process running this tool.") :- whoami(Pid).',
    'probe :- task("Call the whoami tool once, then stop.", string(R)), output(R).',
    // A tool that routes a request to another process and awaits its reply.
    'echo_svc(S) :- receive(Msg), (Msg = request(From, Ref, Value) -> send(From, reply(Ref, Value)), echo_svc(S) ; echo_svc(S)).',
    'start_echo_svc :- serve(echo, echo_svc(ok)).',
    'ask_echo(Reply) :- service_call(echo, hello, Reply).',
    'tool(ask_echo(Reply), "Ask the echo worker and return its reply.") :- ask_echo(Reply).',
    'probe_echo :- task("Call the ask_echo tool once, then stop.", string(R)), output(R).',
  ].join('\n'));
  await harness.session.endTurn();
  repl = harness;
  return harness;
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-tool-proc-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('a DML tool that uses process primitives', () => {
  it('resolves self/1 in the foreground instead of failing the tool engine', async () => {
    const harness = await open('whoami');
    await harness.session.executeGoal('probe', 1);

    const results = toolResults();
    expect(results).not.toContain('Unknown tool engine result');
    expect(results).toMatch(/"functor":"proc"/);
    expect(results).toContain('"main"');
  });

  it('resolves self/1 to the process the tool runs inside', async () => {
    const harness = await open('whoami');
    const pid = await harness.session.spawnProcess('probe', { name: 'probe' });
    await waitForDone(pid.id);

    const results = toolResults();
    expect(results).not.toContain('Unknown tool engine result');
    expect(results).toContain(pid.id);
  });

  it('routes to another process from inside a tool and returns its reply', async () => {
    const harness = await open('ask_echo');
    await harness.session.executeGoal('start_echo_svc', 1);
    await new Promise((resolve) => setTimeout(resolve, 250));

    const pid = await harness.session.spawnProcess('probe_echo', { name: 'router' });
    await waitForDone(pid.id);

    const results = toolResults();
    expect(results).not.toContain('Unknown tool engine result');
    expect(results).toContain('hello');
  });
});
