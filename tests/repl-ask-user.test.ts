/**
 * M6 — ask_user at both levels, and the non-interactive input policy.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { buildCliOnInput } from '../src/cli/repl.js';
import type {
  DMLEvent,
  LLMBackend,
  LLMBackendRequest,
  LLMBackendResponse,
} from '../src/types.js';

interface CannedResponse {
  text?: string;
  toolCalls?: Array<{ name: string; arguments: Record<string, unknown> }>;
}

class CannedBackend implements LLMBackend {
  readonly calls: LLMBackendRequest[] = [];

  constructor(private readonly script: CannedResponse[] | ((request: LLMBackendRequest, index: number) => CannedResponse)) {}

  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls.push(request);
    const index = this.calls.length - 1;
    const response = typeof this.script === 'function'
      ? this.script(request, index)
      : this.script[Math.min(index, this.script.length - 1)];
    return {
      text: response.text ?? '',
      toolCalls: (response.toolCalls ?? []).map((call, callIndex) => ({
        id: `${index}-${callIndex}`,
        name: call.name,
        arguments: call.arguments,
      })),
    };
  }
}

let workspace: string;
let repl: HarnessRepl | null = null;

function defineCall(clauses: string): { name: string; arguments: Record<string, unknown> } {
  const first = clauses
    .split(/\r?\n/)
    .map((line) => line.trim())
    .find((line) => line && !line.startsWith(':-') && !line.startsWith('%'))
    ?.replace(/\s*\.\s*$/, '')
    .replace(/\s*:-.*$/, '') ?? 'defined_predicate';
  return { name: 'define', arguments: { head: first, clauses } };
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

function values(events: DMLEvent[], name: string): string[] {
  return events
    .filter((event) => event.type === 'solution')
    .map((event) => event.solution?.bindings.find((binding) => binding.name === name)?.value ?? '');
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-repl-ask-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = null;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('harness input/2', () => {
  it('routes through the same onInput callback', async () => {
    const asked: string[] = [];
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      onInput: async (prompt) => {
        asked.push(prompt);
        return 'yes';
      },
    });

    const events = await collect(repl.query('input("Include charts?", Answer)'));
    expect(asked).toEqual(['Include charts?']);
    expect(values(events, 'Answer')).toEqual(['"yes"']);
  });

  it('routes input/2 through a replacement handler (setOnInput)', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
    });
    repl.setOnInput(async () => 'replaced');

    const events = await collect(repl.query('input("Anything?", Answer)'));
    expect(values(events, 'Answer')).toEqual(['"replaced"']);
  });

  it('fails input/2 (without hanging) when no channel is configured', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
    });

    const events = await collect(repl.query('input("Anyone there?", Answer)'));
    expect(events.some((event) => event.type === 'solution')).toBe(false);
    expect(['finished', 'error']).toContain(events.at(-1)?.type);
  });

  it('does not deadlock when input/2 runs inside an execute child frame', async () => {
    const asked: string[] = [];
    const backend = new CannedBackend([{
      text: '',
      toolCalls: [
        { name: 'execute', arguments: { goal: 'input("Nested question?", A), output(A)' } },
        { name: 'begin_turn', arguments: {} },
        defineCall('kept(1).'),
        { name: 'end_turn', arguments: {} },
        { name: 'finish', arguments: { success: true } },
      ],
    }]);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
      onInput: async (prompt) => {
        asked.push(prompt);
        return 'nested-ok';
      },
    });

    const events = await collect(repl.prompt('ask during execute'));
    expect(asked).toEqual(['Nested question?']);
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(values(await collect(repl.query('kept(X)')), 'X')).toEqual(['1']);
  });
});

describe('capability policy', () => {
  it('allows exec(ask_user(...)) under a strict whitelist', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      toolPolicy: { mode: 'whitelist', tools: ['bash'] },
      onInput: async () => 'allowed',
    });

    const events = await collect(repl.query(
      'exec(ask_user(prompt: "Proceed?"), R), get_dict(user_response, R, A), output(A)',
    ));
    expect(events.some((event) => event.type === 'output' && event.content === 'allowed')).toBe(true);
  });
});

describe('CLI input policy', () => {
  it('--answer supplies a canned reply', async () => {
    const handler = buildCliOnInput({ answer: '42' });
    expect(handler).toBeDefined();
    expect(await handler!('anything')).toBe('42');
  });

  it('--no-ask fails fast without prompting', async () => {
    const handler = buildCliOnInput({ noAsk: true });
    expect(handler).toBeDefined();
    expect(await handler!('anything')).toBeUndefined();
  });
});
