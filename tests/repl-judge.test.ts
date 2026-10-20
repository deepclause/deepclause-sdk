/**
 * Judgment backends in the REPL.
 *
 * The REPL builds its `DMLRunner` directly, so it must supply a
 * `JudgeSelection`; otherwise the documented `judge/2` family fails with
 * "Unknown judgment backend". These tests assert the wiring: the default `llm`
 * backend built from the injected `LLMBackend`, an explicit registry, and the
 * opt-in `jev` gating.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { DMLSession } from '../src/repl/session.js';
import { resolveReplJudgeSelection } from '../src/repl/judge.js';
import { createMockJudgeBackend } from '../src/judge/index.js';
import type {
  DMLEvent,
  LLMBackend,
  LLMBackendRequest,
  LLMBackendResponse,
} from '../src/types.js';

/** An LLMBackend that answers every request with a verify JSON payload. */
class JsonJudgeBackend implements LLMBackend {
  calls = 0;

  async complete(_request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls += 1;
    return { text: '{"answers":[{"value":"yes","probability":0.9}]}' };
  }
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

function outputs(events: DMLEvent[]): string[] {
  return events.filter((event) => event.type === 'output').map((event) => event.content);
}

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-repl-judge-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('repl judgment backends', () => {
  it('runs verify/3 through the default llm judge', async () => {
    const backend = new JsonJudgeBackend();
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
    });

    const events = await collect(repl.query('verify("the customer is angry", "Is this urgent?", T), output(T)'));

    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(outputs(events)).toContain('yes');
    expect(backend.calls).toBe(1);
  });

  it('gives direct DMLSession users a default llm judge', async () => {
    const backend = new JsonJudgeBackend();
    const session = await DMLSession.open({
      workspacePath: workspace,
      tools: new Map(),
      runnerOptions: {
        model: 'test-model',
        provider: 'openai',
        streaming: false,
        llmBackend: backend,
      },
    });
    try {
      const events: DMLEvent[] = [];
      for await (const event of session.query('verify("the customer is angry", "Is this urgent?", T), output(T)')) {
        events.push(event);
      }
      expect(events.some((event) => event.type === 'error')).toBe(false);
      expect(outputs(events)).toContain('yes');
      expect(backend.calls).toBe(1);
    } finally {
      await session.dispose();
    }
  });

  it('accepts an explicit judgeSelection registry', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      judgeSelection: {
        backends: new Map([
          [
            'mock',
            createMockJudgeBackend({
              answers: (request) => request.questions.map((question) => ({
                id: question.id,
                kind: question.kind,
                value: 'no',
                probability: 0.1,
                basis: 'mock' as const,
              })),
            }),
          ],
        ]),
        defaultName: 'mock',
      },
    });

    const events = await collect(repl.query('verify("state", "Is this urgent?", T), output(T)'));
    expect(outputs(events)).toContain('no');
  });

  it('gates jev on an API key and fails loudly for an unavailable default', () => {
    const missing = resolveReplJudgeSelection({
      model: 'm',
      provider: 'openai',
      jev: { enabled: true, apiKeyEnv: 'DC_TEST_MISSING_KEY' },
    });
    expect([...missing.selection.backends.keys()]).toEqual(['llm']);
    expect(missing.warnings.join(' ')).toContain('DC_TEST_MISSING_KEY');

    const withKey = resolveReplJudgeSelection({
      model: 'm',
      provider: 'openai',
      defaultJudge: 'jev',
      jev: { enabled: true, apiKey: 'secret' },
    });
    expect([...withKey.selection.backends.keys()]).toEqual(['llm', 'jev']);
    expect(withKey.selection.defaultName).toBe('jev');

    expect(() => resolveReplJudgeSelection({
      model: 'm',
      provider: 'openai',
      defaultJudge: 'jev',
    })).toThrow(/not available/);
  });
});
