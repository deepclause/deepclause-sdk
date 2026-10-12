/**
 * `with_model/2`: a scoped model override for task/prompt/sample_token.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { parseModelSpec, type ModelResolver } from '../src/repl/model-resolver.js';
import { buildAgentSystemPrompt } from '../src/repl/agent.js';
import type { DMLEvent, LLMBackend, LLMBackendRequest, ModelSpec, ResolvedModel } from '../src/types.js';

class RecordingBackend implements LLMBackend {
  readonly targets: Array<ResolvedModel | undefined> = [];
  readonly models: Array<string | undefined> = [];
  async complete(request: LLMBackendRequest): Promise<{ text: string; finishReason?: 'stop' }> {
    this.targets.push(request.target);
    this.models.push(request.target?.id);
    return { text: 'ok', finishReason: 'stop' };
  }
}

class CannedResolver implements ModelResolver {
  readonly specs: ModelSpec[] = [];
  async resolve(spec: ModelSpec): Promise<ResolvedModel> {
    this.specs.push(spec);
    const id = typeof spec === 'string' ? 'mini' : spec.id;
    return { provider: 'acme', id, reasoning: false };
  }
  catalog(): Array<{ model: string; inputCost?: number; outputCost?: number }> {
    return [{ model: 'acme:mini', inputCost: 0.1, outputCost: 0.3 }];
  }
  describe(): string {
    return 'acme:mini';
  }
}

let workspace: string;
let repl: HarnessRepl;
let backend: RecordingBackend;
let resolver: CannedResolver;

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-with-model-'));
  backend = new RecordingBackend();
  resolver = new CannedResolver();
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'session-model',
    provider: 'openai',
    llmBackend: backend,
    modelResolver: resolver,
  });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('parseModelSpec', () => {
  it('parses string forms with an optional reasoning suffix', () => {
    expect(parseModelSpec('none')).toBe('default');
    expect(parseModelSpec('anthropic:claude')).toEqual({ provider: 'anthropic', id: 'claude' });
    expect(parseModelSpec('openrouter:google/gemini')).toEqual({ provider: 'openrouter', id: 'google/gemini' });
    expect(parseModelSpec('gpt-4o')).toEqual({ id: 'gpt-4o' });
    expect(parseModelSpec('openai:gpt-4o#high')).toEqual({ provider: 'openai', id: 'gpt-4o', reasoning: 'high' });
    expect(parseModelSpec('gpt-4o#low')).toEqual({ id: 'gpt-4o', reasoning: 'low' });
  });

  it('parses the dict form', () => {
    expect(parseModelSpec({ provider: 'google', id: 'gemini', temperature: 0.2, maxTokens: 100, reasoning: 'high' }))
      .toEqual({ provider: 'google', id: 'gemini', temperature: 0.2, maxTokens: 100, reasoning: 'high' });
  });
});

describe('model visibility', () => {
  it('lists the session models with prices via listModels()', async () => {
    const catalog = await repl.session.listModels();
    expect(catalog).toContain('current model: openai:session-model');
    expect(catalog).toContain('acme:mini');
    expect(catalog).toContain('$0.1/$0.3 per 1M');
  });

  it('injects the catalog and prices into the agent system prompt', async () => {
    const prompt = await buildAgentSystemPrompt({
      currentModel: 'openai:gpt-4o',
      modelCatalog: [
        { model: 'acme:mini', inputCost: 0.1, outputCost: 0.3 },
        { model: 'other:big', inputCost: 1, outputCost: 4 },
      ],
    });
    expect(prompt).toContain('AVAILABLE MODELS');
    expect(prompt).toContain('openai:gpt-4o');
    expect(prompt).toContain('acme:mini ($0.1/$0.3 per 1M)');
    expect(prompt).toContain('other:big ($1/$4 per 1M)');
  });
});

describe('with_model/2', () => {
  it('applies the model only to calls inside the scope', async () => {
    await collect(repl.query('with_model("acme:mini", llm("inside", X))'));
    expect(resolver.specs).toEqual([{ provider: 'acme', id: 'mini' }]);
    expect(backend.targets.at(-1)).toMatchObject({ provider: 'acme', id: 'mini' });
  });

  it('does not affect calls outside the scope', async () => {
    await collect(repl.query('llm("outside", X)'));
    expect(resolver.specs).toEqual([]);
    expect(backend.targets).toEqual([undefined]);
  });

  it('carries a reasoning suffix to the resolved target', async () => {
    await collect(repl.query('with_model("acme:mini#high", llm("inside", X))'));
    expect(resolver.specs).toEqual([{ provider: 'acme', id: 'mini', reasoning: 'high' }]);
  });

  it('accepts the dict form', async () => {
    await collect(repl.query('with_model(model{provider: "acme", id: "mini", temperature: 0.1}, llm("inside", X))'));
    expect(resolver.specs).toEqual([{ provider: 'acme', id: 'mini', temperature: 0.1 }]);
    expect(backend.targets.at(-1)).toMatchObject({ provider: 'acme', id: 'mini' });
  });

  it('clears the scope after the goal', async () => {
    await collect(repl.query('with_model("acme:mini", llm("inside", A)), llm("after", B)'));
    expect(backend.targets).toHaveLength(2);
    expect(backend.targets[0]).toMatchObject({ provider: 'acme' });
    expect(backend.targets[1]).toBeUndefined();
  });

  it('scopes task/N (agentic) calls too', async () => {
    // A task turn: first call sets the result, second finishes.
    let calls = 0;
    const taskBackend: LLMBackend = {
      complete: async (request) => {
        backend.targets.push(request.target);
        calls += 1;
        if (calls === 1) {
          return {
            text: '',
            finishReason: 'tool_use',
            toolCalls: [{ id: '1', name: 'set_result', arguments: { variable: 'Answer', value: 'hi' } }],
          };
        }
        return { text: 'done', finishReason: 'stop' };
      },
    };
    const taskRepl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'session-model',
      provider: 'openai',
      llmBackend: taskBackend,
      modelResolver: resolver,
    });
    try {
      await collect(taskRepl.query('with_model("acme:mini", task("say hi", string(Answer)))'));
      expect(backend.targets.some((target) => target?.provider === 'acme')).toBe(true);
    } finally {
      await taskRepl.dispose();
    }
  });
});
