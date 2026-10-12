/**
 * pi adapter: per-call model target and temperature handling.
 *
 * `with_model` switches models via `LLMBackendRequest.target`. Temperature is
 * only forwarded when a scope explicitly set one, and never for models whose
 * `compat.supportsTemperature` is false (pi itself never forwards an ambient
 * temperature, and some models — e.g. github-copilot — reject the parameter).
 */
import { describe, expect, it } from 'vitest';

import { createPiAdapter, type PiCompletionRequest, type PiModelLike, type PiRegistryLike } from '../src/repl/backend.js';

class FakeRegistry implements PiRegistryLike {
  readonly calls: Array<{ model: PiModelLike; options: Record<string, unknown> }> = [];
  constructor(private readonly models: PiModelLike[]) {}

  getAvailable(): PiModelLike[] {
    return this.models;
  }

  find(provider: string, modelId: string): PiModelLike | undefined {
    return this.models.find((model) => model.provider === provider && model.id === modelId);
  }

  hasConfiguredAuth(): boolean {
    return true;
  }

  async complete(model: PiModelLike, _context: PiCompletionRequest, options: unknown): Promise<never> {
    this.calls.push({ model, options: options as Record<string, unknown> });
    return {
      content: [{ type: 'text', text: 'ok' }],
      usage: { input: 1, output: 1, totalTokens: 2 },
      stopReason: 'stop',
    } as never;
  }
}

const request = { messages: [{ role: 'user' as const, content: 'hi' }] };

describe('pi adapter temperature', () => {
  it('ignores an ambient (session) temperature', async () => {
    const model: PiModelLike = { provider: 'github-copilot', id: 'gpt-5.4', reasoning: false };
    const registry = new FakeRegistry([model]);
    const { backend } = createPiAdapter(registry, model);
    await backend.complete({ ...request, temperature: 0.7 });
    expect(registry.calls.at(-1)?.options.temperature).toBeUndefined();
  });

  it('forwards a temperature set explicitly by a with_model scope', async () => {
    const model: PiModelLike = { provider: 'openai', id: 'gpt-4o', reasoning: false };
    const registry = new FakeRegistry([model]);
    const { backend } = createPiAdapter(registry, model);
    await backend.complete({
      ...request,
      temperature: 0.7,
      target: { provider: 'openai', id: 'gpt-4o', reasoning: false, temperature: 0.2 },
    });
    expect(registry.calls.at(-1)?.options.temperature).toBe(0.2);
  });

  it('never forwards temperature when the model rejects it', async () => {
    const model: PiModelLike = {
      provider: 'anthropic',
      id: 'claude-opus-4.7',
      reasoning: true,
      compat: { supportsTemperature: false },
    };
    const registry = new FakeRegistry([model]);
    const { backend } = createPiAdapter(registry, model);
    await backend.complete({
      ...request,
      target: { provider: 'anthropic', id: 'claude-opus-4.7', reasoning: true, temperature: 0.3 },
    });
    expect(registry.calls.at(-1)?.options.temperature).toBeUndefined();
  });

  it('switches to the target model', async () => {
    const base: PiModelLike = { provider: 'deepseek', id: 'deepseek-flash', reasoning: false };
    const other: PiModelLike = { provider: 'deepseek', id: 'deepseek-v4-pro', reasoning: true };
    const registry = new FakeRegistry([base, other]);
    const { backend } = createPiAdapter(registry, base);
    await backend.complete({
      ...request,
      target: { provider: 'deepseek', id: 'deepseek-v4-pro', reasoning: true },
    });
    expect(registry.calls.at(-1)?.model.id).toBe('deepseek-v4-pro');
  });
});
