/**
 * `sdk-examples/subagents.dml` is the public tour of the concurrency features.
 * Load it and run examples with a scripted model, so the file cannot rot: a
 * renamed primitive, a broken tool scope, or a changed process semantic fails
 * here rather than in a reader's session.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as path from 'path';
import { fileURLToPath } from 'url';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

const examplePath = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  '../sdk-examples/subagents.dml',
);

let repl: HarnessRepl | undefined;
let roles: string[] = [];

/** Route the tour's `task/N` calls the way a cooperative model would. */
function scriptedBackend(): LLMBackend {
  const done = (request: LLMBackendRequest): string[] =>
    request.messages.filter((message) => message.role === 'tool').map((message) => message.content);
  const variable = (request: LLMBackendRequest): string =>
    /Variable must be one of: "([^"]+)"/.exec(
      request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n'),
    )?.[1] ?? 'R';
  const result = (v: string, value: string): LLMBackendResponse => ({
    text: '',
    toolCalls: [
      { id: 'set', name: 'set_result', arguments: { variable: v, value } },
      { id: 'finish', name: 'finish', arguments: { success: true } },
    ],
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  });
  const call = (name: string, args: Record<string, unknown>): LLMBackendResponse => ({
    text: '',
    toolCalls: [{ id: 'call', name, arguments: args }],
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  });

  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const user = request.messages.filter((m) => m.role === 'user').map((m) => m.content).join('\n');
      const text = `${system}\n${user}`;
      const v = variable(request);
      const toolDone = done(request);
      const customer = /Customer message: ([\s\S]*)$/.exec(user)?.[1]?.trim() ?? '';

      // Support desk (section 4): route by role, echo the tool's reply back.
      const role = /You are the front desk/.test(user) ? 'frontdesk'
        : /You are delivery/.test(user) ? 'delivery'
        : /You are the warehouse/.test(user) ? 'warehouse'
        : /You are billing/.test(user) ? 'billing'
        : undefined;
      if (role) {
        roles.push(role);
        if (toolDone.length) {
          const last = JSON.parse(toolDone[toolDone.length - 1]);
          return result(v, typeof last === 'string' ? last : JSON.stringify(last));
        }
        if (role === 'frontdesk') return call('to_delivery', { arg1: customer });
        if (role === 'delivery') return call('to_warehouse', { arg1: customer });
        if (role === 'warehouse') return result(v, 'In stock; ships in 2 business days.');
        return result(v, 'Refund issued.');
      }

      const topic = /Summarise (.+?) in two sentences/.exec(text)?.[1];
      if (topic) return result(v, `brief:${topic}`);
      if (/Attempt \d+/.test(text)) return result(v, 'attempt');
      if (/Candidate answers:/.test(text)) return result(v, 'judge:best');
      const term = /Define (.+?) in one sentence/.exec(text)?.[1];
      if (term) return result(v, `def:${term}`);
      const style = /Style: (\w+)/.exec(text)?.[1];
      if (style) return result(v, `winner:${style}`);
      return result(v, 'ok');
    },
  };
}

const value = (bindings: Array<{ name: string; value: string }>, name: string): string =>
  bindings.find((binding) => binding.name === name)?.value ?? '';

beforeEach(async () => {
  roles = [];
  repl = await createHarnessRepl({
    workspacePath: path.dirname(examplePath),
    model: 'test-model',
    provider: 'openai',
    execution: 'inline',
    llmBackend: scriptedBackend(),
  });
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
});

describe('sdk-examples/subagents.dml', () => {
  it('loads with no error', async () => {
    const loaded = await repl!.session.load(examplePath);
    expect(loaded.error).toBeUndefined();
  });

  it('swarm fan-out returns results in input order (survey/2)', async () => {
    await repl!.session.load(examplePath);
    const r = await repl!.session.executeGoal('survey([alpha, beta, gamma], Briefs)', 1);
    const briefs = value(r.bindings, 'Briefs');
    expect(briefs).toContain('brief:alpha');
    expect(briefs.indexOf('alpha')).toBeLessThan(briefs.indexOf('beta'));
    expect(briefs.indexOf('beta')).toBeLessThan(briefs.indexOf('gamma'));
  });

  it('routes a customer question through three processes via tools (section 4)', async () => {
    await repl!.session.load(examplePath);
    await repl!.session.executeGoal('ops_start', 1);
    await new Promise((resolve) => setTimeout(resolve, 300));
    const r = await repl!.session.executeGoal('ask_frontdesk("Where is my order #4471?", Reply)', 1);

    expect(value(r.bindings, 'Reply')).toContain('In stock');
    // frontdesk -> to_delivery -> delivery -> to_warehouse -> warehouse -> back.
    expect(roles).toEqual(['frontdesk', 'delivery', 'warehouse', 'delivery', 'frontdesk']);
  });

  it('solves the CLP portfolio in subagents (queens_portfolio/1)', async () => {
    await repl!.session.load(examplePath);
    const r = await repl!.session.executeGoal('queens_portfolio(Rows), length(Rows, Count)', 1);
    expect(value(r.bindings, 'Count')).toBe('6');
  }, 30_000);
});
