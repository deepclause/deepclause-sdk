/**
 * A tool withheld to break a call cycle is still declared to the nested agent,
 * with an implementation that refuses and explains why.
 *
 * Dropping it from the tool list entirely made a call to it come back as
 * "Tool not found", which reads as a broken harness: in a live session the
 * nested agent retried the same call, then apologised and did the work by hand
 * while reporting the tool as failing.
 */
import { afterAll, beforeAll, describe, expect, it } from 'vitest';

import { loadProlog, type SWIPLModule } from '../src/prolog/loader.js';
import { DMLRunner } from '../src/runner.js';

let module_: SWIPLModule;

beforeAll(async () => {
  module_ = await loadProlog();
});

afterAll(async () => {
  await module_?.prolog.halt?.();
});

interface BuiltTool {
  description?: string;
  parameters: Record<string, unknown>;
  execute: (args: Record<string, unknown>) => Promise<unknown>;
}

/**
 * `buildAgentTools` is private; call it through a narrow view. It receives the
 * full tool list plus the withheld names, and decides what the model sees.
 */
function buildTools(
  runner: DMLRunner,
  tools: Array<{ name: string; description: string; inputs: Array<{ name: string; type: string }> }>,
  excluded: string[],
): Map<string, BuiltTool> {
  const view = runner as unknown as {
    buildAgentTools: (
      tools: unknown[],
      policy: null,
      exec: (name: string, args: Record<string, unknown>) => Promise<{ result: unknown; outputs: string[] }>,
      excluded: string[],
    ) => Map<string, BuiltTool>;
  };
  return view.buildAgentTools(tools, null, async () => ({ result: { ok: true }, outputs: [] }), excluded);
}

function newRunner(): DMLRunner {
  return new DMLRunner(module_, { model: 'test-model', provider: 'openai', temperature: 0, maxTokens: 1024 });
}

const authorsTool = {
  name: 'extract_authors',
  description: 'Extract authors and affiliations.',
  inputs: [{ name: 'arg1', type: 'string' }],
};

describe('withheld tools in a nested task', () => {
  it('declares a withheld tool and marks it unavailable', () => {
    const tools = buildTools(newRunner(), [authorsTool], ['extract_authors']);
    const tool = tools.get('extract_authors');
    expect(tool).toBeDefined();
    expect(tool?.description).toContain('unavailable in this task');
  });

  it('refuses a withheld tool with the reason, not "not found"', async () => {
    const tools = buildTools(newRunner(), [authorsTool], ['extract_authors']);
    const result = (await tools.get('extract_authors')!.execute({ arg1: 'text' })) as { error?: string };
    expect(result.error).toContain('already running further up the call chain');
    expect(result.error).toContain('Do not call it again');
    expect(result.error).not.toContain('Tool not found');
  });

  it('keeps a withheld tool out of the executable set', async () => {
    // The stub must never reach the real implementation.
    let called = false;
    const view = newRunner() as unknown as {
      buildAgentTools: (
        tools: unknown[],
        policy: null,
        exec: (name: string, args: Record<string, unknown>) => Promise<{ result: unknown; outputs: string[] }>,
        excluded: string[],
      ) => Map<string, BuiltTool>;
    };
    const tools = view.buildAgentTools([authorsTool], null, async () => {
      called = true;
      return { result: { ok: true }, outputs: [] };
    }, ['extract_authors']);
    await tools.get('extract_authors')!.execute({ arg1: 'text' });
    expect(called).toBe(false);
  });

  it('leaves tools that are not withheld runnable', async () => {
    const tools = buildTools(newRunner(), [authorsTool], []);
    expect(tools.get('extract_authors')?.description).not.toContain('unavailable');
    expect(await tools.get('extract_authors')!.execute({ arg1: 'x' })).toEqual({ ok: true });
  });
});
