/**
 * Tool cancellation: the turn's abort signal must reach `tool.execute` so a
 * running `bash` (or any long tool) is terminated on Ctrl-C.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { createReplToolset } from '../src/repl/toolset.js';
import type {
  DMLEvent,
  LLMBackend,
  LLMBackendRequest,
  LLMBackendResponse,
  ToolDefinition,
} from '../src/types.js';

class NoopBackend implements LLMBackend {
  async complete(_request: LLMBackendRequest): Promise<LLMBackendResponse> {
    return { text: '' };
  }
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-tool-signal-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('tool cancellation', () => {
  it('passes the turn abort signal to tool.execute', async () => {
    let seen: AbortSignal | undefined;
    const capture: ToolDefinition = {
      description: 'Capture the abort signal it was called with.',
      parameters: { type: 'object', properties: { x: { type: 'number' } } },
      execute: async (_args, context) => {
        seen = context?.signal;
        return { ok: true };
      },
    };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: new NoopBackend(),
      tools: new Map([['capture', capture]]),
    });
    await repl.session.loadCode('run :- exec(capture(x: 1), R), output(R).');

    const controller = new AbortController();
    await collect(repl.query('run', { signal: controller.signal }));
    // The host also binds session lifetime; identity need not be the original
    // signal, but cancellation and its reason must still propagate.
    expect(seen).toBeDefined();
    expect(seen!.aborted).toBe(false);
    controller.abort('stop');
    expect(seen!.aborted).toBe(true);
    expect(seen!.reason).toBe('stop');
  });

  it('the bash tool kills its child when the per-call signal aborts', async () => {
    const toolset = await createReplToolset({ workspacePath: workspace, toolNames: ['bash'] });
    try {
      const bash = toolset.tools.get('bash');
      expect(bash).toBeDefined();
      const controller = new AbortController();
      const started = Date.now();
      const pending = bash!.execute({ command: 'sleep 30' }, { signal: controller.signal });
      setTimeout(() => controller.abort(), 150);

      await expect(pending).rejects.toMatchObject({ name: 'AbortError' });
      expect(Date.now() - started).toBeLessThan(5000);
    } finally {
      await toolset.dispose();
    }
  });
});
