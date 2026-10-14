/**
 * `:- require_sandbox.` — the harness declares it must be isolated, and the load
 * fails when the *effective* backend cannot provide that.
 *
 * "Effective" is the point: bwrap being installed while the session resolves to
 * clean-room means the code would run unsandboxed, which is exactly what the
 * directive exists to prevent. Availability would pass there and prove nothing.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { buildAgentSystemPrompt } from '../src/repl/agent.js';
import {
  describeShellExecutionBackend,
  type ExecutionBackendKind,
} from '../src/system/runtime/shell-manager.js';

let workspace: string;
const opened: HarnessRepl[] = [];

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-sandbox-'));
});

afterEach(async () => {
  for (const repl of opened.splice(0)) await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

async function open(executionBackend: ExecutionBackendKind): Promise<HarnessRepl> {
  const repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    executionBackend,
  });
  opened.push(repl);
  return repl;
}

const HARNESS = `:- require_sandbox.\n\nhello(world).\n`;

describe('require_sandbox', () => {
  it('refuses to load on an unsandboxed backend, naming the remedy', async () => {
    const repl = await open('clean-room');
    const result = await repl.session.loadCode(HARNESS);

    expect(result.error).toBeTruthy();
    expect(result.error).toContain('sandbox');
    // The message has to be actionable, not just a refusal.
    expect(result.error).toMatch(/bubblewrap|--sandbox|bwrap/i);
  });

  it.each<ExecutionBackendKind>(['bwrap', 'agentvm', 'sandbox-exec'])(
    'loads normally when the backend is %s',
    async (backend) => {
      const repl = await open(backend);
      const result = await repl.session.loadCode(HARNESS);
      expect(result.error).toBeUndefined();
      expect(repl.session.predicateList()).toContain('hello/1');
    },
  );

  it('is checked against the effective backend, not availability', async () => {
    // `wrapper: clean-room` is an explicit request for no sandbox. bwrap may be
    // installed; the directive must still refuse.
    const repl = await open('clean-room');
    const result = await repl.session.loadCode(HARNESS);
    expect(result.error).toBeTruthy();
  });

  it('cannot be added by the agent', async () => {
    const repl = await open('bwrap');
    await repl.session.beginTurn();
    // The agent's edit tools must not be able to declare (or lift) the
    // requirement — it is the user's, set through /set-require-sandbox.
    const result = await repl.session.assertMany(':- require_sandbox.');
    expect(result.ok).toBe(false);
    expect(String(result.error)).toMatch(/user|reserved/i);
  });
});

describe('the environment block', () => {
  it('names the backend, the workspace and the failure mode when unsandboxed', async () => {
    const clean = describeShellExecutionBackend(false, { wrapper: 'clean-room' });
    const prompt = await buildAgentSystemPrompt({
      workspacePath: '/tmp/proj',
      environment: clean,
      now: new Date('2026-02-14T13:31:00Z'),
    });

    expect(prompt).toContain('EXECUTION ENVIRONMENT');
    expect(prompt).toContain('host[clean-room]');
    expect(prompt).toContain('NO sandbox');
    expect(prompt).toContain('persist');
    expect(prompt).toContain('/tmp/proj');
    expect(prompt).toContain('/workspace');
    expect(prompt).toContain('$TMPDIR');
    // Time is present, with a weekday, and the model is told not to guess it.
    expect(prompt).toMatch(/NOW\n- \d{4}-\d{2}-\d{2} \d{2}:\d{2} \(/);
    expect(prompt).toContain('Do not assume');
  });

  it('says it is isolated when there is a sandbox', async () => {
    const sandboxed = describeShellExecutionBackend(false, { wrapper: 'bwrap' });
    const prompt = await buildAgentSystemPrompt({
      workspacePath: '/tmp/proj',
      environment: sandboxed,
      now: new Date('2026-02-14T13:31:00Z'),
    });
    expect(prompt).toContain('host[bwrap]');
    expect(prompt).toContain('isolated');
    expect(prompt).not.toContain('NO sandbox');
  });

  it('keeps the dynamic blocks last, so the stable prompt stays cacheable', async () => {
    const prompt = await buildAgentSystemPrompt({
      workspacePath: '/tmp/proj',
      environment: describeShellExecutionBackend(false, { wrapper: 'bwrap' }),
      now: new Date('2026-02-14T13:31:00Z'),
    });
    expect(prompt.indexOf('EXECUTION ENVIRONMENT')).toBeGreaterThan(prompt.indexOf('# DML Language Reference'));
    expect(prompt.lastIndexOf('NOW')).toBeGreaterThan(prompt.indexOf('EXECUTION ENVIRONMENT'));
  });
});
