/**
 * Tool-call parameter validation.
 *
 * A call naming a parameter the tool does not have used to be accepted with the
 * extra key silently dropped, so this looked like it worked and returned an
 * empty string:
 *
 *     exec(bash(command: "cat", stdin: "hello-from-stdin"), R), get_dict(stdout, R, O)
 *
 * `stdin` is not a bash parameter — data goes in an `argv` element or through a
 * workspace file — and the call is now rejected by name, at the point it is made.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { createReplToolset } from '../src/repl/toolset.js';
import { validateToolArguments } from '../src/repl/tool-arguments.js';

describe('validateToolArguments', () => {
  const BASH = {
    type: 'object',
    properties: {
      command: { type: 'string' },
      argv: { type: 'array' },
      timeoutMs: { type: 'integer' },
    },
  };

  it('accepts a well-formed call', () => {
    expect(validateToolArguments('bash', BASH, { command: 'ls' }).ok).toBe(true);
    expect(validateToolArguments('bash', BASH, { argv: ['ls', '-l'] }).ok).toBe(true);
    expect(validateToolArguments('bash', BASH, {}).ok).toBe(true);
  });

  it('rejects an unknown parameter, naming the ones that exist', () => {
    const result = validateToolArguments('bash', BASH, { command: 'cat', stdin: 'hi' });
    expect(result.ok).toBe(false);
    if (result.ok) throw new Error('unreachable');
    expect(result.error).toContain('stdin');
    expect(result.error).toContain('command');
    expect(result.error).toContain('argv');
  });

  it('explains the trap rather than only naming it', () => {
    const result = validateToolArguments('bash', BASH, { command: 'cat', stdin: 'hi' });
    if (result.ok) throw new Error('unreachable');
    expect(result.error).toMatch(/no stdin/i);
    expect(result.error).toMatch(/argv|workspace file/i);
  });

  it('reports missing required parameters', () => {
    const schema = { type: 'object', properties: { a: { type: 'string' } }, required: ['a'] };
    const result = validateToolArguments('t', schema, {});
    expect(result.ok).toBe(false);
    if (result.ok) throw new Error('unreachable');
    expect(result.error).toContain('missing required');
    expect(result.error).toContain('a');
  });

  it('reports a type mismatch', () => {
    const result = validateToolArguments('bash', BASH, { command: 42 });
    expect(result.ok).toBe(false);
    if (result.ok) throw new Error('unreachable');
    expect(result.error).toContain('command');
    expect(result.error).toContain('string');
    expect(result.error).toContain('number');
  });

  it('stays out of the way when there is nothing to check', () => {
    // A tool with no declared properties may be called positionally; a Zod
    // schema is enforced by the AI SDK; `additionalProperties: true` is explicit.
    expect(validateToolArguments('t', { type: 'object', properties: {} }, { anything: 1 }).ok).toBe(true);
    expect(validateToolArguments('t', undefined, { anything: 1 }).ok).toBe(true);
    expect(validateToolArguments('t', { type: 'object', properties: {} }, {}).ok).toBe(true);
  });

  it('does not mistake Prolog markers for parameters', () => {
    expect(validateToolArguments('bash', BASH, { command: 'ls', $tag: 'x' }).ok).toBe(true);
  });
});

describe('a bad tool call never reaches the shell', () => {
  let workspace: string;
  let repl: HarnessRepl | undefined;
  let toolset: Awaited<ReturnType<typeof createReplToolset>> | undefined;

  beforeEach(async () => {
    workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-args-'));
  });

  afterEach(async () => {
    await repl?.dispose();
    await toolset?.dispose();
    repl = undefined;
    toolset = undefined;
    await fs.rm(workspace, { recursive: true, force: true });
  });

  it('rejects `stdin` on the real bash tool, and does not run the command', async () => {
    toolset = await createReplToolset({
      workspacePath: workspace,
      hostConfig: { wrapper: 'clean-room' },
    });
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      tools: toolset.tools,
      executionBackend: toolset.environment.kind,
    });

    const events: Array<Record<string, unknown>> = [];
    // `cat` with no input would also have hung before stdin was closed; the point
    // here is that it is refused before any process starts.
    for await (const event of repl.query('exec(bash(command: "echo MARKER"), R), write(R)')) {
      events.push(event as Record<string, unknown>);
    }
    // Sanity: the same call without the bad parameter runs.
    const ran = events.some((event) => JSON.stringify(event).includes('MARKER'));
    expect(ran).toBe(true);

    const bad: Array<Record<string, unknown>> = [];
    for await (const event of repl.query('exec(bash(command: "cat", stdin: "hello-from-stdin"), R), write(R)')) {
      bad.push(event as Record<string, unknown>);
    }

    // The refusal is rendered as a tool failure (the same shape a non-zero exit
    // uses), not a bare log — a log event is not rendered in goal mode, which is
    // how an unknown tool used to fail with no explanation at all.
    const messages = bad.map((event) => JSON.stringify(event)).join('\n');
    expect(messages).toContain('stdin');
    expect(messages).toMatch(/unknown parameter/i);
    expect(messages).toMatch(/no stdin/i);
    expect(bad.some((event) => event.type === 'tool_call')).toBe(true);
  }, 30000);
});
