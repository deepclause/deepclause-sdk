/**
 * The `bindings` option on the `execute` tool.
 *
 * A goal commonly binds one large term — a document, a nested dict, a whole
 * table — and returning it in full is noise in the transcript and in the model's
 * context. `brief` keeps the names and shortens the values; `none` returns no
 * bindings at all. Both say what they left out, because a shortened value is
 * indistinguishable from a short one, and an omitted binding looks like a goal
 * that bound nothing.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { BRIEF_VALUE_CHARS, createHarnessTools, readBindingMode } from '../src/repl/harness-tools.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-bindings-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
  await repl.session.beginTurn();
  await repl.session.define('big/1', `big("${'x'.repeat(400)}").`);
  await repl.session.endTurn();
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('readBindingMode', () => {
  it('defaults to full for anything unrecognised', () => {
    expect(readBindingMode(undefined)).toBe('full');
    expect(readBindingMode('full')).toBe('full');
    expect(readBindingMode('everything')).toBe('full');
    expect(readBindingMode(42)).toBe('full');
    expect(readBindingMode('brief')).toBe('brief');
    expect(readBindingMode('none')).toBe('none');
  });
});

describe('execute bindings', () => {
  it('returns the whole value by default', async () => {
    const result = await repl.session.execute('big(V)');
    expect(result.bindings).toHaveLength(1);
    // The value is rendered Prolog text, so it carries the quotes; what matters
    // is that the whole thing is there.
    expect(String(result.bindings[0]?.value)).toContain('x'.repeat(400));
    expect(result.bindingsNote).toBeUndefined();
  });

  it('shortens values in brief mode, and says so', async () => {
    const result = await repl.session.execute('big(V)', undefined, undefined, undefined, 'brief');

    const value = String(result.bindings[0]?.value);
    expect(result.bindings[0]?.name).toBe('V');
    expect(value.length).toBeLessThan(130);
    // Truncated, but still recognisably the start of the value.
    expect(value).not.toContain('x'.repeat(BRIEF_VALUE_CHARS + 1));
    expect(value).toContain('x'.repeat(40));
    // The original length is kept: otherwise a long value looks like a short one.
    expect(value).toMatch(/\(\d+ chars\)/);
    expect(result.bindingsNote).toMatch(/shortened/);
    // `solutions` mirrors `bindings`, so a half-done job would leak the full value.
    expect(String(result.solutions[0]?.[0]?.value)).toMatch(/\(\d+ chars\)/);
  });

  it('omits bindings entirely in none mode, reporting the count', async () => {
    const result = await repl.session.execute('big(V)', undefined, undefined, undefined, 'none');

    expect(result.bindings).toEqual([]);
    expect(result.solutions).toEqual([]);
    expect(result.bindingsNote).toMatch(/1 binding omitted/);
    // The goal still ran: the agent can see it succeeded and what it printed.
    expect(result.error).toBeUndefined();
  });

  it('still reports a goal that bound nothing as nothing', async () => {
    const result = await repl.session.execute('true', undefined, undefined, undefined, 'none');
    expect(result.bindings).toEqual([]);
    expect(result.bindingsNote).toMatch(/0 bindings omitted/);
  });

  it('keeps short values intact in brief mode', async () => {
    await repl.session.beginTurn();
    await repl.session.define('small/1', 'small(7).');
    await repl.session.endTurn();

    const result = await repl.session.execute('small(V)', undefined, undefined, undefined, 'brief');
    expect(result.bindings[0]?.value).toBe('7');
  });

  it('is described to the model on the tool itself', async () => {
    // The agent only learns the option from the tool schema, so the description
    // and the enum-ish values have to be there, not just in the code.
    const tools = createHarnessTools(repl.session as never);
    const execute = tools.get('execute');
    expect(execute).toBeDefined();
    const properties = (execute?.parameters as { properties?: Record<string, { description?: string }> })
      .properties ?? {};
    expect(properties).toHaveProperty('bindings');
    expect(String(properties.bindings?.description)).toMatch(/full/);
    expect(String(properties.bindings?.description)).toMatch(/brief/);
    expect(String(properties.bindings?.description)).toMatch(/none/);

    // And the option is actually wired through, not only advertised.
    const viaTool = await execute?.execute({ goal: 'big(V)', bindings: 'brief' }, {});
    expect(String((viaTool as { bindingsNote?: string }).bindingsNote)).toMatch(/shortened/);
  });
});
