/**
 * Cooperative suspension (`suspend/1`) — docs/DML_REPL_SUSPEND_RESUME.md.
 *
 * A turn stops at a safe point and the host records the goal that resumes it. The
 * guard that matters: a turn holding something a snapshot cannot carry (an open
 * stream, an open form) refuses to suspend, so a resumed session never inherits a
 * half-written file.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent } from '../src/types.js';

let workspace: string;
let repl: HarnessRepl | undefined;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-suspend-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

async function commit(harness: HarnessRepl, nameArity: string, clauses: string): Promise<void> {
  await harness.session.beginTurn();
  await harness.session.define(nameArity, clauses);
  await harness.session.endTurn();
}

async function run(harness: HarnessRepl, goal: string): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of harness.query(goal)) events.push(event);
  return events;
}

describe('suspend/1', () => {
  it('stops the turn and records the continuation plus its memory', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai', history: {},
    });
    await commit(repl, 'loop/1', [
      'loop(N) :-',
      '    (   N >= 2',
      '    ->  suspend(loop(N))',
      '    ;   M is N + 1, loop(M)',
      '    ).',
    ].join('\n'));

    const events = await run(repl, 'loop(0)');

    const suspend = events.find((event) => event.type === 'suspend');
    expect(suspend?.suspend?.continuation).toBe('loop(2)');
    // No error: a suspension is a clean stop, like answer/1.
    expect(events.some((event) => event.type === 'error')).toBe(false);
    expect(repl.session.pendingSuspension()?.continuation).toBe('loop(2)');

    // Durable: the record sits beside the session, so a later process can resume.
    const record = JSON.parse(
      await fs.readFile(path.join(workspace, '.deepclause/repl/sessions', repl.session.getSessionId(), 'suspend.json'), 'utf8'),
    ) as { continuation: string; schemaVersion: number };
    expect(record.continuation).toBe('loop(2)');
    expect(record.schemaVersion).toBe(1);
  });

  it('resumes the continuation in a reopened session', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai', history: {},
    });
    const sessionId = repl.session.getSessionId();
    await commit(repl, 'loop/1', 'loop(N) :- ( N >= 2 -> suspend(loop(N)) ; M is N + 1, loop(M) ).');
    await run(repl, 'loop(0)');
    await repl.dispose();

    repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai',
      history: {}, sessionId, resume: true,
    });
    const pending = repl.session.pendingSuspension();
    expect(pending?.continuation).toBe('loop(2)');

    // Running the continuation suspends at the same point: the module and memory
    // survived, so it is the same program continuing.
    const events = await run(repl, pending!.continuation);
    expect(events.find((event) => event.type === 'suspend')?.suspend?.continuation).toBe('loop(2)');
  });

  it('records the turn memory, and resumes with it', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai', history: {},
    });
    await commit(repl, 'entry/0', 'entry :- system("Persona A."), user("first message"), suspend(entry).');
    await run(repl, 'entry');

    const sessionId = repl.session.getSessionId();
    const record = JSON.parse(
      await fs.readFile(
        path.join(workspace, '.deepclause/repl/sessions', sessionId, 'suspend.json'),
        'utf8',
      ),
    ) as { memory: string };

    // Prolog term text the MI can load back — not `String(list)`.
    expect(record.memory).toContain('message{role: system');
    expect(record.memory).toContain('Persona A.');
    expect(record.memory).toContain('first message');
    expect(record.memory).not.toContain('[object Object]');

    await repl.dispose();
    repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai',
      history: {}, sessionId, resume: true,
    });
    const memory = repl.session.harnessMemory().map((message) => `${message.role}:${message.content}`);
    expect(memory.join('\n')).toContain('Persona A.');
    expect(memory.join('\n')).toContain('first message');
  });

  it('refuses to suspend while a stream is open', async () => {
    repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai', history: {},
    });
    await commit(repl, 'probe/0', [
      'probe :-',
      '    open("/workspace/half-written.txt", write, S),',
      '    write(S, "partial"),',
      '    suspend(probe),',
      '    close(S).',
    ].join('\n'));

    const events = await run(repl, 'probe');
    const error = events.find((event) => event.type === 'error');
    expect(error?.content ?? '').toContain('cannot suspend while holding');
    expect(events.some((event) => event.type === 'suspend')).toBe(false);

    // ...and the same goal suspends once the stream is closed first.
    await commit(repl, 'clean/0', 'clean :- open("/workspace/done.txt", write, S), close(S), suspend(clean).');
    expect((await run(repl, 'clean')).some((event) => event.type === 'suspend')).toBe(true);
  });

  it('refuses to suspend while a form is open', async () => {
    repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
    await commit(repl, 'probe/0', [
      'probe :-',
      '    form(_{title: "t", fields: [_{name: a, type: string}]}, _F),',
      '    suspend(probe).',
    ].join('\n'));

    const events = await run(repl, 'probe');
    expect(events.find((event) => event.type === 'error')?.content ?? '').toContain('cannot suspend while holding form');
  });
});
