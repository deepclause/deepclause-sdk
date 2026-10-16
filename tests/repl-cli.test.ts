/**
 * M1 — REPL CLI: input classifier and goal dispatch.
 */
import { describe, expect, it } from 'vitest';

import {
  cancelsScriptedRetry,
  classifyReplInput,
  formatTurnInput,
  runGoalWithRenderer,
  runInteractiveGoal,
} from '../src/cli/repl.js';
import { ReplRenderer } from '../src/repl/render.js';
import type { DMLEvent } from '../src/types.js';
import type { HarnessRepl } from '../src/repl/index.js';

function fakeRepl(events: DMLEvent[]): HarnessRepl {
  return {
    // eslint-disable-next-line @typescript-eslint/require-await
    async *query() {
      for (const event of events) {
        yield event;
      }
    },
    prompt: (async function* () {}) as HarnessRepl['prompt'],
    load: async () => ({}),
    list: async () => '',
    freeze: async () => undefined,
    unfreeze: async () => undefined,
    export: async () => ({ path: '', clauses: 0, tools: 0, scripts: 0 }),
    isFrozen: () => false,
    reset: async () => undefined,
    dispose: async () => undefined,
    session: {} as HarnessRepl['session'],
  };
}

describe('classifyReplInput', () => {
  it('classifies goal: as goal mode and never as a prompt', () => {
    expect(classifyReplInput('goal: member(X, [a,b,c]).')).toEqual({
      kind: 'goal',
      goal: 'member(X, [a,b,c]).',
    });
    expect(classifyReplInput('goal: member(X, [a,b').kind).toBe('goal');
  });

  it('classifies anything else as an English prompt', () => {
    expect(classifyReplInput('add a rule that computes factorial')).toEqual({
      kind: 'prompt',
      prompt: 'add a rule that computes factorial',
    });
    expect(classifyReplInput('how many rows are in the CSV?').kind).toBe('prompt');
  });

  it('matches out-of-band commands before the classifier', () => {
    expect(classifyReplInput('halt.')).toEqual({ kind: 'halt' });
    expect(classifyReplInput('help.')).toEqual({ kind: 'help' });
    expect(classifyReplInput('reset.')).toEqual({ kind: 'reset' });
    expect(classifyReplInput('rollback.')).toEqual({ kind: 'rollback' });
    expect(classifyReplInput('model.')).toEqual({ kind: 'model' });
    expect(classifyReplInput('model anthropic/claude.')).toEqual({ kind: 'model', model: 'anthropic/claude' });
    expect(classifyReplInput('freeze.')).toEqual({ kind: 'freeze' });
    expect(classifyReplInput('unfreeze.')).toEqual({ kind: 'unfreeze' });
    expect(classifyReplInput('trace.')).toEqual({ kind: 'trace' });
    expect(classifyReplInput('notrace.')).toEqual({ kind: 'notrace' });
    expect(classifyReplInput('params.')).toEqual({ kind: 'params' });
    expect(classifyReplInput('listing.')).toEqual({ kind: 'listing' });
    expect(classifyReplInput('listing(fact/2).')).toEqual({ kind: 'listing', filter: 'fact/2' });
    expect(classifyReplInput('export harness.dml.')).toEqual({ kind: 'export', path: 'harness.dml' });
  });

  it('ignores blank lines and comments', () => {
    expect(classifyReplInput('   ').kind).toBe('empty');
    expect(classifyReplInput('% a comment').kind).toBe('comment');
  });

  it('renders goal turns with a /goal prefix for editor history', () => {
    expect(formatTurnInput({ mode: 'goal', request: 'member(X, [1,2])' })).toBe('/goal member(X, [1,2])');
    expect(formatTurnInput({ mode: 'prompt', request: 'add a fact' })).toBe('add a fact');
  });

  it('classifies slash commands', () => {
    expect(classifyReplInput('/goal member(X, [a,b,c])')).toEqual({ kind: 'goal', goal: 'member(X, [a,b,c])' });
    expect(classifyReplInput('/goal')).toEqual({ kind: 'unknown-command', name: 'goal (missing goal)' });
    expect(classifyReplInput('/listing')).toEqual({ kind: 'listing' });
    expect(classifyReplInput('/listing fact/2')).toEqual({ kind: 'listing', filter: 'fact/2' });
    expect(classifyReplInput('/halt')).toEqual({ kind: 'halt' });
    expect(classifyReplInput('/exit')).toEqual({ kind: 'halt' });
    expect(classifyReplInput('/help')).toEqual({ kind: 'help' });
    expect(classifyReplInput('/reset')).toEqual({ kind: 'reset' });
    expect(classifyReplInput('/compact focus on X')).toEqual({ kind: 'compact', instructions: 'focus on X' });
    expect(classifyReplInput('/model anthropic/claude')).toEqual({ kind: 'model', model: 'anthropic/claude' });
    expect(classifyReplInput('/nope')).toEqual({ kind: 'unknown-command', name: 'nope' });
  });

  it('classifies /ps and its snapshot-inspection flags', () => {
    expect(classifyReplInput('/ps')).toEqual({ kind: 'ps', watch: false, logs: false, code: undefined });
    expect(classifyReplInput('/ps --watch')).toMatchObject({ kind: 'ps', watch: true });
    expect(classifyReplInput('/ps --watch --logs')).toMatchObject({ kind: 'ps', watch: true, logs: true });
    // `--code` alone reports the current version; `--code <target>` inspects a
    // process snapshot (both `--code=x` and `--code x`).
    expect(classifyReplInput('/ps --code')).toMatchObject({ kind: 'ps', code: '' });
    expect(classifyReplInput('/ps --code frontdesk')).toMatchObject({ kind: 'ps', code: 'frontdesk' });
    expect(classifyReplInput('/ps --code=p2')).toMatchObject({ kind: 'ps', code: 'p2' });
  });
});

describe('runGoalWithRenderer', () => {
  it('renders every solution by default', async () => {
    const lines: string[] = [];
    const renderer = new ReplRenderer({ mode: 'plain', sink: (text) => lines.push(text) });
    const events: DMLEvent[] = [
      { type: 'solution', solution: { bindings: [{ name: 'X', value: 'a' }], deterministic: false } },
      { type: 'solution', solution: { bindings: [{ name: 'X', value: 'b' }], deterministic: false } },
      { type: 'finished' },
    ];
    const code = await runGoalWithRenderer(fakeRepl(events), 'member(X,[a,b])', { renderer });
    expect(code).toBe(0);
    expect(lines.join('')).toContain('X = a');
    expect(lines.join('')).toContain('X = b');
  });

  it('prints only the first solution with --first', async () => {
    const lines: string[] = [];
    const renderer = new ReplRenderer({ mode: 'plain', sink: (text) => lines.push(text) });
    const events: DMLEvent[] = [
      { type: 'solution', solution: { bindings: [{ name: 'X', value: 'a' }], deterministic: false } },
      { type: 'solution', solution: { bindings: [{ name: 'X', value: 'b' }], deterministic: false } },
    ];
    await runGoalWithRenderer(fakeRepl(events), 'member(X,[a,b])', { renderer, first: true });
    expect(lines.join('')).toContain('X = a');
    expect(lines.join('')).not.toContain('X = b');
  });

  it('returns a non-zero exit code on error', async () => {
    const lines: string[] = [];
    const renderer = new ReplRenderer({ mode: 'plain', sink: (text) => lines.push(text) });
    const code = await runGoalWithRenderer(fakeRepl([{ type: 'error', content: 'boom' }]), 'bad', { renderer });
    expect(code).toBe(1);
  });

  it('reports a goal with no solutions as `false.` (plain) and a record (json)', async () => {
    const plain: string[] = [];
    await runGoalWithRenderer(fakeRepl([{ type: 'finished' }]), 'fail', {
      renderer: new ReplRenderer({ mode: 'plain', sink: (text) => plain.push(text) }),
    });
    expect(plain.join('')).toContain('false.');

    const json: string[] = [];
    await runGoalWithRenderer(fakeRepl([{ type: 'finished' }]), 'fail', {
      renderer: new ReplRenderer({ mode: 'json', sink: (text) => json.push(text) }),
    });
    // stdout must stay one-JSON-object-per-line: a raw `false.` corrupts it.
    expect(json).toHaveLength(1);
    expect(JSON.parse(json[0])).toEqual({ type: 'no_solution' });
  });

  it('does not report an answered goal as a failed one', async () => {
    const json: string[] = [];
    await runGoalWithRenderer(
      fakeRepl([{ type: 'answer', content: 'done' }, { type: 'finished' }]),
      'q',
      { renderer: new ReplRenderer({ mode: 'json', sink: (text) => json.push(text) }) },
    );
    for (const line of json) expect(() => JSON.parse(line)).not.toThrow();
    expect(json.some((line) => line.includes('no_solution'))).toBe(false);
    expect(json.some((line) => line.includes('"answer"'))).toBe(true);
  });
});

describe('runInteractiveGoal pager', () => {
  const events: DMLEvent[] = [
    { type: 'solution', solution: { bindings: [{ name: 'X', value: 'a' }], deterministic: false } },
    { type: 'solution', solution: { bindings: [{ name: 'X', value: 'b' }], deterministic: false } },
  ];

  it('continues on Enter and stops on "."', async () => {
    const lines: string[] = [];
    const renderer = new ReplRenderer({ mode: 'plain', sink: (text) => lines.push(text) });
    const keys = ['', '.'];
    let index = 0;
    await runInteractiveGoal(fakeRepl(events), renderer, 'member(X,[a,b])', {} as never, async () => keys[index++] ?? '.');
    expect(lines.join('')).toContain('X = a');
    expect(lines.join('')).toContain('X = b');
  });

  it('stops immediately on "."', async () => {
    const lines: string[] = [];
    const renderer = new ReplRenderer({ mode: 'plain', sink: (text) => lines.push(text) });
    await runInteractiveGoal(fakeRepl(events), renderer, 'member(X,[a,b])', {} as never, async () => '.');
    expect(lines.join('')).toContain('X = a');
    expect(lines.join('')).not.toContain('X = b');
  });
});

describe('scripted form answers', () => {
  // The host re-shows a rejected submit so a person can correct it. With
  // `--answer` the same canned reply would be refused, re-shown, refused …
  // forever, so the retry cancels instead. This hung `repl -g agent_main
  // --answer ""` on sdk-examples/forms.dml.
  const rejection = { op: 'update' as const, formId: 'f1', revision: 2, issues: [{ path: 'amount', level: 'error' as const, message: 'is required' }] };

  it('cancels a retry when the run is scripted', () => {
    expect(cancelsScriptedRetry({ answer: '' } as never, rejection)).toBe(true);
    expect(cancelsScriptedRetry({ answer: '5' } as never, rejection)).toBe(true);
  });

  it('leaves interactive runs, the first presentation, and plain updates alone', () => {
    // No canned answer: a person is there to answer again.
    expect(cancelsScriptedRetry({} as never, rejection)).toBe(false);
    // The first presentation is not a retry.
    expect(cancelsScriptedRetry({ answer: '' } as never, { ...rejection, op: 'open', issues: [] })).toBe(false);
    // A model's `update_form` carries no issues and must not be cancelled.
    expect(cancelsScriptedRetry({ answer: '' } as never, { ...rejection, issues: [] })).toBe(false);
  });
});
