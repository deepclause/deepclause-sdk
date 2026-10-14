/**
 * SWI-Prolog's own output, captured instead of printed.
 *
 * Emscripten's defaults send Prolog's stdout/stderr straight to the terminal
 * (`console.log` / `console.warn`). That is what produced this, in the middle of
 * an interactive frame:
 *
 *   Warning: [Thread main] Stream <stream>(0x…):11:87 Deprecated ... Use \c
 *
 * The warning arrives while terms are being parsed, so it lands on top of
 * whatever the UI is drawing. It is now captured by the loader and surfaced as
 * an event, so it renders in order through the same path as everything else —
 * and, for a warning, without needing `--verbose`, because a deprecated or
 * failing construct is not noise the user opted out of.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { takeStartupPrologMessages } from '../src/prolog/loader.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-out-'));
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
  });
  // Anything the loader captured before this session is not this test's.
  repl.session.takePrologMessages();
  takeStartupPrologMessages();
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

async function run(goal: string): Promise<Array<Record<string, unknown>>> {
  const events: Array<Record<string, unknown>> = [];
  for await (const event of repl.query(goal)) {
    events.push(event as Record<string, unknown>);
  }
  return events;
}

describe("Prolog's own output becomes events", () => {
  it('surfaces a warning from stderr as a warning event, and not on the terminal', async () => {
    const stderrWrite = vi.spyOn(process.stderr, 'write').mockImplementation(() => true);
    try {
      const events = await run('format(user_error, "Warning: something is off~n", [])');

      const warning = events.find(
        (event) => event.type === 'log' && String(event.content ?? '').includes('something is off'),
      );
      expect(warning).toBeDefined();
      expect(warning?.level).toBe('warning');
      expect(warning?.channel).toBe('stderr');

      // The whole point: nothing was written to the process's stderr directly.
      const rawWrites = stderrWrite.mock.calls.map(([chunk]) => String(chunk)).join('');
      expect(rawWrites).not.toContain('something is off');
    } finally {
      stderrWrite.mockRestore();
    }
  });

  it('surfaces stdout as output, so a stray write/1 cannot scramble the frame', async () => {
    const events = await run('write("plain prolog output"), nl');

    const output = events.find(
      (event) => event.type === 'output' && String(event.content ?? '').includes('plain prolog output'),
    );
    expect(output).toBeDefined();
  });

  it('labels an error as an error', async () => {
    const events = await run('format(user_error, "ERROR: it broke~n", [])');
    const event = events.find((candidate) => String(candidate.content ?? '').includes('it broke'));
    expect(event?.level).toBe('error');
  });

  it('keeps load-time diagnostics with the load, not the next run', async () => {
    // A deprecated `\`-newline continuation in a quoted string: the reader warns
    // while parsing, which is the message that started this.
    const result = await repl.session.loadCode('go(M) :-\n    M = "alpha \\\n         beta".\n');

    expect(result.error).toBeUndefined();
    expect((result.warnings ?? []).join('\n')).toContain('Deprecated');
    // And it did not leak into the queue to appear as a run event later.
    expect(repl.session.takePrologMessages()).toEqual([]);
  });

  it('does not carry startup diagnostics into a session turn', async () => {
    // The runtime's own module load can warn; those are the loader's, not the
    // first caller's, and must not surface as a spurious event mid-turn.
    const events = await run('true');
    const startup = takeStartupPrologMessages();
    for (const message of startup) {
      expect(events.some((event) => event.content === message.text)).toBe(false);
    }
  });
});
