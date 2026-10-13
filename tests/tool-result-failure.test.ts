/**
 * A tool that fails by *returning* an error must be reported as failed.
 *
 * `inferToolLifecycleFields` only recognised `success: false`, so `{ error: "…" }`
 * — including the runtime's own "already running further up the call chain"
 * refusal — came back as `completed`, and the transcript showed a tick for a call
 * that never ran. The authoring agent loop had the check; the DML one did not.
 */
import { describe, expect, it } from 'vitest';

import { buildToolCompletionEvent } from '../src/system/runtime/shell-tool-events.js';

const event = (result: unknown) => buildToolCompletionEvent('some_tool', { arg1: 'x' }, result);

describe('tool completion state from a returned result', () => {
  it('reports a returned error as failed, with the reason', () => {
    const e = event({ error: 'Tool not available: it is already running further up the call chain.' });
    expect(e.toolState).toBe('failed');
    expect(e.toolError).toContain('already running');
    expect(e.toolSummary).toContain('already running');
  });

  it('reports success:false as failed (existing behaviour)', () => {
    expect(event({ success: false, error: 'boom' }).toolState).toBe('failed');
    expect(event({ success: false }).toolState).toBe('failed');
  });

  it('reports ok:false as failed', () => {
    expect(event({ ok: false, error: 'nope' }).toolState).toBe('failed');
  });

  it('accepts an error object with a message', () => {
    const e = event({ error: { message: 'bad input' } });
    expect(e.toolState).toBe('failed');
    expect(e.toolError).toBe('bad input');
  });

  it('does not mislabel a successful result that carries an empty error field', () => {
    expect(event({ error: '' }).toolState).toBe('completed');
    expect(event({ error: null }).toolState).toBe('completed');
    expect(event({ error: '   ' }).toolState).toBe('completed');
    expect(event({ value: 1 }).toolState).toBe('completed');
    expect(event({ stdout: 'ok', exitCode: 0 }).toolState).toBe('completed');
  });

  it('keeps an explicit summary when there is one', () => {
    expect(event({ error: 'bad', summary: 'the short reason' }).toolSummary).toBe('the short reason');
  });
});
