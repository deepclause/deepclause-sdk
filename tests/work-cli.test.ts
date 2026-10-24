import { describe, expect, it } from 'vitest';
import { classifyReplInput, formatTurnInput } from '../src/cli/repl.js';
import { parseWorkflowCommand, runDeclaredWorkflow } from '../src/cli/work.js';
import { WorkView } from '../src/repl/tui/work-view.js';
import type { HarnessRepl } from '../src/repl/index.js';

describe('terminal Work adapter', () => {
  it('parses modes, details and named workflows without treating inputs as DML', () => {
    expect(classifyReplInput('/mode work support')).toEqual({ kind: 'mode', mode: 'work', harness: 'support' });
    expect(classifyReplInput('/mode other').kind).toBe('unknown-command');
    expect(classifyReplInput('/details off')).toEqual({ kind: 'details', show: false });
    expect(classifyReplInput('/workflow')).toEqual({ kind: 'workflows' });
    expect(parseWorkflowCommand('"Review a file" {"File":"uploads/a b.csv"}')).toEqual({ name: 'Review a file', inputs: { File: 'uploads/a b.csv' } });
    expect(() => parseWorkflowCommand('"Name" []')).toThrow();
    expect(formatTurnInput({ mode: 'goal', request: 'Workflow: A', workflow: { name: 'A', inputs: { Text: 'hi' } } })).toBe('/workflow "A" {"Text":"hi"}');
  });

  it('asks for only declared inputs and calls the deterministic API', async () => {
    const calls: unknown[] = [];
    const repl = {
      workSpec: async () => ({ shell: { workflows: [{ name: 'Review', inputs: [{ name: 'File', type: 'file' }], outputs: [{ name: 'Result', type: 'string' }] }] } }),
      runWorkflow: async function* (name: string, inputs: unknown) {
        calls.push({ name, inputs });
        yield { type: 'workflow_result', workflow: { result: { ok: true } } };
      },
    } as unknown as HarnessRepl;
    const prompts: string[] = [];
    const ok = await runDeclaredWorkflow({ repl, name: 'Review', ask: async prompt => { prompts.push(prompt); return 'uploads/a b.csv'; }, render: () => {} });
    expect(ok).toBe(true);
    expect(prompts).toHaveLength(1);
    expect(calls).toEqual([{ name: 'Review', inputs: { File: 'uploads/a b.csv' } }]);
    await expect(runDeclaredWorkflow({ repl, name: 'Review', render: () => {} })).rejects.toThrow('JSON');
  });

  it('keeps the Work panel honest when no shell exists', () => {
    const panel = new WorkView(() => ({ harness: 'main', hash: 'abc', shell: null, diagnostics: ['No Work shell defined.'] }));
    expect(panel.render(36).join('\n')).toContain('No Work shell');
  });
});
