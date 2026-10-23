import { describe, expect, it } from 'vitest';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent, LLMBackend, ToolDefinition } from '../src/types.js';
import { CANONICAL_EXAMPLES, examplesMarkdown } from '../scripts/canonical-examples.mjs';
import { canonicalBackend } from '../scripts/canonical-backend.mjs';

async function collect(repl: HarnessRepl, goal: string): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of repl.query(goal)) events.push(event);
  return events;
}

describe('canonical examples: current language, host and output contracts', () => {
  it('generates the examples reference, not a hand-maintained prompt copy', async () => {
    expect(await fs.readFile('src/system/assets/docs/HARNESS_EXAMPLES.md', 'utf8')).toBe(examplesMarkdown());
  });

  for (const example of CANONICAL_EXAMPLES) {
    it(`${example.id}: loads, runs and returns the documented result`, async () => {
      const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-canonical-'));
      let repl: HarnessRepl | undefined;
      const backend = canonicalBackend(example.id);
      let turn = 0;
      const python: ToolDefinition = {
        description: 'Fixture Python host', parameters: { type: 'object', properties: {} },
        execute: async (args) => {
          const input = String((args.argv as string[])[0]);
          const full = path.isAbsolute(input) ? input : path.join(workspace, input);
          try {
            const text = (await fs.readFile(full, 'utf8')).toUpperCase();
            return { exitCode: 0, stderr: '', stdout: args.script ? JSON.stringify({ text }) + '\n' : text + '\n' };
          } catch (error) { return { exitCode: 1, stderr: String(error), stdout: '' }; }
        },
      };
      try {
        for (const [file, text] of Object.entries(example.files ?? {})) {
          await fs.mkdir(path.dirname(path.join(workspace, file)), { recursive: true });
          await fs.writeFile(path.join(workspace, file), text as string);
        }
        for (const [file, data] of Object.entries(example.binaryFiles ?? {})) {
          await fs.mkdir(path.dirname(path.join(workspace, file)), { recursive: true });
          await fs.writeFile(path.join(workspace, file), Buffer.from(data as string, 'base64'));
        }
        repl = await createHarnessRepl({
          workspacePath: workspace, model: 'test-model', provider: 'openai', execution: 'inline',
          llmBackend: backend as LLMBackend, tools: new Map([['python', python]]),
          onInput: async () => ['demo', 'Please read canonical/input.txt', 'What did you just read?'][turn++],
          onForm: (form) => {
            if (form.op === 'open') repl!.session.submitForm(form.formId, {
              action: 'submit', values: { name: 'Ada', color: 'blue' }, revision: form.revision,
            });
          },
        });
        expect((await repl.session.loadCode(example.code)).error).toBeUndefined();
        const events = await collect(repl, example.goal);
        expect(events.filter((event) => event.type === 'error'), example.id).toEqual([]);
        const text = events.filter((event) => ['output', 'answer', 'stream'].includes(event.type)).map((event) => event.content ?? '').join('');
        expect(text.replace(/\s+/g, ''), example.id).toContain(example.expected.replace(/\s+/g, ''));
        if (example.id === 'vision') expect(backend.state.sawMedia).toBe(true);
        if (example.id === 'task-memory') { expect(backend.state.sawPriorTask).toBe(true); expect(backend.state.freshPrompt).toBe(true); }
        if (example.id === 'task-tools') expect(backend.state.usedCapability).toBe(true);
        if (example.id === 'dynamic-form') expect(backend.state.formCalls).toEqual(['render_form', 'wait_for_form_change', 'close_form']);
        if (example.id === 'basic-relation') expect((await collect(repl, 'size_label(-1, _)')).some((e) => e.type === 'solution')).toBe(false);
        if (example.id === 'python-files') expect((await collect(repl, 'normalise("missing.txt", _)')).some((e) => e.type === 'error')).toBe(true);
        if (example.id === 'fixed-form') {
          repl.session.setFormHandler((form) => { if (form.op === 'open') repl!.session.submitForm(form.formId, { action: 'cancel', values: {} }); });
          const cancelled = await collect(repl, 'claim_form(V), get_dict(name, V, _)');
          expect(cancelled.some((e) => e.type === 'solution')).toBe(false);
        }
      } finally {
        await repl?.dispose();
        await fs.rm(workspace, { recursive: true, force: true });
      }
    }, 30000);
  }
});
