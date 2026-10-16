/**
 * Forms are implemented — but an authoring agent can only use what it can find.
 *
 * The feature was invisible: the `reference` tool served only the bundled docs,
 * and none mentioned forms; and `current_predicate(form/2)` reports `no` even in
 * a working session (it is a handler, like `exec/2`), so the obvious probe says
 * the opposite of the truth. These tests guard the whole discovery path: the
 * two `reference` topics, the manifest's capability line, the bundled reference
 * doc, the prompt, and a runnable example.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';
import { fileURLToPath } from 'url';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK } from '../src/index.js';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { createHarnessTools } from '../src/repl/harness-tools.js';
import { getAgentPromptAssetPath, getWorkspaceDocAssetPath } from '../src/system/assets/index.js';

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '..');
const bundledFormsDoc = path.join(repoRoot, 'src', 'system', 'assets', 'docs', 'DML_FORMS.md');
const sourceFormsDoc = path.join(repoRoot, 'docs', 'DML_FORMS.md');

/** Every ```prolog block in a markdown document. */
function prologBlocks(markdown: string): string[] {
  const blocks: string[] = [];
  const re = /```prolog\n([\s\S]*?)```/g;
  let match: RegExpExecArray | null;
  while ((match = re.exec(markdown)) !== null) blocks.push(match[1]);
  return blocks;
}

describe('the reference tool serves forms', () => {
  let workspace: string;
  let repl: HarnessRepl;

  beforeEach(async () => {
    workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-forms-discovery-'));
    repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
  });

  afterEach(async () => {
    await repl.dispose();
    await fs.rm(workspace, { recursive: true, force: true });
  });

  it("reference('forms') returns the bundled forms reference", async () => {
    const text = await repl.session.reference('forms');
    expect(text.length).toBeGreaterThan(1000);
    expect(text).toContain('capability(forms)');
    expect(text).toContain('await_form');
  });

  it("reference('capabilities') explains the gate and the handlers", async () => {
    const text = await repl.session.reference('capabilities');
    expect(text).toContain('forms');
    expect(text).toContain('disabled');
    // The two facts that made the probe mislead: handlers are not clauses, and
    // the directive is file-scoped.
    expect(text).toContain('current_predicate');
    expect(text).toContain('handler');
    expect(text).toContain('capability(forms)');
    expect(text).toContain('await_form/2');
  });

  it('lists capabilities in the manifest, disabled until the directive is loaded', async () => {
    expect(await repl.session.manifest()).toContain('- capabilities: forms (disabled)');

    await repl.session.beginTurn();
    await repl.session.loadCode(':- capability(forms).\nprobe :- true.');
    await repl.session.endTurn();

    expect(await repl.session.manifest()).toContain('- capabilities: forms (enabled)');
  });

  it('surfaces the capability error, with the fix, as the execute tool result', async () => {
    await repl.session.beginTurn();
    await repl.session.loadCode('probe :- form(_{version: 1, title: "X", fields: [_{name: n, type: string}]}, F), close_form(F).');
    await repl.session.endTurn();

    const tools = createHarnessTools(repl.session);
    const result = await tools.get('execute')!.execute({ goal: 'probe' });

    const text = JSON.stringify(result);
    expect(text).toContain('capability');
    expect(text).toContain('not enabled');
    expect(text).toContain('capability(forms)');
  });

  it('keeps the bundled reference doc identical to docs/DML_FORMS.md', async () => {
    const bundled = await fs.readFile(bundledFormsDoc, 'utf8');
    const source = await fs.readFile(sourceFormsDoc, 'utf8');
    expect(
      bundled,
      'src/system/assets/docs/DML_FORMS.md is generated from docs/DML_FORMS.md — run: npm run sync:docs',
    ).toBe(source);
  });

  it('documents the capability in the agent prompt', async () => {
    const prompt = await fs.readFile(getAgentPromptAssetPath(), 'utf8');
    expect(prompt).toContain('capability(forms)');
    expect(prompt).toContain('current_predicate');
    // The prompt is the behavioural half plus the reference; make sure the topic
    // list in the behavioural half mentions the way in.
    expect(prompt).toContain('reference("capabilities")');
    expect(getWorkspaceDocAssetPath('dml-forms').endsWith('DML_FORMS.md')).toBe(true);
  });
});

describe('the forms example in HARNESS_EXAMPLES.md', () => {
  it('loads and runs (so the agent copies a shape that works)', async () => {
    const examples = await fs.readFile(
      path.join(repoRoot, 'src', 'system', 'assets', 'docs', 'HARNESS_EXAMPLES.md'),
      'utf8',
    );
    const snippet = prologBlocks(examples).find((block) => block.includes('capability(forms)'));
    expect(snippet, 'HARNESS_EXAMPLES.md has no forms snippet').toBeTruthy();

    let sdk: DeepClauseSDK | undefined;
    try {
      sdk = await createDeepClause({ model: 'test-model', provider: 'openai' });
      const answers: string[] = [];
      for await (const event of sdk.runDML(`${snippet}\nagent_main :- collect_claim.`, {
        onForm: async (form) => {
          sdk!.submitForm(form.formId, {
            action: 'submit',
            values: { vendor: 'acme', amount: 5, notes: 'lunch' },
          });
        },
      })) {
        if (event.type === 'answer' && event.content) answers.push(event.content);
      }
      expect(answers.join('\n')).toContain('filed:');
    } finally {
      await sdk?.dispose();
    }
  });
});
