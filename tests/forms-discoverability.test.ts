/**
 * Forms are built in, and an authoring agent can only use what it can find.
 *
 * Two things used to make them invisible: the `reference` tool served only the
 * bundled docs and none mentioned forms, and `current_predicate(form/2)` reports
 * `no` even in a working session (it is a host handler, like `exec/2`), so the
 * obvious probe said the opposite of the truth. There is no gate any more — the
 * tools are installed into every session — so these tests guard the discovery
 * path and that the directive does not come back.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';
import { fileURLToPath } from 'url';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK } from '../src/index.js';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { getAgentPromptAssetPath, getWorkspaceDocAssetPath } from '../src/system/assets/index.js';

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, '..');
const bundledFormsDoc = path.join(repoRoot, 'src', 'system', 'assets', 'docs', 'DML_FORMS.md');
const sourceFormsDoc = path.join(repoRoot, 'docs', 'DML_FORMS.md');
const examplesDoc = path.join(repoRoot, 'src', 'system', 'assets', 'docs', 'HARNESS_EXAMPLES.md');

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
    expect(text).toContain('form/2');
    expect(text).toContain('await_form');
    // And it says the feature is on, not that it must be enabled.
    expect(text).toContain('always available');
  });

  it('has no capability gate left to teach', async () => {
    // The directive is mentioned only as history (§4 records why it was removed).
    const text = await repl.session.reference('forms');
    expect(text).toMatch(/gate was \*\*removed\*\*|removed/);
    expect(text).not.toContain('add `:- capability(forms).` to the harness');
  });

  it('offers the built-in form tools through listing(tools)', async () => {
    // `listing` is a tool the agent can call, so this is the surface it actually
    // reads — the manifest is recorded in history, not injected into the prompt.
    const listing = await repl.session.listing('tools:all');
    expect(listing).toContain('render_form');
    expect(listing).toContain('wait_for_form_change');

    // ...but they are not harness predicates: the harness stays clean.
    const manifest = await repl.session.manifest();
    expect(manifest).not.toContain('- capabilities:');
    expect(manifest.split('\n').find((line) => line.startsWith('- predicates:'))).not.toContain('render_form');
  });

  it('keeps the bundled reference doc identical to docs/DML_FORMS.md', async () => {
    const bundled = await fs.readFile(bundledFormsDoc, 'utf8');
    const source = await fs.readFile(sourceFormsDoc, 'utf8');
    expect(
      bundled,
      'src/system/assets/docs/DML_FORMS.md is generated from docs/DML_FORMS.md — run: npm run sync:docs',
    ).toBe(source);
  });

  it('documents forms in the agent prompt, without a capability concept', async () => {
    const prompt = await fs.readFile(getAgentPromptAssetPath(), 'utf8');
    expect(prompt).toContain('form/2');
    expect(prompt).toContain('await_form');
    // The behavioural half teaches forms directly; there is no capability topic.
    expect(prompt).toContain('reference("forms")');
    expect(prompt).not.toContain('reference("capabilities")');
    expect(getWorkspaceDocAssetPath('dml-forms').endsWith('DML_FORMS.md')).toBe(true);
  });
});

describe('the forms example in HARNESS_EXAMPLES.md', () => {
  it('loads and runs (so the agent copies a shape that works)', async () => {
    const examples = await fs.readFile(examplesDoc, 'utf8');
    // The no-model snippet: it is the one a test can drive end to end.
    const snippet = prologBlocks(examples).find((block) => block.includes('claim_form(Values)'));
    expect(snippet, 'HARNESS_EXAMPLES.md has no runnable forms snippet').toBeTruthy();

    let sdk: DeepClauseSDK | undefined;
    try {
      sdk = await createDeepClause({ model: 'test-model', provider: 'openai' });
      const answers: string[] = [];
      const code = `${snippet}\nagent_main :- claim_form(V), format(string(M), "filed: ~w", [V]), answer(M).`;
      for await (const event of sdk.runDML(code, {
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
