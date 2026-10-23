import { describe, expect, it } from 'vitest';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl } from '../src/repl/index.js';
import { REFERENCE_TOPICS } from '../src/repl/reference.js';
import { getWorkspaceDocAssetPath } from '../src/system/assets/index.js';

describe('author-facing runnable documentation contracts', () => {
  it('parses every module-shaped Prolog block against the current loader', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-doc-contracts-'));
    try {
      const docs = [path.join(process.cwd(), 'src/system/assets/docs/AGENT_PROMPT.md'),
        ...REFERENCE_TOPICS.map((topic) => getWorkspaceDocAssetPath(topic.asset))];
      for (const file of docs) {
        const text = await fs.readFile(file, 'utf8');
        let index = 0;
        for (const block of text.matchAll(/```prolog\n([\s\S]*?)```/g)) {
          const repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
          try {
            const loaded = await repl.session.loadCode(block[1]);
            expect(loaded.error, `${path.basename(file)} block ${++index}`).toBeUndefined();
            // Check keyed docs describe real arities instead of only string presence.
            const warnings = await repl.session.beginTurn();
            expect(warnings.ok, file).toBe(true);
            const committed = await repl.session.endTurn();
            expect(committed.warnings?.filter((warning) => /documents a predicate with no clauses|different arity/.test(warning)) ?? [], `${file}:${index}`).toEqual([]);
          } finally { await repl.dispose(); }
        }
      }
    } finally { await fs.rm(workspace, { recursive: true, force: true }); }
  }, 30000);

  it('does not teach contradictions that caused the original failures', async () => {
    for (const entry of REFERENCE_TOPICS) {
      const text = await fs.readFile(getWorkspaceDocAssetPath(entry.asset), 'utf8');
      expect(text, entry.topic).not.toContain('DML is single-threaded orchestration');
      expect(text, entry.topic).not.toContain('Every DML program must define');
      expect(text, entry.topic).not.toContain('second `task/N` gets\nfresh memory');
      for (const block of text.matchAll(/```[^\n]*\n([\s\S]*?)```/g)) {
        expect(block[1], entry.topic).not.toMatch(/exec\(bash\(|\.dc-repl|\$TMPDIR/);
      }
    }
  });
});
