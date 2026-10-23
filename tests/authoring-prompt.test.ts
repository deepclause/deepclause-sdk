import { describe, expect, it } from 'vitest';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { buildAuthoringProfiles, generatedDocuments, PROMPT_BUDGETS } from '../scripts/sync-agent-prompt.mjs';
import { REFERENCE_TOPICS, referenceSections } from '../src/repl/reference.js';
import { getWorkspaceDocAssetPath } from '../src/system/assets/index.js';
import { buildAgentSystemPrompt } from '../src/repl/agent.js';

const assets = join(process.cwd(), 'src/system/assets');
const docs = join(assets, 'docs');
const core = readFileSync(join(assets, 'authoring-core.md'), 'utf8');
const profiles = buildAuthoringProfiles(core);

describe('shared bounded authoring profiles', () => {
  it('generates committed profiles and examples from shared sources without a reference appendix', () => {
    for (const [file, expected] of generatedDocuments()) {
      expect(readFileSync(join(docs, file), 'utf8'), file).toBe(expected);
    }
    expect(profiles.normal).not.toContain('# DML Language Reference');
    expect(profiles.normal).not.toContain('# DML language API');
    expect(profiles.normal.length).toBeLessThan(PROMPT_BUDGETS.normal);
    expect(profiles.local.length).toBeLessThan(PROMPT_BUDGETS.local);
    expect(profiles.local.length).toBeLessThan(profiles.normal.length);
  });

  for (const [name, prompt] of Object.entries(profiles)) {
    it(`${name} carries the same essential language and safety contracts`, () => {
      for (const term of [
        'begin_turn', 'define', 'delete', 'end_turn', 'rollback', 'Name/Arity',
        'unifies', 'is 2 + 3', '=:=', '==', 'dif/2', 'Failure', 'once/1',
        'shell/1', 'shell.tools', 'tool/N', 'exec', 'task/N', 'prompt/N',
        'with_fresh_memory', 'Authoring messages', 'llm/2', 'typed outputs',
        'form/2', 'await_form/2', 'foreground', 'read_media', 'image backend',
        'spawn', 'process', 'doc:f/2', 'script_file:extract_py', 'couple of unsuccessful',
      ]) expect(prompt, `${name}: ${term}`).toContain(term);
      expect(prompt).not.toContain('assert_many');
      expect(prompt).toContain('Start small');
      expect(prompt).toContain('Add intermediate `output/1`');
      expect(prompt).toContain('bindings: "brief"');
      expect(prompt).toContain('A different persona/welcome still uses shell');
      expect(prompt).toContain('do not read every');
    });
  }

  it('has real discoverable topics for every feature in the routing table', () => {
    const topics = REFERENCE_TOPICS.map((entry) => entry.topic);
    for (const topic of ['basics', 'tasks', 'shell', 'processes', 'media', 'python', 'forms', 'debugging']) {
      expect(topics).toContain(topic);
    }
    for (const entry of REFERENCE_TOPICS) {
      const text = readFileSync(getWorkspaceDocAssetPath(entry.asset), 'utf8');
      expect(text.length, entry.topic).toBeGreaterThan(100);
      expect(referenceSections(text).length, entry.topic).toBeGreaterThan(0);
    }
  });

  it('bounds additional model discovery context instead of injecting a giant catalog', async () => {
    const prompt = await buildAgentSystemPrompt({ currentModel: 'fixture:active', capabilityTool: 'python',
      modelCatalog: Array.from({ length: 200 }, (_, index) => ({ model: `fixture:model-${index}`, inputCost: 1, outputCost: 2 })),
    });
    expect(prompt).toContain('200 configured models total');
    expect(prompt).toContain('call models');
    expect(prompt).toContain('fixture:model-0');
    expect(prompt).not.toContain('fixture:model-199');
    expect(prompt.length - profiles.normal.length).toBeLessThan(2500);
  });

  it('names a valid precise memory section rather than an inaccessible repository file', () => {
    const tasks = readFileSync(join(docs, 'DML_TASKS.md'), 'utf8');
    expect(referenceSections(tasks).map((section) => section.id)).toContain('memory');
    expect(profiles.normal).toContain('section: "memory"');
    expect(profiles.normal).not.toContain('../../../../docs');
  });
});
