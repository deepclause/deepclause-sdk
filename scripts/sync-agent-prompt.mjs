#!/usr/bin/env node
/** Build bounded normal/local profiles and example docs from shared sources. */
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { CANONICAL_EXAMPLES, examplesMarkdown } from './canonical-examples.mjs';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');
const assets = join(root, 'src', 'system', 'assets');
const docs = join(assets, 'docs');
export const PROMPT_BUDGETS = { normal: 18000, local: 11000 };
export function buildAuthoringProfiles(source) {
  const normal = source.replace(/<!-- local:omit:(?:start|end) -->\n?/g, '').trim() + '\n';
  const local = source.replace(/<!-- local:omit:start -->[\s\S]*?<!-- local:omit:end -->\n?/g, '').trim()
    + '\n\nLocal model: call one tool at a time. Read a focused reference section when needed; do not guess unsupported syntax.\n';
  if (normal.length > PROMPT_BUDGETS.normal || local.length > PROMPT_BUDGETS.local) {
    throw new Error(`Authoring prompt budget exceeded: normal=${normal.length}, local=${local.length}`);
  }
  if (normal.includes('# DML language API') || normal.includes('# DML Language Reference')) {
    throw new Error('Do not embed the full reference in the system prompt.');
  }
  return { normal, local };
}
export function generatedDocuments() {
  const { normal, local } = buildAuthoringProfiles(readFileSync(join(assets, 'authoring-core.md'), 'utf8'));
  return new Map([
    ['AGENT_PROMPT.md', normal],
    ['LOCAL_AGENT_PROMPT.md', local],
    ['HARNESS_EXAMPLES.md', examplesMarkdown()],
  ]);
}
export function syncPrompts(check = false) {
  for (const [name, expected] of generatedDocuments()) {
    const target = join(docs, name);
    if (check) {
      let current = '';
      try { current = readFileSync(target, 'utf8'); } catch { /* show useful drift error */ }
      if (current !== expected) throw new Error(`${name} is out of sync; run npm run sync:agent-prompt`);
    } else writeFileSync(target, expected);
    console.log(`${check ? 'checked' : 'synced'} ${name}: ${expected.length} characters`);
  }
  for (const example of CANONICAL_EXAMPLES) {
    if (!example.requires || !example.goal || !example.expected || !example.failure) throw new Error(`Incomplete example ${example.id}`);
  }
}
if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  try { syncPrompts(process.argv.includes('--check')); } catch (error) { console.error(error.message); process.exit(1); }
}
