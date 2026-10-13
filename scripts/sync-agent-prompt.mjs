#!/usr/bin/env node
/**
 * Keep the authoring agent's prompt in sync with the language reference.
 *
 * `AGENT_PROMPT.md` is the system prompt for the REPL's authoring agent, used
 * verbatim. Its second half is a copy of `DML_REFERENCE.md` so the agent has the
 * language detail in context without a tool call. A hand-maintained copy drifts:
 * five sections added to the reference in one session were missing from the prompt,
 * so the agent was reading a stale language guide while the live one was one
 * `reference("language")` call away.
 *
 * This rewrites the prompt as: the behavioural half, then the reference verbatim.
 * `npm run sync:agent-prompt` fixes drift; tests/authoring-prompt.test.ts fails on
 * it.
 */
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const docs = join(here, '..', 'src', 'system', 'assets', 'docs');
const promptPath = join(docs, 'AGENT_PROMPT.md');
const referencePath = join(docs, 'DML_REFERENCE.md');

/** The appendix marker: the first top-level reference heading. */
export const APPENDIX_MARKER = '# DML Language Reference\n';

/** The behavioural half of the prompt: everything before the appendix. */
export function behaviouralPrefix(prompt) {
  const index = prompt.indexOf(APPENDIX_MARKER);
  if (index === -1) {
    throw new Error('AGENT_PROMPT.md does not contain the reference appendix marker');
  }
  return prompt.slice(0, index);
}

/** The prompt as it should be: behavioural half + the reference. */
export function expectedPrompt(prompt, reference) {
  const prefix = behaviouralPrefix(prompt);
  const body = reference.replace(/\s*$/, '');
  return `${prefix}${body}\n`;
}

const prompt = readFileSync(promptPath, 'utf8');
const reference = readFileSync(referencePath, 'utf8');
const expected = expectedPrompt(prompt, reference);

if (process.argv.includes('--check')) {
  if (prompt !== expected) {
    console.error('AGENT_PROMPT.md is out of sync with DML_REFERENCE.md — run: npm run sync:agent-prompt');
    process.exit(1);
  }
  console.log('AGENT_PROMPT.md is in sync with DML_REFERENCE.md');
} else {
  writeFileSync(promptPath, expected);
  console.log(`synced AGENT_PROMPT.md (${expected.length} bytes)`);
}
