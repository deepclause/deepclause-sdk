/**
 * The authoring agent's prompt is two halves: the behavioural instructions, then a
 * verbatim copy of the language reference. The copy is what the agent reads first
 * and trusts, so it must not lag the reference — five sections added in one session
 * were missing from it, leaving the agent on a stale language guide while the live
 * one was a `reference("language")` call away.
 *
 * A test rather than a review step, because drift is invisible: the prompt still
 * looks complete, it is simply missing the newest thing.
 */
import { describe, expect, it } from 'vitest';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';

import { APPENDIX_MARKER, behaviouralPrefix } from '../scripts/sync-agent-prompt.mjs';

const DOCS = join(process.cwd(), 'src', 'system', 'assets', 'docs');
const prompt = readFileSync(join(DOCS, 'AGENT_PROMPT.md'), 'utf8');
const reference = readFileSync(join(DOCS, 'DML_REFERENCE.md'), 'utf8');

describe('the authoring prompt and the language reference', () => {
  it('embeds the reference verbatim', () => {
    const appendix = prompt.slice(prompt.indexOf(APPENDIX_MARKER));
    expect(appendix.trimEnd()).toBe(reference.trimEnd());
  });

  it('keeps the behavioural half', () => {
    const prefix = behaviouralPrefix(prompt);
    expect(prefix).toContain('# The module');
    expect(prefix.length).toBeGreaterThan(1000);
    // The appendix is the only copy of the reference heading.
    expect(prefix).not.toContain(APPENDIX_MARKER);
  });

  it('names the newest surface in the behavioural half', () => {
    // The appendix carries the reference, but the *instructions* have to point at
    // the newer primitives too, or the agent never learns to reach for them. These
    // are the features added since the prompt was last reviewed end to end.
    const prefix = behaviouralPrefix(prompt);
    for (const term of [
      'schema',
      'pattern',
      'argv',
      'with_fresh_memory',
      'json_get',
      'dict_field',
      'string_chars',
    ]) {
      expect(prefix, `the behavioural half should mention ${term}`).toContain(term);
    }
  });

  it('tells the agent to edit only through the transaction tools', () => {
    const prefix = behaviouralPrefix(prompt);
    expect(prefix).toContain('Change the module only through those tools');
    expect(prefix).toContain('consult');
    // The rule names the tools it wants instead.
    for (const tool of ['begin_turn', 'define', 'assert_many', 'end_turn']) {
      expect(prefix).toContain(tool);
    }
  });

  it('tells the agent to start small and show the user', () => {
    // Scope discipline: the counterweight to everything else in the prompt, which
    // encourages reuse, generality and documentation. Without this an agent
    // generalises and abstracts before anything runs.
    const prefix = behaviouralPrefix(prompt);
    expect(prefix).toContain('Start small, then improve');
    expect(prefix).toContain('show the user');
    expect(prefix).toMatch(/smallest/);
  });

  it('carries the same rule in the authoring guide', () => {
    // DML_AUTHORING.md is not part of this prompt; it is read via
    // reference("authoring"), so it needs its own guard.
    const guide = readFileSync(join(DOCS, 'DML_AUTHORING.md'), 'utf8');
    expect(guide).toContain('**Start small, then improve.**');
    expect(guide).toContain('Signs you are overengineering');
  });

  it('says not to parse in DML, and that exec has no stdin', () => {
    const prefix = behaviouralPrefix(prompt);
    expect(prefix).toContain('Do not parse in DML');
    // The two alternatives, in order of what the work is.
    expect(prefix).toContain('helper script');
    expect(prefix).toContain('model call');
    // And the specific trap: a plausible `stdin` argument that does nothing.
    expect(prefix).toContain('no `stdin`');
    expect(prefix).toMatch(/argv/);
  });

  it('carries the same rule in the authoring guide', () => {
    const guide = readFileSync(join(DOCS, 'DML_AUTHORING.md'), 'utf8');
    expect(guide).toContain('**Do not parse in DML — shell out or ask the model.**');
  });

  it('documents the accepted bash inputs in the reference', () => {
    const reference = readFileSync(join(DOCS, 'DML_REFERENCE.md'), 'utf8');
    expect(reference).toContain('There is\n**no `stdin`**');
    expect(reference).toContain('#### Do not parse in DML');
  });
});
