import { describe, expect, it } from 'vitest';
import { readFile } from 'node:fs/promises';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl } from '../src/repl/index.js';
import { getWorkspaceDocAssetPath } from '../src/system/assets/index.js';
import { REFERENCE_TOPICS, REFERENCE_PAGE_LIMIT, referenceSections, referenceIndex, renderReference } from '../src/repl/reference.js';
import type { LLMBackendRequest } from '../src/types.js';

function nextOffset(text: string): number | undefined {
  const match = /Continue with reference\((\{.*\})\)/.exec(text);
  return match ? JSON.parse(match[1]).offset : undefined;
}
function expectSafe(text: string): void {
  expect(JSON.stringify({ text }).length).toBeLessThanOrEqual(REFERENCE_PAGE_LIMIT);
  expect(text).not.toContain('[truncated]');
  expect((text.match(/^```/gm) ?? []).length % 2).toBe(0);
}

describe('focused bounded reference pages', () => {
  it('keeps the top-level topic map within the actual response budget', () => {
    expectSafe(referenceIndex());
    expect(referenceIndex()).toContain('processes');
    expect(referenceIndex()).toContain('media');
  });

  it('discovers every topic/section including formerly unreachable processes/media', async () => {
    for (const topic of REFERENCE_TOPICS) {
      const doc = await readFile(getWorkspaceDocAssetPath(topic.asset), 'utf8');
      expectSafe(renderReference(topic.topic, doc));
      for (const section of referenceSections(doc)) {
        let offset: number | undefined;
        let count = 0;
        do {
          const text = renderReference(topic.topic, doc, { section: section.id, offset });
          expectSafe(text);
          offset = nextOffset(text);
          expect(++count).toBeLessThan(30);
        } while (offset !== undefined);
      }
    }
  });

  it('pages whole blocks with a usable continuation and a stable revision', () => {
    const doc = '# Test\n\n## Long section\n\n' + Array.from({ length: 15 }, (_, i) => `Paragraph ${i}: ${'x'.repeat(700)}\n\n`).join('') + 'FINAL_SENTINEL';
    let text = renderReference('test', doc, { section: 'long-section' });
    const first = text;
    while (nextOffset(text) !== undefined) text = renderReference('test', doc, { section: 'long-section', offset: nextOffset(text) });
    expect(first).toContain('More: yes'); expectSafe(first); expectSafe(text);
    expect(text).toContain('FINAL_SENTINEL'); expect(text).toContain('More: no.');
  });

  it('ignores headings inside code fences and searches sections', () => {
    const doc = '# Test\n## Real\n\n```python\n## Not a section\nprint(1)\n```\n\n## Images\nVision is a capability.';
    expect(referenceSections(doc).map((s) => s.id)).toEqual(['real', 'images']);
    expect(renderReference('test', doc, { query: 'vision' })).toContain('section="images"');
  });

  it('rejects oversized code blocks or invalid cursors instead of cutting code', () => {
    expect(() => renderReference('test', '# Test\n## Big\n```text\n' + 'x'.repeat(8000) + '\n```', { section: 'big' })).toThrow(/budget/);
    expect(() => renderReference('test', '# Test\n## One\nhello', { section: 'unknown' })).toThrow(/Unknown section/);
    expect(() => renderReference('test', '# Test\n## One\nhello', { section: 'one', offset: -1 })).toThrow(/offset/);
  });

  it('uses the registered host schemas rather than a guessed capability list', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-reference-catalog-'));
    const repl = await createHarnessRepl({ workspacePath: workspace, model: 'test', provider: 'openai',
      hostToolsCatalog: 'Only the fixture Python tool is registered.',
      tools: new Map([['python', { description: 'Fixture Python', parameters: { type: 'object', properties: { code: { type: 'string' }, argv: { type: 'array' } } }, execute: async () => ({}) }]]),
    });
    try {
      expect(await repl.session.reference('host-tools')).toContain('- python:');
      const schema = await repl.session.reference('tools', { section: 'python' });
      expect(schema).toContain('"code"'); expect(schema).toContain('"argv"');
      expect(schema).not.toContain('"stdin"'); expectSafe(schema);
    } finally { await repl.dispose(); await fs.rm(workspace, { recursive: true, force: true }); }
  });

  it('delivers late feature content through the ACTUAL model-facing tool serialization', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-reference-wire-'));
    const calls: LLMBackendRequest[] = [];
    const repl = await createHarnessRepl({
      workspacePath: workspace, model: 'test-model', provider: 'openai',
      llmBackend: { complete: async (request) => {
        calls.push(request);
        return calls.length === 1
          ? { text: '', toolCalls: [{ id: 'ref', name: 'reference', arguments: { topic: 'processes', section: 'lifecycle-and-cleanup' } }] }
          : { text: 'Read the contract.' };
      } },
    });
    try {
      for await (const _event of repl.prompt('Read process cleanup.')) { /* consume */ }
      const message = calls[1].messages.find((m) => m.role === 'tool' && m.toolName === 'reference');
      expect(message?.content).not.toContain('[truncated]');
      const text = JSON.parse(message!.content).text;
      expect(text).toContain('Stop/reap'); expect(text).toContain('worker-hosted'); expectSafe(text);
      expect(await repl.session.reference('dml', { section: 'processes' })).toContain('swarm');
      await expect(repl.session.reference('invented')).rejects.toThrow(/Unknown reference topic/);
    } finally { await repl.dispose(); await fs.rm(workspace, { recursive: true, force: true }); }
  });
});
