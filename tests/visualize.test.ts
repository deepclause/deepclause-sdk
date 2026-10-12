/**
 * `deepclause visualize`: prompt building, Mermaid extraction/validation, output
 * path resolution, and an end-to-end run with a canned backend.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { initConfig } from '../src/cli/config.js';
import { visualizeHarness } from '../src/cli/visualize.js';
import {
  buildDiagramPrompt,
  extractMermaid,
  parseEntry,
  resolveOutputPaths,
  validateMermaid,
} from '../src/visualize/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

class CannedBackend implements LLMBackend {
  readonly calls: LLMBackendRequest[] = [];

  constructor(private readonly reply: string) {}

  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls.push(request);
    return { text: this.reply };
  }
}

const HARNESS = `
module_doc("Support triage harness.").

doc(router/2, "Route a message. router(+Message, -Team).").

router(Message, Team) :-
    classify(Message, Team),
    ( Team == billing -> output("billing") ; output("other") ).

doc(classify/2, "Classify a message. classify(+Message, -Team).").

classify(Message, Team) :-
    task("Route this message to billing, orders, or account: {Message}", Team).
`;

let workspace: string;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-visualize-'));
  await initConfig(workspace);
});

afterEach(async () => {
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('visualize helpers', () => {
  it('builds a mode-specific prompt with the listing and entry', () => {
    const listing = 'router(A, B) :- task("x", B).';
    const presentation = buildDiagramPrompt({ entry: 'router/2', mode: 'presentation', listing });
    expect(presentation).toContain('router/2');
    expect(presentation).toContain('KIND: presentation');
    expect(presentation).toContain(listing);

    const spec = buildDiagramPrompt({ entry: 'router/2', mode: 'spec', listing });
    expect(spec).toContain('KIND: specification');
    expect(spec).toContain('fallback');
  });

  it('extracts mermaid from fences and leading prose', () => {
    expect(extractMermaid('Here you go:\n```mermaid\nflowchart TD\n  A-->B\n```\n')).toBe('flowchart TD\n  A-->B');
    expect(extractMermaid('flowchart LR\n  A-->B')).toBe('flowchart LR\n  A-->B');
  });

  it('validates mermaid lightly', () => {
    expect(validateMermaid('flowchart TD\n  A-->B')).toEqual([]);
    expect(validateMermaid('nonsense')).toEqual([expect.stringContaining('unrecognized')]);
    expect(validateMermaid('flowchart TD\n  A["broken"')).toEqual([expect.stringContaining('unbalanced')]);
    expect(validateMermaid('')).toEqual(['the diagram is empty']);
  });

  it('parses entries and output paths', () => {
    expect(parseEntry('router/2')).toEqual({ name: 'router', arity: 2 });
    expect(parseEntry('router')).toEqual({ name: 'router', arity: undefined });
    expect(() => parseEntry('router/x')).toThrow(/invalid entry/);

    expect(resolveOutputPaths('diagram.png', 'router/2')).toEqual({ mmdPath: 'diagram.mmd', imagePath: 'diagram.png' });
    expect(resolveOutputPaths('flows.mmd', 'router/2')).toEqual({ mmdPath: 'flows.mmd' });
    expect(resolveOutputPaths(undefined, 'router/2')).toEqual({ mmdPath: 'router_2.mmd' });
  });
});

describe('visualizeHarness', () => {
  it('writes a .mmd produced by the model (canned backend)', async () => {
    const dmlPath = path.join(workspace, 'harness.dml');
    await fs.writeFile(dmlPath, HARNESS);
    const outPath = path.join(workspace, 'router.mmd');
    const backend = new CannedBackend('```mermaid\nflowchart TD\n  A["router"] --> B["classify"]\n```');

    const result = await visualizeHarness(dmlPath, 'router/2', {
      mode: 'presentation',
      output: outPath,
      workspace,
      llmBackend: backend,
    });

    expect(result.entry).toBe('router/2');
    expect(result.mode).toBe('presentation');
    expect(result.warnings).toEqual([]);
    expect(result.imagePath).toBeUndefined();
    expect(result.mermaid).toContain('flowchart TD');
    expect(result.ascii).toContain('router');
    expect(result.svg).toContain('<svg');
    expect(await fs.readFile(outPath, 'utf8')).toContain('router');

    expect(backend.calls).toHaveLength(1);
    const prompt = backend.calls[0].messages[0].content;
    expect(prompt).toContain('Support triage harness.');
    expect(prompt).toContain('classify');
  });

  it('rejects an entry the harness does not define', async () => {
    const dmlPath = path.join(workspace, 'harness.dml');
    await fs.writeFile(dmlPath, HARNESS);

    await expect(visualizeHarness(dmlPath, 'missing/1', {
      mode: 'spec',
      output: path.join(workspace, 'x.mmd'),
      workspace,
      llmBackend: new CannedBackend('flowchart TD'),
    })).rejects.toThrow(/does not define/);
  });

  it('reports a wrong arity for an existing predicate', async () => {
    const dmlPath = path.join(workspace, 'harness.dml');
    await fs.writeFile(dmlPath, HARNESS);

    await expect(visualizeHarness(dmlPath, 'router/3', {
      mode: 'spec',
      output: path.join(workspace, 'x.mmd'),
      workspace,
      llmBackend: new CannedBackend('flowchart TD'),
    })).rejects.toThrow(/not router\/3/);
  });

  it('writes an SVG without a browser (canned backend)', async () => {
    const dmlPath = path.join(workspace, 'harness.dml');
    await fs.writeFile(dmlPath, HARNESS);
    const outPath = path.join(workspace, 'router.svg');
    const backend = new CannedBackend('flowchart TD\n  A["router"] --> B{"classify"}\n  B -->|billing| C["billing"]');

    const result = await visualizeHarness(dmlPath, 'router/2', {
      mode: 'presentation',
      output: outPath,
      workspace,
      llmBackend: backend,
    });

    expect(result.imagePath).toBe(outPath);
    expect(result.renderer).toBe('svg');
    expect(await fs.readFile(outPath, 'utf8')).toContain('<svg');
  });
});
