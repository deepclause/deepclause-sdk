/**
 * Terminal visualization: Mermaid → ASCII tree / SVG, terminal-image encoding
 * (pi-tui Kitty/iTerm2), the `/viz` command classifier, and an end-to-end
 * `HarnessRepl.diagram` run with a canned backend.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';
import { resetCapabilitiesCache, setCapabilities } from '@earendil-works/pi-tui';

import {
  parseMermaidFlowchart,
  rasterizerAvailable,
  renderFlowchartSvg,
  renderMermaidAscii,
  renderTerminalImage,
  resolveGraphicsProtocol,
} from '../src/visualize/index.js';
import { classifyReplInput, parseVizArgs } from '../src/cli/repl.js';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

const MERMAID = [
  'flowchart TD',
  'A["Load system prompt"] --> B{"Which team?"}',
  'B -->|billing| C["Refund queue"]',
  'B -->|orders| D["Orders queue"]',
  'C --> E["answer(D)"]',
  'D --> E',
].join('\n');

/** A 24-byte buffer with a valid PNG signature + IHDR dimensions. */
function tinyPng(width = 2, height = 2): Buffer {
  const buf = Buffer.alloc(24);
  Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]).copy(buf, 0);
  buf.writeUInt32BE(width, 16);
  buf.writeUInt32BE(height, 20);
  return buf;
}

class CannedBackend implements LLMBackend {
  readonly calls: LLMBackendRequest[] = [];
  constructor(private readonly reply: string) {}
  async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
    this.calls.push(request);
    return { text: this.reply };
  }
}

describe('mermaid → ASCII tree', () => {
  it('parses nodes, shapes, edge labels and chained edges', () => {
    const parsed = parseMermaidFlowchart(MERMAID);
    expect(parsed.direction).toBe('TD');
    expect(parsed.nodes.find((n) => n.id === 'B')?.shape).toBe('diamond');
    expect(parsed.nodes.find((n) => n.id === 'A')?.label).toBe('Load system prompt');
    expect(parsed.edges).toHaveLength(5);
    expect(parsed.edges.find((e) => e.to === 'C')?.label).toBe('billing');
  });

  it('renders an indented flow with a cycle marker', () => {
    const ascii = renderMermaidAscii(MERMAID);
    expect(ascii.split('\n')[0]).toBe('Load system prompt');
    expect(ascii).toContain('└─▶ Which team?');
    expect(ascii).toContain('(billing) Refund queue');
    expect(ascii).toContain('↻');
  });

  it('falls back to the raw source when nothing parses', () => {
    expect(renderMermaidAscii('not a diagram')).toBe('not a diagram');
  });
});

describe('mermaid → SVG', () => {
  it('produces a self-contained SVG with every label', () => {
    const { svg, width, height, nodes } = renderFlowchartSvg(parseMermaidFlowchart(MERMAID));
    expect(svg.startsWith('<svg')).toBe(true);
    expect(svg).toContain('</svg>');
    expect(svg).toContain('Refund queue');
    expect(svg).not.toContain('"Segoe UI"'); // no raw quotes inside attributes
    expect(width).toBeGreaterThan(200);
    expect(height).toBeGreaterThan(100);
    expect(nodes).toBe(6);
  });
});

describe('graphics protocol selection', () => {
  afterEach(() => {
    resetCapabilitiesCache();
  });

  it('honours explicit modes and detection', () => {
    setCapabilities({ images: 'kitty', trueColor: true, hyperlinks: true });
    expect(resolveGraphicsProtocol('auto')).toBe('kitty');
    expect(resolveGraphicsProtocol('ascii')).toBeUndefined();
    expect(resolveGraphicsProtocol('off')).toBeUndefined();
    expect(resolveGraphicsProtocol('iterm2')).toBe('iterm2');

    setCapabilities({ images: null, trueColor: true, hyperlinks: false });
    expect(resolveGraphicsProtocol('auto')).toBeUndefined();
    expect(resolveGraphicsProtocol('kitty')).toBe('kitty');
  });

  it('encodes a PNG for kitty and iTerm2', () => {
    const png = tinyPng();
    const kitty = renderTerminalImage(png, { protocol: 'kitty', maxWidthCells: 80 });
    expect(kitty?.protocol).toBe('kitty');
    expect(kitty?.lines[0].startsWith('\x1b_G')).toBe(true);
    expect(kitty?.lines.length).toBe(kitty?.rows);

    const iterm = renderTerminalImage(png, { protocol: 'iterm2', maxWidthCells: 80 });
    expect(iterm?.protocol).toBe('iterm2');
    expect(iterm?.lines.join('')).toContain('\x1b]1337;File=');
  });
});

describe('/viz classification', () => {
  it('parses entry, spec and graphics flags', () => {
    expect(parseVizArgs('patch/3 --spec --graphics=off')).toEqual({ entry: 'patch/3', mode: 'spec', graphics: 'off' });
    expect(parseVizArgs('--graphics kitty agent_main/2')).toEqual({ entry: 'agent_main/2', mode: 'presentation', graphics: 'kitty' });
    expect(parseVizArgs('')).toEqual({ entry: undefined, mode: 'presentation', graphics: 'auto' });
  });

  it('classifies /viz (and the /diagram alias)', () => {
    expect(classifyReplInput('/viz')).toMatchObject({ kind: 'viz', mode: 'presentation', graphics: 'auto' });
    expect(classifyReplInput('/viz research/2 --spec')).toMatchObject({ kind: 'viz', entry: 'research/2', mode: 'spec' });
    expect(classifyReplInput('/diagram router/2 --ascii')).toMatchObject({ kind: 'viz', entry: 'router/2', graphics: 'ascii' });
  });
});

describe('HarnessRepl.diagram', () => {
  let workspace: string;
  let repl: HarnessRepl;
  let backend: CannedBackend;

  beforeEach(async () => {
    workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-viz-'));
    backend = new CannedBackend(MERMAID);
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
    });
    await repl.session.loadCode(`
module_doc("Support triage harness.").
doc(router/2, "Route a message. router(+Message, -Team).").
router(Message, Team) :- classify(Message, Team).
classify(Message, Team) :- task("Route {Message}", Team).
`);
  });

  afterEach(async () => {
    await repl.dispose();
    await fs.rm(workspace, { recursive: true, force: true });
  });

  it('returns mermaid, ASCII and SVG from one model call', async () => {
    const result = await repl.diagram('router/2');
    expect(backend.calls).toHaveLength(1);
    expect(result.entry).toBe('router/2');
    expect(result.ascii).toContain('Which team?');
    expect(result.svg).toContain('<svg');
    expect(result.width).toBeGreaterThan(0);
    expect(result.mermaid).toContain('flowchart');
  });

  it('defaults to a real predicate when no entry is given', async () => {
    const result = await repl.diagram();
    expect(['router/2', 'classify/2']).toContain(result.entry);
  });

  it('returns a media event with a rasterized body for the diagram', async () => {
    const { message, event } = await repl.session.visualize('router/2');
    expect(event?.type).toBe('media');
    expect(event?.media?.mimeType).toBe('image/svg+xml');
    if (await rasterizerAvailable()) {
      expect(event?.media?.data?.startsWith('iVBOR')).toBe(true);
      expect(message).toContain('displayed to the user');
    } else {
      expect(message).toContain('Which team?');
    }
  });
});
