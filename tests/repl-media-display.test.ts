/**
 * Media in DML: `output(media(...))`, `save_image/2`, and `task(…, media(X))`
 * image generation with an injected backend.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { rasterizerAvailable, resolveMedia } from '../src/visualize/index.js';
import type { DMLEvent, ImageBackend, ImageGenerationRequest } from '../src/types.js';

const SVG = "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'/>";

let workspace: string;
let repl: HarnessRepl;
let generated: ImageGenerationRequest[];

function cannedImageBackend(): ImageBackend {
  return {
    async generate(request: ImageGenerationRequest) {
      generated.push(request);
      return { path: 'generated/pic.svg', mimeType: 'image/svg+xml' };
    },
  };
}

async function collect(gen: AsyncGenerator<DMLEvent>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of gen) {
    events.push(event);
  }
  return events;
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-media-'));
  generated = [];
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    imageBackend: cannedImageBackend(),
  });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('output(media(...))', () => {
  it('emits a media event for an inline SVG', async () => {
    const events = await collect(repl.query(`output(media{svg: "${SVG}"})`));
    const media = events.find((event) => event.type === 'media');
    expect(media).toBeDefined();
    expect(media?.media?.mimeType).toBe('image/svg+xml');
    // ASCII/plain renderers still produce the base64 PNG when a rasterizer exists.
    if (media?.media?.data) {
      expect(media.media.data.length).toBeGreaterThan(0);
    }
  });

  it('emits a media event with a path for a workspace file', async () => {
    await fs.writeFile(path.join(workspace, 'pic.svg'), SVG, 'utf8');
    const events = await collect(repl.query('output(media("pic.svg"))'));
    const media = events.find((event) => event.type === 'media');
    expect(media?.media?.path).toBe('pic.svg');
    expect(media?.media?.mimeType).toBe('image/svg+xml');
  });

  it('does not treat a plain output string as media', async () => {
    const events = await collect(repl.query('output("hello world")'));
    expect(events.some((event) => event.type === 'media')).toBe(false);
    expect(events.some((event) => event.type === 'output' && event.content === 'hello world')).toBe(true);
  });
});

describe('save_image/2', () => {
  it('writes an inline SVG to a workspace file', async () => {
    await fs.mkdir(path.join(workspace, 'out'), { recursive: true });
    await collect(repl.query(`save_image(media{svg: "${SVG}"}, "out/saved.svg")`));
    const written = await fs.readFile(path.join(workspace, 'out/saved.svg'), 'utf8');
    expect(written).toContain('<svg');
  });

  it('copies a media file reference', async () => {
    await fs.writeFile(path.join(workspace, 'src.svg'), SVG, 'utf8');
    await collect(repl.query('save_image(media("src.svg"), "copy.svg")'));
    const copied = await fs.readFile(path.join(workspace, 'copy.svg'), 'utf8');
    expect(copied).toContain('<svg');
  });
});

describe('task(…, media(X))', () => {
  it('generates and binds the path without a model call', async () => {
    const events = await collect(repl.query('task("a red fox in snow", media(Img))'));
    const binding = events
      .filter((event) => event.type === 'solution')
      .flatMap((event) => event.solution?.bindings ?? [])
      .find((b) => b.name === 'Img');
    expect(binding?.value).toContain('generated/pic.svg');
    expect(generated).toHaveLength(1);
    expect(generated[0].prompt).toContain('a red fox');
  });

  it('fails cleanly when no image backend is configured', async () => {
    const plain = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
    });
    try {
      const events = await collect(plain.query('task("a cat", media(Img))'));
      expect(events.some((event) => event.type === 'error')).toBe(true);
    } finally {
      await plain.dispose();
    }
  });
});

describe('resolveMedia', () => {
  it('rasterizes inline SVG to a PNG', async () => {
    const resolved = await resolveMedia(workspace, { kind: 'inline_svg', data: SVG, mimeType: 'image/svg+xml' });
    expect(resolved?.mimeType).toBe('image/svg+xml');
    if (resolved?.pngBase64) {
      expect(resolved.pngBase64.startsWith('iVBOR')).toBe(true);
    }
  });
});

describe('agent showImage', () => {
  it('returns a media event with a rasterized body for an SVG', async () => {
    await fs.writeFile(path.join(workspace, 'pic.svg'), SVG, 'utf8');
    const { message, event } = await repl.session.showImage('pic.svg');
    expect(event?.type).toBe('media');
    expect(event?.media?.path).toBe('pic.svg');
    expect(event?.media?.mimeType).toBe('image/svg+xml');
    if (await rasterizerAvailable()) {
      expect(event?.media?.data?.startsWith('iVBOR')).toBe(true);
      expect(message).toContain('displayed');
    } else {
      expect(message).toContain('not rasterized');
    }
  });

  it('reports a missing file', async () => {
    const { message, event } = await repl.session.showImage('missing.png');
    expect(event?.media?.path).toBe('missing.png');
    expect(message).toContain('at workspace path');
  });
});
