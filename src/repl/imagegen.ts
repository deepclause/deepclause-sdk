/**
 * Image generation over pi's image models.
 *
 * Implements the injectable `ImageBackend` used by `task(…, media(X))`. It reuses
 * pi's credential store and model catalog (the same one the chat backend uses),
 * picks the first available image model unless one is configured, and writes the
 * result into the workspace media directory. No separate API keys.
 */

import * as fs from 'fs/promises';
import * as path from 'path';

import type { ImageBackend, ImageGenerationRequest, ImageGenerationResult } from '../types.js';
import { openPiRuntime, type PiRuntimeHandle } from './backend.js';

const EXT_BY_MIME: Record<string, string> = {
  'image/png': 'png',
  'image/jpeg': 'jpg',
  'image/webp': 'webp',
  'image/gif': 'gif',
};

export interface PiImageBackendOptions {
  workspacePath: string;
  agentDir?: string;
  modelsPath?: string | null;
  authPath?: string;
  /** Explicit image model as `provider:id` (or `provider/id`). */
  model?: string;
  /** Workspace-relative output directory (default `.deepclause/media`). */
  dir?: string;
  size?: number;
}

function parseModelRef(ref: string | undefined): { provider: string; id: string } | undefined {
  if (!ref) {
    return undefined;
  }
  const match = ref.match(/^([^:/]+)[:/](.+)$/);
  return match ? { provider: match[1], id: match[2] } : undefined;
}

/** List the ids of available image models (for diagnostics / `/gen`). */
export async function listImageModels(handle: PiRuntimeHandle): Promise<string[]> {
  const runtime = handle.runtime;
  if (typeof runtime.getAvailableOfType !== 'function') {
    return [];
  }
  const models = (await runtime.getAvailableOfType('image')) as Array<{ provider?: string; id?: string }>;
  return models.map((m) => (m.provider && m.id ? `${m.provider}:${m.id}` : String(m)));
}

/**
 * Build a pi-backed image generator, or undefined when the installed pi runtime
 * cannot generate images (no image-capable provider / missing API).
 */
export async function createPiImageBackend(options: PiImageBackendOptions): Promise<ImageBackend | undefined> {
  const handle = await openPiRuntime({
    agentDir: options.agentDir,
    modelsPath: options.modelsPath ?? undefined,
    authPath: options.authPath,
  });
  const runtime = handle.runtime;
  if (typeof runtime.getAvailableOfType !== 'function' || typeof runtime.generateImages !== 'function') {
    return undefined;
  }

  const dir = options.dir ?? '.deepclause/media';

  return {
    async generate(request: ImageGenerationRequest): Promise<ImageGenerationResult> {
      const available = (await runtime.getAvailableOfType!('image')) as Array<Record<string, unknown>>;
      if (available.length === 0) {
        throw new Error('no image model is available — check that a provider with image generation is signed in');
      }
      const wanted = parseModelRef(options.model);
      let model: unknown = available[0];
      if (wanted && typeof runtime.getModelOfType === 'function') {
        const found = runtime.getModelOfType('image', wanted.provider, wanted.id);
        if (found) {
          model = found;
        }
      }

      const input: Array<Record<string, unknown>> = [{ type: 'text', text: request.prompt }];
      for (const ref of request.refs ?? []) {
        const abs = path.resolve(options.workspacePath, ref.path);
        const bytes = await fs.readFile(abs);
        input.push({ type: 'image', data: bytes.toString('base64'), mimeType: ref.mimeType });
      }

      const result = await runtime.generateImages!(model, { input }, request.signal ? { signal: request.signal } : undefined);
      const image = (result.output ?? []).find((part) => part.type === 'image' && part.data);
      if (!image?.data) {
        throw new Error(result.errorMessage || 'image generation returned no image');
      }
      const mimeType = image.mimeType ?? 'image/png';
      const ext = EXT_BY_MIME[mimeType] ?? 'png';
      const rel = path.posix.join(dir, `image-${Date.now()}.${ext}`);
      const abs = path.resolve(options.workspacePath, rel);
      await fs.mkdir(path.dirname(abs), { recursive: true });
      await fs.writeFile(abs, Buffer.from(image.data, 'base64'));
      return { path: rel, mimeType };
    },
  };
}
