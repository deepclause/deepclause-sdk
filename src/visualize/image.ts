/**
 * Resolve a media reference (`output(media(...))`, a file, inline SVG, or inline
 * bytes) into a base64 PNG for terminal display, and to bytes for saving.
 *
 * Same pipeline as the diagram renderer: SVG → resvg, PNG passthrough, other
 * rasters → optional `sharp` (`transcode.ts`). When no raster can be produced,
 * the caller falls back to showing the path.
 */

import * as fs from 'fs/promises';
import * as path from 'path';

import { rasterizeSvg } from './raster.js';
import { transcodeToPng } from './transcode.js';

export interface MediaInput {
  /** Workspace-relative path (for file references). */
  path?: string;
  mimeType?: string;
  /** `file` (default) | `inline_svg` | `inline_data`. */
  kind?: string;
  /** SVG source text (`inline_svg`) or base64 bytes (`inline_data`). */
  data?: string;
}

export interface ResolvedMedia {
  mimeType: string;
  relPath?: string;
  absPath?: string;
  /** Base64 PNG when one could be produced (for inline terminal display). */
  pngBase64?: string;
  /** Raw bytes when read from a file. */
  bytes?: Buffer;
}

const DEFAULT_SVG_WIDTH = 900;

const MIME_BY_EXT: Record<string, string> = {
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.gif': 'image/gif',
  '.webp': 'image/webp',
  '.svg': 'image/svg+xml',
};

/** Mime for a reference: explicit wins, else inferred from the extension. */
function inferMime(relPath: string | undefined, given: string | undefined): string {
  if (given) {
    return given;
  }
  const ext = relPath ? path.extname(relPath).toLowerCase() : '';
  return MIME_BY_EXT[ext] ?? 'image/png';
}

function isSvg(mimeType: string | undefined, source: string): boolean {
  return mimeType === 'image/svg+xml' || /^\s*<svg[\s>]/i.test(source);
}

/** Resolve a media reference to (optionally) a base64 PNG + path metadata. */
export async function resolveMedia(workspacePath: string, media: MediaInput): Promise<ResolvedMedia | undefined> {
  const mimeType = inferMime(media.path, media.mimeType);

  // Inline SVG text or inline bytes.
  if (media.kind === 'inline_svg' || (media.data && isSvg(mimeType, media.data))) {
    const png = await rasterizeSvg(media.data ?? '', { width: DEFAULT_SVG_WIDTH, height: 0 });
    return { mimeType: 'image/svg+xml', pngBase64: png?.toString('base64') };
  }
  if (media.kind === 'inline_data' || (media.data && !media.path)) {
    const bytes = Buffer.from(media.data ?? '', 'base64');
    const png = await transcodeToPng(bytes, mimeType);
    return { mimeType, pngBase64: png?.toString('base64'), bytes };
  }

  if (!media.path) {
    return undefined;
  }
  const absPath = path.resolve(workspacePath, media.path);
  let bytes: Buffer;
  try {
    bytes = await fs.readFile(absPath);
  } catch {
    return { mimeType, relPath: media.path, absPath };
  }

  if (mimeType === 'image/svg+xml') {
    const png = await rasterizeSvg(bytes.toString('utf8'), { width: DEFAULT_SVG_WIDTH, height: 0 });
    return { mimeType, relPath: media.path, absPath, bytes, pngBase64: png?.toString('base64') };
  }
  const png = await transcodeToPng(bytes, mimeType);
  return { mimeType, relPath: media.path, absPath, bytes, pngBase64: png?.toString('base64') };
}

/**
 * Materialize a media reference to a workspace file. Inline SVG/bytes and file
 * references are written to `outPath` (workspace-relative). When the target is
 * `.svg`, an inline SVG is written verbatim; otherwise raster content is written.
 */
export async function saveMedia(
  workspacePath: string,
  media: MediaInput,
  outPath: string,
): Promise<{ path: string; mimeType: string }> {
  const resolved = await resolveMedia(workspacePath, media);
  if (!resolved) {
    throw new Error('empty media reference');
  }
  const abs = path.resolve(workspacePath, outPath);
  await fs.mkdir(path.dirname(abs), { recursive: true });

  if (outPath.toLowerCase().endsWith('.svg')) {
    const svg = media.kind === 'inline_svg' || isSvg(media.mimeType, media.data ?? '')
      ? (media.data ?? '')
      : resolved.bytes?.toString('utf8');
    if (svg) {
      await fs.writeFile(abs, svg, 'utf8');
      return { path: outPath, mimeType: 'image/svg+xml' };
    }
  }

  if (resolved.pngBase64) {
    await fs.writeFile(abs, Buffer.from(resolved.pngBase64, 'base64'));
    return { path: outPath, mimeType: 'image/png' };
  }
  if (resolved.bytes) {
    await fs.writeFile(abs, resolved.bytes);
    return { path: outPath, mimeType: resolved.mimeType };
  }
  throw new Error(`cannot save media to ${outPath}`);
}
