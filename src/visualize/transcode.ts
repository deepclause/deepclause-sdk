/**
 * Optional raster transcoding (JPEG/WebP/GIF → PNG) for terminal display.
 *
 * PNG passes through untouched and SVG is handled by resvg (`raster.ts`); the
 * other formats need a decoder. `sharp` is loaded lazily and is not a hard
 * dependency — when it is unavailable those formats fall back to the
 * path + external-open behaviour.
 */

type SharpModule = {
  default?: (input: Buffer) => { png(): { toBuffer(): Promise<Buffer> } };
  (input: Buffer): { png(): { toBuffer(): Promise<Buffer> } };
};

async function loadSharp(): Promise<SharpModule | undefined> {
  // Variable specifier keeps this compiling when the optional dep is absent.
  const specifier = 'sharp';
  try {
    return (await import(specifier)) as unknown as SharpModule;
  } catch {
    return undefined;
  }
}

/** True when a raster transcoder is available. */
export async function transcoderAvailable(): Promise<boolean> {
  return (await loadSharp()) !== undefined;
}

/** Transcode arbitrary raster bytes to PNG, or undefined when unsupported. */
export async function transcodeToPng(bytes: Buffer, mimeType: string): Promise<Buffer | undefined> {
  if (mimeType === 'image/png') {
    return bytes;
  }
  const sharp = await loadSharp();
  if (!sharp) {
    return undefined;
  }
  try {
    const factory = sharp.default ?? sharp;
    return await factory(bytes).png().toBuffer();
  } catch {
    return undefined;
  }
}
