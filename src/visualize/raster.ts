/**
 * Rasterize our generated SVG to PNG for the terminal-image path.
 *
 * `@resvg/resvg-js` is an optional dependency: it is a native module with
 * prebuilt binaries, so it is loaded lazily and the caller falls back to the
 * ASCII renderer when it is unavailable. No browser, no `mmdc`.
 */

export interface RasterOptions {
  /** Logical SVG width in px (from `renderFlowchartSvg`). */
  width: number;
  /** Logical SVG height in px. */
  height: number;
  /** Device scale factor (default 2 for crisp text). */
  scale?: number;
}

interface ResvgLike {
  render(): { asPng(): Uint8Array };
}

interface ResvgModule {
  Resvg?: new (svg: string, options?: Record<string, unknown>) => ResvgLike;
}

/** True when a PNG rasterizer is installed and importable. */
export async function rasterizerAvailable(): Promise<boolean> {
  const mod = await loadResvg();
  return typeof mod?.Resvg === 'function';
}

async function loadResvg(): Promise<ResvgModule | undefined> {
  // A variable specifier keeps this file compiling when the optional native
  // dependency is not installed.
  const specifier = '@resvg/resvg-js';
  try {
    return (await import(specifier)) as ResvgModule;
  } catch {
    return undefined;
  }
}

/**
 * Rasterize SVG text to a PNG buffer, or return undefined when no rasterizer is
 * available (the caller should then use `renderMermaidAscii`).
 */
export async function rasterizeSvg(svg: string, options: RasterOptions): Promise<Buffer | undefined> {
  const mod = await loadResvg();
  if (!mod?.Resvg) {
    return undefined;
  }
  const scale = options.scale ?? 2;
  const width = Math.max(1, Math.round(options.width * scale));
  try {
    const resvg = new mod.Resvg(svg, {
      fitTo: { mode: 'width', value: width },
      background: 'white',
      font: { loadSystemFonts: true },
    });
    return Buffer.from(resvg.render().asPng());
  } catch {
    return undefined;
  }
}
