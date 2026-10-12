/**
 * Shared "diagram → terminal image" step.
 *
 * Both `/viz` (user-typed) and the authoring agent's `visualize` tool use this,
 * so a diagram is painted the same way everywhere: resolve the terminal's image
 * protocol, rasterize the SVG (no browser), and encode an inline image. Returns
 * undefined when the terminal has no image support or no rasterizer is
 * installed, so the caller can fall back to the ASCII tree.
 */

import { rasterizeSvg } from './raster.js';
import { renderTerminalImage, resolveGraphicsProtocol, type GraphicsMode } from './graphics.js';

export interface DiagramImageSource {
  svg: string;
  width: number;
  height: number;
}

export interface DiagramImageOptions {
  /** `auto` (default) or a forced protocol; `ascii`/`off` disable the image. */
  graphics?: GraphicsMode;
  maxWidthCells?: number;
  maxHeightCells?: number;
}

/** Encode a diagram as inline terminal-image lines, or undefined. */
export async function renderDiagramImage(
  source: DiagramImageSource,
  options: DiagramImageOptions = {},
): Promise<string[] | undefined> {
  const protocol = resolveGraphicsProtocol(options.graphics ?? 'auto');
  if (!protocol) {
    return undefined;
  }
  const png = await rasterizeSvg(source.svg, { width: source.width, height: source.height });
  if (!png) {
    return undefined;
  }
  const image = renderTerminalImage(png, {
    protocol,
    maxWidthCells: options.maxWidthCells ?? 80,
    maxHeightCells: options.maxHeightCells,
  });
  return image?.lines;
}
