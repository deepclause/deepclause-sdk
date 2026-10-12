/**
 * Terminal graphics over `@earendil-works/pi-tui`'s `terminal-image` module.
 *
 * We reuse pi's Kitty / iTerm2 inline-image encoding and its capability
 * detection (Kitty, Ghostty, WezTerm, iTerm2 — tmux-aware); when the terminal
 * advertises no image protocol the caller falls back to `renderMermaidAscii`.
 *
 * The returned `lines` mirror pi-tui's `Image` component: the escape sequence
 * plus the empty padding rows the TUI must reserve for the image.
 */

import {
  allocateImageId,
  encodeITerm2,
  encodeKitty,
  getCapabilities,
  getCellDimensions,
  getPngDimensions,
  renderImage,
} from '@earendil-works/pi-tui';

/** User-facing graphics selection (`/viz --graphics=<mode>`). */
export type GraphicsMode = 'auto' | 'kitty' | 'iterm2' | 'ascii' | 'off';

export type GraphicsProtocol = 'kitty' | 'iterm2';

/** Resolve the protocol to use for a mode, or undefined for ASCII/no image. */
export function resolveGraphicsProtocol(mode: GraphicsMode = 'auto'): GraphicsProtocol | undefined {
  if (mode === 'off' || mode === 'ascii') {
    return undefined;
  }
  if (mode === 'kitty' || mode === 'iterm2') {
    return mode;
  }
  const detected = getCapabilities().images;
  return detected ?? undefined;
}

/** True when the current terminal can display inline images. */
export function graphicsSupported(): boolean {
  return resolveGraphicsProtocol('auto') !== undefined;
}

/**
 * Fit an image (px) into a cell box, mirroring pi-tui's `calculateImageCellSize`
 * (which the package does not re-export from its index).
 */
function fitToCells(
  dimensions: { widthPx: number; heightPx: number },
  maxWidthCells: number,
  maxHeightCells?: number,
): { columns: number; rows: number } {
  const cell = getCellDimensions();
  const maxWidth = Math.max(1, Math.floor(maxWidthCells));
  const maxHeight = maxHeightCells === undefined ? undefined : Math.max(1, Math.floor(maxHeightCells));
  const imageWidth = Math.max(1, dimensions.widthPx);
  const imageHeight = Math.max(1, dimensions.heightPx);
  const widthScale = (maxWidth * cell.widthPx) / imageWidth;
  const heightScale = maxHeight === undefined ? widthScale : (maxHeight * cell.heightPx) / imageHeight;
  const scale = Math.min(widthScale, heightScale);
  const columns = Math.max(1, Math.min(maxWidth, Math.ceil((imageWidth * scale) / cell.widthPx)));
  let rows = Math.max(1, Math.ceil((imageHeight * scale) / cell.heightPx));
  if (maxHeight !== undefined) {
    rows = Math.min(maxHeight, rows);
  }
  return { columns, rows };
}

export interface RenderedImage {
  /** Lines to emit: the escape sequence plus reserved padding rows. */
  lines: string[];
  columns: number;
  rows: number;
  protocol: GraphicsProtocol;
}

export interface RenderImageOptions {
  protocol: GraphicsProtocol;
  maxWidthCells?: number;
  maxHeightCells?: number;
}

/**
 * Encode a PNG buffer for inline display. Returns undefined when the PNG is not
 * recognizable or sizing fails.
 */
export function renderTerminalImage(png: Buffer, options: RenderImageOptions): RenderedImage | undefined {
  const base64 = png.toString('base64');
  const dimensions = getPngDimensions(base64);
  if (!dimensions) {
    return undefined;
  }
  const maxWidth = Math.max(1, options.maxWidthCells ?? 80);
  const detected = getCapabilities().images;

  // When the requested protocol is the detected one, use pi-tui's `renderImage`:
  // it registers Kitty metadata that the TUI needs for row reservation and
  // scrolling. Forced mismatches fall back to the raw encoders.
  if (detected === options.protocol) {
    const result = renderImage(base64, dimensions, {
      maxWidthCells: maxWidth,
      maxHeightCells: options.maxHeightCells,
      imageId: options.protocol === 'kitty' ? allocateImageId() : undefined,
      moveCursor: false,
    });
    if (!result) {
      return undefined;
    }
    const rowOffset = result.rows - 1;
    const lines = options.protocol === 'kitty'
      ? [result.sequence, ...Array(rowOffset).fill('')]
      : [...Array(rowOffset).fill(''), `${rowOffset > 0 ? `\x1b[${rowOffset}A` : ''}${result.sequence}`];
    return { lines, columns: result.columns, rows: result.rows, protocol: options.protocol };
  }

  const size = fitToCells(dimensions, maxWidth, options.maxHeightCells);
  if (options.protocol === 'kitty') {
    const sequence = encodeKitty(base64, { columns: size.columns, rows: size.rows, imageId: allocateImageId(), moveCursor: false });
    const lines = [sequence];
    for (let i = 0; i < size.rows - 1; i++) {
      lines.push('');
    }
    return { lines, columns: size.columns, rows: size.rows, protocol: 'kitty' };
  }
  const sequence = encodeITerm2(base64, { width: size.columns, height: 'auto', preserveAspectRatio: true });
  const lines: string[] = [];
  for (let i = 0; i < size.rows - 1; i++) {
    lines.push('');
  }
  const rowOffset = size.rows - 1;
  lines.push(`${rowOffset > 0 ? `\x1b[${rowOffset}A` : ''}${sequence}`);
  return { lines, columns: size.columns, rows: size.rows, protocol: 'iterm2' };
}
