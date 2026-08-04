/**
 * Overlay — ANSI-aware string composition for modals/dialogs.
 *
 * Renders an overlay component on top of a background screen buffer,
 * centering it (or placing at specified coordinates) without clearing
 * the entire screen.
 */

import type { ScreenBuffer } from '../types.js';
import { clipAnsi, padRight, stripAnsi } from '../util/ansi.js';

export interface OverlayPosition {
  /** Center the overlay horizontally */
  centerX?: boolean;
  /** Center the overlay vertically */
  centerY?: boolean;
  /** Fixed x position (overrides centerX) */
  x?: number;
  /** Fixed y position (overrides centerY) */
  y?: number;
}

/**
 * Compose an overlay on top of a background screen buffer.
 * The overlay replaces characters at the specified position.
 */
export function composeOverlay(
  background: ScreenBuffer,
  overlayRows: string[],
  overlayWidth: number,
  screenWidth: number,
  screenHeight: number,
  position: OverlayPosition = { centerX: true, centerY: true },
): ScreenBuffer {
  if (overlayRows.length === 0) return background;

  const overlayHeight = overlayRows.length;

  // Calculate position
  let startX: number;
  let startY: number;

  if (position.x !== undefined) {
    startX = position.x;
  } else if (position.centerX) {
    startX = Math.max(0, Math.floor((screenWidth - overlayWidth) / 2));
  } else {
    startX = 0;
  }

  if (position.y !== undefined) {
    startY = position.y;
  } else if (position.centerY) {
    startY = Math.max(0, Math.floor((screenHeight - overlayHeight) / 2));
  } else {
    startY = 0;
  }

  // Compose
  const result: ScreenBuffer = [];
  for (let row = 0; row < screenHeight; row++) {
    const bgRow = background[row] ?? '';

    if (row >= startY && row < startY + overlayHeight) {
      const overlayLine = overlayRows[row - startY] ?? '';
      result.push(spliceRow(bgRow, overlayLine, startX, overlayWidth));
    } else {
      result.push(bgRow);
    }
  }

  return result;
}

/**
 * Replace a section of a background row with overlay content.
 * Simple approach: pads background to start position, inserts overlay.
 */
function spliceRow(bgRow: string, overlayLine: string, startX: number, overlayWidth: number): string {
  const bgPlain = stripAnsi(bgRow);
  const left = bgPlain.padEnd(startX).slice(0, startX);
  const rightStart = startX + overlayWidth;
  const right = bgPlain.length > rightStart ? bgPlain.slice(rightStart) : '';
  const paddedOverlay = padRight(clipAnsi(overlayLine, overlayWidth), overlayWidth);

  return left + paddedOverlay + right;
}
