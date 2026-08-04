/**
 * Input parser — converts raw terminal keypress data into structured KeyEvent objects.
 * Also handles mouse event parsing for SGR (1006) mouse mode.
 */

import type { KeyEvent, MouseEvent } from '../types.js';

/**
 * Parse a raw keypress into a KeyEvent.
 * Node's readline module already does most of the parsing; this normalizes the result.
 */
export function parseKeypress(
  ch: string | undefined,
  key: { name?: string; sequence?: string; ctrl?: boolean; meta?: boolean; shift?: boolean } | undefined,
): KeyEvent | null {
  if (!key && !ch) return null;

  return {
    name: key?.name ?? ch ?? '',
    sequence: key?.sequence ?? ch ?? '',
    ctrl: key?.ctrl ?? false,
    meta: key?.meta ?? false,
    shift: key?.shift ?? false,
  };
}

/**
 * Try to parse an SGR mouse event from a raw sequence.
 * SGR format: ESC [ < Cb ; Cx ; Cy M/m
 */
export function parseMouseEvent(sequence: string): MouseEvent | null {
  const match = sequence.match(/^\x1b\[<(\d+);(\d+);(\d+)([Mm])$/);
  if (!match) return null;

  const cb = parseInt(match[1], 10);
  const col = parseInt(match[2], 10) - 1; // 0-indexed
  const row = parseInt(match[3], 10) - 1; // 0-indexed
  const isRelease = match[4] === 'm';

  const shift = !!(cb & 4);
  const meta = !!(cb & 8);
  const ctrl = !!(cb & 16);
  const baseButton = cb & 3;
  const isMotion = !!(cb & 32);
  const isWheel = !!(cb & 64);

  let button: MouseEvent['button'];

  if (isRelease) {
    button = 'release';
  } else if (isMotion) {
    button = 'motion';
  } else if (isWheel) {
    button = baseButton === 0 ? 'wheel-up' : 'wheel-down';
  } else {
    switch (baseButton) {
      case 0: button = 'left'; break;
      case 1: button = 'middle'; break;
      case 2: button = 'right'; break;
      default: button = 'left';
    }
  }

  return { button, row, col, shift, ctrl, meta };
}
