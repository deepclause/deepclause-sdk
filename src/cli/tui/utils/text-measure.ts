/**
 * Grapheme-aware string width measurement utilities.
 */

import { stripAnsi } from './ansi.js';

interface GraphemeSegment {
  segment: string;
}

interface GraphemeSegmenter {
  segment(text: string): Iterable<GraphemeSegment>;
}

type IntlWithSegmenter = typeof Intl & {
  Segmenter?: new (
    locales?: string | string[],
    options?: { granularity: 'grapheme' },
  ) => GraphemeSegmenter;
};

const COMBINING_MARK_RE = /\p{Mark}/u;
const EXTENDED_PICTOGRAPHIC_RE = /\p{Extended_Pictographic}/u;

const graphemeSegmenter = (() => {
  const Segmenter = (Intl as IntlWithSegmenter).Segmenter;
  return Segmenter ? new Segmenter(undefined, { granularity: 'grapheme' }) : null;
})();

/**
 * Measure the display width of a string, accounting for wide characters
 * (CJK, emoji) and ignoring ANSI escape codes.
 */
export function measureDisplayWidth(text: string): number {
  const stripped = stripAnsi(text);
  if (!graphemeSegmenter) {
    return stripped.length;
  }

  let width = 0;
  for (const { segment } of graphemeSegmenter.segment(stripped)) {
    if (COMBINING_MARK_RE.test(segment) && segment.length === 1) {
      continue;
    }
    const codePoint = segment.codePointAt(0) ?? 0;
    if (EXTENDED_PICTOGRAPHIC_RE.test(segment)) {
      width += 2;
    } else if (isWideCodePoint(codePoint)) {
      width += 2;
    } else {
      width += 1;
    }
  }
  return width;
}

function isWideCodePoint(cp: number): boolean {
  return (
    (cp >= 0x1100 && cp <= 0x115f) ||
    (cp >= 0x2e80 && cp <= 0x303e) ||
    (cp >= 0x3041 && cp <= 0x33bf) ||
    (cp >= 0x3400 && cp <= 0x4dbf) ||
    (cp >= 0x4e00 && cp <= 0x9fff) ||
    (cp >= 0xa000 && cp <= 0xa4cf) ||
    (cp >= 0xac00 && cp <= 0xd7af) ||
    (cp >= 0xf900 && cp <= 0xfaff) ||
    (cp >= 0xfe10 && cp <= 0xfe6f) ||
    (cp >= 0xff01 && cp <= 0xff60) ||
    (cp >= 0xffe0 && cp <= 0xffe6) ||
    (cp >= 0x1f300 && cp <= 0x1f9ff) ||
    (cp >= 0x20000 && cp <= 0x2fffd) ||
    (cp >= 0x30000 && cp <= 0x3fffd)
  );
}

/** Truncate text to fit within a given display width, appending '…' if needed. */
export function ellipsize(text: string, maxWidth: number): string {
  if (maxWidth <= 0) return '';
  if (measureDisplayWidth(text) <= maxWidth) return text;
  
  const stripped = stripAnsi(text);
  let result = '';
  let width = 0;
  
  if (!graphemeSegmenter) {
    for (const ch of stripped) {
      if (width + 1 >= maxWidth) break;
      result += ch;
      width += 1;
    }
    return result + '…';
  }

  for (const { segment } of graphemeSegmenter.segment(stripped)) {
    const segWidth = EXTENDED_PICTOGRAPHIC_RE.test(segment) ? 2 : 1;
    if (width + segWidth >= maxWidth) break;
    result += segment;
    width += segWidth;
  }
  return result + '…';
}
