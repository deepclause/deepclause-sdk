/**
 * ANSI string manipulation utilities.
 *
 * Provides functions for stripping, measuring, and slicing strings
 * that contain ANSI escape sequences.
 */

// eslint-disable-next-line no-control-regex
const ANSI_REGEX = /\u001b\[[0-9;]*[A-Za-z]/g;

/** Strip all ANSI escape sequences from a string. */
export function stripAnsi(text: string): string {
  return text.replace(ANSI_REGEX, '');
}

/** Measure the visible display width of a string (ignoring ANSI codes). */
export function visibleLength(text: string): number {
  return stripAnsi(text).length;
}

/**
 * Truncate a string to a maximum visible width, preserving ANSI codes.
 * Appends '…' if truncated.
 */
export function truncate(text: string, maxWidth: number): string {
  if (maxWidth <= 0) return '';
  if (visibleLength(text) <= maxWidth) return text;

  let visible = 0;
  let result = '';
  let i = 0;

  while (i < text.length && visible < maxWidth - 1) {
    if (text[i] === '\x1b') {
      // Consume ANSI escape sequence
      const match = text.slice(i).match(/^\u001b\[[0-9;]*[A-Za-z]/);
      if (match) {
        result += match[0];
        i += match[0].length;
        continue;
      }
    }
    result += text[i];
    visible++;
    i++;
  }

  return result + '…';
}

/**
 * Pad a string with spaces to reach the desired visible width.
 * Accounts for ANSI codes not counting as visible characters.
 */
export function padRight(text: string, width: number): string {
  const vis = visibleLength(text);
  if (vis >= width) return text;
  return text + ' '.repeat(width - vis);
}

/**
 * Center a string within a given width.
 */
export function center(text: string, width: number): string {
  const vis = visibleLength(text);
  if (vis >= width) return text;
  const leftPad = Math.floor((width - vis) / 2);
  const rightPad = width - vis - leftPad;
  return ' '.repeat(leftPad) + text + ' '.repeat(rightPad);
}

/** ANSI color/style codes. */
export const ANSI = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  italic: '\x1b[3m',
  underline: '\x1b[4m',
  inverse: '\x1b[7m',
  black: '\x1b[30m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
  brightBlack: '\x1b[90m',
  brightRed: '\x1b[91m',
  brightGreen: '\x1b[92m',
  brightYellow: '\x1b[93m',
  brightBlue: '\x1b[94m',
  brightMagenta: '\x1b[95m',
  brightCyan: '\x1b[96m',
  brightWhite: '\x1b[97m',
  bgBlue: '\x1b[44m',
  bgCyan: '\x1b[46m',
  bgWhite: '\x1b[47m',
  bgBrightBlue: '\x1b[104m',
} as const;

/** Apply a style to text and auto-reset at the end. */
export function style(text: string, ...styles: string[]): string {
  if (styles.length === 0) return text;
  return styles.join('') + text + ANSI.reset;
}
