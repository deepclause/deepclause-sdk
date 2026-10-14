/**
 * Minimal ANSI theme for the REPL TUI.
 *
 * The transcript is fed by `ReplRenderer` (rich mode) and the status bar is
 * rendered as plain text; these helpers add the few colors the TUI itself owns
 * (user-message blocks, the editor border, and the dim footer).
 */

import { visibleWidth, type EditorTheme } from '@earendil-works/pi-tui';

const ANSI = {
  reset: '\u001b[0m',
  bold: '\u001b[1m',
  dim: '\u001b[2m',
  cyan: '\u001b[36m',
  yellow: '\u001b[33m',
  /** Blueish chrome for the input/status area border. */
  border: '\u001b[38;5;75m',
  /** Orange "input needed" border/label for ask_user / input/2. */
  prompt: '\u001b[38;5;214m',
  /** pi-style user message: blue background, bright foreground. */
  userBg: '\u001b[48;5;24m',
  userFg: '\u001b[38;5;255m',
  /** Steering message: distinct amber background so it reads as an interjection. */
  steerBg: '\u001b[48;5;94m',
  steerFg: '\u001b[38;5;230m',
  boldOff: '\u001b[22m',
} as const;

function paint(text: string, code: string): string {
  return `${code}${text}${ANSI.reset}`;
}

/**
 * Render one line of a user message as a full-width blue block (pi-style). The
 * text is padded to the terminal width so the background spans the row.
 */
export function renderUserLine(text: string, width: number): string {
  const body = ` ${text} `;
  const padding = Math.max(0, width - visibleWidth(body));
  return `${ANSI.userBg}${ANSI.userFg}${body}${' '.repeat(padding)}${ANSI.reset}`;
}

/** An `ask_user` / `input/2` answer echo (distinct from a normal prompt). */
export function formatInputAnswer(text: string): string {
  return paint(`> ${text}`, ANSI.yellow);
}

/**
 * Render one line of a steering message as a full-width amber block, prefixed
 * with `Steering: ` on the first line. Padded to the terminal width.
 */
export const STEER_LABEL = ' Steering: ';
const STEER_INDENT = ' '.repeat(STEER_LABEL.length);

export function renderSteerLine(text: string, width: number, first = true): string {
  const label = first ? STEER_LABEL : STEER_INDENT;
  const visible = label.length + visibleWidth(text);
  const padding = Math.max(0, width - visible);
  const labelStyled = first ? `${ANSI.bold}${STEER_LABEL}${ANSI.boldOff}` : label;
  return `${ANSI.steerBg}${ANSI.steerFg}${labelStyled}${text}${' '.repeat(padding)}${ANSI.reset}`;
}

/** The border of the editing box while the runtime needs human input. */
export function promptBorder(text: string): string {
  return paint(text, ANSI.prompt);
}

/** The question shown inside the editing box while input is needed. */
export function promptLabel(text: string): string {
  return paint(text, `${ANSI.bold}${ANSI.prompt}`);
}

/** Dim the status bar so it recedes behind the transcript. */
export function dim(text: string): string {
  return paint(text, ANSI.dim);
}

/** Editor theme: a blue border, cyan selection. */
export function editorTheme(): EditorTheme {
  return {
    borderColor: (text: string) => paint(text, ANSI.border),
    selectList: {
      selectedPrefix: (text: string) => paint(text, ANSI.cyan),
      selectedText: (text: string) => paint(text, `${ANSI.bold}${ANSI.cyan}`),
      description: (text: string) => paint(text, ANSI.dim),
      scrollInfo: (text: string) => paint(text, ANSI.dim),
      noMatch: (text: string) => paint(text, ANSI.dim),
    },
  };
}

/**
 * Pi's theme system, adopted for the component layer.
 *
 * Markdown and editor themes come from pi so prose and the input box match the
 * application the user already runs (`getMarkdownTheme`/`getEditorTheme` are
 * resolved against pi's active theme). Our own chrome is styled through pi-tui's
 * `styleText`, which converts to the terminal's colour mode for us, so the
 * palette below is *colours* rather than pre-baked escape sequences.
 *
 * `initTheme()` is best-effort: if pi's theme files are unreadable the SDK still
 * renders, with the fallbacks below.
 */

import {
  getTerminalColorMode,
  indexedColor,
  styleText,
  type Color,
  type MarkdownTheme,
  type SelectListTheme,
  type TextStyle,
} from '@earendil-works/pi-tui';
import { getMarkdownTheme, getSelectListTheme, initTheme } from '@earendil-works/pi-coding-agent';

let themeInitialized = false;

/** Load pi's active theme once. Never throws: a missing theme is not fatal. */
export function initTuiTheme(): void {
  if (themeInitialized) return;
  themeInitialized = true;
  try {
    initTheme();
  } catch {
    // pi's theme is a nicety; the fallbacks below cover a missing theme.
  }
}

/** Palette entries, as colours so pi-tui owns the escape-sequence conversion. */
const C = {
  accent: indexedColor(75),
  border: indexedColor(75),
  borderMuted: indexedColor(240),
  title: indexedColor(180),
  muted: indexedColor(245),
  dim: indexedColor(240),
  success: indexedColor(114),
  error: indexedColor(203),
  warning: indexedColor(214),
  output: indexedColor(150),
  thinking: indexedColor(245),
} as const;

export type TuiToken = keyof typeof C;

/** Style text with a semantic colour, honouring the terminal's colour mode. */
export function paintToken(token: TuiToken, text: string, extra?: TextStyle): string {
  return styleText(text, { fg: C[token] as Color, ...extra }, getTerminalColorMode());
}

/** Convenience wrappers for the tokens the components use most. */
export const tuiStyle = {
  accent: (text: string) => paintToken('accent', text),
  title: (text: string) => paintToken('title', text, { bold: true }),
  muted: (text: string) => paintToken('muted', text),
  dim: (text: string) => paintToken('dim', text),
  success: (text: string) => paintToken('success', text),
  error: (text: string) => paintToken('error', text),
  warning: (text: string) => paintToken('warning', text),
  output: (text: string) => paintToken('output', text),
  thinking: (text: string) => paintToken('thinking', text, { italic: true }),
  borderMuted: (text: string) => paintToken('borderMuted', text),
} as const;

/** Scrollbar colours for the transcript's `ScrollView`. */
export const scrollbarStyles = {
  track: (text: string) => paintToken('borderMuted', text),
  thumb: (text: string) => paintToken('accent', text),
} as const;

/** Fallback markdown theme, used only if pi's cannot be resolved. */
function fallbackMarkdownTheme(): MarkdownTheme {
  const id = (text: string) => text;
  return {
    heading: tuiStyle.title,
    link: tuiStyle.accent,
    linkUrl: tuiStyle.dim,
    code: tuiStyle.output,
    codeBlock: tuiStyle.output,
    codeBlockBorder: tuiStyle.dim,
    quote: tuiStyle.muted,
    quoteBorder: tuiStyle.dim,
    hr: tuiStyle.dim,
    listBullet: tuiStyle.accent,
    bold: (text: string) => paintToken('title', text, { bold: true }),
    italic: (text: string) => paintToken('thinking', text, { italic: true }),
    strikethrough: id,
    underline: id,
  };
}

let cachedMarkdownTheme: MarkdownTheme | undefined;

/**
 * Markdown theme: pi's, so prose matches the rest of the user's terminal. The
 * import is synchronous because this is called from render paths, and pi is
 * already a hard dependency (compaction uses it).
 */
export function markdownTheme(): MarkdownTheme {
  if (cachedMarkdownTheme) return cachedMarkdownTheme;
  try {
    // pi's theme functions are lazy: they throw on *use* when the theme was
    // never initialized, so probe one rather than trusting the getter.
    initTuiTheme();
    const theme = getMarkdownTheme();
    theme.heading('probe');
    cachedMarkdownTheme = theme;
    return theme;
  } catch {
    cachedMarkdownTheme = fallbackMarkdownTheme();
    return cachedMarkdownTheme;
  }
}

/** Editor theme: pi's if available, else the SDK's existing one. */
let cachedEditorTheme: EditorTheme | undefined;

export function tuiEditorTheme(): EditorTheme {
  if (cachedEditorTheme) return cachedEditorTheme;
  cachedEditorTheme = editorTheme();
  return cachedEditorTheme;
}

/** Fallback list theme, used only if pi's cannot be resolved. */
function fallbackSelectListTheme(): SelectListTheme {
  return {
    selectedPrefix: (text: string) => tuiStyle.accent(text),
    selectedText: (text: string) => paintToken('title', text, { bold: true }),
    description: (text: string) => tuiStyle.dim(text),
    scrollInfo: (text: string) => tuiStyle.dim(text),
    noMatch: (text: string) => tuiStyle.dim(text),
  };
}

let cachedSelectListTheme: SelectListTheme | undefined;

/** Selection-list theme: pi's, so pickers look like the rest of pi. */
export function selectListTheme(): SelectListTheme {
  if (cachedSelectListTheme) return cachedSelectListTheme;
  try {
    initTuiTheme();
    const theme = getSelectListTheme();
    // pi's theme functions are lazy and throw when the theme was never initialized.
    theme.selectedPrefix('probe');
    cachedSelectListTheme = theme;
    return theme;
  } catch {
    cachedSelectListTheme = fallbackSelectListTheme();
    return cachedSelectListTheme;
  }
}
