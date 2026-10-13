/**
 * Line-oriented REPL renderer.
 *
 * Three modes:
 *  - `rich` (default on a TTY): coloured bindings and labels, no live rewriting.
 *  - `plain` (default when stdout is not a TTY): one static line per event, no ANSI.
 *  - `json`: one JSON object per event for tooling.
 *
 * This is the M1 surface (solutions, output, answer, errors). Live streaming
 * of model text/tool calls is added in M3.
 */

import type { DMLBinding, DMLEvent } from '../types.js';
import { spawn } from 'child_process';
import * as path from 'path';
import { hyperlink } from '@earendil-works/pi-tui';
import { renderTerminalImage, resolveGraphicsProtocol } from '../visualize/index.js';
import { formatListing } from './highlight.js';
import { MarkdownStream } from './markdown.js';

export type RenderMode = 'rich' | 'plain' | 'json';

export interface ReplRendererOptions {
  mode?: RenderMode;
  /** Colour output (rich mode only). */
  color?: boolean;
  /** Show DML `log` events. */
  verbose?: boolean;
  /** Render streamed model text/reasoning (default true). `--no-stream` disables. */
  stream?: boolean;
  /** How much model reasoning to show: `off`, `summary` (bounded), or `full`. */
  reasoning?: 'off' | 'summary' | 'full';
  /** Render assistant text as markdown (default true). */
  markdown?: boolean;
  /** Render activity events (task activity, tool calls, usage, logs); default true. `--no-events` disables. */
  events?: boolean;
  /** Terminal width for the tool-call panel background (default: stdout columns, or 80). */
  width?: number;
  /** Sink for normal lines. */
  sink?: (text: string) => void;
  /** Sink for error lines. */
  errorSink?: (text: string) => void;
  /** Observe every event before filtering (used by the status tracker). */
  onEvent?: (event: DMLEvent) => void;
  /** Workspace root, used to resolve media paths for the non-image fallback. */
  workspace?: string;
  /** Hand images to the OS viewer (xdg-open/open) when the terminal cannot show them. */
  openImages?: boolean;
}

/** Activity events hidden by `--no-events` (output/answer/input/error stay). */
const ACTIVITY_EVENTS = new Set<DMLEvent['type']>([
  'task_activity',
  'tool_call',
  'model_call',
  'usage',
  'log',
  'memory_compaction',
  'meta_result',
]);

function formatArgs(args: Record<string, unknown> | undefined): string {
  if (!args || Object.keys(args).length === 0) {
    return '';
  }
  const parts = Object.entries(args).map(([key, value]) => {
    const text = (typeof value === 'string' ? value : JSON.stringify(value)).replace(/\s+/g, ' ').trim();
    const compact = text.length > 60 ? `${text.slice(0, 57)}…` : text;
    return `${key}=${compact}`;
  });
  return `(${parts.join(', ')})`;
}

/** Max captured output lines previewed under a completed tool call. */
const TOOL_OUTPUT_LINES = 3;

/** Max physical lines of a single `output` event before it is collapsed. */
const OUTPUT_LINES = 24;

/**
 * Bound a potentially huge `output` event (e.g. a `write/1` of a large term).
 * Keeps the head and tail and collapses the middle; `--verbose` shows it whole.
 */
function boundOutput(text: string, max = OUTPUT_LINES): string[] {
  const lines = text.replace(/\r\n?/g, '\n').split('\n');
  if (lines.length <= max) {
    return lines;
  }
  const head = Math.ceil((max - 1) / 2);
  const tail = max - 1 - head;
  const omitted = lines.length - head - tail;
  return [
    ...lines.slice(0, head),
    `… (+${omitted} more line${omitted === 1 ? '' : 's'})`,
    ...lines.slice(lines.length - tail),
  ];
}

/** Captured output of a tool result (bash stdout, else stderr). */
function toolResultText(result: unknown): string {
  if (!result || typeof result !== 'object') return '';
  const r = result as Record<string, unknown>;
  const stdout = typeof r.stdout === 'string' ? r.stdout : '';
  const stderr = typeof r.stderr === 'string' ? r.stderr : '';
  return stdout.trim() ? stdout : stderr;
}

/** True when the result has no usable stdout, so the text shown is stderr. */
function isStderrOnly(result: unknown): boolean {
  if (!result || typeof result !== 'object') return false;
  const r = result as Record<string, unknown>;
  const stdout = typeof r.stdout === 'string' ? r.stdout : '';
  const stderr = typeof r.stderr === 'string' ? r.stderr : '';
  return !stdout.trim() && Boolean(stderr.trim());
}

/** The lines of a tool result, normalised for display. */
function extractToolOutput(result: unknown): string[] {
  const text = toolResultText(result);
  if (!text.trim()) return [];
  return text
    .replace(/\r\n?/g, '\n')
    .split('\n')
    .map((line) => line.replace(/\t/g, '    ').trimEnd())
    .filter((line) => line.trim() !== '');
}

/**
 * A one-line description of captured output: how much there is, and whether it
 * looks like structured data. Prefer this over dumping raw lines, whose only
 * readable part would be an arbitrary tail of a JSON payload.
 */
function describeToolOutput(result: unknown): string | undefined {
  const text = toolResultText(result);
  if (!text.trim()) return undefined;
  const lines = extractToolOutput(result);
  const bytes = Buffer.byteLength(text, 'utf8');
  const head = text.trimStart();
  const shape = head.startsWith('{') || head.startsWith('[')
    ? 'JSON'
    : /^<[!a-zA-Z]/.test(head)
      ? 'HTML'
      : undefined;
  const parts = [
    `${lines.length} line${lines.length === 1 ? '' : 's'}`,
    bytes < 1024 ? `${bytes} B` : `${(bytes / 1024).toFixed(1)} kB`,
  ];
  if (shape) parts.push(shape);
  return parts.join(' · ');
}

/** Word-wrap a single line to `width`, hard-breaking tokens that are longer. */
function wrapLine(text: string, width: number): string[] {
  if (width <= 0 || text.length <= width) {
    return [text];
  }
  const out: string[] = [];
  let remaining = text;
  while (remaining.length > width) {
    let breakAt = remaining.lastIndexOf(' ', width);
    if (breakAt <= 0) {
      breakAt = width; // no space: hard-break a long token (URL, path, flag)
    }
    out.push(remaining.slice(0, breakAt));
    remaining = remaining.slice(breakAt).replace(/^ +/, '');
  }
  out.push(remaining);
  return out;
}

function truncate(text: string, max: number): string {
  const singleLine = text.replace(/\s+/g, ' ').trim();
  return singleLine.length > max ? `${singleLine.slice(0, max - 1)}…` : singleLine;
}

/** Human-readable wall-clock duration: `450ms`, `1.2s`, `2m05s`. */
export function formatDuration(ms: number): string {
  if (!Number.isFinite(ms) || ms < 0) {
    return '';
  }
  if (ms < 1000) {
    return `${Math.round(ms)}ms`;
  }
  const seconds = ms / 1000;
  if (seconds < 60) {
    return `${seconds.toFixed(seconds < 10 ? 1 : 0)}s`;
  }
  const minutes = Math.floor(seconds / 60);
  const remainder = Math.round(seconds % 60);
  return `${minutes}m${String(remainder).padStart(2, '0')}s`;
}

const ANSI = {
  reset: '\u001b[0m',
  bold: '\u001b[1m',
  dim: '\u001b[2m',
  cyan: '\u001b[36m',
  green: '\u001b[32m',
  red: '\u001b[31m',
  /** Model reasoning: italic grey, like pi's thinking text. */
  thinking: '\u001b[3;38;5;245m',
  /** DML `output/1` intermediate results. */
  output: '\u001b[38;5;150m',
  /** The final `answer/1`. */
  answer: '\u001b[1;38;5;118m',
} as const;

/** True when the terminal advertises 24-bit colour (COLORTERM). */
const TRUECOLOR = /^(truecolor|24bit)$/i.test(process.env.COLORTERM ?? '');

/**
 * Tool-call panel: a subtle, near-transparent green tint behind activity
 * (truecolor when available, else a 256-colour fallback). Kept light so it
 * reads as a faint wash over the terminal background rather than a solid fill.
 */
const TOOL_PANEL_GREEN_BG = TRUECOLOR ? '\u001b[48;2;40;54;44m' : '\u001b[48;5;22m';
/** Blue tint for `execute` (a goal, not a shell command). */
const TOOL_PANEL_BLUE_BG = TRUECOLOR ? '\u001b[48;2;34;44;62m' : '\u001b[48;5;24m';
const TOOL_PANEL_FG = '\u001b[38;5;252m';
const TOOL_PANEL_ERROR_BG = TRUECOLOR ? '\u001b[48;2;58;36;36m' : '\u001b[48;5;52m';
const TOOL_PANEL_ERROR_FG = '\u001b[38;5;224m';

type ToolTone = 'green' | 'blue' | 'error';

/**
 * Tools whose main argument is a long literal worth showing in full (wrapped
 * and indented under the header), with the panel colour for that tool.
 */
const TOOL_DETAIL: Record<string, { arg: string; tone: ToolTone }> = {
  bash: { arg: 'command', tone: 'green' },
  execute: { arg: 'goal', tone: 'blue' },
};

/** A copy of `args` without `key` (for the compact header of a detail tool). */
function omitKey(args: Record<string, unknown> | undefined, key: string): Record<string, unknown> | undefined {
  if (!args) return undefined;
  const rest: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(args)) {
    if (k !== key) rest[k] = v;
  }
  return rest;
}

/** Spinner frames for the live model-call indicator. */
const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'] as const;

/** Strip ANSI SGR sequences (for measuring visible width). */
function stripAnsi(text: string): string {
  return text.replace(/\u001b\[[0-9;]*m/g, '');
}

/** Format solution bindings as `Name = value` lines (or `true`). */
export function formatBindings(bindings: DMLBinding[]): string {
  if (bindings.length === 0) {
    return 'true';
  }
  return bindings.map((binding) => `${binding.name} = ${binding.value}`).join('\n');
}

/** Max characters of reasoning shown in `summary` mode. */
const REASONING_SUMMARY_CHARS = 600;

export class ReplRenderer {
  readonly mode: RenderMode;
  private readonly color: boolean;
  private readonly verbose: boolean;
  private readonly showStream: boolean;
  private readonly showEvents: boolean;
  private readonly reasoningMode: 'off' | 'summary' | 'full';
  private readonly markdownEnabled: boolean;
  private readonly markdown = new MarkdownStream();
  private pendingTextEvent?: DMLEvent;
  private width: number;
  private sink: (text: string) => void;
  private errorSink: (text: string) => void;
  private imageSink?: (lines: string[]) => void;
  private eventSink?: (event: DMLEvent) => void;
  private workspace?: string;
  private openImages: boolean;
  /** Frame key of the currently open streamed-text region, or null. */
  private lastStreamFrame: string | null = null;
  /** Reasoning region key and characters shown so far (for `summary` mode). */
  private reasoningRegion = '';
  private reasoningChars = 0;
  private reasoningTruncated = false;
  private atLineStart = true;
  /** Last detail body printed for a running call (dedupes start/running). */
  private lastToolDetail?: string;
  /** Live model-call indicator state. */
  private spinnerTimer?: ReturnType<typeof setInterval>;
  private spinnerFrame = 0;
  private spinnerPrefix = '';
  private spinnerLabel = '';
  private spinnerVisible = false;

  constructor(options: ReplRendererOptions = {}) {
    this.mode = options.mode ?? 'plain';
    this.color = (options.color ?? true) && this.mode === 'rich';
    this.verbose = options.verbose ?? false;
    this.showStream = options.stream ?? true;
    this.showEvents = options.events ?? true;
    this.reasoningMode = options.reasoning ?? 'full';
    this.markdownEnabled = options.markdown ?? true;
    this.width = options.width ?? (process.stdout.columns || 80);
    this.sink = options.sink ?? ((text) => process.stdout.write(text));
    this.errorSink = options.errorSink ?? ((text) => process.stderr.write(text));
    this.eventSink = options.onEvent;
    this.workspace = options.workspace;
    this.openImages = options.openImages ?? true;
  }

  /** Replace the output sinks (used when moving into a TUI transcript). */
  setSink(sink: (text: string) => void, errorSink?: (text: string) => void): void {
    this.sink = sink;
    if (errorSink) this.errorSink = errorSink;
  }

  /**
   * Route terminal-image blocks (from `image()`) into a TUI transcript. When no
   * sink is set, `image()` writes the escape sequence to stdout directly.
   */
  setImageSink(sink?: (lines: string[]) => void): void {
    this.imageSink = sink;
  }

  /** Current rendering width in cells. */
  getWidth(): number {
    return this.width;
  }

  /** Update the rendering width (TUI resize). */
  setWidth(width: number): void {
    if (width > 0) this.width = width;
  }

  /** Clean up and syntax-highlight a Prolog listing for display. */
  listing(text: string): string {
    return formatListing(text, { color: this.color && this.mode === 'rich' });
  }

  /**
   * Suppress `input_required` prompt lines. The TUI editor shows the question in
   * its border, so a transcript line would be a duplicate.
   */
  setSuppressInputPrompt(suppress: boolean): void {
    this.suppressInputPrompt = suppress;
  }

  /** Stop the live indicator (call before the renderer goes out of scope). */
  dispose(): void {
    this.stopSpinner();
  }

  /** Last rendered block group, used to separate tool/text/output blocks. */
  private currentBlock: string | null = null;
  private toolBlockCounter = 0;
  private currentToolBlock: string | null = null;
  private taskBlockCounter = 0;
  private currentTaskBlock: string | null = null;
  /** When true, `input_required` events are rendered by the input UI instead. */
  private suppressInputPrompt = false;

  /** Write a raw chunk to the normal sink. */
  write(text: string): void {
    this.stopSpinner();
    this.sink(text);
  }

  /** Write a line to the normal sink. */
  writeLine(text = ''): void {
    this.stopSpinner();
    this.flushStream();
    this.sink(`${text}\n`);
    this.lastStreamFrame = null;
    this.atLineStart = true;
  }

  /**
   * Write a terminal-image block (the escape sequence plus reserved blank rows).
   * In a TUI this goes to the image sink; otherwise it is written raw.
   */
  image(lines: string[]): void {
    if (lines.length === 0) {
      return;
    }
    this.stopSpinner();
    this.flushStream();
    if (this.imageSink) {
      this.imageSink(lines);
    } else {
      this.sink(`${lines.join('\n')}\n`);
    }
    this.lastStreamFrame = null;
    this.atLineStart = true;
  }

  /**
   * Render a `media` event (from `output(media(...))`): an inline terminal image
   * when supported, otherwise an OSC-8 hyperlinked path (optionally opened).
   */
  private renderMedia(event: DMLEvent): void {
    const media = event.media;
    if (!media) {
      return;
    }
    if (media.data) {
      const protocol = resolveGraphicsProtocol('auto');
      if (protocol) {
        const image = renderTerminalImage(Buffer.from(media.data, 'base64'), {
          protocol,
          maxWidthCells: Math.max(20, this.width - 2),
          maxHeightCells: Math.max(8, (process.stdout.rows ?? 40) - 8),
        });
        if (image) {
          this.image(image.lines);
          return;
        }
      }
    }
    const rel = media.path;
    const abs = rel ? (this.workspace ? path.resolve(this.workspace, rel) : rel) : undefined;
    const label = abs ? `${media.mimeType}  ${abs}` : `[image ${media.mimeType}]`;
    this.writeLine(abs ? hyperlink(label, `file://${abs}`) : label);
    if (abs && rel && this.openImages) {
      this.openExternal(abs);
    }
  }

  /** Hand a file to the OS viewer (best effort, never blocks). */
  private openExternal(abs: string): void {
    const command = process.platform === 'darwin' ? 'open' : process.platform === 'win32' ? 'start' : 'xdg-open';
    try {
      spawn(command, [abs], { detached: true, stdio: 'ignore' }).unref();
    } catch {
      // Ignore: the path line above is the guaranteed fallback.
    }
  }
  flush(): void {
    this.flushStream();
  }

  writeErrorLine(text: string): void {
    this.stopSpinner();
    this.errorSink(`${text}\n`);
  }

  /**
   * The visual block an event belongs to. Events in the same block are printed
   * contiguously; switching blocks inserts one blank line. Returns null for
   * events that do not participate (solutions, spinners, metadata).
   */
  private blockKeyFor(event: DMLEvent): string | null {
    switch (event.type) {
      case 'stream':
        return `text:${event.frameKind ?? 'run'}@${event.depth ?? 0}`;
      case 'reasoning':
        return event.content ? `reasoning:${event.frameKind ?? 'run'}@${event.depth ?? 0}` : null;
      case 'tool_call': {
        if (this.currentToolBlock === null || event.toolState === 'starting') {
          this.toolBlockCounter += 1;
          this.currentToolBlock = `tool:${this.toolBlockCounter}`;
        }
        return this.currentToolBlock;
      }
      case 'task_activity': {
        if (this.currentTaskBlock === null || event.taskState === 'started') {
          this.taskBlockCounter += 1;
          this.currentTaskBlock = `task:${this.taskBlockCounter}`;
        }
        return this.currentTaskBlock;
      }
      case 'output':
      case 'answer':
      case 'error':
        return 'output';
      case 'memory_compaction':
        return 'compaction';
      default:
        return null;
    }
  }

  /** Insert a separating blank line when the block changes. */
  private beginBlock(key: string | null): void {
    if (!key) return;
    if (this.currentBlock !== null && this.currentBlock !== key) {
      this.writeLine('');
    }
    this.currentBlock = key;
  }

  /** Render one runtime event. */
  render(event: DMLEvent): void {    this.eventSink?.(event);
    if (!this.showStream && (event.type === 'stream' || event.type === 'reasoning')) {
      return;
    }
    if (!this.showEvents && ACTIVITY_EVENTS.has(event.type)) {
      return;
    }
    if (this.mode === 'json') {
      this.sink(`${JSON.stringify(event)}\n`);
      return;
    }

    if (event.type === 'media') {
      this.renderMedia(event);
      return;
    }

    // The top-level prompt task and the meta frame simply echo work that is
    // already shown by the user message and the streamed authoring text, and
    // skipped compactions are noise. Hide them unless verbose; the status
    // tracker still observes every event via `eventSink`.
    if (
      event.type === 'task_activity'
      && !event.frameKind
    ) {
      return;
    }
    if (event.type === 'memory_compaction' && event.compactionAction === 'skipped' && !this.verbose) {
      return;
    }

    if (event.type !== 'stream' && event.type !== 'reasoning') {
      this.stopSpinner();
      this.flushStream();
    }
    this.beginBlock(this.blockKeyFor(event));
    const prefix = this.frameLabel(event);

    switch (event.type) {
      case 'solution':
        this.solution(
          event.solution?.bindings ?? [],
          undefined,
          prefix,
          // Nested (`execute` child) bindings often carry a whole result term;
          // keep the transcript readable by truncating them.
          event.frameKind === 'nested' ? 160 : undefined,
        );
        break;
      case 'output': {
        // A single event can carry a large multi-line term (`write/1` of an
        // `exec(...)` result). Bound the line count, then fit each line to the
        // terminal width so nothing wraps (which would desync the TUI). Nested
        // activity is truncated to one line each; top-level output wraps.
        const max = event.frameKind === 'nested' ? 6 : OUTPUT_LINES;
        const bounded = this.verbose
          ? (event.content ?? '')
          : boundOutput(event.content ?? '', max).join('\n');
        this.writeFrameText(prefix, bounded, ANSI.output, event.frameKind === 'nested');
        break;
      }
      case 'answer':
        this.writeFrameText(prefix, event.content ?? '', ANSI.answer, false);
        break;
      case 'model_call':
        this.renderModelCall(event, prefix);
        break;
      case 'stream':
        // Visible model text supersedes the working indicator.
        this.stopSpinner();
        this.writeStream(event, event.content ?? '', 'text');
        break;
      case 'reasoning':
        if (event.content) {
          if (this.writeReasoning(event, event.content)) {
            this.stopSpinner();
          } else {
            // Reasoning is hidden (`off`) or truncated (`summary`): commit any
            // visible reasoning, then keep a spinner so the user still sees the
            // model working.
            this.flushStream();
            this.ensureThinkingSpinner(prefix);
          }
        }
        break;
      case 'log':
        if (this.verbose) {
          this.writeLine(this.paint(`${prefix}[log] ${event.content ?? ''}`, ANSI.dim));
        }
        break;
      case 'task_activity':
        this.renderTaskActivity(event, prefix);
        break;
      case 'tool_call':
        this.renderToolCall(event, prefix);
        break;
      case 'input_required':
        if (!this.suppressInputPrompt) {
          this.writeLine(this.paint(`${prefix}? ${event.prompt ?? ''}`, ANSI.bold));
        }
        break;
      case 'error':
        this.writeErrorLine(this.paint(`${prefix}ERROR: ${event.content ?? ''}`, ANSI.red));
        break;
      case 'usage':
        if (this.verbose && event.usage) {
          this.writeLine(this.paint(
            `${prefix}[usage] in=${event.usage.inputTokens} out=${event.usage.outputTokens} total=${event.usage.totalTokens}`,
            ANSI.dim,
          ));
        }
        break;
      case 'memory_compaction':
        this.writeLine(this.paint(`${prefix}${event.content ?? 'memory compacted'}`, ANSI.dim));
        break;
      case 'finished':
      case 'meta_result':
      default:
        break;
    }
  }

  private renderTaskActivity(event: DMLEvent, prefix: string): void {
    const state = event.taskState ?? 'started';
    if (state === 'started') {
      this.writeLine(this.paint(`${prefix}▸ Task  ${this.describeTask(event)}`, ANSI.dim));
      return;
    }
    const ok = state === 'completed';
    this.writeLine(this.paint(`${prefix}${ok ? '✔' : '✖'} Task  ${ok ? 'done' : 'failed'}`, ok ? ANSI.dim : ANSI.red));
  }

  private describeTask(event: DMLEvent): string {
    const description = (event.taskDescription ?? '').replace(/\s+/g, ' ').trim();
    if (!description) return 'task';
    return description.length > 72 ? `${description.slice(0, 69)}…` : description;
  }

  private renderToolCall(event: DMLEvent, prefix: string): void {
    const name = event.toolName ?? 'tool';
    const state = event.toolState;
    const pidSuffix = typeof event.toolPid === 'number' ? ` [${event.toolPid}]` : '';
    // Some tools have a long literal argument (a shell script, a DML goal) that
    // is shown in full, wrapped and indented, rather than truncated inline.
    const spec = TOOL_DETAIL[name];
    const rawDetail = spec ? event.toolArgs?.[spec.arg] : undefined;
    const detail = typeof rawDetail === 'string' ? rawDetail : undefined;
    const tone: ToolTone = spec?.tone ?? 'green';
    const args = detail === undefined ? formatArgs(event.toolArgs) : '';

    if (state === 'completed') {
      const duration = typeof event.toolDurationMs === 'number' ? ` · ${formatDuration(event.toolDurationMs)}` : '';
      const exit = typeof event.toolExitCode === 'number' && event.toolExitCode !== 0 ? ` · exit ${event.toolExitCode}` : '';
      const summary = event.toolSummary ? ` — ${truncate(event.toolSummary, 80)}` : '';
      this.lastToolDetail = undefined;
      this.writeLine(this.toolPanel(`${prefix}  ✔ ${name}${args}${duration}${exit}${summary}`, tone));
      // The output size, not a raw tail: the last few lines of a JSON payload are
      // never the useful part. `--verbose` prints the full output, and a DML
      // `execute` result already shows what the goal bound.
      if (!this.verbose) {
        for (const line of this.toolOutputLines(prefix, tone, event.toolResult)) {
          this.writeLine(line);
        }
      }
      return;
    }
    if (state === 'failed') {
      const duration = typeof event.toolDurationMs === 'number' ? ` · ${formatDuration(event.toolDurationMs)}` : '';
      const reason = event.toolError ?? event.toolSummary ?? 'failed';
      this.lastToolDetail = undefined;
      this.writeLine(this.toolPanel(`${prefix}  ✖ ${name}${args}${duration} — ${truncate(reason, 120)}`, 'error'));
      return;
    }

    if (detail !== undefined) {
      // A call emits `starting` then `running`; print the body once.
      if (detail !== this.lastToolDetail) {
        this.lastToolDetail = detail;
        const others = formatArgs(omitKey(event.toolArgs, spec!.arg));
        this.writeLine(this.toolPanel(`${prefix}  ⚙ ${name}${others}`, tone));
        // Print the body on panel lines, word-wrapped to the available width
        // and indented under the header so wrapped bodies stay aligned.
        const indent = '    ';
        const avail = Math.max(20, this.width - stripAnsi(prefix).length - indent.length);
        for (const rawLine of detail.replace(/\r\n?/g, '\n').split('\n')) {
          for (const segment of wrapLine(rawLine, avail)) {
            this.writeLine(this.toolPanel(`${prefix}${indent}${segment}`, tone));
          }
        }
      }
      this.ensureToolSpinner(prefix, `${name}${pidSuffix} running…`);
      return;
    }

    this.writeLine(this.toolPanel(`${prefix}  ⚙ ${name}${args}`, tone));
    this.ensureToolSpinner(prefix, `${name}${pidSuffix} running…`);
  }

  /**
   * A summary line for the captured output, plus a few of its lines.
   *
   * Which lines depends on where the useful end is: stdout is shown from the
   * *head* (for structured output the head carries the shape — `{`, a first
   * record — while the tail is arbitrary fragments), and stderr from the *tail*,
   * where the diagnostic normally is. Full output is `--verbose`.
   */
  private toolOutputLines(prefix: string, tone: ToolTone, result: unknown): string[] {
    const lines = extractToolOutput(result);
    if (lines.length === 0) {
      return [];
    }
    const indent = '    ';
    const avail = Math.max(20, this.width - stripAnsi(prefix).length - indent.length);
    const out: string[] = [];
    const description = describeToolOutput(result);
    if (description && lines.length > TOOL_OUTPUT_LINES) {
      out.push(this.toolPanel(`${prefix}${indent}${description} — --verbose for all`, tone));
    }
    const fromTail = !toolResultText(result).trim().startsWith('{') && isStderrOnly(result);
    const shown = fromTail ? lines.slice(-TOOL_OUTPUT_LINES) : lines.slice(0, TOOL_OUTPUT_LINES);
    const omitted = lines.length - shown.length;
    if (fromTail && omitted > 0) {
      out.push(this.toolPanel(`${prefix}${indent}… (+${omitted} earlier line${omitted === 1 ? '' : 's'})`, tone));
    }
    for (const line of shown) {
      out.push(this.toolPanel(`${prefix}${indent}${line.length > avail ? `${line.slice(0, avail - 1)}…` : line}`, tone));
    }
    if (!fromTail && omitted > 0) {
      out.push(this.toolPanel(`${prefix}${indent}… (+${omitted} more line${omitted === 1 ? '' : 's'})`, tone));
    }
    return out;
  }

  /** Show/hide the live model-call indicator. */
  private renderModelCall(event: DMLEvent, prefix: string): void {
    if (event.modelCallState === 'started') {
      this.startSpinner(prefix, 'working…');
      return;
    }
    this.stopSpinner();
  }

  /** Start a `\r`-based spinner on the current line (rich TTY only). */
  private startSpinner(prefix: string, label: string): void {
    if (!this.color || this.mode !== 'rich') {
      return;
    }
    this.stopSpinner();
    this.spinnerPrefix = prefix;
    this.spinnerLabel = label;
    this.spinnerFrame = 0;
    this.paintSpinner();
    this.spinnerTimer = setInterval(() => {
      this.spinnerFrame += 1;
      this.paintSpinner();
    }, 90);
  }

  private paintSpinner(): void {
    const frame = SPINNER_FRAMES[this.spinnerFrame % SPINNER_FRAMES.length];
    this.sink(`\r\u001b[K${this.spinnerPrefix}${this.paint(`${frame} ${this.spinnerLabel}`, ANSI.dim)}`);
    this.spinnerVisible = true;
  }

  /** Clear the spinner line and stop its timer (no-op when inactive). */
  private stopSpinner(): void {
    if (this.spinnerTimer) {
      clearInterval(this.spinnerTimer);
      this.spinnerTimer = undefined;
    }
    if (this.spinnerVisible) {
      this.sink('\r\u001b[K');
      this.spinnerVisible = false;
    }
  }

  /**
   * A full-width, tinted line for tool activity (pi-style panel). Falls back to
   * the bare text when colour is disabled. Existing ANSI in `text` is preserved
   * and the visible width (ignoring SGR) is padded out to the terminal width.
   */
  private toolPanel(text: string, tone: ToolTone): string {
    if (!this.color) {
      return text;
    }
    const bg = tone === 'error'
      ? TOOL_PANEL_ERROR_BG
      : tone === 'blue'
        ? TOOL_PANEL_BLUE_BG
        : TOOL_PANEL_GREEN_BG;
    const fg = tone === 'error' ? TOOL_PANEL_ERROR_FG : TOOL_PANEL_FG;
    const width = this.width - stripAnsi(text).length;
    const pad = width > 0 ? ' '.repeat(width) : '';
    return `${bg}${fg}${text}${pad}${ANSI.reset}`;
  }

  /**
   * Write `text` as frame-prefixed lines that each fit the terminal width:
   * wrap by default, or truncate each physical line (nested activity).
   */
  private writeFrameText(prefix: string, text: string, code: string, truncate: boolean): void {
    const avail = Math.max(20, this.width - stripAnsi(prefix).length);
    const normalized = text.replace(/\r\n?/g, '\n').replace(/\t/g, '    ');
    for (const rawLine of normalized.split('\n')) {
      const segments = truncate
        ? [rawLine.length > avail ? `${rawLine.slice(0, avail - 1)}…` : rawLine]
        : wrapLine(rawLine, avail);
      for (const segment of segments) {
        this.writeLine(`${prefix}${this.paint(segment, code)}`);
      }
    }
  }

  /**
   * Stream reasoning according to `showReasoning`: `off` hides it, `summary`
   * shows a bounded preview, `full` shows everything (all dimmed).
   */
  /**
   * Stream reasoning according to `showReasoning`: `off` hides it, `summary`
   * shows a bounded preview, `full` shows everything (all dimmed). Returns
   * whether anything was made visible (so the caller can keep a spinner when it
   * is hidden).
   */
  private writeReasoning(event: DMLEvent, chunk: string): boolean {
    if (this.reasoningMode === 'off') {
      return false;
    }
    const region = `${event.frameKind ?? 'plain'}@${event.depth ?? 0}`;
    if (region !== this.reasoningRegion) {
      this.reasoningRegion = region;
      this.reasoningChars = 0;
      this.reasoningTruncated = false;
    }
    const limit = this.reasoningMode === 'summary' ? REASONING_SUMMARY_CHARS : Infinity;
    if (this.reasoningChars >= limit) {
      this.announceReasoningTruncated(event);
      return false;
    }
    const remaining = limit - this.reasoningChars;
    const text = chunk.length > remaining ? chunk.slice(0, remaining) : chunk;
    this.reasoningChars += text.length;
    this.writeStream(event, this.paint(text, ANSI.thinking), 'reasoning');
    if (this.reasoningChars >= limit) {
      this.announceReasoningTruncated(event);
    }
    return text.length > 0;
  }

  /** Keep a `thinking…` spinner running (restart once if the label differs). */
  private ensureThinkingSpinner(prefix: string): void {
    if (this.spinnerTimer && this.spinnerLabel === 'thinking…') {
      return;
    }
    this.startSpinner(prefix, 'thinking…');
  }

  /** Keep a live indicator on screen while a tool (e.g. bash) is running. */
  private ensureToolSpinner(prefix: string, label: string): void {
    if (this.spinnerTimer && this.spinnerLabel === label) {
      return;
    }
    this.startSpinner(prefix, label);
  }

  private announceReasoningTruncated(event: DMLEvent): void {
    if (this.reasoningTruncated) {
      return;
    }
    this.reasoningTruncated = true;
    this.writeStream(
      event,
      this.paint(' … (reasoning truncated; set showReasoning=full to see it all)', ANSI.dim),
      'reasoning',
    );
  }

  /** Write a streamed chunk, prefixing it with its frame label on first use. */
  private writeStream(event: DMLEvent, chunk: string, kind: 'text' | 'reasoning'): void {
    if (kind === 'text' && this.markdownEnabled) {
      this.pendingTextEvent = event;
      for (const line of this.markdown.push(chunk)) {
        this.writeStreamChunk(event, `${line}\n`, kind);
      }
      return;
    }
    this.writeStreamChunk(event, chunk, kind);
  }

  private writeStreamChunk(event: DMLEvent, chunk: string, kind: 'text' | 'reasoning'): void {
    // Include the kind so switching reasoning <-> text starts a fresh region.
    const key = `${event.frameKind ?? 'plain'}@${event.depth ?? 0}:${kind}`;
    if (key !== this.lastStreamFrame) {
      if (!this.atLineStart) {
        this.sink('\n');
      }
      const prefix = this.frameLabel(event);
      if (prefix) this.sink(prefix);
      this.lastStreamFrame = key;
      this.atLineStart = false;
    }
    this.sink(chunk);
    this.atLineStart = chunk.endsWith('\n');
  }

  /** Close an open streamed-text region before writing a line. */
  private flushStream(): void {
    if (this.markdownEnabled && this.pendingTextEvent) {
      const event = this.pendingTextEvent;
      this.pendingTextEvent = undefined;
      for (const line of this.markdown.flush()) {
        this.writeStreamChunk(event, `${line}\n`, 'text');
      }
    }
    if (this.lastStreamFrame !== null) {
      if (!this.atLineStart) this.sink('\n');
      this.lastStreamFrame = null;
      this.atLineStart = true;
    }
  }

  /** `run │ ` for an object frame; nested (`execute` child) events use indentation + `│`. */
  private frameLabel(event: DMLEvent): string {
    // `--no-events` also drops the frame label, so output/answer lines are plain.
    if (!this.showEvents || !event.frameKind) return '';
    const depth = event.depth ?? 0;
    // Nested (`execute` child) activity reads as belonging to the parent tool
    // call through indentation plus a hanging `│`, with no `nested` label.
    if (event.frameKind === 'nested') {
      return `${'  '.repeat(depth + 1)}│ `;
    }
    const label = event.frameKind === 'object' ? 'run' : event.frameKind;
    const indent = '  '.repeat(depth);
    return `${indent}${label} │ `;
  }

  /**
   * Render one solution's bindings. `terminator` is appended after the last
   * binding line ('.' for the final answer, ' ' while more may follow).
   */
  solution(bindings: DMLBinding[], terminator?: string, prefix = '', maxValue?: number): void {
    if (this.mode === 'json') {
      this.sink(`${JSON.stringify({ type: 'solution', solution: { bindings, deterministic: false } })}\n`);
      return;
    }
    this.flushStream();
    if (bindings.length === 0) {
      this.writeLine(`${prefix}true${terminator ?? '.'}`);
      return;
    }
    const lines = bindings.map((binding) => {
      const value = maxValue !== undefined ? truncate(binding.value, maxValue) : binding.value;
      const name = this.paint(binding.name, ANSI.cyan);
      return `${prefix}${name} = ${value}`;
    });
    const last = lines.length - 1;
    lines.forEach((line, index) => {
      this.writeLine(index === last ? `${line}${terminator ?? '.'}` : line);
    });
  }

  private paint(text: string, code: string): string {
    if (!this.color) {
      return text;
    }
    return `${code}${text}${ANSI.reset}`;
  }
}
