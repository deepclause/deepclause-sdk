/**
 * Component renderer for the fullscreen TUI.
 *
 * It implements the same surface as `ReplRenderer` so the CLI's dispatch code is
 * unchanged, but it emits *components* where a component earns its keep:
 *
 * - a tool call becomes a `DmlToolPane` (collapsible, with its own status),
 * - streamed assistant text becomes a live `Markdown` block, so prose renders as
 *   it arrives instead of one line at a time,
 * - the model-call indicator moves to the dock (`onWorking`), where a `Loader`
 *   cannot be flushed away by unrelated output the way a transcript line could.
 *
 * Everything else is delegated to a `ReplRenderer` whose output is funnelled into
 * the document, so the existing formatting — output, logs, errors, usage,
 * compaction, solutions, user/steer blocks — is reused rather than reinvented.
 */

import { Markdown, type Component } from '@earendil-works/pi-tui';

import type { DMLBinding, DMLEvent } from '../../types.js';
import { framePrefix, ReplRenderer, type ReplRendererOptions } from '../render.js';
import { TranscriptDocument } from './document.js';
import { markdownTheme } from './theme.js';
import { DmlToolPane } from './tool-pane.js';

/** Events that produce no transcript output of their own. */
const QUIET_EVENTS = new Set(['model_call', 'finished', 'meta_result', 'usage']);

/** A markdown block whose text grows as the model streams. */
class StreamingBlock implements Component {
  private buffer = '';
  private readonly markdown: Markdown;
  /** The frame's indentation, so nested answers stay inside the tree. */
  private prefix: string;

  constructor(prefix: string) {
    this.prefix = prefix;
    this.markdown = new Markdown('', 0, 0, markdownTheme());
  }

  setPrefix(prefix: string): void {
    this.prefix = prefix;
  }

  append(chunk: string): void {
    this.buffer += chunk;
    this.markdown.setText(this.buffer);
  }

  get isEmpty(): boolean {
    return this.buffer.trim() === '';
  }

  render(width: number): string[] {
    const lines = this.markdown.render(Math.max(1, width - this.prefix.length));
    if (!this.prefix) return lines;
    return lines.map((line) => (line === '' ? line : `${this.prefix}${line}`));
  }

  invalidate(): void {
    this.markdown.invalidate();
  }
}

export interface TuiRendererOptions extends ReplRendererOptions {
  document: TranscriptDocument;
  /**
   * Show or hide the working indicator. The dock owns the `Loader`; the renderer
   * only reports the state, so the indicator does not depend on which event
   * happened to arrive last.
   */
  onWorking?: (label: string | null) => void;
}

export class TuiRenderer {
  /** Kept for parity with `ReplRenderer`; the TUI is always rich. */
  readonly mode = 'rich' as const;
  private readonly document: TranscriptDocument;
  private readonly text: ReplRenderer;
  private readonly onWorking?: (label: string | null) => void;
  /** Whether activity events (and therefore frame labels) are shown. */
  private readonly showEvents: boolean;
  /** Tool panes in order, so the last one can be expanded on demand. */
  private readonly panes: DmlToolPane[] = [];
  private currentPane?: DmlToolPane;
  private stream?: StreamingBlock;
  /** The live (unterminated) line written by the delegated renderer. */
  private pending = '';
  /**
   * What was emitted last: a text block or a component.
   *
   * The line renderer already separates its own blocks, but it cannot see the
   * components (panes, streamed markdown) that never pass through it — so text
   * followed by a component, or the reverse, ran together. Any blank requested here
   * collapses with one the line renderer adds, so this cannot double them.
   */
  private lastBlock: 'none' | 'text' | 'component' = 'none';

  constructor(options: TuiRendererOptions) {
    this.document = options.document;
    this.onWorking = options.onWorking;
    this.showEvents = options.events !== false;
    this.text = new ReplRenderer({
      mode: 'rich',
      color: options.color,
      verbose: options.verbose,
      stream: options.stream,
      events: options.events,
      reasoning: options.reasoning,
      markdown: options.markdown,
    });
    this.text.setSink(
      (text) => this.writeDocumentText(text),
      (text) => this.writeDocumentText(text),
    );
  }

  /** Keep unlike output apart: one blank line between a text block and a component. */
  private separate(kind: 'text' | 'component'): void {
    if (this.lastBlock !== 'none' && this.lastBlock !== kind) {
      this.document.appendText('');
    }
    this.lastBlock = kind;
  }

  /**
   * Close any open streamed-text block.
   *
   * Called at a turn boundary by the host, which knows when a prompt was submitted
   * even if the run that was streaming never reported `finished` (an interrupted
   * turn, for instance).
   */
  endTextBlock(): void {
    this.endStream();
  }

  /** Expand or collapse the most recent tool pane (ctrl+o). */
  toggleLastPane(): boolean {
    const pane = this.panes[this.panes.length - 1];
    if (!pane) return false;
    pane.toggle();
    this.document.onChange?.();
    return true;
  }

  render(event: DMLEvent): void {
    switch (event.type) {
      case 'tool_call':
        this.renderToolCall(event);
        return;
      case 'model_call':
        this.onWorking?.(event.modelCallState === 'started' ? 'working…' : null);
        return;
      case 'stream':
        if (event.content) {
          if (!this.stream) this.separate('component');
          this.stream ??= this.newStreamBlock(framePrefix(event, this.showEvents));
          this.stream.append(event.content);
          this.document.onChange?.();
        }
        return;
      case 'answer':
      case 'error':
        this.endStream();
        this.separate('text');
        this.text.render(event);
        return;
      case 'finished':
        // A run ended. Without closing the block, the next turn's streamed text is
        // appended to this one and renders *above* the prompt the user just typed,
        // because the block sits earlier in the transcript.
        if ((event.depth ?? 0) === 0) this.endStream();
        return;
      default:
        // Events that render nothing must not add a separator, or switching models
        // or finishing a turn would open a gap.
        if (!QUIET_EVENTS.has(event.type)) this.separate('text');
        this.text.render(event);
    }
  }

  private renderToolCall(event: DMLEvent): void {
    const name = event.toolName ?? 'tool';
    const state = event.toolState ?? 'running';
    const prefix = framePrefix(event, this.showEvents);

    if (state === 'starting' || !this.currentPane) {
      this.endStream();
      this.separate('component');
      this.currentPane = new DmlToolPane({ toolName: name, args: event.toolArgs ?? {}, prefix });
      this.panes.push(this.currentPane);
      this.document.appendComponent(this.currentPane);
      this.onWorking?.(`${name}…`);
    } else {
      // A `completed` event can carry the frame metadata the start event lacked.
      this.currentPane.setPrefix(prefix);
      if (event.toolArgs) this.currentPane.setArgs(event.toolArgs);
    }

    if (state === 'completed' || state === 'failed') {
      this.currentPane.setResult(
        event.toolResult ?? event.toolSummary ?? '',
        state === 'completed',
        event.toolDurationMs,
      );
      this.onWorking?.(null);
      this.currentPane = undefined;
      this.document.onChange?.();
    }
  }

  private newStreamBlock(prefix: string): StreamingBlock {
    const block = new StreamingBlock(prefix);
    this.document.appendComponent(block);
    return block;
  }

  /** Finish the live block, so later text starts a new one. */
  private endStream(): void {
    this.stream = undefined;
  }

  /**
   * Feed the delegated renderer's output into the document, mirroring the line
   * sink the readline path uses: `\r` replaces the live line, `\n` commits it, and
   * `ESC[K`/`ESC[J` are dropped because the pending line already replaces it.
   */
  private writeDocumentText(chunk: string): void {
    let index = 0;
    while (index < chunk.length) {
      const char = chunk[index];
      if (char === '\r') {
        this.pending = '';
        this.document.setPending('');
        index += 1;
        continue;
      }
      if (char === '\n') {
        this.document.appendText(this.pending);
        this.pending = '';
        this.document.setPending('');
        index += 1;
        continue;
      }
      if (char === '\u001b' && chunk[index + 1] === '[') {
        const match = /^\u001b\[[0-9;]*[JK]/.exec(chunk.slice(index));
        if (match) {
          index += match[0].length;
          continue;
        }
      }
      this.pending += char;
      this.document.setPending(this.pending);
      index += 1;
    }
  }

  // ---- ReplRenderer-compatible surface ----

  /**
   * While the TUI is live everything goes through the document. The CLI calls
   * this again on the way out (to "restore stdout" for the exit footer), and that
   * is exactly when output should stop being a component and go to the terminal
   * again — otherwise the footer is written into a viewport that has been torn
   * down and the user never sees it.
   */
  setSink(sink?: (text: string) => void, errorSink?: (text: string) => void): void {
    if (!sink) return;
    this.text.setSink(sink, errorSink);
  }

  setImageSink(sink?: (lines: string[]) => void): void {
    this.text.setImageSink(sink
      ? (lines) => {
        this.endStream();
        sink(lines);
      }
      : undefined);
  }

  setSuppressInputPrompt(suppress: boolean): void {
    this.text.setSuppressInputPrompt(suppress);
  }

  setReasoningMode(mode: 'off' | 'summary' | 'full'): void {
    this.text.setReasoningMode(mode);
  }

  getReasoningMode(): 'off' | 'summary' | 'full' {
    return this.text.getReasoningMode();
  }

  getWidth(): number {
    return this.text.getWidth();
  }

  setWidth(width: number): void {
    this.text.setWidth(width);
  }

  writeLine(text = ''): void {
    this.text.writeLine(text);
  }

  writeErrorLine(text: string): void {
    this.text.writeErrorLine(text);
  }

  listing(text: string): string {
    this.endStream();
    return this.text.listing(text);
  }

  image(lines: string[]): void {
    this.endStream();
    this.text.image(lines);
  }

  solution(bindings: DMLBinding[], terminator?: string, prefix = '', maxValue?: number): void {
    this.endStream();
    this.text.solution(bindings, terminator, prefix, maxValue);
  }

  flush(): void {
    this.text.flush();
  }

  dispose(): void {
    this.text.dispose();
  }
}
