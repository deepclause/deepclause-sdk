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
import { ReplRenderer, type ReplRendererOptions } from '../render.js';
import { TranscriptDocument } from './document.js';
import { markdownTheme } from './theme.js';
import { DmlToolPane } from './tool-pane.js';

/** A markdown block whose text grows as the model streams. */
class StreamingBlock implements Component {
  private buffer = '';
  private readonly markdown: Markdown;

  constructor() {
    this.markdown = new Markdown('', 0, 0, markdownTheme());
  }

  append(chunk: string): void {
    this.buffer += chunk;
    this.markdown.setText(this.buffer);
  }

  get isEmpty(): boolean {
    return this.buffer.trim() === '';
  }

  render(width: number): string[] {
    return this.markdown.render(width);
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
  /** Tool panes in order, so the last one can be expanded on demand. */
  private readonly panes: DmlToolPane[] = [];
  private currentPane?: DmlToolPane;
  private stream?: StreamingBlock;
  /** The live (unterminated) line written by the delegated renderer. */
  private pending = '';

  constructor(options: TuiRendererOptions) {
    this.document = options.document;
    this.onWorking = options.onWorking;
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
          this.stream ??= this.newStreamBlock();
          this.stream.append(event.content);
          this.document.onChange?.();
        }
        return;
      case 'answer':
      case 'error':
        this.endStream();
        this.text.render(event);
        return;
      default:
        this.text.render(event);
    }
  }

  private renderToolCall(event: DMLEvent): void {
    const name = event.toolName ?? 'tool';
    const state = event.toolState ?? 'running';

    if (state === 'starting' || !this.currentPane) {
      this.endStream();
      this.currentPane = new DmlToolPane({ toolName: name, args: event.toolArgs ?? {} });
      this.panes.push(this.currentPane);
      this.document.appendComponent(this.currentPane);
      this.onWorking?.(`${name}…`);
    } else if (event.toolArgs) {
      this.currentPane.setArgs(event.toolArgs);
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

  private newStreamBlock(): StreamingBlock {
    const block = new StreamingBlock();
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
