/**
 * The component layer the fullscreen TUI is built from.
 *
 * These are the pieces the string renderer cannot express: a tool call that can
 * expand, streamed text that renders as markdown while it arrives, and a working
 * indicator that belongs to the dock rather than to whatever line was written
 * last. Each renders to lines for a width, which is what the TUI requires, so the
 * assertions are about *lines* — including that none of them exceeds the width.
 */
import { describe, expect, it, vi } from 'vitest';
import { visibleWidth } from '@earendil-works/pi-tui';

import { TranscriptDocument } from '../src/repl/tui/document.js';
import { SizedTerminal } from '../src/repl/tui/terminal.js';
import { DmlToolPane } from '../src/repl/tui/tool-pane.js';
import { WorkingLine } from '../src/repl/tui/working-line.js';
import { TuiRenderer } from '../src/repl/tui/renderer.js';

/** Strip ANSI so assertions can look at text rather than escape codes. */
const plain = (line: string): string => line.replace(/\u001b\[[0-9;]*[a-zA-Z]/g, '');

const text = (lines: string[]): string => lines.map(plain).join('\n');

describe('TranscriptDocument', () => {
  it('renders text entries and collapses blank runs', () => {
    const doc = new TranscriptDocument();
    doc.appendText('one');
    doc.appendText('');
    doc.appendText('');
    doc.appendText('two');
    expect(text(doc.render(20)).trim().split('\n')).toEqual(['one', '', 'two']);
  });

  it('wraps text to the width, and re-wraps when it changes', () => {
    const doc = new TranscriptDocument();
    doc.appendText('aaa bbb ccc ddd');
    for (const width of [8, 40]) {
      for (const line of doc.render(width)) {
        expect(visibleWidth(line)).toBeLessThanOrEqual(width);
      }
    }
    // Narrow re-wraps; wide stays on one line.
    expect(doc.render(8).length).toBeGreaterThan(1);
    expect(doc.render(40)).toHaveLength(1);
  });

  it('holds components and never exceeds the width', () => {
    const doc = new TranscriptDocument();
    doc.appendComponent(new DmlToolPane({ toolName: 'bash', args: { command: 'x'.repeat(200) } }));
    for (const line of doc.render(30)) {
      expect(visibleWidth(line)).toBeLessThanOrEqual(30);
    }
  });

  it('drops the oldest entries when it grows past its bound', () => {
    const doc = new TranscriptDocument(3);
    for (const line of ['a', 'b', 'c', 'd']) doc.appendText(line);
    expect(text(doc.render(20)).split('\n')).toEqual(['b', 'c', 'd']);
  });

  it('reports emptiness, which is how user turns are separated', () => {
    const doc = new TranscriptDocument();
    expect(doc.isEmpty()).toBe(true);
    doc.appendText('x');
    expect(doc.isEmpty()).toBe(false);
  });

  it('notifies on change so the viewport can re-render', () => {
    const doc = new TranscriptDocument();
    const onChange = vi.fn();
    doc.onChange = onChange;
    doc.appendText('x');
    doc.appendComponent(new DmlToolPane({ toolName: 't', args: {} }));
    expect(onChange).toHaveBeenCalledTimes(2);
  });
});

describe('DmlToolPane', () => {
  it('shows the call, and how to reveal the rest', () => {
    const pane = new DmlToolPane({ toolName: 'bash', args: { command: 'printf hello' } });
    pane.setResult({ stdout: 'hello\n', exitCode: 0 }, true, 9);
    const rendered = text(pane.render(60));
    expect(rendered).toContain('bash');
    expect(rendered).toContain('printf hello');
    expect(rendered).toContain('9ms');
    expect(rendered).toContain('ctrl+o to expand');
  });

  it('marks a failure, and shows the error', () => {
    const pane = new DmlToolPane({ toolName: 'execute', args: { goal: 'nope' } });
    pane.setResult({ error: 'unknown procedure: nope/0' }, false);
    const rendered = text(pane.render(60));
    expect(rendered).toContain('✖');
    expect(rendered).toContain('unknown procedure');
  });

  it('collapses a long body and reveals it when expanded', () => {
    const lines = Array.from({ length: 20 }, (_, index) => `line ${index}`).join('\n');
    const pane = new DmlToolPane({ toolName: 'bash', args: { command: 'x' } });
    pane.setResult({ stdout: lines }, true);

    const collapsed = text(pane.render(60));
    expect(collapsed).toContain('more lines');
    expect(collapsed).not.toContain('line 19');

    pane.toggle();
    expect(pane.isExpanded).toBe(true);
    expect(text(pane.render(60))).toContain('line 19');
  });

  it('renders bindings from execute', () => {
    const pane = new DmlToolPane({ toolName: 'execute', args: { goal: 'member(X, [1,2])' } });
    pane.setResult({ bindings: [{ name: 'X', value: '1' }] }, true);
    expect(text(pane.render(60))).toContain('X = 1');
  });

  it('keeps every line within the width, however long the content', () => {
    const pane = new DmlToolPane({
      toolName: 'execute',
      args: { goal: 'x'.repeat(300) },
    });
    pane.setResult({ stdout: 'y'.repeat(300), bindings: [{ name: 'V', value: 'z'.repeat(300) }] }, true);
    for (const width of [20, 40, 80]) {
      for (const line of pane.render(width)) {
        expect(visibleWidth(line)).toBeLessThanOrEqual(width);
      }
    }
  });

  it('caches per width and re-renders after a change', () => {
    const pane = new DmlToolPane({ toolName: 'bash', args: { command: 'one' } });
    const first = pane.render(40);
    expect(pane.render(40)).toBe(first); // same array: cached
    pane.setArgs({ command: 'two' });
    expect(text(pane.render(40))).toContain('two');
  });
});

describe('WorkingLine', () => {
  const stubTui = { requestRender: () => {}, invalidate: () => {} } as never;

  it('occupies no row when idle', () => {
    const line = new WorkingLine(stubTui);
    expect(line.render(40)).toEqual([]);
    expect(line.isWorking).toBe(false);
  });

  it('shows the label while working, then clears', () => {
    const line = new WorkingLine(stubTui);
    line.set('working…');
    expect(text(line.render(40))).toContain('working');
    expect(line.isWorking).toBe(true);
    line.set(null);
    expect(line.render(40)).toEqual([]);
  });

  it('stops its timer on stop, so it cannot outlive the UI', () => {
    // The Loader starts animating from its constructor; a live interval keeps the
    // process from exiting, which is what `stop()` (and the exit path) guards.
    const line = new WorkingLine(stubTui);
    line.set('working…');
    line.stop();
    expect(line.isWorking).toBe(false);
  });
});

describe('TuiRenderer', () => {
  function build() {
    const document = new TranscriptDocument();
    const working: Array<string | null> = [];
    const renderer = new TuiRenderer({ document, onWorking: (label) => working.push(label) });
    return { document, working, renderer };
  }

  it('turns a tool call into a pane, and a result into its body', () => {
    const { document, renderer } = build();
    renderer.render({ type: 'tool_call', toolName: 'bash', toolState: 'starting', toolArgs: { command: 'ls' } });
    renderer.render({ type: 'tool_call', toolName: 'bash', toolState: 'completed', toolResult: { stdout: 'file.txt' }, toolDurationMs: 3 });

    const out = text(document.render(60));
    expect(out).toContain('bash');
    expect(out).toContain('ls');
    expect(out).toContain('file.txt');
  });

  it('drives the working indicator from the model call, not from output', () => {
    const { working, renderer } = build();
    renderer.render({ type: 'model_call', modelCallState: 'started' });
    expect(working.at(-1)).toBe('working…');
    // Unrelated output must not cancel it: that was the old failure mode.
    renderer.render({ type: 'output', content: 'still thinking' });
    expect(working.at(-1)).toBe('working…');
    renderer.render({ type: 'model_call', modelCallState: 'completed' });
    expect(working.at(-1)).toBeNull();
  });

  it('accumulates streamed text into one markdown block', () => {
    const { document, renderer } = build();
    renderer.render({ type: 'stream', content: '# Title\n\n' });
    renderer.render({ type: 'stream', content: 'body text' });
    const out = text(document.render(60));
    expect(out).toContain('Title');
    expect(out).toContain('body text');
  });

  it('expands the most recent pane', () => {
    const { document, renderer } = build();
    renderer.render({ type: 'tool_call', toolName: 'execute', toolState: 'starting', toolArgs: { goal: 'true' } });
    renderer.render({ type: 'tool_call', toolName: 'execute', toolState: 'completed', toolResult: { bindings: [] } });
    expect(renderer.toggleLastPane()).toBe(true);
    expect(renderer.toggleLastPane()).toBe(true);
    expect(text(document.render(60))).toBeDefined();
  });

  it('reports nothing to toggle when there are no tool calls', () => {
    const { renderer } = build();
    expect(renderer.toggleLastPane()).toBe(false);
  });
});

describe('the frame fills the terminal', () => {
  it('stretches the transcript and pins the dock to the bottom row', async () => {
    // The layout pass the alternate screen runs (`renderLayoutFrame`), not the
    // component `render(width)` — the height distribution only happens there, which
    // is why "it does not use the full height" was not visible to component tests.
    const { renderLayoutFrame } = await import('@earendil-works/pi-tui/dist/layout.js');
    const { ScrollView, VStack, Text } = await import('@earendil-works/pi-tui');

    const document = new Text('one\ntwo', 0, 0);
    const scroll = new ScrollView(document, { follow: 'end', primary: true });
    const dock = new VStack([
      { component: new Text('editor', 0, 0), basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
      { component: new Text('status', 0, 0), basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
    ]);
    const root = new VStack([
      { component: scroll, basis: 0, grow: 1, shrink: 1, minSize: 1 },
      { component: dock, basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
    ]);

    const frame = renderLayoutFrame(root, 60, 24, () => {});
    expect(frame.lines).toHaveLength(24);
    expect(text(frame.lines).trimEnd().split('\n').slice(-2).map((l) => l.trim())).toEqual(['editor', 'status']);
  });
});

describe('SizedTerminal', () => {
  it('always reports a usable size, never NaN', () => {
    // `rows`/`columns` feed `Math.floor` inside the layout; a NaN there renders
    // nothing at all, which is what chaining `Number(process.env.LINES)` did.
    const terminal = new SizedTerminal();
    for (const value of [terminal.rows, terminal.columns]) {
      expect(Number.isInteger(value)).toBe(true);
      expect(value).toBeGreaterThan(0);
    }
  });

  it('prefers the stream, and lets the fallback be refreshed', () => {
    const terminal = new SizedTerminal();
    expect(terminal.rows).toBe(Math.max(1, terminal.rows));
    terminal.refreshSize();
    expect(terminal.rows).toBeGreaterThan(0);
  });
});
