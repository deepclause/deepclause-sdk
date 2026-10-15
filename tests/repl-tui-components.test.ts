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

import { StatusBarComponent } from '../src/repl/tui/status-bar.js';
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
  it('shows the call and its duration', () => {
    const pane = new DmlToolPane({ toolName: 'bash', args: { command: 'printf hello' } });
    pane.setResult({ stdout: 'hello\n', exitCode: 0 }, true, 9);
    const rendered = text(pane.render(60));
    expect(rendered).toContain('bash');
    expect(rendered).toContain('printf hello');
    expect(rendered).toContain('9ms');
  });

  it('advertises ctrl+o only when something is hidden', () => {
    const trivial = new DmlToolPane({ toolName: 'bash', args: { command: 'true' } });
    trivial.setResult({ stdout: 'ok' }, true);
    expect(text(trivial.render(60))).not.toContain('ctrl+o');

    const long = new DmlToolPane({ toolName: 'execute', args: { goal: 'rows' } });
    long.setResult({ stdout: Array.from({ length: 12 }, (_, i) => `row ${i}`).join('\n') }, true);
    expect(text(long.render(60))).toContain('ctrl+o to expand');
    expect(text(long.render(60))).toContain('more lines');
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

  it('clears the working indicator when a run ends or errors', () => {
    const { working, renderer } = build();
    // An aborted turn may never report the model call completed, so `error` and
    // `finished` must clear the dock spinner themselves or it animates forever.
    renderer.render({ type: 'model_call', modelCallState: 'started' });
    expect(working.at(-1)).toBe('working…');
    renderer.render({ type: 'error', content: 'cancelled by user' });
    expect(working.at(-1)).toBeNull();

    renderer.render({ type: 'model_call', modelCallState: 'started' });
    renderer.render({ type: 'finished', depth: 0 });
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

  it('keeps a multi-line argument on one indented line', () => {
    // A `content` argument with newlines used to leak them into the pane: the
    // wrapper did not split on them, so the text after a newline rendered at
    // column 0 (the stray `"""` in the report).
    const pane = new DmlToolPane({
      toolName: 'write_file',
      args: { content: '#!/usr/bin/env python3\n"""docstring"""\nprint(1)', path: 's.py' },
    });
    pane.setResult({ ok: true }, true, 1);
    const lines = pane.render(80);
    expect(lines.every((line) => !line.includes('\n'))).toBe(true);
    expect(lines.join('\n')).toContain('#!/usr/bin/env python3');
    expect(lines.join('\n')).toContain('⏎');
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

describe('tree indentation', () => {
  function render(events: Array<Record<string, unknown>>): string[] {
    const document = new TranscriptDocument();
    const renderer = new TuiRenderer({ document });
    for (const event of events) renderer.render(event as never);
    return document.render(70).map(plain).map((line) => line.replace(/\s+$/, ''));
  }

  it('hangs a tool pane off its frame prefix with a single stem', () => {
    const lines = render([
      { type: 'tool_call', toolName: 'bash', toolState: 'completed', toolResult: { stdout: 'hi' }, frameKind: 'run', depth: 0 },
    ]);
    // Header under the frame prefix, body indented under the header — the shape the
    // line renderer has always used for tool detail.
    expect(lines[0]).toMatch(/^run │ {3}✔ bash/);
    expect(lines[1]).toMatch(/^run │ {5}hi$/);
    // One stem: the frame's. The pane used to add a second at a different column,
    // so a nested call showed `    │   │` while surrounding output showed `    │`.
    for (const line of lines) {
      expect((line.match(/│/g) ?? []).length).toBeLessThanOrEqual(1);
    }
  });

  it('keeps a nested pane at the nested depth', () => {
    const lines = render([
      { type: 'tool_call', toolName: 'bash', toolState: 'completed', toolResult: { stdout: '42' }, frameKind: 'nested', depth: 1 },
      { type: 'output', content: 'plain nested output', frameKind: 'nested', depth: 1 },
    ]);
    expect(lines[0]).toMatch(/^ {4}│ {3}✔ bash/);
    // The pane's body and a plain output line agree on where the frame's stem is.
    // Found by content: a blank line now separates a component from following text.
    const paneBody = lines.find((line) => line.includes('42'))!;
    const plainOutput = lines.find((line) => line.includes('plain nested output'))!;
    expect(paneBody.indexOf('│')).toBe(plainOutput.indexOf('│'));
  });

  it('indents streamed answers inside their frame', () => {
    const lines = render([
      { type: 'stream', content: 'an answer', frameKind: 'run', depth: 0 },
    ]);
    expect(lines[0]).toMatch(/^run │ an answer/);
  });
});

describe('status bar', () => {
  const snapshot = () => ({
    model: { id: 'deepseek-flash', provider: 'pi', reasoning: true, thinkingLevel: 'medium' },
    totals: { inputTokens: 12500, outputTokens: 4200, cacheReadTokens: 8000, cacheWriteTokens: 100 },
    byGroup: {
      agent: { inputTokens: 9000, outputTokens: 3000, cacheReadTokens: 6000, cacheWriteTokens: 0 },
      child: { inputTokens: 3500, outputTokens: 1200, cacheReadTokens: 2000, cacheWriteTokens: 100 },
      judge: { inputTokens: 0, outputTokens: 0, cacheReadTokens: 0, cacheWriteTokens: 0 },
    },
    activeGroup: 'agent', activeDepth: 0, activeSlot: 'agent',
    context: {
      agent: { tokens: 9400, contextWindow: 128000, percent: 7 },
      harness: { tokens: 0, contextWindow: 128000, percent: 0 },
    },
    activeContext: { tokens: 9400, contextWindow: 128000, percent: 7 },
  } as never);

  function bar(overrides: Record<string, unknown> = {}) {
    return new StatusBarComponent({
      getSnapshot: snapshot,
      workspace: '/home/andreas/git/gh/deepclause-pi-dev/deepclause-sdk',
      sessionId: 'repl_mv82il31_1',
      harness: 'main',
      backend: 'pi',
      isFrozen: () => false,
      isRunning: () => true,
      ...overrides,
    });
  }

  it('is one row that fits the width, at every width', () => {
    for (const width of [24, 32, 40, 60, 80, 100, 120, 200]) {
      const lines = bar().render(width);
      expect(lines).toHaveLength(1);
      expect(visibleWidth(lines[0]!)).toBeLessThanOrEqual(width);
    }
  });

  it('never cuts a segment in half', () => {
    // The failure this replaced read `deepseek-flash · ` — a separator with nothing
    // after it, which is indistinguishable from a short value.
    for (const width of [24, 30, 40, 60, 80, 120, 200]) {
      const visible = plain(bar().render(width)[0]!).trimEnd();
      expect(visible.endsWith('·')).toBe(false);
      expect(visible.includes(' ·  · ')).toBe(false);
      expect(visible.includes('  ·')).toBe(false);
    }
  });

  it('keeps the model, and drops the counters before it', () => {
    // A run is identified by its model; the token counters are the least important
    // thing on the row, so they go first when space runs out.
    for (const width of [40, 60, 80, 120]) {
      expect(plain(bar().render(width)[0]!)).toContain('deepseek-flash · medium');
    }
    expect(plain(bar().render(40)[0]!)).not.toContain('↑');
    expect(plain(bar().render(80)[0]!)).toContain('↑21k');
  });

  it('shortens the path, and drops it before the model', () => {
    const wide = plain(bar().render(140)[0]!);
    expect(wide).toContain('…/deepclause-sdk');
    expect(wide).not.toContain('/home/andreas/git/gh');
    expect(wide).toContain('harness main');
    expect(wide).toContain('backend pi');

    // At 24 columns only the model survives — right-aligned, not truncated.
    const narrow = plain(bar().render(24)[0]!).trim();
    expect(narrow).toBe('deepseek-flash · medium');
  });

  it('says `running`, and only spells out the steering hint when there is room', () => {
    expect(plain(bar().render(40)[0]!)).toContain('running');
    expect(plain(bar().render(40)[0]!)).not.toContain('type to steer');
    expect(plain(bar().render(140)[0]!)).toContain('running (type to steer)');
  });

  it('says when it is frozen', () => {
    expect(plain(bar({ isFrozen: () => true }).render(140)[0]!)).toContain('FROZEN');
  });
});

describe('separation between blocks', () => {
  function lines(events: Array<Record<string, unknown>>): string[] {
    const document = new TranscriptDocument();
    const renderer = new TuiRenderer({ document });
    for (const event of events) renderer.render(event as never);
    return document.render(72).map(plain).map((line) => line.replace(/\s+$/, ''));
  }

  const pane = (name: string) => ({
    type: 'tool_call', toolName: name, toolState: 'completed', toolResult: { stdout: 'out' }, frameKind: 'run', depth: 0,
  });

  it('keeps a blank line between text and a tool call, and back again', () => {
    const rows = lines([
      { type: 'stream', content: 'looking at the module', frameKind: 'run', depth: 0 },
      pane('listing'),
      { type: 'solution', solution: { bindings: [{ name: 'R', value: '1' }] }, frameKind: 'run', depth: 0 },
      pane('execute'),
      { type: 'stream', content: 'now I will write it', frameKind: 'run', depth: 0 },
    ]);
    // Four transitions: text→pane, pane→text, text→pane, pane→text.
    expect(rows.filter((line) => line === '')).toHaveLength(4);
    // Never two blanks in a row, and never a blank at the very start.
    expect(rows[0]).not.toBe('');
    for (let i = 1; i < rows.length; i += 1) {
      if (rows[i] === '') expect(rows[i - 1]).not.toBe('');
    }
    // Each pane is preceded by a blank.
    for (const name of ['listing', 'execute']) {
      const index = rows.findIndex((line) => line.includes(name));
      expect(index).toBeGreaterThan(0);
      expect(rows[index - 1]).toBe('');
    }
  });

  it('does not separate chunks of the same streamed block', () => {
    const rows = lines([
      { type: 'stream', content: 'one ', frameKind: 'run', depth: 0 },
      { type: 'stream', content: 'two', frameKind: 'run', depth: 0 },
    ]);
    expect(rows.filter((line) => line === '')).toHaveLength(0);
  });

  it('adds nothing for events that render nothing', () => {
    // A model call or a usage update is not output; separating on them would open a
    // gap in the transcript for every turn.
    const rows = lines([
      { type: 'output', content: 'first', frameKind: 'run', depth: 0 },
      { type: 'usage', usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 }, frameKind: 'run', depth: 0 },
      { type: 'model_call', modelCallState: 'started' } as never,
      { type: 'output', content: 'second', frameKind: 'run', depth: 0 },
    ]);
    expect(rows.filter((line) => line === '')).toHaveLength(0);
  });
});

describe('wrapped lines keep their position in the tree', () => {
  function render(line: string, width: number): string[] {
    const document = new TranscriptDocument();
    document.appendText(line);
    return document.render(width).map(plain);
  }

  it('repeats the frame stem on continuation lines', () => {
    const wrapped = render(`run │ T = "${'x'.repeat(120)}"`, 40);
    expect(wrapped.length).toBeGreaterThan(1);
    for (const line of wrapped) expect(line.startsWith('run │ ')).toBe(true);
  });

  it('repeats the deeper indent of pane content', () => {
    const wrapped = render(`    │       ${'y'.repeat(120)}`, 40);
    expect(wrapped.length).toBeGreaterThan(1);
    for (const line of wrapped) expect(line.startsWith('    │       ')).toBe(true);
  });

  it('repeats a plain leading indent, and leaves flush text alone', () => {
    const indented = render(`  ${'z'.repeat(120)}`, 40);
    for (const line of indented) expect(line.startsWith('  ')).toBe(true);

    const flush = render('a '.repeat(60).trim(), 40);
    for (const line of flush) expect(line.startsWith(' ')).toBe(false);
  });
});

describe('streamed text belongs to the turn that produced it', () => {
  function rows(events: Array<Record<string, unknown>>): string[] {
    const document = new TranscriptDocument();
    const renderer = new TuiRenderer({ document });
    for (const event of events) renderer.render(event as never);
    return document.render(72).map(plain).map((line) => line.replace(/\s+$/, ''));
  }

  const textBlock = (content: string) => ({ type: 'stream', content, frameKind: 'run', depth: 0 });

  it('starts a new block after a run finishes', () => {
    // The bug: the second turn's text was appended to the first turn's block, which
    // sits earlier in the transcript — so the reply rendered *above* the prompt.
    const lines = rows([
      textBlock('first reply'),
      { type: 'finished' },
      { type: 'output', content: 'what is up', frameKind: 'run', depth: 0 },
      textBlock('second reply'),
    ]);
    const joined = lines.join('\n');
    expect(joined).toContain('first reply');
    expect(joined).toContain('second reply');
    // They are separate lines, not one run-on block.
    expect(lines.some((line) => line.includes('first reply') && line.includes('second reply'))).toBe(false);
    // And the second reply comes after the prompt, not before it.
    const promptIndex = lines.findIndex((line) => line.includes('what is up'));
    const secondIndex = lines.findIndex((line) => line.includes('second reply'));
    expect(secondIndex).toBeGreaterThan(promptIndex);
  });

  it('does not close the block for a nested run', () => {
    // A nested frame finishes many times inside one answer; closing on it would chop
    // the answer into pieces.
    const lines = rows([
      textBlock('part one '),
      { type: 'finished', frameKind: 'nested', depth: 1 },
      textBlock('part two'),
    ]);
    expect(lines.some((line) => line.includes('part one part two'))).toBe(true);
  });

  it('can be closed by the host', () => {
    const document = new TranscriptDocument();
    const renderer = new TuiRenderer({ document });
    renderer.render(textBlock('first') as never);
    renderer.endTextBlock();
    renderer.render(textBlock('second') as never);
    const lines = document.render(72).map(plain);
    expect(lines.some((line) => line.includes('first') && line.includes('second'))).toBe(false);
  });
});
