/**
 * TUI building blocks: transcript line handling and the status bar.
 */
import { describe, expect, it } from 'vitest';

import { LineSink, TranscriptComponent } from '../src/repl/tui/transcript.js';
import { StatusBarComponent } from '../src/repl/tui/status-bar.js';
import { dim, editorTheme, formatInputAnswer, promptBorder, promptLabel, renderSteerLine, renderUserLine } from '../src/repl/tui/theme.js';
import { PromptEditor } from '../src/repl/tui/editor.js';
import { visibleWidth, type TUI } from '@earendil-works/pi-tui';
import { ReplStatusTracker } from '../src/repl/status.js';

describe('TranscriptComponent + LineSink', () => {
  it('commits complete lines and keeps an unterminated line pending', () => {
    const transcript = new TranscriptComponent();
    const sink = new LineSink(transcript);
    sink.write('first\nsecond');
    expect(transcript.render(80)).toEqual(['first', 'second']);
    sink.write('\n');
    expect(transcript.render(80)).toEqual(['first', 'second']);
    expect(transcript.getLines()).toEqual(['first', 'second']);
  });

  it('handles carriage-return spinner updates by replacing the live line', () => {
    const transcript = new TranscriptComponent();
    const sink = new LineSink(transcript);
    sink.write('\r\u001b[K⠋ working');
    expect(transcript.render(80)).toEqual(['⠋ working']);
    sink.write('\r\u001b[K✔ done');
    expect(transcript.render(80)).toEqual(['✔ done']);
    sink.write('\n');
    expect(transcript.getLines()).toEqual(['✔ done']);
  });

  it('accumulates streamed text until a newline', () => {
    const transcript = new TranscriptComponent();
    const sink = new LineSink(transcript);
    sink.write('hel');
    sink.write('lo');
    expect(transcript.render(80)).toEqual(['hello']);
    sink.write('\n');
    expect(transcript.getLines()).toEqual(['hello']);
  });

  it('bounds the transcript length', () => {
    const transcript = new TranscriptComponent(3);
    const sink = new LineSink(transcript);
    for (let index = 0; index < 6; index += 1) sink.write(`line ${index}\n`);
    expect(transcript.getLines()).toEqual(['line 3', 'line 4', 'line 5']);
  });

  it('collapses consecutive blank lines', () => {
    const transcript = new TranscriptComponent();
    transcript.append('a');
    transcript.append('');
    transcript.append('');
    transcript.append('b');
    expect(transcript.getLines()).toEqual(['a', '', 'b']);
  });

  it('renders user entries as full-width blue blocks', () => {
    const transcript = new TranscriptComponent();
    transcript.append('hello', 'user');
    const rendered = transcript.render(20);
    expect(rendered[0]).toContain('\u001b[48;5;24m');
    expect(rendered[0].replace(/\u001b\[[0-9;]*m/g, '')).toHaveLength(20);
  });

  it('renders steer entries as full-width Steering blocks', () => {
    const transcript = new TranscriptComponent();
    transcript.append('focus on X', 'steer');
    const rendered = transcript.render(40);
    expect(rendered[0]).toContain('\u001b[48;5;94m');
    expect(rendered[0].replace(/\u001b\[[0-9;]*m/g, '')).toHaveLength(40);
  });

  it('wraps long user entries without exceeding the width', () => {
    const transcript = new TranscriptComponent();
    transcript.append('x'.repeat(100), 'user');
    const rendered = transcript.render(20);
    expect(rendered.length).toBeGreaterThan(1);
    for (const line of rendered) {
      expect(line.replace(/\u001b\[[0-9;]*m/g, '').length).toBeLessThanOrEqual(20);
    }
  });

  it('wraps over-long lines (with ANSI) to the render width', () => {
    const transcript = new TranscriptComponent();
    const sink = new LineSink(transcript);
    sink.write(`\u001b[32m${'x'.repeat(500)}\u001b[0m\n`);
    const lines = transcript.render(40);
    expect(lines.length).toBeGreaterThan(1);
    for (const line of lines) {
      expect(line.replace(/\u001b\[[0-9;]*m/g, '').length).toBeLessThanOrEqual(40);
    }
  });
});

describe('TUI theme', () => {
  it('renders a user line as a full-width blue block', () => {
    const line = renderUserLine('hello', 20);
    expect(line).toContain('\u001b[48;5;24m');
    const visible = line.replace(/\u001b\[[0-9;]*m/g, '');
    expect(visible).toBe(` hello ${' '.repeat(13)}`);
  });

  it('colors input answers distinctly', () => {
    const line = formatInputAnswer('yes');
    expect(line).toContain('\u001b[33m');
    expect(line.replace(/\u001b\[[0-9;]*m/g, '')).toBe('> yes');
  });

  it('provides a dimming helper and a blue-bordered editor', () => {
    expect(dim('x')).toContain('\u001b[2m');
    const theme = editorTheme();
    expect(theme.borderColor('─')).toContain('\u001b[38;5;75m');
    expect(theme.selectList.selectedText('x')).toContain('\u001b[36m');
  });

  it('styles the input-needed prompt distinctly', () => {
    expect(promptBorder('──')).toContain('\u001b[38;5;214m');
    expect(promptLabel('? x')).toContain('\u001b[38;5;214m');
    expect(promptLabel('? x').replace(/\u001b\[[0-9;]*m/g, '')).toBe('? x');
  });

  it('renders a steering line as a full-width "Steering:" block', () => {
    const line = renderSteerLine('focus on X', 40);
    expect(line).toContain('\u001b[48;5;94m');
    const visible = line.replace(/\u001b\[[0-9;]*m/g, '');
    expect(visible.startsWith(' Steering: focus on X')).toBe(true);
    expect(visible).toHaveLength(40);
  });
});

describe('PromptEditor', () => {
  it('shows the question in the border at the exact width', () => {
    const tui = { requestRender() {}, setFocus() {}, terminal: { columns: 60, rows: 20 } } as unknown as TUI;
    const editor = new PromptEditor(tui, editorTheme());
    editor.setPrompt('What is your name?');
    const lines = editor.render(60);
    expect(lines[0]).toContain('? What is your name?');
    expect(lines[0]).toContain('\u001b[38;5;214m');
    for (const line of lines) {
      expect(visibleWidth(line)).toBe(60);
    }
  });

  it('uses the normal border when no prompt is set', () => {
    const tui = { requestRender() {}, setFocus() {}, terminal: { columns: 40, rows: 20 } } as unknown as TUI;
    const editor = new PromptEditor(tui, editorTheme());
    const lines = editor.render(40);
    expect(lines[0]).not.toContain('?');
    expect(lines[0]).toContain('\u001b[38;5;75m');
  });
});

describe('StatusBarComponent', () => {
  it('renders workspace, usage, context, and model within the width', () => {
    const tracker = new ReplStatusTracker({
      model: { id: 'gpt-4o', provider: 'openai', reasoning: true, thinkingLevel: 'high' },
      contextWindow: 128_000,
    });
    tracker.observe({
      type: 'usage',
      usage: { inputTokens: 1_000, outputTokens: 200, totalTokens: 1_200, cacheReadTokens: 500 },
    });
    const bar = new StatusBarComponent({
      getSnapshot: () => tracker.snapshot(),
      workspace: '/work/project',
      sessionId: 'repl_1',
      backend: 'pi',
      isFrozen: () => false,
    });

    // One row: where the harness lives on the left, the model on the right, and the
    // counters between them. Anything that does not fit is dropped whole, so a
    // longer row carries the same segments and the session id is on the exit footer.
    const lines = bar.render(100);
    expect(lines).toHaveLength(1);
    const row = lines[0]!;
    expect(row).toContain('/work/project');
    expect(row).toContain('↑1.5k');
    expect(row).toContain('agent 1.5k/128k');
    expect(row).toContain('gpt-4o · high');
    // The session id only appears when the row is wide enough for it.
    expect(bar.render(200)[0]).toContain('repl_1');
    for (const line of lines) {
      // strip ANSI for width measurement
      expect(line.replace(/\u001b\[[0-9;]*m/g, '').length).toBeLessThanOrEqual(100);
    }
  });

  it('shows a FROZEN marker', () => {
    const tracker = new ReplStatusTracker({ model: { id: 'x', provider: 'env' } });
    const bar = new StatusBarComponent({
      getSnapshot: () => tracker.snapshot(),
      workspace: '/w',
      sessionId: 's',
      backend: 'env',
      isFrozen: () => true,
    });
    expect(bar.render(120)[0]).toContain('FROZEN');
  });

  it('shows a running/steering hint while a turn is active', () => {
    const tracker = new ReplStatusTracker({ model: { id: 'x', provider: 'env' } });
    const bar = new StatusBarComponent({
      getSnapshot: () => tracker.snapshot(),
      workspace: '/w',
      sessionId: 's',
      backend: 'env',
      isRunning: () => true,
    });
    expect(bar.render(120)[0]).toContain('type to steer');
  });
});
