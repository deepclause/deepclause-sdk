/**
 * Transcript framing for tool results. A completed tool used to print the last
 * few lines of its captured output, which for a JSON payload is arbitrary tail
 * fragments. It now reports the output size and kind, then the opening lines.
 */
import { describe, expect, it } from 'vitest';

import { ReplRenderer } from '../src/repl/render.js';
import { LineSink, TranscriptComponent } from '../src/repl/tui/transcript.js';
import type { DMLEvent } from '../src/types.js';

function renderEvents(events: DMLEvent[], width = 100): string[] {
  const transcript = new TranscriptComponent();
  const sink = new LineSink(transcript);
  const renderer = new ReplRenderer({
    mode: 'rich',
    color: false,
    verbose: false,
    events: true,
    width,
    sink: (text) => sink.write(text),
  });
  for (const event of events) renderer.render(event);
  return transcript.render(width).map((line) => line.replace(/\u001b\[[0-9;]*m/g, '').trimEnd());
}

const bash = (result: Record<string, unknown>): DMLEvent => ({
  type: 'tool_call',
  toolName: 'bash',
  toolState: 'completed',
  toolArgs: { command: 'python3 search.py' },
  toolDurationMs: 7100,
  toolResult: result,
});

describe('tool result framing', () => {
  it('summarises a large JSON result and shows its head, not its tail', () => {
    const stdout = `{\n${Array.from({ length: 400 }, (_, i) => `  "line${i}": "value ${i}",`).join('\n')}\n}`;
    const lines = renderEvents([bash({ exitCode: 0, stdout, stderr: '' })]);
    const joined = lines.join('\n');
    expect(joined).toMatch(/\d+ lines · [\d.]+ kB · JSON/);
    expect(joined).toContain('--verbose');
    // the opening brace, not the closing tail
    expect(joined).toContain('{');
    expect(lines.some((line) => line.includes('"line399"'))).toBe(false);
    expect(lines.some((line) => line.includes('"line0"'))).toBe(true);
  });

  it('reports the exit code of a failed command', () => {
    const lines = renderEvents([
      { type: 'tool_call', toolName: 'bash', toolState: 'completed', toolExitCode: 2, toolDurationMs: 30, toolResult: { exitCode: 2, stdout: '', stderr: 'boom\n' } },
    ]);
    expect(lines.join('\n')).toContain('exit 2');
  });

  it('does not annotate short output', () => {
    const lines = renderEvents([bash({ exitCode: 0, stdout: 'ok\n', stderr: '' })]);
    expect(lines.join('\n')).not.toContain('--verbose');
    expect(lines.join('\n')).toContain('ok');
  });

  it('keeps every line within the render width', () => {
    const lines = renderEvents([bash({ exitCode: 0, stdout: `{"a": "${'x'.repeat(300)}"}`, stderr: '' })], 80);
    for (const line of lines) expect(line.length).toBeLessThanOrEqual(80);
  });

  it('renders a refused tool call as a failure, with the reason', () => {
    // A refusal reaches the model correctly and used to render as a tick, which is
    // why a working guard read as "the tool ran again".
    const lines = renderEvents([
      {
        type: 'tool_call',
        toolName: 'extract_authors',
        toolState: 'failed',
        toolArgs: { arg1: 'x' },
        toolError: "Tool 'extract_authors' is not available in this task: it is already running further up the call chain.",
        toolDurationMs: 12,
      },
    ]);
    const text = lines.join('\n');
    expect(text).toContain('✖');
    expect(text).not.toContain('✔');
    expect(text).toContain('already running further up the call chain');
  });
});
