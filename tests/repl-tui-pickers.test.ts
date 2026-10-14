/**
 * The chooser overlay (model, reasoning) and the live reasoning switch.
 *
 * The picker is a component the host places with `tui.showOverlay`; what matters
 * here is that it renders its items, marks the current one, and reports a selection
 * or a cancel. The host's overlay wiring is exercised by hand in a pty, since arrow
 * keys through a scripted pty depend on escape-sequence timing.
 */
import { describe, expect, it, vi } from 'vitest';
import { visibleWidth } from '@earendil-works/pi-tui';

import { ChoicePicker } from '../src/repl/tui/picker.js';
import { REPL_COMMANDS } from '../src/repl/tui/autocomplete.js';
import { ReplRenderer } from '../src/repl/render.js';

const plain = (line: string): string => line.replace(/\u001b\[[0-9;]*[a-zA-Z]/g, '');

const ITEMS = [
  { value: 'off', label: 'off', description: 'hide reasoning entirely' },
  { value: 'summary', label: 'summary', description: 'a bounded preview' },
  { value: 'full', label: 'full', description: 'everything the model emits' },
];

describe('ChoicePicker', () => {
  it('renders the title, hint and every item within the width', () => {
    const picker = new ChoicePicker({
      title: 'Reasoning',
      hint: 'How much of the model thinking to show',
      items: ITEMS,
      current: 'summary',
      onSelect: () => {},
      onCancel: () => {},
    });
    const lines = picker.render(50).map(plain);
    expect(lines[0]).toContain('Reasoning');
    expect(lines[1]).toContain('How much of the model');
    const body = lines.slice(2).join('\n');
    for (const item of ITEMS) expect(body).toContain(item.label);
    for (const line of picker.render(50)) {
      expect(visibleWidth(line)).toBeLessThanOrEqual(50);
    }
  });

  it('reports the selection', () => {
    const onSelect = vi.fn();
    const onCancel = vi.fn();
    const picker = new ChoicePicker({
      title: 'Reasoning', items: ITEMS, current: 'summary', onSelect, onCancel,
    });
    picker.handleInput('\r');
    expect(onCancel).not.toHaveBeenCalled();
    expect(onSelect).toHaveBeenCalledTimes(1);
    expect(onSelect.mock.calls[0]?.[0]).toMatchObject({ value: 'summary' });
  });

  it('reports a cancel', () => {
    const onSelect = vi.fn();
    const onCancel = vi.fn();
    const picker = new ChoicePicker({ title: 'Reasoning', items: ITEMS, onSelect, onCancel });
    picker.handleInput('\u001b');
    expect(onSelect).not.toHaveBeenCalled();
    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  it('sizes itself to its content, so the host can bound the overlay', () => {
    const picker = new ChoicePicker({
      title: 'Model', hint: 'hint', items: ITEMS, onSelect: () => {}, onCancel: () => {},
    });
    expect(picker.contentHeight).toBe(2 + 1 + ITEMS.length);
  });
});

describe('the reasoning display mode can change while running', () => {
  it('shows reasoning after switching from off to full', () => {
    const written: string[] = [];
    const renderer = new ReplRenderer({ mode: 'rich', reasoning: 'off' });
    renderer.setSink((text) => written.push(text));

    const reasoning = { type: 'reasoning' as const, content: 'weighing the options', frameKind: 'run' as const, depth: 0 };
    renderer.render(reasoning);
    expect(written.join('')).not.toContain('weighing the options');

    renderer.setReasoningMode('full');
    expect(renderer.getReasoningMode()).toBe('full');
    renderer.render(reasoning);
    expect(written.join('')).toContain('weighing the options');
  });
});

describe('the two reasoning settings are distinct', () => {
  it('classifies /reasoning as the level and /show-reasoning as the display mode', async () => {
    const { classifyReplInput } = await import('../src/cli/repl.js');
    expect(classifyReplInput('/reasoning')).toMatchObject({ kind: 'reasoning' });
    expect(classifyReplInput('/reasoning high')).toMatchObject({ kind: 'reasoning', level: 'high' });
    expect(classifyReplInput('/reasoning-level low')).toMatchObject({ kind: 'reasoning', level: 'low' });
    expect(classifyReplInput('/show-reasoning')).toMatchObject({ kind: 'show-reasoning' });
    expect(classifyReplInput('/reasoning-summary')).toMatchObject({ kind: 'show-reasoning' });
  });

  it('applies a level change to the backend and the status bar', async () => {
    const { createPiAdapter } = await import('../src/repl/backend.js');
    // The adapter is what carries the level; the CLI calls these two on it.
    const source = await import('node:fs/promises').then((fs) => fs.readFile('src/repl/backend.ts', 'utf8'));
    expect(source).toContain('setThinkingLevel(level: string): void');
    // Per call, not captured: a captured level ignored the picker entirely.
    expect(source).toContain('const baseResolved = (): ReturnType<typeof resolveThinkingLevel> =>');
    expect(source).toContain('return completeWith(model, baseResolved(), maxTokens, request);');
    expect(typeof createPiAdapter).toBe('function');
  });

  it('updates the level shown next to the model', async () => {
    const { ReplStatusTracker } = await import('../src/repl/status.js');
    const tracker = new ReplStatusTracker({
      model: { id: 'gpt-4o', provider: 'openai', reasoning: true, thinkingLevel: 'medium' },
      contextWindow: 128_000,
    });
    expect(tracker.snapshot().model.thinkingLevel).toBe('medium');
    tracker.setThinkingLevel('high');
    expect(tracker.snapshot().model.thinkingLevel).toBe('high');
  });
});

describe('autocomplete and the parser agree', () => {
  // Names the parser accepts but deliberately does not advertise (aliases and the
  // shorter spellings users already know).
  const ALIASES = new Set([
    'exit', 'quit', 'reasoning-level', 'reasoning-summary', 'diagram', 'image', 'imagine',
  ]);

  /** A command that needs an argument, probed with one so it can be classified. */
  const PROBES: Record<string, string> = {
    goal: '/goal true',
    show: '/show x.png',
    gen: '/gen a cat',
    image: '/image x.png',
    imagine: '/imagine a cat',
  };
  const probe = (name: string): string => PROBES[name] ?? `/${name}`;

  /** Commands the parser handles that should be visible in the completion list. */
  const ADVERTISED = [
    'goal', 'listing', 'halt', 'help', 'reset', 'rollback', 'compact', 'export',
    'model', 'reasoning', 'show-reasoning', 'harness', 'freeze', 'unfreeze', 'params',
    'trace', 'notrace', 'viz', 'show', 'gen',
  ];

  it('advertises every command the parser accepts', async () => {
    const { classifyReplInput } = await import('../src/cli/repl.js');
    const listed = new Set(REPL_COMMANDS.map((command) => command.name));
    for (const name of ADVERTISED) {
      // `/reasoning` and `/show-reasoning` were missing from this list, so they
      // parsed but could not be found from the editor.
      expect(listed.has(name), `${name} is missing from the autocomplete list`).toBe(true);
      expect(classifyReplInput(probe(name)).kind, `/${name} does not parse`).not.toBe('unknown-command');
    }
  });

  it('lists nothing the parser rejects', async () => {
    const { classifyReplInput } = await import('../src/cli/repl.js');
    for (const command of REPL_COMMANDS) {
      expect(classifyReplInput(probe(command.name)).kind, `/${command.name} does not parse`).not.toBe(
        'unknown-command',
      );
    }
  });

  it('keeps the alias list honest', async () => {
    const { classifyReplInput } = await import('../src/cli/repl.js');
    for (const alias of ALIASES) {
      // An alias must still parse; otherwise it is a stale entry in this list.
      expect(classifyReplInput(probe(alias)).kind, `/${alias} does not parse`).not.toBe('unknown-command');
    }
  });
});
