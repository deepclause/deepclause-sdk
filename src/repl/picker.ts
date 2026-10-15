/**
 * A small readline picker.
 *
 * pi's catalog has hundreds of models across dozens of providers, so printing
 * an exhaustive numbered list ("endless lists") is unusable. Instead we show a
 * bounded page and let the user type to filter, then select by number, id, or
 * Enter for the first match. No full TUI is needed.
 */

import * as readline from 'readline';

export interface PickItem {
  id: string;
  label: string;
  /** Optional annotation, e.g. `[reasoning]` or `✓ configured`. */
  hint?: string;
}

export interface PickOptions {
  message: string;
  items: PickItem[];
  pageSize?: number;
  /** Case-insensitive matcher over the raw (lowercased) query. */
  match?: (item: PickItem, query: string) => boolean;
  /** Marks the currently-selected item. */
  currentId?: string;
}

/** Pure filter helper (exported for tests). */
export function filterItems(
  items: PickItem[],
  query: string,
  match?: (item: PickItem, query: string) => boolean,
): PickItem[] {
  const needle = query.trim().toLowerCase();
  if (!needle) return items;
  const matcher = match
    ?? ((item: PickItem, q: string) => item.label.toLowerCase().includes(q) || item.id.toLowerCase().includes(q));
  return items.filter((item) => matcher(item, needle));
}

/** Build the display block for one picker iteration (exported for tests). */
export function renderPick(
  options: PickOptions,
  query: string,
): { lines: string[]; page: PickItem[]; filtered: PickItem[] } {
  const pageSize = options.pageSize ?? 15;
  const filtered = filterItems(options.items, query, options.match);
  const page = filtered.slice(0, pageSize);
  const lines: string[] = [options.message];
  if (options.currentId) {
    const current = options.items.find((item) => item.id === options.currentId);
    if (current) lines.push(`  current: ${current.label}`);
  }
  if (page.length === 0) {
    lines.push('  (no matches)');
  } else {
    page.forEach((item, index) => {
      lines.push(`  ${index + 1}. ${item.label}${item.hint ? `  ${item.hint}` : ''}`);
    });
    if (filtered.length > page.length) {
      lines.push(`  … ${filtered.length - page.length} more — type to filter`);
    }
  }
  lines.push(
    page.length === 0
      ? 'no matches; refine the filter or q to cancel'
      : `${page.length} of ${options.items.length} • number selects, Enter = first, q cancels`,
  );
  return { lines, page, filtered };
}

/** Interactive picker. Returns the chosen item id, or null on cancel / no items. */
export async function pickFromList(options: PickOptions): Promise<string | null> {
  if (options.items.length === 0) return null;
  if (options.items.length === 1) return options.items[0].id;
  if (process.stdin.isTTY && process.stdout.isTTY && typeof process.stdin.setRawMode === 'function') {
    return pickInteractive(options);
  }
  return pickReadline(options);
}

/** One keypress, classified. Exported for tests. */
export type PickKey = 'up' | 'down' | 'pageup' | 'pagedown' | 'enter' | 'cancel' | 'backspace' | 'text' | 'ignore';

export function classifyKey(input: string): PickKey {
  if (input === '\x1b[A' || input === '\x1bOA') return 'up';
  if (input === '\x1b[B' || input === '\x1bOB') return 'down';
  if (input === '\x1b[5~') return 'pageup';
  if (input === '\x1b[6~') return 'pagedown';
  if (input === '\r' || input === '\n') return 'enter';
  if (input === '\x1b' || input === '\x03' || input === '\x04') return 'cancel';
  if (input === '\x7f' || input === '\b') return 'backspace';
  if (input.length === 1 && input.charCodeAt(0) >= 32) return 'text';
  return 'ignore';
}

/**
 * Full-screen picker on a TTY: arrow keys scroll, typing filters live, Enter
 * selects, Esc cancels. Used by `deepclause login`/`set-backend` where the
 * catalog is too large for a paged list.
 */
function pickInteractive(options: PickOptions): Promise<string | null> {
  return new Promise((resolve) => {
    const pageSize = Math.max(3, options.pageSize ?? 12);
    const stdin = process.stdin;
    const stdout = process.stdout;
    let query = '';
    let selected = 0;
    let offset = 0;
    let printed = 0;

    const current = (): PickItem[] => filterItems(options.items, query, options.match);

    const frame = (): string[] => {
      const items = current();
      selected = Math.max(0, Math.min(selected, items.length - 1));
      if (selected < offset) offset = selected;
      if (selected >= offset + pageSize) offset = selected - pageSize + 1;
      const view = items.slice(offset, offset + pageSize);
      const lines: string[] = [options.message];
      if (options.currentId) {
        const item = options.items.find((candidate) => candidate.id === options.currentId);
        if (item) lines.push(`  current: ${item.label}`);
      }
      lines.push(`  filter: ${query || '(type to filter)'}`);
      if (view.length === 0) lines.push('  (no matches)');
      for (let index = 0; index < view.length; index += 1) {
        const item = view[index];
        const marker = offset + index === selected ? '❯' : ' ';
        lines.push(`${marker} ${item.label}${item.hint ? `  ${item.hint}` : ''}`);
      }
      const position = items.length > pageSize ? `  ${selected + 1}/${items.length}` : `  ${items.length}`;
      lines.push(`${position} · ↑↓ move · type filter · enter select · esc cancel`);
      return lines;
    };

    const draw = (): void => {
      const lines = frame();
      if (printed > 0) stdout.write(`\x1b[${printed}A`);
      stdout.write('\x1b[0J');
      stdout.write(`${lines.join('\n')}\n`);
      printed = lines.length;
    };

    const cleanup = (value: string | null): void => {
      stdin.off('data', onData);
      if (stdin.setRawMode) stdin.setRawMode(false);
      stdin.pause();
      stdout.write('\x1b[?25h');
      resolve(value);
    };

    const onData = (data: Buffer | string): void => {
      const input = typeof data === 'string' ? data : data.toString('utf8');
      switch (classifyKey(input)) {
        case 'up': selected -= 1; draw(); return;
        case 'down': selected += 1; draw(); return;
        case 'pageup': selected -= pageSize; draw(); return;
        case 'pagedown': selected += pageSize; draw(); return;
        case 'backspace': query = query.slice(0, -1); selected = 0; offset = 0; draw(); return;
        case 'text': query += input; selected = 0; offset = 0; draw(); return;
        case 'enter': {
          const items = current();
          cleanup(items[selected] ? items[selected].id : null);
          return;
        }
        case 'cancel': cleanup(null); return;
        default: return;
      }
    };

    stdin.setRawMode?.(true);
    stdin.resume();
    stdout.write('\x1b[?25l');
    stdin.on('data', onData);
    draw();
  });
}

/** Readline fallback for non-TTY stdin (piped input, tests). */
async function pickReadline(options: PickOptions): Promise<string | null> {
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
  const ask = (question: string): Promise<string> => new Promise((resolve) => rl.question(question, resolve));
  let query = '';
  try {
    for (;;) {
      const { lines, page } = renderPick(options, query);
      process.stdout.write(`${lines.join('\n')}\n`);
      const answer = (await ask('> ')).trim();
      if (answer === 'q' || answer === 'quit') return null;
      if (answer === '') {
        if (page.length > 0) return page[0].id;
        continue;
      }
      const asNumber = Number(answer);
      if (Number.isInteger(asNumber) && asNumber >= 1 && asNumber <= page.length) {
        return page[asNumber - 1].id;
      }
      const exact = options.items.find((item) => item.id === answer);
      if (exact) return exact.id;
      query = answer;
    }
  } finally {
    rl.close();
  }
}
