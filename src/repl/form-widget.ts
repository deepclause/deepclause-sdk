/**
 * A form editing widget: focus between fields, edit text, toggle booleans, cycle
 * selects, then submit or cancel.
 *
 * The render and key handling are pure functions (exported for tests); the
 * driver at the bottom takes over a raw TTY like `pickFromList`, with a
 * line-oriented fallback for non-TTY stdin.
 */
import * as readline from 'readline';

import {
  actionsOf,
  isPlainObject,
  valueAtPath,
  type FormAction,
  type FormChange,
  type FormIssue,
  type FormSpec,
  type Json,
  type JsonObject,
} from '../forms/spec.js';

export interface FormWidgetState {
  spec: FormSpec;
  values: JsonObject;
  issues: FormIssue[];
  /** Index into `rows(spec)`. */
  focus: number;
  /** Editing the focused field as text. */
  editing: boolean;
  buffer: string;
}

export type FormRow = { kind: 'field'; path: string; name: string; title: string; type: string; required: boolean; options?: string[] } | { kind: 'action'; action: FormAction };

/** The rows the widget navigates: editable fields, then the actions. */
export function rows(spec: FormSpec): FormRow[] {
  const out: FormRow[] = [];
  for (const ref of flatten(spec.fields ?? [])) out.push(ref);
  for (const action of actionsOf(spec)) out.push({ kind: 'action', action });
  return out;
}

function flatten(fields: FormSpec['fields'], prefix = ''): FormRow[] {
  const out: FormRow[] = [];
  for (const field of fields) {
    const path = prefix ? `${prefix}.${field.name}` : field.name;
    if (field.type === 'object') {
      out.push(...flatten(field.properties, path));
      continue;
    }
    if (field.widget === 'hidden') continue;
    const options = field.type === 'string' && typeof (field as { enum?: string[] }).enum
      ? (field as { enum?: string[] }).enum
      : undefined;
    out.push({
      kind: 'field',
      path,
      name: field.name,
      title: field.title ?? field.name,
      type: field.type,
      required: field.required ?? false,
      options,
    });
  }
  return out;
}

export function initialState(spec: FormSpec, values: JsonObject = {}, issues: FormIssue[] = []): FormWidgetState {
  return { spec, values, issues, focus: 0, editing: false, buffer: '' };
}

/** The issue for a path, if any (error wins over warning). */
function issueFor(issues: FormIssue[], path: string): FormIssue | undefined {
  return issues.find((entry) => entry.path === path && entry.level === 'error')
    ?? issues.find((entry) => entry.path === path);
}

function display(value: Json | undefined): string {
  if (value === undefined || value === null) return '';
  return Array.isArray(value) ? value.join(', ') : String(value);
}

/** The widget's display block (exported for tests). */
export function renderForm(state: FormWidgetState): string[] {
  const { spec, values, issues, focus, editing, buffer } = state;
  const list = rows(spec);
  const lines: string[] = [];
  const title = spec.title ?? 'form';
  lines.push(spec.description ? `${title} — ${spec.description}` : title);

  list.forEach((row, index) => {
    const marker = index === focus ? '❯' : ' ';
    if (row.kind === 'action') {
      const label = row.action.kind === 'primary' ? `[ ${row.action.title} ]` : `( ${row.action.title} )`;
      lines.push(`${marker} ${label}`);
      return;
    }
    const issue = issueFor(issues, row.path);
    const raw = valueAtPath(values, row.path);
    const shown = index === focus && editing ? buffer : display(raw);
    const cursor = index === focus && editing ? '▌' : '';
    const star = row.required ? ' *' : '';
    lines.push(`${marker} ${row.title}${star}: ${shown}${cursor}`);
    if (issue) lines.push(`      ${issue.level === 'error' ? '✗' : '!'} ${issue.message}`);
  });

  lines.push(editing
    ? 'enter commit · esc cancel edit'
    : '↑↓ move · enter edit/activate · space toggle · ^s submit · esc cancel');
  return lines;
}

export type FormKey = 'up' | 'down' | 'enter' | 'escape' | 'space' | 'submit' | 'backspace' | 'text' | 'ignore';

export function classifyFormKey(input: string): FormKey {
  if (input === '\x1b[A' || input === '\x1bOA') return 'up';
  if (input === '\x1b[B' || input === '\x1bOB') return 'down';
  if (input === '\r' || input === '\n') return 'enter';
  if (input === '\x13') return 'submit';
  if (input === '\x1b' || input === '\x03' || input === '\x04') return 'escape';
  if (input === ' ') return 'space';
  if (input === '\x7f' || input === '\b') return 'backspace';
  if (input.length === 1 && input.charCodeAt(0) >= 32) return 'text';
  return 'ignore';
}

export interface FormKeyResult {
  state: FormWidgetState;
  action?: { type: 'submit' | 'cancel'; action: string; values: JsonObject };
}

/** Apply one key. Pure: returns the next state and, optionally, a terminal action. */
export function applyFormKey(state: FormWidgetState, key: FormKey, text = ''): FormKeyResult {
  const list = rows(state.spec);
  const focused = list[state.focus];

  if (state.editing) {
    if (key === 'escape') return { state: { ...state, editing: false, buffer: '' } };
    if (key === 'backspace') return { state: { ...state, buffer: state.buffer.slice(0, -1) } };
    if (key === 'text') return { state: { ...state, buffer: state.buffer + text } };
    if (key === 'enter') {
      const path = focused?.kind === 'field' ? focused.path : '';
      const value = coerce(focused?.kind === 'field' ? focused.type : 'string', state.buffer);
      return { state: { ...commit(state, path, value), editing: false, buffer: '' } };
    }
    return { state };
  }

  if (key === 'up') return { state: { ...state, focus: (state.focus - 1 + list.length) % list.length } };
  if (key === 'down') return { state: { ...state, focus: (state.focus + 1) % list.length } };
  if (key === 'escape') {
    const cancel = list.find(
      (row): row is { kind: 'action'; action: FormAction } =>
        row.kind === 'action' && row.action.name === 'cancel',
    );
    return { state, action: { type: 'cancel', action: cancel?.action.name ?? 'cancel', values: state.values } };
  }
  if (key === 'submit') {
    const submit = list.find(
      (row): row is { kind: 'action'; action: FormAction } =>
        row.kind === 'action' && row.action.name !== 'cancel',
    );
    return { state, action: { type: 'submit', action: submit?.action.name ?? 'submit', values: state.values } };
  }
  if (!focused) return { state };

  if (key === 'space' && focused.kind === 'field') {
    if (focused.type === 'boolean') {
      return { state: commit(state, focused.path, valueAtPath(state.values, focused.path) !== true) };
    }
    if (focused.options && focused.options.length > 0) {
      const current = display(valueAtPath(state.values, focused.path));
      const next = focused.options[(focused.options.indexOf(current) + 1) % focused.options.length];
      return { state: commit(state, focused.path, next) };
    }
  }

  if (key === 'enter') {
    if (focused.kind === 'action') {
      const type = focused.action.name === 'cancel' ? 'cancel' : 'submit';
      return { state, action: { type, action: focused.action.name, values: state.values } };
    }
    return { state: { ...state, editing: true, buffer: display(valueAtPath(state.values, focused.path)) } };
  }

  return { state };
}

function commit(state: FormWidgetState, path: string, value: Json): FormWidgetState {
  if (!path) return state;
  const values = { ...state.values };
  const parts = path.split('.');
  let node: JsonObject = values;
  for (const part of parts.slice(0, -1)) {
    const next = node[part];
    node = isPlainObject(next) ? next as JsonObject : {};
    if (!isPlainObject(values[part])) (values as JsonObject)[part] = node;
  }
  node[parts[parts.length - 1]] = value;
  return { ...state, values };
}

function coerce(type: string, raw: string): Json {
  if (type === 'number' || type === 'integer') {
    const n = Number(raw);
    return Number.isFinite(n) ? n : raw;
  }
  if (type === 'boolean') return /^(y|yes|true|1)$/i.test(raw);
  return raw;
}

// ---------------------------------------------------------------------------
// Driver
// ---------------------------------------------------------------------------

export interface PickFormOptions {
  spec: FormSpec;
  values?: JsonObject;
  issues?: FormIssue[];
  /** Called after each submit so the caller can fold in hard-validation errors. */
  validate?: (values: JsonObject) => FormIssue[];
  out?: NodeJS.WriteStream;
  input?: NodeJS.ReadStream;
}

/**
 * Interactive form. Returns the submitted change, or null on cancel. A `submit`
 * runs `validate`; if it returns errors they are shown and editing continues, so
 * the widget never hands the caller a submit the rules rejected.
 */
export async function pickForm(options: PickFormOptions): Promise<FormChange | null> {
  const out = options.out ?? process.stdout;
  const input = options.input ?? process.stdin;
  if (input.isTTY && out.isTTY && typeof input.setRawMode === 'function') {
    return pickFormInteractive(options, input, out);
  }
  return pickFormReadline(options);
}

async function pickFormInteractive(
  options: PickFormOptions,
  input: NodeJS.ReadStream,
  out: NodeJS.WriteStream,
): Promise<FormChange | null> {
  return new Promise<FormChange | null>((resolve) => {
    let state = initialState(options.spec, options.values ?? {}, options.issues ?? []);
    let printed = 0;

    const draw = (): void => {
      const lines = renderForm(state);
      if (printed > 0) out.write(`\x1b[${printed}A`);
      out.write('\x1b[0J');
      out.write(`${lines.join('\n')}\n`);
      printed = lines.length;
    };

    const cleanup = (change: FormChange | null): void => {
      if (escTimer) clearTimeout(escTimer);
      input.off('data', onData);
      if (input.setRawMode) input.setRawMode(false);
      input.pause();
      out.write('\x1b[?25h');
      resolve(change);
    };

    /** One complete key chunk. */
    const dispatch = (raw: string): void => {
      const key = classifyFormKey(raw);
      const result = applyFormKey(state, key, raw);
      state = result.state;
      if (result.action) {
        if (result.action.type === 'cancel') {
          cleanup(null);
          return;
        }
        const errors = options.validate?.(result.action.values) ?? [];
        if (errors.length > 0) {
          state = { ...state, issues: errors };
          draw();
          return;
        }
        cleanup({ formId: '', revision: 0, action: result.action.action, values: result.action.values });
        return;
      }
      draw();
    };

    // A lone ESC may be the first byte of an arrow-key sequence (`\x1b[A`), which
    // a terminal can deliver in pieces. Wait briefly before treating it as
    // cancel; if more bytes arrive, re-join them.
    let escBuffer: string | null = null;
    let escTimer: ReturnType<typeof setTimeout> | null = null;
    const onData = (data: Buffer | string): void => {
      let raw = typeof data === 'string' ? data : data.toString('utf8');
      if (escBuffer !== null) {
        raw = escBuffer + raw;
        escBuffer = null;
        if (escTimer) {
          clearTimeout(escTimer);
          escTimer = null;
        }
      }
      if (raw === '\x1b') {
        escBuffer = raw;
        escTimer = setTimeout(() => {
          escBuffer = null;
          escTimer = null;
          dispatch('\x1b');
        }, 25);
        return;
      }
      dispatch(raw);
    };

    input.setRawMode?.(true);
    input.resume();
    out.write('\x1b[?25l');
    input.on('data', onData);
    draw();
  });
}

/** Line fallback: one prompt per field, then submit. */
async function pickFormReadline(options: PickFormOptions): Promise<FormChange | null> {
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
  const ask = (question: string): Promise<string> => new Promise((resolve) => rl.question(question, resolve));
  const state = initialState(options.spec, options.values ?? {}, options.issues ?? []);
  try {
    for (const row of rows(options.spec)) {
      if (row.kind !== 'field') continue;
      const current = display(valueAtPath(state.values, row.path));
      const answer = (await ask(`${row.title}${row.required ? ' *' : ''}${current ? ` (${current})` : ''}: `)).trim();
      if (answer !== '') commit(state, row.path, coerce(row.type, answer));
    }
    return { formId: '', revision: 0, action: 'submit', values: state.values };
  } finally {
    rl.close();
  }
}
