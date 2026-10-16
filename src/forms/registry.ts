/**
 * The form registry: the host-owned state a form lives in.
 *
 * One registry per root session. It owns the spec, the values, a monotonic
 * revision, and the waiters. `submit`/`close` (a *UI action*) produce a
 * `FormChange`; `update`/`setIssues` (an *agent edit*) bump the revision for
 * rendering but never wake a waiter — otherwise the agent's own edit would
 * resolve its own `await_form`. See `docs/DML_FORMS.md` §7.
 */

import {
  applyDefaults,
  isPlainObject,
  type FormChange,
  type FormIssue,
  type FormSpec,
  type Json,
  type JsonObject,
} from './spec.js';

export type FormOp = 'open' | 'update' | 'close';

export interface FormEvent {
  type: 'form';
  op: FormOp;
  formId: string;
  revision: number;
  spec?: FormSpec;
  patch?: JsonObject;
  values?: JsonObject;
  issues?: FormIssue[];
  state?: 'open' | 'submitted' | 'cancelled';
  reason?: string;
  source?: { name?: string; pid?: string };
}

export interface FormRecord {
  formId: string;
  spec: FormSpec;
  values: JsonObject;
  issues: FormIssue[];
  revision: number;
  state: 'open' | 'submitted' | 'cancelled';
  changed?: string[];
  source?: { name?: string; pid?: string };
}

interface Waiter {
  since: number;
  resolve: (change: FormChange) => void;
  timer?: ReturnType<typeof setTimeout>;
  onAbort?: () => void;
}

export interface OpenOptions {
  formId?: string;
  values?: JsonObject;
  source?: { name?: string; pid?: string };
}

export interface WaitOptions {
  timeoutMs?: number;
  signal?: AbortSignal;
}

export interface FormRegistryOptions {
  onEvent?: (event: FormEvent) => void;
}

export class FormRegistry {
  private readonly forms = new Map<string, FormRecord>();
  private readonly waiters = new Map<string, Waiter[]>();
  private readonly lastChange = new Map<string, FormChange>();
  private counter = 0;

  constructor(private readonly options: FormRegistryOptions = {}) {}

  /** Open (or replace) a form. Returns its id and revision. */
  open(spec: FormSpec, options: OpenOptions = {}): { formId: string; revision: number; values: JsonObject } {
    const formId = options.formId ?? spec.id ?? `form_${++this.counter}`;
    const existing = this.forms.get(formId);
    const revision = (existing?.revision ?? 0) + 1;
    const values = applyDefaults(spec, options.values ?? existing?.values ?? {});
    const record: FormRecord = {
      formId,
      spec,
      values,
      issues: [],
      revision,
      state: 'open',
      source: options.source ?? existing?.source,
    };
    this.forms.set(formId, record);
    this.emit({ type: 'form', op: 'open', formId, revision, spec, values, issues: [], source: record.source });
    return { formId, revision, values };
  }

  /** Merge-patch a form (agent edit). Bumps the revision; does not wake waiters. */
  update(formId: string, patch: JsonObject): { revision: number; values: JsonObject } | undefined {
    const record = this.forms.get(formId);
    if (!record) return undefined;
    record.values = mergePatch(record.values, patch) as JsonObject;
    record.revision += 1;
    this.emit({ type: 'form', op: 'update', formId, revision: record.revision, patch, values: record.values, issues: record.issues });
    return { revision: record.revision, values: record.values };
  }

  /** Attach per-field messages (agent edit). An empty list clears them. */
  setIssues(formId: string, issues: FormIssue[]): { revision: number } | undefined {
    const record = this.forms.get(formId);
    if (!record) return undefined;
    record.issues = issues;
    record.revision += 1;
    this.emit({
      type: 'form',
      op: 'update',
      formId,
      revision: record.revision,
      values: record.values,
      issues,
    });
    return { revision: record.revision };
  }

  /**
   * A UI action: record it, wake the waiters, and return the change. This is the
   * only path that produces a change for `wait`.
   */
  submit(formId: string, action: string, values: JsonObject = {}, errors: FormIssue[] = []): FormChange | undefined {
    const record = this.forms.get(formId);
    if (!record) return undefined;
    const previous = record.values;
    record.values = { ...values };
    record.revision += 1;
    const changed = diffPaths(previous, record.values);
    const change: FormChange = {
      formId,
      revision: record.revision,
      action,
      values: record.values,
      changed,
      errors,
    };
    record.changed = changed;
    this.lastChange.set(formId, change);
    this.wake(formId, change);
    return change;
  }

  /** Close a form. Pending waiters resolve with `closed`. */
  close(formId: string, state: 'submitted' | 'cancelled' = 'submitted', reason?: string): void {
    const record = this.forms.get(formId);
    if (!record) return;
    record.state = state;
    record.revision += 1;
    const change: FormChange = { formId, revision: record.revision, action: 'closed', values: record.values };
    this.lastChange.set(formId, change);
    this.emit({ type: 'form', op: 'close', formId, revision: record.revision, state, values: record.values, reason });
    this.wake(formId, change);
  }

  /** Resolve every pending waiter (turn abort). */
  cancelPending(action = 'cancelled'): void {
    for (const [formId, list] of this.waiters) {
      const record = this.forms.get(formId);
      const change: FormChange = {
        formId,
        revision: record?.revision ?? 0,
        action,
        values: record?.values ?? {},
      };
      for (const waiter of list.splice(0)) this.settle(waiter, change);
    }
  }

  /**
   * Wait for a UI change after `since`. Resolves immediately when a later change
   * already exists (so a submit that raced the wait is not lost).
   */
  wait(formId: string, since: number, options: WaitOptions = {}): Promise<FormChange> {
    const record = this.forms.get(formId);
    if (!record) {
      return Promise.resolve({ formId, revision: 0, action: 'unavailable', values: {} });
    }
    const existing = this.lastChange.get(formId);
    if (existing && existing.revision > since) return Promise.resolve(existing);
    if (record.state !== 'open') {
      return Promise.resolve({ formId, revision: record.revision, action: 'closed', values: record.values });
    }

    return new Promise<FormChange>((resolve) => {
      const waiter: Waiter = { since, resolve };
      if (options.timeoutMs !== undefined && options.timeoutMs > 0) {
        waiter.timer = setTimeout(() => {
          this.removeWaiter(formId, waiter);
          resolve({ formId, revision: record.revision, action: 'timeout', values: record.values });
        }, options.timeoutMs);
        waiter.timer.unref?.();
      }
      if (options.signal) {
        const onAbort = (): void => {
          this.removeWaiter(formId, waiter);
          resolve({ formId, revision: record.revision, action: 'cancelled', values: record.values });
        };
        if (options.signal.aborted) {
          onAbort();
          return;
        }
        waiter.onAbort = onAbort;
        options.signal.addEventListener('abort', onAbort, { once: true });
      }
      const list = this.waiters.get(formId) ?? [];
      list.push(waiter);
      this.waiters.set(formId, list);
    });
  }

  get(formId: string): FormRecord | undefined {
    return this.forms.get(formId);
  }

  list(): FormRecord[] {
    return [...this.forms.values()];
  }

  // -------------------------------------------------------------------------

  private wake(formId: string, change: FormChange): void {
    const list = this.waiters.get(formId);
    if (!list) return;
    for (const waiter of list.splice(0)) this.settle(waiter, change);
  }

  private settle(waiter: Waiter, change: FormChange): void {
    if (waiter.timer) clearTimeout(waiter.timer);
    if (waiter.onAbort) waiter.onAbort();
    waiter.resolve(change);
  }

  private removeWaiter(formId: string, waiter: Waiter): void {
    const list = this.waiters.get(formId);
    if (!list) return;
    const index = list.indexOf(waiter);
    if (index >= 0) list.splice(index, 1);
  }

  private emit(event: FormEvent): void {
    this.options.onEvent?.(event);
  }
}

// ---------------------------------------------------------------------------
// Merge patch (RFC 7386) and a shallow path diff
// ---------------------------------------------------------------------------

export function mergePatch(target: Json, patch: Json): Json {
  if (!isPlainObject(patch)) return patch;
  const base: JsonObject = isPlainObject(target) ? { ...(target as JsonObject) } : {};
  for (const [key, value] of Object.entries(patch)) {
    if (value === null) delete base[key];
    else base[key] = mergePatch(base[key], value);
  }
  return base;
}

/** Dotted paths that differ between two value trees. */
export function diffPaths(before: JsonObject, after: JsonObject, prefix = ''): string[] {
  const out: string[] = [];
  const keys = [...new Set([...Object.keys(before), ...Object.keys(after)])].sort();
  for (const key of keys) {
    const path = prefix ? `${prefix}.${key}` : key;
    const a = before[key];
    const b = after[key];
    if (isPlainObject(a) && isPlainObject(b)) out.push(...diffPaths(a as JsonObject, b as JsonObject, path));
    else if (JSON.stringify(a) !== JSON.stringify(b)) out.push(path);
  }
  return out;
}
