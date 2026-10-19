/**
 * Persistent session / trace / harness history.
 *
 * See `docs/DML_REPL_SESSION_HISTORY.md`. Storage lives under a
 * workspace-relative directory (default `.deepclause/repl/`):
 *
 *   history.dml                       append-only harness_* facts, consulted at
 *                                     session start and queryable from DML
 *   harness/HEAD.json                 current version pointer
 *   harness/changes.jsonl             one line per harness change
 *   harness/versions/NNNN_<ts>.json   full module snapshot per version
 *   sessions/<id>/session.json        session metadata
 *   sessions/<id>/turns.jsonl         one summary per turn
 *   sessions/<id>/events.jsonl        runtime event trace
 *
 * The recorder is best-effort: callers swallow its errors so a turn never fails
 * because history could not be written.
 */

import { createHash } from 'crypto';
import * as fs from 'fs/promises';
import * as path from 'path';

export type HistoryTrace = 'off' | 'summary' | 'full';

export interface HistoryOptions {
  /** Workspace-relative root (default `.deepclause/repl`). */
  dir?: string;
  /** `off`, `summary` (event metadata), or `full` (include streamed text). */
  trace?: HistoryTrace;
  maxEventBytes?: number;
  maxSessions?: number;
  maxVersions?: number;
  includeInExport?: boolean;
  /** Regex sources applied to recorded text before writing. */
  redact?: string[];
}

export interface HistoryEventContext {
  turn: number;
  frame?: string;
}

export interface HistoryTurnRecord {
  turn: number;
  mode: 'goal' | 'prompt';
  request: string;
  outcome: 'committed' | 'answered' | 'failed' | 'aborted' | 'query' | 'running';
  commitVersion?: number;
  durationMs: number;
  modelCalls: number;
  error?: string;
  usage?: { inputTokens?: number; outputTokens?: number };
}

/** One persisted runtime event (`sessions/<id>/events.jsonl`). */
export interface HistoryEventRecord {
  ts?: string;
  sessionId?: string;
  turn?: number;
  frame?: string;
  type: string;
  [key: string]: unknown;
}

export interface HistoryChangedPredicate {
  predicate: string;
  action: 'added' | 'removed' | 'replaced';
}

export interface HistoryChangeRecord {
  kind: 'commit' | 'load' | 'reset';
  sessionId: string;
  turn: number;
  changed: HistoryChangedPredicate[];
  scripts: Array<{ path: string; sha256: string }>;
  module: string;
  manifest: string;
}

/** A linked script file and its content hash. */
export interface HistoryScriptRef {
  path: string;
  sha256: string;
}

/**
 * Durable snapshot of a session's harness, written beside the session so the
 * harness can be restored by `--session` even after the process exits or the
 * scratch bundle is gone. See `docs/DML_REPL_SESSION_HISTORY.md`.
 */
export interface SessionHarnessSnapshot {
  sessionId: string;
  /** Last recorded harness version for the session (0 = none). */
  version: number;
  /** sha256 of `module` + `scripts` (see `harnessHash`). */
  hash: string;
  /** True when `module` still matches the recorded version's snapshot. */
  clean: boolean;
  savedAt: string;
  scripts: HistoryScriptRef[];
  /** Serialized conversation memory, when captured. */
  memory?: {
    harness: string;
    meta: string;
    /** Authoring-agent conversation (TS agent mode), for resume. */
    agent?: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>;
  };
  module: string;
}

/** Metadata about a past REPL session, for `--session` resolution. */
export interface SessionSummary {
  id: string;
  startedAt?: string;
  endedAt?: string | null;
  resumedAt?: string[];
  lastTurn?: number;
}

const DEFAULT_DIR = '.deepclause/repl';

/** Stable hash of a harness module plus its linked scripts. */
export function harnessHash(
  module: string,
  scripts: Array<{ path: string; sha256: string }>,
): string {
  return createHash('sha256')
    .update(module)
    .update('\n')
    .update(JSON.stringify(scripts))
    .digest('hex');
}

/**
 * Short, stable content hash of a harness module alone. This is the identity of
 * a process snapshot: two processes with the same `moduleVersion` run the same
 * code, and it changes whenever the module does.
 */
export function moduleVersion(module: string): string {
  return createHash('sha256').update(module).digest('hex').slice(0, 12);
}

function nowIso(): string {
  return new Date().toISOString();
}

/** Single-quoted Prolog atom. */
export function prologAtom(value: string): string {
  return `'${value.replace(/\\/g, '\\\\').replace(/'/g, "''")}'`;
}

export class HistoryStore {
  readonly rootRel: string;
  readonly sessionDirRel: string;
  private readonly rootAbs: string;
  private readonly sessionDirAbs: string;
  private readonly trace: HistoryTrace;
  private readonly maxEventBytes: number;
  private readonly maxSessions: number;
  private readonly maxVersions: number;
  private readonly redactions: RegExp[];
  private eventBuffer: string[] = [];
  private flushTimer?: ReturnType<typeof setTimeout>;
  /** Current HEAD version (0 = empty harness) and the highest ever issued. */
  private headVersion = 0;
  private maxVersion = 0;
  /** Hash recorded for the current HEAD (for session-snapshot consistency). */
  private headHash?: string;

  constructor(
    workspacePath: string,
    private readonly sessionId: string,
    options: HistoryOptions = {},
    /** Harness this session edits (a project can hold several). */
    readonly harnessName: string = 'main',
  ) {
    this.rootRel = (options.dir ?? DEFAULT_DIR).replace(/\\/g, '/').replace(/\/+$/, '');
    this.rootAbs = path.resolve(workspacePath, this.rootRel);
    this.sessionDirRel = `${this.rootRel}/sessions/${sessionId}`;
    this.sessionDirAbs = path.resolve(workspacePath, this.sessionDirRel);
    this.trace = options.trace ?? 'summary';
    this.maxEventBytes = options.maxEventBytes ?? 8192;
    this.maxSessions = options.maxSessions ?? 50;
    this.maxVersions = options.maxVersions ?? 200;
    this.redactions = (options.redact ?? []).map((source) => new RegExp(source, 'g'));
  }

  get currentVersion(): number {
    return this.headVersion;
  }

  get currentHash(): string | undefined {
    return this.headHash;
  }

  get tracePath(): string {
    return `${this.sessionDirRel}/events.jsonl`;
  }

  private get historyFile(): string {
    return path.join(this.rootAbs, 'history.dml');
  }

  /** Per-harness store: `.deepclause/repl/harnesses/<name>/`. */
  private get harnessDir(): string {
    return path.join(this.rootAbs, 'harnesses', this.harnessName);
  }

  /** The single pre-harnesses store, migrated to `harnesses/main` once. */
  private get legacyHarnessDir(): string {
    return path.join(this.rootAbs, 'harness');
  }

  /** The current content (source of truth; portable as a `.dml`). */
  private get harnessFile(): string {
    return path.join(this.harnessDir, 'harness.dml');
  }

  /** The module the harness started from (version 0 / rollback floor). */
  private get baselineFile(): string {
    return path.join(this.harnessDir, 'baseline.dml');
  }

  private get headFile(): string {
    return path.join(this.harnessDir, 'HEAD.json');
  }

  private get changesFile(): string {
    return path.join(this.harnessDir, 'changes.jsonl');
  }

  /** Whether the harness has any committed versions (HEAD > baseline). */
  get hasCommits(): boolean {
    return this.headVersion > 0;
  }

  /** Facts for this harness only (`harness_change`/`changed`/`head`/`rollback`). */
  private get harnessFactsFile(): string {
    return path.join(this.harnessDir, 'history.dml');
  }

  /** Create directories, read the current version, and prune old entries. */
  async init(): Promise<void> {
    await fs.mkdir(this.sessionDirAbs, { recursive: true });
    await this.migrateLegacyHarness();
    await fs.mkdir(path.join(this.harnessDir, 'versions'), { recursive: true });
    try {
      const head = JSON.parse(await fs.readFile(this.headFile, 'utf8')) as {
        version?: number;
        maxVersion?: number;
        hash?: string;
      };
      if (typeof head.version === 'number') this.headVersion = head.version;
      if (typeof head.hash === 'string') this.headHash = head.hash;
      this.maxVersion = typeof head.maxVersion === 'number' ? head.maxVersion : this.headVersion;
    } catch {
      // no HEAD yet
    }
    // A harness always has a baseline (empty for a fresh one) and a current file.
    await this.ensureHarnessFiles();
    await this.prune();
  }

  /** Move the legacy single `harness/` store into `harnesses/main/` (once). */
  private async migrateLegacyHarness(): Promise<void> {
    if (this.harnessName !== 'main') return;
    try {
      await fs.access(path.join(this.legacyHarnessDir, 'HEAD.json'));
    } catch {
      return; // nothing to migrate
    }
    try {
      await fs.access(this.harnessDir);
      return; // the per-harness dir already exists
    } catch {
      // proceed
    }
    await fs.mkdir(path.dirname(this.harnessDir), { recursive: true });
    await fs.rename(this.legacyHarnessDir, this.harnessDir);
  }

  /** Ensure `baseline.dml` and `harness.dml` exist (materializing from HEAD). */
  private async ensureHarnessFiles(): Promise<void> {
    let baseline = '';
    try {
      baseline = await fs.readFile(this.baselineFile, 'utf8');
    } catch {
      await fs.writeFile(this.baselineFile, '', 'utf8');
    }
    try {
      const current = await fs.readFile(this.harnessFile, 'utf8');
      if (current.trim() || this.headVersion <= 0) return;
    } catch {
      // write below
    }
    const head = this.headVersion > 0 ? await this.readVersion(this.headVersion) : null;
    await fs.writeFile(this.harnessFile, head?.module ?? baseline, 'utf8');
  }

  /** The module at HEAD: the live `harness.dml`, or the HEAD version snapshot. */
  async readHeadModule(): Promise<{ version: number; module: string } | null> {
    let module = '';
    try {
      module = await fs.readFile(this.harnessFile, 'utf8');
    } catch {
      module = '';
    }
    if (module.trim()) return { version: this.headVersion, module };
    if (this.headVersion > 0) {
      const current = await this.readVersion(this.headVersion);
      if (current?.module.trim()) return { version: this.headVersion, module: current.module };
    }
    return null;
  }

  /** The module this harness started from (version 0). */
  async readBaseline(): Promise<string> {
    try {
      return await fs.readFile(this.baselineFile, 'utf8');
    } catch {
      return '';
    }
  }

  /**
   * Establish the baseline of a fresh harness from its first content (e.g. a
   * harness file loaded with `-l`). No-op once the harness has a baseline or
   * any committed version.
   */
  async setInitialBaseline(module: string): Promise<void> {
    if (this.headVersion > 0) return;
    if ((await this.readBaseline()).trim()) return;
    await fs.mkdir(this.harnessDir, { recursive: true });
    await fs.writeFile(this.baselineFile, module, 'utf8');
    await fs.writeFile(this.harnessFile, module, 'utf8');
  }

  /** The persisted facts, to be loaded into the module at session start. */
  async readFacts(): Promise<string> {
    const read = async (file: string): Promise<string> => {
      try {
        return await fs.readFile(file, 'utf8');
      } catch {
        return '';
      }
    };
    const [workspace, harness] = await Promise.all([
      read(this.historyFile),
      read(this.harnessFactsFile),
    ]);
    return `${workspace}${harness}`;
  }

  /** The turns recorded for a session, oldest first (used for input history). */
  async readTurns(sessionId: string = this.sessionId): Promise<HistoryTurnRecord[]> {
    const file = path.join(this.sessionsDir(), sessionId, 'turns.jsonl');
    let text: string;
    try {
      text = await fs.readFile(file, 'utf8');
    } catch {
      return [];
    }
    const byTurn = new Map<number, HistoryTurnRecord>();
    const untracked: HistoryTurnRecord[] = [];
    for (const line of text.split('\n')) {
      if (!line.trim()) continue;
      try {
        const parsed = JSON.parse(line) as HistoryTurnRecord;
        if (typeof parsed?.request !== 'string') continue;
        if (typeof parsed.turn === 'number') byTurn.set(parsed.turn, parsed);
        else untracked.push(parsed);
      } catch {
        // skip malformed lines
      }
    }
    return [...byTurn.values()].sort((a, b) => a.turn - b.turn).concat(untracked);
  }

  async startSession(
    info: { model: string; backend: string },
    options: { resume?: boolean } = {},
  ): Promise<string[]> {
    const sessionFile = path.join(this.sessionDirAbs, 'session.json');
    if (options.resume) {
      const resumedAt = nowIso();
      const facts = [
        `harness_session_resume(${prologAtom(this.sessionId)}, ${prologAtom(resumedAt)}).`,
      ];
      await this.appendFacts(facts);
      let existing: Record<string, unknown> = {};
      try {
        existing = JSON.parse(await fs.readFile(sessionFile, 'utf8')) as Record<string, unknown>;
      } catch {
        // first resume of a session that never wrote session.json
      }
      const previous = Array.isArray(existing.resumedAt) ? (existing.resumedAt as string[]) : [];
      await this.writeJson(sessionFile, {
        ...existing,
        sessionId: this.sessionId,
        model: info.model,
        backend: info.backend,
        traceDir: this.sessionDirRel,
        endedAt: null,
        exit: null,
        resumedAt: [...previous, resumedAt],
      });
      return facts;
    }

    const startedAt = nowIso();
    const facts = [
      `harness_session(${prologAtom(this.sessionId)}, ${prologAtom(startedAt)}, `
        + `${prologAtom(info.model)}, ${prologAtom(info.backend)}, ${prologAtom(this.sessionDirRel)}).`,
    ];
    await this.appendFacts(facts);
    await this.writeJson(sessionFile, {
      sessionId: this.sessionId,
      startedAt,
      endedAt: null,
      exit: null,
      model: info.model,
      backend: info.backend,
      traceDir: this.sessionDirRel,
    });
    return facts;
  }

  async endSession(reason: string): Promise<string[]> {
    await this.flushEvents();
    const endedAt = nowIso();
    const facts = [`harness_session_end(${prologAtom(this.sessionId)}, ${prologAtom(endedAt)}).`];
    await this.appendFacts(facts);
    const sessionFile = path.join(this.sessionDirAbs, 'session.json');
    try {
      const existing = JSON.parse(await fs.readFile(sessionFile, 'utf8')) as Record<string, unknown>;
      await this.writeJson(sessionFile, { ...existing, endedAt, exit: reason });
    } catch {
      // best effort
    }
    return facts;
  }

  async recordTurn(record: HistoryTurnRecord): Promise<string[]> {
    const ts = nowIso();
    const facts = [
      `harness_turn(${prologAtom(this.sessionId)}, ${record.turn}, ${prologAtom(ts)}, `
        + `${prologAtom(record.mode)}, ${prologAtom(record.outcome)}).`,
      `harness_trace(${prologAtom(this.sessionId)}, ${record.turn}, ${prologAtom(this.tracePath)}).`,
    ];
    await this.appendFacts(facts);
    await this.appendJsonl(path.join(this.sessionDirAbs, 'turns.jsonl'), { ts, ...record });
    return facts;
  }

  /**
   * Record a harness version. Returns the facts to assert and the new version.
   */
  async recordChange(record: HistoryChangeRecord): Promise<{ facts: string[]; version: number }> {
    const ts = nowIso();
    const hash = harnessHash(record.module, record.scripts);
    // Committing content identical to HEAD is a no-op: do not burn a version.
    if (hash === this.headHash && this.headVersion > 0) {
      return { facts: [], version: this.headVersion };
    }
    const version = this.maxVersion + 1;
    this.maxVersion = version;
    const parentVersion = this.headVersion;
    const facts = [
      `harness_change(${version}, ${prologAtom(ts)}, ${prologAtom(record.kind)}, `
        + `${prologAtom(record.sessionId)}, ${record.turn}).`,
      ...record.changed.map((change) => {
        const [name, arity] = splitPredicate(change.predicate);
        return `harness_changed(${version}, ${prologAtom(name)}, ${arity}, ${prologAtom(change.action)}).`;
      }),
      `harness_head(${version}, ${prologAtom(ts)}, ${prologAtom(record.sessionId)}, ${record.turn}).`,
    ];
    await this.appendHarnessFacts(facts);
    await this.appendJsonl(this.changesFile, {
      version,
      ts,
      sessionId: record.sessionId,
      turn: record.turn,
      kind: record.kind,
      hash,
      changed: record.changed,
      scripts: record.scripts,
    });
    await this.writeJson(
      path.join(this.harnessDir, 'versions', `${String(version).padStart(4, '0')}_${fileStamp(ts)}.json`),
      {
        version,
        ts,
        parentVersion: parentVersion > 0 ? parentVersion : null,
        kind: record.kind,
        sessionId: record.sessionId,
        turn: record.turn,
        hash,
        changed: record.changed,
        scripts: record.scripts,
        module: record.module,
        manifest: record.manifest,
      },
    );
    await this.writeJson(this.headFile, {
      version,
      maxVersion: this.maxVersion,
      ts,
      sessionId: record.sessionId,
      turn: record.turn,
      hash,
    });
    // Keep the live harness file in step with HEAD (it is the source of truth).
    await fs.writeFile(this.harnessFile, record.module, 'utf8');
    this.headVersion = version;
    this.headHash = hash;
    return { facts, version };
  }

  /**
   * Plan a rollback to the parent of HEAD. Returns null when there is nothing
   * to undo, otherwise the module source to restore (floored at the baseline).
   */
  async prepareRollback(): Promise<{ from: number; to: number; module: string } | null> {
    if (this.headVersion <= 0) return null;
    const current = await this.readVersion(this.headVersion);
    const to = current?.parentVersion ?? Math.max(this.headVersion - 1, 0);
    const module = to > 0 ? (await this.readVersion(to))?.module ?? '' : await this.readBaseline();
    return { from: this.headVersion, to, module };
  }

  /** Persist a rollback (facts + HEAD + change log + harness file). */
  async recordRollback(
    from: number,
    to: number,
    turn: number,
    sessionId: string,
    module?: string,
  ): Promise<string[]> {
    const ts = nowIso();
    const facts = [
      `harness_rollback(${from}, ${to}, ${prologAtom(ts)}, ${prologAtom(sessionId)}, ${turn}).`,
      `harness_head(${to}, ${prologAtom(ts)}, ${prologAtom(sessionId)}, ${turn}).`,
    ];
    await this.appendHarnessFacts(facts);
    await this.appendJsonl(this.changesFile, { ts, sessionId, turn, kind: 'rollback', from, to });
    await this.writeJson(this.headFile, {
      version: to,
      maxVersion: this.maxVersion,
      ts,
      sessionId,
      turn,
      rollback: true,
    });
    if (module !== undefined) {
      await fs.writeFile(this.harnessFile, module, 'utf8');
    }
    this.headVersion = to;
    // The hash no longer matches HEAD's content until the next commit.
    this.headHash = undefined;
    return facts;
  }

  private async readVersion(
    version: number,
  ): Promise<{ module: string; parentVersion: number | null } | null> {
    const dir = path.join(this.harnessDir, 'versions');
    try {
      const prefix = `${String(version).padStart(4, '0')}_`;
      const name = (await fs.readdir(dir)).find((entry) => entry.startsWith(prefix));
      if (!name) return null;
      const data = JSON.parse(await fs.readFile(path.join(dir, name), 'utf8')) as {
        module?: string;
        parentVersion?: number | null;
      };
      return { module: data.module ?? '', parentVersion: data.parentVersion ?? null };
    } catch {
      return null;
    }
  }

  // ---- Session lookup & durable harness snapshots (resume) ----

  private sessionsDir(): string {
    return path.join(this.rootAbs, 'sessions');
  }

  private sessionHarnessFile(sessionId: string): string {
    return path.join(this.sessionsDir(), sessionId, 'harness.json');
  }

  /** True when a session directory exists for `sessionId`. */
  async sessionExists(sessionId: string): Promise<boolean> {
    try {
      return (await fs.stat(path.join(this.sessionsDir(), sessionId))).isDirectory();
    } catch {
      return false;
    }
  }

  /** All recorded sessions, oldest first. */
  async sessionSummaries(): Promise<SessionSummary[]> {
    let ids: string[];
    try {
      ids = await fs.readdir(this.sessionsDir());
    } catch {
      return [];
    }
    const summaries: SessionSummary[] = [];
    for (const id of ids) {
      let meta: Record<string, unknown> = {};
      try {
        meta = JSON.parse(
          await fs.readFile(path.join(this.sessionsDir(), id, 'session.json'), 'utf8'),
        ) as Record<string, unknown>;
      } catch {
        // session without metadata (e.g. history disabled mid-way)
      }
      summaries.push({
        id,
        startedAt: typeof meta.startedAt === 'string' ? meta.startedAt : undefined,
        endedAt: typeof meta.endedAt === 'string' ? meta.endedAt : null,
        resumedAt: Array.isArray(meta.resumedAt) ? (meta.resumedAt as string[]) : undefined,
        lastTurn: await this.lastTurn(id),
      });
    }
    summaries.sort((a, b) => (a.startedAt ?? '').localeCompare(b.startedAt ?? ''));
    return summaries;
  }

  /** The most recently started session, or null when there is none. */
  async latestSessionId(): Promise<string | null> {
    const summaries = await this.sessionSummaries();
    return summaries.length > 0 ? summaries[summaries.length - 1].id : null;
  }

  /** The highest turn number recorded for a session (0 when none). */
  async lastTurn(sessionId: string = this.sessionId): Promise<number> {
    const file = path.join(this.sessionsDir(), sessionId, 'turns.jsonl');
    let text: string;
    try {
      text = await fs.readFile(file, 'utf8');
    } catch {
      return 0;
    }
    const lines = text.split('\n').filter((line) => line.trim());
    for (let index = lines.length - 1; index >= 0; index -= 1) {
      try {
        const record = JSON.parse(lines[index]) as { turn?: number };
        if (typeof record.turn === 'number') return record.turn;
      } catch {
        // skip a malformed line
      }
    }
    return 0;
  }

  /**
   * Persisted runtime events for a session, oldest first, capped at the last
   * `limit`. Used to render a resumed session on load.
   */
  async readEvents(sessionId: string = this.sessionId, limit = 400): Promise<HistoryEventRecord[]> {
    let text: string;
    try {
      text = await fs.readFile(path.join(this.sessionsDir(), sessionId, 'events.jsonl'), 'utf8');
    } catch {
      return [];
    }
    const records: HistoryEventRecord[] = [];
    for (const line of text.split('\n')) {
      if (!line.trim()) continue;
      try {
        const parsed = JSON.parse(line) as HistoryEventRecord;
        if (parsed && typeof parsed.type === 'string') records.push(parsed);
      } catch {
        // skip a malformed line
      }
    }
    return limit >= records.length ? records : records.slice(records.length - limit);
  }

  /** Persist the durable harness snapshot beside a session (best effort). */
  async writeSessionHarness(
    snapshot: SessionHarnessSnapshot,
    sessionId: string = this.sessionId,
  ): Promise<void> {
    await this.writeJson(this.sessionHarnessFile(sessionId), snapshot);
  }

  /** Read a session's durable harness snapshot, or null when absent. */
  async readSessionHarness(
    sessionId: string = this.sessionId,
  ): Promise<SessionHarnessSnapshot | null> {
    try {
      const data = JSON.parse(
        await fs.readFile(this.sessionHarnessFile(sessionId), 'utf8'),
      ) as SessionHarnessSnapshot;
      return typeof data?.module === 'string' ? data : null;
    } catch {
      return null;
    }
  }

  /** Buffer one event line (flushed at turn end / dispose). */
  recordEvent(context: HistoryEventContext, event: { type: string } & Record<string, unknown>): void {
    if (this.trace === 'off') return;
    try {
      const line = this.serializeEvent(context, event);
      if (!line) return;
      this.eventBuffer.push(line);
      if (this.eventBuffer.length >= 200) {
        void this.flushEvents().catch(() => undefined);
      } else if (!this.flushTimer) {
        // Write through during the turn, not only at turn end: a closed tab or
        // a worker crash must not lose the whole transcript. Keep the batch
        // small enough that an abrupt stop loses at most a fraction of a second.
        this.flushTimer = setTimeout(() => {
          this.flushTimer = undefined;
          void this.flushEvents().catch(() => undefined);
        }, 250);
      }
    } catch {
      // never let tracing break the turn
    }
  }

  async flushEvents(): Promise<void> {
    if (this.flushTimer) {
      clearTimeout(this.flushTimer);
      this.flushTimer = undefined;
    }
    if (this.eventBuffer.length === 0) return;
    // Leading newline terminates a partial line left by an interrupted append,
    // so the next complete record is still parseable on resume.
    const payload = `\n${this.eventBuffer.join('\n')}\n`;
    this.eventBuffer = [];
    await this.appendRaw(path.join(this.sessionDirAbs, 'events.jsonl'), payload);
  }

  private serializeEvent(
    context: HistoryEventContext,
    event: { type: string } & Record<string, unknown>,
  ): string | null {
    const base: Record<string, unknown> = {
      ts: nowIso(),
      sessionId: this.sessionId,
      turn: context.turn,
      frame: context.frame,
      type: event.type,
    };
    const put = (key: string, value: unknown): void => {
      if (value !== undefined && value !== null) base[key] = this.scrub(value);
    };
    switch (event.type) {
      case 'stream':
        if (this.trace === 'full') put('content', event.content);
        break;
      case 'output':
      case 'answer':
      case 'error':
      case 'log':
        put('content', event.content);
        break;
      case 'tool_call':
        put('toolName', event.toolName);
        put('toolState', event.toolState);
        put('toolSummary', event.toolSummary);
        put('toolArgs', event.toolArgs);
        put('toolResult', event.toolResult);
        put('toolError', event.toolError);
        put('toolDurationMs', event.toolDurationMs);
        put('toolExitCode', event.toolExitCode);
        break;
      case 'solution':
        put('bindings', (event.solution as { bindings?: unknown } | undefined)?.bindings);
        break;
      case 'usage':
        put('usage', event.usage);
        break;
      case 'task_activity':
        put('taskState', event.taskState);
        put('taskDescription', event.taskDescription);
        break;
      default:
        break;
    }
    return JSON.stringify(base);
  }

  /** Redact and truncate strings recursively. */
  private scrub(value: unknown): unknown {
    if (typeof value === 'string') {
      let text = value;
      for (const pattern of this.redactions) text = text.replace(pattern, '[redacted]');
      if (Buffer.byteLength(text, 'utf8') > this.maxEventBytes) {
        text = `${text.slice(0, this.maxEventBytes)}…[truncated]`;
      }
      return text;
    }
    if (Array.isArray(value)) return value.map((entry) => this.scrub(entry));
    if (value && typeof value === 'object') {
      return Object.fromEntries(
        Object.entries(value as Record<string, unknown>).map(([key, entry]) => [key, this.scrub(entry)]),
      );
    }
    return value;
  }

  private async appendFacts(facts: string[]): Promise<void> {
    if (facts.length === 0) return;
    await this.appendRaw(this.historyFile, `${facts.join('\n')}\n`);
  }

  private async appendHarnessFacts(facts: string[]): Promise<void> {
    if (facts.length === 0) return;
    await this.appendRaw(this.harnessFactsFile, `${facts.join('\n')}\n`);
  }

  private async appendJsonl(file: string, record: Record<string, unknown>): Promise<void> {
    if (this.trace === 'off' && file.endsWith('events.jsonl')) return;
    await this.appendRaw(file, `\n${JSON.stringify(record)}\n`);
  }

  /**
   * Append a session-scoped service record (a declaration or lifecycle event).
   * See `docs/DML_REPL_PROCESSES_SPEC.md` §7.
   */
  async appendServiceRecord(record: Record<string, unknown>): Promise<void> {
    try {
      await this.appendRaw(path.join(this.sessionDirAbs, 'services.jsonl'), `${JSON.stringify(record)}\n`);
    } catch {
      // History is best-effort; a failed write must never fail a turn.
    }
  }

  /** Read the recorded service declarations and lifecycle events, in order. */
  async readServiceRecords(): Promise<Array<Record<string, unknown>>> {
    try {
      const text = await fs.readFile(path.join(this.sessionDirAbs, 'services.jsonl'), 'utf8');
      return text
        .split('\n')
        .filter((line) => line.trim().length > 0)
        .flatMap((line) => {
          try {
            return [JSON.parse(line) as Record<string, unknown>];
          } catch {
            return [];
          }
        });
    } catch {
      return [];
    }
  }

  /** The workspace-relative directory of this session's records. */
  get sessionDirectory(): string {
    return this.sessionDirRel;
  }

  private async appendRaw(file: string, text: string): Promise<void> {
    await fs.mkdir(path.dirname(file), { recursive: true });
    await fs.appendFile(file, text, 'utf8');
  }

  private async writeJson(file: string, value: unknown): Promise<void> {
    await fs.mkdir(path.dirname(file), { recursive: true });
    await fs.writeFile(file, `${JSON.stringify(value, null, 2)}\n`, 'utf8');
  }

  /** Keep only the newest sessions and versions. */
  private async prune(): Promise<void> {
    await this.pruneDir(path.join(this.rootAbs, 'sessions'), this.maxSessions);
    await this.pruneDir(path.join(this.harnessDir, 'versions'), this.maxVersions);
  }

  private async pruneDir(dir: string, keep: number): Promise<void> {
    try {
      const entries = await fs.readdir(dir);
      if (entries.length <= keep) return;
      const sorted = entries.sort();
      for (const entry of sorted.slice(0, entries.length - keep)) {
        await fs.rm(path.join(dir, entry), { recursive: true, force: true });
      }
    } catch {
      // best effort
    }
  }
}

function splitPredicate(predicate: string): [string, number] {
  const slash = predicate.lastIndexOf('/');
  if (slash === -1) return [predicate, 0];
  return [predicate.slice(0, slash), Number(predicate.slice(slash + 1)) || 0];
}

function fileStamp(iso: string): string {
  return iso.replace(/[:.]/g, '-');
}
