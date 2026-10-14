/**
 * DMLSession — a persistent REPL living module.
 *
 * Owns one Prolog session (the "living module"), one `DMLRunner` bound to it,
 * and the harness memory slot. Goal-mode queries create a `query_start/5`
 * frame, register it with the runner, and drive it with `runFrame`.
 */

import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';
import { strFromU8, strToU8, unzipSync, zipSync } from 'fflate';

import {
  loadProlog,
  mountWorkspace,
  takePrologMessages,
  type PrologMessage,
  type SWIPLModule,
} from '../prolog/loader.js';
import {
  describeShellExecutionBackend,
  type ExecutionBackendKind,
  type ExecutionEnvironment,
} from '../system/runtime/shell-manager.js';
import { getWorkspaceDocAssetPath } from '../system/assets/index.js';
import { DMLRunner, toJsValue, type Frame, type InternalRunOptions, type RunnerOptions } from '../runner.js';
import type { HarnessSnapshot, HarnessPredicate } from './tui/harness-view.js';
import {
  compactMemory,
  estimateMemoryTokens,
  shouldCompact,
  toCompactionEvent,
  type CompactionSettings,
} from './compaction.js';
import { HarnessAgent } from './agent.js';
import { BRIEF_VALUE_CHARS, createHarnessTools } from './harness-tools.js';
import type { BindingMode, HarnessCommitResult, HarnessExecuteResult } from './harness-tools.js';
import type {
  DMLEvent,
  FrameKind,
  ImageBackend,
  LLMBackend,
  MemoryMessage,
  ToolDefinition,
  ToolPolicy,
} from '../types.js';
import { buildParamsTerm, plainValue, quotePrologString } from './frames.js';
import {
  buildHarnessHeader,
  type HarnessExportOptions,
  type HarnessExportResult,
} from './export.js';
import {
  HistoryStore,
  harnessHash,
  type HistoryChangedPredicate,
  type HistoryOptions,
  type HistoryTurnRecord,
  type SessionHarnessSnapshot,
} from './history.js';
import {
  buildDiagramPrompt,
  extractMermaid,
  parseMermaidFlowchart,
  rasterizeSvg,
  renderFlowchartSvg,
  renderMermaidAscii,
  resolveMedia,
  validateMermaid,
  type DiagramMode,
} from '../visualize/index.js';
import { generateLlmReply } from '../prolog/bridge.js';
import type { Provider } from '../system/config/model-slots.js';
import type { ModelResolver } from './model-resolver.js';
import { formatModelCatalogEntry } from './model-resolver.js';

/** A generated diagram of one harness entry predicate. */
export interface DiagramResult {
  entry: string;
  mode: DiagramMode;
  mermaid: string;
  /** Unicode tree (always available, terminal-safe). */
  ascii: string;
  /** Self-contained SVG (input to the PNG rasterizer). */
  svg: string;
  width: number;
  height: number;
  warnings: string[];
}

export interface DMLSessionOptions {
  /** Optional explicit session id; generated when omitted. */
  sessionId?: string;
  workspacePath?: string;
  /** Session parameters exposed via param/2 and string interpolation. */
  params?: Record<string, unknown>;
  gasLimit?: number;
  /** Enable the DML execution trace (surfaced on the `finished` event). */
  trace?: boolean;
  /** Runner configuration (model/provider/temperature/backend). */
  runnerOptions: RunnerOptions;
  tools?: Map<string, ToolDefinition>;
  toolPolicy?: ToolPolicy | null;
  /** Legacy harness-only input handler (prefer onInput). */
  onInputRequired?: (prompt: string) => Promise<string>;
  /** Unified human-in-the-loop channel for meta ask_user and harness input/2. */
  onInput?: (prompt: string, options?: string[]) => Promise<string | undefined>;
  initialHarnessMemory?: MemoryMessage[];
  /** Maximum nested frame depth (execute -> object task). Default 3. */
  maxDepth?: number;
  /** Maximum model calls across all frames for one turn (default 100; 0 = unlimited). */
  maxModelCalls?: number;
  /** Host tool catalog text injected into the meta prompt (M5). */
  hostToolsCatalog?: string;
  /**
   * The effective shell backend for this session (`agentvm`, `bwrap`,
   * `sandbox-exec`, or `clean-room`). Injected into the Prolog session so the
   * harness `require_sandbox` directive can refuse to load when the backend
   * would run unsandboxed, and reported to the model in its environment block.
   * Callers that build a toolset should pass `toolset.environment.kind` so the
   * check and the toolset cannot disagree.
   */
  executionBackend?: ExecutionBackendKind;
  /** Human-in-the-loop channel for DML `input/2` / `exec(ask_user(...))`. */
  askUser?: (prompt: string, options?: string[]) => Promise<{ answer: string } | { cancelled: true }>;
  /**
   * Persistent session/trace/harness history. When set, the session records
   * its turns, traces, and harness versions and loads prior harness_* facts
   * into the module so DML can query them. See
   * `docs/DML_REPL_SESSION_HISTORY.md`.
   */
  history?: HistoryOptions;
  /** TypeScript compaction settings (ported from pi). Off by default. */
  compaction?: { settings: CompactionSettings; auto: boolean };
  /** Active model context window, used for compaction thresholds. */
  contextWindow?: number;
  /**
   * Resume an existing session (identified by `sessionId`): restore its saved
   * harness and conversation memory and continue its turn numbering. Requires
   * `history` and a `sessionId` that already exists.
   */
  resume?: boolean;
  /** Harness this session edits (a project can hold several). Default `main`. */
  harnessName?: string;
  /**
   * Load the harness's current content (`harness.dml`) on open. Default true.
   * Set false when the caller supplies content explicitly (e.g. `-l`).
   */
  autoLoadHarness?: boolean;
}

export interface DMLQueryOptions {
  tools?: Map<string, ToolDefinition>;
  toolPolicy?: ToolPolicy | null;
  onInputRequired?: (prompt: string) => Promise<string>;
  /** Per-run override of the unified input channel. */
  onInput?: (prompt: string, options?: string[]) => Promise<string | undefined>;
  signal?: AbortSignal;
  gasLimit?: number;
  /** Frame kind for the query (defaults to `object`). */
  frameKind?: FrameKind;
}

let sessionCounter = 0;

function nextSessionId(): string {
  sessionCounter += 1;
  return `repl_${Date.now().toString(36)}_${sessionCounter}`;
}

export class DMLSession {
  private swipl: SWIPLModule;
  private runner: DMLRunner;
  private sessionId: string;
  private workspacePath?: string;
  private gasLimit?: number;
  private model: string;
  private provider: string;
  private tools: Map<string, ToolDefinition>;
  private toolPolicy: ToolPolicy | null;
  private inputHandler?: (prompt: string, options?: string[]) => Promise<string | undefined>;
  private maxDepth: number;
  private maxModelCalls: number;
  private hostToolsCatalog?: string;
  /** TypeScript compaction (ported from pi). */
  private compactionSettings?: CompactionSettings;
  private compactionAuto = false;
  private contextWindow?: number;
  private llmBackend?: LLMBackend;
  private imageBackend?: ImageBackend;
  private modelResolver?: ModelResolver;
  private temperature = 0.7;
  private maxTokens = 65536;
  /** Persisted authoring-agent conversation (agent mode). */
  private agentMessages: MemoryMessage[] = [];
  private harnessAgent?: HarnessAgent;
  /** Open edit transaction (agent mode): predicate list at snapshot. */
  private transactionBefore: string[] | null = null;
  private transactionDirty = false;
  /** Whether the last agent turn committed an edit (for the history record). */
  private lastTurnCommitted = false;
  /** Version recorded by the last commit, when history is enabled. */
  private lastCommitVersion?: number;

  /** Current nested frame depth (used for the depth budget). */
  private frameDepth = 0;
  /** Model calls made in the current turn (shared across all frames). */
  private modelCallsThisTurn = 0;
  /** Current budget; grows by maxModelCalls when the user extends it. */
  private modelCallBudget: number;
  private dmlReference?: string;
  private dmlAuthoring?: string;
  private harnessExamples?: string;
  private harnessPatterns?: string;
  private prologSnippets?: string;
  private disposed = false;
  /** History recorder; present only when the caller enables it. */
  private history?: HistoryStore;
  private historyOptions?: HistoryOptions;
  private resume: boolean;
  /** Harness this session edits. */
  private readonly harnessName: string;
  /** Effective shell backend; see DMLSessionOptions.executionBackend. */
  readonly executionBackend: ExecutionBackendKind;
  /** Full resolution for the backend, for the agent's environment block. */
  readonly executionEnvironment: ExecutionEnvironment;
  private readonly autoLoadHarness: boolean;
  /** Monotonic per-session turn counter used by history records. */
  private turnCounter = 0;
  private historyWarned = false;

  private constructor(swipl: SWIPLModule, options: DMLSessionOptions) {
    this.swipl = swipl;
    this.sessionId = options.sessionId ?? nextSessionId();
    this.workspacePath = options.workspacePath;
    this.gasLimit = options.gasLimit;
    this.model = options.runnerOptions.model;
    this.provider = String(options.runnerOptions.provider);
    this.tools = options.tools ?? new Map();
    this.toolPolicy = options.toolPolicy ?? null;
    this.inputHandler = options.onInput
      ?? (options.askUser ? (prompt, choices) => options.askUser!(prompt, choices).then((r) => ('cancelled' in r ? undefined : r.answer)) : undefined)
      ?? options.onInputRequired
      ?? undefined;
    this.maxDepth = options.maxDepth ?? 3;
    this.maxModelCalls = options.maxModelCalls ?? 100;
    this.modelCallBudget = this.maxModelCalls;
    this.hostToolsCatalog = options.hostToolsCatalog;
    this.historyOptions = options.history;
    this.resume = options.resume ?? false;
    this.harnessName = options.harnessName ?? 'main';
    this.executionBackend = options.executionBackend ?? 'clean-room';
    this.executionEnvironment = describeShellExecutionBackend(
      this.executionBackend === 'agentvm',
      this.executionBackend === 'agentvm' ? {} : { wrapper: this.executionBackend },
    );
    this.autoLoadHarness = options.autoLoadHarness ?? true;
    this.compactionSettings = options.compaction?.settings;
    this.compactionAuto = options.compaction?.auto ?? false;
    this.contextWindow = options.contextWindow;
    this.llmBackend = options.runnerOptions.llmBackend;
    this.imageBackend = options.runnerOptions.imageBackend;
    this.modelResolver = options.runnerOptions.modelResolver;
    this.temperature = options.runnerOptions.temperature;
    this.maxTokens = options.runnerOptions.maxTokens;
    this.runner = new DMLRunner(swipl, {
      ...options.runnerOptions,
      environment: this.executionEnvironment,
      compactMessages: options.compaction
        ? (messages, lastInputTokens, contextWindow) => this.compactLoopMessages(messages, lastInputTokens, contextWindow)
        : undefined,
    });
    this.runner.bindSession(this.sessionId);
  }

  static async open(options: DMLSessionOptions): Promise<DMLSession> {
    const swipl = await loadProlog();
    const session = new DMLSession(swipl, options);

    // Mount the workspace so DML file I/O and `[file].` consult resolve under
    // /workspace exactly as they do for runDML.
    if (options.workspacePath) {
      mountWorkspace(swipl, options.workspacePath);
    }

    // Resuming: read the saved session snapshot up-front so the restored
    // conversation memory can seed create_session; the harness itself is
    // loaded once the session exists.
    let snapshot: SessionHarnessSnapshot | null = null;
    if (options.resume) {
      if (!options.history) {
        throw new Error('resume requires history to be enabled');
      }
      const store = new HistoryStore(
        options.workspacePath ?? process.cwd(),
        session.sessionId,
        options.history,
        session.harnessName,
      );
      snapshot = await store.readSessionHarness();
      if (!snapshot) {
        throw new Error(`session not found or has no saved harness: ${session.sessionId}`);
      }
    }

    const sessionParams: Record<string, unknown> = {
      workspace_path: options.workspacePath ?? './',
      trace: options.trace ?? false,
      // `sandbox-exec` is a subtraction expression to a Prolog parser, so the
      // hyphen becomes an underscore on the way in; the model still sees the
      // real name in its environment block.
      sandbox: session.executionBackend.replace(/-/g, '_'),
      ...options.params,
    };
    if (options.gasLimit !== undefined) {
      sessionParams.gas_limit = options.gasLimit;
    }

    const harnessMemory = snapshot?.memory?.harness?.trim()
      ? snapshot.memory.harness
      : `[${session.serializeMemory(options.initialHarnessMemory ?? [])}]`;
    const metaMemory = snapshot?.memory?.meta?.trim() ? snapshot.memory.meta : '[]';
    const result = session.prologQuery(
      `deepclause_mi:create_session('${session.sessionId}', ${buildParamsTerm(sessionParams)}, ${harnessMemory}, ${metaMemory})`
    );
    if (!result) {
      throw new Error('Failed to create REPL session');
    }
    await session.initHistory();
    if (snapshot) {
      if (snapshot.memory?.agent) {
        session.agentMessages = snapshot.memory.agent;
      }
      await session.restoreHarnessFromHistory(snapshot);
    } else if (session.autoLoadHarness) {
      // One harness per name: a fresh session starts from the harness's current
      // content file (`harness.dml`), not an empty module.
      const head = await session.history?.readHeadModule();
      if (head?.module.trim()) {
        const loaded = await session.loadCode(head.module);
        if (loaded.error) {
          session.warnHistory(new Error(`could not load harness '${session.harnessName}': ${loaded.error}`));
        }
      }
    }
    return session;
  }

  getSessionId(): string {
    return this.sessionId;
  }

  /**
   * The live harness, for the side view: what predicates exist right now, how big
   * each is, and what the open transaction has touched.
   *
   * Read from the session module rather than from the last commit, so a side view
   * shows the harness *being built* — including uncommitted edits.
   */
  harnessView(): HarnessSnapshot {
    const result = this.prologQuery(`deepclause_mi:repl_harness_view('${this.sessionId}', Rows)`);
    const raw = toJsValue(result?.Rows);
    const predicates: HarnessPredicate[] = Array.isArray(raw)
      ? raw
        .map((row) => String(row).split('|'))
        .filter((parts) => parts.length === 3)
        .map(([indicator, clauses, flags]) => {
          const [name, arityText] = indicator.split('/');
          return {
            indicator: indicator.trim(),
            name: String(name).trim(),
            arity: Number(arityText),
            clauses: Number(clauses),
            flags: String(flags).split(',').map((flag) => flag.trim()).filter(Boolean),
          };
        })
      : [];
    const versionResult = this.prologQuery(
      `( current_predicate(${this.sessionId}:harness_head/4)`
      + ` -> ${this.sessionId}:harness_head(V, _, _, _) ; V = none )`,
    );
    const version = plainValue(versionResult?.V);
    return {
      predicates: predicates.sort((a, b) => a.indicator.localeCompare(b.indicator)),
      version: version === 'none' ? undefined : Number(version),
      transactionOpen: this.transactionBefore !== null,
      touched: this.editTouched(),
      lastCommit: this.lastTurnCommitted,
    };
  }

  /**
   * Prolog output captured since the last drain.
   *
   * The step loop surfaces this while a goal runs; this accessor is for messages
   * produced by paths that are not the step loop — a load, a host query — so a
   * host can render them rather than have them vanish into a queue.
   */
  takePrologMessages(): PrologMessage[] {
    return takePrologMessages();
  }

  /** Parse and load DML source into the living module. */
  async loadCode(code: string): Promise<{ error?: string; warnings?: string[] }> {
    if (this.disposed) {
      return { error: 'Session has been disposed' };
    }
    const result = this.prologQuery(
      `deepclause_mi:load_code('${this.sessionId}', ${quoteString(code)}, Error)`
    );
    const error = plainValue(result?.Error);
    if (error === undefined || error === null) {
      return { error: 'Failed to load code' };
    }
    if (error !== 'none') return { error: String(error) };
    // Parsing is where the term reader speaks up (`\`-continuation, singleton
    // warnings), so load-time diagnostics belong with the load's warnings
    // instead of arriving mid-render later.
    const prologWarnings = takePrologMessages().map((message) => message.text);
    return { warnings: [...this.unhandledDirectives(), ...prologWarnings] };
  }

  /**
   * Directives the loader recorded because it does not handle them. They did
   * nothing, which is worth saying: `:- repl_delete(f/1).` silently discarded was
   * a predicate the author believed they had removed.
   */
  private unhandledDirectives(): string[] {
    const result = this.prologQuery(
      `deepclause_mi:repl_unhandled_directives('${this.sessionId}', Ds)`,
    );
    const raw = plainValue(result?.Ds);
    if (!Array.isArray(raw)) return [];
    return raw.map((directive) => `unrecognised directive ignored: ${formatTerm(directive)}`);
  }

  /**
   * Load a DML file (or a `.zip` bundle) into the living module.
   *
   * A `.zip` bundle extracts its script files into the workspace and loads
   * `harness.dml`; a single-file `.dml` export materializes any inlined
   * `embedded_file/2` facts back into the workspace.
   */
  async load(file: string): Promise<{ error?: string; warnings?: string[] }> {
    const before = this.predicateList();
    // On a fresh harness the loaded file *is* the baseline, not a version.
    const recordLoad = this.history?.hasCommits ?? false;
    const target = path.isAbsolute(file)
      ? file
      : path.resolve(this.workspacePath ?? process.cwd(), file);
    try {
      if (target.toLowerCase().endsWith('.zip')) {
        const result = await this.loadBundle(target);
        if (!result.error && recordLoad) await this.recordHistoryChange('load', this.turnCounter, before);
        return result;
      }
      const code = await fs.readFile(target, 'utf8');
      const result = await this.loadCode(code);
      if (!result.error) {
        this.prologQuery(`deepclause_mi:repl_materialize_embedded('${this.sessionId}')`);
        if (recordLoad) await this.recordHistoryChange('load', this.turnCounter, before);
      }
      return result;
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { error: `Failed to read ${file}: ${message}` };
    }
  }

  /**
   * Establish a fresh harness's baseline from its current module (called after
   * any `-l` files are loaded). A no-op once the harness has a baseline.
   */
  async finalizeHarnessBaseline(): Promise<void> {
    if (!this.history) return;
    await this.history.setInitialBaseline(this.moduleSource());
  }

  /** Workspace-relative files linked by `script_file/2` facts. */
  private scriptFiles(): string[] {
    const result = this.prologQuery(
      `deepclause_mi:repl_script_files('${this.sessionId}', Paths)`
    );
    const paths = plainValue(result?.Paths);
    if (!Array.isArray(paths)) return [];
    return paths.map((value) => String(plainValue(value) ?? '')).filter(Boolean);
  }

  // ---- History (docs/DML_REPL_SESSION_HISTORY.md) ----

  private async initHistory(): Promise<void> {
    if (!this.historyOptions) return;
    try {
      const store = new HistoryStore(
        this.workspacePath ?? process.cwd(),
        this.sessionId,
        this.historyOptions,
        this.harnessName,
      );
      await store.init();
      const existing = await store.readFacts();
      if (existing.trim()) {
        this.prologQuery(`deepclause_mi:repl_history_load('${this.sessionId}', ${quoteString(existing)})`);
      }
      this.history = store;
      this.assertHistoryFacts(
        await store.startSession({ model: this.model, backend: this.provider }, { resume: this.resume }),
      );
    } catch (error) {
      this.warnHistory(error);
    }
  }

  /**
   * Restore a resumed session's harness from its durable snapshot and continue
   * its turn numbering. The saved module is a restore, not a user load, so it
   * is not recorded as a new change.
   */
  private async restoreHarnessFromHistory(snapshot: SessionHarnessSnapshot): Promise<void> {
    this.turnCounter = (await this.history?.lastTurn(this.sessionId)) ?? 0;
    const module = snapshot.module.trim();
    if (module) {
      const result = await this.loadCode(snapshot.module);
      if (result.error) {
        this.warnHistory(new Error(`could not restore harness: ${result.error}`));
      }
    }
    // Consistency check: the saved hash covers the module *and* its scripts;
    // scripts live in the workspace and may have been edited since.
    const hash = harnessHash(snapshot.module, await this.scriptHashes());
    if (snapshot.hash && hash !== snapshot.hash) {
      this.warnHistory(new Error('harness or its scripts differ from the saved session state'));
    }
  }

  /**
   * Write the durable harness snapshot beside the session so `--session` can
   * restore it even after the process exits. Captures the *current* module
   * (including goal-mode drift) plus the serialized conversation memory.
   */
  private async persistSessionHarness(version: number, clean?: boolean): Promise<void> {
    if (!this.history) return;
    try {
      const module = this.moduleSource();
      const scripts = await this.scriptHashes();
      const hash = harnessHash(module, scripts);
      const isClean = clean
        ?? (this.history.currentHash ? hash === this.history.currentHash : version === 0);
      await this.history.writeSessionHarness({
        sessionId: this.sessionId,
        version,
        hash,
        clean: isClean,
        savedAt: new Date().toISOString(),
        scripts,
        memory: {
          harness: this.dumpMemoryTerm('harness'),
          meta: this.dumpMemoryTerm('meta'),
          ...(this.agentMessages.length > 0 ? { agent: this.agentMessages } : {}),
        },
        module,
      });
    } catch (error) {
      this.warnHistory(error);
    }
  }

  /** Serialize a live memory slot back to a Prolog term (for resume). */
  private dumpMemoryTerm(slot: 'harness' | 'meta'): string {
    const result = this.prologQuery(
      `deepclause_mi:repl_memory_term('${this.sessionId}', ${slot}, Text)`
    );
    const text = plainValue(result?.Text);
    return typeof text === 'string' && text.trim() ? text : '[]';
  }

  private assertHistoryFacts(facts: string[]): void {
    if (!this.history || facts.length === 0) return;
    try {
      this.prologQuery(
        `deepclause_mi:repl_history_record('${this.sessionId}', ${quoteString(facts.join('\n'))})`
      );
    } catch (error) {
      this.warnHistory(error);
    }
  }

  private recordHistoryEvent(turn: number, event: DMLEvent): void {
    this.history?.recordEvent(
      { turn, frame: event.frameKind },
      event as unknown as { type: string } & Record<string, unknown>,
    );
  }

  private manifestText(): string {
    const result = this.prologQuery(`deepclause_mi:repl_manifest('${this.sessionId}', Text)`);
    return String(plainValue(result?.Text) ?? '');
  }

  private predicateList(): string[] {
    const result = this.prologQuery(`deepclause_mi:repl_user_predicates_text('${this.sessionId}', Text)`);
    return String(plainValue(result?.Text) ?? '')
      .split(',')
      .map((entry) => entry.trim())
      .filter(Boolean);
  }

  private moduleSource(): string {
    const result = this.prologQuery(`deepclause_mi:export_harness('${this.sessionId}', none, Text)`);
    return String(plainValue(result?.Text) ?? '');
  }

  private async scriptHashes(): Promise<Array<{ path: string; sha256: string }>> {
    const { createHash } = await import('crypto');
    const out: Array<{ path: string; sha256: string }> = [];
    for (const rel of this.scriptFiles()) {
      try {
        const content = await fs.readFile(this.resolveWorkspacePath(rel));
        out.push({ path: rel, sha256: createHash('sha256').update(content).digest('hex') });
      } catch {
        out.push({ path: rel, sha256: 'missing' });
      }
    }
    return out;
  }

  private diffPredicates(
    before: string[],
    after: string[],
    touched: string[],
  ): HistoryChangedPredicate[] {
    const beforeSet = new Set(before);
    const afterSet = new Set(after);
    const byPredicate = new Map<string, HistoryChangedPredicate>();
    for (const predicate of touched) {
      if (!afterSet.has(predicate)) continue;
      byPredicate.set(predicate, { predicate, action: beforeSet.has(predicate) ? 'replaced' : 'added' });
    }
    for (const predicate of before) {
      if (!afterSet.has(predicate)) byPredicate.set(predicate, { predicate, action: 'removed' });
    }
    return [...byPredicate.values()].sort((a, b) => a.predicate.localeCompare(b.predicate));
  }

  private touchedPredicates(): string[] {
    return this.lastCommitPredicates().split(',').map((entry) => entry.trim()).filter(Boolean);
  }

  /** Record a harness version; returns the new version number. */
  private async recordHistoryChange(
    kind: 'commit' | 'load' | 'reset',
    turn: number,
    before: string[],
    overrides: { touched?: string[] } = {},
  ): Promise<number | undefined> {
    if (!this.history) return undefined;
    try {
      const after = this.predicateList();
      const touched = overrides.touched ?? (kind === 'commit' ? this.touchedPredicates() : after);
      const changed = this.diffPredicates(before, after, touched);
      const { facts, version } = await this.history.recordChange({
        kind,
        sessionId: this.sessionId,
        turn,
        changed,
        scripts: await this.scriptHashes(),
        module: this.moduleSource(),
        manifest: this.manifestText(),
      });
      this.assertHistoryFacts(facts);
      await this.persistSessionHarness(version, true);
      return version;
    } catch (error) {
      this.warnHistory(error);
      return undefined;
    }
  }

  private async finishHistoryTurn(record: HistoryTurnRecord): Promise<void> {
    if (!this.history) return;
    try {
      this.assertHistoryFacts(await this.history.recordTurn(record));
      await this.history.flushEvents();
    } catch (error) {
      this.warnHistory(error);
    }
  }

  private warnHistory(error: unknown): void {
    if (this.historyWarned) return;
    this.historyWarned = true;
    if (process.env.DEBUG_REPL) {
      console.error('[repl] history recording failed:', error);
    }
  }

  /** Extract a `.zip` bundle and load its harness.dml. */
  private async loadBundle(zipPath: string): Promise<{ error?: string }> {
    const data = new Uint8Array(await fs.readFile(zipPath));
    const entries = unzipSync(data);
    const harnessName = Object.keys(entries).find((name) => name === 'harness.dml' || name.endsWith('/harness.dml'));
    if (!harnessName) {
      return { error: 'bundle does not contain harness.dml' };
    }

    const files: Array<{ rel: string; bytes: Uint8Array }> = [];
    for (const [name, bytes] of Object.entries(entries)) {
      if (name === harnessName || name === 'manifest.json') continue;
      const rel = name.replace(/^\.\//, '');
      if (path.isAbsolute(rel) || rel.split('/').includes('..')) {
        return { error: `unsafe bundle entry: ${name}` };
      }
      this.resolveWorkspacePath(rel);
      files.push({ rel, bytes });
    }

    for (const file of files) {
      const abs = this.resolveWorkspacePath(file.rel);
      await fs.mkdir(path.dirname(abs), { recursive: true });
      await fs.writeFile(abs, Buffer.from(file.bytes));
    }

    return this.loadCode(strFromU8(entries[harnessName]));
  }

  /** Read-only listing of the living module (`all`, `Name/Arity`, or `tools`). */
  async listing(filter: string = 'all'): Promise<string> {
    const filterTerm = filter === 'all' || filter === 'tools'
      ? filter
      : toListingFilter(filter);
    const result = this.prologQuery(
      `deepclause_mi:repl_listing('${this.sessionId}', ${filterTerm}, Text)`
    );
    const text = plainValue(result?.Text);
    return typeof text === 'string' ? text : '';
  }

  /**
   * Run a goal and enumerate its solutions.
   *
   * Yields `solution` events (one per answer), interleaved with `output`/`log`/
   * `answer` events from the goal, and finally `finished` or `error`.
   */
  async *query(goal: string, options: DMLQueryOptions = {}): AsyncGenerator<DMLEvent> {
    if (this.disposed) {
      yield { type: 'error', content: 'Session has been disposed' };
      return;
    }
    this.modelCallsThisTurn = 0;
    this.modelCallBudget = this.maxModelCalls;
    const frameKind = options.frameKind ?? 'object';
    const start = this.prologQuery(
      `deepclause_mi:repl_initial_state('${this.sessionId}', ${frameKind}, State), `
      + `deepclause_mi:query_start('${this.sessionId}', ${quoteString(goal)}, ${frameKind}, State, Error)`
    );
    const error = plainValue(start?.Error);
    if (error === undefined || error === null) {
      yield { type: 'error', content: 'Failed to start query' };
      return;
    }
    if (error !== 'none') {
      yield { type: 'error', content: String(error) };
      return;
    }

    this.turnCounter += 1;
    const historyTurn = this.turnCounter;
    const historyStart = Date.now();

    const frame = this.runner.registerDefaultFrame(frameKind);
    if (!frame) {
      yield { type: 'error', content: 'Failed to create query frame' };
      return;
    }

    let historyOutcome: HistoryTurnRecord['outcome'] = 'query';
    try {
      for await (const event of this.runner.runFrame(frame.id, this.buildRunOptions(options))) {
        this.recordHistoryEvent(historyTurn, event);
        if (event.type === 'error') historyOutcome = 'failed';
        yield event;
      }
    } finally {
      this.runner.endFrame(frame.id);
    }

    // An aborted goal may have asserted scratch state; preserve it.
    if (options.signal?.aborted) {
      historyOutcome = 'aborted';
      const saved = await this.saveCurrentHarness('cancelled');
      yield {
        type: 'error',
        content: saved ? `turn aborted; current harness saved to ${saved}` : 'turn aborted',
      };
    }

    await this.finishHistoryTurn({
      turn: historyTurn,
      mode: 'goal',
      request: goal,
      outcome: historyOutcome,
      durationMs: Date.now() - historyStart,
      modelCalls: this.modelCallsThisTurn,
    });
  }

  /** Load the bundled DML reference once, for inclusion in the meta prompt. */
  private async loadDmlReference(): Promise<string> {
    if (this.dmlReference !== undefined) {
      return this.dmlReference;
    }
    try {
      this.dmlReference = await fs.readFile(getWorkspaceDocAssetPath('dml-reference'), 'utf8');
    } catch {
      this.dmlReference = '';
    }
    return this.dmlReference;
  }

  /** Load the curated authoring patterns once, for the meta prompt. */
  private async loadDmlAuthoring(): Promise<string> {
    if (this.dmlAuthoring !== undefined) {
      return this.dmlAuthoring;
    }
    try {
      this.dmlAuthoring = await fs.readFile(getWorkspaceDocAssetPath('dml-authoring'), 'utf8');
    } catch {
      this.dmlAuthoring = '';
    }
    return this.dmlAuthoring;
  }

  /** Load a bundled doc asset once (cached by field). */
  private async loadDocAsset(name: 'harness-examples' | 'harness-patterns' | 'prolog-snippets'): Promise<string> {
    const cached = name === 'harness-examples'
      ? this.harnessExamples
      : name === 'harness-patterns'
        ? this.harnessPatterns
        : this.prologSnippets;
    if (cached !== undefined) return cached;
    let text = '';
    try {
      text = await fs.readFile(getWorkspaceDocAssetPath(name), 'utf8');
    } catch {
      text = '';
    }
    if (name === 'harness-examples') this.harnessExamples = text;
    else if (name === 'harness-patterns') this.harnessPatterns = text;
    else this.prologSnippets = text;
    return text;
  }

  /** Predicates replaced by the most recent successful commit ("name/arity, …"). */
  private lastCommitPredicates(): string {
    const result = this.prologQuery(
      `deepclause_mi:repl_last_commit_summary('${this.sessionId}', Text)`
    );
    return String(plainValue(result?.Text) ?? '');
  }

  /**
   * Serialize the *current* living module (including any scratch edits) to a
   * temp .dml file. Used as an abort safety net so a cancelled turn's work is
   * not lost.
   */
  /** Sorted arities of a user predicate in the living module (e.g. `agent_main`). */
  predicateArities(name: string): number[] {
    const safe = /^[a-z_][A-Za-z0-9_]*$/.test(name) ? name : `'${name.replace(/'/g, "''")}'`;
    const result = this.prologQuery(
      `deepclause_mi:repl_predicate_arities('${this.sessionId}', ${safe}, Arities)`
    );
    const arities = plainValue(result?.Arities);
    if (!Array.isArray(arities)) return [];
    return arities
      .map((arity) => Number(plainValue(arity)))
      .filter((arity) => Number.isFinite(arity));
  }

  /** True when the living module has no user predicates (nothing worth saving). */
  isEmpty(): boolean {
    const result = this.prologQuery(
      `deepclause_mi:repl_user_predicates_text('${this.sessionId}', Text)`
    );
    return String(plainValue(result?.Text) ?? '').trim() === '';
  }

  /** Number of user predicates (`Name/Arity`) currently in the harness. */
  predicateCount(): number {
    return this.predicateList().length;
  }

  async saveCurrentHarness(
    reason: string,
    header: string = `aborted turn: ${reason}`,
  ): Promise<string | null> {
    const result = this.prologQuery(
      `deepclause_mi:export_harness('${this.sessionId}', ${quoteString(header)}, Text)`
    );
    const text = String(plainValue(result?.Text) ?? '');
    if (!text.trim()) {
      return null;
    }
    // A bundle (module + current scripts) so an aborted
    // turn's work is fully recoverable, not just the module source.
    const file = path.join(
      os.tmpdir(),
      `deepclause-repl-${this.sessionId}-${Date.now()}.zip`,
    );
    await this.writeBundle(file, text, this.scriptFiles(), header);
    return file;
  }

  /** Clear the living module and memories (the `reset.` command). */
  async reset(): Promise<void> {
    if (this.isFrozen()) {
      throw new Error('harness is frozen; reset is disabled');
    }
    const before = this.predicateList();
    this.prologQuery(`deepclause_mi:clear_session('${this.sessionId}')`);
    this.runner.clearFrames();
    await this.recordHistoryChange('reset', this.turnCounter, before);
  }

  /**
   * Undo the most recent harness transaction by restoring the previous history
   * version. No-op when there is nothing to undo; refused when frozen. The
   * rollback itself is recorded as a change (harness_rollback/5).
   */
  async rollback(): Promise<{ ok: boolean; from?: number; to?: number; error?: string }> {
    if (this.disposed) return { ok: false, error: 'Session has been disposed' };
    if (!this.history) return { ok: false, error: 'history is not enabled; rollback is unavailable' };
    if (this.isFrozen()) return { ok: false, error: 'harness is frozen; rollback is disabled' };
    const plan = await this.history.prepareRollback();
    if (!plan) return { ok: false, error: 'no harness changes to roll back' };
    const result = this.prologQuery(
      `deepclause_mi:repl_rollback('${this.sessionId}', ${quoteString(plan.module)}, Result)`
    );
    const outcome = plainValue(result?.Result);
    if (outcome !== 'ok') {
      return { ok: false, error: String(outcome ?? 'rollback failed') };
    }
    this.assertHistoryFacts(
      await this.history.recordRollback(plan.from, plan.to, this.turnCounter, this.sessionId, plan.module)
    );
    return { ok: true, from: plan.from, to: plan.to };
  }

  // ---- Freeze lock (M2) ----

  /** Make the living module read-only (assert/retract/load/commit refused). */
  async freeze(): Promise<void> {
    this.prologQuery(`deepclause_mi:repl_freeze('${this.sessionId}')`);
  }

  /** Like freeze(), but `unfreeze.` is refused for the rest of the process. */
  async lock(): Promise<void> {
    this.prologQuery(`deepclause_mi:repl_lock('${this.sessionId}')`);
  }

  /** Clear the lock list. Throws when the harness is irreversibly locked. */
  async unfreeze(): Promise<void> {
    const result = this.prologQuery(
      `deepclause_mi:repl_try_unfreeze('${this.sessionId}', Error)`
    );
    const error = plainValue(result?.Error);
    if (error !== undefined && error !== null && error !== 'none') {
      throw new Error(String(error));
    }
  }

  isFrozen(): boolean {
    const result = this.prologQuery(`deepclause_mi:repl_is_frozen('${this.sessionId}')`);
    return result?.success === true;
  }

  // ---- Export (M2) ----

  /**
   * Serialize the living module.
   *
   * `.zip` produces the self-contained bundle (harness.dml + linked script
   * files + manifest.json). Any other extension produces a single `.dml` with
   * the linked scripts auto-inlined as `embedded_file/2` facts.
   */
  async export(file?: string, options: HarnessExportOptions = {}): Promise<HarnessExportResult> {
    const header = options.header ?? buildHarnessHeader({
      timestamp: new Date().toISOString(),
      backend: options.skill ? 'skill' : this.provider,
      model: this.model,
    });
    const result = this.prologQuery(
      `deepclause_mi:export_harness('${this.sessionId}', ${quoteString(header)}, Text)`
    );
    const moduleText = String(plainValue(result?.Text) ?? '');

    const target = path.resolve(
      this.workspacePath ?? process.cwd(),
      file ?? options.path ?? 'harness.dml',
    );
    const scripts = this.scriptFiles();

    if (target.toLowerCase().endsWith('.zip')) {
      await this.writeBundle(target, moduleText, scripts, header);
    } else {
      await fs.writeFile(target, await this.inlineScripts(moduleText, scripts), 'utf8');
    }

    const stats = this.prologQuery(
      `deepclause_mi:export_stats('${this.sessionId}', Clauses, Tools, Scripts)`
    );
    return {
      path: target,
      clauses: Number(plainValue(stats?.Clauses) ?? 0),
      tools: Number(plainValue(stats?.Tools) ?? 0),
      scripts: Number(plainValue(stats?.Scripts) ?? 0),
    };
  }

  /** Inline linked script files as `embedded_file/2` facts on a single .dml. */
  private async inlineScripts(moduleText: string, scripts: string[]): Promise<string> {
    const facts: string[] = [];
    for (const rel of [...scripts].sort()) {
      const content = await this.readWorkspaceFile(rel);
      if (content === null) continue;
      facts.push(`embedded_file(${quotePrologString(rel)}, ${quotePrologString(content)}).`);
    }
    if (facts.length === 0) return moduleText;
    return `% inlined script files (materialized on load)\n${facts.join('\n')}\n\n${moduleText}`;
  }

  /** Write a `.zip` bundle: harness.dml + linked files + manifest.json. */
  private async writeBundle(
    target: string,
    moduleText: string,
    scripts: string[],
    header: string,
  ): Promise<void> {
    const entries: Record<string, Uint8Array> = { 'harness.dml': strToU8(moduleText) };
    const manifestFiles: Array<{ path: string; bytes: number }> = [];
    for (const rel of [...scripts].sort()) {
      const content = await this.readWorkspaceFile(rel);
      if (content === null) continue;
      const bytes = strToU8(content);
      entries[rel] = bytes;
      manifestFiles.push({ path: rel, bytes: bytes.length });
    }
    entries['manifest.json'] = strToU8(`${JSON.stringify({
      version: 1,
      header,
      model: this.model,
      provider: this.provider,
      timestamp: new Date().toISOString(),
      files: manifestFiles,
    }, null, 2)}\n`);

    await fs.mkdir(path.dirname(target), { recursive: true });
    await fs.writeFile(target, Buffer.from(zipSync(entries, { level: 6 })));
  }

  /**
   * Run a goal in an isolated `execute` child frame and collect its result.
   * The child frame uses the living scratch module and may itself call task()
   * (RLM-style recursion); its harness-memory writes are discarded afterward.
   */
  async executeGoal(
    goal: string,
    limit = 5,
    onEvent?: (event: DMLEvent) => void,
    signal?: AbortSignal,
    bindingMode: BindingMode = 'full',
  ): Promise<HarnessExecuteResult> {
    const startedAt = Date.now();
    const elapsed = (): number => Date.now() - startedAt;
    const childDepth = this.frameDepth + 1;
    if (childDepth > this.maxDepth) {
      return {
        bindings: [],
        solutions: [],
        output: '',
        error: `maximum frame depth (${this.maxDepth}) exceeded`,
        elapsedMs: elapsed(),
      };
    }

    const start = this.prologQuery(
      `deepclause_mi:repl_execute_frame('${this.sessionId}', ${quoteString(goal)}, FrameId, Error)`
    );
    const error = plainValue(start?.Error);
    if (error === undefined || error === null) {
      return { bindings: [], solutions: [], output: '', error: 'Failed to start execute frame', elapsedMs: elapsed() };
    }
    if (error !== 'none') {
      return { bindings: [], solutions: [], output: '', error: String(error), elapsedMs: elapsed() };
    }

    const frameId = String(plainValue(start?.FrameId));
    const frame = this.runner.registerFrame(frameId, 'nested', 1);
    // `execute` is a scratch test: capture harness memory and restore it after,
    // so a goal's `system/1`/`user/1` writes do not leak into the live harness.
    const memoryBefore = this.getMemory('harness');
    const solutions: HarnessExecuteResult['solutions'] = [];
    let output = '';
    let runError: string | undefined;
    this.frameDepth = childDepth;
    try {
      for await (const event of this.runner.runFrame(frame.id, this.buildRunOptions({ signal }))) {
        onEvent?.(event);
        if (event.type === 'solution') {
          if (solutions.length < limit) {
            solutions.push(event.solution?.bindings ?? []);
          }
          // Stop enumerating once the limit is reached: a nondeterministic goal
          // (e.g. an unlabelled CLP model) can otherwise produce unbounded
          // solutions and flood the session before the turn timeout.
          if (solutions.length >= limit) {
            break;
          }
        } else if (event.type === 'output' || event.type === 'answer') {
          output += `${event.content ?? ''}\n`;
        } else if (event.type === 'error') {
          runError = event.content;
        }
      }
    } finally {
      this.runner.endFrame(frame.id);
      this.frameDepth = childDepth - 1;
      this.setMemory('harness', memoryBefore);
    }

    const bindingCount = solutions[0]?.length ?? 0;
    return {
      ...shapeBindings(solutions, bindingMode),
      output: output.slice(0, 4000),
      error: runError,
      elapsedMs: elapsed(),
      ...(bindingMode === 'full'
        ? {}
        : {
          bindingsNote: bindingMode === 'none'
            ? `${bindingCount} binding${bindingCount === 1 ? '' : 's'} omitted (use bindings: "brief" to see the names, or "full" for the values)`
            : `values shortened to ${BRIEF_VALUE_CHARS} characters (use bindings: "full" for the whole value)`,
        }),
    };
  }

  // ---- Authoring agent (TS) ----------------------------------------------

  /**
   * The TypeScript authoring agent: a normal `LLMBackend` agent with harness
   * tools. It runs one or more committed turns in a loop until the model calls
   * `finish`. It runs one or more committed turns per request.
   */
  async *agentPrompt(request: string, options: DMLQueryOptions = {}): AsyncGenerator<DMLEvent> {
    if (this.disposed) {
      yield { type: 'error', content: 'Session has been disposed' };
      return;
    }
    if (!this.llmBackend) {
      yield { type: 'error', content: 'authoring agent requires an injected LLM backend' };
      return;
    }

    this.turnCounter += 1;
    const turn = this.turnCounter;
    const historyStart = Date.now();
    this.lastTurnCommitted = false;
    this.lastCommitVersion = undefined;
    // No automatic transaction: `define`/`assert`/… are refused until the agent
    // calls `begin_turn`, so the transaction is explicit and visible.

    const agent = await this.getHarnessAgent();
    let failed = false;
    let nudges = 0;
    const maxNudges = 3;
    let requestText = request;
    try {
      for (;;) {
        for await (const event of agent.run(requestText, { signal: options.signal })) {
          // A nested (`execute` child) error is a tool result the agent can
          // react to; only the agent's own errors fail the turn.
          if (event.type === 'error' && event.frameKind !== 'nested') failed = true;
          this.recordHistoryEvent(turn, event);
          yield event;
        }
        if (options.signal?.aborted || failed) break;
        // The loop ended with an open transaction: nudge the agent to commit or
        // discard it rather than auto-committing silently.
        if (this.transactionBefore === null) break;
        if (nudges >= maxNudges) break;
        nudges += 1;
        requestText = 'NUDGE: you still have an OPEN edit transaction with uncommitted changes. '
          + 'Call `end_turn` to commit it (or `rollback` to discard it), then call `finish`.';
        this.recordHistoryEvent(turn, { type: 'output', content: requestText });
        yield { type: 'output', content: requestText };
      }
    } catch (error) {
      failed = true;
      const message = error instanceof Error ? error.message : String(error);
      this.recordHistoryEvent(turn, { type: 'error', content: message });
      yield { type: 'error', content: message };
    } finally {
      this.agentMessages = agent.getMessages();
    }

    if (options.signal?.aborted || failed) {
      await this.rollbackTurn();
    } else if (this.transactionBefore !== null) {
      // Last resort after the nudges: commit so nothing is lost, and show it.
      await this.endTurn();
      const marker: DMLEvent = {
        type: 'tool_call',
        toolName: 'end_turn',
        toolArgs: {},
        toolState: 'completed',
        toolSummary: 'commit transaction (host; agent did not end_turn)',
      };
      this.recordHistoryEvent(turn, marker);
      yield marker;
    }
    await this.persistSessionHarness(this.history?.currentVersion ?? 0);

    const outcome = options.signal?.aborted
      ? 'aborted'
      : failed
        ? 'failed'
        : this.lastTurnCommitted
          ? 'committed'
          : 'answered';
    await this.finishHistoryTurn({
      turn,
      mode: 'prompt',
      request,
      outcome,
      commitVersion: this.lastCommitVersion,
      durationMs: Date.now() - historyStart,
      modelCalls: this.modelCallsThisTurn,
      error: failed ? 'authoring turn failed' : undefined,
    });
  }

  /**
   * Deliver a steering message into the running authoring turn (agent mode).
   * Ignored when no agent turn is active.
   */
  steer(text: string): void {
    this.harnessAgent?.steer(text);
  }

  private async getHarnessAgent(): Promise<HarnessAgent> {
    if (this.harnessAgent) return this.harnessAgent;
    this.harnessAgent = new HarnessAgent({
      tools: createHarnessTools(this),
      llmBackend: this.llmBackend!,
      model: this.model,
      temperature: this.temperature,
      maxTokens: this.maxTokens,
      streaming: true,
      maxModelCalls: this.maxModelCalls,
      askInput: (prompt, options) => this.requireInput(prompt, options),
      contextWindow: this.contextWindow,
      compaction: this.compactionSettings
        ? { settings: this.compactionSettings, auto: this.compactionAuto }
        : undefined,
      workspacePath: this.workspacePath,
      environment: this.executionEnvironment,
      currentModel: `${this.provider}:${this.model}`,
      modelCatalog: this.modelResolver?.catalog(),
      knownMessages: this.agentMessages,
    });
    return this.harnessAgent;
  }

  /** Read the current harness manifest for the agent system prompt. */
  async manifest(): Promise<string> {
    return this.manifestText();
  }

  /** The models available in this session (agent `models` tool). */
  async listModels(): Promise<string> {
    const current = `${this.provider}:${this.model}`;
    const resolver = this.modelResolver;
    if (!resolver) {
      return `current model: ${current}\n(no model catalog is configured; with_model is unavailable)`;
    }
    const models = resolver.catalog();
    const body = models.length > 0
      ? models.map((model) => `  ${formatModelCatalogEntry(model)}`).join('\n')
      : '  (none)';
    return `current model: ${current}\n\navailable models (price = USD per 1M tokens, input/output):\n${body}`;
  }

  /**
   * Generate a diagram of one entry predicate: ask the active model for Mermaid,
   * then render it deterministically to a Unicode tree (always) and an SVG (for
   * the terminal-image path). One model call.
   */
  async diagram(
    entry?: string,
    mode: DiagramMode = 'presentation',
    options: { signal?: AbortSignal } = {},
  ): Promise<DiagramResult> {
    if (!this.llmBackend) {
      throw new Error('diagram generation needs a model backend');
    }
    const listing = await this.listing('all');
    const resolved = (entry ?? '').trim() || this.defaultDiagramEntry(listing);
    if (!resolved) {
      throw new Error('the harness has no predicate to diagram yet');
    }
    const prompt = buildDiagramPrompt({ entry: resolved, mode, listing });
    const { text } = await generateLlmReply({
      messages: [{ role: 'user', content: prompt }],
      modelOptions: { provider: this.provider as Provider, model: this.model, temperature: this.temperature },
      llmBackend: this.llmBackend,
      signal: options.signal,
    });
    const mermaid = extractMermaid(text);
    const warnings = validateMermaid(mermaid);
    const svg = renderFlowchartSvg(parseMermaidFlowchart(mermaid));
    return {
      entry: resolved,
      mode,
      mermaid,
      ascii: renderMermaidAscii(mermaid),
      svg: svg.svg,
      width: svg.width,
      height: svg.height,
      warnings,
    };
  }

  /** Diagram of a skill for the authoring agent's `visualize` tool. */
  async visualize(entry?: string, mode: DiagramMode = 'presentation'): Promise<{ message: string; event?: DMLEvent }> {
    const result = await this.diagram(entry, mode);
    const png = await rasterizeSvg(result.svg, { width: result.width, height: result.height });
    if (png) {
      return {
        message: `Diagram of ${result.entry} (${result.mode}) is displayed to the user as an image. Do not repeat the diagram text.`,
        event: { type: 'media', media: { mimeType: 'image/svg+xml', data: png.toString('base64') } },
      };
    }
    return { message: result.ascii };
  }

  /** Display a workspace image (agent `show_image` tool). */
  async showImage(relPath: string): Promise<{ message: string; event?: DMLEvent }> {
    return this.mediaEventFor(relPath);
  }

  /** Generate an image and display it (agent `generate_image` tool). */
  async generateImage(prompt: string): Promise<{ message: string; event?: DMLEvent }> {
    if (!this.imageBackend) {
      return { message: 'Image generation is unavailable: no image backend is configured.' };
    }
    const result = await this.imageBackend.generate({ prompt });
    const shown = await this.mediaEventFor(result.path);
    return { message: `Generated ${result.path} (${result.mimeType}). ${shown.message}`, event: shown.event };
  }

  /** Write a workspace file (agent `write_file` tool); creates parent dirs. */
  async writeFile(relPath: string, content: string): Promise<{ ok: boolean; error?: string }> {
    try {
      const abs = this.resolveWorkspacePath(relPath);
      await fs.mkdir(path.dirname(abs), { recursive: true });
      await fs.writeFile(abs, content, 'utf8');
      return { ok: true };
    } catch (error) {
      return { ok: false, error: (error as Error).message };
    }
  }

  /**
   * Build a `media` event for a workspace image (resolved to a PNG for inline
   * display). Returning an event lets any host render it through its normal
   * event pipeline — no session-specific image channel needed.
   */
  private async mediaEventFor(relPath: string): Promise<{ message: string; event?: DMLEvent }> {
    const resolved = await resolveMedia(this.workspacePath ?? process.cwd(), { path: relPath });
    if (!resolved) {
      return { message: `Cannot read image ${relPath}.` };
    }
    const event: DMLEvent = {
      type: 'media',
      media: { path: relPath, mimeType: resolved.mimeType, data: resolved.pngBase64 },
    };
    const message = resolved.pngBase64
      ? `Image ${relPath} (${resolved.mimeType}) is displayed to the user.`
      : `Image ${relPath} (${resolved.mimeType}) is at workspace path ${relPath} `
        + '(not rasterized for inline display: SVG needs @resvg/resvg-js, rasters need sharp).';
    return { message, event };
  }

  /** Pick a default diagram entry: `agent_main/N`, else the first predicate. */
  private defaultDiagramEntry(listing: string): string | undefined {
    const mains = this.predicateArities('agent_main');
    if (mains.length > 0) {
      return `agent_main/${mains[0]}`;
    }
    const reserved = new Set(['doc', 'module_doc', 'tool', 'system', 'user', 'param', 'parameters', 'answer']);
    for (const line of listing.split('\n')) {
      const match = /^([a-z][A-Za-z0-9_]*)\/(\d+)\s*$/.exec(line.trim());
      if (match && !reserved.has(match[1])) {
        return `${match[1]}/${match[2]}`;
      }
    }
    return undefined;
  }

  async beginTurn(): Promise<{ ok: boolean; error?: string }> {
    // A second begin_turn would re-snapshot the *mid-transaction* state, so
    // `rollback` would stop meaning "the last committed harness" and the
    // recorded diff would lose the edits made before it.
    if (this.transactionBefore !== null) {
      return {
        ok: false,
        error: 'a transaction is already open; call end_turn to commit it, or rollback to discard it',
      };
    }
    this.prologQuery(`deepclause_mi:repl_snapshot_save('${this.sessionId}')`);
    this.prologQuery(`deepclause_mi:repl_edit_touched_clear('${this.sessionId}')`);
    this.transactionBefore = this.predicateList();
    this.transactionDirty = false;
    return { ok: true };
  }

  async endTurn(): Promise<HarnessCommitResult> {
    if (this.transactionBefore === null) {
      return { committed: false, changed: [], error: 'no open transaction' };
    }
    const before = this.transactionBefore;
    const after = this.predicateList();
    const changed = this.simplePredicateDiff(before, after);
    const touched = this.editTouched();
    this.transactionBefore = null;
    const committed = this.transactionDirty;
    let version: number | undefined;
    if (committed && this.history) {
      version = await this.recordHistoryChange('commit', this.turnCounter, before, { touched });
    }
    this.lastTurnCommitted = committed;
    this.lastCommitVersion = version;
    this.transactionDirty = false;
    this.prologQuery(`deepclause_mi:repl_edit_touched_clear('${this.sessionId}')`);
    await this.persistSessionHarness(this.history?.currentVersion ?? 0);

    // Advisory: probe changed pure predicates for accidental nondeterminism.
    let warnings: string[] = [];
    if (committed) {
      const candidates = new Set<string>();
      for (const entry of touched) candidates.add(entry.trim());
      for (const entry of changed) if (entry.startsWith('+')) candidates.add(entry.slice(1));
      warnings = await this.probeNondeterminism([...candidates]);
      warnings.push(...this.probeToolContracts());
      warnings.push(...this.probeDocContracts());
    }
    return { committed: true, changed, warnings };
  }

  /**
   * Check every registered `tool/2` annotation: the nested agent inside `task/N`
   * calls the tool body, so it must reach real logic with the same name and
   * arity. Violations only surface when a nested agent calls the tool, which may
   * be long after the commit — report them here instead.
   */
  private probeToolContracts(): string[] {
    try {
      const result = this.prologQuery(
        `deepclause_mi:repl_validate_tools('${this.sessionId}', Warnings)`,
      );
      const raw = plainValue(result?.Warnings);
      if (!Array.isArray(raw)) return [];
      return raw.map((entry) => String(entry)).filter(Boolean);
    } catch {
      // A validation failure must never fail the commit.
      return [];
    }
  }

  /**
   * Warn about `doc/2` facts that document a predicate with no clauses. Such a
   * predicate fails silently when called, and its name still appears in the
   * manifest, so the harness looks fine and returns empty results.
   */
  private probeDocContracts(): string[] {
    try {
      const result = this.prologQuery(
        `deepclause_mi:repl_validate_docs('${this.sessionId}', Warnings)`,
      );
      const raw = plainValue(result?.Warnings);
      if (!Array.isArray(raw)) return [];
      return raw.map((entry) => String(entry)).filter(Boolean);
    } catch {
      // A validation failure must never fail the commit.
      return [];
    }
  }

  /**
   * Probe changed predicates for accidental nondeterminism: a pure predicate
   * that yields more than `threshold` solutions on fresh variables is very likely
   * missing an `once/1`/`!` and would flood an `execute`. Fact tables and
   * side-effecting predicates are skipped by the Prolog probe.
   */
  private async probeNondeterminism(indicators: string[]): Promise<string[]> {
    const reserved = new Set(['doc', 'module_doc', 'tool']);
    const warnings: string[] = [];
    const threshold = 100;
    const bound = 200000;
    for (const indicator of indicators.slice(0, 8)) {
      const match = /^([a-z][A-Za-z0-9_]*)\/(\d+)$/.exec(indicator);
      if (!match || reserved.has(match[1])) continue;
      const name = match[1];
      const arity = match[2];
      try {
        const result = this.prologQuery(
          `deepclause_mi:repl_probe_nondeterminism('${this.sessionId}', ${name}, ${arity}, ${threshold}, ${bound}, Result)`,
        );
        const verdict = String(plainValue(result?.Result) ?? '');
        if (verdict === 'many') {
          warnings.push(
            `${name}/${arity} yields more than ${threshold} solutions when called with fresh variables. `
            + 'If a single answer is intended, make it deterministic with once/1 or !.',
          );
        } else if (verdict === 'timeout') {
          warnings.push(
            `${name}/${arity} did not settle within the inference limit when probed `
            + '(possible unbounded search, e.g. label/1 without once/1).',
          );
        }
      } catch {
        // A probe failure must never fail the commit.
      }
    }
    return warnings;
  }

  /** Predicates touched by edit tools in the current transaction. */
  private editTouched(): string[] {
    const result = this.prologQuery(`deepclause_mi:repl_edit_touched_text('${this.sessionId}', Text)`);
    return String(plainValue(result?.Text) ?? '')
      .split(',')
      .map((entry) => entry.trim())
      .filter(Boolean);
  }

  async rollbackTurn(): Promise<void> {
    if (this.transactionBefore === null) return;
    this.prologQuery(`deepclause_mi:repl_snapshot_restore('${this.sessionId}')`);
    this.transactionBefore = null;
    this.lastTurnCommitted = false;
    this.lastCommitVersion = undefined;
    this.transactionDirty = false;
  }

  private simplePredicateDiff(before: string[], after: string[]): string[] {
    const beforeSet = new Set(before);
    const afterSet = new Set(after);
    const changed: string[] = [];
    for (const predicate of after) if (!beforeSet.has(predicate)) changed.push(`+${predicate}`);
    for (const predicate of before) if (!afterSet.has(predicate)) changed.push(`-${predicate}`);
    return changed.sort();
  }

  /** Edit tools require an open transaction (opened by `begin_turn`). */
  private requireTransaction(): { ok: false; error: string } | undefined {
    if (this.transactionBefore === null) {
      return { ok: false, error: 'no open transaction; call begin_turn before editing' };
    }
    return undefined;
  }

  private editResult(result: unknown): { ok: boolean; error?: string; note?: string } {
    if (result === 'ok') {
      this.transactionDirty = true;
      return { ok: true };
    }
    // `ok(Note)` carries a detail the agent should see, e.g. how many clauses a
    // retract actually removed. Reporting it is the point of counting them.
    const match = /^ok\(([\s\S]*)\)$/.exec(String(result).trim());
    if (match) {
      this.transactionDirty = true;
      return { ok: true, note: match[1] };
    }
    return { ok: false, error: String(result) };
  }

  async define(head: string, clauses: string): Promise<{ ok: boolean; error?: string }> {
    const closed = this.requireTransaction();
    if (closed) return closed;
    const result = this.prologQuery(
      `deepclause_mi:repl_define('${this.sessionId}', ${quoteString(head)}, ${quoteString(clauses)}, Result)`
    );
    return this.editResult(plainValue(result?.Result));
  }

  async assertClause(clause: string): Promise<{ ok: boolean; error?: string }> {
    const closed = this.requireTransaction();
    if (closed) return closed;
    const result = this.prologQuery(`deepclause_mi:repl_assert('${this.sessionId}', ${quoteString(clause)}, Result)`);
    return this.editResult(plainValue(result?.Result));
  }

  async assertMany(clauses: string): Promise<{ ok: boolean; error?: string }> {
    const closed = this.requireTransaction();
    if (closed) return closed;
    const result = this.prologQuery(`deepclause_mi:repl_assert_many('${this.sessionId}', ${quoteString(clauses)}, Result)`);
    return this.editResult(plainValue(result?.Result));
  }

  async retractClause(clause: string): Promise<{ ok: boolean; error?: string }> {
    const closed = this.requireTransaction();
    if (closed) return closed;
    const result = this.prologQuery(`deepclause_mi:repl_retract('${this.sessionId}', ${quoteString(clause)}, Result)`);
    return this.editResult(plainValue(result?.Result));
  }

  async deletePredicate(nameArity: string): Promise<{ ok: boolean; error?: string }> {
    const closed = this.requireTransaction();
    if (closed) return closed;
    const match = /^\s*([^\s/]+)\s*\/\s*(\d+)\s*$/.exec(nameArity);
    if (!match) {
      return { ok: false, error: `expected Name/Arity, got '${nameArity}'` };
    }
    const [, name, arityText] = match;
    const atom = /^[a-z][A-Za-z0-9_]*$/.test(name) ? name : `'${name.replace(/'/g, "''")}'`;
    const result = this.prologQuery(
      `deepclause_mi:repl_delete_predicate('${this.sessionId}', ${atom}, ${Number(arityText)}, Result)`
    );
    return this.editResult(plainValue(result?.Result));
  }

  async reference(topic: string): Promise<string> {
    switch (topic.trim().toLowerCase()) {
      case 'language':
      case 'dml':
        return this.loadDmlReference();
      case 'authoring':
        return this.loadDmlAuthoring();
      case 'examples':
        return this.loadDocAsset('harness-examples');
      case 'patterns':
      case 'harness-patterns':
        return this.loadDocAsset('harness-patterns');
      case 'snippets':
        return this.loadDocAsset('prolog-snippets');
      case 'tools':
        return this.hostToolsCatalog ?? '(no host capability tools registered)';
      default:
        return `unknown reference topic '${topic}'. Topics: language, authoring, examples, patterns, snippets, tools.`;
    }
  }

  async execute(
    goal: string,
    limit?: number,
    signal?: AbortSignal,
    onEvent?: (event: DMLEvent) => void,
    bindingMode: BindingMode = 'full',
  ): Promise<HarnessExecuteResult> {
    return this.executeGoal(goal, limit ?? 5, onEvent, signal, bindingMode);
  }

  /**
   * Prompt mode: English request -> meta task -> DML -> commit -> entry run.
   *
   * The meta task runs in a `meta` frame with exactly the host meta tools.
   * Its scratch edits are discarded; the returned DML source is the single
   * source of truth and is committed replace-by-predicate. A failed commit
   * triggers at most one repair round; any failure leaves the harness
   * byte-identical.
   */
  /** Prompt mode: author the harness through the TypeScript agent. */
  async *prompt(request: string, options: DMLQueryOptions = {}): AsyncGenerator<DMLEvent> {
    yield* this.agentPrompt(request, options);
  }

  async dispose(): Promise<void> {
    if (this.disposed) return;
    this.disposed = true;
    if (this.history) {
      try {
        await this.persistSessionHarness(this.history.currentVersion);
        this.assertHistoryFacts(await this.history.endSession('dispose'));
      } catch (error) {
        this.warnHistory(error);
      }
    }
    this.prologQuery(`deepclause_mi:destroy_session('${this.sessionId}')`);
    this.runner.clearFrames();
    await this.runner.dispose();
  }

  private buildRunOptions(options: DMLQueryOptions): InternalRunOptions {
    const inputHandler = options.onInput ?? this.inputHandler;
    return {
      workspacePath: this.workspacePath,
      gasLimit: options.gasLimit ?? this.gasLimit,
      tools: options.tools ?? this.tools,
      toolPolicy: options.toolPolicy ?? this.toolPolicy,
      onInputRequired: inputHandler
        ? (prompt) => this.requireInput(prompt)
        : async () => undefined,
      signal: options.signal,
      onModelCall: () => this.reserveModelCall(),
    };
  }

  // ---- TypeScript compaction (ported from pi) ----

  /** Read a live memory slot as `MemoryMessage[]`. */
  private getMemory(slot: 'meta' | 'harness'): MemoryMessage[] {
    const result = this.prologQuery(
      `deepclause_mi:repl_get_memory('${this.sessionId}', ${slot}, Messages)`
    );
    const raw = toJsValue(result?.Messages);
    if (!Array.isArray(raw)) return [];
    const messages: MemoryMessage[] = [];
    for (const entry of raw) {
      const value = toJsValue(entry) as { role?: unknown; content?: unknown } | null;
      if (!value || typeof value !== 'object') continue;
      const role = String(toJsValue(value.role) ?? 'user');
      const content = String(toJsValue(value.content) ?? '');
      if (role !== 'system' && role !== 'user' && role !== 'assistant') continue;
      messages.push({ role, content });
    }
    return messages;
  }

  /** Replace a live memory slot. */
  private setMemory(slot: 'meta' | 'harness', messages: MemoryMessage[]): void {
    const term = messages
      .map((message) => `message{role: ${message.role}, content: ${quoteString(message.content)}}`)
      .join(', ');
    this.prologQuery(`deepclause_mi:repl_set_memory('${this.sessionId}', ${slot}, [${term}])`);
  }

  /** Build the `memory_compaction` event for a compaction result. */
  private compactionEvent(
    scope: 'loop' | 'session',
    trigger: 'before_model_call' | 'before_user_message',
    result: import('./compaction.js').CompactMemoryResult,
  ): DMLEvent {
    const event = toCompactionEvent(scope, trigger, result);
    const suffix = event.error ? ` (${event.error})` : '';
    return {
      type: 'memory_compaction',
      content: `compact ${scope}.${trigger} ${event.action} ${event.beforeTokens} -> ${event.afterTokens} tokens${suffix}`,
      compactionScope: scope,
      compactionTrigger: trigger,
      compactionAction: event.action,
      beforeTokens: event.beforeTokens,
      afterTokens: event.afterTokens,
      compactionError: event.error,
    };
  }

  /** Loop compaction hook: run before each model call inside a frame. */
  private async compactLoopMessages(
    messages: MemoryMessage[],
    lastInputTokens?: number,
    contextWindow?: number,
  ): Promise<{ messages: MemoryMessage[]; event?: DMLEvent }> {
    const settings = this.compactionSettings;
    if (!settings?.enabled || !this.llmBackend) {
      return { messages };
    }
    const contextTokens = lastInputTokens ?? estimateMemoryTokens(messages);
    if (!shouldCompact(contextTokens, contextWindow ?? this.contextWindow ?? 0, settings)) {
      return { messages };
    }
    const result = await compactMemory({
      messages,
      backend: this.llmBackend,
      model: this.model,
      settings,
      force: true,
    });
    return result.applied
      ? { messages: result.messages, event: this.compactionEvent('loop', 'before_model_call', result) }
      : { messages };
  }

  /**
   * Explicitly compact the authoring conversation. Used by `compact.`; returns
   * the before/after token counts.
   */
  async compact(customInstructions?: string): Promise<{ applied: boolean; beforeTokens: number; afterTokens: number; error?: string }> {
    if (!this.harnessAgent) {
      return { applied: false, beforeTokens: 0, afterTokens: 0, error: 'compaction is disabled' };
    }
    const result = await this.harnessAgent.compactMemory(true, undefined, customInstructions);
    return {
      applied: result.applied,
      beforeTokens: result.tokensBefore,
      afterTokens: result.tokensAfter,
      error: result.error,
    };
  }

  // ---- Workspace file tools (meta task) ----

  /** Resolve a workspace-relative path, rejecting traversal/absolute paths. */
  private resolveWorkspacePath(relPath: string): string {
    const workspace = path.resolve(this.workspacePath ?? process.cwd());
    if (path.isAbsolute(relPath)) {
      throw new Error(`path must be workspace-relative: ${relPath}`);
    }
    const abs = path.resolve(workspace, relPath);
    if (abs !== workspace && !abs.startsWith(workspace + path.sep)) {
      throw new Error(`path escapes the workspace: ${relPath}`);
    }
    return abs;
  }

  private normalizeRel(relPath: string): string {
    return path.relative(path.resolve(this.workspacePath ?? process.cwd()), this.resolveWorkspacePath(relPath))
      .split(path.sep).join('/');
  }

  /** Read a workspace file. */
  private async readWorkspaceFile(relPath: string): Promise<string | null> {
    try {
      return await fs.readFile(this.resolveWorkspacePath(this.normalizeRel(relPath)), 'utf8');
    } catch {
      return null;
    }
  }

  /**
   * Replace the human-in-the-loop channel after construction. The CLI uses
   * this to route input through the REPL's own readline interface instead of a
   * second interface on the same stdin.
   */
  setInputHandler(handler: ((prompt: string, options?: string[]) => Promise<string | undefined>) | undefined): void {
    this.inputHandler = handler;
  }

  /**
   * @deprecated Terminal images now flow as `media` events (rendered by the
   * host), so this is a no-op kept for backwards compatibility.
   */
  setImageHandler(_handler?: (lines: string[]) => void): void {
    // no-op
  }

  /** Resolve a human answer, or undefined when no channel is configured. */
  private async requireInput(prompt: string, options?: string[]): Promise<string | undefined> {
    if (!this.inputHandler) {
      return undefined;
    }
    return this.inputHandler(prompt, options);
  }

  /**
   * Reserve one model call for the current turn. When the budget is reached,
   * the human is asked whether to extend it by another `maxModelCalls`
   * (interactive runs); autonomous runs abort. `0` means unlimited.
   */
  private async reserveModelCall(): Promise<boolean> {
    if (this.maxModelCalls <= 0) {
      this.modelCallsThisTurn += 1;
      return true;
    }
    if (this.modelCallsThisTurn >= this.modelCallBudget) {
      const extend = await this.askBudgetExtension();
      if (!extend) {
        return false;
      }
      this.modelCallBudget += this.maxModelCalls;
    }
    this.modelCallsThisTurn += 1;
    return true;
  }

  private async askBudgetExtension(): Promise<boolean> {
    if (!this.inputHandler) {
      return false;
    }
    const answer = await this.inputHandler(
      `Max model-call budget reached (${this.modelCallBudget}). Continue for another ${this.maxModelCalls} calls? (yes/no)\n`
      + 'change the limit: repl.maxModelCalls in .deepclause/config.json '
      + '("repl": { "maxModelCalls": N }), or pass --max-model-calls N; 0 means unlimited',
      ['yes', 'no'],
    );
    return /^(y|yes)$/i.test((answer ?? '').trim());
  }

  private prologQuery(goal: string): Record<string, unknown> | null {
    try {
      const result = this.swipl.prolog.query(qualifyWithSession(goal, this.sessionId)).once();
      return (result ?? null) as Record<string, unknown> | null;
    } catch (error) {
      if (process.env.DEBUG_REPL) {
        console.error('[repl] Prolog query failed:', goal, error);
      }
      return null;
    }
  }

  private serializeMemory(messages: MemoryMessage[]): string {
    return messages
      .map((message) => `message{role: ${message.role}, content: ${quoteString(message.content)}}`)
      .join(', ');
  }
}

/**
 * Qualify a goal with the session module unless it already names one.
 *
 * DML code resolves unqualified goals in the session module, which is where the
 * harness and the workspace predicates live. A host query would otherwise
 * resolve them in `user`, so `read_file("f.txt", C)` failed from here while
 * working from `execute` or `/goal`. Goals that already carry a qualifier
 * (`deepclause_mi:…`) are left alone.
 */
export function qualifyWithSession(goal: string, sessionId: string): string {
  const trimmed = goal.trim();
  if (trimmed.length === 0) return goal;
  if (/^[a-z][A-Za-z0-9_]*\s*:/.test(trimmed)) return goal;
  return `${sessionId}:(${trimmed})`;
}

/**
 * Apply the `bindings` option to the solutions returned to the agent.
 *
 * `bindings` mirrors `solutions[0]`, so the mode applies to both — a half-done
 * job would leave the full value sitting in the other field.
 */
function shapeBindings(
  solutions: HarnessExecuteResult['solutions'],
  mode: BindingMode,
): Pick<HarnessExecuteResult, 'bindings' | 'solutions'> {
  const first = solutions[0] ?? [];
  if (mode === 'none') return { bindings: [], solutions: [] };
  if (mode === 'brief') {
    const shorten = (entries: HarnessExecuteResult['bindings']) =>
      entries.map(({ name, value }) => ({
        name,
        value: value.length > BRIEF_VALUE_CHARS
          ? `${value.slice(0, BRIEF_VALUE_CHARS)}… (${value.length} chars)`
          : value,
      }));
    return { bindings: shorten(first), solutions: solutions.map(shorten) };
  }
  return { bindings: first, solutions };
}

/** Render a Prolog value for a warning message. */
function formatTerm(value: unknown): string {
  if (typeof value === 'string') return value;
  if (value === null || value === undefined) return String(value);
  if (typeof value === 'object') {
    const record = value as Record<string, unknown>;
    const keys = Object.keys(record).filter((k) => !k.startsWith('$'));
    if (keys.length === 1) {
      const inner = record[keys[0]];
      const args = Array.isArray(inner) && inner.length === 1 && Array.isArray(inner[0]) ? inner[0] : [];
      return `${keys[0]}(${args.map((a) => formatTerm(a)).join(', ')})`;
    }
    try { return JSON.stringify(value); } catch { return String(value); }
  }
  return String(value);
}

/** Quote a JS string as a Prolog double-quoted string. */
export function quoteString(value: string): string {
  return `"${value.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`;
}

/** Turn `fact/2` into the Prolog term `fact/2`; other filters become atoms. */
function toListingFilter(filter: string): string {
  if (/^[a-z][a-zA-Z0-9_]*\/\d+$/.test(filter)) {
    return filter;
  }
  if (/^[a-z][a-zA-Z0-9_]*$/.test(filter)) {
    return filter;
  }
  return `'${filter.replace(/'/g, "''")}'`;
}

export type { Frame };
