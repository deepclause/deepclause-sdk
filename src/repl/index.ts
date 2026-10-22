/**
 * Public REPL facade.
 *
 * `createHarnessRepl` opens a persistent `DMLSession` and exposes the surface
 * used by the CLI (and, later, by pi). M1 implements goal mode, loading, and
 * reset; prompt mode / freeze / export are filled in by later milestones.
 */

import type {
  DMLEvent,
  FormEventPayload,
  ImageBackend,
  LLMBackend,
  MemoryMessage,
  ToolDefinition,
  ToolPolicy,
} from '../types.js';
import type { FrameKind } from '../types.js';
import { DMLSession, type DMLQueryOptions, type DiagramResult } from './session.js';
import type { HarnessExportOptions, HarnessExportResult } from './export.js';
import { resolveReplJudgeSelection, type JevJudgeOptions } from './judge.js';
import { createLimitedBackend } from './backend.js';
import type { HistoryOptions } from './history.js';
import type { JudgeSelection } from '../judge/types.js';
import type { CompactionSettings } from './compaction.js';
import type { DiagramMode } from '../visualize/index.js';
import type { ModelResolver } from './model-resolver.js';
import type { ExecutionBackendKind } from '../system/runtime/shell-manager.js';

export type RunOpts = DMLQueryOptions;

/** Goal-mode events are DMLEvents today; kept as a named alias for the API. */
export type DMLQueryEvent = DMLEvent;
export type ReplEvent = DMLEvent;

export interface HarnessReplOptions {
  sessionId?: string;
  /** Resume the session identified by `sessionId` (restore its saved harness). */
  resume?: boolean;
  workspacePath?: string;
  params?: Record<string, unknown>;
  gasLimit?: number;
  model: string;
  provider: 'openai' | 'anthropic' | 'google' | 'openrouter';
  temperature?: number;
  maxTokens?: number;
  /** Stream model text/reasoning/tool events. Defaults to true for the REPL. */
  streaming?: boolean;
  llmBackend?: LLMBackend;
  tools?: Map<string, ToolDefinition>;
  /** Extra host tools offered to the authoring agent (not DML task/N). */
  authoringTools?: Map<string, ToolDefinition>;
  toolPolicy?: ToolPolicy | null;
  /** Legacy harness-only input handler (prefer onInput). */
  onInputRequired?: (prompt: string) => Promise<string>;
  /** Unified human-in-the-loop channel for meta ask_user and harness input/2. */
  onInput?: (prompt: string, options?: string[]) => Promise<string | undefined>;
  initialHarnessMemory?: MemoryMessage[];
  /** Maximum nested frame depth (default 3). */
  maxDepth?: number;
  /** Maximum model calls per turn (default 24; 0 = unlimited). */
  maxModelCalls?: number;
  /** Host tool catalog text injected into the meta prompt (M5). */
  hostToolsCatalog?: string;
  /** Replace the bundled authoring prompt (e.g. a compact local-model profile). */
  agentSystemPrompt?: string;
  /** This is a Work assistant: expose shell tools instead of authoring tools. */
  workMode?: boolean;
  /** Restrict the authoring agent to these tools (small local-model profile). */
  authoringToolNames?: string[];
  /**
   * Effective shell backend (`agentvm`/`bwrap`/`sandbox-exec`/`clean-room`).
   * Pass `toolset.environment.kind` so the harness `require_sandbox` check and
   * the model's environment block match the toolset that will run commands.
   */
  executionBackend?: ExecutionBackendKind;
  /** Name of the single capability host tool (default `bash`; the browser uses `python`). */
  capabilityTool?: string;
  /** Factory for the L2 process worker (default: `worker_threads`; browser: a Web Worker). */
  processWorkerFactory?: import('./worker-host.js').WorkerFactory;
  /** Awaited before the L2 process worker is created (e.g. flush the workspace). */
  beforeProcessStart?: () => Promise<void>;
  /** Awaited before every spawn/foreground request (parent publishes, worker pulls). */
  beforeProcessSpawn?: () => Promise<void>;
  /** Run processes in embedding-controlled sessions (e.g. one worker per process). */
  createProcessSession?: import('./session.js').DMLSessionOptions['createProcessSession'];
  /** Human-in-the-loop channel for DML `input/2` / `exec(ask_user(...))`. */
  askUser?: (prompt: string, options?: string[]) => Promise<{ answer: string } | { cancelled: true }>;
  /** Called when a form opens (docs/DML_FORMS.md). */
  onForm?: (form: FormEventPayload) => void | Promise<void>;
  /** Explicit judgment backend registry. When omitted, an `llm` backend is built from the active model. */
  judgeSelection?: JudgeSelection;
  /** Default judgment backend name (default: `'llm'`). */
  defaultJudge?: string;
  /** Opt-in Jev (TypeSafe System One) judgment backend. */
  jev?: JevJudgeOptions;
  /** Receives startup diagnostics, e.g. a skipped `jev` backend. */
  onDiagnostic?: (message: string) => void;
  /** Persistent session/trace/harness history. Off unless provided. */
  history?: HistoryOptions;
  /** Harness this session edits (a project can hold several). Default `main`. */
  harness?: string;
  /** Process execution mode: `worker` (L2, default) or `inline` (L1). */
  execution?: 'inline' | 'worker';
  /** Max concurrent model calls across the session and its subagents (default 4; 0 = unlimited). */
  providerConcurrency?: number;
  /** Load the harness's current content on open (default true; `-l` sets false). */
  autoLoadHarness?: boolean;
  /** TypeScript compaction settings (ported from pi). */
  compaction?: { settings: CompactionSettings; auto: boolean };
  /** Active model context window, used for compaction thresholds. */
  contextWindow?: number;
  /** Host image generator for `task(…, media(X))` typed outputs. */
  imageBackend?: ImageBackend;
  /** Resolves `with_model/2` scopes to concrete models. */
  modelResolver?: ModelResolver;
}

export interface RollbackResult {
  ok: boolean;
  from?: number;
  to?: number;
  error?: string;
}

export interface HarnessRepl {
  /** Goal mode: run a goal, enumerating solutions/events. */
  query(goal: string, opts?: RunOpts): AsyncGenerator<DMLQueryEvent>;
  /** Prompt mode (M3): NL -> task -> DML -> commit -> run. */
  prompt(request: string, opts?: RunOpts): AsyncGenerator<ReplEvent>;
  /** Load a DML file into the living module. */
  load(file: string): Promise<{ error?: string; warnings?: string[] }>;
  /** Read-only listing of the living module. */
  list(filter?: string): Promise<string>;
  /** Replace the human-in-the-loop channel (used by the CLI's shared readline). */
  setOnInput(handler: ((prompt: string, options?: string[]) => Promise<string | undefined>) | undefined): void;
  /** Read-only lock (M2). */
  freeze(): Promise<void>;
  /** Irreversible freeze: `unfreeze.` is refused (--locked). */
  lock(): Promise<void>;
  unfreeze(): Promise<void>;
  /** Write the harness to a self-contained .dml file (M2). */
  export(file?: string, options?: HarnessExportOptions): Promise<HarnessExportResult>;
  isFrozen(): boolean;
  reset(): Promise<void>;
  /** Undo the last harness transaction (requires history). */
  rollback(): Promise<RollbackResult>;
  /** Compact the session (meta) memory (TypeScript, ported from pi). */
  compact(customInstructions?: string): Promise<{ applied: boolean; beforeTokens: number; afterTokens: number; error?: string }>;
  /** Generate a diagram of an entry predicate (Mermaid → ASCII tree + SVG). */
  diagram(entry?: string, mode?: DiagramMode, options?: { signal?: AbortSignal }): Promise<DiagramResult>;
  /** Deliver a steering message into the running authoring turn. */
  steer(text: string): void;
  /** All persisted events from a resumed session, oldest first (resumed sessions only). */
  resumedTranscript(limit?: number): Promise<DMLEvent[]>;
  dispose(): Promise<void>;
  /** The underlying session (used by the CLI for session id / frame kind). */
  readonly session: DMLSession;
}

class HarnessReplImpl implements HarnessRepl {
  readonly session: DMLSession;

  constructor(session: DMLSession) {
    this.session = session;
  }

  setOnInput(handler: ((prompt: string, options?: string[]) => Promise<string | undefined>) | undefined): void {
    this.session.setInputHandler(handler);
  }

  query(goal: string, opts?: RunOpts): AsyncGenerator<DMLQueryEvent> {
    return this.session.query(goal, opts);
  }

  prompt(request: string, opts?: RunOpts): AsyncGenerator<ReplEvent> {
    return this.session.prompt(request, opts);
  }

  load(file: string): Promise<{ error?: string; warnings?: string[] }> {
    return this.session.load(file);
  }

  list(filter = 'all'): Promise<string> {
    return this.session.listing(filter);
  }

  freeze(): Promise<void> {
    return this.session.freeze();
  }

  lock(): Promise<void> {
    return this.session.lock();
  }

  unfreeze(): Promise<void> {
    return this.session.unfreeze();
  }

  export(file?: string, options?: HarnessExportOptions): Promise<HarnessExportResult> {
    return this.session.export(file, options);
  }

  isFrozen(): boolean {
    return this.session.isFrozen();
  }

  reset(): Promise<void> {
    return this.session.reset();
  }

  rollback(): Promise<RollbackResult> {
    return this.session.rollback();
  }

  compact(customInstructions?: string): Promise<{ applied: boolean; beforeTokens: number; afterTokens: number; error?: string }> {
    return this.session.compact(customInstructions);
  }

  diagram(entry?: string, mode?: DiagramMode, options?: { signal?: AbortSignal }): Promise<DiagramResult> {
    return this.session.diagram(entry, mode, options);
  }

  steer(text: string): void {
    this.session.steer(text);
  }

  resumedTranscript(limit?: number): Promise<DMLEvent[]> {
    return this.session.resumedTranscript(limit);
  }

  dispose(): Promise<void> {
    return this.session.dispose();
  }
}

export async function createHarnessRepl(options: HarnessReplOptions): Promise<HarnessRepl> {
  const { selection: judgeSelection, warnings } = resolveReplJudgeSelection({
    judgeSelection: options.judgeSelection,
    llmBackend: options.llmBackend,
    model: options.model,
    provider: options.provider,
    temperature: options.temperature ?? 0.7,
    maxTokens: options.maxTokens ?? 65536,
    defaultJudge: options.defaultJudge,
    jev: options.jev,
  });
  for (const warning of warnings) {
    options.onDiagnostic?.(warning);
  }

  const limitedBackend = options.llmBackend
    ? createLimitedBackend(options.llmBackend, options.providerConcurrency ?? 4)
    : undefined;

  const session = await DMLSession.open({
    sessionId: options.sessionId,
    resume: options.resume,
    workspacePath: options.workspacePath,
    params: options.params,
    gasLimit: options.gasLimit,
    initialHarnessMemory: options.initialHarnessMemory,
    tools: options.tools,
    authoringTools: options.authoringTools,
    toolPolicy: options.toolPolicy,
    onInputRequired: options.onInputRequired,
    onInput: options.onInput,
    onForm: options.onForm,
    maxDepth: options.maxDepth,
    maxModelCalls: options.maxModelCalls,
    hostToolsCatalog: options.hostToolsCatalog,
    agentSystemPrompt: options.agentSystemPrompt,
    workMode: options.workMode,
    authoringToolNames: options.authoringToolNames,
    executionBackend: options.executionBackend,
    capabilityTool: options.capabilityTool,
    processWorkerFactory: options.processWorkerFactory,
    beforeProcessStart: options.beforeProcessStart,
    beforeProcessSpawn: options.beforeProcessSpawn,
    createProcessSession: options.createProcessSession,
    askUser: options.askUser,
    history: options.history,
    harnessName: options.harness,
    autoLoadHarness: options.autoLoadHarness,
    execution: options.execution,
    compaction: options.compaction,
    contextWindow: options.contextWindow,
    runnerOptions: {
      model: options.model,
      provider: options.provider,
      temperature: options.temperature ?? 0.7,
      maxTokens: options.maxTokens ?? 65536,
      streaming: options.streaming ?? true,
      llmBackend: limitedBackend,
      judgeSelection,
      imageBackend: options.imageBackend,
      modelResolver: options.modelResolver,
    },
  });
  return new HarnessReplImpl(session);
}

export type { DMLSession, DMLQueryOptions, FrameKind };
