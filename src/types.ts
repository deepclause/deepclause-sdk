/**
 * Type definitions for DeepClause SDK
 */

import { z } from 'zod';
import type { JudgeBackend, JudgeCapabilities } from './judge/types.js';
import type { FormIssue, FormSpec, JsonObject } from './forms/spec.js';

export type {
  JudgeAnswer,
  JudgeBackend,
  JudgeBackendRequest,
  JudgeBackendResponse,
  JudgeBasis,
  JudgeCapabilities,
  JudgeLevel,
  JudgeOption,
  JudgeQuestion,
  JudgeQuestionKind,
  JudgeSelection,
  JudgeUsage,
  JsonValue,
} from './judge/types.js';

/**
 * Options for creating a DeepClause SDK instance
 */
export interface CreateOptions {
  /** API key for the LLM provider */
  apiKey?: string;
  /** LLM model to use (e.g., 'gpt-4o', 'claude-3-sonnet') */
  model: string;
  /** LLM provider (auto-detected from model if not specified) */
  provider?: 'openai' | 'anthropic' | 'google' | 'openrouter';
  /** Temperature for LLM calls (0-1) */
  temperature?: number;
  /** Maximum tokens for LLM responses */
  maxTokens?: number;
  /** Base URL for API (for custom endpoints) */
  baseUrl?: string;
  /** Enable execution trace logging */
  trace?: boolean;
  /** Enable streaming for LLM responses (task/prompt predicates) */
  streaming?: boolean;
  /** Enable debug logging (shows prompts, tool calls, etc.) */
  debug?: boolean;
  /** Provider-specific options passed to the AI SDK (e.g. Google thinkingConfig, OpenAI reasoningEffort) */
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  providerOptions?: Record<string, Record<string, any>>;
  /** Reasoning effort type (auto-resolved from model database) */
  reasoningType?: import('./system/config/model-database.js').ReasoningType;
  /** Reasoning budget map for token-based reasoning control */
  reasoningBudgetMap?: Record<string, number>;
  /** Model context window in tokens (from model database) */
  contextWindow?: number;
  /** Host-provided LLM backend. When set, no provider credentials or environment variables are used. */
  llmBackend?: LLMBackend;
  /** Default judgment backend. When omitted, an LLM judge backend is created from the model options. */
  judgeBackend?: JudgeBackend;
  /** Named judgment backends selectable from DML with with_judgment/2. */
  judgeBackends?: Record<string, JudgeBackend>;
  /** Name of the default judgment backend. Defaults to the first registered backend. */
  defaultJudge?: string;
}

/** A workspace-relative media reference (never the bytes). */
export interface MediaRef {
  path: string;
  mimeType: string;
}

/**
 * A parsed `model_spec` from a `with_model/2` scope: a provider+id with optional
 * temperature/maxTokens/reasoning overrides. `default`/`none` means "no override".
 */
export type ModelSpec =
  | 'default'
  | {
      provider?: string;
      id: string;
      temperature?: number;
      maxTokens?: number;
      /** Reasoning level (`off|minimal|low|medium|high|xhigh|max`). */
      reasoning?: string;
    };

/** A fully resolved model profile used for one call. */
export interface ResolvedModel {
  provider: string;
  id: string;
  temperature?: number;
  maxTokens?: number;
  /** Context window, for nested compaction thresholds. */
  contextWindow?: number;
  /** Whether the model supports a reasoning/thinking mode. */
  reasoning: boolean;
  /** Requested reasoning level for this scope, if any. */
  reasoningLevel?: string;
  /** For the env backend: how reasoning maps to provider options. */
  reasoningType?: 'effort' | 'budget_tokens' | 'thinking_config' | 'none';
  reasoningBudgetMap?: Record<string, number>;
  /** For the pi backend: thinking levels the model accepts. */
  supportedThinkingLevels?: string[];
}

/** Request passed to an injected image-generation backend (see `task/…, media(X))`). */
export interface ImageGenerationRequest {
  /** The prompt (the task description, interpolated). */
  prompt: string;
  /** Attached input images, for image-to-image. */
  refs?: MediaRef[];
  /** Optional square/edge size in px. */
  size?: number;
  signal?: AbortSignal;
}

export interface ImageGenerationResult {
  /** Workspace-relative path of the written image. */
  path: string;
  mimeType: string;
}

/**
 * A host image generator. The pi backend implements this on top of pi's image
 * models and credential store; tests inject a canned one.
 */
export interface ImageBackend {
  generate(request: ImageGenerationRequest): Promise<ImageGenerationResult>;
}

/** Resolved media bytes attached to a backend message (base64). */
export interface LLMBackendMedia {
  mimeType: string;
  data: string;
}

export interface LLMBackendMessage {
  role: 'system' | 'user' | 'assistant' | 'tool';
  content: string;
  /** Resolved media bytes to send with this message. */
  media?: LLMBackendMedia[];
  /** Reference-only media carried through to conversation memory. */
  mediaRefs?: MediaRef[];
  toolCalls?: LLMBackendToolCall[];
  toolCallId?: string;
  toolName?: string;
  /** Opaque host-native message used to preserve provider state across backend calls. */
  providerData?: unknown;
}

export interface LLMBackendTool {
  name: string;
  description: string;
  parameters: Record<string, unknown>;
}

export interface LLMBackendToolCall {
  id: string;
  name: string;
  arguments: Record<string, unknown>;
}

export interface LLMBackendRequest {
  messages: LLMBackendMessage[];
  tools?: LLMBackendTool[];
  temperature?: number;
  maxTokens?: number;
  signal?: AbortSignal;
  /** Receives model text as it becomes available. Backends may emit one final chunk. */
  onText?: (chunk: string) => void;
  /** Receives reasoning/thinking text as it becomes available (display only). */
  onReasoning?: (chunk: string) => void;
  /** Per-call model override from `with_model/2`. */
  target?: ResolvedModel;
}

export interface LLMBackendResponse {
  text: string;
  toolCalls?: LLMBackendToolCall[];
  usage?: LLMUsage;
  finishReason?: 'stop' | 'length' | 'tool_use' | 'error' | 'aborted';
  /** Accumulated reasoning/thinking text (display only; not written to DML memory). */
  reasoning?: string;
  /** Opaque host-native assistant message to replay on a subsequent backend call. */
  providerData?: unknown;
}

export interface LLMBackend {
  /**
   * Change the reasoning level of the active model, when the backend supports it.
   * Optional: a backend that cannot change it simply omits this.
   */
  setThinkingLevel?(level: string): void;
  getThinkingLevel?(): string | undefined;
  supportedThinkingLevels?(): string[];
  complete(request: LLMBackendRequest): Promise<LLMBackendResponse>;
}

export type CompactionScope = 'session' | 'loop' | 'run';
export type CompactionTrigger = 'before_user_message' | 'before_model_call' | 'before_task' | 'after_task';
export type CompactionAction = 'running' | 'applied' | 'skipped' | 'failed';

/**
 * Options for running DML code
 */
export interface RunOptions {
  /** Positional arguments for agent_main (argv-style) */
  args?: unknown[];
  /** Named parameters available via param/2 and string interpolation */
  params?: Record<string, unknown>;
  /** Path to workspace directory */
  workspacePath?: string;
  /** Per-run default judgment backend name override. */
  judgeBackend?: string;
  /** Maximum number of execution steps (gas) */
  gasLimit?: number;
  /** Handler for user input requests */
  onUserInput?: (prompt: string) => Promise<string>;
  /** Abort signal for cancellation */
  signal?: AbortSignal;
  /** Initial conversation messages seeded into memory before DML execution.
    *  These appear as proper memory messages, not in the synthesized system prompt. */
    initialMessages?: MemoryMessage[];
  /** Called when a form opens, so an interactive host can collect values. */
  onForm?: (form: FormEventPayload) => void | Promise<void>;
}

/**
 * Trace entry for execution logging
 */
export interface TraceEntry {
  /** Timestamp in milliseconds */
  timestamp: number;
  /** Type of trace event */
  type: 'call' | 'exit' | 'fail' | 'output' | 'llm_call' | 'exec';
  /** Predicate or operation name */
  predicate?: string;
  /** Arguments or details */
  args?: unknown[];
  /** Result or additional info */
  result?: unknown;
  /** Call depth for indentation */
  depth?: number;
}

/**
 * Events emitted during DML execution
 */
/**
 * Token usage data from a single LLM call.
 */
export interface LLMUsage {
  inputTokens: number;
  outputTokens: number;
  totalTokens: number;
  cacheReadTokens?: number;
  cacheWriteTokens?: number;
  reasoningTokens?: number;
}

/**
 * A single rendered variable binding from a goal-mode solution.
 * `value` is the Prolog text of the binding ('_' when unbound).
 */
export interface DMLBinding {
  name: string;
  value: string;
}

/**
 * One goal-mode answer. A goal with N solutions yields N solution events.
 * `deterministic` is an optimization hint only; the pager can always offer
 * the next solution.
 */
export interface DMLSolution {
  bindings: DMLBinding[];
  deterministic: boolean;
}

/** Frame metadata attached to events emitted by a nested/reentrant run. */
export interface FrameEventMeta {
  frameId?: string;
  depth?: number;
  frameKind?: FrameKind;
}

/** What a consumer sees when a form is opened, updated or closed. */
export interface FormEventPayload {
  op: 'open' | 'update' | 'close';
  formId: string;
  revision: number;
  spec?: FormSpec;
  values?: JsonObject;
  patch?: JsonObject;
  issues?: FormIssue[];
  state?: 'open' | 'submitted' | 'cancelled';
  reason?: string;
  source?: { name?: string; pid?: string };
}

/** A turn stopped at a safe point, with the goal that resumes it. */
export interface SuspendEventPayload {
  /** The goal to run to continue where the turn stopped, e.g. `loop(3)`. */
  continuation: string;
  /** Turn memory at the stop, so a resume continues the same conversation. */
  memory?: string;
}

/** What a consumer sends back when the user acts on a form. */
export interface FormSubmitPayload {
  action: string;
  values?: JsonObject;
  revision?: number;
  errors?: FormIssue[];
}

export interface DMLEvent extends FrameEventMeta {
  type: 'output' | 'log' | 'diagnostic' | 'answer' | 'input_required' | 'error' | 'finished' | 'stream' | 'reasoning' | 'tool_call' | 'model_call' | 'usage' | 'memory_compaction' | 'task_activity' | 'solution' | 'meta_result' | 'media' | 'html' | 'form' | 'suspend' | 'workflow_result';
  content?: string;
  /**
   * Severity of a `log` event. Prolog's own stderr arrives this way; `warning`
   * and `error` are shown even without `--verbose`, because a deprecated or
   * failing construct is not low-level noise the user opted out of seeing.
   */
  level?: 'info' | 'warning' | 'error';
  /** Which stream a diagnostic came from (only for Prolog diagnostics). */
  channel?: 'stdout' | 'stderr';
  /** Media produced by a step (path in the workspace, optional inline base64). */
  media?: { path?: string; mimeType: string; data?: string; kind?: string };
  /** A declared workflow's truthful, typed terminal result. */
  workflow?: { name: string; result: import('./work/index.js').WorkResult; harness: string; hash: string; version?: number };
  /** Workspace HTML miniapp produced by `output(html(Path))`. */
  html?: { path: string };
  /** Form interaction (only for 'form' events). See docs/DML_FORMS.md. */
  form?: FormEventPayload;
  /** A turn stopped at a safe point (only for 'suspend' events). */
  suspend?: SuspendEventPayload;
  /** Model-call lifecycle (only for 'model_call' events). */
  modelCallState?: 'started' | 'completed' | 'failed';
  /** Model id for a 'model_call' event, when known. */
  modelId?: string;
  /** Goal-mode solution (only for 'solution' events). */
  solution?: DMLSolution;
  /** Prompt-mode meta task result (only for 'meta_result' events). */
  metaResult?: { code: string; answer: string };
  prompt?: string;
  /** Execution trace (only present in 'finished' event when trace mode enabled) */
  trace?: TraceEntry[];
  /** Whether this is the final chunk of a stream (only for 'stream' events) */
  done?: boolean;
  /** Tool name (only for 'tool_call' events) */
  toolName?: string;
  /** Tool arguments (only for 'tool_call' events) */
  toolArgs?: Record<string, unknown>;
  /** Tool result (only for 'tool_call' events, set after execution) */
  toolResult?: unknown;
  /** Tool lifecycle state (only for 'tool_call' events) */
  toolState?: 'starting' | 'running' | 'completed' | 'failed';
  /** Process identifier when available (only for 'tool_call' events) */
  toolPid?: number;
  /** Tool execution backend when available (only for 'tool_call' events) */
  toolBackend?: 'host' | 'sandbox';
  /** Tool exit code when available (only for 'tool_call' events) */
  toolExitCode?: number;
  /** Wall-clock duration of the tool call in ms (only for completed/failed 'tool_call' events) */
  toolDurationMs?: number;
  /** Tool summary message when available (only for 'tool_call' events) */
  toolSummary?: string;
  /** Tool failure message when available (only for 'tool_call' events) */
  toolError?: string;
  /** Token usage from an LLM call (only for 'usage' events) */
  usage?: LLMUsage;
  /** Compaction scope (only for 'memory_compaction' events) */
  compactionScope?: CompactionScope;
  /** Compaction trigger (only for 'memory_compaction' events) */
  compactionTrigger?: CompactionTrigger;
  /** Compaction action outcome (only for 'memory_compaction' events) */
  compactionAction?: CompactionAction;
  /** Estimated tokens before compaction (only for 'memory_compaction' events) */
  beforeTokens?: number;
  /** Estimated tokens after compaction (only for 'memory_compaction' events) */
  afterTokens?: number;
  /** Optional failure reason when compaction could not be applied */
  compactionError?: string;
  /** Step lifecycle state (only for 'task_activity' events) */
  taskState?: 'started' | 'completed' | 'failed';
  /** Step description (only for 'task_activity' started events) */
  taskDescription?: string;
  /** Correlation ID for matching start/end of a step (only for 'task_activity' events) */
  taskId?: string;
  /** Judgment backend name (only for judge activity events) */
  judgeBackend?: string;
  /** Judged question ids (only for judge activity events) */
  judgeQuestionIds?: string[];
  /** Judgment step lifecycle state (only for judge activity events) */
  judgeState?: 'started' | 'completed' | 'failed';
  /** Which subsystem produced a usage event. */
  usageSource?: 'llm' | 'judge';
  /** Model that produced a usage event. */
  usageModel?: string;
}

/**
 * Tool definition for registering external tools
 */
/** Per-call context handed to a tool's `execute`. */
export interface ToolExecutionContext {
  /** Abort signal for the current turn; long-running tools should honor it. */
  signal?: AbortSignal;
  /** Stream events produced while the tool runs (e.g. an `execute` child frame). */
  onEvent?: (event: DMLEvent) => void;
}

export interface ToolDefinition {
  /** Human-readable description of what the tool does */
  description: string;
  /** JSON Schema for tool parameters */
  parameters: JsonSchema;
  /** Function to execute the tool */
  execute: (args: Record<string, unknown>, context?: ToolExecutionContext) => Promise<unknown>;
}

/**
 * JSON Schema type for tool parameters
 */
export interface JsonSchema {
  type: 'object';
  additionalProperties?: boolean;
  properties: Record<string, {
    type: string;
    description?: string;
    enum?: string[];
  }>;
  required?: string[];
}

/**
 * Tool access control policy
 */
export interface ToolPolicy {
  /** Whether to whitelist or blacklist tools */
  mode: 'whitelist' | 'blacklist';
  /** Tool names to allow/deny (supports wildcards like 'file_*') */
  tools: string[];
}

/**
 * Main SDK interface
 */
export interface DeepClauseSDK {
  /**
   * Execute DML code
   * @param code - DML/Prolog source code
   * @param options - Execution options
   * @yields DML events (output, log, answer, etc.)
   */
  runDML(code: string, options?: RunOptions): AsyncGenerator<DMLEvent>;

  /**
   * Register an external tool
   * @param name - Tool name (used in exec())
   * @param tool - Tool definition
   */
  registerTool(name: string, tool: ToolDefinition): void;

  /**
   * Send a form action (the UI's half of `docs/DML_FORMS.md`). Resolves the
   * `await_form`/`wait_for_form_change` that is waiting on the form.
   * @returns whether a form was listening
   */
  submitForm(formId: string, change: FormSubmitPayload): boolean;

  /**
   * Set tool access policy
   * @param policy - Whitelist/blacklist configuration
   */
  setToolPolicy(policy: ToolPolicy): void;

  /**
   * Clear tool policy (allow all tools)
   */
  clearToolPolicy(): void;

  /**
   * Get current tool policy
   */
  getToolPolicy(): ToolPolicy | null;

  /**
   * Get list of registered tool names
   */
  getTools(): string[];

  /**
   * Provide input in response to input_required event
   * @param input - User's input string
   */
  provideInput(input: string): void;

  /**
   * Get the current conversation memory
   * @returns Array of memory messages (system, user, assistant)
   */
  getMemory(): MemoryMessage[];

  /** Register or replace a named judgment backend. */
  registerJudgeBackend(name: string, backend: JudgeBackend): void;

  /** Select the default judgment backend by name. */
  setJudgeBackend(name: string): void;

  /** List registered judgment backend names. */
  getJudgeBackends(): string[];

  /** Get the capabilities of a judgment backend (default when omitted). */
  getJudgeCapabilities(name?: string): JudgeCapabilities;

  /**
   * Clean up resources
   */
  dispose(): Promise<void>;
}

/**
 * Kind of execution frame in the runner's reentrant frame stack.
 *
 * - `query`  — the default engine created by `runDML` (one solution / agent_main)
 * - `nested` — an `execute` child frame run by the authoring agent
 * - `object` — a harness run after a prompt commit (runtime tool policy)
 */
export type FrameKind = 'query' | 'nested' | 'object';

/**
 * Variable with type information for type-safe LLM outputs
 */
export interface SchemaField {
  name: string;
  type: string;
  /** For `list(T)` fields: the element type. */
  itemType?: string;
  /** For a nested `schema(Name)` (or a list of one): its fields. */
  fields?: SchemaField[];
  required: boolean;
  /** Absent when the field declares no default. */
  default?: unknown;
}

export interface ShellWorkflowArg {
  name: string;
  type: string;
}

export interface ShellWorkflow {
  name: string;
  goal: string;
  inputs: ShellWorkflowArg[];
  outputs: ShellWorkflowArg[];
}

/** A DML predicate exposed to the Work agent as a real tool. */
export interface ShellTool extends ShellWorkflow {
  description: string;
}

/** `shell/1` (or `shell/3`) presentation contract for Work mode. */
export interface ShellSpec {
  kind: 'agent' | 'workflow';
  system: string;
  hostTools: string[];
  /** Agent-facing capabilities. Not directly runnable from the UI. */
  tools: ShellTool[];
  /** User-facing recurring tasks. */
  workflows: ShellWorkflow[];
  welcome: string;
}

export interface TypedVar {
  name: string;
  type: 'string' | 'number' | 'integer' | 'boolean' | 'array' | 'object' | 'media';
  itemType?: TypedVar['type']; // For arrays
  /** Name of the declared `schema/2`, when the output names one. */
  schema?: string;
  /** The schema's fields, resolved at run time (see `decodeAgentOutputVars`). */
  fields?: SchemaField[];
}

/**
 * Internal types for Prolog bridge communication
 */
export interface PrologRequest {
  type: 'agent_loop' | 'exec' | 'wait_input';
  payload: unknown;
}

export interface PrologResponse {
  type: 'output' | 'log' | 'answer' | 'request' | 'finished' | 'error';
  content?: string;
  request?: PrologRequest;
}

/**
 * Memory message structure
 */
export interface MemoryMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
  /** Reference-only media attached to this message (resolved at call time). */
  media?: MediaRef[];
}

/**
 * Agent loop tool definitions
 */
export interface AgentTool {
  name: string;
  description: string;
  parameters: z.ZodSchema;
}
