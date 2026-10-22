/**
 * DeepClause SDK - Simplified DML Language Runtime
 * 
 * A neurosymbolic AI system combining Prolog-based symbolic reasoning
 * with LLM-powered task execution.
 */

export { createDeepClause } from './sdk.js';
// Engine host seam: lets a non-Node embedding (the browser) supply the SWI-Prolog
// loader and workspace mount. See docs/DML_FORMS.md-adjacent embedding notes.
export { setPrologHost, getPrologHost, recordPrologMessage } from './prolog/loader.js';
// Form spec/types/validators: reusable by a host renderer (the browser UI).
export * from './forms/index.js';
export type { PrologHost } from './prolog/loader.js';
export type { 
  CompactionAction,
  CompactionScope,
  CompactionTrigger,
  CreateOptions,
  DeepClauseSDK,
  LLMBackend,
  LLMBackendMessage,
  LLMBackendRequest,
  LLMBackendResponse,
  LLMBackendTool,
  LLMBackendToolCall,
  LLMUsage,
  MemoryMessage,
  RunOptions,
  DMLEvent,
  TraceEntry,
  ToolDefinition,
  ToolPolicy,
  FormEventPayload,
  FormSubmitPayload,
  ShellSpec,
  ShellTool,
  ShellWorkflow,
  ShellWorkflowArg,
} from './types.js';

// DML REPL (goal + prompt mode) for CLI and embedding
export { createHarnessRepl } from './repl/index.js';
export { DMLSession } from './repl/session.js';export type { HarnessRepl, HarnessReplOptions, RunOpts, DMLQueryEvent, ReplEvent, RollbackResult } from './repl/index.js';
export type { DMLSessionOptions, DMLQueryOptions, ProcessSession } from './repl/session.js';
export { formatListing, highlightProlog, reflowListing } from './repl/highlight.js';

// TypeScript authoring agent
export { HarnessAgent, buildAgentSystemPrompt, describeTools } from './repl/agent.js';
export type { HarnessAgentOptions, HarnessAgentRunOptions } from './repl/agent.js';
export { createHarnessTools } from './repl/harness-tools.js';
export { loadProjectContext } from './repl/project-context.js';
export type { ProjectContext } from './repl/project-context.js';
export type {
  HarnessToolController,
  HarnessExecuteResult,
  HarnessCommitResult,
} from './repl/harness-tools.js';

// RepL status accounting (status bar) and TypeScript compaction (ported from pi)
export {
  ReplStatusTracker,
  formatStatusLine,
  formatTokens,
  formatUsageSegment,
  formatContextSegment,
  formatModelSegment,
} from './repl/status.js';
export type {
  ReplStatusSnapshot,
  ReplStatusTrackerOptions,
  StatusModelInfo,
  StatusContextUsage,
  StatusCompactionInfo,
} from './repl/status.js';
export {
  DEFAULT_COMPACTION_SETTINGS,
  SUMMARY_PREFIX,
  buildSummaryMessage,
  buildSummarizationPrompt,
  calculateContextTokens,
  compactMemory,
  estimateMemoryTokens,
  findCompactionCut,
  findExistingSummary,
  generateMemorySummary,
  serializeMemoryConversation,
  shouldCompact,
  toCompactionEvent,
} from './repl/compaction.js';
export type {
  CompactionSettings,
  CompactMemoryResult,
  CompactMemoryReason,
  MemoryCompactionEvent,
} from './repl/compaction.js';
export type { HarnessExportOptions, HarnessExportResult } from './repl/export.js';
// L2 process worker body, for hosts that run it outside `worker_threads`.
export { startProcessWorker } from './repl/process-worker.js';
export type { ProcessWorkerPort } from './repl/process-worker.js';
export { createPiBackend, createPiAdapter, resolvePiPaths, selectPiModel } from './repl/backend.js';
export { createReplToolset, buildToolCatalog } from './repl/toolset.js';
export { pickFromList, filterItems, renderPick } from './repl/picker.js';
export type { PickItem, PickOptions } from './repl/picker.js';
export {
  apiKeyInteraction,
  createTerminalAuthInteraction,
  listLoginProviders,
  loginToProvider,
  logoutProvider,
} from './repl/auth.js';
export type { LoginProviderOption } from './repl/auth.js';
export { HistoryStore } from './repl/history.js';
export type {
  HistoryOptions,
  HistoryTrace,
  HistoryTurnRecord,
  HistoryChangeRecord,
  HistoryChangedPredicate,
  HistoryEventContext,
} from './repl/history.js';

// DML validation and linting — also available via 'deepclause-sdk/validate'
export { validateWithProlog } from './validate.js';
export type { ValidationResult } from './validate.js';

// Judgment layer: generic judge primitives with pluggable backends
export {
  createMockJudgeBackend,
  createMockJevJudgeBackend,
  createJevJudgeBackend,
  createLLMJudgeBackend,
  buildJudgePrompt,
  parseJudgeResponse,
} from './judge/index.js';
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
  JevJudgeBackendOptions,
  LLMJudgeBackendOptions,
  MockAnswerSource,
  MockJudgeBackendOptions,
} from './judge/index.js';

// Session-scoped REPL processes: the L1 core and the L1↔L2 seam.
// See docs/DML_REPL_PROCESSES_SPEC.md and docs/DML_REPL_PROCESSES_IMPLEMENTATION_PLAN.md.
export {
  LocalProcessHost,
  ProcessExit,
  NoProcError,
  MailboxFullError,
  labelForGoal,
  copyMessage,
  pidKey,
} from './repl/processes.js';
export type {
  Pid,
  NodeId,
  ProcessInfo,
  ProcessState,
  ProcessPriority,
  ProcessEvent,
  ProcessContext,
  ProcessBody,
  ProcessHostOptions,
  SpawnOptions,
  ReceiveResult,
  CapabilityBroker,
  CapabilityContext,
} from './repl/processes.js';
