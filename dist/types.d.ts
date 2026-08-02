/**
 * Type definitions for DeepClause SDK
 */
import { z } from 'zod';
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
    providerOptions?: Record<string, Record<string, any>>;
    /** Optional memory compaction settings for task()/prompt() execution */
    compaction?: CompactionOptions;
    /** Reasoning effort type (auto-resolved from model database) */
    reasoningType?: import('./system/config/model-database.js').ReasoningType;
    /** Reasoning budget map for token-based reasoning control */
    reasoningBudgetMap?: Record<string, number>;
    /** Model context window in tokens (from model database) */
    contextWindow?: number;
}
export type CompactorSourceType = 'inline' | 'file' | 'auto';
export type CompactionScope = 'session' | 'loop' | 'run';
export type CompactionTrigger = 'before_user_message' | 'before_model_call' | 'before_task' | 'after_task';
export type CompactionAction = 'running' | 'applied' | 'skipped' | 'failed';
export interface CompactorDefinition {
    /** Inline DML source or a path to a DML file */
    source: string;
    /** How to interpret the source field. Defaults to 'auto'. */
    sourceType?: CompactorSourceType;
    /** Timeout for the DML compactor run in milliseconds. */
    timeoutMs?: number;
    /** Optional gas limit for the compactor DML run. */
    gasLimit?: number;
    /** Optional model override for compactor task()/prompt() calls. */
    model?: string;
    /** Optional provider override for the compactor model. */
    provider?: 'openai' | 'anthropic' | 'google' | 'openrouter';
    /** Whether the compactor may access the caller's registered exec() tools. Defaults to false. */
    inheritTools?: boolean;
    /** Optional tool policy applied to the compactor run. */
    toolPolicy?: ToolPolicy | null;
}
export interface CompactorBinding {
    /** Optional binding label used in logs and events. */
    name?: string;
    /** Runtime scope to which this compactor is attached. */
    scope: CompactionScope;
    /** Trigger point within the scope. */
    trigger: CompactionTrigger;
    /** The DML compactor to run when this binding is evaluated. */
    compactor: CompactorDefinition;
}
export interface CompactionOptions {
    /** Enables compactor evaluation. Defaults to false. */
    enabled?: boolean;
    /** Compactor bindings evaluated at specific runtime hook points. */
    bindings?: CompactorBinding[];
}
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
    /** Maximum number of execution steps (gas) */
    gasLimit?: number;
    /** Handler for user input requests */
    onUserInput?: (prompt: string) => Promise<string>;
    /** Abort signal for cancellation */
    signal?: AbortSignal;
    /** Initial conversation messages seeded into memory before DML execution.
      *  These appear as proper memory messages, not in the synthesized system prompt. */
    initialMessages?: MemoryMessage[];
    /** Per-run override for SDK compaction settings. */
    compaction?: CompactionOptions;
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
export interface DMLEvent {
    type: 'output' | 'log' | 'answer' | 'input_required' | 'error' | 'finished' | 'stream' | 'tool_call' | 'usage' | 'memory_compaction' | 'task_activity';
    content?: string;
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
    /** Binding label used for the compaction event when available */
    compactionBindingName?: string;
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
}
/**
 * Tool definition for registering external tools
 */
export interface ToolDefinition {
    /** Human-readable description of what the tool does */
    description: string;
    /** JSON Schema for tool parameters */
    parameters: JsonSchema;
    /** Function to execute the tool */
    execute: (args: Record<string, unknown>) => Promise<unknown>;
}
/**
 * Tool representation for the compiler
 */
export interface CompileTool {
    name: string;
    description: string;
    provider: string;
    schema?: object;
}
/**
 * Static Analysis Warning
 */
export interface AnalysisWarning {
    level: 'critical' | 'high' | 'medium' | 'low';
    message: string;
}
/**
 * Result of DML Analysis
 */
export interface AnalysisResult {
    valid: boolean;
    warnings: AnalysisWarning[];
    capabilities: string[];
    auditorReport?: string;
    error?: string;
}
/**
 * Options for DML compilation
 */
export interface CompileOptions {
    /** Model to use for compilation */
    model?: string;
    /** Provider for the model */
    provider?: 'openai' | 'anthropic' | 'google' | 'openrouter';
    /** Temperature for LLM calls (0-1) */
    temperature?: number;
    /** Maximum number of compilation attempts */
    maxAttempts?: number;
    /** Enable verbose logging during compilation */
    verbose?: boolean;
    /** Custom tools to describe to the compiler */
    tools?: CompileTool[];
    /** Run LLM-based security audit */
    audit?: boolean;
    /** Base URL for the LLM API (e.g. a proxy endpoint) */
    baseUrl?: string;
}
/**
 * Result of a DML compilation
 */
export interface CompileResult {
    /** Compiled DML code */
    dml: string;
    /** Tool dependencies extracted from the DML */
    tools: string[];
    /** Brief explanation of what the program does */
    explanation?: string;
    /** Number of attempts taken to compile */
    attempts?: number;
    /** Validation details */
    valid: boolean;
    errors?: string[];
    /** Static and Semantic Analysis results */
    analysis?: AnalysisResult;
    /** Token usage from LLM calls during compilation */
    usageByModel?: import('./system/runtime/token-usage.js').TokenUsageByModel;
}
/**
 * JSON Schema type for tool parameters
 */
export interface JsonSchema {
    type: 'object';
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
     * Compile Markdown task description to DML
     * @param source - Markdown content or path to .md file
     * @param options - Compilation options
     * @returns Compiled DML and metadata
     */
    compile(source: string, options?: CompileOptions): Promise<CompileResult>;
    /**
     * Register an external tool
     * @param name - Tool name (used in exec())
     * @param tool - Tool definition
     */
    registerTool(name: string, tool: ToolDefinition): void;
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
    /**
     * Clean up resources
     */
    dispose(): Promise<void>;
}
/**
 * Variable with type information for type-safe LLM outputs
 */
export interface TypedVar {
    name: string;
    type: 'string' | 'number' | 'integer' | 'boolean' | 'array' | 'object';
    itemType?: TypedVar['type'];
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
}
/**
 * Agent loop tool definitions
 */
export interface AgentTool {
    name: string;
    description: string;
    parameters: z.ZodSchema;
}
//# sourceMappingURL=types.d.ts.map