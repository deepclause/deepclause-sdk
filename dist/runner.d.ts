/**
 * DML Runner - Executes DML code using SWI-Prolog WASM
 */
import type { SWIPLModule } from './prolog/loader.js';
import type { DMLEvent, ToolDefinition, ToolPolicy, MemoryMessage } from './types.js';
export interface RunnerOptions {
    model: string;
    provider: string;
    temperature: number;
    maxTokens: number;
    baseUrl?: string;
    trace?: boolean;
    streaming?: boolean;
    debug?: boolean;
}
export interface InternalRunOptions {
    args?: unknown[];
    params?: Record<string, unknown>;
    workspacePath?: string;
    tools: Map<string, ToolDefinition>;
    toolPolicy: ToolPolicy | null;
    onInputRequired: (prompt: string) => Promise<string>;
    signal?: AbortSignal;
}
/**
 * DML execution engine
 *
 * ARCHITECTURE NOTE:
 * DML-defined tools now run in ISOLATED Prolog engines, not sharing state
 * with the parent task. This simplifies parallel tool execution (AI SDK
 * may call multiple tools concurrently) and eliminates race conditions.
 */
export declare class DMLRunner {
    private swipl;
    private options;
    private engine;
    private sessionId;
    private currentMemory;
    constructor(swipl: SWIPLModule, options: RunnerOptions);
    /**
     * Get the current conversation memory
     */
    getMemory(): MemoryMessage[];
    /**
     * Run DML code and yield events
     */
    run(code: string, options: InternalRunOptions): AsyncGenerator<DMLEvent>;
    /**
     * Parse DML code into Prolog clauses
     */
    private parseCode;
    /**
     * Build args list for Prolog (positional arguments for agent_main)
     */
    private buildArgs;
    /**
     * Build params dictionary for Prolog (named parameters)
     */
    private buildParams;
    /**
     * Convert JS value to Prolog term string
     */
    private toPrologTerm;
    /**
     * Create cooperative execution engine
     */
    private createEngine;
    /**
     * Step the cooperative engine
     */
    private stepEngine;
    /**
     * Handle agent loop request (task() predicate)
     */
    private handleAgentLoop;
    /**
     * Handle exec request (exec() predicate)
     */
    private handleExec;
    /**
     * Convert positional args to object based on tool schema
     */
    private argsToObject;
    /**
     * Parse user tool info from Prolog dict
     */
    private parseUserToolInfo;
    /**
     * Build tools available to agent loop
     * Only includes tools defined in DML - registered TypeScript tools are NOT exposed to the LLM.
     * DML tools can internally call exec/2 to use registered tools.
     */
    private buildAgentTools;
    /**
     * Execute a DML-defined tool in isolation using its own engine
     *
     * SIMPLIFIED: Tools now run in their own isolated Prolog engine, not sharing
     * state with the parent task. This eliminates:
     * - Race conditions from parallel tool calls
     * - Complex signal-based coordination
     * - Mutex requirements
     *
     * The tool can still call exec() for external tools (TypeScript registered tools).
     * We step the tool engine and handle request_exec signals.
     *
     * @param excludedTools - Tools currently on the call stack (for recursion prevention)
     * @param onToolCall - Callback to emit tool call events from nested tasks
     */
    private executeToolIsolated;
    /**
     * Extract memory from payload (now passed via state threading)
     */
    private extractMemoryFromPayload;
    /**
     * Post agent loop result back to Prolog
     */
    private postAgentResult;
    /**
     * Post exec result back to Prolog
     */
    private postExecResult;
    /**
     * Post tool engine agent loop result back to Prolog
     */
    private postToolAgentResult;
    /**
     * Provide user input to waiting Prolog
     */
    private provideInput;
    /**
     * Execute a Prolog query
     */
    private query;
    /**
     * Cleanup resources
     */
    private cleanup;
    /**
     * Dispose runner
     */
    dispose(): Promise<void>;
}
//# sourceMappingURL=runner.d.ts.map