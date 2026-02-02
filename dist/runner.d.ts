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
 */
export declare class DMLRunner {
    private swipl;
    private options;
    private engine;
    private sessionId;
    private currentMemory;
    private toolMutexLocked;
    private toolMutexQueue;
    private toolExecutionDepth;
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
     * Acquire the tool execution mutex
     * Returns a release function to call when done
     */
    private acquireToolMutex;
    /**
     * Execute a tool inline via the main Prolog engine
     * Posts execute_tool signal, handles yields (output, exec requests), returns tool_result
     * Tools now share state with the task() that calls them.
     * Uses a mutex to serialize concurrent tool calls (AI SDK may execute multiple tools in parallel).
     */
    private executeToolInline;
    /**
     * Internal implementation of executeToolInline (called with mutex held)
     */
    private executeToolInlineImpl;
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
     * Post exec_done signal after tool execution (from exec/2 in DML).
     * Uses post_signal_to_engine helper which has fallback to session_pending_signal.
     */
    private postExecDone;
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