/**
 * DeepClause CLI - Execution Module
 *
 * Executes compiled DML programs with full tool support.
 */
import { type Provider } from './config.js';
import type { DMLEvent } from '../types.js';
export interface RunOptions {
    configRoot?: string;
    workspace?: string;
    verbose?: boolean;
    stream?: boolean;
    headless?: boolean;
    sandbox?: boolean;
    trace?: string;
    dryRun?: boolean;
    model?: string;
    provider?: Provider;
    temperature?: number;
    audit?: boolean;
    gasLimit?: number;
    signal?: AbortSignal;
    toolAbortSignalRef?: {
        signal?: AbortSignal;
    };
    params?: Record<string, string>;
    prompt?: string;
    onUserInput?: (prompt: string) => Promise<string>;
    onEvent?: (event: DMLEvent) => void;
    onChildEvent?: (childSlug: string, event: DMLEvent) => void;
}
export interface RunResult {
    output: string[];
    answer?: string;
    error?: string;
    dryRun?: boolean;
    wouldExecute?: string;
    trace?: object;
    events?: DMLEvent[];
    usageByModel?: Record<string, {
        calls: number;
        inputTokens: number;
        outputTokens: number;
        totalTokens: number;
        cacheReadTokens: number;
        cacheWriteTokens: number;
        reasoningTokens: number;
    }>;
}
/**
 * Execute a compiled DML program or generate and run DML from a prompt
 */
export declare function run(file: string | undefined, args: string[], options?: RunOptions): Promise<RunResult>;
//# sourceMappingURL=run.d.ts.map