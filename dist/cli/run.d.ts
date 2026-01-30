/**
 * DeepClause CLI - Execution Module
 *
 * Executes compiled DML programs with full tool support.
 */
import type { DMLEvent } from '../types.js';
import { type Provider } from './config.js';
export interface RunOptions {
    workspace?: string;
    verbose?: boolean;
    stream?: boolean;
    headless?: boolean;
    trace?: string;
    dryRun?: boolean;
    model?: string;
    provider?: Provider;
    params?: Record<string, string>;
}
export interface RunResult {
    output: string[];
    answer?: string;
    error?: string;
    dryRun?: boolean;
    wouldExecute?: string;
    trace?: object;
    events?: DMLEvent[];
}
/**
 * Execute a compiled DML program
 */
export declare function run(file: string, args: string[], options?: RunOptions): Promise<RunResult>;
//# sourceMappingURL=run.d.ts.map