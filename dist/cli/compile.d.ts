/**
 * DeepClause CLI - Compilation Module
 *
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */
import { type Provider } from './config.js';
import type { DMLEvent } from '../types.js';
import { extractDescription as extractDescriptionFromCompiler, extractParameters as extractParametersFromCompiler, extractToolDependencies as extractToolDependenciesFromCompiler } from '../compiler.js';
import type { AnalysisResult } from '../types.js';
export interface CompileOptions {
    force?: boolean;
    validateOnly?: boolean;
    sandbox?: boolean;
    model?: string;
    provider?: Provider;
    temperature?: number;
    maxAttempts?: number;
    verbose?: boolean;
    stream?: boolean;
    audit?: boolean;
    headless?: boolean;
    trace?: string;
    signal?: AbortSignal;
    onEvent?: (event: DMLEvent) => void;
}
export interface CompileResult {
    output: string;
    tools: string[];
    skipped: boolean;
    valid: boolean;
    dml?: string;
    meta?: MetaFile;
    explanation?: string;
    attempts?: number;
    analysis?: AnalysisResult;
    runtimeOutput?: string[];
    trace?: object;
    events?: DMLEvent[];
}
export interface CompileAllResult {
    compiled: number;
    skipped: number;
    failed: number;
    errors: Array<{
        file: string;
        error: string;
    }>;
}
export interface MetaFile {
    version: string;
    source: string;
    sourceHash: string;
    compiledAt: string;
    model: string;
    provider: string;
    name?: string;
    triggerPhrases?: string[];
    capabilities?: string[];
    description: string;
    parameters: Array<{
        name: string;
        description?: string;
        required?: boolean;
        default?: string;
        position: number;
    }>;
    tools: string[];
    history: Array<{
        version: number;
        timestamp: string;
        sourceHash: string;
        model: string;
        provider: string;
    }>;
}
/**
 * Compile a Markdown task description to DML using an agentic loop
 */
export declare function compile(sourcePath: string, outputDir: string, options?: CompileOptions): Promise<CompileResult>;
/**
 * Compile all Markdown files in a directory
 */
export declare function compileAll(sourceDir: string, outputDir: string, options?: CompileOptions): Promise<CompileAllResult>;
/**
 * Compile a natural language prompt directly to DML without saving to disk
 */
export declare function compilePrompt(prompt: string, options?: CompileOptions): Promise<{
    dml: string;
    tools: string[];
}>;
export declare const extractToolDependencies: typeof extractToolDependenciesFromCompiler;
export declare const extractParameters: typeof extractParametersFromCompiler;
export declare const extractDescription: typeof extractDescriptionFromCompiler;
export declare function validateDMLSyntax(dml: string): {
    valid: boolean;
    errors: string[];
};
//# sourceMappingURL=compile.d.ts.map