/**
 * DeepClause CLI - Compilation Module
 *
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */
import { type Provider } from './config.js';
export interface CompileOptions {
    force?: boolean;
    validateOnly?: boolean;
    model?: string;
    provider?: Provider;
    temperature?: number;
    maxAttempts?: number;
    verbose?: boolean;
    stream?: boolean;
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
interface ValidationResult {
    valid: boolean;
    errors: string[];
    warnings?: string[];
}
interface CompilationAttempt {
    dml: string;
    validation: ValidationResult;
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
/**
 * Generate DML using LLM with streaming
 */
export declare function generateDMLWithStreaming(systemPrompt: string, userMessage: string, previousAttempts: CompilationAttempt[], model: string, provider: Provider, temperature?: number, onChunk?: (chunk: string) => void): Promise<string>;
/**
 * Extract tool dependencies from DML code
 */
export declare function extractToolDependencies(dml: string): string[];
/**
 * Extract parameters from agent_main signature
 */
export declare function extractParameters(dml: string): Array<{
    name: string;
    position: number;
    description?: string;
    required?: boolean;
}>;
/**
 * Extract description from markdown
 */
export declare function extractDescription(markdown: string): string;
/**
 * Basic DML syntax validation (exported for backwards compatibility)
 */
export declare function validateDMLSyntax(dml: string): {
    valid: boolean;
    errors: string[];
};
export {};
//# sourceMappingURL=compile.d.ts.map