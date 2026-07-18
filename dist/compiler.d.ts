/**
 * DeepClause SDK - Compilation Module
 *
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */
import { CompileOptions, CompileResult, AnalysisResult } from './types.js';
import { type TokenUsageByModel } from './system/runtime/token-usage.js';
interface ValidationResult {
    valid: boolean;
    errors: string[];
    warnings?: string[];
}
/**
 * Validate DML code using the actual Prolog parser
 */
export declare function validateWithProlog(dml: string): Promise<ValidationResult>;
/**
 * Run static analysis on the DML code
 */
export declare function analyzeDML(dml: string): Promise<AnalysisResult>;
/**
 * Run LLM-based security audit
 */
export declare function runLLMSecurityAudit(dml: string, staticAnalysis: AnalysisResult, model: string, provider: string, baseUrl?: string, usageByModel?: TokenUsageByModel): Promise<string>;
export declare function analyzeAndAuditDML(dml: string, options: {
    audit?: boolean;
    model: string;
    provider: string;
    baseUrl?: string;
    usageByModel?: TokenUsageByModel;
}): Promise<AnalysisResult>;
/**
 * Compile a natural language prompt or markdown directly to DML
 */
export declare function compileToDML(source: string, options: CompileOptions): Promise<CompileResult>;
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
export {};
//# sourceMappingURL=compiler.d.ts.map