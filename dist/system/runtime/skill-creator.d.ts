import type { AnalysisResult, DMLEvent } from '../../types.js';
import type { Config } from '../../cli/config.js';
import type { MetaFile } from '../../cli/compile.js';
import type { ResolvedModelConfig } from '../config/model-slots.js';
import { type TokenUsageByModel } from './token-usage.js';
export interface SkillCreatorCompileOptions {
    sourcePath: string;
    outputDir: string;
    baseName: string;
    workspaceRoot?: string;
    workspacePath: string;
    config: Config;
    compileSelection: ResolvedModelConfig;
    runSelection: ResolvedModelConfig;
    sandbox?: boolean;
    validateOnly?: boolean;
    maxAttempts?: number;
    verbose?: boolean;
    stream?: boolean;
    trace?: boolean;
    audit?: boolean;
    onUserInput?: (prompt: string) => Promise<string>;
    signal?: AbortSignal;
    onEvent?: (event: DMLEvent) => void;
}
export interface SkillCreatorCompileResult {
    dml: string;
    meta: MetaFile;
    tools: string[];
    outputPath: string;
    explanation: string;
    analysis: AnalysisResult;
    usageByModel: TokenUsageByModel;
}
export declare function compileWithSkillCreator(markdown: string, options: SkillCreatorCompileOptions): Promise<SkillCreatorCompileResult>;
export declare function normalizeSkillSlug(value: string, fallback?: string): string;
export declare function deriveSkillSlugFromMarkdown(markdown: string, fallbackBaseName?: string): string;
//# sourceMappingURL=skill-creator.d.ts.map