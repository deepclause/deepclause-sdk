import type { Config } from '../../cli/config.js';
import type { DMLEvent, DeepClauseSDK, MemoryMessage } from '../../types.js';
import { type TokenUsageByModel } from './token-usage.js';
import type { ResolvedModelConfig } from '../config/model-slots.js';
import { type ShellManager } from './shell-manager.js';
export interface DmlExecutionContext {
    config: Config;
    workspacePath: string;
    selection: ResolvedModelConfig;
    shell: ShellManager;
}
export interface ExecuteDmlOptions {
    dmlCode: string;
    config: Config;
    workspaceRoot?: string;
    workspacePath: string;
    selection: ResolvedModelConfig;
    args?: unknown[];
    params?: Record<string, unknown>;
    gasLimit?: number;
    stream?: boolean;
    trace?: boolean;
    verbose?: boolean;
    headless?: boolean;
    sandbox?: boolean;
    signal?: AbortSignal;
    toolAbortSignalRef?: {
        signal?: AbortSignal;
    };
    onUserInput?: (prompt: string) => Promise<string>;
    initialMessages?: MemoryMessage[];
    onEvent?: (event: DMLEvent) => void;
    skillCatalog?: {
        workspaceRoot: string;
        currentSkillSlug?: string;
        invocationStack?: string[];
        maxDepth?: number;
        includeSystemSkillsInList?: boolean;
        onChildEvent?: (slug: string, event: DMLEvent) => void;
    };
    registerAdditionalTools?: (sdk: DeepClauseSDK, context: DmlExecutionContext) => Promise<void> | void;
}
export interface ExecuteDmlResult {
    output: string[];
    answer?: string;
    error?: string;
    trace?: object;
    events: DMLEvent[];
    usageByModel?: TokenUsageByModel;
}
export declare function executeDml(options: ExecuteDmlOptions): Promise<ExecuteDmlResult>;
//# sourceMappingURL=dml-executor.d.ts.map