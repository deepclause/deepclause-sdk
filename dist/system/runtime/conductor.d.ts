import { resolveModelSlot, type Config } from '../../cli/config.js';
import type { DMLEvent } from '../../types.js';
import { type TokenUsageByModel } from './token-usage.js';
export interface ConductorSessionSummary {
    id: string;
    title: string;
    createdAt: string;
    updatedAt: string;
}
export interface ConductorSessionMessage {
    role: 'user' | 'assistant';
    content: string;
    timestamp: string;
}
export interface ConductorSessionDetail extends ConductorSessionSummary {
    messages: ConductorSessionMessage[];
    assistantMemory: string;
    taskMemory: string;
    usageByModel?: TokenUsageByModel;
    executionLogPath: string;
}
export interface ConductorTurnOptions {
    sessionId?: string;
    sessionTitle?: string;
    workspaceRoot?: string;
    workspacePath?: string;
    config?: Config;
    verbose?: boolean;
    trace?: boolean;
    gasLimit?: number;
    stream?: boolean;
    headless?: boolean;
    sandbox?: boolean;
    signal?: AbortSignal;
    toolAbortSignalRef?: {
        signal?: AbortSignal;
    };
    onUserInput?: (prompt: string) => Promise<string>;
    onEvent?: (event: ConductorLogEvent) => void;
}
export interface ConductorTurnResult {
    sessionId: string;
    output: string[];
    answer?: string;
    error?: string;
    trace?: object;
}
export interface ConductorLogEvent {
    scope: 'main' | 'child';
    childSlug?: string;
    modelId?: string;
    event: DMLEvent;
}
export type SessionExecutionKind = 'conductor' | 'skill' | 'skill-creator';
export interface SessionExecutionLogWriter {
    readonly executionId: string;
    readonly logPath: string;
    recordEvent(event: ConductorLogEvent): void;
    finish(summary: {
        status: 'success' | 'error';
        answer?: string;
        error?: string;
        outputCount?: number;
        usageByModel?: TokenUsageByModel;
    }): Promise<void>;
    flush(): Promise<void>;
}
export declare function listConductorSessions(workspaceRoot?: string): Promise<ConductorSessionSummary[]>;
export declare function createConductorSession(workspaceRoot?: string, title?: string): Promise<ConductorSessionSummary>;
export declare function getConductorSessionDetail(workspaceRoot: string | undefined, sessionId: string): Promise<ConductorSessionDetail>;
export declare function appendConductorSessionMessages(workspaceRoot: string | undefined, sessionId: string, messages: Array<{
    role: 'user' | 'assistant';
    content: string;
}>): Promise<void>;
export declare function runConductorTurn(userMessage: string, options?: ConductorTurnOptions): Promise<ConductorTurnResult>;
export declare function consultRecipes(options: {
    workspaceRoot: string;
    query: string;
    maxResults?: number;
}): Promise<Record<string, unknown>>;
export declare function createLocalSkill(options: {
    spec: string;
    workspaceRoot: string;
    workspacePath: string;
    config: Config;
    compileSelection: ReturnType<typeof resolveModelSlot>;
    runSelection: ReturnType<typeof resolveModelSlot>;
    sessionId: string;
    verbose?: boolean;
    sandbox: boolean;
    signal?: AbortSignal;
    onUserInput: (prompt: string) => Promise<string>;
    onEvent?: (event: ConductorLogEvent) => void;
}): Promise<Record<string, unknown>>;
export declare function createSessionExecutionLogWriter(options: {
    workspaceRoot: string;
    sessionId: string;
    executionKind: SessionExecutionKind;
    inputText: string;
    skillName?: string;
    args?: unknown[];
    modelId?: string;
}): SessionExecutionLogWriter;
export declare function mergeSessionUsage(workspaceRoot: string, sessionId: string, delta: TokenUsageByModel): Promise<void>;
//# sourceMappingURL=conductor.d.ts.map