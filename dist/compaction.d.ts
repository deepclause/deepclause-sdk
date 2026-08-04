import type { CompactionAction, CompactionOptions, CompactionScope, CompactionTrigger, CompactorBinding, CompactorDefinition, DMLEvent, MemoryMessage, ToolPolicy } from './types.js';
import type { Provider, ProviderConfig, ResolvedModelConfig } from './system/config/model-slots.js';
export interface ResolvedCompactorDefinition {
    source: string;
    sourceType: 'inline' | 'file' | 'auto';
    timeoutMs: number;
    gasLimit?: number;
    model?: string;
    provider?: 'openai' | 'anthropic' | 'google' | 'openrouter';
    inheritTools: boolean;
    toolPolicy?: ToolPolicy | null;
}
export interface ResolvedCompactorBinding {
    name?: string;
    scope: CompactionScope;
    trigger: CompactionTrigger;
    compactor: ResolvedCompactorDefinition;
}
export interface ResolvedCompactionOptions {
    enabled: boolean;
    bindings: ResolvedCompactorBinding[];
}
export interface ParsedCompactorDecision {
    apply: boolean;
    messages?: MemoryMessage[];
    rewrite?: CompactorRewriteSpec;
}
export interface CompactorRewriteSpec {
    keepLastMessages: number;
    summary: string;
}
export interface CompactorExecutionRequest {
    binding: ResolvedCompactorBinding;
    messages: MemoryMessage[];
    params: Record<string, unknown>;
}
export interface CompactorExecutionResponse {
    answer?: string;
    error?: string;
    usageByModel?: import('./system/runtime/token-usage.js').TokenUsageByModel;
}
export interface AppliedCompactorResult {
    messages: MemoryMessage[];
    event: DMLEvent;
    applied: boolean;
    usageByModel?: import('./system/runtime/token-usage.js').TokenUsageByModel;
}
export interface ResolvedCompactorModelConfig {
    model: string;
    modelId: string;
    provider: Provider;
    apiKey?: string;
    baseUrl?: string;
    temperature: number;
    maxOutputTokens?: number;
}
export declare function resolveCompactionOptions(base?: CompactionOptions, override?: CompactionOptions): ResolvedCompactionOptions | null;
export declare function resolveBinding(binding: CompactorBinding): ResolvedCompactorBinding;
export declare function resolveCompactorDefinition(definition: CompactorDefinition): ResolvedCompactorDefinition;
export declare function getCompactionBindings(options: ResolvedCompactionOptions | null, scope: CompactionScope, trigger: CompactionTrigger): ResolvedCompactorBinding[];
export declare function estimateTokensForText(text: string): number;
export declare function estimateTokensForMessages(messages: MemoryMessage[]): number;
export declare function detectProviderFromModel(model: string): Provider;
export declare function resolveCompactorModelConfig(params: {
    binding: ResolvedCompactorBinding;
    selection: ResolvedModelConfig;
    providerConfigs?: Partial<Record<Provider, ProviderConfig>>;
    baseUrl?: string;
}): ResolvedCompactorModelConfig;
export declare function buildCompactorParams(binding: ResolvedCompactorBinding, messages: MemoryMessage[], knownInputTokens?: number, maxContextTokens?: number): Record<string, unknown>;
export declare function executeCompactor(params: {
    binding: ResolvedCompactorBinding;
    messages: MemoryMessage[];
    knownInputTokens?: number;
    maxContextTokens?: number;
    execute: (request: CompactorExecutionRequest) => Promise<CompactorExecutionResponse>;
    emitEvent?: (event: DMLEvent) => void;
}): Promise<AppliedCompactorResult>;
export declare function parseCompactorAnswer(answer: string): ParsedCompactorDecision | null;
export declare function applyCompactorRewrite(binding: ResolvedCompactorBinding, messages: MemoryMessage[], rewrite: CompactorRewriteSpec): MemoryMessage[] | null;
export declare function normalizeMessageArray(value: unknown): MemoryMessage[] | null;
export declare function validateMessageArray(messages: MemoryMessage[]): string | null;
export declare function buildCompactionEvent(params: {
    binding: ResolvedCompactorBinding;
    action: CompactionAction;
    beforeTokens: number;
    afterTokens?: number;
    error?: string;
}): DMLEvent;
export declare function formatCompactionEventContent(params: {
    binding: ResolvedCompactorBinding;
    action: CompactionAction;
    beforeTokens: number;
    afterTokens?: number;
    error?: string;
}): string;
export declare function getBindingLabel(binding: ResolvedCompactorBinding): string;
//# sourceMappingURL=compaction.d.ts.map