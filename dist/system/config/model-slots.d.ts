export type Provider = 'openai' | 'anthropic' | 'google' | 'openrouter';
export type ModelSlot = 'gateway' | 'run' | 'compile';
export type ModelComplexity = 'high' | 'medium' | 'low';
export type ReasoningType = 'effort' | 'budget_tokens' | 'thinking_config' | 'none';
export interface ProviderConfig {
    apiKey?: string;
    baseUrl?: string;
}
export interface SlotModelConfig {
    models: Record<ModelSlot, string>;
    temperatures: Record<ModelSlot, number>;
    providers?: Partial<Record<Provider, ProviderConfig>>;
    modelOptions?: Partial<Record<ModelSlot, ModelSlotOverrides>>;
}
export interface ModelSlotOverrides {
    maxContextTokens?: number;
    maxOutputTokens?: number;
    reasoningEffort?: string;
}
export interface ParsedModelId {
    id: string;
    provider: Provider;
    model: string;
    customProviderName?: string;
}
export interface ResolvedModelConfig extends ParsedModelId {
    slot: ModelSlot;
    temperature: number;
    baseUrl?: string;
    apiKey?: string;
    contextWindow?: number;
    maxOutputTokens?: number;
    reasoning?: boolean;
    complexity?: ModelComplexity;
    reasoningType?: ReasoningType;
    reasoningValues?: string[];
    reasoningBudgetMap?: Record<string, number>;
    defaultEffort?: string;
}
export declare const DEFAULT_MODEL_IDS: Record<ModelSlot, string>;
export declare const DEFAULT_TEMPERATURES: Record<ModelSlot, number>;
export declare function normalizeModelId(modelId: string): string;
export declare function formatModelId(modelId: string): string;
export declare function parseModelId(modelId: string): ParsedModelId;
export declare function resolveModelSlotConfig(config: SlotModelConfig, slot: ModelSlot, overrides?: {
    modelId?: string;
    temperature?: number;
}): ResolvedModelConfig;
export declare function buildModelOverride(model?: string, provider?: Provider): string | undefined;
//# sourceMappingURL=model-slots.d.ts.map