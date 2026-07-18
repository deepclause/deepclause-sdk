export type ModelComplexity = 'high' | 'medium' | 'low';
export type ReasoningType = 'effort' | 'budget_tokens' | 'thinking_config' | 'none';
export interface ModelDatabaseEntry {
    name: string;
    reasoning: boolean;
    tool_call: boolean;
    structured_output: boolean;
    limit: {
        context: number;
        output: number;
    };
    open_weights: boolean;
    complexity: ModelComplexity;
    family: string;
}
export interface ProviderDatabaseEntry {
    reasoning_type: ReasoningType;
    reasoning_values?: string[];
    budget_map?: Record<string, number>;
    default_effort?: string;
}
export declare function lookupModel(modelId: string): ModelDatabaseEntry | undefined;
export declare function lookupProvider(providerName: string): ProviderDatabaseEntry | undefined;
export declare function getAllModels(): Record<string, ModelDatabaseEntry>;
export interface ResolvedModelCapabilities {
    contextWindow?: number;
    maxOutputTokens?: number;
    reasoning: boolean;
    complexity: ModelComplexity;
    reasoningType: ReasoningType;
    reasoningValues?: string[];
    reasoningBudgetMap?: Record<string, number>;
    defaultEffort?: string;
}
export declare function resolveModelCapabilities(modelId: string, providerName: string): ResolvedModelCapabilities;
export declare function buildReasoningProviderOptions(effort: string, reasoningType: ReasoningType, budgetMap?: Record<string, number>): Record<string, unknown>;
export interface AvailableProvider {
    provider: string;
    modelId: string;
    label: string;
    direct: boolean;
}
export declare function getAvailableProviders(dbModelId: string): AvailableProvider[];
export interface ModelSearchResult {
    modelId: string;
    entry: ModelDatabaseEntry;
    providers: AvailableProvider[];
}
export declare function searchModels(query: string): ModelSearchResult[];
//# sourceMappingURL=model-database.d.ts.map