import type { LLMUsage } from '../../types.js';
export interface TokenUsageTotals {
    calls: number;
    inputTokens: number;
    outputTokens: number;
    totalTokens: number;
    cacheReadTokens: number;
    cacheWriteTokens: number;
    reasoningTokens: number;
}
export type TokenUsageByModel = Record<string, TokenUsageTotals>;
export declare function createTokenUsageTotals(): TokenUsageTotals;
export declare function isTokenUsageEmpty(usageByModel: TokenUsageByModel): boolean;
export declare function recordTokenUsage(usageByModel: TokenUsageByModel, modelId: string | undefined, usage: LLMUsage | undefined): void;
export declare function mergeTokenUsageMaps(base: TokenUsageByModel, delta: TokenUsageByModel): TokenUsageByModel;
//# sourceMappingURL=token-usage.d.ts.map