export function createTokenUsageTotals() {
    return {
        calls: 0,
        inputTokens: 0,
        outputTokens: 0,
        totalTokens: 0,
        cacheReadTokens: 0,
        cacheWriteTokens: 0,
        reasoningTokens: 0,
    };
}
export function isTokenUsageEmpty(usageByModel) {
    return Object.keys(usageByModel).length === 0;
}
export function recordTokenUsage(usageByModel, modelId, usage) {
    if (!modelId || !usage) {
        return;
    }
    const totals = usageByModel[modelId] ?? (usageByModel[modelId] = createTokenUsageTotals());
    totals.calls += 1;
    totals.inputTokens += usage.inputTokens ?? 0;
    totals.outputTokens += usage.outputTokens ?? 0;
    totals.totalTokens += usage.totalTokens ?? 0;
    totals.cacheReadTokens += usage.cacheReadTokens ?? 0;
    totals.cacheWriteTokens += usage.cacheWriteTokens ?? 0;
    totals.reasoningTokens += usage.reasoningTokens ?? 0;
}
export function mergeTokenUsageMaps(base, delta) {
    const merged = {};
    for (const [modelId, usage] of Object.entries(base)) {
        merged[modelId] = { ...usage };
    }
    for (const [modelId, usage] of Object.entries(delta)) {
        const totals = merged[modelId] ?? (merged[modelId] = createTokenUsageTotals());
        totals.calls += usage.calls;
        totals.inputTokens += usage.inputTokens;
        totals.outputTokens += usage.outputTokens;
        totals.totalTokens += usage.totalTokens;
        totals.cacheReadTokens += usage.cacheReadTokens;
        totals.cacheWriteTokens += usage.cacheWriteTokens;
        totals.reasoningTokens += usage.reasoningTokens;
    }
    return merged;
}
//# sourceMappingURL=token-usage.js.map