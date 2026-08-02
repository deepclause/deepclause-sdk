import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
let cachedDatabase = null;
function loadDatabase() {
    if (cachedDatabase) {
        return cachedDatabase;
    }
    const dbPath = join(__dirname, '..', 'assets', 'model-database.json');
    cachedDatabase = JSON.parse(readFileSync(dbPath, 'utf8'));
    return cachedDatabase;
}
export function lookupModel(modelId) {
    const db = loadDatabase();
    return db.models[modelId];
}
export function lookupProvider(providerName) {
    const db = loadDatabase();
    return db.providers[providerName];
}
export function getAllModels() {
    return loadDatabase().models;
}
export function resolveModelCapabilities(modelId, providerName) {
    const modelEntry = lookupModel(modelId);
    const providerEntry = lookupProvider(providerName);
    if (!modelEntry) {
        return {
            reasoning: false,
            complexity: 'medium',
            reasoningType: providerEntry?.reasoning_type ?? 'none',
            reasoningValues: providerEntry?.reasoning_values,
            reasoningBudgetMap: providerEntry?.budget_map,
            defaultEffort: providerEntry?.default_effort,
        };
    }
    return {
        contextWindow: modelEntry.limit.context,
        maxOutputTokens: modelEntry.limit.output,
        reasoning: modelEntry.reasoning,
        complexity: modelEntry.complexity,
        reasoningType: providerEntry?.reasoning_type ?? 'none',
        reasoningValues: providerEntry?.reasoning_values,
        reasoningBudgetMap: providerEntry?.budget_map,
        defaultEffort: providerEntry?.default_effort,
    };
}
export function buildReasoningProviderOptions(effort, reasoningType, budgetMap) {
    if (effort === 'none' || reasoningType === 'none') {
        return {};
    }
    switch (reasoningType) {
        case 'effort':
            return { reasoning_effort: effort };
        case 'budget_tokens': {
            const budget = budgetMap?.[effort] ?? 16000;
            return { thinking: { type: 'enabled', budget_tokens: budget } };
        }
        case 'thinking_config': {
            const budget = budgetMap?.[effort] ?? 8192;
            return { thinkingConfig: { thinkingBudget: budget } };
        }
        default:
            return {};
    }
}
const VENDOR_TO_PROVIDER = {
    openai: 'openai',
    anthropic: 'anthropic',
    google: 'google',
};
export function getAvailableProviders(dbModelId) {
    const vendor = dbModelId.split('/')[0];
    const providers = [];
    const directProvider = VENDOR_TO_PROVIDER[vendor];
    if (directProvider) {
        providers.push({
            provider: directProvider,
            modelId: dbModelId.split('/').slice(1).join('/'),
            label: `${directProvider} (direct)`,
            direct: true,
        });
    }
    providers.push({
        provider: 'openrouter',
        modelId: dbModelId,
        label: 'openrouter',
        direct: false,
    });
    return providers;
}
export function searchModels(query) {
    const db = loadDatabase();
    const q = query.toLowerCase().trim();
    const results = [];
    for (const [id, entry] of Object.entries(db.models)) {
        const haystack = `${id} ${entry.name} ${entry.family}`.toLowerCase();
        if (haystack.includes(q)) {
            results.push({
                modelId: id,
                entry,
                providers: getAvailableProviders(id),
            });
        }
    }
    return results;
}
//# sourceMappingURL=model-database.js.map