export const DEFAULT_MODEL_IDS = {
    gateway: 'openai:gpt-4o',
    run: 'openai:gpt-4o',
    compile: 'openai:gpt-4o',
};
export const DEFAULT_TEMPERATURES = {
    gateway: 0.7,
    run: 0.7,
    compile: 0.4,
};
const RUNTIME_PROVIDERS = new Set(['openai', 'anthropic', 'google', 'openrouter']);
function inferProvider(model) {
    if (model.startsWith('gpt-') || model.startsWith('o1') || model.startsWith('o3')) {
        return 'openai';
    }
    if (model.startsWith('claude-')) {
        return 'anthropic';
    }
    if (model.startsWith('gemini-')) {
        return 'google';
    }
    return 'openrouter';
}
function normalizeOpenRouterModel(remainder) {
    if (remainder.includes('/')) {
        return remainder;
    }
    return remainder.replace(':', '/');
}
export function normalizeModelId(modelId) {
    const trimmed = modelId.trim();
    if (!trimmed) {
        throw new Error('Invalid model id: value cannot be empty');
    }
    if (trimmed.startsWith('custom:')) {
        return trimmed;
    }
    const colonIndex = trimmed.indexOf(':');
    if (colonIndex > 0) {
        const prefix = trimmed.slice(0, colonIndex).toLowerCase();
        const remainder = trimmed.slice(colonIndex + 1);
        if (prefix === 'custom') {
            return `custom:${remainder}`;
        }
        if (RUNTIME_PROVIDERS.has(prefix)) {
            if (prefix === 'openrouter') {
                return `openrouter:${normalizeOpenRouterModel(remainder)}`;
            }
            return `${prefix}:${remainder}`;
        }
    }
    const slashIndex = trimmed.indexOf('/');
    if (slashIndex > 0) {
        const prefix = trimmed.slice(0, slashIndex).toLowerCase();
        const remainder = trimmed.slice(slashIndex + 1);
        if (RUNTIME_PROVIDERS.has(prefix)) {
            return prefix === 'openrouter'
                ? `openrouter:${remainder}`
                : `${prefix}:${remainder}`;
        }
    }
    return `${inferProvider(trimmed)}:${trimmed}`;
}
export function formatModelId(modelId) {
    return normalizeModelId(modelId);
}
export function parseModelId(modelId) {
    const id = normalizeModelId(modelId);
    const [prefix, ...rest] = id.split(':');
    if (prefix === 'custom') {
        const [customProviderName, ...modelParts] = rest;
        if (!customProviderName || modelParts.length === 0) {
            throw new Error(`Invalid custom model id: ${modelId}`);
        }
        return {
            id,
            provider: 'openai',
            model: modelParts.join(':'),
            customProviderName,
        };
    }
    if (!RUNTIME_PROVIDERS.has(prefix) || rest.length === 0) {
        throw new Error(`Invalid model id: ${modelId}`);
    }
    return {
        id,
        provider: prefix,
        model: rest.join(':'),
    };
}
function readCustomProviderEnv(customProviderName) {
    const envPrefix = `LLM_PROVIDER_${customProviderName.toUpperCase().replace(/[^A-Z0-9]+/g, '_')}`;
    return {
        apiKey: process.env[`${envPrefix}_API_KEY`],
        baseUrl: process.env[`${envPrefix}_BASE_URL`],
    };
}
import { resolveModelCapabilities } from './model-database.js';
export function resolveModelSlotConfig(config, slot, overrides = {}) {
    const parsed = parseModelId(overrides.modelId ?? config.models[slot] ?? DEFAULT_MODEL_IDS[slot]);
    const standardProviderConfig = config.providers?.[parsed.provider] ?? {};
    const customProviderConfig = parsed.customProviderName
        ? readCustomProviderEnv(parsed.customProviderName)
        : {};
    const providerName = parsed.customProviderName ? 'custom' : parsed.provider;
    const dbModelId = parsed.customProviderName
        ? parsed.model
        : (parsed.provider === 'openrouter' ? parsed.model : `${parsed.provider}/${parsed.model}`);
    const caps = resolveModelCapabilities(dbModelId, providerName);
    const slotOverrides = config.modelOptions?.[slot] ?? {};
    return {
        ...parsed,
        slot,
        temperature: overrides.temperature ?? config.temperatures[slot] ?? DEFAULT_TEMPERATURES[slot],
        apiKey: customProviderConfig.apiKey ?? standardProviderConfig.apiKey,
        baseUrl: customProviderConfig.baseUrl ?? standardProviderConfig.baseUrl,
        contextWindow: slotOverrides.maxContextTokens ?? caps.contextWindow,
        maxOutputTokens: slotOverrides.maxOutputTokens ?? caps.maxOutputTokens,
        reasoning: caps.reasoning,
        complexity: caps.complexity,
        reasoningType: caps.reasoningType,
        reasoningValues: caps.reasoningValues,
        reasoningBudgetMap: caps.reasoningBudgetMap,
        defaultEffort: caps.defaultEffort,
    };
}
export function buildModelOverride(model, provider) {
    if (!model) {
        return undefined;
    }
    if (provider && !model.includes(':') && !model.includes('/')) {
        return normalizeModelId(`${provider}:${model}`);
    }
    return normalizeModelId(model);
}
//# sourceMappingURL=model-slots.js.map