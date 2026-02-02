/**
 * DeepClause SDK - Main SDK Implementation
 */
import { DMLRunner } from './runner.js';
import { loadProlog } from './prolog/loader.js';
/**
 * Create a new DeepClause SDK instance
 */
export async function createDeepClause(options) {
    // Detect provider from model name if not specified
    const provider = options.provider ?? detectProvider(options.model);
    // Set API key in environment if provided
    if (options.apiKey) {
        setApiKey(provider, options.apiKey);
    }
    // Initialize SWI-Prolog WASM
    const swipl = await loadProlog();
    // Create the runner instance
    const runner = new DMLRunner(swipl, {
        model: options.model,
        provider,
        temperature: options.temperature ?? 0.7,
        maxTokens: options.maxTokens ?? 65536,
        baseUrl: options.baseUrl,
        trace: options.trace ?? false,
        streaming: options.streaming ?? false,
        debug: options.debug ?? false,
    });
    // Tool registry
    const tools = new Map();
    let toolPolicy = null;
    let disposed = false;
    // Input handling
    let pendingInputResolve = null;
    const sdk = {
        async *runDML(code, runOptions) {
            if (disposed) {
                throw new Error('SDK has been disposed');
            }
            // Run with tool policy and custom tools
            yield* runner.run(code, {
                ...runOptions,
                tools,
                toolPolicy,
                onInputRequired: async (prompt) => {
                    if (runOptions?.onUserInput) {
                        return runOptions.onUserInput(prompt);
                    }
                    // Return a promise that will be resolved by provideInput()
                    return new Promise((resolve) => {
                        pendingInputResolve = resolve;
                    });
                },
            });
        },
        registerTool(name, tool) {
            if (disposed) {
                throw new Error('SDK has been disposed');
            }
            tools.set(name, tool);
        },
        setToolPolicy(policy) {
            if (disposed) {
                throw new Error('SDK has been disposed');
            }
            toolPolicy = policy;
        },
        clearToolPolicy() {
            toolPolicy = null;
        },
        getToolPolicy() {
            return toolPolicy;
        },
        getTools() {
            return Array.from(tools.keys());
        },
        provideInput(input) {
            if (pendingInputResolve) {
                pendingInputResolve(input);
                pendingInputResolve = null;
            }
        },
        getMemory() {
            return runner.getMemory();
        },
        async dispose() {
            if (disposed)
                return;
            disposed = true;
            await runner.dispose();
        },
    };
    return sdk;
}
/**
 * Detect LLM provider from model name
 */
function detectProvider(model) {
    const lower = model.toLowerCase();
    if (lower.includes('gpt') || lower.includes('o1') || lower.includes('o3')) {
        return 'openai';
    }
    if (lower.includes('claude')) {
        return 'anthropic';
    }
    if (lower.includes('gemini') || lower.includes('palm')) {
        return 'google';
    }
    // Default to openrouter for unknown models
    return 'openrouter';
}
/**
 * Set API key in environment for the provider
 */
function setApiKey(provider, apiKey) {
    switch (provider) {
        case 'openai':
            process.env.OPENAI_API_KEY = apiKey;
            break;
        case 'anthropic':
            process.env.ANTHROPIC_API_KEY = apiKey;
            break;
        case 'google':
            process.env.GOOGLE_GENERATIVE_AI_API_KEY = apiKey;
            break;
        case 'openrouter':
            process.env.OPENROUTER_API_KEY = apiKey;
            break;
    }
}
//# sourceMappingURL=sdk.js.map