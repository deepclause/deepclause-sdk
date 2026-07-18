/**
 * DeepClause SDK - Main SDK Implementation
 */
import { DMLRunner } from './runner.js';
import { loadProlog } from './prolog/loader.js';
import { compileToDML } from './compiler.js';
import { getBuiltInCompileTools } from './tools.js';
import * as fs from 'fs/promises';
import * as path from 'path';
function extractAgentMainArities(code) {
    const strippedCode = code.replace(/%.*$/gm, '');
    const arities = new Set();
    const rulePattern = /^\s*agent_main\s*(?:\(([^)]*)\))?\s*:-/gm;
    let match;
    while ((match = rulePattern.exec(strippedCode)) !== null) {
        const rawArgs = match[1]?.trim();
        if (!rawArgs) {
            arities.add(0);
            continue;
        }
        const args = rawArgs
            .split(',')
            .map((arg) => arg.trim())
            .filter((arg) => arg.length > 0);
        arities.add(args.length);
    }
    if (/^\s*agent_main\s*\.\s*$/m.test(strippedCode)) {
        arities.add(0);
    }
    return [...arities].sort((left, right) => left - right);
}
function validateRunArguments(code, args) {
    const supportedArities = extractAgentMainArities(code);
    if (supportedArities.length === 0) {
        return undefined;
    }
    const providedCount = args?.length ?? 0;
    if (supportedArities.includes(providedCount)) {
        return undefined;
    }
    if (supportedArities.length === 1) {
        const [expectedArity] = supportedArities;
        const expectedLabel = expectedArity === 1 ? '1 positional argument' : `${expectedArity} positional arguments`;
        const receivedLabel = providedCount === 1 ? '1 argument' : `${providedCount} arguments`;
        return `Argument validation failed: agent_main expects ${expectedLabel}, but received ${receivedLabel}. Provide all positional arguments so agent_main variables are bound at entry.`;
    }
    return `Argument validation failed: agent_main supports arities ${supportedArities.join(', ')}, but received ${providedCount} positional arguments. Provide an argument count that exactly matches one defined agent_main signature.`;
}
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
        providerOptions: options.providerOptions,
        compaction: options.compaction,
        reasoningType: options.reasoningType,
        reasoningBudgetMap: options.reasoningBudgetMap,
        contextWindow: options.contextWindow,
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
            const argumentValidationError = validateRunArguments(code, runOptions?.args);
            if (argumentValidationError) {
                yield { type: 'error', content: argumentValidationError };
                return;
            }
            // Run with tool policy and custom tools
            yield* runner.run(code, {
                ...runOptions,
                compaction: runOptions?.compaction,
                tools,
                toolPolicy,
                gasLimit: runOptions?.gasLimit,
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
        async compile(source, compileOptions) {
            if (disposed) {
                throw new Error('SDK has been disposed');
            }
            let markdown = source;
            // If source looks like a file path and ends with .md, try to read it
            if (source.endsWith('.md') && source.length < 1024) {
                try {
                    // Use absolute path if it exists, otherwise relative to cwd
                    const filePath = path.isAbsolute(source) ? source : path.resolve(process.cwd(), source);
                    markdown = await fs.readFile(filePath, 'utf-8');
                }
                catch {
                    // If file reading fails, assume source is the markdown content itself
                }
            }
            // Merge options
            const mergedOptions = {
                model: options.model,
                provider: provider,
                temperature: options.temperature,
                tools: getBuiltInCompileTools(),
                ...compileOptions
            };
            return compileToDML(markdown, mergedOptions);
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