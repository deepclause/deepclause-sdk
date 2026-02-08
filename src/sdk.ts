/**
 * DeepClause SDK - Main SDK Implementation
 */

import type {
  CompileOptions,
  CompileResult,
  CreateOptions,
  DeepClauseSDK,
  DMLEvent,
  MemoryMessage,
  RunOptions,
  ToolDefinition,
  ToolPolicy,
} from './types.js';
import { DMLRunner } from './runner.js';
import { loadProlog } from './prolog/loader.js';
import { compileToDML } from './compiler.js';
import { getBuiltInCompileTools } from './tools.js';
import * as fs from 'fs/promises';
import * as path from 'path';

/**
 * Create a new DeepClause SDK instance
 */
export async function createDeepClause(options: CreateOptions): Promise<DeepClauseSDK> {
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
    maxTokens: options.maxTokens ??  65536,
    baseUrl: options.baseUrl,
    trace: options.trace ?? false,
    streaming: options.streaming ?? false,
    debug: options.debug ?? false,
  });

  // Tool registry
  const tools = new Map<string, ToolDefinition>();
  let toolPolicy: ToolPolicy | null = null;
  let disposed = false;

  // Input handling
  let pendingInputResolve: ((input: string) => void) | null = null;

  const sdk: DeepClauseSDK = {
    async *runDML(code: string, runOptions?: RunOptions): AsyncGenerator<DMLEvent> {
      if (disposed) {
        throw new Error('SDK has been disposed');
      }

      // Run with tool policy and custom tools
      yield* runner.run(code, {
        ...runOptions,
        tools,
        toolPolicy,
        gasLimit: runOptions?.gasLimit,
        onInputRequired: async (prompt: string) => {
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

    async compile(source: string, compileOptions?: CompileOptions): Promise<CompileResult> {
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
        } catch {
          // If file reading fails, assume source is the markdown content itself
        }
      }

      // Merge options
      const mergedOptions: CompileOptions = {
        model: options.model,
        provider: provider as any,
        temperature: options.temperature,
        tools: getBuiltInCompileTools(),
        ...compileOptions
      };

      return compileToDML(markdown, mergedOptions);
    },

    registerTool(name: string, tool: ToolDefinition): void {
      if (disposed) {
        throw new Error('SDK has been disposed');
      }
      tools.set(name, tool);
    },

    setToolPolicy(policy: ToolPolicy): void {
      if (disposed) {
        throw new Error('SDK has been disposed');
      }
      toolPolicy = policy;
    },

    clearToolPolicy(): void {
      toolPolicy = null;
    },

    getToolPolicy(): ToolPolicy | null {
      return toolPolicy;
    },

    getTools(): string[] {
      return Array.from(tools.keys());
    },

    provideInput(input: string): void {
      if (pendingInputResolve) {
        pendingInputResolve(input);
        pendingInputResolve = null;
      }
    },

    getMemory(): MemoryMessage[] {
      return runner.getMemory();
    },

    async dispose(): Promise<void> {
      if (disposed) return;
      disposed = true;
      await runner.dispose();
    },
  };

  return sdk;
}

/**
 * Detect LLM provider from model name
 */
function detectProvider(model: string): 'openai' | 'anthropic' | 'google' | 'openrouter' {
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
function setApiKey(provider: string, apiKey: string): void {
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
