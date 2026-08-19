/**
 * DML Runner - Executes DML code using SWI-Prolog WASM
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import type { SWIPLModule } from './prolog/loader.js';
import type {
  CompactionOptions,
  DMLEvent,
  MemoryMessage,
  LLMBackend,
  ToolDefinition,
  ToolPolicy,
  TypedVar,
} from './types.js';
import { mountWorkspace } from './prolog/loader.js';
import { runAgentLoop } from './agent.js';
import {
  executeCompactor,
  getCompactionBindings,
  resolveCompactionOptions,
  type CompactorExecutionRequest,
  type CompactorExecutionResponse,
} from './compaction.js';
import {
  buildToolCompletionEvent,
  buildToolFailureEvent,
  buildToolStartEvent,
} from './system/runtime/shell-tool-events.js';
import { checkToolPolicy } from './tools.js';
import { generateLlmReply, sampleSingleToken } from './prolog/bridge.js';
import { recordTokenUsage } from './system/runtime/token-usage.js';
import { buildReasoningProviderOptions, type ReasoningType } from './system/config/model-database.js';

/**
 * Convert swipl-wasm value to plain JavaScript value
 * Handles PrologString, PrologAtom, PrologList, etc.
 */
function toJsValue(value: unknown): unknown {
  if (value === null || value === undefined) {
    return value;
  }
  
  // Handle arrays FIRST (before checking toString)
  if (Array.isArray(value)) {
    return value.map(toJsValue);
  }
  
  // Check for swipl-wasm special types with $t marker
  if (typeof value === 'object' && value !== null && '$t' in value) {
    const obj = value as { $t: string; v?: unknown; s?: string };
    switch (obj.$t) {
      case 's': // PrologString
        return obj.v ?? '';
      case 'a': // PrologAtom  
        return obj.v;
      case 'n': // PrologNumber
        return obj.v;
      default:
        return obj.v ?? value;
    }
  }
  
  // Check for toString method (another way swipl-wasm represents strings)
  // But NOT for arrays or plain objects
  if (typeof value === 'object' && value !== null && 
      !Array.isArray(value) && 
      'toString' in value && 
      typeof (value as Record<string, unknown>).toString === 'function') {
    const str = (value as { toString: () => string }).toString();
    // Only return string if it's not the default [object Object]
    if (!str.startsWith('[object ')) {
      return str;
    }
  }
  
  // Handle plain objects (recursively convert)
  if (typeof value === 'object' && value !== null) {
    const result: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value)) {
      result[k] = toJsValue(v);
    }
    return result;
  }
  
  return value;
}

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
  return 'openrouter';
}

function createChildAbortController(
  parentSignal: AbortSignal | undefined,
  timeoutMs: number,
): { signal: AbortSignal; dispose: () => void } {
  const controller = new AbortController();
  const disposers: Array<() => void> = [];

  if (parentSignal) {
    if (parentSignal.aborted) {
      controller.abort(parentSignal.reason);
    } else {
      const onAbort = () => controller.abort(parentSignal.reason);
      parentSignal.addEventListener('abort', onAbort, { once: true });
      disposers.push(() => parentSignal.removeEventListener('abort', onAbort));
    }
  }

  let timeout: NodeJS.Timeout | undefined;
  if (timeoutMs > 0) {
    timeout = setTimeout(() => controller.abort(new Error(`Compactor timed out after ${timeoutMs}ms`)), timeoutMs);
    disposers.push(() => clearTimeout(timeout));
  }

  return {
    signal: controller.signal,
    dispose: () => {
      for (const dispose of disposers) {
        dispose();
      }
    },
  };
}

export function decodeAgentOutputVars(rawOutputVars: unknown): (string | TypedVar)[] {
  const normalized = toJsValue(rawOutputVars);
  if (!Array.isArray(normalized)) {
    return [];
  }

  return normalized.map((value) => {
    if (
      typeof value === 'object'
      && value !== null
      && 'name' in value
      && 'type' in value
      && typeof (value as Record<string, unknown>).name === 'string'
      && typeof (value as Record<string, unknown>).type === 'string'
    ) {
      return value as TypedVar;
    }
    return String(value);
  });
}

export interface RunnerOptions {
  model: string;
  provider: string;
  temperature: number;
  maxTokens: number;
  baseUrl?: string;
  trace?: boolean;
  streaming?: boolean;
  debug?: boolean;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  providerOptions?: Record<string, Record<string, any>>;
  compaction?: CompactionOptions;
  reasoningType?: ReasoningType;
  reasoningBudgetMap?: Record<string, number>;
  contextWindow?: number;
  llmBackend?: LLMBackend;
}

export interface InternalRunOptions {
  args?: unknown[];
  params?: Record<string, unknown>;
  workspacePath?: string;
  gasLimit?: number;
  tools: Map<string, ToolDefinition>;
  toolPolicy: ToolPolicy | null;
  onInputRequired: (prompt: string) => Promise<string>;
  signal?: AbortSignal;
  initialMessages?: MemoryMessage[];
  compaction?: CompactionOptions;
}

interface RunnerExecutionContext {
  workspacePath?: string;
  signal?: AbortSignal;
  compaction?: CompactionOptions;
  toolPolicy?: ToolPolicy | null;
}

/**
 * Tool parameter info extracted from Prolog schema
 */
interface ToolParamInfo {
  name: string;
  type: string;
}

/**
 * User tool info from Prolog
 */
interface UserToolInfo {
  name: string;
  description: string;
  inputs: ToolParamInfo[];
  outputs: ToolParamInfo[];
  source: string;
}

/**
 * Map Prolog types to JSON Schema types
 */
function prologTypeToJsonType(prologType: string): string {
  switch (prologType) {
    case 'integer': return 'integer';
    case 'number': return 'number';
    case 'boolean': return 'boolean';
    case 'array': return 'array';
    case 'object': return 'object';
    case 'string':
    default: return 'string';
  }
}

/**
 * DML execution engine
 * 
 * ARCHITECTURE NOTE: 
 * DML-defined tools now run in ISOLATED Prolog engines, not sharing state
 * with the parent task. This simplifies parallel tool execution (AI SDK
 * may call multiple tools concurrently) and eliminates race conditions.
 */
export class DMLRunner {
  private swipl: SWIPLModule;
  private options: RunnerOptions;
  private engine: unknown = null;
  private sessionId: string = '';
  private currentMemory: MemoryMessage[] = [];
  private stepCounter = 0;

  constructor(swipl: SWIPLModule, options: RunnerOptions) {
    this.swipl = swipl;
    this.options = options;
  }

  /**
   * Get the current conversation memory
   */
  getMemory(): MemoryMessage[] {
    return [...this.currentMemory];
  }

  /**
   * Run DML code and yield events
   */
  async *run(code: string, options: InternalRunOptions): AsyncGenerator<DMLEvent> {
    // Generate unique session ID
    this.sessionId = `sess_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    const memoryId = `mem_${this.sessionId}`; // Kept for API compatibility

    try {
      // Mount workspace if path provided
      if (options.workspacePath) {
        try {
          mountWorkspace(this.swipl, options.workspacePath);
        } catch (err) {
          const message = err instanceof Error ? err.message : String(err);
          yield { type: 'error', content: `Failed to mount workspace: ${message}` };
          return;
        }
      }

      // Seed the MI state so top-level DML can inspect initial messages via get_memory/1.
      this.currentMemory = (options.initialMessages ?? []).map((message) => ({
        role: message.role,
        content: message.content,
      }));

    // Build args and params
    const args = this.buildArgs(options);
    const params = this.buildParams(options);

      // Parse the DML code and assert params as facts
      const parseResult = this.parseCode(code, memoryId, params);
      if (parseResult.error) {
        yield { type: 'error', content: parseResult.error };
        return;
      }

      // Create cooperative engine with args
      const engineResult = this.createEngine(memoryId, args, params);
      if (engineResult.error) {
        yield { type: 'error', content: engineResult.error };
        return;
      }

      // Cooperative execution loop
      while (true) {
        // Check for abort
        if (options.signal?.aborted) {
          const reason = options.signal.reason;
          const abortMessage = reason instanceof Error
            ? reason.message
            : typeof reason === 'string' && reason.trim().length > 0
              ? reason
              : 'Execution aborted';
          yield { type: 'error', content: abortMessage };
          break;
        }

        // Step the engine
        const step = this.stepEngine();

        if (process.env.DEBUG_STEPS) {
          console.log('[MAIN_LOOP] step:', step.status, step.content?.substring(0, 50));
        }

        switch (step.status) {
          case 'output':
            if (step.content) {
              if (process.env.DEBUG_STEPS) {
                console.log('[MAIN_LOOP] yielding output:', step.content.substring(0, 50));
              }
              yield { type: 'output', content: step.content };
            }
            break;

          case 'log':
            if (step.content) {
              yield { type: 'log', content: step.content };
            }
            break;

          case 'answer':
            yield { type: 'answer', content: step.content };
            break;

          case 'request_agent_loop':
            // Run agent loop for task() predicate
            yield* this.handleAgentLoop(
              step.payload,
              memoryId,
              options
            );
            break;

          case 'request_exec':
            // Execute external tool
            yield* this.handleExec(
              step.payload,
              options
            );
            break;

          case 'request_sample_token':
            yield* this.handleSampleToken(step.payload, options);
            break;

          case 'request_llm':
            yield* this.handleLlm(step.payload, options);
            break;

          case 'wait_input':
            // Request user input
            yield { type: 'input_required', prompt: step.prompt };
            const input = await options.onInputRequired(step.prompt ?? '');
            this.provideInput(input);
            break;

          case 'finished':
            // Include trace data if trace mode was enabled
            if (this.options.trace && step.payload) {
              const traceData = toJsValue((step.payload as Record<string, unknown>).trace);
              yield { type: 'finished', trace: Array.isArray(traceData) ? traceData : undefined } as DMLEvent;
            } else {
              yield { type: 'finished' };
            }
            return;

          case 'error':
            if (this.options.trace && step.payload) {
              const traceData = toJsValue((step.payload as Record<string, unknown>).trace);
              yield { type: 'error', content: step.content, trace: Array.isArray(traceData) ? traceData : undefined } as DMLEvent;
            } else {
              yield { type: 'error', content: step.content };
            }
            return;

          default:
            yield { type: 'error', content: `Unknown step status: ${step.status}` };
            return;
        }
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      yield { type: 'error', content: message };
    } finally {
      // Cleanup
      this.cleanup();
    }
  }

  /**
   * Parse DML code into Prolog clauses
   */
  private parseCode(code: string, memoryId: string, params: string): { error?: string } {
    try {
      // Write code to temp file in WASM filesystem
      const tempPath = `/tmp/dml_${Date.now()}.pl`;
      this.swipl.FS.writeFile(tempPath, code);

      // Parse using meta-interpreter, passing params to assert as facts
      const result = this.query(
        `deepclause_mi:parse_dml('${tempPath}', '${this.sessionId}', '${memoryId}', ${params}, Error)`
      );

      if (result && result.Error && result.Error !== 'none') {
        return { error: String(result.Error) };
      }

      return {};
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { error: `Parse error: ${message}` };
    }
  }

  /**
   * Build args list for Prolog (positional arguments for agent_main)
   */
  private buildArgs(options: InternalRunOptions): string {
    const args = options.args ?? [];
    const items = args.map(v => this.toPrologTerm(v)).join(', ');
    return `[${items}]`;
  }

  /**
   * Build params dictionary for Prolog (named parameters)
   */
  private buildParams(options: InternalRunOptions): string {
    const params: Record<string, unknown> = {
      workspace_path: options.workspacePath ?? './workspace',
      trace: this.options.trace ?? false,
      ...options.params,
    };

    if (options.gasLimit !== undefined) {
      params['gas_limit'] = options.gasLimit;
    }

    // Convert to Prolog dict syntax (keys must be lowercase atoms)
    const entries = Object.entries(params)
      .map(([k, v]) => `${k.toLowerCase()}: ${this.toPrologTerm(v)}`)
      .join(', ');

    return `params{${entries}}`;
  }

  /**
   * Quote a string as a Prolog atom if it contains non-atom characters.
   * Valid unquoted atoms: start with lowercase letter, contain only [a-zA-Z0-9_]
   */
  private quoteAtom(str: string): string {
    if (/^[a-z][a-zA-Z0-9_]*$/.test(str)) {
      return str;
    }
    // Quote and escape internal single quotes
    return `'${str.replace(/'/g, "''")}'`;
  }

  /**
   * Convert JS value to Prolog term string
   */
  private toPrologTerm(value: unknown): string {
    if (typeof value === 'string') {
      // Escape special characters
      const escaped = value
        .replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"');
      return `"${escaped}"`;
    }
    if (typeof value === 'number') {
      return String(value);
    }
    if (typeof value === 'boolean') {
      return value ? 'true' : 'false';
    }
    if (Array.isArray(value)) {
      const items = value.map(v => this.toPrologTerm(v)).join(', ');
      return `[${items}]`;
    }
    if (value && typeof value === 'object') {
      const entries = Object.entries(value)
        .map(([k, v]) => `${this.quoteAtom(k)}: ${this.toPrologTerm(v)}`)
        .join(', ');
      return `dict{${entries}}`;
    }
    return 'null';
  }

  /**
   * Create cooperative execution engine
   */
  private createEngine(memoryId: string, args: string, params: string): { error?: string } {
    try {
      const initialMemory = this.serializeMemoryMessages(this.currentMemory);
      const result = this.query(
        `deepclause_mi:create_engine('${this.sessionId}', '${memoryId}', ${args}, ${params}, [${initialMemory}], Engine)`
      );

      if (result && result.Engine) {
        this.engine = result.Engine;
        return {};
      }

      return { error: 'Failed to create engine' };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { error: `Engine creation failed: ${message}` };
    }
  }

  /**
   * Step the cooperative engine
   */
  private stepEngine(): {
    status: string;
    content?: string;
    prompt?: string;
    payload?: unknown;
  } {
    try {
      const result = this.query(
        `deepclause_mi:step_engine('${this.sessionId}', Status, Content, Payload)`
      );

      // Convert swipl-wasm values to plain JS
      const status = toJsValue(result?.Status);
      const content = toJsValue(result?.Content);
      const payload = toJsValue(result?.Payload);

      return {
        status: String(status ?? 'error'),
        content: typeof content === 'string' ? content : undefined,
        prompt: typeof content === 'string' ? content : undefined,
        payload,
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { status: 'error', content: message };
    }
  }

  /**
   * Handle agent loop request (task() predicate)
   */
  private async *handleAgentLoop(
    payload: unknown,
    _memoryId: string, // No longer used - memory comes from payload
    options: InternalRunOptions
  ): AsyncGenerator<DMLEvent> {
    const rawPayload = payload as Record<string, unknown>;
    const taskDescription = String(toJsValue(rawPayload.taskDescription) ?? '');
    const stepId = `step_${this.stepCounter++}`;

    yield { type: 'task_activity', taskState: 'started', taskDescription, taskId: stepId };

    let stepSucceeded: boolean | null = null;

    try {
    // Ensure outputVars is an array of either strings or TypedVar objects
    const outputVars = decodeAgentOutputVars(rawPayload.outputVars);
    
    // Parse userTools - now contains schema info as array of tool_info dicts
    const userTools: UserToolInfo[] = [];
    const rawUserTools = toJsValue(rawPayload.userTools);
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] Raw userTools from Prolog:', JSON.stringify(rawUserTools, null, 2));
    }
    if (Array.isArray(rawUserTools)) {
      for (const tool of rawUserTools) {
        const toolObj = toJsValue(tool) as Record<string, unknown>;
        const toolInfo = this.parseUserToolInfo(toolObj);
        if (toolInfo) {
          userTools.push(toolInfo);
        }
      }
    }

    // Queue for streaming and internal runtime events
    const streamQueue: DMLEvent[] = [];
    let streamResolve: (() => void) | null = null;

    const emitRunnerEvent = (event: DMLEvent) => {
      streamQueue.push(event);
      if (streamResolve) {
        streamResolve();
        streamResolve = null;
      }
    };

    // Extract memory from payload (now passed via state threading)
    const preparedMemory = this.extractMemoryFromPayload(rawPayload);

    // Extract reasoning effort and recipe context (from with_reasoning/with_recipe scoping)
    const reasoningEffort = String(toJsValue(rawPayload.reasoningEffort) ?? 'none');
    const recipeContext = String(toJsValue(rawPayload.recipeContext) ?? '');

    // Build provider options — merge reasoning effort if set
    const baseProviderOptions = this.options.providerOptions ?? {};
    let effectiveProviderOptions = baseProviderOptions;
    if (reasoningEffort !== 'none' && this.options.reasoningType && this.options.reasoningType !== 'none') {
      const reasoningOpts = buildReasoningProviderOptions(
        reasoningEffort,
        this.options.reasoningType,
        this.options.reasoningBudgetMap,
      );
      effectiveProviderOptions = { ...baseProviderOptions, ...reasoningOpts } as typeof baseProviderOptions;
    }

    // Prepend recipe context as a system message if present
    let effectiveMemory = preparedMemory;
    if (recipeContext) {
      effectiveMemory = [
        { role: 'system', content: `Recipe guidance:\n\n${recipeContext}` },
        ...preparedMemory,
      ];
    }

    // Store current memory for getMemory() access
    this.currentMemory = effectiveMemory;

    const emitToolEvent = (event: DMLEvent) => {
      emitRunnerEvent(event);
    };

    // Callback for tool output events
    const onToolOutput = (text: string) => {
      emitToolEvent({ type: 'output', content: text });
    };

    // Augment registered tools with internal ask_user so DML tools can call exec(ask_user(...))
    const augmentedTools = new Map(options.tools);
    if (!augmentedTools.has('ask_user')) {
      augmentedTools.set('ask_user', {
        description: 'Ask the user for input or clarification',
        parameters: {
          type: 'object',
          properties: {
            prompt: { type: 'string', description: 'The question or prompt to show the user' }
          },
          required: ['prompt']
        },
        execute: async (args) => {
          const argsObj = args as Record<string, unknown>;
          const prompt = (argsObj.prompt ?? argsObj.arg1 ?? Object.values(argsObj).find(v => typeof v === 'string')) as string;
          if (!prompt) {
            return { error: 'No prompt provided to ask_user' };
          }
          try {
            emitToolEvent({ type: 'input_required', prompt });
            const response = await options.onInputRequired(prompt);
            return { user_response: response };
          } catch (err) {
            return { error: String(err) };
          }
        },
      });
    }

    // Callback to emit tool call events
    const emitToolCall = (toolName: string, args: Record<string, unknown>) => {
      emitToolEvent({ type: 'tool_call', toolName, toolArgs: args });
    };

    // Build available tools for agent - SIMPLIFIED: tools run in isolated engines
    // The callback runs DML tools in isolated engines with no shared state.
    const availableTools = this.buildAgentTools(
      userTools,
      options.toolPolicy,
      async (toolName: string, args: Record<string, unknown>) => {
        return this.executeToolIsolated(
          toolName,
          args,
          augmentedTools,
          onToolOutput,
          {
            workspacePath: options.workspacePath,
            signal: options.signal,
            compaction: options.compaction,
            toolPolicy: options.toolPolicy,
          },
          [],
          emitToolCall,
          emitToolEvent,
        );
      }
    );

    // Run agent loop with streaming support
    const resultPromise = runAgentLoop({
      taskDescription,
      outputVars,
      memory: effectiveMemory,
      tools: availableTools,
      workspacePath: options.workspacePath,
      modelOptions: {
        model: this.options.model,
        provider: this.options.provider,
        temperature: this.options.temperature,
        maxOutputTokens: this.options.maxTokens,
        baseUrl: this.options.baseUrl,
        providerOptions: effectiveProviderOptions,
      },
      streaming: this.options.streaming,
      debug: this.options.debug,
      onOutput: (_text) => { /* Final outputs handled after completion */ },
      onStream: (chunk: string, done: boolean) => {
        streamQueue.push({ type: 'stream', content: chunk, done });
        if (streamResolve) {
          streamResolve();
          streamResolve = null;
        }
      },
      onToolCall: (toolName: string, args: Record<string, unknown>) => {
        streamQueue.push({ type: 'tool_call', toolName, toolArgs: args });
        if (streamResolve) {
          streamResolve();
          streamResolve = null;
        }
      },
      onUsage: (usage) => {
        streamQueue.push({ type: 'usage', usage });
        if (streamResolve) {
          streamResolve();
          streamResolve = null;
        }
      },
      onBeforeModelCall: async (messages, lastInputTokens) => this.applyCompactionBindings({
        messages,
        lastInputTokens,
        scope: 'loop',
        trigger: 'before_model_call',
        options,
        executionContext: {
          workspacePath: options.workspacePath,
          signal: options.signal,
          compaction: options.compaction,
        },
        registeredTools: options.tools,
        toolPolicy: options.toolPolicy,
        emitEvent: emitRunnerEvent,
      }),
      onAskUser: options.onInputRequired,
      signal: options.signal,
      llmBackend: this.options.llmBackend,
    });

    // Yield streaming events as they arrive
    if (this.options.streaming) {
      while (true) {
        // Process any queued events
        while (streamQueue.length > 0) {
          yield streamQueue.shift()!;
        }

        // Check if result is ready
        const raceResult = await Promise.race([
          resultPromise.then(r => ({ type: 'done' as const, result: r })),
          new Promise<{ type: 'stream' }>(resolve => {
            streamResolve = () => resolve({ type: 'stream' });
          }),
        ]);

        if (raceResult.type === 'done') {
          // Drain remaining queue
          while (streamQueue.length > 0) {
            yield streamQueue.shift()!;
          }
          
          // In streaming mode, output was already shown via onStream - don't repeat it
          // The outputs array is kept for memory/history purposes only

          const finalizedMemory = [
            ...preparedMemory.filter((message) => message.role === 'system'),
            ...raceResult.result.messages,
          ];

          while (streamQueue.length > 0) {
            yield streamQueue.shift()!;
          }

          // Update memory with full agent conversation
          this.currentMemory = finalizedMemory;

          // Post result back to Prolog
          stepSucceeded = raceResult.result.success;
          this.postAgentResult(raceResult.result, finalizedMemory);
          return;
        }
        // Otherwise continue loop to yield queued stream events
      }
    } else {
      // Non-streaming mode - wait for completion
      const result = await resultPromise;

      // Drain tool_call events from queue (they were queued during agent execution)
      if (process.env.DEBUG_QUEUE) {
        console.log('[RUNNER] Non-streaming: streamQueue length before drain:', streamQueue.length);
        if (streamQueue.length > 0) {
          console.log('[RUNNER] streamQueue contents:', streamQueue.map(e => `${e.type}:${e.content?.substring(0, 40)}`));
        }
      }
      while (streamQueue.length > 0) {
        yield streamQueue.shift()!;
      }

      if (process.env.DEBUG_QUEUE) {
        console.log('[RUNNER] Non-streaming agent result.outputs:', result.outputs);
      }

      // Stream any output from the agent
      for (const output of result.outputs) {
        yield { type: 'output', content: output };
      }

      const finalizedMemory = [
        ...preparedMemory.filter((message) => message.role === 'system'),
        ...result.messages,
      ];

      while (streamQueue.length > 0) {
        yield streamQueue.shift()!;
      }

      // Update memory with full agent conversation
      this.currentMemory = finalizedMemory;

      // Post result back to Prolog
      stepSucceeded = result.success;
      this.postAgentResult(result, finalizedMemory);
    }
    } finally {
      yield {
        type: 'task_activity',
        taskState: stepSucceeded === true ? 'completed' : 'failed',
        taskId: stepId,
      };
    }
  }

  /**
   * Handle one-token sampling request (sample_token/2-3)
   */
  private async *handleSampleToken(
    payload: unknown,
    options: InternalRunOptions,
  ): AsyncGenerator<DMLEvent> {
    const data = payload as Record<string, unknown>;
    const prompt = String(toJsValue(data.prompt) ?? '');
    const rawAllowedTokens = toJsValue(data.allowedTokens);
    const allowedTokens = Array.isArray(rawAllowedTokens)
      ? rawAllowedTokens.map((token) => String(token))
      : [];
    const stepId = `step_${this.stepCounter++}`;
    const taskDescription = prompt || 'sample_token';

    yield { type: 'task_activity', taskState: 'started', taskDescription, taskId: stepId };

    let succeeded = false;
    try {
      const { token, usage } = await sampleSingleToken({
        prompt,
        allowedTokens,
        modelOptions: {
          provider: this.options.provider,
          model: this.options.model,
          temperature: this.options.temperature,
          baseUrl: this.options.baseUrl,
          providerOptions: this.options.providerOptions,
        },
        signal: options.signal,
        llmBackend: this.options.llmBackend,
      });
      succeeded = true;
      this.postSampleTokenResult({ success: true, token });
      if (usage) {
        yield { type: 'usage', usage } as DMLEvent;
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      this.postSampleTokenResult({ success: false, error: message });
      yield { type: 'log', content: `sample_token failed: ${message}` };
    } finally {
      yield { type: 'task_activity', taskState: succeeded ? 'completed' : 'failed', taskId: stepId };
    }
  }

  /**
   * Handle direct one-shot LLM request (llm/2)
   */
  private async *handleLlm(
    payload: unknown,
    options: InternalRunOptions,
  ): AsyncGenerator<DMLEvent> {
    const data = payload as Record<string, unknown>;
    const messages = this.extractMessagesFromValue(data.messages);
    const stepId = `step_${this.stepCounter++}`;
    const taskDescription = summarizeLlmMessages(messages);

    yield { type: 'task_activity', taskState: 'started', taskDescription, taskId: stepId };

    let succeeded = false;
    try {
      const { text, usage } = await generateLlmReply({
        messages,
        modelOptions: {
          provider: this.options.provider,
          model: this.options.model,
          temperature: this.options.temperature,
          maxOutputTokens: this.options.maxTokens,
          baseUrl: this.options.baseUrl,
          providerOptions: this.options.providerOptions,
        },
        signal: options.signal,
        llmBackend: this.options.llmBackend,
      });
      succeeded = true;
      this.postLlmResult({ success: true, text });
      if (usage) {
        yield { type: 'usage', usage } as DMLEvent;
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      this.postLlmResult({ success: false, error: message });
      yield { type: 'log', content: `llm/2 failed: ${message}` };
    } finally {
      yield { type: 'task_activity', taskState: succeeded ? 'completed' : 'failed', taskId: stepId };
    }
  }

  /**
   * Handle exec request (exec() predicate)
   */
  private async *handleExec(
    payload: unknown,
    options: InternalRunOptions
  ): AsyncGenerator<DMLEvent> {
    const { toolName, args } = payload as {
      toolName: string;
      args: unknown[];
    };

    // Check tool policy
    const policyCheck = checkToolPolicy(toolName, options.toolPolicy);
    if (!policyCheck.allowed) {
      yield { type: 'log', content: `Tool '${toolName}' blocked by policy: ${policyCheck.reason}` };
      this.postExecResult({ success: false, error: policyCheck.reason });
      return;
    }

    // Get tool definition
    const tool = options.tools.get(toolName);
    if (!tool) {
      yield { type: 'log', content: `Tool '${toolName}' not found` };
      this.postExecResult({ success: false, error: `Tool not found: ${toolName}` });
      return;
    }

    let argsObj: Record<string, unknown> | null = null;

    try {
      // Execute tool
      argsObj = this.argsToObject(args, tool);
      
      // Emit tool_call event before execution
      yield buildToolStartEvent(toolName, argsObj);
      
      const result = await tool.execute(argsObj);

      yield buildToolCompletionEvent(toolName, argsObj, result);

      // Post result back to Prolog
      this.postExecResult({ success: true, result });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      if (argsObj) {
        yield buildToolFailureEvent(toolName, argsObj, error);
      }
      yield { type: 'log', content: `Tool '${toolName}' error: ${message}` };
      this.postExecResult({ success: false, error: message });
    }
  }

  /**
   * Convert positional args to object based on tool schema
   */
  private argsToObject(args: unknown[], tool: ToolDefinition): Record<string, unknown> {
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] argsToObject input:', JSON.stringify(args));
    }
    
    // Helper to unwrap Prolog values (PrologString, etc.)
    const unwrap = (val: unknown): unknown => {
      if (val && typeof val === 'object') {
        const obj = val as Record<string, unknown>;
        // PrologString: { '$t': 's', v: 'actual value' }
        if (obj['$t'] === 's' && 'v' in obj) {
          return obj['v'];
        }
        // Recursively unwrap
        return toJsValue(val);
      }
      return val;
    };

    // Helper to check if object looks like a dict (has regular keys, not just Prolog markers)
    const isDictLike = (obj: Record<string, unknown>): boolean => {
      const keys = Object.keys(obj);
      // Skip if it's a Prolog tagged value
      if ('$t' in obj || '$tag' in obj) return false;
      // Has at least one regular key
      return keys.some(k => !k.startsWith('$'));
    };
    
    // Helper to extract named args from Prolog key:value terms
    // Format: { '$t': 't', ':': [['key', value]] } or similar
    const extractNamedArgs = (args: unknown[]): Record<string, unknown> | null => {
      const result: Record<string, unknown> = {};
      let foundNamed = false;
      
      for (const arg of args) {
        if (arg && typeof arg === 'object') {
          const obj = arg as Record<string, unknown>;
          // Handle { '$t': 't', ':': [['name', value]] }
          if (obj['$t'] === 't' && ':' in obj) {
            const colonData = obj[':'] as unknown[];
            if (Array.isArray(colonData) && colonData.length > 0) {
              const [name, value] = colonData[0] as [string, unknown];
              result[name] = unwrap(value);
              foundNamed = true;
            }
          }
          // Handle { '$tag': ':', ... } format
          else if (obj['$tag'] === ':') {
            const name = String(obj[1] ?? '');
            const value = obj[2];
            if (name) {
              result[name] = unwrap(value);
              foundNamed = true;
            }
          }
        }
      }
      
      return foundNamed ? result : null;
    };
    
    // First, try to extract named args (query:Value format)
    const namedArgs = extractNamedArgs(args);
    if (namedArgs) {
      if (process.env.DEBUG_RUNNER) {
        console.log('[RUNNER] Extracted named args:', JSON.stringify(namedArgs));
      }
      return namedArgs;
    }

    // If args is a single dict-like object, unwrap and return it
    if (args.length === 1 && typeof args[0] === 'object' && args[0] !== null) {
      const obj = args[0] as Record<string, unknown>;
      if (isDictLike(obj)) {
        // It's already a dict, unwrap values and return
        const result: Record<string, unknown> = {};
        for (const [k, v] of Object.entries(obj)) {
          if (!k.startsWith('$')) {
            result[k] = unwrap(v);
          }
        }
        return result;
      }
      // Fall through to positional mapping
    }

    // Map positional args to schema properties
    // Handle both JSON Schema (with properties) and Zod schema (with shape)
    let keys: string[] = [];
    const params = tool.parameters as unknown as Record<string, unknown>;
    
    // Check for JSON Schema properties
    if (params.properties && typeof params.properties === 'object') {
      keys = Object.keys(params.properties as Record<string, unknown>);
    }
    // Check for Zod schema shape
    else if (params._def && typeof params._def === 'object') {
      const def = params._def as { shape?: () => Record<string, unknown> };
      if (typeof def.shape === 'function') {
        keys = Object.keys(def.shape());
      }
    }
    // Try zodToJsonSchema shape property directly
    else if (params.shape && typeof params.shape === 'object') {
      keys = Object.keys(params.shape as Record<string, unknown>);
    }

    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] Tool parameter keys:', keys);
    }
    
    if (keys.length > 0) {
      const result: Record<string, unknown> = {};
      keys.forEach((key, i) => {
        if (i < args.length) {
          result[key] = unwrap(args[i]);
        }
      });
      return result;
    }

    // Fallback: first arg as input
    return { input: unwrap(args[0]) };
  }

  /**
   * Parse user tool info from Prolog dict
   */
  private parseUserToolInfo(toolObj: Record<string, unknown>): UserToolInfo | null {
    const name = String(toJsValue(toolObj.name) ?? '');
    if (!name) return null;

    const schema = toJsValue(toolObj.schema) as Record<string, unknown> | null;
    const source = String(toJsValue(toolObj.source) ?? '');
    
    let description = '';
    const inputs: ToolParamInfo[] = [];
    const outputs: ToolParamInfo[] = [];

    if (schema) {
      description = String(toJsValue(schema.description) ?? '');
      
      const rawInputs = toJsValue(schema.inputs);
      if (Array.isArray(rawInputs)) {
        for (const input of rawInputs) {
          const inputObj = toJsValue(input) as Record<string, unknown>;
          inputs.push({
            name: String(toJsValue(inputObj.name) ?? ''),
            type: String(toJsValue(inputObj.type) ?? 'string'),
          });
        }
      }

      const rawOutputs = toJsValue(schema.outputs);
      if (Array.isArray(rawOutputs)) {
        for (const output of rawOutputs) {
          const outputObj = toJsValue(output) as Record<string, unknown>;
          outputs.push({
            name: String(toJsValue(outputObj.name) ?? ''),
            type: String(toJsValue(outputObj.type) ?? 'string'),
          });
        }
      }
    }

    return { name, description, inputs, outputs, source };
  }

  /**
   * Build tools available to agent loop
   * Only includes tools defined in DML - registered TypeScript tools are NOT exposed to the LLM.
   * DML tools can internally call exec/2 to use registered tools.
   */
  private buildAgentTools(
    userTools: UserToolInfo[],
    _policy: ToolPolicy | null,
    executeToolInline: (toolName: string, args: Record<string, unknown>) => Promise<{ result: unknown; outputs: string[] }>
  ): Map<string, ToolDefinition> {
    const result = new Map<string, ToolDefinition>();

    // Add user-defined tools from DML file with schema info
    // These are the ONLY tools the LLM can call during task()
    // Do not apply the host runtime-tool policy to these DML predicates. Their
    // nested exec/2 calls pass through handleExec/executeToolIsolated, where
    // the policy still restricts actual host capabilities.
    for (const tool of userTools) {
      // Build JSON Schema properties from inputs
      const properties: Record<string, { type: string; description?: string }> = {};
      const required: string[] = [];
      
      for (const input of tool.inputs) {
        properties[input.name] = {
          type: prologTypeToJsonType(input.type),
          description: `Input parameter (${input.type})`,
        };
        required.push(input.name);
      }

      // Build description (without Prolog source - keep prompts clean)
      const description = tool.description || `Tool: ${tool.name}`;

      // Capture tool name for closure
      const toolName = tool.name;
      
      result.set(tool.name, {
        description,
        parameters: { 
          type: 'object', 
          properties,
          required: required.length > 0 ? required : undefined,
        },
        execute: async (args) => {
          // Execute tool inline via main Prolog engine (shares state with task)
          const { result: toolResult } = await executeToolInline(toolName, args);
          return toolResult;
        },
      });
    }

    // NOTE: Registered TypeScript tools are NOT added here.
    // They are only available via exec/2 in DML code.
    // DML tools can call exec/2 internally to use registered tools.

    return result;
  }

  /**
   * Execute a DML-defined tool in isolation using its own engine
   * 
   * SIMPLIFIED: Tools now run in their own isolated Prolog engine, not sharing
   * state with the parent task. This eliminates:
   * - Race conditions from parallel tool calls
   * - Complex signal-based coordination
   * - Mutex requirements
   * 
   * The tool can still call exec() for external tools (TypeScript registered tools).
   * We step the tool engine and handle request_exec signals.
   * 
   * @param excludedTools - Tools currently on the call stack (for recursion prevention)
   * @param onToolCall - Callback to emit tool call events from nested tasks
   */
  private async executeToolIsolated(
    toolName: string,
    args: Record<string, unknown>,
    registeredTools: Map<string, ToolDefinition>,
    onOutput: (text: string) => void,
    runContext: RunnerExecutionContext,
    excludedTools: string[] = [],
    onToolCall?: (toolName: string, args: Record<string, unknown>) => void,
    onToolEvent?: (event: DMLEvent) => void,
  ): Promise<{ result: unknown; outputs: string[] }> {
    const outputs: string[] = [];
    
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] executeToolIsolated called for:', toolName);
    }
    
    // Convert args to Prolog list format (sorted by key name for consistent ordering)
    const sortedArgs = Object.entries(args)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([, v]) => this.toPrologTerm(v));
    const argsListStr = `[${sortedArgs.join(', ')}]`;
    
    // Create tool engine
    const createGoal = `deepclause_mi:create_tool_engine('${this.sessionId}', "${toolName}", ${argsListStr}, ToolEngineId)`;
    
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] Creating tool engine:', createGoal);
    }
    
    const createResult = this.query(createGoal);
    if (createResult === null) {
      return { result: { error: `Failed to create tool engine for ${toolName}` }, outputs };
    }
    
    const toolEngineId = String(toJsValue(createResult.ToolEngineId));
    
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] Tool engine created:', toolEngineId);
    }
    
    try {
      // Step the tool engine until it finishes
      while (true) {
        const stepGoal = `deepclause_mi:step_tool_engine('${toolEngineId}', Status, Content, Payload)`;
        const stepResult = this.query(stepGoal);
        
        if (stepResult === null) {
          return { result: { error: `Failed to step tool engine` }, outputs };
        }
        
        const status = String(toJsValue(stepResult.Status));
        const content = String(toJsValue(stepResult.Content) ?? '');
        const payload = toJsValue(stepResult.Payload) as Record<string, unknown> | null;
        
        if (process.env.DEBUG_RUNNER) {
          console.log('[RUNNER] Tool engine step:', status, content?.substring(0, 100));
        }
        
        switch (status) {
          case 'finished': {
            // Tool completed - extract result from payload
            const toolResult = payload?.result ?? true;
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] Tool finished with result:', JSON.stringify(toolResult).substring(0, 200));
            }
            return { result: toolResult, outputs };
          }
          
          case 'request_exec': {
            // Tool is calling exec() - handle it inline
            const execToolName = String(payload?.toolName ?? '');
            const execArgs = payload?.args as unknown[];
            
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] Tool exec request:', execToolName, execArgs);
            }
            
            // Find and execute the registered tool
            const tool = registeredTools.get(execToolName);
            if (!tool) {
              // Tool not found - post failure and continue
              const postGoal = `deepclause_mi:post_exec_result('${this.sessionId}', failure, "Tool not found: ${execToolName}")`;
              this.query(postGoal);
            } else {
              let argsObj: Record<string, unknown> | null = null;
              try {
                argsObj = this.argsToObject(execArgs, tool);
                onToolEvent?.(buildToolStartEvent(execToolName, argsObj));
                const execResult = await tool.execute(argsObj);
                onToolEvent?.(buildToolCompletionEvent(execToolName, argsObj, execResult));
                const postGoal = `deepclause_mi:post_exec_result('${this.sessionId}', success, ${this.toPrologTerm(execResult)})`;
                this.query(postGoal);
              } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                if (argsObj) {
                  onToolEvent?.(buildToolFailureEvent(execToolName, argsObj, error));
                }
                const postGoal = `deepclause_mi:post_exec_result('${this.sessionId}', failure, "${message.replace(/"/g, '\\"')}")`;
                this.query(postGoal);
              }
            }
            break;
          }

          case 'request_sample_token': {
            const prompt = String(toJsValue(payload?.prompt) ?? '');
            const rawAllowedTokens = toJsValue(payload?.allowedTokens);
            const allowedTokens = Array.isArray(rawAllowedTokens)
              ? rawAllowedTokens.map((token) => String(token))
              : [];
            const nestedStepId = `step_${this.stepCounter++}`;
            onToolEvent?.({ type: 'task_activity', taskState: 'started', taskDescription: prompt || 'sample_token', taskId: nestedStepId });

            try {
              const { token, usage } = await sampleSingleToken({
                prompt,
                allowedTokens,
                modelOptions: {
                  provider: this.options.provider,
                  model: this.options.model,
                  temperature: this.options.temperature,
                  baseUrl: this.options.baseUrl,
                  providerOptions: this.options.providerOptions,
                },
                signal: runContext.signal,
                llmBackend: this.options.llmBackend,
              });
              this.postSampleTokenResult({ success: true, token });
              if (usage) {
                onToolEvent?.({ type: 'usage', usage });
              }
              onToolEvent?.({ type: 'task_activity', taskState: 'completed', taskId: nestedStepId });
            } catch (error) {
              const message = error instanceof Error ? error.message : String(error);
              this.postSampleTokenResult({ success: false, error: message });
              onToolEvent?.({ type: 'task_activity', taskState: 'failed', taskId: nestedStepId });
              if (process.env.DEBUG_RUNNER) {
                console.log('[RUNNER] sample_token request failed:', message);
              }
            }
            break;
          }

          case 'request_llm': {
            const messages = this.extractMessagesFromValue(payload?.messages);
            const nestedStepId = `step_${this.stepCounter++}`;
            onToolEvent?.({ type: 'task_activity', taskState: 'started', taskDescription: summarizeLlmMessages(messages), taskId: nestedStepId });

            try {
              const { text, usage } = await generateLlmReply({
                messages,
                modelOptions: {
                  provider: this.options.provider,
                  model: this.options.model,
                  temperature: this.options.temperature,
                  maxOutputTokens: this.options.maxTokens,
                  baseUrl: this.options.baseUrl,
                  providerOptions: this.options.providerOptions,
                },
                signal: runContext.signal,
                llmBackend: this.options.llmBackend,
              });
              this.postLlmResult({ success: true, text });
              if (usage) {
                onToolEvent?.({ type: 'usage', usage });
              }
              onToolEvent?.({ type: 'task_activity', taskState: 'completed', taskId: nestedStepId });
            } catch (error) {
              const message = error instanceof Error ? error.message : String(error);
              this.postLlmResult({ success: false, error: message });
              onToolEvent?.({ type: 'task_activity', taskState: 'failed', taskId: nestedStepId });
              if (process.env.DEBUG_RUNNER) {
                console.log('[RUNNER] llm/2 request failed:', message);
              }
            }
            break;
          }
          
          case 'request_agent_loop': {
            // Tool is calling task() or prompt() - run nested agent loop
            const taskDescription = String(payload?.taskDescription ?? '');
            
            // Parse output vars
            const outputVars = decodeAgentOutputVars(payload?.outputVars);
            
            // Parse userTools for the nested agent
            const userTools: UserToolInfo[] = [];
            const rawUserTools = toJsValue(payload?.userTools);
            if (Array.isArray(rawUserTools)) {
              for (const tool of rawUserTools) {
                const toolObj = toJsValue(tool) as Record<string, unknown>;
                const toolInfo = this.parseUserToolInfo(toolObj);
                if (toolInfo) {
                  userTools.push(toolInfo);
                }
              }
            }
            
            // Parse tool scope (manual scoping via with_tools/without_tools)
            // Prolog term format: {$t: 't', whitelist: [[toolList]]} or {$t: 't', blacklist: [[toolList]]}
            const rawToolScope = toJsValue(payload?.toolScope);
            let toolScopeType: 'none' | 'whitelist' | 'blacklist' = 'none';
            let toolScopeList: string[] = [];
            
            if (rawToolScope && typeof rawToolScope === 'object' && '$t' in rawToolScope) {
              const compound = rawToolScope as Record<string, unknown>;
              // Check for {$t: 't', whitelist: ...} or {$t: 't', blacklist: ...}
              if ('whitelist' in compound) {
                toolScopeType = 'whitelist';
                // Extract tools from the nested array structure: [[toolList]]
                const args = toJsValue(compound.whitelist);
                if (Array.isArray(args) && args.length > 0) {
                  const inner = toJsValue(args[0]);
                  if (Array.isArray(inner) && inner.length > 0) {
                    const toolList = toJsValue(inner[0]);
                    if (Array.isArray(toolList)) {
                      toolScopeList = toolList.map(t => String(t));
                    }
                  }
                }
              } else if ('blacklist' in compound) {
                toolScopeType = 'blacklist';
                const args = toJsValue(compound.blacklist);
                if (Array.isArray(args) && args.length > 0) {
                  const inner = toJsValue(args[0]);
                  if (Array.isArray(inner) && inner.length > 0) {
                    const toolList = toJsValue(inner[0]);
                    if (Array.isArray(toolList)) {
                      toolScopeList = toolList.map(t => String(t));
                    }
                  }
                }
              }
            } else if (typeof rawToolScope === 'string') {
              // Simple atom 'none'
              if (rawToolScope !== 'none') {
                if (process.env.DEBUG_RUNNER) {
                  console.log('[RUNNER] Unexpected toolScope format:', rawToolScope);
                }
              }
            }
            
            // Build the call stack exclusion list (current tool + previously excluded)
            const nestedExcludedTools = [...excludedTools, toolName];
            
            // Filter tools based on:
            // 1. Call stack exclusion (prevent recursion)
            // 2. Manual scope (if specified via with_tools/without_tools)
            let filteredTools = userTools.filter(t => !nestedExcludedTools.includes(t.name));
            
            if (toolScopeType === 'whitelist') {
              // Only include tools in the whitelist
              filteredTools = filteredTools.filter(t => toolScopeList.includes(t.name));
            } else if (toolScopeType === 'blacklist') {
              // Exclude tools in the blacklist
              filteredTools = filteredTools.filter(t => !toolScopeList.includes(t.name));
            }
            
            // Extract memory
            const memory = this.extractMemoryFromPayload(payload as Record<string, unknown>);
            const preparedMemory = memory;

            // Extract reasoning effort and recipe context for nested loops
            const nestedReasoningEffort = String(toJsValue((payload as Record<string, unknown>)?.reasoningEffort) ?? 'none');
            const nestedRecipeContext = String(toJsValue((payload as Record<string, unknown>)?.recipeContext) ?? '');

            // Build provider options for nested loop
            const nestedBaseProviderOptions = this.options.providerOptions ?? {};
            let nestedProviderOptions = nestedBaseProviderOptions;
            if (nestedReasoningEffort !== 'none' && this.options.reasoningType && this.options.reasoningType !== 'none') {
              const reasoningOpts = buildReasoningProviderOptions(
                nestedReasoningEffort,
                this.options.reasoningType,
                this.options.reasoningBudgetMap,
              );
              nestedProviderOptions = { ...nestedBaseProviderOptions, ...reasoningOpts } as typeof nestedBaseProviderOptions;
            }

            // Prepend recipe context if present
            let nestedEffectiveMemory = preparedMemory;
            if (nestedRecipeContext) {
              nestedEffectiveMemory = [
                { role: 'system', content: `Recipe guidance:\n\n${nestedRecipeContext}` },
                ...preparedMemory,
              ];
            }
            
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] Tool agent loop request:', taskDescription.substring(0, 100));
              console.log('[RUNNER] Tool agent outputVars:', outputVars);
              console.log('[RUNNER] Call stack excluded tools:', nestedExcludedTools);
              console.log('[RUNNER] Tool scope:', toolScopeType, toolScopeList);
              console.log('[RUNNER] Available tools for nested agent:', filteredTools.map(t => t.name));
              console.log('[RUNNER] Memory passed to nested agent:', memory.length, 'messages');
              if (memory.length > 0) {
                console.log('[RUNNER] Memory contents:', JSON.stringify(memory, null, 2));
              }
            }
            
            // Build tools for nested agent with filtered tool list
            const nestedToolCallback = async (nestedToolName: string, nestedArgs: Record<string, unknown>) => {
                return this.executeToolIsolated(
                nestedToolName,
                nestedArgs,
                registeredTools,
                onOutput,
                  runContext,
                nestedExcludedTools,
                onToolCall,
                onToolEvent,
              );
            };
            
            const availableTools = this.buildAgentTools(
              filteredTools,
              null, // No policy override for nested
              nestedToolCallback
            );
            
            const nestedStepId = `step_${this.stepCounter++}`;
            onToolEvent?.({ type: 'task_activity', taskState: 'started', taskDescription, taskId: nestedStepId });

            try {
              // Run the nested agent loop
              const result = await runAgentLoop({
                taskDescription,
                outputVars,
                memory: nestedEffectiveMemory,
                tools: availableTools,
                modelOptions: {
                  model: this.options.model,
                  provider: this.options.provider,
                  temperature: this.options.temperature,
                  maxOutputTokens: this.options.maxTokens,
                  baseUrl: this.options.baseUrl,
                  providerOptions: nestedProviderOptions,
                },
                streaming: this.options.streaming,
                debug: this.options.debug,
                llmBackend: this.options.llmBackend,
                onOutput: (text) => {
                  outputs.push(text);
                  onOutput(text);
                },
                onStream: (chunk, done) => {
                  onToolEvent?.({ type: 'stream', content: chunk, done });
                },
                onUsage: (usage) => {
                  onToolEvent?.({ type: 'usage', usage });
                },
                onToolCall: onToolCall ?? (() => {}), // Forward tool call events to parent
                onBeforeModelCall: async (messages, lastInputTokens) => this.applyCompactionBindings({
                  messages,
                  lastInputTokens,
                  scope: 'loop',
                  trigger: 'before_model_call',
                  options: { compaction: runContext.compaction },
                  executionContext: runContext,
                  registeredTools,
                  toolPolicy: runContext.toolPolicy ?? null,
                  emitEvent: onToolEvent,
                }),
                onAskUser: async () => { 
                  // Nested agent loops don't support ask_user directly
                  // If needed, the tool should use exec(ask_user, ...) instead
                  return ''; 
                },
                signal: runContext.signal,
              });
              
              if (process.env.DEBUG_RUNNER) {
                console.log('[RUNNER] Nested agent loop completed:', result.success);
              }

              const finalizedMemory = [
                ...preparedMemory.filter((message) => message.role === 'system'),
                ...result.messages,
              ];
              
              // Post result back to tool engine
              this.postToolAgentResult(toolEngineId, result, finalizedMemory);
              onToolEvent?.({ type: 'task_activity', taskState: 'completed', taskId: nestedStepId });
            } catch (error) {
              const message = error instanceof Error ? error.message : String(error);
              if (process.env.DEBUG_RUNNER) {
                console.log('[RUNNER] Nested agent loop error:', message);
              }
              // Post failure result
              this.postToolAgentResult(toolEngineId, {
                success: false,
                outputs: [],
                variables: {},
                messages: preparedMemory.filter((message) => message.role !== 'system'),
              });
              onToolEvent?.({ type: 'task_activity', taskState: 'failed', taskId: nestedStepId });
            }
            break;
          }
          
          case 'output': {
            outputs.push(content);
            onOutput(content);
            break;
          }
          
          case 'log': {
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] Tool log:', content);
            }
            break;
          }
          
          case 'error': {
            return { result: { error: content }, outputs };
          }
          
          default: {
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] Unknown tool engine status:', status);
            }
            return { result: { error: `Unknown tool engine status: ${status}` }, outputs };
          }
        }
      }
    } finally {
      // Clean up tool engine
      const destroyGoal = `deepclause_mi:destroy_tool_engine('${toolEngineId}')`;
      this.query(destroyGoal);
      
      if (process.env.DEBUG_RUNNER) {
        console.log('[RUNNER] Tool engine destroyed:', toolEngineId);
      }
    }
  }

  private async applyCompactionBindings(params: {
    messages: MemoryMessage[];
    lastInputTokens?: number;
    scope: 'loop' | 'session' | 'run';
    trigger: 'before_model_call' | 'before_user_message' | 'before_task' | 'after_task';
    options: { compaction?: CompactionOptions };
    executionContext: RunnerExecutionContext;
    registeredTools: Map<string, ToolDefinition>;
    toolPolicy: ToolPolicy | null;
    emitEvent?: (event: DMLEvent) => void;
  }): Promise<MemoryMessage[]> {
    const resolved = resolveCompactionOptions(this.options.compaction, params.options.compaction);
    const bindings = getCompactionBindings(resolved, params.scope, params.trigger);
    if (bindings.length === 0) {
      return params.messages;
    }

    let messages = params.messages;
    for (const binding of bindings) {
      const result = await executeCompactor({
        binding,
        messages,
        knownInputTokens: params.lastInputTokens,
        maxContextTokens: this.options.contextWindow,
        emitEvent: params.emitEvent,
        execute: (request) => this.runBoundCompactor({
          request,
          executionContext: params.executionContext,
          registeredTools: params.registeredTools,
          toolPolicy: params.toolPolicy,
        }),
      });
      params.emitEvent?.(result.event);
      if (result.usageByModel) {
        for (const totals of Object.values(result.usageByModel)) {
          params.emitEvent?.({ type: 'usage', usage: {
            inputTokens: totals.inputTokens,
            outputTokens: totals.outputTokens,
            totalTokens: totals.totalTokens,
            cacheReadTokens: totals.cacheReadTokens || undefined,
            cacheWriteTokens: totals.cacheWriteTokens || undefined,
            reasoningTokens: totals.reasoningTokens || undefined,
          } });
        }
      }
      messages = result.messages;
    }

    return messages;
  }

  private async runBoundCompactor(params: {
    request: CompactorExecutionRequest;
    executionContext: RunnerExecutionContext;
    registeredTools: Map<string, ToolDefinition>;
    toolPolicy: ToolPolicy | null;
  }): Promise<CompactorExecutionResponse> {
    const { binding, params: compactorParams } = params.request;
    const code = await this.resolveCompactorSource(
      binding.compactor.source,
      binding.compactor.sourceType,
      params.executionContext.workspacePath,
    );

    const model = binding.compactor.model ?? this.options.model;
    const provider = binding.compactor.provider
      ?? (binding.compactor.model ? detectProvider(model) : this.options.provider);
    const compactorRunner = new DMLRunner(this.swipl, {
      ...this.options,
      model,
      provider,
      streaming: false,
      compaction: { enabled: false },
    });
    const { signal, dispose } = createChildAbortController(params.executionContext.signal, binding.compactor.timeoutMs);

    try {
      const tools = binding.compactor.inheritTools ? params.registeredTools : new Map<string, ToolDefinition>();
      let answer = '';
      let errorContent = '';
      const usageByModel: Record<string, import('./system/runtime/token-usage.js').TokenUsageTotals> = {};
      for await (const event of compactorRunner.run(code, {
        workspacePath: params.executionContext.workspacePath,
        gasLimit: binding.compactor.gasLimit,
        params: compactorParams,
        initialMessages: params.request.messages,
        tools,
        toolPolicy: binding.compactor.toolPolicy ?? (binding.compactor.inheritTools ? params.toolPolicy : null) ?? null,
        onInputRequired: async (prompt) => {
          throw new Error(`Compactor requested unexpected input: ${prompt}`);
        },
        signal,
        compaction: { enabled: false },
      })) {
        if (event.type === 'answer' && event.content) {
          answer = event.content;
        } else if (event.type === 'error' && event.content) {
          errorContent = event.content;
        } else if (event.type === 'usage' && event.usage) {
          recordTokenUsage(usageByModel, this.options.model, event.usage);
        }
      }

      return {
        answer,
        error: errorContent || undefined,
        usageByModel: Object.keys(usageByModel).length > 0 ? usageByModel : undefined,
      };
    } finally {
      dispose();
      await compactorRunner.dispose();
    }
  }

  private async resolveCompactorSource(
    source: string,
    sourceType: 'inline' | 'file' | 'auto',
    workspacePath?: string,
  ): Promise<string> {
    if (sourceType === 'inline') {
      return source;
    }

    const basePath = workspacePath ?? process.cwd();
    const candidatePath = path.isAbsolute(source) ? source : path.resolve(basePath, source);
    try {
      return await fs.readFile(candidatePath, 'utf8');
    } catch (error) {
      if (sourceType === 'file') {
        throw error;
      }
      return source;
    }
  }

  /**
   * Extract memory from payload (now passed via state threading)
   */
  private extractMessagesFromValue(rawMessages: unknown): MemoryMessage[] {
    const normalized = toJsValue(rawMessages);
    if (!Array.isArray(normalized)) {
      return [];
    }

    return normalized.map((messageValue: unknown) => {
      const msg = toJsValue(messageValue) as { role: string; content: string };
      const role = String(toJsValue(msg.role) ?? 'user');
      const content = String(toJsValue(msg.content) ?? '');

      if (role !== 'system' && role !== 'user' && role !== 'assistant') {
        return { role: 'user' as const, content };
      }

      return { role: role as 'system' | 'user' | 'assistant', content };
    });
  }

  private extractMemoryFromPayload(rawPayload: Record<string, unknown>): MemoryMessage[] {
    const rawMemory = toJsValue(rawPayload.memory);
    return this.extractMessagesFromValue(rawMemory);
  }

  private serializeMemoryMessages(messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>): string {
    return messages
      .map((message) => `message{role: ${message.role}, content: ${this.toPrologTerm(message.content)}}`)
      .join(', ');
  }

  /**
   * Post agent loop result back to Prolog
   */
  private postAgentResult(result: {
    success: boolean;
    outputs: string[];
    variables: Record<string, unknown>;
    messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>;
  }, fullMemory?: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>): void {
    // Keys need to be quoted because they start with uppercase (Var1, Var2, etc.)
    const varsStr = Object.entries(result.variables)
      .map(([k, v]) => `'${k}': ${this.toPrologTerm(v)}`)
      .join(', ');

    // Convert messages to Prolog list format
    const messagesStr = this.serializeMemoryMessages(result.messages);
    const fullMemoryStr = this.serializeMemoryMessages(fullMemory ?? []);

    this.query(
      `deepclause_mi:post_agent_result('${this.sessionId}', ${result.success}, vars{${varsStr}}, [${messagesStr}], [${fullMemoryStr}])`
    );
  }

  /**
   * Post exec result back to Prolog
   */
  private postExecResult(result: {
    success: boolean;
    result?: unknown;
    error?: string;
  }): void {
    if (result.success) {
      const termStr = this.toPrologTerm(result.result);
      this.query(
        `deepclause_mi:post_exec_result('${this.sessionId}', success, ${termStr})`
      );
    } else {
      const escapedErr = this.toPrologTerm(result.error ?? 'Unknown error');
      this.query(
        `deepclause_mi:post_exec_result('${this.sessionId}', failure, ${escapedErr})`
      );
    }
  }

  /**
   * Post one-token sampling result back to Prolog
   */
  private postSampleTokenResult(result: {
    success: boolean;
    token?: string;
    error?: string;
  }): void {
    if (result.success) {
      const token = this.toPrologTerm(result.token ?? '');
      this.query(
        `deepclause_mi:post_sample_token_result('${this.sessionId}', success, ${token})`
      );
    } else {
      const error = this.toPrologTerm(result.error ?? 'Unknown error');
      this.query(
        `deepclause_mi:post_sample_token_result('${this.sessionId}', failure, ${error})`
      );
    }
  }

  /**
   * Post direct llm/2 result back to Prolog
   */
  private postLlmResult(result: {
    success: boolean;
    text?: string;
    error?: string;
  }): void {
    if (result.success) {
      const text = this.toPrologTerm(result.text ?? '');
      this.query(
        `deepclause_mi:post_llm_result('${this.sessionId}', success, ${text})`
      );
    } else {
      const error = this.toPrologTerm(result.error ?? 'Unknown error');
      this.query(
        `deepclause_mi:post_llm_result('${this.sessionId}', failure, ${error})`
      );
    }
  }

  /**
   * Post tool engine agent loop result back to Prolog
   */
  private postToolAgentResult(
    toolEngineId: string,
    result: {
      success: boolean;
      outputs: string[];
      variables: Record<string, unknown>;
      messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>;
    },
    fullMemory?: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>,
  ): void {
    // Keys need to be quoted because they start with uppercase (Var1, Var2, etc.)
    const varsStr = Object.entries(result.variables)
      .map(([k, v]) => `'${k}': ${this.toPrologTerm(v)}`)
      .join(', ');

    // Convert messages to Prolog list format
    const messagesStr = this.serializeMemoryMessages(result.messages);
    const fullMemoryStr = this.serializeMemoryMessages(fullMemory ?? []);

    this.query(
      `deepclause_mi:post_tool_agent_result('${this.sessionId}', '${toolEngineId}', ${result.success}, vars{${varsStr}}, [${messagesStr}], [${fullMemoryStr}])`
    );
  }

  /**
   * Provide user input to waiting Prolog
   */
  private provideInput(input: string): void {
    const escaped = input.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    this.query(
      `deepclause_mi:provide_input('${this.sessionId}', "${escaped}")`
    );
  }

  /**
   * Execute a Prolog query
   */
  private query(goal: string): Record<string, unknown> | null {
    try {
      const q = this.swipl.prolog.query(goal);
      const result = q.once();
      return result as Record<string, unknown> | null;
    } catch (error) {
      console.error(`Prolog query failed: ${goal}`, error);
      return null;
    }
  }

  /**
   * Cleanup resources
   */
  private cleanup(): void {
    if (this.engine) {
      try {
        this.query(`deepclause_mi:destroy_engine('${this.sessionId}')`);
      } catch {
        // Ignore cleanup errors
      }
      this.engine = null;
    }
  }

  /**
   * Dispose runner
   */
  async dispose(): Promise<void> {
    this.cleanup();
  }
}

function summarizeLlmMessages(messages: Array<{ role: string; content: string }>): string {
  if (messages.length === 0) return 'llm()';
  const last = messages[messages.length - 1];
  const text = typeof last.content === 'string' ? last.content : '';
  const summary = text.replace(/\s+/g, ' ').trim();
  if (!summary) return `llm(${messages.length} messages)`;
  return summary.length > 80 ? summary.slice(0, 77) + '...' : summary;
}
