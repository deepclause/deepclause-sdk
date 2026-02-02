/**
 * DML Runner - Executes DML code using SWI-Prolog WASM
 */

import type { SWIPLModule } from './prolog/loader.js';
import type { DMLEvent, ToolDefinition, ToolPolicy, MemoryMessage } from './types.js';
import { mountWorkspace } from './prolog/loader.js';
import { runAgentLoop } from './agent.js';
import { checkToolPolicy } from './tools.js';

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

export interface RunnerOptions {
  model: string;
  provider: string;
  temperature: number;
  maxTokens: number;
  baseUrl?: string;
  trace?: boolean;
  streaming?: boolean;
  debug?: boolean;
}

export interface InternalRunOptions {
  args?: unknown[];
  params?: Record<string, unknown>;
  workspacePath?: string;
  tools: Map<string, ToolDefinition>;
  toolPolicy: ToolPolicy | null;
  onInputRequired: (prompt: string) => Promise<string>;
  signal?: AbortSignal;
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
 */
export class DMLRunner {
  private swipl: SWIPLModule;
  private options: RunnerOptions;
  private engine: unknown = null;
  private sessionId: string = '';
  private currentMemory: MemoryMessage[] = [];
  // Mutex for serializing tool execution (AI SDK may call tools in parallel)
  private toolExecutionLock: Promise<void> = Promise.resolve();
  // Depth counter for re-entrant tool execution (nested task() calls)
  private toolExecutionDepth: number = 0;

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

      // Memory is now managed internally by the MI via state threading
      // No external initialization needed

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
          yield { type: 'error', content: 'Execution aborted' };
          break;
        }

        // Step the engine
        const step = this.stepEngine();

        switch (step.status) {
          case 'output':
            if (step.content) {
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
            yield { type: 'error', content: step.content };
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

    // Convert to Prolog dict syntax (keys must be lowercase atoms)
    const entries = Object.entries(params)
      .map(([k, v]) => `${k.toLowerCase()}: ${this.toPrologTerm(v)}`)
      .join(', ');

    return `params{${entries}}`;
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
        .map(([k, v]) => `${k}: ${this.toPrologTerm(v)}`)
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
      const result = this.query(
        `deepclause_mi:create_engine('${this.sessionId}', '${memoryId}', ${args}, ${params}, Engine)`
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
    // Safely extract payload fields with proper conversion
    const rawPayload = payload as Record<string, unknown>;
    const taskDescription = String(toJsValue(rawPayload.taskDescription) ?? '');
    
    // Ensure outputVars is an array
    let outputVars: string[] = [];
    const rawOutputVars = toJsValue(rawPayload.outputVars);
    if (Array.isArray(rawOutputVars)) {
      outputVars = rawOutputVars.map(v => String(v));
    }
    
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

    // Extract memory from payload (now passed via state threading)
    const memory = this.extractMemoryFromPayload(rawPayload);
    
    // Store current memory for getMemory() access
    this.currentMemory = memory;

    // Callback for tool output events
    const onToolOutput = (text: string) => {
      streamQueue.push({ type: 'output', content: text });
      if (streamResolve) {
        streamResolve();
        streamResolve = null;
      }
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
            const response = await options.onInputRequired(prompt);
            return { user_response: response };
          } catch (err) {
            return { error: String(err) };
          }
        },
      });
    }

    // Build available tools for agent
    const availableTools = this.buildAgentTools(
      userTools, 
      options.toolPolicy,
      // executeToolInline callback - runs tool in main engine with shared state
      async (toolName: string, args: Record<string, unknown>) => {
        return this.executeToolInline(toolName, args, augmentedTools, onToolOutput, options);
      }
    );

    // Queue for streaming events
    const streamQueue: DMLEvent[] = [];
    let streamResolve: (() => void) | null = null;

    // Run agent loop with streaming support
    const resultPromise = runAgentLoop({
      taskDescription,
      outputVars,
      memory,
      tools: availableTools,
      modelOptions: this.options,
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
      onAskUser: options.onInputRequired,
      signal: options.signal,
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

          // Update memory with full agent conversation
          this.currentMemory = raceResult.result.messages;

          // Post result back to Prolog
          this.postAgentResult(raceResult.result);
          return;
        }
        // Otherwise continue loop to yield queued stream events
      }
    } else {
      // Non-streaming mode - wait for completion
      const result = await resultPromise;

      // Drain tool_call events from queue (they were queued during agent execution)
      while (streamQueue.length > 0) {
        yield streamQueue.shift()!;
      }

     // if (process.env.DEBUG_RUNNER) {
        console.log('[RUNNER] Non-streaming agent result.outputs:', result.outputs);
    //  }

      // Stream any output from the agent
      for (const output of result.outputs) {
        yield { type: 'output', content: output };
      }

      // Update memory with full agent conversation
      this.currentMemory = result.messages;

      // Post result back to Prolog
      this.postAgentResult(result);
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

    try {
      // Execute tool
      const argsObj = this.argsToObject(args, tool);
      
      // Emit tool_call event before execution
      yield { type: 'tool_call', toolName, toolArgs: argsObj };
      
      const result = await tool.execute(argsObj);

      // Post result back to Prolog
      this.postExecResult({ success: true, result });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
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
    policy: ToolPolicy | null,
    executeToolInline: (toolName: string, args: Record<string, unknown>) => Promise<{ result: unknown; outputs: string[] }>
  ): Map<string, ToolDefinition> {
    const result = new Map<string, ToolDefinition>();

    // Add user-defined tools from DML file with schema info
    // These are the ONLY tools the LLM can call during task()
    for (const tool of userTools) {
      // Check policy for DML tools too
      const check = checkToolPolicy(tool.name, policy);
      if (!check.allowed) {
        continue;
      }

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

      // Build description with source code
      let description = tool.description || `Prolog tool: ${tool.name}`;
      if (tool.source) {
        description += `\n\nProlog implementation:\n${tool.source}`;
      }

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
   * Execute a tool inline via the main Prolog engine
   * Posts execute_tool signal, handles yields (output, exec requests), returns tool_result
   * Tools now share state with the task() that calls them.
   * Uses a mutex to serialize concurrent tool calls (AI SDK may execute multiple tools in parallel).
   */
  private async executeToolInline(
    toolName: string,
    args: Record<string, unknown>,
    registeredTools: Map<string, ToolDefinition>,
    onOutput: (text: string) => void,
    options?: InternalRunOptions
  ): Promise<{ result: unknown; outputs: string[] }> {
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] executeToolInline called for:', toolName, '- depth:', this.toolExecutionDepth);
    }
    
    // If we're already inside a tool execution (nested task()), skip the mutex
    // The mutex is only for parallel calls from the AI SDK, not for nested calls
    if (this.toolExecutionDepth > 0) {
      if (process.env.DEBUG_RUNNER) {
        console.log('[RUNNER] Nested tool call - skipping mutex for:', toolName);
      }
      this.toolExecutionDepth++;
      try {
        return await this.executeToolInlineImpl(toolName, args, registeredTools, onOutput, options);
      } finally {
        this.toolExecutionDepth--;
      }
    }
    
    // Acquire mutex - wait for any previous tool execution to complete
    const previousLock = this.toolExecutionLock;
    let releaseLock: () => void;
    this.toolExecutionLock = new Promise(resolve => { releaseLock = resolve; });
    await previousLock;
    
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] executeToolInline acquired mutex for:', toolName);
    }
    
    this.toolExecutionDepth++;
    try {
      return await this.executeToolInlineImpl(toolName, args, registeredTools, onOutput, options);
    } finally {
      this.toolExecutionDepth--;
      if (process.env.DEBUG_RUNNER) {
        console.log('[RUNNER] executeToolInline releasing mutex for:', toolName);
      }
      releaseLock!();
    }
  }

  /**
   * Internal implementation of executeToolInline (called with mutex held)
   */
  private async executeToolInlineImpl(
    toolName: string,
    args: Record<string, unknown>,
    registeredTools: Map<string, ToolDefinition>,
    onOutput: (text: string) => void,
    options?: InternalRunOptions
  ): Promise<{ result: unknown; outputs: string[] }> {
    const outputs: string[] = [];
    
    // Convert args to Prolog list format
    const sortedArgs = Object.entries(args)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([, v]) => this.toPrologTerm(v));
    
    // Post execute_tool signal to main engine
    const argsListStr = `[${sortedArgs.join(', ')}]`;
    
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] About to post execute_tool signal:', toolName);
    }
    
    try {
      this.query(
        `deepclause_mi:session_engine('${this.sessionId}', Engine),
         engine_post(Engine, execute_tool("${toolName}", ${argsListStr}))`
      );
    } catch (err) {
      console.error('[RUNNER] Failed to post execute_tool:', err);
      return { result: { error: `Failed to execute tool: ${err}` }, outputs };
    }
    
    if (process.env.DEBUG_RUNNER) {
      console.log('[RUNNER] Posted execute_tool signal:', toolName, sortedArgs);
    }
    
    // Step engine and handle yields until we get tool_result
    while (true) {
      const step = this.stepEngine();
      
      if (process.env.DEBUG_RUNNER) {
        console.log('[RUNNER] Tool inline step:', step.status, step.content, step.payload);
      }
      
      switch (step.status) {
        case 'tool_result': {
          // Tool execution completed
          const payload = step.payload as { result: unknown } | undefined;
          return { result: payload?.result, outputs };
        }
        
        case 'output': {
          // Tool body emitted output
          if (step.content) {
            outputs.push(step.content);
            onOutput(step.content);
          }
          break;
        }
        
        case 'log': {
          // Log message - ignore for now
          break;
        }
        
        case 'request_exec': {
          // Tool body needs to call an external tool via exec()
          const payload = step.payload as { toolName: string; args: unknown[] } | undefined;
          if (payload) {
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] request_exec for tool:', payload.toolName);
              console.log('[RUNNER] Available tools:', Array.from(registeredTools.keys()));
            }
            const tool = registeredTools.get(payload.toolName);
            if (tool) {
              const mappedArgs = this.argsToObject(payload.args, tool);
              if (process.env.DEBUG_RUNNER) {
                console.log('[RUNNER] Mapped args for', payload.toolName, ':', mappedArgs);
              }
              try {
                const execResult = await tool.execute(mappedArgs);
                if (process.env.DEBUG_RUNNER) {
                  console.log('[RUNNER] Inline exec result:', execResult);
                }
                // Post result back to engine
                const resultStr = this.toPrologTerm(execResult);
                this.query(
                  `assertz(deepclause_mi:session_exec_result('${this.sessionId}', result{status: success, result: ${resultStr}})),
                   deepclause_mi:session_engine('${this.sessionId}', Engine),
                   engine_post(Engine, exec_done)`
                );
              } catch (error) {
                const errMsg = error instanceof Error ? error.message : String(error);
                this.query(
                  `assertz(deepclause_mi:session_exec_result('${this.sessionId}', result{status: error, result: "${errMsg}"})),
                   deepclause_mi:session_engine('${this.sessionId}', Engine),
                   engine_post(Engine, exec_done)`
                );
              }
            } else {
              this.query(
                `assertz(deepclause_mi:session_exec_result('${this.sessionId}', result{status: error, result: "Tool not found: ${payload.toolName}"})),
                 deepclause_mi:session_engine('${this.sessionId}', Engine),
                 engine_post(Engine, exec_done)`
              );
            }
          }
          break;
        }

        case 'request_agent_loop': {
          // Tool body contains a nested task() - run the agent loop
          if (!options) {
            console.warn('[RUNNER] Cannot run nested task() - no options available');
            return { result: { error: 'Nested task() requires agent loop options' }, outputs };
          }
          
          if (process.env.DEBUG_RUNNER) {
            console.log('[RUNNER] Nested task() in tool - running agent loop');
          }
          
          // Run nested agent loop and collect results
          // Note: This is synchronous from the Prolog perspective - we consume all events
          for await (const event of this.handleAgentLoop(step.payload, '', options)) {
            if (process.env.DEBUG_RUNNER) {
              console.log('[RUNNER] Nested agent loop event:', event.type, event.content?.substring(0, 50));
            }
            if (event.type === 'output' && event.content) {
              outputs.push(event.content);
              onOutput(event.content);
            }
            // Other event types (stream, tool_call, etc.) are also forwarded
          }
          if (process.env.DEBUG_RUNNER) {
            console.log('[RUNNER] Nested agent loop completed, continuing tool execution');
          }
          break;
        }
        
        case 'error': {
          return { result: { error: step.content }, outputs };
        }
        
        case 'finished': {
          // Engine finished without tool_result - shouldn't happen
          return { result: { error: 'Tool execution ended unexpectedly' }, outputs };
        }
        
        default: {
          console.warn('[RUNNER] Unexpected step during tool execution:', step.status);
        }
      }
    }
  }

  /**
   * Extract memory from payload (now passed via state threading)
   */
  private extractMemoryFromPayload(rawPayload: Record<string, unknown>): MemoryMessage[] {
    const rawMemory = toJsValue(rawPayload.memory);
    if (!Array.isArray(rawMemory)) {
      return [];
    }

    return rawMemory.map((m: unknown) => {
      const msg = toJsValue(m) as { role: string; content: string };
      const role = String(toJsValue(msg.role) ?? 'user');
      const content = String(toJsValue(msg.content) ?? '');
      
      // Validate role
      if (role !== 'system' && role !== 'user' && role !== 'assistant') {
        return { role: 'user' as const, content };
      }
      
      return { role: role as 'system' | 'user' | 'assistant', content };
    });
  }

  /**
   * Post agent loop result back to Prolog
   */
  private postAgentResult(result: {
    success: boolean;
    outputs: string[];
    variables: Record<string, unknown>;
    messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>;
  }): void {
    // Keys need to be quoted because they start with uppercase (Var1, Var2, etc.)
    const varsStr = Object.entries(result.variables)
      .map(([k, v]) => `'${k}': ${this.toPrologTerm(v)}`)
      .join(', ');

    // Convert messages to Prolog list format
    const messagesStr = result.messages
      .map(m => `message{role: ${m.role}, content: ${this.toPrologTerm(m.content)}}`)
      .join(', ');

    this.query(
      `deepclause_mi:post_agent_result('${this.sessionId}', ${result.success}, vars{${varsStr}}, [${messagesStr}])`
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
      this.query(
        `deepclause_mi:post_exec_result('${this.sessionId}', failure, "${result.error}")`
      );
    }
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
