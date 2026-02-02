/**
 * DML Runner - Executes DML code using SWI-Prolog WASM
 */
import { mountWorkspace } from './prolog/loader.js';
import { runAgentLoop } from './agent.js';
import { checkToolPolicy } from './tools.js';
/**
 * Convert swipl-wasm value to plain JavaScript value
 * Handles PrologString, PrologAtom, PrologList, etc.
 */
function toJsValue(value) {
    if (value === null || value === undefined) {
        return value;
    }
    // Handle arrays FIRST (before checking toString)
    if (Array.isArray(value)) {
        return value.map(toJsValue);
    }
    // Check for swipl-wasm special types with $t marker
    if (typeof value === 'object' && value !== null && '$t' in value) {
        const obj = value;
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
        typeof value.toString === 'function') {
        const str = value.toString();
        // Only return string if it's not the default [object Object]
        if (!str.startsWith('[object ')) {
            return str;
        }
    }
    // Handle plain objects (recursively convert)
    if (typeof value === 'object' && value !== null) {
        const result = {};
        for (const [k, v] of Object.entries(value)) {
            result[k] = toJsValue(v);
        }
        return result;
    }
    return value;
}
/**
 * Map Prolog types to JSON Schema types
 */
function prologTypeToJsonType(prologType) {
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
    swipl;
    options;
    engine = null;
    sessionId = '';
    currentMemory = [];
    // Proper mutex for serializing tool execution (AI SDK may call tools in parallel)
    toolMutexLocked = false;
    toolMutexQueue = [];
    // Depth counter for re-entrant tool execution (nested task() calls)
    toolExecutionDepth = 0;
    constructor(swipl, options) {
        this.swipl = swipl;
        this.options = options;
    }
    /**
     * Get the current conversation memory
     */
    getMemory() {
        return [...this.currentMemory];
    }
    /**
     * Run DML code and yield events
     */
    async *run(code, options) {
        // Generate unique session ID
        this.sessionId = `sess_${Date.now()}_${Math.random().toString(36).slice(2)}`;
        const memoryId = `mem_${this.sessionId}`; // Kept for API compatibility
        try {
            // Mount workspace if path provided
            if (options.workspacePath) {
                try {
                    mountWorkspace(this.swipl, options.workspacePath);
                }
                catch (err) {
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
                        yield* this.handleAgentLoop(step.payload, memoryId, options);
                        break;
                    case 'request_exec':
                        // Execute external tool
                        yield* this.handleExec(step.payload, options);
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
                            const traceData = toJsValue(step.payload.trace);
                            yield { type: 'finished', trace: Array.isArray(traceData) ? traceData : undefined };
                        }
                        else {
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
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            yield { type: 'error', content: message };
        }
        finally {
            // Cleanup
            this.cleanup();
        }
    }
    /**
     * Parse DML code into Prolog clauses
     */
    parseCode(code, memoryId, params) {
        try {
            // Write code to temp file in WASM filesystem
            const tempPath = `/tmp/dml_${Date.now()}.pl`;
            this.swipl.FS.writeFile(tempPath, code);
            // Parse using meta-interpreter, passing params to assert as facts
            const result = this.query(`deepclause_mi:parse_dml('${tempPath}', '${this.sessionId}', '${memoryId}', ${params}, Error)`);
            if (result && result.Error && result.Error !== 'none') {
                return { error: String(result.Error) };
            }
            return {};
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            return { error: `Parse error: ${message}` };
        }
    }
    /**
     * Build args list for Prolog (positional arguments for agent_main)
     */
    buildArgs(options) {
        const args = options.args ?? [];
        const items = args.map(v => this.toPrologTerm(v)).join(', ');
        return `[${items}]`;
    }
    /**
     * Build params dictionary for Prolog (named parameters)
     */
    buildParams(options) {
        const params = {
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
    toPrologTerm(value) {
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
    createEngine(memoryId, args, params) {
        try {
            const result = this.query(`deepclause_mi:create_engine('${this.sessionId}', '${memoryId}', ${args}, ${params}, Engine)`);
            if (result && result.Engine) {
                this.engine = result.Engine;
                return {};
            }
            return { error: 'Failed to create engine' };
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            return { error: `Engine creation failed: ${message}` };
        }
    }
    /**
     * Step the cooperative engine
     */
    stepEngine() {
        try {
            const result = this.query(`deepclause_mi:step_engine('${this.sessionId}', Status, Content, Payload)`);
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
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            return { status: 'error', content: message };
        }
    }
    /**
     * Handle agent loop request (task() predicate)
     */
    async *handleAgentLoop(payload, _memoryId, // No longer used - memory comes from payload
    options) {
        // Safely extract payload fields with proper conversion
        const rawPayload = payload;
        const taskDescription = String(toJsValue(rawPayload.taskDescription) ?? '');
        // Ensure outputVars is an array
        let outputVars = [];
        const rawOutputVars = toJsValue(rawPayload.outputVars);
        if (Array.isArray(rawOutputVars)) {
            outputVars = rawOutputVars.map(v => String(v));
        }
        // Parse userTools - now contains schema info as array of tool_info dicts
        const userTools = [];
        const rawUserTools = toJsValue(rawPayload.userTools);
        if (process.env.DEBUG_RUNNER) {
            console.log('[RUNNER] Raw userTools from Prolog:', JSON.stringify(rawUserTools, null, 2));
        }
        if (Array.isArray(rawUserTools)) {
            for (const tool of rawUserTools) {
                const toolObj = toJsValue(tool);
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
        const onToolOutput = (text) => {
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
                    const argsObj = args;
                    const prompt = (argsObj.prompt ?? argsObj.arg1 ?? Object.values(argsObj).find(v => typeof v === 'string'));
                    if (!prompt) {
                        return { error: 'No prompt provided to ask_user' };
                    }
                    try {
                        const response = await options.onInputRequired(prompt);
                        return { user_response: response };
                    }
                    catch (err) {
                        return { error: String(err) };
                    }
                },
            });
        }
        // Build available tools for agent
        const availableTools = this.buildAgentTools(userTools, options.toolPolicy, 
        // executeToolInline callback - runs tool in main engine with shared state
        async (toolName, args) => {
            return this.executeToolInline(toolName, args, augmentedTools, onToolOutput, options);
        });
        // Queue for streaming events
        const streamQueue = [];
        let streamResolve = null;
        // Run agent loop with streaming support
        const resultPromise = runAgentLoop({
            taskDescription,
            outputVars,
            memory,
            tools: availableTools,
            modelOptions: this.options,
            streaming: this.options.streaming,
            debug: this.options.debug,
            onOutput: (_text) => { },
            onStream: (chunk, done) => {
                streamQueue.push({ type: 'stream', content: chunk, done });
                if (streamResolve) {
                    streamResolve();
                    streamResolve = null;
                }
            },
            onToolCall: (toolName, args) => {
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
                    yield streamQueue.shift();
                }
                // Check if result is ready
                const raceResult = await Promise.race([
                    resultPromise.then(r => ({ type: 'done', result: r })),
                    new Promise(resolve => {
                        streamResolve = () => resolve({ type: 'stream' });
                    }),
                ]);
                if (raceResult.type === 'done') {
                    // Drain remaining queue
                    while (streamQueue.length > 0) {
                        yield streamQueue.shift();
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
        }
        else {
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
                yield streamQueue.shift();
            }
            if (process.env.DEBUG_QUEUE) {
                console.log('[RUNNER] Non-streaming agent result.outputs:', result.outputs);
            }
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
    async *handleExec(payload, options) {
        const { toolName, args } = payload;
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
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            yield { type: 'log', content: `Tool '${toolName}' error: ${message}` };
            this.postExecResult({ success: false, error: message });
        }
    }
    /**
     * Convert positional args to object based on tool schema
     */
    argsToObject(args, tool) {
        if (process.env.DEBUG_RUNNER) {
            console.log('[RUNNER] argsToObject input:', JSON.stringify(args));
        }
        // Helper to unwrap Prolog values (PrologString, etc.)
        const unwrap = (val) => {
            if (val && typeof val === 'object') {
                const obj = val;
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
        const isDictLike = (obj) => {
            const keys = Object.keys(obj);
            // Skip if it's a Prolog tagged value
            if ('$t' in obj || '$tag' in obj)
                return false;
            // Has at least one regular key
            return keys.some(k => !k.startsWith('$'));
        };
        // Helper to extract named args from Prolog key:value terms
        // Format: { '$t': 't', ':': [['key', value]] } or similar
        const extractNamedArgs = (args) => {
            const result = {};
            let foundNamed = false;
            for (const arg of args) {
                if (arg && typeof arg === 'object') {
                    const obj = arg;
                    // Handle { '$t': 't', ':': [['name', value]] }
                    if (obj['$t'] === 't' && ':' in obj) {
                        const colonData = obj[':'];
                        if (Array.isArray(colonData) && colonData.length > 0) {
                            const [name, value] = colonData[0];
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
            const obj = args[0];
            if (isDictLike(obj)) {
                // It's already a dict, unwrap values and return
                const result = {};
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
        let keys = [];
        const params = tool.parameters;
        // Check for JSON Schema properties
        if (params.properties && typeof params.properties === 'object') {
            keys = Object.keys(params.properties);
        }
        // Check for Zod schema shape
        else if (params._def && typeof params._def === 'object') {
            const def = params._def;
            if (typeof def.shape === 'function') {
                keys = Object.keys(def.shape());
            }
        }
        // Try zodToJsonSchema shape property directly
        else if (params.shape && typeof params.shape === 'object') {
            keys = Object.keys(params.shape);
        }
        if (process.env.DEBUG_RUNNER) {
            console.log('[RUNNER] Tool parameter keys:', keys);
        }
        if (keys.length > 0) {
            const result = {};
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
    parseUserToolInfo(toolObj) {
        const name = String(toJsValue(toolObj.name) ?? '');
        if (!name)
            return null;
        const schema = toJsValue(toolObj.schema);
        const source = String(toJsValue(toolObj.source) ?? '');
        let description = '';
        const inputs = [];
        const outputs = [];
        if (schema) {
            description = String(toJsValue(schema.description) ?? '');
            const rawInputs = toJsValue(schema.inputs);
            if (Array.isArray(rawInputs)) {
                for (const input of rawInputs) {
                    const inputObj = toJsValue(input);
                    inputs.push({
                        name: String(toJsValue(inputObj.name) ?? ''),
                        type: String(toJsValue(inputObj.type) ?? 'string'),
                    });
                }
            }
            const rawOutputs = toJsValue(schema.outputs);
            if (Array.isArray(rawOutputs)) {
                for (const output of rawOutputs) {
                    const outputObj = toJsValue(output);
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
    buildAgentTools(userTools, policy, executeToolInline) {
        const result = new Map();
        // Add user-defined tools from DML file with schema info
        // These are the ONLY tools the LLM can call during task()
        for (const tool of userTools) {
            // Check policy for DML tools too
            const check = checkToolPolicy(tool.name, policy);
            if (!check.allowed) {
                continue;
            }
            // Build JSON Schema properties from inputs
            const properties = {};
            const required = [];
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
     * Acquire the tool execution mutex
     * Returns a release function to call when done
     */
    acquireToolMutex() {
        return new Promise(resolve => {
            const tryAcquire = () => {
                if (!this.toolMutexLocked) {
                    this.toolMutexLocked = true;
                    resolve(() => {
                        this.toolMutexLocked = false;
                        const next = this.toolMutexQueue.shift();
                        if (next) {
                            // Use setImmediate/setTimeout to avoid stack overflow with many queued tools
                            setTimeout(next, 0);
                        }
                    });
                }
                else {
                    this.toolMutexQueue.push(tryAcquire);
                }
            };
            tryAcquire();
        });
    }
    /**
     * Execute a tool inline via the main Prolog engine
     * Posts execute_tool signal, handles yields (output, exec requests), returns tool_result
     * Tools now share state with the task() that calls them.
     * Uses a mutex to serialize concurrent tool calls (AI SDK may execute multiple tools in parallel).
     */
    async executeToolInline(toolName, args, registeredTools, onOutput, options) {
        const timestamp = () => new Date().toISOString().substr(11, 12);
        if (process.env.DEBUG_RUNNER) {
            console.log('[RUNNER] executeToolInline called for:', toolName, '- depth:', this.toolExecutionDepth);
        }
        // Always log tool execution start for debugging parallel issues
        if (process.env.DEBUG_TOOLS) {
            console.log(`[TOOLS ${timestamp()}] >>> ENTER executeToolInline: ${toolName} (depth=${this.toolExecutionDepth}, queueLen=${this.toolMutexQueue.length}, locked=${this.toolMutexLocked})`);
        }
        // The mutex is only for parallel calls from the AI SDK, not for nested calls
        if (this.toolExecutionDepth > 0) {
            if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
                console.log(`[TOOLS ${timestamp()}] Nested tool call - skipping mutex for: ${toolName}`);
            }
            this.toolExecutionDepth++;
            try {
                return await this.executeToolInlineImpl(toolName, args, registeredTools, onOutput, options);
            }
            finally {
                this.toolExecutionDepth--;
                if (process.env.DEBUG_TOOLS) {
                    console.log(`[TOOLS ${timestamp()}] <<< EXIT (nested) executeToolInline: ${toolName}`);
                }
            }
        }
        // Acquire mutex - wait for any previous tool execution to complete
        if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
            console.log(`[TOOLS ${timestamp()}] Tool ${toolName} waiting for mutex... (queueLen=${this.toolMutexQueue.length})`);
        }
        const releaseMutex = await this.acquireToolMutex();
        if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
            console.log(`[TOOLS ${timestamp()}] Tool ${toolName} ACQUIRED mutex`);
        }
        this.toolExecutionDepth++;
        try {
            if (process.env.DEBUG_TOOLS) {
                console.log(`[TOOLS ${timestamp()}] Tool ${toolName} STARTING execution`);
            }
            const result = await this.executeToolInlineImpl(toolName, args, registeredTools, onOutput, options);
            if (process.env.DEBUG_TOOLS) {
                console.log(`[TOOLS ${timestamp()}] Tool ${toolName} COMPLETED with result:`, JSON.stringify(result.result).substring(0, 100));
            }
            return result;
        }
        finally {
            this.toolExecutionDepth--;
            if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
                console.log(`[TOOLS ${timestamp()}] Tool ${toolName} RELEASING mutex`);
            }
            releaseMutex();
        }
    }
    /**
     * Internal implementation of executeToolInline (called with mutex held)
     */
    async executeToolInlineImpl(toolName, args, registeredTools, onOutput, options) {
        const outputs = [];
        const timestamp = () => new Date().toISOString().substr(11, 12);
        if (process.env.DEBUG_TOOLS) {
            console.log(`[TOOLS ${timestamp()}] executeToolInlineImpl START: ${toolName}`);
        }
        // Convert args to Prolog list format
        const sortedArgs = Object.entries(args)
            .sort(([a], [b]) => a.localeCompare(b))
            .map(([, v]) => this.toPrologTerm(v));
        // Post execute_tool signal to main engine
        const argsListStr = `[${sortedArgs.join(', ')}]`;
        if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
            console.log(`[TOOLS ${timestamp()}] About to post execute_tool signal: ${toolName}`);
        }
        // ALWAYS use dynamic predicate for signaling - engine_post has reliability issues
        // with WASM engines when multiple signals are posted in quick succession
        const signalResult = this.query(`assertz(deepclause_mi:session_pending_signal('${this.sessionId}', execute_tool("${toolName}", ${argsListStr})))`);
        if (signalResult === null) {
            console.error(`[TOOLS ${timestamp()}] Failed to post signal for ${toolName}`);
            return { result: { error: `Failed to execute tool: signal assertion failed` }, outputs };
        }
        if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
            console.log(`[TOOLS ${timestamp()}] Signal posted for: ${toolName}`);
        }
        if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
            console.log(`[TOOLS ${timestamp()}] Posted signal, now stepping engine for: ${toolName}`);
        }
        // Step engine and handle yields until we get tool_result
        let stepCount = 0;
        while (true) {
            stepCount++;
            const step = this.stepEngine();
            if (process.env.DEBUG_TOOLS || process.env.DEBUG_RUNNER) {
                console.log(`[TOOLS ${timestamp()}] Tool ${toolName} step #${stepCount}: status=${step.status}, content=${step.content?.substring(0, 50)}`);
            }
            switch (step.status) {
                case 'tool_result': {
                    // Tool execution completed
                    const payload = step.payload;
                    if (process.env.DEBUG_TOOLS) {
                        console.log(`[TOOLS ${timestamp()}] Tool ${toolName} got tool_result, returning`);
                    }
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
                    const payload = step.payload;
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
                                this.postExecDone('success', execResult);
                            }
                            catch (error) {
                                const errMsg = error instanceof Error ? error.message : String(error);
                                console.error(`${payload.toolName} failed:`, error);
                                this.postExecDone('error', errMsg);
                            }
                        }
                        else {
                            this.postExecDone('error', `Tool not found: ${payload.toolName}`);
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
                case 'answer': {
                    // Tool body called answer() - treat as a tool result
                    // The answer text becomes the result
                    return { result: step.content ?? true, outputs };
                }
                case 'finished': {
                    // Engine finished without tool_result - shouldn't happen
                    return { result: { error: 'Tool execution ended unexpectedly' }, outputs };
                }
                default: {
                    // Unknown step - return error to prevent infinite loop
                    console.warn('[RUNNER] Unexpected step during tool execution:', step.status);
                    return { result: { error: `Unexpected step: ${step.status}` }, outputs };
                }
            }
        }
    }
    /**
     * Extract memory from payload (now passed via state threading)
     */
    extractMemoryFromPayload(rawPayload) {
        const rawMemory = toJsValue(rawPayload.memory);
        if (!Array.isArray(rawMemory)) {
            return [];
        }
        return rawMemory.map((m) => {
            const msg = toJsValue(m);
            const role = String(toJsValue(msg.role) ?? 'user');
            const content = String(toJsValue(msg.content) ?? '');
            // Validate role
            if (role !== 'system' && role !== 'user' && role !== 'assistant') {
                return { role: 'user', content };
            }
            return { role: role, content };
        });
    }
    /**
     * Post agent loop result back to Prolog
     */
    postAgentResult(result) {
        // Keys need to be quoted because they start with uppercase (Var1, Var2, etc.)
        const varsStr = Object.entries(result.variables)
            .map(([k, v]) => `'${k}': ${this.toPrologTerm(v)}`)
            .join(', ');
        // Convert messages to Prolog list format
        const messagesStr = result.messages
            .map(m => `message{role: ${m.role}, content: ${this.toPrologTerm(m.content)}}`)
            .join(', ');
        this.query(`deepclause_mi:post_agent_result('${this.sessionId}', ${result.success}, vars{${varsStr}}, [${messagesStr}])`);
    }
    /**
     * Post exec result back to Prolog
     */
    postExecResult(result) {
        if (result.success) {
            const termStr = this.toPrologTerm(result.result);
            this.query(`deepclause_mi:post_exec_result('${this.sessionId}', success, ${termStr})`);
        }
        else {
            const escapedErr = this.toPrologTerm(result.error ?? 'Unknown error');
            this.query(`deepclause_mi:post_exec_result('${this.sessionId}', failure, ${escapedErr})`);
        }
    }
    /**
     * Post exec_done signal after tool execution (from exec/2 in DML).
     * Uses post_signal_to_engine helper which has fallback to session_pending_signal.
     */
    postExecDone(status, result) {
        const resultStr = this.toPrologTerm(result);
        // Try to assert result and post signal using the Prolog helper (has fallback)
        try {
            this.query(`retractall(deepclause_mi:session_exec_result('${this.sessionId}', _)),
         assertz(deepclause_mi:session_exec_result('${this.sessionId}', result{status: ${status}, result: ${resultStr}})),
         deepclause_mi:post_signal_to_engine('${this.sessionId}', exec_done)`);
        }
        catch (err) {
            // If the combined query fails, try to at least post exec_done with an error
            console.error('[RUNNER] Failed to post exec result:', err);
            try {
                // Fallback: assert a simple error result and post exec_done via helper
                const fallbackErr = this.toPrologTerm('Internal error posting result');
                this.query(`retractall(deepclause_mi:session_exec_result('${this.sessionId}', _)),
           assertz(deepclause_mi:session_exec_result('${this.sessionId}', result{status: error, result: ${fallbackErr}})),
           deepclause_mi:post_signal_to_engine('${this.sessionId}', exec_done)`);
            }
            catch (fallbackErr) {
                // Last resort: try using the dynamic predicate directly
                console.error('[RUNNER] Fallback also failed:', fallbackErr);
                try {
                    this.query(`assertz(deepclause_mi:session_pending_signal('${this.sessionId}', exec_done))`);
                }
                catch {
                    console.error('[RUNNER] Could not post exec_done - engine may be in bad state');
                }
            }
        }
    }
    /**
     * Provide user input to waiting Prolog
     */
    provideInput(input) {
        const escaped = input.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
        this.query(`deepclause_mi:provide_input('${this.sessionId}', "${escaped}")`);
    }
    /**
     * Execute a Prolog query
     */
    query(goal) {
        try {
            const q = this.swipl.prolog.query(goal);
            const result = q.once();
            return result;
        }
        catch (error) {
            console.error(`Prolog query failed: ${goal}`, error);
            return null;
        }
    }
    /**
     * Cleanup resources
     */
    cleanup() {
        if (this.engine) {
            try {
                this.query(`deepclause_mi:destroy_engine('${this.sessionId}')`);
            }
            catch {
                // Ignore cleanup errors
            }
            this.engine = null;
        }
    }
    /**
     * Dispose runner
     */
    async dispose() {
        this.cleanup();
    }
}
//# sourceMappingURL=runner.js.map