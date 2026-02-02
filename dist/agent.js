/**
 * Agent Loop Implementation
 * Runs an LLM agent loop for task() predicate execution
 */
import { generateText, streamText, tool as aiTool, jsonSchema } from 'ai';
import { createModelProvider } from './prolog/bridge.js';
/** Maximum number of retries for LLM error finish reasons */
const MAX_ERROR_RETRIES = 3;
/**
 * Clean Prolog dict markers ($tag, $t) from tool results
 * This makes the data more readable for the LLM
 */
function cleanPrologMarkers(data) {
    if (data === null || data === undefined) {
        return data;
    }
    if (Array.isArray(data)) {
        return data.map(cleanPrologMarkers);
    }
    if (typeof data === 'object') {
        const obj = data;
        const cleaned = {};
        for (const [key, value] of Object.entries(obj)) {
            // Skip Prolog-specific markers
            if (key === '$tag' || key === '$t') {
                continue;
            }
            cleaned[key] = cleanPrologMarkers(value);
        }
        return cleaned;
    }
    return data;
}
/**
 * Extract variable names from task description
 * Looks for patterns like "Store X in VariableName" or "Store it in VariableName"
 */
/**
 * Run an agent loop for a task
 */
export async function runAgentLoop(options) {
    const { taskDescription, outputVars, memory, tools, modelOptions, onOutput, onStream, onToolCall, 
    // onAskUser is now handled via registered tools in the runner
    signal, streaming = false, debug = false, } = options;
    // Debug helper - logs if debug is enabled or DEBUG_AGENT env var is set
    const debugLog = (...args) => {
        if (debug || process.env.DEBUG_AGENT) {
            console.log('[AGENT]', ...args);
        }
    };
    debugLog('Output vars:', outputVars);
    const outputs = [];
    const variables = {};
    let finished = false;
    let success = false;
    let errorRetryCount = 0;
    // Build the AI SDK tools - use CoreTool type for compatibility
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const aiTools = {};
    // Add finish tool
    aiTools['finish'] = aiTool({
        description: 'Complete the task. Call with true for success, false for failure.',
        parameters: jsonSchema({
            type: 'object',
            properties: {
                success: { type: 'boolean', description: 'Whether the task was completed successfully' }
            },
            required: ['success']
        }),
        execute: async (args) => {
            const { success: s } = args;
            finished = true;
            success = s;
            return { finished: true, success: s };
        },
    });
    // Note: ask_user is now handled via DML tool definitions.
    // The internal ask_user functionality is added to registered tools in the runner
    // so that DML tools can call exec(ask_user(...)) to prompt the user.
    // Add set_result tool for output variables
    // outputVars now contains actual variable names from Prolog (e.g., "Industry" not "Var1")
    if (outputVars.length > 0) {
        const varList = outputVars.map((v, i) => `"${v}"${i < outputVars.length - 1 ? ',' : ''}`).join(' ');
        aiTools['set_result'] = aiTool({
            description: `Set a result value for an output variable. You MUST call this tool to return results from the task. Use the exact variable name as specified.`,
            parameters: jsonSchema({
                type: 'object',
                properties: {
                    variable: { type: 'string', description: `The variable name to set. Must be exactly one of: ${varList}` },
                    value: { type: 'string', description: 'The value to set (must be a string)' }
                },
                required: ['variable', 'value']
            }),
            execute: async (args) => {
                const { variable, value } = args;
                if (outputVars.includes(variable)) {
                    variables[variable] = value;
                    return { success: true, variable, value };
                }
                return { success: false, error: `Unknown variable: ${variable}. Must be one of: ${varList}` };
            },
        });
    }
    // Reserved internal tool names that cannot be overwritten by user-defined tools
    const reservedToolNames = ['finish', 'set_result'];
    // Add user-defined tools - all parameters are JSON Schema, wrap with jsonSchema()
    for (const [name, tool] of tools) {
        // Skip tools that would overwrite internal tools - the internal implementation is used instead
        if (reservedToolNames.includes(name)) {
            // Don't log - this is expected when DML declares ask_user to enable the internal tool
            continue;
        }
        aiTools[name] = aiTool({
            description: tool.description,
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            parameters: jsonSchema(tool.parameters),
            execute: async (args) => {
                try {
                    return await tool.execute(args);
                }
                catch (error) {
                    const message = error instanceof Error ? error.message : String(error);
                    return { error: message };
                }
            },
        });
    }
    // Build the base system prompt with task instructions and tools
    const baseSystemPrompt = buildSystemPrompt(taskDescription, outputVars, tools);
    // Extract system context from memory (user-defined system() calls)
    // These will be appended to the base system prompt
    const systemContext = memory
        .filter(m => m.role === 'system' && typeof m.content === 'string')
        .map(m => m.content)
        .join('\n\n');
    // Combine into a single system message
    // User context comes first (sets the persona/context), then task instructions
    const combinedSystemPrompt = systemContext
        ? `${systemContext}\n\n---\n\n${baseSystemPrompt}`
        : baseSystemPrompt;
    // Filter non-system messages from memory for conversation history
    const conversationHistory = memory.filter(m => m.role !== 'system' &&
        typeof m.content === 'string' &&
        ['user', 'assistant'].includes(m.role));
    // Build messages with single system message
    const messages = [
        { role: 'system', content: combinedSystemPrompt },
        ...conversationHistory.map(m => ({
            role: m.role,
            content: m.content
        })),
        { role: 'user', content: `Subtask: ${taskDescription}` },
    ];
    // Debug: log messages being sent
    debugLog('System prompt:', combinedSystemPrompt);
    debugLog('Conversation history:', conversationHistory);
    debugLog('Subtask:', taskDescription);
    // Create model provider
    const model = createModelProvider(modelOptions.provider, modelOptions.model, modelOptions.baseUrl);
    // Agent loop
    const maxIterations = 50;
    let iteration = 0;
    // Helper to allow event loop to breathe
    const tick = () => new Promise(resolve => setTimeout(resolve, 0));
    while (!finished && iteration < maxIterations) {
        iteration++;
        // Debug iteration
        debugLog(`Iteration ${iteration}`);
        // Check for abort
        if (signal?.aborted) {
            break;
        }
        try {
            // Allow event loop to process before making API call
            await tick();
            if (streaming && onStream) {
                // Use streaming mode
                const response = streamText({
                    model,
                    messages,
                    tools: aiTools,
                    toolChoice: 'auto',
                    temperature: modelOptions.temperature,
                    maxTokens: modelOptions.maxTokens,
                    abortSignal: signal,
                });
                // Collect streamed text
                let fullText = '';
                for await (const chunk of response.textStream) {
                    fullText += chunk;
                    onStream(chunk, false);
                }
                // Signal stream done for this iteration
                if (fullText) {
                    onStream('', true);
                }
                // Get final results - must await these promises
                const toolCalls = await response.toolCalls;
                const toolResults = await response.toolResults;
                const finishReason = await response.finishReason;
                // Check if finish was called during tool execution
                if (finished) {
                    debugLog('Finish tool was called, exiting loop');
                    break;
                }
                // Debug response
                debugLog(`Response text: ${fullText || '(empty)'}`);
                debugLog(`Tool calls: ${toolCalls?.length ?? 0}`);
                debugLog(`Finish reason: ${finishReason}`);
                if (toolCalls && toolCalls.length > 0) {
                    debugLog(`Tool call details:`, toolCalls.map(tc => tc.toolName));
                }
                // Log for debugging empty responses
                if (!fullText && (!toolCalls || toolCalls.length === 0)) {
                    debugLog(`Empty response - no text and no tool calls`);
                    // If finish reason is error, try to get more info
                    if (finishReason === 'error') {
                        // Log everything we can about the response
                        let finishMessage = '';
                        try {
                            const warnings = await response.warnings;
                            const usage = await response.usage;
                            // Try to extract the finishMessage from the response body (for Gemini)
                            const responseObj = await response.response;
                            // eslint-disable-next-line @typescript-eslint/no-explicit-any
                            const body = responseObj?.body;
                            finishMessage = body?.candidates?.[0]?.finishMessage || '';
                            debugLog(`Empty response - full object:`, JSON.stringify({
                                text: fullText,
                                toolCalls,
                                finishReason,
                                warnings,
                                usage,
                                response: responseObj,
                            }, null, 2));
                        }
                        catch (e) {
                            debugLog(`ERROR: LLM returned error finish reason. Could not get full response:`, e);
                        }
                        // Check for malformed function call error (Gemini specific)
                        if (finishMessage && finishMessage.includes('Malformed function call')) {
                            debugLog(`Detected malformed function call, adding retry message`);
                            messages.push({
                                role: 'user',
                                content: `ERROR: Your function call was malformed. The error was: "${finishMessage}"

DO NOT write code syntax like print(), default_api.X(), or function(). 
Instead, use the structured tool-calling interface to invoke tools.
Each tool should be called as a separate function call, not embedded in text.

Please try again - invoke the tool correctly using the tool interface.`,
                            });
                            // Don't break - continue the loop to retry
                            continue;
                        }
                        // Retry on general error finish reason (up to errorRetryCount times)
                        errorRetryCount++;
                        if (errorRetryCount <= MAX_ERROR_RETRIES) {
                            debugLog(`ERROR: LLM returned error finish reason (attempt ${errorRetryCount}/${MAX_ERROR_RETRIES}). Retrying...`);
                            // Add a small delay before retry to avoid hammering the API
                            await new Promise(resolve => setTimeout(resolve, 1000 * errorRetryCount));
                            continue;
                        }
                        // Break the loop after max retries
                        debugLog(`ERROR: LLM returned error finish reason. Max retries (${MAX_ERROR_RETRIES}) exceeded.`);
                        outputs.push('Error: LLM API returned an error. Check API key and rate limits.');
                        break;
                    }
                }
                // Process the response
                if (fullText) {
                    outputs.push(fullText);
                    onOutput(fullText);
                    messages.push({ role: 'assistant', content: fullText });
                }
                // Process tool calls - add to message history using proper AI SDK format
                if (toolCalls && toolCalls.length > 0) {
                    // Build assistant message with tool calls
                    const toolCallContents = [];
                    const toolResultContents = [];
                    for (const toolCall of toolCalls) {
                        debugLog(`Tool call: ${toolCall.toolName}`, JSON.stringify(toolCall.args));
                        // Emit tool call event
                        if (onToolCall) {
                            onToolCall(toolCall.toolName, toolCall.args);
                        }
                        const toolResultObj = toolResults?.[toolCalls.indexOf(toolCall)];
                        // Extract the actual result from the tool result object
                        const rawResult = toolResultObj?.result ?? toolResultObj;
                        // Clean Prolog markers ($tag, $t) from the result
                        const toolResult = cleanPrologMarkers(rawResult);
                        debugLog(`Tool result:`, toolResult !== undefined ? JSON.stringify(toolResult).substring(0, 500) : '(undefined)');
                        const callId = toolCall.toolCallId || `call_${Date.now()}_${Math.random().toString(36).slice(2)}`;
                        toolCallContents.push({
                            type: 'tool-call',
                            toolCallId: callId,
                            toolName: toolCall.toolName,
                            args: toolCall.args,
                        });
                        toolResultContents.push({
                            type: 'tool-result',
                            toolCallId: callId,
                            toolName: toolCall.toolName,
                            result: toolResult ?? 'No result returned',
                        });
                    }
                    // Add assistant message with tool calls
                    messages.push({
                        role: 'assistant',
                        content: toolCallContents,
                    });
                    // Add tool results
                    messages.push({
                        role: 'tool',
                        content: toolResultContents,
                    });
                    // Break immediately if finish was called
                    if (finished) {
                        break;
                    }
                }
                // If no tool calls were made, prompt the agent to take action
                if (!toolCalls || toolCalls.length === 0) {
                    if (!finished) {
                        // Check if the model wrote text that looks like a tool call
                        if (fullText && /called\s+tool|calling\s+tool|finish\s*\(|set_result\s*\(|TOOL_INVOKED|\[TOOL_/i.test(fullText)) {
                            messages.push({
                                role: 'user',
                                content: 'ERROR: You wrote text about calling a tool instead of actually invoking it. You must USE the tool by making a proper tool call, not by writing about it. Try again - invoke the tool directly.',
                            });
                        }
                        else {
                            messages.push({
                                role: 'user',
                                content: 'Please take action using the available tools, or call finish() when done.',
                            });
                        }
                    }
                }
            }
            else {
                // Non-streaming mode (original behavior)
                const response = await generateText({
                    model,
                    messages,
                    tools: aiTools,
                    toolChoice: 'auto',
                    temperature: modelOptions.temperature,
                    maxTokens: modelOptions.maxTokens,
                    abortSignal: signal,
                });
                // Check if finish was called during tool execution
                if (finished) {
                    debugLog('Finish tool was called, exiting loop');
                    break;
                }
                // Debug response
                debugLog(`Response text: ${response.text || '(empty)'}`);
                debugLog(`Tool calls: ${response.toolCalls?.length ?? 0}`);
                debugLog(`Finish reason: ${response.finishReason}`);
                if (response.toolCalls && response.toolCalls.length > 0) {
                    debugLog(`Tool call details:`, response.toolCalls.map(tc => tc.toolName));
                }
                // Log full response object for debugging empty responses
                if (!response.text && (!response.toolCalls || response.toolCalls.length === 0)) {
                    // Try to extract the finishMessage from the response body (for Gemini)
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    const body = response.response?.body;
                    const finishMessage = body?.candidates?.[0]?.finishMessage || '';
                    debugLog(`Empty response - full object:`, JSON.stringify({
                        text: response.text,
                        toolCalls: response.toolCalls,
                        finishReason: response.finishReason,
                        warnings: response.warnings,
                        usage: response.usage,
                        response: response.response,
                    }, null, 2));
                    // If finish reason is error, check for recoverable errors
                    if (response.finishReason === 'error') {
                        // Check for malformed function call error (Gemini specific)
                        if (finishMessage && finishMessage.includes('Malformed function call')) {
                            debugLog(`Detected malformed function call, adding retry message`);
                            messages.push({
                                role: 'user',
                                content: `ERROR: Your function call was malformed. The error was: "${finishMessage}"

DO NOT write code syntax like print(), default_api.X(), or function(). 
Instead, use the structured tool-calling interface to invoke tools.
Each tool should be called as a separate function call, not embedded in text.

Please try again - invoke the tool correctly using the tool interface.`,
                            });
                            // Don't break - continue the loop to retry
                            continue;
                        }
                        // Retry on general error finish reason (up to errorRetryCount times)
                        errorRetryCount++;
                        if (errorRetryCount <= MAX_ERROR_RETRIES) {
                            debugLog(`ERROR: LLM returned error finish reason (attempt ${errorRetryCount}/${MAX_ERROR_RETRIES}). Retrying...`);
                            // Add a small delay before retry to avoid hammering the API
                            await new Promise(resolve => setTimeout(resolve, 1000 * errorRetryCount));
                            continue;
                        }
                        // Break the loop after max retries
                        debugLog(`ERROR: LLM returned error finish reason. Max retries (${MAX_ERROR_RETRIES}) exceeded.`);
                        outputs.push('Error: LLM API returned an error. Check API key and rate limits.');
                        break;
                    }
                }
                // Process the response
                if (response.text) {
                    outputs.push(response.text);
                    onOutput(response.text);
                    messages.push({ role: 'assistant', content: response.text });
                }
                // Process tool calls - add to message history using proper AI SDK format
                if (response.toolCalls && response.toolCalls.length > 0) {
                    debugLog(`response.toolResults type:`, typeof response.toolResults);
                    debugLog(`response.toolResults:`, JSON.stringify(response.toolResults));
                    // Build assistant message with tool calls
                    const toolCallContents = [];
                    const toolResultContents = [];
                    for (const toolCall of response.toolCalls) {
                        debugLog(`Tool call: ${toolCall.toolName}`, JSON.stringify(toolCall.args));
                        // Emit tool call event
                        if (onToolCall) {
                            onToolCall(toolCall.toolName, toolCall.args);
                        }
                        // Find the tool result - toolResults is an array of results
                        const toolResultObj = response.toolResults?.[response.toolCalls.indexOf(toolCall)];
                        debugLog(`Tool result object (full):`, JSON.stringify(toolResultObj));
                        debugLog(`Tool result object keys:`, toolResultObj ? Object.keys(toolResultObj) : 'null');
                        // Extract the actual result from the tool result object
                        // The AI SDK returns {type, toolCallId, toolName, args, result}
                        const rawResult = toolResultObj?.result ?? toolResultObj;
                        // Clean Prolog markers ($tag, $t) from the result
                        const toolResult = cleanPrologMarkers(rawResult);
                        debugLog(`Tool result (extracted):`, toolResult !== undefined ? JSON.stringify(toolResult).substring(0, 500) : '(undefined)');
                        const callId = toolCall.toolCallId || `call_${Date.now()}_${Math.random().toString(36).slice(2)}`;
                        toolCallContents.push({
                            type: 'tool-call',
                            toolCallId: callId,
                            toolName: toolCall.toolName,
                            args: toolCall.args,
                        });
                        toolResultContents.push({
                            type: 'tool-result',
                            toolCallId: callId,
                            toolName: toolCall.toolName,
                            result: toolResult ?? 'No result returned',
                        });
                    }
                    // Add assistant message with tool calls
                    messages.push({
                        role: 'assistant',
                        content: toolCallContents,
                    });
                    // Add tool results
                    messages.push({
                        role: 'tool',
                        content: toolResultContents,
                    });
                    // Break immediately if finish was called
                    if (finished) {
                        break;
                    }
                }
                // If no tool calls were made, prompt the agent to take action
                if (!response.toolCalls || response.toolCalls.length === 0) {
                    if (!finished) {
                        // Check if the model wrote text that looks like a tool call
                        if (response.text && /called\s+tool|calling\s+tool|finish\s*\(|set_result\s*\(|TOOL_INVOKED|\[TOOL_/i.test(response.text)) {
                            messages.push({
                                role: 'user',
                                content: 'ERROR: You wrote text about calling a tool instead of actually invoking it. You must USE the tool by making a proper tool call, not by writing about it. Try again - invoke the tool directly.',
                            });
                        }
                        else {
                            messages.push({
                                role: 'user',
                                content: 'Please take action using the available tools, or call finish() when done.',
                            });
                        }
                    }
                }
            }
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            outputs.push(`Error: ${message}`);
            break;
        }
    }
    // If we hit max iterations without finishing, fail
    if (!finished) {
        success = false;
        outputs.push('Agent loop reached maximum iterations without completing');
    }
    // For memory persistence between tasks, keep the conversation flow:
    // 1. Previous conversation history (from memory)
    // 2. Current task as user message
    // 3. Summary of result as assistant message
    // Filter out internal tool call/result messages which are implementation details
    const persistentMessages = [];
    // Keep previous conversation history
    for (const m of conversationHistory) {
        if (m.role === 'user' || m.role === 'assistant') {
            persistentMessages.push({
                role: m.role,
                content: typeof m.content === 'string' ? m.content : JSON.stringify(m.content),
            });
        }
    }
    // Add the current subtask
    persistentMessages.push({
        role: 'user',
        content: `Subtask: ${taskDescription}`,
    });
    // Add a summary of the result
    if (success && Object.keys(variables).length > 0) {
        // If we have stored variables, include them
        const varSummary = Object.entries(variables)
            .map(([k, v]) => `${k}: ${v}`)
            .join(', ');
        persistentMessages.push({
            role: 'assistant',
            content: `Task completed. Results: ${varSummary}`,
        });
    }
    else if (success && outputs.length > 0) {
        // For task/1 without variables, include the actual LLM output
        // Filter out error messages and take the last meaningful output
        const meaningfulOutputs = outputs.filter(o => !o.startsWith('Error:') &&
            !o.includes('maximum iterations') &&
            o.trim().length > 0);
        if (meaningfulOutputs.length > 0) {
            persistentMessages.push({
                role: 'assistant',
                content: meaningfulOutputs.join('\n'),
            });
        }
        else {
            persistentMessages.push({
                role: 'assistant',
                content: 'Task completed successfully.',
            });
        }
    }
    else if (success) {
        persistentMessages.push({
            role: 'assistant',
            content: 'Task completed successfully.',
        });
    }
    debugLog('Persistent messages for next task:', persistentMessages.length, 'messages');
    return {
        success,
        outputs,
        variables,
        messages: persistentMessages,
    };
}
/**
 * Build the system prompt for the agent
 */
function buildSystemPrompt(taskDescription, outputVars, tools) {
    const hasAskUser = tools.has('ask_user');
    const parts = [
        `You are an AI agent executing a subtask within a larger workflow. Your job is to complete the following subtask:`,
        '',
        `Subtask: ${taskDescription}`,
        '',
        'Available tools:',
        '',
        '- finish(success: boolean): Signal task completion. Call finish(true) when done successfully, or finish(false) if the task cannot be completed.',
    ];
    // Only include ask_user if it's declared in the DML
    if (hasAskUser) {
        parts.push('- ask_user(prompt: string): Ask the user for input or clarification.');
    }
    if (outputVars.length > 0) {
        const varList = outputVars.map(v => `"${v}"`).join(', ');
        parts.push(`- set_result(variable: string, value: string): Set a result value for an output variable. The variable parameter must be exactly one of: ${varList}. The value must be a simple string.`);
    }
    // Add user-defined tools, skipping reserved names that we handle internally
    for (const [name, tool] of tools) {
        if (name === 'ask_user')
            continue; // Already documented above
        parts.push(`- ${name}: ${tool.description}`);
    }
    parts.push('');
    parts.push('Workflow:');
    parts.push('1. Analyze the task and gather any needed information using available tools.');
    if (outputVars.length > 0) {
        const varList = outputVars.map(v => `"${v}"`).join(', ');
        parts.push(`2. Once you have the answer, call set_result() for each required variable: ${varList}`);
        parts.push('3. Immediately after setting results, call finish(true) to complete the task.');
    }
    else {
        parts.push('2. Once the task is complete, call finish(true).');
    }
    parts.push('');
    parts.push('If you determine the task cannot be completed, call finish(false) immediately.');
    parts.push('');
    parts.push('CRITICAL INSTRUCTIONS:');
    parts.push('- You MUST use the structured tool-calling interface to invoke tools.');
    parts.push('- NEVER write code syntax like print(), default_api.X(), or function_name() in your text response.');
    parts.push('- NEVER describe tool calls in text - actually invoke them using the tool interface.');
    parts.push('- Each tool call should be a separate structured function call, not embedded in text.');
    parts.push('- When storing values, pass simple strings without code formatting or escaping.');
    parts.push('');
    parts.push('EXAMPLES OF CORRECT TOOL USAGE:');
    parts.push('');
    parts.push('To complete a task successfully, invoke the finish tool with:');
    parts.push('  Tool: finish');
    parts.push('  Arguments: { "success": true }');
    parts.push('');
    if (outputVars.length > 0) {
        parts.push('To set a result value, invoke the set_result tool with:');
        parts.push('  Tool: set_result');
        parts.push(`  Arguments: { "variable": "${outputVars[0]}", "value": "your result here" }`);
        parts.push('');
    }
    if (hasAskUser) {
        parts.push('To ask the user a question, invoke the ask_user tool with:');
        parts.push('  Tool: ask_user');
        parts.push('  Arguments: { "prompt": "What would you like me to do?" }');
        parts.push('');
    }
    parts.push('WRONG (do NOT do this):');
    parts.push('- print(finish(success=true))');
    parts.push('- default_api.set_result(variable="X", value="Y")');
    parts.push('- I will now call finish(true)');
    parts.push('- ```finish(success=true)```');
    return parts.join('\n');
}
//# sourceMappingURL=agent.js.map