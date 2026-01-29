/**
 * Agent Loop Implementation
 * Runs an LLM agent loop for task() predicate execution
 */
import { generateText, streamText, tool as aiTool, jsonSchema } from 'ai';
import { z } from 'zod';
import { createModelProvider } from './prolog/bridge.js';
/**
 * Check if parameters is a Zod schema
 */
function isZodSchema(params) {
    return params !== null &&
        typeof params === 'object' &&
        '_def' in params &&
        typeof params._def === 'object' &&
        params._def !== null &&
        'typeName' in params._def;
}
/**
 * Run an agent loop for a task
 */
export async function runAgentLoop(options) {
    const { taskDescription, outputVars, memory, tools, modelOptions, onOutput, onStream, onAskUser, signal, streaming = false, debug = false, } = options;
    // Debug helper - logs if debug is enabled or DEBUG_AGENT env var is set
    const debugLog = (...args) => {
        if (debug || process.env.DEBUG_AGENT) {
            console.log('[AGENT]', ...args);
        }
    };
    const outputs = [];
    const variables = {};
    let finished = false;
    let success = false;
    // Build the AI SDK tools - use CoreTool type for compatibility
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const aiTools = {};
    // Add finish tool
    aiTools['finish'] = aiTool({
        description: 'Complete the task. Call with true for success, false for failure.',
        parameters: z.object({
            success: z.boolean().describe('Whether the task was completed successfully'),
        }),
        execute: async ({ success: s }) => {
            finished = true;
            success = s;
            return { finished: true, success: s };
        },
    });
    // Add ask_user tool
    aiTools['ask_user'] = aiTool({
        description: 'Ask the user for input or clarification',
        parameters: z.object({
            prompt: z.string().describe('The question or prompt to show the user'),
        }),
        execute: async ({ prompt }) => {
            const response = await onAskUser(prompt);
            return { user_response: response };
        },
    });
    // Add store tool for output variables
    if (outputVars.length > 0) {
        const varList = outputVars.map((v, i) => `"${v}"${i < outputVars.length - 1 ? ',' : ''}`).join(' ');
        aiTools['store'] = aiTool({
            description: `Store a result value in an output variable. You MUST call this tool to return results from the task. Use the exact variable name as specified.`,
            parameters: z.object({
                variable: z.string().describe(`The variable name to store to. Must be exactly one of: ${varList}`),
                value: z.string().describe('The value to store (must be a string)'),
            }),
            execute: async ({ variable, value }) => {
                if (outputVars.includes(variable)) {
                    variables[variable] = value;
                    return { stored: true, variable, value };
                }
                return { stored: false, error: `Unknown variable: ${variable}. Must be one of: ${varList}` };
            },
        });
    }
    // Add user-defined tools
    for (const [name, tool] of tools) {
        // Handle both Zod schemas and JSON Schema
        const params = isZodSchema(tool.parameters)
            ? tool.parameters
            : jsonSchema(tool.parameters);
        aiTools[name] = aiTool({
            description: tool.description,
            parameters: params,
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
        { role: 'user', content: `Task: ${taskDescription}` },
    ];
    // Debug: log messages being sent
    debugLog('System prompt:', combinedSystemPrompt);
    debugLog('Conversation history:', conversationHistory);
    debugLog('Task:', taskDescription);
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
                // Debug response
                debugLog(`Response text: ${fullText?.substring(0, 200)}${fullText && fullText.length > 200 ? '...' : ''}`);
                debugLog(`Tool calls: ${toolCalls?.length ?? 0}`);
                // Process the response
                if (fullText) {
                    outputs.push(fullText);
                    onOutput(fullText);
                    messages.push({ role: 'assistant', content: fullText });
                }
                // Process tool calls
                if (toolCalls && toolCalls.length > 0) {
                    for (const toolCall of toolCalls) {
                        debugLog(`Tool call: ${toolCall.toolName}`, JSON.stringify(toolCall.args));
                        const toolResult = toolResults?.[toolCalls.indexOf(toolCall)];
                        if (toolResult !== undefined) {
                            messages.push({
                                role: 'assistant',
                                content: `Called tool ${toolCall.toolName} with args: ${JSON.stringify(toolCall.args)}`,
                            });
                            messages.push({
                                role: 'user',
                                content: `Tool result: ${JSON.stringify(toolResult)}`,
                            });
                        }
                    }
                    // Break immediately if finish was called
                    if (finished) {
                        break;
                    }
                }
                // If no tool calls were made, prompt the agent to take action
                if (!toolCalls || toolCalls.length === 0) {
                    if (!finished) {
                        messages.push({
                            role: 'user',
                            content: 'Please take action using the available tools, or call finish() when done.',
                        });
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
                // Debug response
                debugLog(`Response text: ${response.text?.substring(0, 200)}${response.text && response.text.length > 200 ? '...' : ''}`);
                debugLog(`Tool calls: ${response.toolCalls?.length ?? 0}`);
                // Process the response
                if (response.text) {
                    outputs.push(response.text);
                    onOutput(response.text);
                    messages.push({ role: 'assistant', content: response.text });
                }
                // Process tool calls
                if (response.toolCalls && response.toolCalls.length > 0) {
                    for (const toolCall of response.toolCalls) {
                        debugLog(`Tool call: ${toolCall.toolName}`, JSON.stringify(toolCall.args));
                        // Find the tool result - toolResults is an array of results
                        const toolResult = response.toolResults?.[response.toolCalls.indexOf(toolCall)];
                        if (toolResult !== undefined) {
                            // Add tool call and result to messages for context
                            messages.push({
                                role: 'assistant',
                                content: `Called tool ${toolCall.toolName} with args: ${JSON.stringify(toolCall.args)}`,
                            });
                            messages.push({
                                role: 'user',
                                content: `Tool result: ${JSON.stringify(toolResult)}`,
                            });
                        }
                    }
                    // Break immediately if finish was called
                    if (finished) {
                        break;
                    }
                }
                // If no tool calls were made, prompt the agent to take action
                if (!response.toolCalls || response.toolCalls.length === 0) {
                    if (!finished) {
                        messages.push({
                            role: 'user',
                            content: 'Please take action using the available tools, or call finish() when done.',
                        });
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
    // Return only non-system messages for memory persistence
    // System messages are task-specific and rebuilt for each task
    const persistentMessages = messages
        .filter(m => m.role !== 'system')
        .map(m => ({
        role: m.role,
        content: typeof m.content === 'string' ? m.content : JSON.stringify(m.content),
    }));
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
    const parts = [
        `You are an AI agent executing a task. Your job is to complete the following task:`,
        '',
        `Task: ${taskDescription}`,
        '',
        'Available tools:',
        '',
        '- finish(success: boolean): Complete the task. Call with true for success, false for failure.',
        '- ask_user(prompt: string): Ask the user for input or clarification.',
    ];
    if (outputVars.length > 0) {
        const varList = outputVars.map(v => `"${v}"`).join(', ');
        parts.push(`- store(variable: string, value: string): Store a result in an output variable. The variable parameter must be exactly one of: ${varList}. The value must be a simple string.`);
    }
    for (const [name, tool] of tools) {
        parts.push(`- ${name}: ${tool.description}`);
    }
    parts.push('');
    parts.push('Instructions:');
    parts.push('1. Analyze the task and determine what tools you need to use.');
    parts.push('2. Execute tools as needed to gather information or perform actions.');
    if (outputVars.length > 0) {
        const varList = outputVars.map(v => `"${v}"`).join(', ');
        parts.push(`3. IMPORTANT: You MUST use store() to save your results. Call store("VariableName", "your result") for each required variable: ${varList}`);
        parts.push('4. Call finish(true) ONLY AFTER you have stored all required values.');
    }
    else {
        parts.push('3. Call finish(true) when the task is complete.');
    }
    parts.push(`${outputVars.length > 0 ? '5' : '4'}. Call finish(false) if you cannot complete the task.`);
    parts.push('');
    parts.push('Be concise and efficient. Take action immediately.');
    return parts.join('\n');
}
//# sourceMappingURL=agent.js.map