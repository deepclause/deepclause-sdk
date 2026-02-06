/**
 * Agent Loop Implementation
 * Runs an LLM agent loop for task() predicate execution
 * 
 * Based on AI SDK v6 agent patterns:
 * - Uses Zod schemas for tool definitions
 * - Uses result.response.messages for message history management
 */

import { generateText, streamText, tool as aiTool } from 'ai';
import { z } from 'zod';
import type { ToolDefinition, MemoryMessage, TypedVar } from './types.js';
import { createModelProvider } from './prolog/bridge.js';

/** Maximum number of retries for LLM error finish reasons */
const MAX_ERROR_RETRIES = 3;

/**
 * Clean Prolog dict markers ($tag, $t) from tool results
 * This makes the data more readable for the LLM
 */
function cleanPrologMarkers(data: unknown): unknown {
  if (data === null || data === undefined) {
    return data;
  }
  
  if (Array.isArray(data)) {
    return data.map(cleanPrologMarkers);
  }
  
  if (typeof data === 'object') {
    const obj = data as Record<string, unknown>;
    const cleaned: Record<string, unknown> = {};
    
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

export interface AgentLoopOptions {
  taskDescription: string;
  outputVars: (string | TypedVar)[];
  memory: MemoryMessage[];
  tools: Map<string, ToolDefinition>;
  modelOptions: {
    model: string;
    provider: string;
    temperature: number;
    maxOutputTokens: number;
    baseUrl?: string;
  };
  onOutput: (text: string) => void;
  onStream?: (chunk: string, done: boolean) => void;
  onToolCall?: (toolName: string, args: Record<string, unknown>) => void;
  onAskUser: (prompt: string) => Promise<string>;
  signal?: AbortSignal;
  streaming?: boolean;
  debug?: boolean;
}

export interface AgentLoopResult {
  success: boolean;
  outputs: string[];
  variables: Record<string, unknown>;
  /** Conversation messages from the agent loop (excludes system messages which are task-specific) */
  messages: Array<{ role: 'user' | 'assistant'; content: string }>;
}

/**
 * Convert TypedVar type to Zod schema
 */
function typedVarToZod(type: TypedVar['type'], itemType?: TypedVar['type']): z.ZodTypeAny {
  switch (type) {
    case 'string': return z.string();
    case 'number': return z.number();
    case 'integer': return z.number().int();
    case 'boolean': return z.boolean();
    case 'array': return z.array(itemType ? typedVarToZod(itemType) : z.unknown());
    case 'object': return z.record(z.unknown());
    default: return z.unknown();
  }
}

/**
 * Convert JSON Schema to Zod schema
 * Handles basic JSON Schema types used in tool definitions
 */
function jsonSchemaToZod(schema: Record<string, unknown>): z.ZodTypeAny {
  const type = schema.type as string;
  const description = schema.description as string | undefined;
  
  let zodType: z.ZodTypeAny;
  
  switch (type) {
    case 'string':
      zodType = z.string();
      break;
    case 'number':
      zodType = z.number();
      break;
    case 'integer':
      zodType = z.number().int();
      break;
    case 'boolean':
      zodType = z.boolean();
      break;
    case 'array': {
      const items = schema.items as Record<string, unknown> | undefined;
      zodType = z.array(items ? jsonSchemaToZod(items) : z.unknown());
      break;
    }
    case 'object': {
      const properties = schema.properties as Record<string, Record<string, unknown>> | undefined;
      const required = (schema.required as string[]) || [];
      
      if (properties) {
        const shape: Record<string, z.ZodTypeAny> = {};
        for (const [key, propSchema] of Object.entries(properties)) {
          let propZod = jsonSchemaToZod(propSchema);
          if (!required.includes(key)) {
            propZod = propZod.optional();
          }
          shape[key] = propZod;
        }
        zodType = z.object(shape);
      } else {
        zodType = z.record(z.unknown());
      }
      break;
    }
    default:
      zodType = z.unknown();
  }
  
  if (description) {
    zodType = zodType.describe(description);
  }
  
  return zodType;
}

/**
 * Run an agent loop for a task
 */
export async function runAgentLoop(options: AgentLoopOptions): Promise<AgentLoopResult> {
  const {
    taskDescription,
    outputVars,
    memory,
    tools,
    modelOptions,
    onOutput,
    onStream,
    onToolCall,
    signal,
    streaming = false,
    debug = false,
  } = options;

  // Debug helper - logs if debug is enabled or DEBUG_AGENT env var is set
  const debugLog = (...args: unknown[]) => {
    if (debug || process.env.DEBUG_AGENT) {
      console.log('[AGENT]', ...args);
    }
  };
  
  // Normalize outputVars to TypedVar[]
  const normalizedOutputVars: TypedVar[] = outputVars.map(v => 
    typeof v === 'string' ? { name: v, type: 'string' } : v
  );
  
  debugLog('Output vars:', normalizedOutputVars.map(v => `${v.name}:${v.type}`));

  const outputs: string[] = [];
  const variables: Record<string, unknown> = {};
  let finished = false;
  let success = false;
  let errorRetryCount = 0;

  // Build the AI SDK tools using Zod schemas
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const aiTools: Record<string, any> = {};

  // Add finish tool with Zod schema
  aiTools['finish'] = aiTool({
    description: 'CRITICAL: You MUST call this tool to complete the task and return success/failure. Call with success=true if you have set all required results, or success=false if the task is impossible.',
    inputSchema: z.object({
      success: z.boolean().describe('Whether the task was completed successfully')
    }),
    execute: async ({ success: s }: { success: boolean }) => {
      finished = true;
      success = s;
      return { finished: true, success: s };
    },
  });

  // Add set_result tool for output variables
  if (normalizedOutputVars.length > 0) {
    const varNames = normalizedOutputVars.map(v => v.name);
    
    // Create discriminated union schema for variable-specific validation
    // This ensures that if variable="X", value must match the type of X
    const unionOptions = normalizedOutputVars.map(v => {
      return z.object({
        variable: z.literal(v.name).describe(`Set value for '${v.name}' (${v.type})`),
        value: typedVarToZod(v.type, v.itemType).describe(`Value for '${v.name}' (must be ${v.type})`)
      });
    });

    // Create the union schema - handle single var case specially since union needs >= 2 options
    const inputSchema = unionOptions.length > 1 
      ? z.discriminatedUnion('variable', unionOptions as any)
      : unionOptions[0];

    aiTools['set_result'] = aiTool({
      description: `Set a result value for an output variable. You MUST call this tool to return results from the task. Use the exact variable name as specified.`,
      inputSchema: inputSchema as any,
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      execute: async ({ variable, value }: { variable: string; value: any }) => {
        if (varNames.includes(variable)) {
          variables[variable] = value;
          return { success: true, variable, value };
        }
        return { success: false, error: `Unknown variable: ${variable}` };
      },
    });
  }

  // Reserved internal tool names that cannot be overwritten by user-defined tools
  const reservedToolNames = ['finish', 'set_result'];

  // Add user-defined tools - convert JSON Schema to Zod
  for (const [name, tool] of tools) {
    if (reservedToolNames.includes(name)) {
      continue;
    }
    
    // Convert JSON Schema parameters to Zod schema
    const zodSchema = jsonSchemaToZod(tool.parameters as unknown as Record<string, unknown>);
    
    aiTools[name] = aiTool({
      description: tool.description,
      inputSchema: zodSchema,
      execute: async (input: unknown) => {
        try {
          const result = await tool.execute(input as Record<string, unknown>);
          return cleanPrologMarkers(result);
        } catch (error) {
          const message = error instanceof Error ? error.message : String(error);
          return { error: message };
        }
      },
    });
  }

  // Build the base system prompt with task instructions and tools
  const baseSystemPrompt = buildSystemPrompt(taskDescription, normalizedOutputVars, tools);

  // Extract system context from memory (user-defined system() calls)
  const systemContext = memory
    .filter(m => m.role === 'system' && typeof m.content === 'string')
    .map(m => m.content)
    .join('\n\n');

  // Combine into a single system message
  const combinedSystemPrompt = systemContext
    ? `${systemContext}\n\n---\n\n${baseSystemPrompt}`
    : baseSystemPrompt;

  // Filter non-system messages from memory for conversation history
  const conversationHistory = memory.filter(m => 
    m.role !== 'system' && 
    typeof m.content === 'string' && 
    ['user', 'assistant'].includes(m.role)
  );

  // Build initial messages - AI SDK v6 uses ModelMessage[]
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  let messages: any[] = [
    { role: 'system', content: combinedSystemPrompt },
    ...conversationHistory.map(m => ({ 
      role: m.role as 'user' | 'assistant', 
      content: m.content 
    })),
    { role: 'user', content: `Subtask: ${taskDescription}` },
  ];

  debugLog('System prompt:', combinedSystemPrompt);
  debugLog('Conversation history:', conversationHistory);
  debugLog('Subtask:', taskDescription);

  // Create model provider
  const model = createModelProvider(modelOptions.provider, modelOptions.model, modelOptions.baseUrl);

  // Agent loop
  const maxIterations = 50;
  let iteration = 0;

  // Helper to allow event loop to breathe
  const tick = () => new Promise<void>(resolve => setTimeout(resolve, 0));

  while (!finished && iteration < maxIterations) {
    iteration++;
    debugLog(`Iteration ${iteration}`);

    if (signal?.aborted) {
      break;
    }

    try {
      await tick();
      
      if (streaming && onStream) {
        // Streaming mode
        const result = streamText({
          model,
          messages,
          tools: aiTools,
          toolChoice: 'auto',
          temperature: modelOptions.temperature,
          maxOutputTokens: modelOptions.maxOutputTokens,
          abortSignal: signal,
        });

        // Collect streamed text
        let fullText = '';
        for await (const chunk of result.textStream) {
          fullText += chunk;
          onStream(chunk, false);
        }

        if (fullText) {
          onStream('', true);
        }

        // Get results
        const finishReason = await result.finishReason;
        const toolCalls = await result.toolCalls;
        const toolResults = await result.toolResults;
        const responseObj = await result.response;
        
        // Check if finish was called during tool execution
        if (finished) {
          debugLog('Finish tool was called, exiting loop');
          break;
        }

        debugLog(`Response text: ${fullText || '(empty)'}`);
        debugLog(`Tool calls: ${toolCalls?.length ?? 0}`);
        debugLog(`Finish reason: ${finishReason}`);

        // Handle errors
        if (finishReason === 'error') {
          errorRetryCount++;
          debugLog(`ERROR: LLM returned error (attempt ${errorRetryCount}/${MAX_ERROR_RETRIES}).`);
          if (errorRetryCount <= MAX_ERROR_RETRIES) {
            await new Promise(resolve => setTimeout(resolve, 1000 * errorRetryCount));
            continue;
          }
          outputs.push(`Error: LLM API returned an error.`);
          break;
        }

        // Process text output
        if (fullText) {
          outputs.push(fullText);
          onOutput(fullText);
        }

        // Emit tool call events
        if (toolCalls && toolCalls.length > 0) {
          for (const tc of toolCalls) {
            debugLog(`Tool call: ${tc.toolName}`, JSON.stringify(tc.input));
            if (onToolCall) {
              onToolCall(tc.toolName, tc.input as Record<string, unknown>);
            }
          }
          
          // Log tool results
          if (toolResults) {
            for (const tr of toolResults) {
              debugLog(`Tool result for ${tr.toolName}:`, JSON.stringify(tr.output).substring(0, 500));
            }
          }
        }

        // Use response.messages to update message history (AI SDK v6 pattern)
        if (responseObj?.messages) {
          messages = [...messages, ...responseObj.messages];
        } else {
          // Fallback: manually add messages if response.messages not available
          if (fullText) {
            messages.push({ role: 'assistant', content: fullText });
          }
          if (toolCalls && toolCalls.length > 0 && toolResults) {
            // Add tool calls as assistant message
            messages.push({
              role: 'assistant',
              content: toolCalls.map(tc => ({
                type: 'tool-call',
                toolCallId: tc.toolCallId,
                toolName: tc.toolName,
                input: tc.input,
              })),
            });
            // Add tool results
            messages.push({
              role: 'tool',
              content: toolResults.map(tr => ({
                type: 'tool-result',
                toolCallId: tr.toolCallId,
                toolName: tr.toolName,
                output: tr.output,
              })),
            });
          }
        }

        // If no tool calls and model stopped, prompt to continue or we're done
        if (finishReason === 'stop' && (!toolCalls || toolCalls.length === 0)) {
          if (!finished) {
            messages.push({
              role: 'user',
              content: 'Please take action using the available tools, or call finish() when done.',
            });
          }
        }

      } else {
        // Non-streaming mode
        const result = await generateText({
          model,
          messages,
          tools: aiTools,
          toolChoice: 'auto',
          temperature: modelOptions.temperature,
          maxOutputTokens: modelOptions.maxOutputTokens,
          abortSignal: signal,
        });

        // Check if finish was called during tool execution
        if (finished) {
          debugLog('Finish tool was called, exiting loop');
          break;
        }

        debugLog(`Response text: ${result.text || '(empty)'}`);
        debugLog(`Tool calls: ${result.toolCalls?.length ?? 0}`);
        debugLog(`Finish reason: ${result.finishReason}`);

        // Handle errors
        if (result.finishReason === 'error') {
          errorRetryCount++;
          if (errorRetryCount <= MAX_ERROR_RETRIES) {
            debugLog(`ERROR: LLM returned error (attempt ${errorRetryCount}/${MAX_ERROR_RETRIES}). Retrying...`);
            await new Promise(resolve => setTimeout(resolve, 1000 * errorRetryCount));
            continue;
          }
          outputs.push('Error: LLM API returned an error. Check API key and rate limits.');
          break;
        }

        // Process text output
        if (result.text) {
          outputs.push(result.text);
          onOutput(result.text);
        }

        // Emit tool call events
        if (result.toolCalls && result.toolCalls.length > 0) {
          for (const tc of result.toolCalls) {
            debugLog(`Tool call: ${tc.toolName}`, JSON.stringify(tc.input));
            if (onToolCall) {
              onToolCall(tc.toolName, tc.input as Record<string, unknown>);
            }
          }
          
          // Log tool results
          if (result.toolResults) {
            for (const tr of result.toolResults) {
              debugLog(`Tool result for ${tr.toolName}:`, JSON.stringify(tr.output).substring(0, 500));
            }
          }
        }

        // Use response.messages to update message history (AI SDK v6 pattern)
        // This is the key change - let the SDK handle message formatting
        messages = [...messages, ...result.response.messages];

        // If no tool calls and model stopped, prompt to continue or we're done
        if (result.finishReason === 'stop' && (!result.toolCalls || result.toolCalls.length === 0)) {
          if (!finished) {
            messages.push({
              role: 'user',
              content: 'Please take action using the available tools, or call finish() when done.',
            });
          }
        }
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      debugLog(`ERROR in agent loop: ${message}`);
      if (error instanceof Error && error.stack) {
        debugLog(`Stack trace: ${error.stack}`);
      }
      outputs.push(`Error: ${message}`);
      break;
    }
  }

  debugLog(`Loop ended: finished=${finished}, success=${success}, iterations=${iteration}`);

  // If we hit max iterations without finishing, fail
  if (!finished) {
    success = false;
    outputs.push('Agent loop reached maximum iterations without completing');
  }

  // Build persistent messages for memory
  const persistentMessages: Array<{ role: 'user' | 'assistant'; content: string }> = [];
  
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
  
  // Extract assistant text responses from the conversation
  const assistantTextResponses: string[] = [];
  for (const m of messages) {
    if (m.role === 'assistant' && typeof m.content === 'string' && m.content.trim()) {
      assistantTextResponses.push(m.content);
    }
  }
  
  // Add result to persistent messages
  if (success && Object.keys(variables).length > 0) {
    const varSummary = Object.entries(variables)
      .map(([k, v]) => `${k}: ${v}`)
      .join(', ');
    persistentMessages.push({
      role: 'assistant',
      content: `Task completed. Results: ${varSummary}`,
    });
  } else if (assistantTextResponses.length > 0) {
    persistentMessages.push({
      role: 'assistant',
      content: assistantTextResponses[assistantTextResponses.length - 1],
    });
  } else if (success) {
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
function buildSystemPrompt(
  taskDescription: string,
  outputVars: (string | TypedVar)[],
  tools: Map<string, ToolDefinition>
): string {
  const toolDescriptions: string[] = [];
  
  // Normalize outputVars to TypedVar[]
  const normalizedOutputVars: TypedVar[] = outputVars.map(v => 
    typeof v === 'string' ? { name: v, type: 'string' } : v
  );
  
  // Add finish tool description
  toolDescriptions.push('- finish(success: boolean): Signal task completion. Call finish(true) when done successfully, or finish(false) if the task cannot be completed.');
  
  // Add set_result tool if we have output variables
  if (normalizedOutputVars.length > 0) {
    const varList = normalizedOutputVars.map(v => {
      const typeStr = v.type === 'array' && v.itemType ? `array<${v.itemType}>` : v.type;
      return `"${v.name}" (${typeStr})`;
    }).join(', ');
    toolDescriptions.push(`- set_result(variable: string, value: any): Store a result value. Variable must be one of: ${varList}`);
  }
  
  // Add user-defined tools
  for (const [name, tool] of tools) {
    if (name === 'finish' || name === 'set_result') continue;
    
    // Build parameter signature from schema
    const params = tool.parameters;
    const props = (params.properties || {}) as Record<string, { type?: string; description?: string }>;
    const required = (params.required || []) as string[];
    
    const paramList = Object.entries(props)
      .map(([pname, pschema]) => {
        const opt = required.includes(pname) ? '' : '?';
        return `${pname}${opt}: ${pschema.type || 'any'}`;
      })
      .join(', ');
    
    toolDescriptions.push(`- ${name}(${paramList}): ${tool.description}`);
  }
  
  let prompt = `You are an AI agent executing a subtask within a larger workflow. Your job is to complete the following subtask:

Subtask: ${taskDescription}

Available tools:

${toolDescriptions.join('\n')}

Workflow:
1. Analyze the task and gather any needed information using available tools.
2. Once the task is complete, call finish(true).

If you determine the task cannot be completed, call finish(false) immediately.`;

  if (normalizedOutputVars.length > 0) {
    const varList = normalizedOutputVars.map(v => {
      const typeStr = v.type === 'array' && v.itemType ? `array<${v.itemType}>` : v.type;
      return `"${v.name}" (${typeStr})`;
    }).join(', ');
    
    prompt += `

IMPORTANT: You MUST use set_result() to store values for: ${varList}
Call set_result for each variable before calling finish.
The tool will enforce strict type checking based on the variable type.`;
  }

  prompt += `

CRITICAL INSTRUCTIONS:
- You MUST use the structured tool-calling interface to invoke tools.
- NEVER write code syntax like print(), default_api.X(), or function_name() in your text response.
- NEVER describe tool calls in text - actually invoke them using the tool interface.
- Each tool call should be a separate structured function call, not embedded in text.
- When storing values, pass simple strings without code formatting or escaping unless the type is 'string'.

EXAMPLES OF CORRECT TOOL USAGE:

To complete a task successfully, invoke the finish tool with:
  Tool: finish
  Arguments: { "success": true }

WRONG (do NOT do this):
- print(finish(success=true))
- default_api.set_result(variable="X", value="Y")
- I will now call finish(true)
- \`\`\`finish(success=true)\`\`\``;

  return prompt;
}
