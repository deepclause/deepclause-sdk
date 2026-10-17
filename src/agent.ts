/**
 * Agent Loop Implementation
 * Runs an LLM agent loop for task() predicate execution
 * 
 * Based on AI SDK v6 agent patterns:
 * - Uses Zod schemas for tool definitions
 * - Uses result.response.messages for message history management
 */

import { generateText, streamText, hasToolCall, stepCountIs, tool as aiTool } from 'ai';
import { z } from 'zod';
import type {
  LLMBackend,
  LLMBackendMedia,
  LLMBackendMessage,
  LLMBackendResponse,
  LLMBackendTool,
  MediaRef,
  ResolvedModel,
  ToolDefinition,
  MemoryMessage,
  TypedVar,
} from './types.js';
import { createModelProvider, type RawProviderResponseSnapshot } from './prolog/bridge.js';
import { readSystemPromptAsset } from './system/assets/index.js';
import { describeCurrentTime, describeEnvironment } from './system/environment.js';
import { validateToolArguments } from './repl/tool-arguments.js';
import type { ExecutionEnvironment } from './system/runtime/shell-manager.js';

/** Maximum number of retries for LLM error finish reasons */
const MAX_ERROR_RETRIES = 3;
const DEFAULT_STREAM_RESPONSE_AWAIT_TIMEOUT_MS = 2_000;
const MAX_STALLED_ITERATIONS = 3;

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
  /**
   * Conversational mode: a chat turn, not a task.
   *
   * A traditional agent loop — tools, but no `finish`/`set_result`, and the turn
   * ends when the model stops calling tools. That is how a chat agent works: it
   * replies, and the next message starts the next turn. Task mode, by contrast,
   * insists on an explicit `finish` and nudges the model when it does not.
   */
  conversational?: boolean;
  memory: MemoryMessage[];
  tools: Map<string, ToolDefinition>;
  /**
   * Tool names withheld from this loop to break a call cycle. Reported with the
   * reason when called, so a working harness is not mistaken for a broken one.
   */
  excludedTools?: string[];
  workspacePath?: string;
  /**
   * Effective shell backend for this run, so the nested task agent is told where
   * it is running and what time it is. Without it a sub-task reasons about paths
   * and dates in the dark.
   */
  environment?: ExecutionEnvironment;
  /** Name of the single capability host tool (default `bash`; the browser uses `python`). */
  capabilityTool?: string;
  modelOptions: {
    model: string;
    provider: string;
    temperature: number;
    maxOutputTokens: number;
    baseUrl?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    providerOptions?: Record<string, Record<string, any>>;
    target?: ResolvedModel;
  };
  onOutput: (text: string) => void;
  onStream?: (chunk: string, done: boolean) => void;
  /** Reasoning/thinking deltas, surfaced as the `reasoning` DMLEvent. */
  onReasoning?: (chunk: string, done: boolean) => void;
  onToolCall?: (toolName: string, args: Record<string, unknown>) => void;
  onUsage?: (usage: import('./types.js').LLMUsage) => void;
  onBeforeModelCall?: (messages: MemoryMessage[], lastInputTokens?: number) => Promise<MemoryMessage[]>;
  /** Returns false to abort the turn when the model-call budget is exhausted. */
  onModelCall?: () => boolean | Promise<boolean>;
  /** Model-call lifecycle, for a live "in progress" indicator. */
  onModelCallPhase?: (phase: 'started' | 'completed' | 'failed') => void;
  /**
   * Tools that emit their own completion events (e.g. meta tools). For every
   * other tool, `onToolResult` is called so the host can emit a completion
   * event with the result (used to show a short bash output preview).
   */
  selfReportingTools?: Set<string>;
  onToolResult?: (toolName: string, args: Record<string, unknown>, result: unknown) => void;
  onAskUser: (prompt: string) => Promise<string | undefined>;
  signal?: AbortSignal;
  streaming?: boolean;
  debug?: boolean;
  llmBackend?: LLMBackend;
  /** Resolve reference-only media to bytes for the provider request. */
  resolveMedia?: (refs: MediaRef[]) => Promise<LLMBackendMedia[]>;
}

export interface AgentLoopResult {
  success: boolean;
  outputs: string[];
  variables: Record<string, unknown>;
  /** Conversation messages from the agent loop (excludes system messages which are task-specific) */
  messages: Array<{ role: 'user' | 'assistant'; content: string; media?: MediaRef[] }>;
}

type ResponseMessage = { role: 'user' | 'assistant' | 'system'; content: unknown };

interface ResponseMessagesResolution {
  messages: ResponseMessage[];
  timedOut: boolean;
  elapsedMs: number;
}

interface CompletedToolActivity {
  toolName: string;
  input?: unknown;
  output?: unknown;
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function normalizeMessageContent(content: unknown): string {
  if (typeof content === 'string') {
    return content;
  }

  try {
    return JSON.stringify(content);
  } catch {
    return String(content);
  }
}

function normalizeMessagesForCompaction(messages: Array<{ role: string; content: unknown }>): MemoryMessage[] {
  return messages
    .filter((message) => message.role === 'system' || message.role === 'user' || message.role === 'assistant')
    .map((message) => ({
      role: message.role as 'system' | 'user' | 'assistant',
      content: normalizeMessageContent(message.content),
    }));
}

function normalizeLoopText(text: string): string {
  return text.trim().replace(/\s+/g, ' ').toLowerCase().slice(0, 400);
}

function sortJsonValue(value: unknown): unknown {
  if (Array.isArray(value)) {
    return value.map(sortJsonValue);
  }

  if (!isPlainObject(value)) {
    return value;
  }

  return Object.fromEntries(
    Object.entries(value)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([key, entryValue]) => [key, sortJsonValue(entryValue)]),
  );
}

function stableSerialize(value: unknown): string {
  try {
    return JSON.stringify(sortJsonValue(value));
  } catch {
    return String(value);
  }
}

function buildIterationSignature(options: {
  finishReason: string;
  text: string;
  toolCalls: Array<{ toolName: string; input: unknown }>;
  variables: Record<string, unknown>;
}): string {
  return stableSerialize({
    finishReason: options.finishReason,
    text: normalizeLoopText(options.text),
    toolCalls: options.toolCalls.map((toolCall) => ({
      toolName: toolCall.toolName,
      input: toolCall.input,
    })),
    variables: options.variables,
  });
}

function buildRecoveryPrompt(outputVars: TypedVar[], repeatedStallCount: number, noToolCalls: boolean): string | null {
  const resultInstruction = outputVars.length > 0
    ? `If you can answer now, call set_result for ${outputVars.map((variable) => variable.name).join(', ')} and then finish(true).`
    : 'If you can answer now, call finish(true).';

  if (repeatedStallCount > 0) {
    return [
      'You are repeating the same response or action without making progress.',
      'Do not repeat the previous step.',
      resultInstruction,
      'Otherwise call one different tool, or call finish(false) if the task is blocked.',
    ].join(' ');
  }

  if (noToolCalls) {
    return [
      'You did not call any tool and you did not finish the subtask.',
      resultInstruction,
      'If more information is required, call exactly one relevant tool.',
      'If the task cannot be completed, call finish(false).',
    ].join(' ');
  }

  return null;
}

function buildAssistantContinuationPrompt(outputVars: TypedVar[]): string {
  const resultInstruction = outputVars.length > 0
    ? `If you can answer now, call set_result for ${outputVars.map((variable) => variable.name).join(', ')} and then finish(true).`
    : 'If you can answer now, call finish(true).';

  return [
    'Your previous step ended with an assistant message, but the task is not complete.',
    resultInstruction,
    'Continue from the latest state without repeating the prior assistant message verbatim.',
    'If more work is needed, call exactly one relevant tool. If the task is blocked, call finish(false).',
  ].join(' ');
}

function getLastMessageRole(messages: Array<{ role?: unknown }>): string | undefined {
  const lastMessage = messages[messages.length - 1];
  return typeof lastMessage?.role === 'string' ? lastMessage.role : undefined;
}

function sanitizeMessages<T extends { role: string; content: unknown }>(messages: T[]): T[] {
  if (messages.length === 0) {
    return messages;
  }

  const result: T[] = [];
  for (const msg of messages) {
    const prev = result[result.length - 1];
    if (prev && prev.role === msg.role && msg.role !== 'system') {
      // Two consecutive same-role messages — merge content if both are strings,
      // otherwise insert a minimal separator user message
      const prevContent = typeof prev.content === 'string' ? prev.content : '';
      const msgContent = typeof msg.content === 'string' ? msg.content : '';
      if (prevContent && msgContent) {
        result[result.length - 1] = {
          ...prev,
          content: prevContent + '\n\n' + msgContent,
        };
        continue;
      }
      // Can't merge non-string content — insert a separator
      if (msg.role === 'assistant') {
        result.push({ role: 'user', content: 'Continue.' } as unknown as T);
      } else {
        result.push({ role: 'assistant', content: 'Understood.' } as unknown as T);
      }
    }
    result.push(msg);
  }

  // Ensure the conversation doesn't end with two assistant messages
  // (some APIs reject this even after merging)
  if (result.length >= 2) {
    const last = result[result.length - 1];
    const secondLast = result[result.length - 2];
    if (last.role === 'assistant' && secondLast.role === 'assistant') {
      result.splice(result.length - 1, 0, { role: 'user', content: 'Continue.' } as unknown as T);
    }
  }

  return result;
}

function truncateSummaryText(text: string, maxLength = 240): string {
  if (text.length <= maxLength) {
    return text;
  }

  return text.slice(0, Math.max(0, maxLength - 3)) + '...';
}

function renderSummaryValue(value: unknown, maxLength = 140): string | undefined {
  if (value === undefined) {
    return undefined;
  }

  const cleaned = cleanPrologMarkers(value);
  const rendered = typeof cleaned === 'string' ? cleaned : stableSerialize(cleaned);
  const normalized = rendered.trim().replace(/\s+/g, ' ');
  if (!normalized) {
    return undefined;
  }

  return truncateSummaryText(normalized, maxLength);
}

function summarizeCompletedToolActivity(activity: CompletedToolActivity): string {
  const renderedInput = renderSummaryValue(activity.input, 100);
  const renderedOutput = renderSummaryValue(activity.output, 140);

  if (renderedInput && renderedOutput) {
    return `${activity.toolName}(${renderedInput}) -> ${renderedOutput}`;
  }

  if (renderedOutput) {
    return `${activity.toolName} -> ${renderedOutput}`;
  }

  if (renderedInput) {
    return `${activity.toolName}(${renderedInput})`;
  }

  return activity.toolName;
}

function buildImplicitTaskSummary(
  assistantTextResponses: string[],
  completedToolActivities: CompletedToolActivity[],
): string {
  if (assistantTextResponses.length > 0) {
    const latestAssistantText = assistantTextResponses[assistantTextResponses.length - 1]?.trim();
    if (latestAssistantText) {
      return latestAssistantText.startsWith('Task completed')
        ? latestAssistantText
        : `Task completed. Summary: ${truncateSummaryText(latestAssistantText)}`;
    }
  }

  const renderedActivities = completedToolActivities
    .map(summarizeCompletedToolActivity)
    .filter((entry) => entry.length > 0);

  if (renderedActivities.length > 0) {
    return `Task completed. Summary: ${renderedActivities.slice(-3).join('; ')}`;
  }

  return 'Task completed successfully.';
}

type ModelLike = ReturnType<typeof createModelProvider>;

async function generateTaskSummary(
  model: ModelLike,
  messages: Array<{ role: string; content: unknown }>,
  modelOptions: { temperature: number; maxOutputTokens: number; providerOptions?: Record<string, Record<string, unknown>> },
  signal?: AbortSignal,
): Promise<string> {
  // Strip tool-call and tool-result messages — the AI SDK requires matching
  // results for every tool call ID, which may not survive message reconstruction.
  // Keep only plain text system/user/assistant messages for context.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const textOnlyMessages: any[] = [];
  for (const m of messages) {
    if (m.role === 'system' || m.role === 'user') {
      const content = typeof m.content === 'string' ? m.content : '';
      if (content.trim()) {
        textOnlyMessages.push({ role: m.role, content });
      }
    } else if (m.role === 'assistant') {
      // Only keep assistant messages that are plain strings (text output, not tool calls)
      const content = typeof m.content === 'string' ? m.content : '';
      if (content.trim()) {
        textOnlyMessages.push({ role: 'assistant', content });
      }
    }
  }
  textOnlyMessages.push({
    role: 'user',
    content: 'Write a detailed summary of what you just did and what you found. Include: files examined and their key contents, vulnerabilities or issues discovered (with file paths and line numbers), commands run and their results, decisions made, and anything the next task should know. Do not include raw tool call syntax.',
  });
  const result = await generateText({
    model,
    messages: textOnlyMessages,
    temperature: 0.3,
    maxOutputTokens: 4096,
    abortSignal: signal,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    providerOptions: modelOptions.providerOptions as any,
  });
  return result.text?.trim() || 'Task completed successfully.';
}

function getStreamResponseAwaitTimeoutMs(): number {
  const raw = process.env.DC_STREAM_RESPONSE_TIMEOUT_MS;
  if (raw != null) {
    const parsed = Number.parseInt(raw, 10);
    if (Number.isFinite(parsed) && parsed >= 0) {
      return parsed;
    }
  }
  return DEFAULT_STREAM_RESPONSE_AWAIT_TIMEOUT_MS;
}

async function resolveResponseMessagesWithTimeout(
  responsePromise: PromiseLike<unknown>,
  timeoutMs: number,
): Promise<ResponseMessagesResolution> {
  const startMs = Date.now();

  if (timeoutMs <= 0) {
    return {
      messages: getResponseMessages(await responsePromise),
      timedOut: false,
      elapsedMs: Date.now() - startMs,
    };
  }

  const timeoutResult = Symbol('timeout');
  const raced = await Promise.race([
    responsePromise,
    new Promise<symbol>((resolve) => {
      setTimeout(() => resolve(timeoutResult), timeoutMs);
    }),
  ]);

  if (raced === timeoutResult) {
    return {
      messages: [],
      timedOut: true,
      elapsedMs: Date.now() - startMs,
    };
  }

  return {
    messages: getResponseMessages(raced),
    timedOut: false,
    elapsedMs: Date.now() - startMs,
  };
}

/**
 * Describe a declared schema's fields for the model, so `set_result` asks for the
 * right keys rather than leaving it to guess (`{}` was the whole instruction
 * before). Returns undefined when the output names no schema.
 */
function describeSchemaFields(typedVar: TypedVar): string | undefined {
  const fields = typedVar.fields;
  if (!fields || fields.length === 0) return undefined;
  return fields
    .map((field) => {
      const notes = [field.type];
      if (field.required) notes.push('required');
      else if (field.default !== undefined) notes.push(`default ${JSON.stringify(field.default)}`);
      return `${field.name} (${notes.join(', ')})`;
    })
    .join(', ');
}

/**
 * A declared schema is checked here as well as in the runtime, so the agent is
 * told which field is wrong *inside* the `set_result` loop and can correct it in
 * the same turn. The runtime remains the authority — it fills defaults and
 * re-checks when the value binds — but by then the turn is over.
 */
function validateSchemaFields(typedVar: TypedVar, value: unknown): string | null {
  const fields = typedVar.fields;
  if (!fields || fields.length === 0) return null;
  if (!isPlainObject(value)) return 'Expected an object value';
  const record = value as Record<string, unknown>;
  for (const field of fields) {
    const present = Object.prototype.hasOwnProperty.call(record, field.name)
      || Object.keys(record).some((key) => normaliseFieldKey(key) === normaliseFieldKey(field.name));
    if (!present) {
      if (field.required) return `Missing required field "${field.name}"`;
      continue;
    }
    const found = record[field.name] ?? Object.entries(record)
      .find(([key]) => normaliseFieldKey(key) === normaliseFieldKey(field.name))?.[1];
    if (!fieldTypeAccepts(field.type, found)) {
      return `Field "${field.name}" should be ${field.type}`;
    }
  }
  return null;
}

/** Field names are compared the way dict_field does: case and separators ignored. */
function normaliseFieldKey(key: string): string {
  return key.toLowerCase().replace(/[ _-]/g, '');
}

function fieldTypeAccepts(type: string, value: unknown): boolean {
  switch (type) {
    case 'string': return typeof value === 'string';
    case 'integer': return typeof value === 'number' && Number.isInteger(value);
    case 'number': return typeof value === 'number';
    case 'boolean': return typeof value === 'boolean';
    case 'array': return Array.isArray(value);
    case 'object': return isPlainObject(value);
    default: return true;
  }
}

function validateTypedResultValue(typedVar: TypedVar, value: unknown): string | null {
  switch (typedVar.type) {
    case 'string':
      return typeof value === 'string' ? null : 'Expected a string value';
    case 'number':
      return typeof value === 'number' ? null : 'Expected a number value';
    case 'integer':
      return typeof value === 'number' && Number.isInteger(value) ? null : 'Expected an integer value';
    case 'boolean':
      return typeof value === 'boolean' ? null : 'Expected a boolean value';
    case 'array':
      if (!Array.isArray(value)) {
        return 'Expected an array value';
      }
      if (typedVar.fields?.length) {
        for (const item of value) {
          const itemError = validateSchemaFields(typedVar, item);
          if (itemError) return itemError;
        }
        return null;
      }
      if (!typedVar.itemType) {
        return null;
      }
      for (const item of value) {
        const itemError = validateTypedResultValue({ name: typedVar.name, type: typedVar.itemType }, item);
        if (itemError) {
          return `Expected array<${typedVar.itemType}> value`;
        }
      }
      return null;
    case 'object':
      if (!isPlainObject(value)) return 'Expected an object value';
      if (typedVar.fields?.length) return validateSchemaFields(typedVar, value);
      return null;
    default:
      return null;
  }
}

function coerceTypedResultValue(typedVar: TypedVar, value: unknown): unknown {
  if (typeof value !== 'string') {
    return value;
  }

  const trimmed = value.trim();
  if (!trimmed) {
    return value;
  }

  switch (typedVar.type) {
    case 'array':
      try {
        const parsed = JSON.parse(trimmed);
        return Array.isArray(parsed) ? parsed : value;
      } catch {
        return value;
      }
    case 'object':
      try {
        const parsed = JSON.parse(trimmed);
        return isPlainObject(parsed) ? parsed : value;
      } catch {
        return value;
      }
    case 'number': {
      const parsed = Number(trimmed);
      return Number.isFinite(parsed) ? parsed : value;
    }
    case 'integer': {
      const parsed = Number(trimmed);
      return Number.isInteger(parsed) ? parsed : value;
    }
    case 'boolean':
      if (trimmed === 'true') return true;
      if (trimmed === 'false') return false;
      return value;
    default:
      return value;
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
        // `.strict()`: a plain object schema *strips* unknown keys, so a call
        // naming a parameter that does not exist (e.g. `stdin` on bash) would be
        // silently accepted with the extra key removed. Strict makes the AI SDK
        // report it back to the model, which can then fix the call.
        zodType = z.object(shape).strict();
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

interface InjectedAgentLoopState {
  outputs: string[];
  variables: Record<string, unknown>;
  finished: boolean;
  success: boolean;
}

async function runInjectedAgentLoop(
  options: AgentLoopOptions,
  normalizedOutputVars: TypedVar[],
  combinedSystemPrompt: string,
  conversationHistory: MemoryMessage[],
): Promise<AgentLoopResult> {
  const backend = options.llmBackend!;
  const state: InjectedAgentLoopState = {
    outputs: [],
    variables: {},
    finished: false,
    success: false,
  };
  const requiredVarNames = normalizedOutputVars.map((variable) => variable.name);
  // A chat turn has no `finish`/`set_result`: it ends when the model stops calling
  // tools, like a traditional agent loop.
  const backendTools: LLMBackendTool[] = options.conversational ? [] : [{
    name: 'finish',
    description: 'Complete the task. success=true requires every requested result variable to be set.',
    parameters: {
      type: 'object',
      properties: { success: { type: 'boolean' } },
      required: ['success'],
      additionalProperties: false,
    },
  }];

  if (normalizedOutputVars.length > 0) {
    const resultTypes = normalizedOutputVars
      .map((variable) => `${variable.name}: ${variable.type}${variable.itemType ? `<${variable.itemType}>` : ''}`)
      .join(', ');
    backendTools.push({
      name: 'set_result',
      description: `Set one requested output variable before calling finish. Required results: ${resultTypes}.`,
      parameters: {
        type: 'object',
        properties: {
          variable: { type: 'string', enum: requiredVarNames },
          value: {
            anyOf: [
              { type: 'string' },
              { type: 'number' },
              { type: 'boolean' },
              { type: 'array' },
              { type: 'object' },
            ],
          },
        },
        required: ['variable', 'value'],
        additionalProperties: false,
      },
    });
  }

  for (const [name, tool] of options.tools) {
    if (name === 'finish' || name === 'set_result') continue;
    backendTools.push({
      name,
      description: tool.description,
      parameters: tool.parameters as unknown as Record<string, unknown>,
    });
  }

  // Reference-only media in memory is resolved to bytes here so the provider
  // sees image blocks; `mediaRefs` keeps the reference for the next round-trip.
  const historyMessages: LLMBackendMessage[] = [];
  for (const message of conversationHistory) {
    const refs = message.media;
    const media = refs && refs.length > 0 && options.resolveMedia
      ? await options.resolveMedia(refs)
      : undefined;
    historyMessages.push({
      role: message.role,
      content: message.content,
      media,
      mediaRefs: refs,
    });
  }
  const messages: LLMBackendMessage[] = [
    { role: 'system', content: combinedSystemPrompt },
    ...historyMessages,
    { role: 'user', content: options.conversational ? options.taskDescription : `Subtask: ${options.taskDescription}` },
  ];
  const maxIterations = Math.max(1, Number(process.env.DC_MAX_ITERATIONS) || 1000);

  for (let iteration = 0; iteration < maxIterations && !state.finished; iteration++) {
    if (options.signal?.aborted) break;

    if (options.onModelCall && !(await options.onModelCall())) {
      throw new Error('model-call budget exhausted for this turn');
    }

    let emittedText = false;
    let emittedReasoning = false;
    let response: LLMBackendResponse;
    options.onModelCallPhase?.('started');
    try {
      response = await backend.complete({
        messages,
        tools: backendTools,
        temperature: options.modelOptions.temperature,
        maxTokens: options.modelOptions.maxOutputTokens,
        target: options.modelOptions.target,
        signal: options.signal,
        onText: options.streaming && options.onStream
          ? (chunk) => {
              emittedText = emittedText || chunk.length > 0;
              options.onStream?.(chunk, false);
            }
          : undefined,
        onReasoning: options.streaming && options.onReasoning
          ? (chunk) => {
              emittedReasoning = emittedReasoning || chunk.length > 0;
              options.onReasoning?.(chunk, false);
            }
          : undefined,
      });
    } catch (error) {
      options.onModelCallPhase?.('failed');
      throw error;
    }
    options.onModelCallPhase?.('completed');

    if (response.usage) options.onUsage?.(response.usage);
    if (options.streaming && options.onStream && response.text && !emittedText) {
      options.onStream(response.text, false);
    }
    if (options.streaming && options.onStream && response.text) {
      options.onStream('', true);
    }
    if (options.streaming && options.onReasoning && response.reasoning && !emittedReasoning) {
      options.onReasoning(response.reasoning, false);
      emittedReasoning = true;
    }
    if (options.streaming && options.onReasoning && emittedReasoning) {
      options.onReasoning('', true);
    }
    if (response.text) {
      state.outputs.push(response.text);
      options.onOutput(response.text);
    }

    const toolCalls = response.toolCalls ?? [];
    messages.push({ role: 'assistant', content: response.text, toolCalls, providerData: response.providerData });

    for (const call of toolCalls) {
      options.onToolCall?.(call.name, call.arguments);
      let result: unknown;

      if (call.name === 'finish') {
        const requestedSuccess = call.arguments.success === true;
        const missing = requiredVarNames.filter((name) => !(name in state.variables));
        if (requestedSuccess && missing.length > 0) {
          result = { finished: false, error: `Missing required result variable(s): ${missing.join(', ')}` };
        } else {
          state.finished = true;
          state.success = requestedSuccess;
          result = { finished: true, success: requestedSuccess };
        }
      } else if (call.name === 'set_result') {
        const variableName = String(call.arguments.variable ?? '');
        const typedVar = normalizedOutputVars.find((variable) => variable.name === variableName);
        if (!typedVar) {
          result = { success: false, error: `Unknown variable: ${variableName}` };
        } else {
          const value = coerceTypedResultValue(typedVar, call.arguments.value);
          const validationError = validateTypedResultValue(typedVar, value);
          if (validationError) {
            result = { success: false, error: `${validationError} for ${variableName}` };
          } else {
            state.variables[variableName] = value;
            result = { success: true, variable: variableName, value };
          }
        }
      } else {
        const tool = options.tools.get(call.name);
        if (!tool) {
          const excluded = options.excludedTools?.includes(call.name) ?? false;
          result = {
            error: excluded
              ? `Tool '${call.name}' is not available in this task: it is already running further up `
                + 'the call chain, and calling it again would recurse. Do not call it again; work with '
                + 'the text you were given, or say what is missing.'
              : `Tool not found: ${call.name}`,
          };
        } else {
          const validation = validateToolArguments(call.name, tool.parameters, call.arguments);
          if (!validation.ok) {
            result = { error: validation.error };
          } else {
            try {
              result = cleanPrologMarkers(await tool.execute(call.arguments, { signal: options.signal }));
            } catch (error) {
              result = { error: error instanceof Error ? error.message : String(error) };
            }
          }
        }
        if (!options.selfReportingTools?.has(call.name)) {
          options.onToolResult?.(call.name, call.arguments, result);
        }
      }

      messages.push({
        role: 'tool',
        content: JSON.stringify(result),
        toolCallId: call.id,
        toolName: call.name,
      });
    }

    if (toolCalls.length === 0 && !state.finished) {
      if (options.conversational) {
        // The reply is the answer: end the turn, and do not nudge.
        state.finished = true;
        state.success = true;
      } else {
        messages.push({
          role: 'user',
          content: 'Continue the task. Use set_result for each requested output, then call finish.',
        });
      }
    }
  }

  return {
    success: state.success,
    outputs: state.outputs,
    variables: state.variables,
    messages: messages
      .filter((message): message is LLMBackendMessage & { role: 'user' | 'assistant' } =>
        message.role === 'user' || message.role === 'assistant')
      .map((message) => ({ role: message.role, content: message.content, media: message.mediaRefs })),
  };
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
    onReasoning,
    onToolCall,
    onUsage,
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
  const completedToolActivities: CompletedToolActivity[] = [];
  const pendingStreamingToolCalls: Array<{ toolName: string; input: unknown }> = [];
  let finished = false;
  let success = false;
  let errorRetryCount = 0;
  let previousIterationSignature: string | null = null;
  let repeatedStallCount = 0;

  // Build the AI SDK tools using Zod schemas
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const aiTools: Record<string, any> = {};

  // Add finish tool with Zod schema. A chat turn has no finish: the model replies
  // and stops, and the next message starts the next turn.
  const requiredVarNames = normalizedOutputVars.map(v => v.name);
  if (!options.conversational) aiTools['finish'] = aiTool({
    description: 'CRITICAL: You MUST call this tool to complete the task and return success/failure. Call with success=true if you have set all required results, or success=false if the task is impossible.',
    inputSchema: z.object({
      success: z.boolean().describe('Whether the task was completed successfully')
    }),
    execute: async ({ success: s }: { success: boolean }) => {
      // Guard: success=true requires all output variables to be set
      if (s && requiredVarNames.length > 0) {
        const missing = requiredVarNames.filter(v => !(v in variables));
        if (missing.length > 0) {
          return { finished: false, error: `Cannot finish with success=true — missing required result variable(s): ${missing.join(', ')}. Call set_result for each before finishing.` };
        }
      }
      finished = true;
      success = s;
      return { finished: true, success: s };
    },
  });

  // Add set_result tool for output variables
  if (normalizedOutputVars.length > 0) {
    const varNames = normalizedOutputVars.map(v => v.name);
    const typeSummary = normalizedOutputVars.map(v => {
      const renderedType = v.type === 'array' && v.itemType ? `array<${v.itemType}>` : v.type;
      const fields = describeSchemaFields(v);
      return fields ? `${v.name}: array of objects, each ${fields}` : `${v.name}: ${renderedType}`;
    }).join(', ');
    const variableSchema = varNames.length === 1
      ? z.literal(varNames[0])
      : z.enum(varNames as [string, ...string[]]);
    const inputSchema = z.object({
      variable: variableSchema.describe(`Output variable name. Must be one of: ${varNames.join(', ')}`),
      value: z.union([
        z.string(),
        z.number(),
        z.boolean(),
        z.array(z.unknown()),
        z.record(z.unknown()),
      ]).describe(`Value for the selected variable. Expected types: ${typeSummary}`),
    });

    aiTools['set_result'] = aiTool({
      description: `Set a result value for an output variable. You MUST call this tool to return results from the task. Use the exact variable name as specified.`,
      inputSchema: inputSchema as any,
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      execute: async ({ variable, value }: { variable: string; value: any }) => {
        const typedVar = normalizedOutputVars.find(v => v.name === variable);
        if (typedVar) {
          const coercedValue = coerceTypedResultValue(typedVar, value);
          const validationError = validateTypedResultValue(typedVar, coercedValue);
          if (validationError) {
            return {
              success: false,
              error: `${validationError} for ${variable}`,
            };
          }
          variables[variable] = coercedValue;
          return { success: true, variable, value: coercedValue };
        }
        return { success: false, error: `Unknown variable: ${variable}` };
      },
    });
  }

  // Reserved internal tool names that cannot be overwritten by user-defined tools
  const reservedToolNames = ['finish', 'set_result'];

  // Add user-defined tools - convert JSON Schema to Zod
  for (const [name, tool] of options.tools) {
    if (reservedToolNames.includes(name)) {
      continue;
    }
    
    // Convert JSON Schema parameters to Zod schema
    const zodSchema = jsonSchemaToZod(tool.parameters as unknown as Record<string, unknown>);
    
    aiTools[name] = aiTool({
      description: tool.description,
      inputSchema: zodSchema,
      execute: async (input: unknown, execOptions?: { abortSignal?: AbortSignal }) => {
        // The AI SDK validates the shape it was given, but the schema it was
        // given is our JSON Schema converted to Zod; check the same rule here so
        // both tool-calling paths report the same, specific error.
        const validation = validateToolArguments(name, tool.parameters, input as Record<string, unknown>);
        if (!validation.ok) return { error: validation.error };
        try {
          const result = await tool.execute(input as Record<string, unknown>, {
            signal: execOptions?.abortSignal ?? options.signal,
          });
          return cleanPrologMarkers(result);
        } catch (error) {
          const message = error instanceof Error ? error.message : String(error);
          return { error: message };
        }
      },
    });
  }

  // Build the base system prompt with task instructions and tools
  const baseSystemPrompt = await buildSystemPrompt({
    taskDescription,
    outputVars: normalizedOutputVars,
    tools,
    workspacePath: options.workspacePath,
    environment: options.environment,
    capabilityTool: options.capabilityTool,
    conversational: options.conversational,
  });

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

  if (options.llmBackend) {
    return runInjectedAgentLoop(options, normalizedOutputVars, combinedSystemPrompt, conversationHistory);
  }

  debugLog('System prompt:', combinedSystemPrompt);
  debugLog('Conversation history:', conversationHistory);
  debugLog('Subtask:', taskDescription);

  let latestRawProviderResponse: Promise<RawProviderResponseSnapshot> | null = null;

  const maybeLogEmptyOtherProviderResponse = async (finishReason: string, text: string): Promise<void> => {
    if (finishReason !== 'other' || text) {
      latestRawProviderResponse = null;
      return;
    }

    if (!latestRawProviderResponse) {
      debugLog('Raw upstream provider response unavailable for empty finishReason=other result.');
      return;
    }

    const snapshot = await latestRawProviderResponse;
    latestRawProviderResponse = null;

    debugLog(
      `[fetch] Raw upstream provider response requestId=${snapshot.requestId} status=${snapshot.status} transport=${snapshot.transport} content-type=${snapshot.contentType ?? 'unknown'}`,
    );
    if (snapshot.captureError) {
      debugLog(`[fetch] Raw upstream response capture error: ${snapshot.captureError}`);
    }
    debugLog(snapshot.bodyText || '(empty raw upstream response body)');
  };

  // Create model provider
  const model = createModelProvider(
    modelOptions.provider,
    modelOptions.model,
    modelOptions.baseUrl,
    debugLog,
    (snapshot) => {
      latestRawProviderResponse = snapshot;
    },
  );

  // Agent loop
  const maxIterations = Math.max(1, Number(process.env.DC_MAX_ITERATIONS) || 1000);
  let iteration = 0;
  let lastActualInputTokens: number | undefined;

  // Helper to allow event loop to breathe
  const tick = () => new Promise<void>(resolve => setTimeout(resolve, 0));

  while (!finished && iteration < maxIterations) {
    iteration++;
    if (options.onModelCall && !(await options.onModelCall())) {
      throw new Error('model-call budget exhausted for this turn');
    }
    const iterStartMs = Date.now();
    debugLog(`Iteration ${iteration}`);

    if (signal?.aborted) {
      break;
    }

    try {
      await tick();

      if (options.onBeforeModelCall) {
        messages = await options.onBeforeModelCall(normalizeMessagesForCompaction(messages), lastActualInputTokens);
      }

      // Ensure proper role alternation — some APIs reject consecutive same-role messages
      messages = sanitizeMessages(messages);
      
      if (streaming && onStream) {
        latestRawProviderResponse = null;

        // Signal LLM call start so TTFT (time-to-first-token) is captured in timing.
        // For thinking models (e.g. Claude with extended thinking), textStream yields
        // nothing during the thinking phase — without this signal, the thinking time
        // (potentially 60+ seconds) would be invisible in LLM timing metrics.
        onStream('', false);


        //save messages to a json file for debugging
        if (debug || process.env.DEBUG_AGENT) {
          const fs = await import('fs/promises');
          await fs.writeFile(`agent_messages_iteration_${iteration}.json`, JSON.stringify(messages, null, 2), 'utf-8');
        }
        
        // Streaming mode — tools WITH execute, SDK handles tool execution.
        // stopWhen: hasToolCall('finish') stops multi-step after finish is called.
        const apiCallMs = Date.now();

        const result = streamText({
          model,
          messages,
          tools: aiTools,
          toolChoice: 'auto',
          temperature: modelOptions.temperature,
          maxOutputTokens: modelOptions.maxOutputTokens,
          abortSignal: signal,
          providerOptions: modelOptions.providerOptions,
          stopWhen: options.conversational ? stepCountIs(1) : hasToolCall('finish'),
          onStepFinish: (step) => {
            debugLog(`Step finished: finishReason=${step.finishReason} toolCalls=${step.toolCalls?.length ?? 0}`);
          },
        });

        // Collect results from fullStream
        let fullText = '';
        const toolCallsThisIteration: Array<{ toolName: string; input: unknown }> = [];
        let ttftMs: number | null = null;       // time to first TEXT token
        let ttfeMs: number | null = null;       // time to first event (any type)
        let ttfrMs: number | null = null;       // time to first reasoning token
        let ttftiMs: number | null = null;      // time to first tool-input token
        let firstToolCallMs: number | null = null; // time to first tool-call (complete)
        let chunkCount = 0;
        let reasoningChunks = 0;
        let reasoningChars = 0;
        let toolInputChunks = 0;
        let toolInputChars = 0;
        let finishReason: string = 'other';
        let stepUsage: { inputTokens?: number; outputTokens?: number; totalTokens?: number; inputTokenDetails?: { cacheReadTokens?: number; cacheWriteTokens?: number }; outputTokenDetails?: { reasoningTokens?: number } } | null = null;
        let lastEventMs = apiCallMs;
        const eventCounts: Record<string, number> = {};

        for await (const part of result.fullStream) {
          const nowMs = Date.now();
          if (ttfeMs === null) ttfeMs = nowMs - apiCallMs;
          eventCounts[part.type] = (eventCounts[part.type] ?? 0) + 1;

          switch (part.type) {
            case 'text-delta':
              if (ttftMs === null) ttftMs = nowMs - apiCallMs;
              chunkCount++;
              fullText += part.text;
              onStream(part.text, false);
              break;

            case 'reasoning-delta':
              if (ttfrMs === null) {
                ttfrMs = nowMs - apiCallMs;
                debugLog(`First reasoning token at ${ttfrMs}ms`);
              }
              reasoningChunks++;
              reasoningChars += (part as { text?: string }).text?.length ?? 0;
              onReasoning?.((part as { text?: string }).text ?? '', false);
              break;

            case 'tool-input-delta':
              if (ttftiMs === null) {
                ttftiMs = nowMs - apiCallMs;
                debugLog(`First tool-input token at ${ttftiMs}ms`);
              }
              toolInputChunks++;
              toolInputChars += (part as { delta?: string }).delta?.length ?? 0;
              break;

            case 'tool-call':
              if (firstToolCallMs === null) firstToolCallMs = nowMs - apiCallMs;
              toolCallsThisIteration.push({ toolName: part.toolName, input: part.input });
              pendingStreamingToolCalls.push({ toolName: part.toolName, input: part.input });
              debugLog(`Tool call: ${part.toolName}`, JSON.stringify(part.input));
              if (onToolCall) {
                onToolCall(part.toolName, part.input as Record<string, unknown>);
              }
              break;

            case 'tool-result':
              {
                const pendingIndex = pendingStreamingToolCalls.findIndex((pending) => pending.toolName === part.toolName);
                const pendingCall = pendingIndex >= 0
                  ? pendingStreamingToolCalls.splice(pendingIndex, 1)[0]
                  : undefined;
                completedToolActivities.push({
                  toolName: part.toolName,
                  input: pendingCall?.input,
                  output: (part as { output?: unknown }).output,
                });
              }
              debugLog(`Tool result for ${part.toolName}:`, JSON.stringify(part.output).substring(0, 500));
              break;

            case 'error':
              debugLog(`Stream error:`, part.error);
              break;

            case 'finish-step':
              finishReason = part.finishReason;
              stepUsage = part.usage;
              break;

            default:
              break;
          }
          lastEventMs = nowMs;
        }
        const streamDoneMs = Date.now() - apiCallMs;
        const gapAfterLastEvent = Date.now() - lastEventMs;
        debugLog(`Iteration ${iteration} fullStream: ${chunkCount} text, ${reasoningChunks} reasoning (${reasoningChars}ch), ${toolInputChunks} tool-input (${toolInputChars}ch), ${streamDoneMs}ms`);
        debugLog(`Iteration ${iteration} stream timing: TTFE=${ttfeMs ?? '-'}ms TTFR=${ttfrMs ?? '-'}ms TTFTI=${ttftiMs ?? '-'}ms TTFT=${ttftMs ?? '-'}ms firstToolCall=${firstToolCallMs ?? '-'}ms gapAfterLastEvent=${gapAfterLastEvent}ms`);
        debugLog(`Iteration ${iteration} event counts:`, eventCounts);

        if (fullText) {
          onStream('', true);
        }
        if (reasoningChunks > 0) {
          onReasoning?.('', true);
        }

        // Emit usage data from the last step
        let usageStr = '';
        if (stepUsage) {
          const cacheRead = stepUsage.inputTokenDetails?.cacheReadTokens ?? 0;
          const cacheWrite = stepUsage.inputTokenDetails?.cacheWriteTokens ?? 0;
          const reasoning = stepUsage.outputTokenDetails?.reasoningTokens ?? 0;
          usageStr = ` | in=${stepUsage.inputTokens ?? 0} out=${stepUsage.outputTokens ?? 0}` +
            (cacheRead ? ` cacheRead=${cacheRead}` : '') +
            (cacheWrite ? ` cacheWrite=${cacheWrite}` : '') +
            (reasoning ? ` reasoning=${reasoning}` : '');
          lastActualInputTokens = stepUsage.inputTokens ?? undefined;
          if (onUsage) {
            onUsage({
              inputTokens: stepUsage.inputTokens ?? 0,
              outputTokens: stepUsage.outputTokens ?? 0,
              totalTokens: stepUsage.totalTokens ?? 0,
              cacheReadTokens: cacheRead || undefined,
              cacheWriteTokens: cacheWrite || undefined,
              reasoningTokens: reasoning || undefined,
            });
          }
        }

        debugLog(`Iteration ${iteration} timing: TTFE=${ttfeMs ?? '-'}ms TTFR=${ttfrMs ?? '-'}ms TTFT=${ttftMs ?? 'no-text'}ms stream=${streamDoneMs}ms total=${Date.now() - iterStartMs}ms${usageStr}`);
        
        if (finished) {
          debugLog('Finish tool was called; finalizing streamed iteration before exit');
        }

        debugLog(`Response text: ${fullText || '(empty)'}`);
        debugLog(`Finish reason: ${finishReason}`);
  await maybeLogEmptyOtherProviderResponse(finishReason, fullText);

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

        // Use SDK's response.messages for message history.
        // The SDK handles tool execution and includes tool results in messages.
        const responseResolution = await resolveResponseMessagesWithTimeout(
          result.response,
          getStreamResponseAwaitTimeoutMs(),
        );
        const responseMessages = responseResolution.messages;
        if (responseResolution.timedOut) {
          debugLog(
            `Iteration ${iteration} timed out waiting ${responseResolution.elapsedMs}ms for result.response after stream completion; continuing without SDK response messages.`,
          );
        } else if (responseResolution.elapsedMs > 100) {
          debugLog(`Iteration ${iteration} await result.response took ${responseResolution.elapsedMs}ms (unexpectedly slow)`);
        }
        if (responseMessages.length > 0) {
          messages.push(...responseMessages);
        } else if (responseResolution.timedOut && fullText) {
          messages.push({ role: 'assistant', content: fullText });
        }

        const iterationSignature = buildIterationSignature({
          finishReason,
          text: fullText,
          toolCalls: toolCallsThisIteration,
          variables,
        });
        repeatedStallCount = iterationSignature === previousIterationSignature ? repeatedStallCount + 1 : 0;
        previousIterationSignature = iterationSignature;

        if (!finished && repeatedStallCount >= MAX_STALLED_ITERATIONS) {
          success = false;
          outputs.push('Agent loop detected repeated non-progressing responses and stopped early.');
          break;
        }

        // A chat turn ends when the model stops calling tools: that reply is the
        // answer, and the next message starts the next turn.
        if (options.conversational && !finished && toolCallsThisIteration.length === 0) {
          finished = true;
          success = true;
        }

        // If no tool calls were made, nudge the model to act.
        // This handles 'stop', 'other', and any unexpected finish reason.
        // A chat turn is never nudged: the model simply finished its reply.
        const recoveryPrompt = options.conversational
          ? null
          : buildRecoveryPrompt(normalizedOutputVars, repeatedStallCount, toolCallsThisIteration.length === 0);
        const continuationPrompt = !options.conversational && !finished && getLastMessageRole(messages) === 'assistant'
          ? buildAssistantContinuationPrompt(normalizedOutputVars)
          : null;
        const nextPrompt = recoveryPrompt ?? continuationPrompt;
        if (nextPrompt && !finished) {
          messages.push({
            role: 'user',
            content: nextPrompt,
          });
        }

      } else {
        latestRawProviderResponse = null;

        // Signal LLM call start for TTFT measurement (even in non-streaming mode)
        if (onStream) {
          onStream('', false);
        }

        // Non-streaming mode
        const apiCallMs = Date.now();
        const result = await generateText({
          model,
          messages,
          tools: aiTools,
          toolChoice: 'auto',
          temperature: modelOptions.temperature,
          maxOutputTokens: modelOptions.maxOutputTokens,
          abortSignal: signal,
          providerOptions: modelOptions.providerOptions,
        });

        // Emit usage data (fetch before timing log so we can include token counts)
        let genUsageStr = '';
        if (result.usage) {
          const u = result.usage;
          const cacheRead = u.inputTokenDetails?.cacheReadTokens ?? 0;
          const cacheWrite = u.inputTokenDetails?.cacheWriteTokens ?? 0;
          const reasoning = u.outputTokenDetails?.reasoningTokens ?? 0;
          genUsageStr = ` | in=${u.inputTokens ?? 0} out=${u.outputTokens ?? 0}` +
            (cacheRead ? ` cacheRead=${cacheRead}` : '') +
            (cacheWrite ? ` cacheWrite=${cacheWrite}` : '') +
            (reasoning ? ` reasoning=${reasoning}` : '');
          lastActualInputTokens = u.inputTokens ?? undefined;
          if (onUsage) {
            onUsage({
              inputTokens: u.inputTokens ?? 0,
              outputTokens: u.outputTokens ?? 0,
              totalTokens: u.totalTokens ?? 0,
              cacheReadTokens: cacheRead || undefined,
              cacheWriteTokens: cacheWrite || undefined,
              reasoningTokens: reasoning || undefined,
            });
          }
        }

        debugLog(`Iteration ${iteration} timing: generateText=${Date.now() - apiCallMs}ms total=${Date.now() - iterStartMs}ms${genUsageStr}`);

        if (finished) {
          debugLog('Finish tool was called; finalizing non-streaming iteration before exit');
        }

        debugLog(`Response text: ${result.text || '(empty)'}`);
        debugLog(`Tool calls: ${result.toolCalls?.length ?? 0}`);
        debugLog(`Finish reason: ${result.finishReason}`);
        await maybeLogEmptyOtherProviderResponse(result.finishReason, result.text);

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
            for (const [index, tr] of result.toolResults.entries()) {
              const matchingToolCall = result.toolCalls?.[index];
              completedToolActivities.push({
                toolName: tr.toolName,
                input: matchingToolCall?.input,
                output: tr.output,
              });
              debugLog(`Tool result for ${tr.toolName}:`, JSON.stringify(tr.output).substring(0, 500));
            }
          }
        }

        // Use response.messages to update message history (AI SDK v6 pattern)
        // This is the key change - let the SDK handle message formatting
        messages = [...messages, ...getResponseMessages(result.response)];

        const toolCallsThisIteration = (result.toolCalls ?? []).map((toolCall) => ({
          toolName: toolCall.toolName,
          input: toolCall.input,
        }));
        const iterationSignature = buildIterationSignature({
          finishReason: result.finishReason,
          text: result.text,
          toolCalls: toolCallsThisIteration,
          variables,
        });
        repeatedStallCount = iterationSignature === previousIterationSignature ? repeatedStallCount + 1 : 0;
        previousIterationSignature = iterationSignature;

        if (!finished && repeatedStallCount >= MAX_STALLED_ITERATIONS) {
          success = false;
          outputs.push('Agent loop detected repeated non-progressing responses and stopped early.');
          break;
        }

        // A chat turn ends when the model stops calling tools: that reply is the
        // answer, and the next message starts the next turn.
        if (options.conversational && !finished && toolCallsThisIteration.length === 0) {
          finished = true;
          success = true;
        }

        // If no tool calls were made, nudge the model to act.
        // This handles 'stop', 'other', and any unexpected finish reason.
        // A chat turn is never nudged: the model simply finished its reply.
        const recoveryPrompt = options.conversational
          ? null
          : buildRecoveryPrompt(normalizedOutputVars, repeatedStallCount, toolCallsThisIteration.length === 0);
        const continuationPrompt = !options.conversational && !finished && getLastMessageRole(messages) === 'assistant'
          ? buildAssistantContinuationPrompt(normalizedOutputVars)
          : null;
        const nextPrompt = recoveryPrompt ?? continuationPrompt;
        if (nextPrompt && !finished) {
          messages.push({
            role: 'user',
            content: nextPrompt,
          });
        }
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      errorRetryCount++;
      debugLog(`ERROR in agent loop (attempt ${errorRetryCount}/${MAX_ERROR_RETRIES}): ${message}`);
      if (error instanceof Error && error.stack) {
        debugLog(`Stack trace: ${error.stack}`);
      }
      if (errorRetryCount <= MAX_ERROR_RETRIES) {
        debugLog(`Retrying in ${errorRetryCount}s...`);
        await new Promise(resolve => setTimeout(resolve, 1000 * errorRetryCount));
        continue;
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
  if (options.conversational) {
    // A chat turn keeps the conversation, not a task summary: the person's
    // message and what the agent actually said. No extra summarising model call.
    return {
      success: true,
      outputs,
      variables,
      messages: [
        ...conversationHistory
          .filter((message) => message.role === 'user' || message.role === 'assistant')
          .map((message) => ({
            role: message.role as 'user' | 'assistant',
            content: typeof message.content === 'string' ? message.content : JSON.stringify(message.content),
          })),
        { role: 'user' as const, content: taskDescription },
        ...assistantTextResponses.map((text) => ({ role: 'assistant' as const, content: text })),
      ],
    };
  }

  if (success && Object.keys(variables).length > 0) {
    const varSummary = Object.entries(variables)
      .map(([k, v]) => `${k}: ${v}`)
      .join(', ');
    persistentMessages.push({
      role: 'assistant',
      content: `Task completed. Results: ${varSummary}`,
    });
  } else if (success && normalizedOutputVars.length === 0) {
    // Try to generate a meaningful summary via LLM
    let summary: string | null = null;
    try {
      summary = await generateTaskSummary(model, messages, modelOptions, signal);
      debugLog(`Generated task summary: ${summary?.slice(0, 100)}`);
    } catch (err) {
      debugLog(`Summary generation failed: ${err instanceof Error ? err.message : err}`);
    }
    persistentMessages.push({
      role: 'assistant',
      content: summary ?? buildImplicitTaskSummary(assistantTextResponses, completedToolActivities),
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

function getResponseMessages(response: unknown): ResponseMessage[] {
  if (!response || typeof response !== 'object') {
    return [];
  }

  const maybeMessages = (response as { messages?: unknown }).messages;
  return Array.isArray(maybeMessages) ? maybeMessages as ResponseMessage[] : [];
}

/**
 * Build the system prompt for the agent
 */
async function buildSystemPrompt(options: {
  taskDescription: string;
  outputVars: TypedVar[];
  tools: Map<string, ToolDefinition>;
  workspacePath?: string;
  environment?: ExecutionEnvironment;
  capabilityTool?: string;
  conversational?: boolean;
}): Promise<string> {
  const toolDescriptions: string[] = [];
  const normalizedOutputVars = options.outputVars;

  if (!options.conversational) {
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
  }
  
  // Add user-defined tools
  for (const [name, tool] of options.tools) {
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
  
  const resultSection = normalizedOutputVars.length > 0
    ? [
        'You must set all named results before finishing successfully.',
        ...normalizedOutputVars.map((variable) => {
          const typeStr = variable.type === 'array' && variable.itemType ? `array<${variable.itemType}>` : variable.type;
          return `- ${variable.name}: ${typeStr}`;
        }),
      ].join('\n')
    : 'No named result variables are required. Call finish(true) as soon as the subtask is complete.';

  const stallGuidance = [
    '- Do not repeat the same explanation or identical tool call if the previous turn did not change the state.',
    '- If you are stuck, either choose one different tool or call finish(false).',
    '- Once enough information exists, stop planning, set results if needed, and finish.',
  ].join('\n');

  if (options.conversational) {
    const chatTemplate = await readSystemPromptAsset('chat', { workspacePath: options.workspacePath });
    return chatTemplate
      .replace('{TOOL_DESCRIPTIONS}', toolDescriptions.join('\n'))
      .replace('{ENVIRONMENT_SECTION}', describeEnvironment({
        workspacePath: options.workspacePath,
        environment: options.environment,
        capabilityTool: options.capabilityTool,
      }) + describeCurrentTime());
  }

  const template = await readSystemPromptAsset('task', { workspacePath: options.workspacePath });
  return template
    .replace('{TASK_DESCRIPTION}', options.taskDescription)
    .replace('{TOOL_DESCRIPTIONS}', toolDescriptions.join('\n'))
    .replace('{RESULT_SECTION}', resultSection)
    .replace('{STALL_GUIDANCE}', stallGuidance)
    .replace('{ENVIRONMENT_SECTION}', describeEnvironment({
      workspacePath: options.workspacePath,
      environment: options.environment,
      capabilityTool: options.capabilityTool,
    }) + describeCurrentTime());
}
