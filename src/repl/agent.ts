/**
 * The REPL authoring agent.
 *
 * This is a normal TypeScript agent loop over the injected `LLMBackend`: a
 * system prompt plus harness tools. It replaces the old DML `task_named` meta
 * frame for authoring, so the runtime's frame machinery is only used for
 * DML-internal `task/N` calls.
 */

import { readAgentPromptAsset } from '../system/assets/index.js';
import { validateToolArguments } from './tool-arguments.js';
import { describeRuntimeContext } from '../system/environment.js';
import type { ExecutionEnvironment } from '../system/runtime/shell-manager.js';
import { loadProjectContext } from './project-context.js';
import type {
  DMLEvent,
  LLMBackend,
  LLMBackendMessage,
  LLMBackendResponse,
  LLMBackendToolCall,
  MemoryMessage,
  ToolDefinition,
} from '../types.js';
import { compactMemory, shouldCompact, estimateMemoryTokens, type CompactionSettings, type CompactMemoryResult } from './compaction.js';
import { formatModelCatalogEntry, type ModelCatalogEntry } from './model-resolver.js';

export interface HarnessAgentOptions {
  tools: Map<string, ToolDefinition>;
  /** Tool names withheld to break a call cycle (see `excludedTools`). */
  excludedTools?: string[];
  llmBackend: LLMBackend;
  model: string;
  temperature?: number;
  maxTokens?: number;
  streaming?: boolean;
  /** Maximum model calls for one `run` (0 = unlimited). */
  maxModelCalls?: number;
  /**
   * Ask the human a question mid-turn (used to ask whether to extend the
   * model-call budget). Returns undefined when there is no interactive channel.
   */
  askInput?: (prompt: string, options?: string[]) => Promise<string | undefined>;
  contextWindow?: number;
  compaction?: { settings: CompactionSettings; auto: boolean };
  /** Workspace root, so a `.deepclause/system/AGENT_PROMPT.md` override is used. */
  workspacePath?: string;
  /** Effective shell backend, reported to the agent in its environment block. */
  environment?: ExecutionEnvironment;
  /** Name of the single capability host tool (default `bash`; the browser uses `python`). */
  capabilityTool?: string;
  /** The session's active model (`provider:model`), shown to the agent. */
  currentModel?: string;
  /** Models available this session (via `with_model/2`). */
  modelCatalog?: ModelCatalogEntry[];
  /** Pre-loaded documentation for the `reference` tool (handled by the controller). */
  knownMessages?: MemoryMessage[];
  /**
   * Replace the bundled authoring prompt. Used by embeddings with a smaller
   * model/context, where the full guide would exceed the context window.
   */
  agentSystemPrompt?: string;
}

export interface HarnessAgentRunOptions {
  signal?: AbortSignal;
}

const MAX_TOOL_RESULT_CHARS = 8000;

/** Generate the `TOOLS` section of the system prompt from the tool schemas. */
/**
 * The message a nested agent sees for a tool it may not call.
 *
 * Two different situations share this path, and they need different advice:
 * a name that was never a tool (a hallucinated or misspelled call), and a real
 * tool withheld to break a call cycle. Saying only "Tool not found" for the
 * second makes a working harness look broken, and the agent retries it.
 */
function unavailableToolMessage(name: string, excluded: Set<string>): string {
  if (excluded.has(name)) {
    return `Tool '${name}' is not available in this task: it is already running further up the call `
      + 'chain, and would recurse. Do not call it again. Work with the text you were given, or '
      + 'state what is missing.';
  }
  return `Tool not found: ${name}`;
}

export function describeTools(tools: Map<string, ToolDefinition>): string {
  const lines: string[] = [];
  for (const [name, tool] of tools) {
    const properties = (tool.parameters.properties ?? {}) as Record<string, { type?: string }>;
    const required = new Set(tool.parameters.required ?? []);
    const params = Object.entries(properties)
      .map(([param, schema]) => `${param}${required.has(param) ? '' : '?'}: ${schema.type ?? 'any'}`)
      .join(', ');
    lines.push(`- ${name}(${params}): ${tool.description}`);
  }
  return lines.join('\n');
}

/**
 * Build the authoring agent's system prompt from the single bundled prompt file
 * (or a `.deepclause/system/AGENT_PROMPT.md` override). The file is used as-is —
 * no template substitution — plus the workspace `AGENTS.md` when present.
 */
export async function buildAgentSystemPrompt(options: {
  workspacePath?: string;
  currentModel?: string;
  modelCatalog?: ModelCatalogEntry[];
  /** Effective shell backend; see DMLSessionOptions.executionBackend. */
  environment?: ExecutionEnvironment;
  /** Name of the single capability host tool (default `bash`; the browser uses `python`). */
  capabilityTool?: string;
  /** Injectable clock, so tests do not depend on the wall calendar. */
  now?: Date;
  /** Optional replacement for the bundled prompt (e.g. a local-model profile). */
  systemPrompt?: string;
} = {}): Promise<string> {
  let prompt = '';
  if (options.systemPrompt !== undefined) {
    prompt = options.systemPrompt;
  } else {
    try {
      prompt = await readAgentPromptAsset({ workspacePath: options.workspacePath });
    } catch {
      prompt = 'You are an agent with access to a DML REPL.';
    }
  }

  // The models this session can switch to with `with_model/2`, with catalog prices.
  // A replacement prompt is already a deliberate, bounded profile; appending the
  // full catalog to it defeats that budget.
  const catalog = options.systemPrompt === undefined ? (options.modelCatalog ?? []) : [];
  if (options.currentModel || catalog.length > 0) {
    // Discovery is on demand through models(); do not append dozens of price
    // entries to every authoring turn. Keep a bounded preview for simple routing.
    const shown = catalog.slice(0, 4).map(formatModelCatalogEntry);
    prompt += [
      '',
      'AVAILABLE MODELS  (price = USD per 1M tokens, input/output)',
      options.currentModel ? `- current: ${options.currentModel}` : '',
      shown.length > 0 ? `- with_model targets (preview): ${shown.join(', ')}${catalog.length > shown.length ? `; ${catalog.length} configured models total — call models for all ids/prices.` : ''}` : '',
      '- Pick the cheapest model that can do the job; use `with_model/2` to route calls.',
      '- A `with_model` spec must name an available configured model. Use the host model settings for other models; do not invent model ids or credentials.',
    ].filter(Boolean).join('\n') + '\n';
  }

  // Append a workspace `AGENTS.md` so repo-specific rules always apply. A
  // replacement profile is self-contained: appending the project contract can
  // double its size and push a local model over its context window.
  if (options.systemPrompt === undefined) {
    const project = await loadProjectContext(options.workspacePath);
    if (project) {
      prompt += `\n\nPROJECT INSTRUCTIONS (AGENTS.md)\n${project.content}\n`;
    }
  }

  // Where we are running and what time it is, last: these change per turn, so
  // everything stable (the docs, the model catalog, AGENTS.md) stays ahead of
  // them and can still be prefix-cached.
  prompt += describeRuntimeContext({
    workspacePath: options.workspacePath,
    environment: options.environment,
    capabilityTool: options.capabilityTool,
    now: options.now,
  });
  return prompt;
}

/** Project the agent's message list onto `MemoryMessage[]` for compaction. */
function toMemoryMessages(messages: LLMBackendMessage[]): MemoryMessage[] {
  const out: MemoryMessage[] = [];
  for (const message of messages) {
    if (message.role === 'system') {
      out.push({ role: 'system', content: message.content });
    } else if (message.role === 'user') {
      out.push({ role: 'user', content: message.content });
    } else if (message.role === 'assistant') {
      const calls = (message.toolCalls ?? [])
        .map((call) => `${call.name}(${JSON.stringify(call.arguments)})`)
        .join(', ');
      out.push({ role: 'assistant', content: calls ? `${message.content}\n[tool calls: ${calls}]` : message.content });
    } else {
      out.push({ role: 'user', content: `[tool result: ${message.toolName ?? 'tool'}] ${message.content}` });
    }
  }
  return out;
}

function fromMemoryMessages(messages: MemoryMessage[]): LLMBackendMessage[] {
  return messages.map((message) => ({ role: message.role, content: message.content }));
}

function stringifyToolResult(result: unknown): string {
  let text: string;
  if (typeof result === 'string') {
    text = result;
  } else {
    try {
      text = JSON.stringify(result);
    } catch {
      text = String(result);
    }
  }
  return text.length > MAX_TOOL_RESULT_CHARS
    ? `${text.slice(0, MAX_TOOL_RESULT_CHARS)}\n… [truncated]`
    : text;
}

/** A tool that returned `{ ok: false }` or `{ error }` should render as failed. */
function toolResultFailure(result: unknown): string | undefined {
  if (!result || typeof result !== 'object') return undefined;
  const record = result as Record<string, unknown>;
  if (record.status === 'suspended') return undefined; // call returned a resumable pause, not an execution error
  if (record.ok === false) {
    return typeof record.error === 'string' && record.error ? record.error : 'failed';
  }
  if (typeof record.error === 'string' && record.error) return record.error;
  return undefined;
}

/**
 * A multi-turn authoring agent. `run` yields `DMLEvent`s (streaming text,
 * reasoning, tool calls, usage, errors) and follows the tool loop until the
 * model calls `finish` or the budget runs out.
 */
export class HarnessAgent {
  readonly tools: Map<string, ToolDefinition>;
  /**
   * Tool names withheld from this agent to break a call cycle. They are still
   * declared to the model, so a call to one can be answered with the reason
   * instead of looking like a missing tool.
   */
  readonly excludedTools: Set<string>;
  private readonly options: HarnessAgentOptions;
  private messages: LLMBackendMessage[];
  /** User messages delivered while a turn is running (pi-style steering). */
  private readonly pendingSteers: string[] = [];
  /** Aborts the in-flight model call when a steer arrives so it is seen now. */
  private steerAbort?: AbortController;

  constructor(options: HarnessAgentOptions) {
    this.options = options;
    this.tools = options.tools;
    this.excludedTools = new Set(options.excludedTools ?? []);
    this.messages = options.knownMessages
      ? options.knownMessages
          // Defensively drop any older host nudge persisted as a user message.
          .filter((message) => message.content.trim() !== 'Continue: call a tool, or call finish when done.')
          .map((message) => ({ role: message.role, content: message.content }))
      : [];
  }

  /** Record a user-triggered external workflow result without fabricating tool calls. */
  addContext(text: string): void { this.messages.push({ role: 'user', content: text }); }

  /** The persisted conversation (real user/assistant turns only, no host nudges). */
  getMessages(): MemoryMessage[] {
    return this.messages
      .filter((message) => message.role === 'system' || message.role === 'user' || message.role === 'assistant')
      // Drop assistant turns that only carry tool calls (empty text).
      .filter((message) => message.role !== 'assistant' || (message.content ?? '').trim() !== '')
      .map((message) => ({ role: message.role as MemoryMessage['role'], content: message.content }));
  }

  /**
   * Deliver a message into the running turn (pi-style steering). It is queued
   * and injected before the next model call; when a model call is in flight it
   * is aborted so the steer is seen immediately.
   */
  steer(text: string): void {
    const trimmed = text.trim();
    if (!trimmed) return;
    this.pendingSteers.push(trimmed);
    this.steerAbort?.abort();
  }

  /**
   * Compact the agent's own conversation. `force` ignores the threshold (used by
   * the manual `compact.` command); the loop hook checks the threshold itself.
   */
  async compactMemory(force = false, signal?: AbortSignal, customInstructions?: string): Promise<CompactMemoryResult> {
    const settings = this.options.compaction?.settings;
    if (!settings?.enabled) {
      return { applied: false, reason: 'disabled', messages: this.getMessages(), tokensBefore: 0, tokensAfter: 0 };
    }
    const result = await compactMemory({
      messages: toMemoryMessages(this.messages),
      backend: this.options.llmBackend,
      model: this.options.model,
      contextWindow: this.options.contextWindow,
      settings,
      signal,
      customInstructions,
      force,
    });
    if (result.applied) {
      this.messages = fromMemoryMessages(result.messages);
    }
    return result;
  }

  /**
   * Ask the human whether to continue after the model-call budget is exhausted.
   * Returns false when there is no interactive channel (autonomous runs keep the
   * current abort behaviour).
   */
  private async askBudgetExtension(used: number): Promise<boolean> {
    const ask = this.options.askInput;
    if (!ask) {
      return false;
    }
    const perChunk = this.options.maxModelCalls ?? 0;
    const answer = await ask(
      `The turn has reached its model-call budget (${used} calls).\n`
      + `Continue for another ${perChunk} calls? [yes/no]\n`
      + 'change the limit: repl.maxModelCalls in .deepclause/config.json '
      + '("repl": { "maxModelCalls": N }), or pass --max-model-calls N; 0 means unlimited',
      ['yes', 'no'],
    );
    return /^(y|yes)$/i.test((answer ?? '').trim());
  }

  async *run(request: string, options: HarnessAgentRunOptions = {}): AsyncGenerator<DMLEvent> {
    const { llmBackend, tools } = this.options;
    const streaming = this.options.streaming ?? true;
    const maxModelCalls = this.options.maxModelCalls ?? 0;
    const settings = this.options.compaction?.settings;
    const autoCompact = this.options.compaction?.auto ?? false;

    const systemPrompt = await buildAgentSystemPrompt({
      workspacePath: this.options.workspacePath,
      currentModel: this.options.currentModel,
      modelCatalog: this.options.modelCatalog,
      environment: this.options.environment,
      capabilityTool: this.options.capabilityTool,
      systemPrompt: this.options.agentSystemPrompt,
    });

    this.messages.push({ role: 'user', content: request });

    const backendTools = [...tools].map(([name, tool]) => ({
      name,
      description: tool.description,
      parameters: tool.parameters as unknown as Record<string, unknown>,
    }));

    let calls = 0;
    // The model-call budget for this turn; `0` means unlimited. It grows by
    // `maxModelCalls` each time the human agrees to continue.
    let budget = maxModelCalls;
    let finished = false;
    let lastText = '';
    let lastInputTokens: number | undefined;
    const maxIterations = 1000;

    while (!finished && calls < maxIterations) {
      if (options.signal?.aborted) break;
      if (budget > 0 && calls >= budget) {
        const extend = await this.askBudgetExtension(budget);
        if (!extend) {
          yield {
            type: 'error',
            content: `turn stopped: model-call budget exhausted (${budget} calls). `
              + 'Raise repl.maxModelCalls in .deepclause/config.json (or pass --max-model-calls); 0 = unlimited.',
          };
          break;
        }
        budget += maxModelCalls;
      }
      calls += 1;

      // Inject any steering messages submitted during the previous iteration.
      while (this.pendingSteers.length > 0) {
        this.messages.push({ role: 'user', content: this.pendingSteers.shift()! });
      }

      if (autoCompact && settings?.enabled && this.options.contextWindow) {
        if (shouldCompact(lastInputTokens ?? estimateMemoryTokens(toMemoryMessages(this.messages)), this.options.contextWindow, settings)) {
          const result = await this.compactMemory(true, options.signal);
          if (result.applied) {
            yield {
              type: 'memory_compaction',
              content: `compact agent.before_model_call applied ${result.tokensBefore} -> ${result.tokensAfter} tokens`,
              compactionScope: 'session',
              compactionTrigger: 'before_model_call',
              compactionAction: 'applied',
              beforeTokens: result.tokensBefore,
              afterTokens: result.tokensAfter,
            };
          }
        }
      }

      yield { type: 'model_call', modelCallState: 'started', modelId: this.options.model };

      // Stream deltas live via a queue while the (single) backend call runs.
      const streamQueue: DMLEvent[] = [];
      let wake: (() => void) | null = null;
      let settled = false;
      let response: LLMBackendResponse | undefined;
      let failure: unknown;
      const push = (event: DMLEvent): void => {
        streamQueue.push(event);
        if (wake) {
          const resolve = wake;
          wake = null;
          resolve();
        }
      };
      const requestMessages: LLMBackendMessage[] = [
        { role: 'system', content: systemPrompt },
        ...this.messages,
      ];

      this.steerAbort = new AbortController();
      const callSignal = options.signal
        ? AbortSignal.any([options.signal, this.steerAbort.signal])
        : this.steerAbort.signal;

      const completion = llmBackend.complete({
        messages: requestMessages,
        tools: backendTools,
        temperature: this.options.temperature,
        maxTokens: this.options.maxTokens,
        signal: callSignal,
        onText: streaming ? (chunk) => push({ type: 'stream', content: chunk }) : undefined,
        onReasoning: streaming ? (chunk) => push({ type: 'reasoning', content: chunk }) : undefined,
      }).then(
        (value) => { response = value; settled = true; if (wake) { const r = wake; wake = null; r(); } },
        (error) => { failure = error; settled = true; if (wake) { const r = wake; wake = null; r(); } },
      );

      while (true) {
        while (streamQueue.length > 0) yield streamQueue.shift()!;
        if (settled) break;
        await new Promise<void>((resolve) => { wake = resolve; });
      }
      while (streamQueue.length > 0) yield streamQueue.shift()!;
      await completion;

      if (failure || !response) {
        const steered = Boolean(this.steerAbort?.signal.aborted) && !options.signal?.aborted;
        this.steerAbort = undefined;
        if (steered && this.pendingSteers.length > 0) {
          // A steer interrupted this call; re-run with the queued message.
          continue;
        }
        yield { type: 'model_call', modelCallState: 'failed', modelId: this.options.model };
        yield { type: 'error', content: failure instanceof Error ? failure.message : String(failure) };
        return;
      }
      this.steerAbort = undefined;
      yield { type: 'model_call', modelCallState: 'completed', modelId: this.options.model };

      if (response.usage) {
        lastInputTokens = (response.usage.inputTokens ?? 0)
          + (response.usage.cacheReadTokens ?? 0)
          + (response.usage.cacheWriteTokens ?? 0);
        yield { type: 'usage', usage: response.usage, usageSource: 'llm', usageModel: this.options.model };
      }
      if (response.text) {
        lastText = response.text;
      }

      const toolCalls: LLMBackendToolCall[] = response.toolCalls ?? [];
      this.messages.push({
        role: 'assistant',
        content: response.text,
        toolCalls,
        providerData: response.providerData,
      });

      if (toolCalls.length === 0) {
        // No tool call: the model answered directly or stopped; end the turn.
        finished = true;
        break;
      }

      for (const call of toolCalls) {
        yield { type: 'tool_call', toolName: call.name, toolArgs: call.arguments, toolState: 'starting' };
        // `finish` is not offered to the model any more; a turn ends when the
        // model replies without tool calls. Tolerate a stray `finish` (older
        // transcripts, tests) as an explicit stop.
        if (call.name === 'finish') {
          finished = true;
          yield { type: 'tool_call', toolName: 'finish', toolArgs: call.arguments, toolState: 'completed', toolSummary: 'done' };
          this.messages.push({ role: 'tool', content: '{"finished":true}', toolCallId: call.id, toolName: 'finish' });
          break;
        }
        const tool = tools.get(call.name);
        if (!tool) {
          const message = unavailableToolMessage(call.name, this.excludedTools);
          yield { type: 'tool_call', toolName: call.name, toolState: 'failed', toolError: message };
          this.messages.push({ role: 'tool', content: JSON.stringify({ error: message }), toolCallId: call.id, toolName: call.name });
          continue;
        }
        let result: unknown;
        let toolFailure: unknown;
        let toolSettled = false;
        const toolEvents: DMLEvent[] = [];
        let toolWake: (() => void) | null = null;
        const wakeTool = (): void => {
          if (toolWake) { const resolve = toolWake; toolWake = null; resolve(); }
        };
        const toolStartedAt = Date.now();
        // An unknown parameter cannot mean what it says; report it as this
        // tool's result so the model can correct the call on its next turn.
        const argumentError = validateToolArguments(call.name, tools.get(call.name)?.parameters, call.arguments);
        const toolPromise = (argumentError.ok
          ? tool.execute(call.arguments, {
            signal: options.signal,
            onEvent: (event) => { toolEvents.push(event); wakeTool(); },
          })
          : Promise.resolve({ error: argumentError.error })
        ).then(
          (value) => { result = value; toolSettled = true; wakeTool(); },
          (error) => { toolFailure = error; toolSettled = true; wakeTool(); },
        );
        // Stream events the tool produces (e.g. an `execute` child frame) live.
        while (true) {
          while (toolEvents.length > 0) yield toolEvents.shift()!;
          if (toolSettled) break;
          await new Promise<void>((resolve) => { toolWake = resolve; });
        }
        while (toolEvents.length > 0) yield toolEvents.shift()!;
        await toolPromise;
        const failureMessage = toolFailure
          ? (toolFailure instanceof Error ? toolFailure.message : String(toolFailure))
          : toolResultFailure(result);
        const text = stringifyToolResult(failureMessage ? { error: failureMessage } : result);
        if (failureMessage) {
          yield { type: 'tool_call', toolName: call.name, toolArgs: call.arguments, toolState: 'failed', toolError: text.slice(0, 200), toolDurationMs: Date.now() - toolStartedAt };
        } else {
          yield { type: 'tool_call', toolName: call.name, toolArgs: call.arguments, toolState: 'completed', toolSummary: text.slice(0, 200), toolDurationMs: Date.now() - toolStartedAt };
        }
        this.messages.push({ role: 'tool', content: text, toolCallId: call.id, toolName: call.name });
      }
    }

    if (!finished && !options.signal?.aborted) {
      yield { type: 'log', content: 'authoring agent reached its iteration limit without finishing' };
    }
    if (!streaming && lastText) {
      yield { type: 'output', content: lastText };
    }
  }
}
