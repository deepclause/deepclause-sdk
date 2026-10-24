/**
 * TypeScript context compaction for the REPL (and, later, the one-shot runner).
 *
 * This ports pi's compaction algorithm (`core/compaction/compaction.ts`) to the
 * SDK's `MemoryMessage[]` model. It intentionally shares pi's shapes:
 *
 *  - chars/4 token estimation (`estimateMemoryTokens`);
 *  - `shouldCompact` threshold semantics (`contextWindow - reserveTokens`);
 *  - the cut-point search that keeps a recent tail;
 *  - the structured summarization prompts (Goal / Constraints / Progress /
 *    Decisions / Next Steps / Critical Context).
 *
 * Unlike pi's version it does not use `SessionEntry`/`AgentMessage` trees: the
 * REPL stores memory as `message{role, content}` Prolog terms, mapped to
 * `MemoryMessage[]` on the host. The summarization call goes through the active
 * `LLMBackend`, so it reuses pi's model, credentials, streaming, and cancellation.
 */

import type { LLMBackend, LLMUsage, MemoryMessage } from '../types.js';
import {
  DEFAULT_COMPACTION_SETTINGS,
  shouldCompact as piShouldCompact,
} from '@earendil-works/pi-coding-agent';

// =============================================================================
// Settings
// =============================================================================

/**
 * pi's compaction settings (same shape as its `core/compaction` settings).
 * `DEFAULT_COMPACTION_SETTINGS` and the threshold come straight from pi; only
 * the cut/summarization below is the SDK's port.
 */
export interface CompactionSettings {
  enabled: boolean;
  reserveTokens: number;
  keepRecentTokens: number;
}

export { DEFAULT_COMPACTION_SETTINGS };

/** Keep configured defaults useful for small local-model context windows too. */
export function boundCompactionSettings(settings: CompactionSettings, contextWindow?: number): CompactionSettings {
  if (!contextWindow || contextWindow < 4) return { ...settings };
  const reserveTokens = settings.reserveTokens >= contextWindow ? Math.max(1, Math.floor(contextWindow / 4)) : settings.reserveTokens;
  const available = contextWindow - reserveTokens;
  const keepRecentTokens = settings.keepRecentTokens >= available ? Math.max(1, Math.floor(available / 2)) : settings.keepRecentTokens;
  return { ...settings, reserveTokens, keepRecentTokens };
}

/** Leading marker of a compacted checkpoint message. */
export const SUMMARY_PREFIX = '[Context summary]';

// =============================================================================
// Token accounting
// =============================================================================

/** Per-message overhead, matching the SDK's historical estimate. */
const PER_MESSAGE_OVERHEAD = 4;

/** Conservative chars/4 estimate for one message. */
export function estimateMessageTokens(message: MemoryMessage): number {
  return PER_MESSAGE_OVERHEAD + Math.max(1, Math.ceil(message.content.length / 4));
}

/** Conservative chars/4 estimate for a message list. */
export function estimateMemoryTokens(messages: MemoryMessage[]): number {
  return messages.reduce((total, message) => total + estimateMessageTokens(message), 0);
}

/** Total context tokens from a usage record, preferring the provider total. */
export function calculateContextTokens(usage: LLMUsage): number {
  return usage.totalTokens || (usage.inputTokens + usage.outputTokens + (usage.cacheReadTokens ?? 0) + (usage.cacheWriteTokens ?? 0));
}

/** Whether the context has grown past `contextWindow - reserveTokens`. */
export function shouldCompact(
  contextTokens: number,
  contextWindow: number,
  settings: CompactionSettings = DEFAULT_COMPACTION_SETTINGS,
): boolean {
  // Guard the unknown-window case (pi assumes a positive window).
  if (contextWindow <= 0) {
    return false;
  }
  return piShouldCompact(contextTokens, contextWindow, settings);
}

// =============================================================================
// Cut point
// =============================================================================

export interface CompactionCut {
  /** Index of the first message to keep. `0` means "nothing to summarize". */
  cutIndex: number;
  /** Messages to hand to the summarizer. */
  messagesToSummarize: MemoryMessage[];
  /** Messages retained verbatim. */
  keptMessages: MemoryMessage[];
}

/**
 * Walk from the newest message backwards, accumulating estimated tokens, until
 * `keepRecentTokens` is reached. The cut is then moved back to the nearest
 * `user` boundary so an assistant turn is never separated from its prompt.
 *
 * System messages are never summarized (they carry the checkpoint and the
 * runtime contract) and are excluded from the accumulated size, matching the
 * intent of pi's "never cut the current system prompt" rule.
 */
export function findCompactionCut(
  messages: MemoryMessage[],
  keepRecentTokens: number = DEFAULT_COMPACTION_SETTINGS.keepRecentTokens,
): CompactionCut {
  let accumulated = 0;
  let candidate = messages.length;
  for (let index = messages.length - 1; index >= 0; index -= 1) {
    const message = messages[index];
    if (message.role === 'system') {
      continue;
    }
    accumulated += estimateMessageTokens(message);
    if (accumulated >= keepRecentTokens) {
      candidate = index;
      break;
    }
  }

  if (candidate >= messages.length) {
    return { cutIndex: 0, messagesToSummarize: [], keptMessages: messages.slice() };
  }

  // Move back to a user boundary (the start of a turn), skipping assistants.
  let cutIndex = candidate;
  while (cutIndex > 0 && messages[cutIndex]?.role !== 'user') {
    cutIndex -= 1;
  }

  const messagesToSummarize = messages
    .slice(0, cutIndex)
    .filter((message) => message.role !== 'system');
  const preservedSystem = messages
    .slice(0, cutIndex)
    .filter((message) => message.role === 'system');
  const keptMessages = [...preservedSystem, ...messages.slice(cutIndex)];

  return { cutIndex, messagesToSummarize, keptMessages };
}

// =============================================================================
// Summary detection
// =============================================================================

/** The most recent compacted checkpoint, if the memory already contains one. */
export function findExistingSummary(messages: MemoryMessage[]): string | undefined {
  for (let index = messages.length - 1; index >= 0; index -= 1) {
    const message = messages[index];
    if (message.role === 'system' && message.content.startsWith(SUMMARY_PREFIX)) {
      return message.content.slice(SUMMARY_PREFIX.length).trim();
    }
  }
  return undefined;
}

/** Wrap a summary as the checkpoint message prepended to compacted memory. */
export function buildSummaryMessage(summary: string): MemoryMessage {
  return { role: 'system', content: `${SUMMARY_PREFIX}\n${summary.trim()}` };
}

// =============================================================================
// Summarization prompts (ported from pi)
// =============================================================================

/** Ported verbatim from pi's `SUMMARIZATION_SYSTEM_PROMPT`. */
export const SUMMARIZATION_SYSTEM_PROMPT = `You are a context summarization assistant. Your task is to read a conversation between a user and an AI assistant, then produce a structured summary following the exact format specified.

Do NOT continue the conversation. Do NOT respond to any questions in the conversation. ONLY output the structured summary.`;

/** Ported from pi's `SUMMARIZATION_PROMPT`. */
const SUMMARIZATION_PROMPT = `The messages above are a conversation to summarize. Create a structured context checkpoint summary that another LLM will use to continue the work.

Use this EXACT format:

## Goal
[What is the user trying to accomplish? Can be multiple items if the session covers different tasks.]

## Constraints & Preferences
- [Any constraints, preferences, or requirements mentioned by user]
- [Or "(none)" if none were mentioned]

## Progress
### Done
- [x] [Completed tasks/changes]

### In Progress
- [ ] [Current work]

### Blocked
- [Issues preventing progress, if any]

## Key Decisions
- **[Decision]**: [Brief rationale]

## Next Steps
1. [Ordered list of what should happen next]

## Critical Context
- [Any data, examples, or references needed to continue]
- [Or "(none)" if not applicable]

Keep each section concise. Preserve exact file paths, function names, and error messages.`;

/** Ported from pi's `UPDATE_SUMMARIZATION_INSTRUCTIONS`. */
const UPDATE_SUMMARIZATION_INSTRUCTIONS = `Update the existing structured summary with new information. RULES:
- PRESERVE all existing information from the previous summary
- ADD new progress, decisions, and context from the new messages
- UPDATE the Progress section: move items from "In Progress" to "Done" when completed
- UPDATE "Next Steps" based on what was accomplished
- PRESERVE exact file paths, function names, and error messages
- If something is no longer relevant, you may remove it

Use this EXACT format:

## Goal
[Preserve existing goals, add new ones if the task expanded]

## Constraints & Preferences
- [Preserve existing, add new ones discovered]

## Progress
### Done
- [x] [Include previously done items AND newly completed items]

### In Progress
- [ ] [Current work - update based on progress]

### Blocked
- [Current blockers - remove if resolved]

## Key Decisions
- **[Decision]**: [Brief rationale] (preserve all previous, add new)

## Next Steps
1. [Update based on current state]

## Critical Context
- [Preserve important context, add new if needed]

Keep each section concise. Preserve exact file paths, function names, and error messages.`;

const UPDATE_SUMMARIZATION_PROMPT = `The messages above are NEW conversation messages to incorporate into the existing summary provided in <previous-summary> tags.

${UPDATE_SUMMARIZATION_INSTRUCTIONS}`;

/** Serialize a conversation to text so the model summarizes instead of continuing. */
export function serializeMemoryConversation(messages: MemoryMessage[]): string {
  return messages
    .map((message) => {
      const label = message.role === 'assistant'
        ? 'Assistant'
        : message.role === 'system'
          ? 'System'
          : 'User';
      return `[${label}]: ${message.content}`;
    })
    .join('\n\n');
}

/** Build the user-side summarization prompt, wrapping the conversation in tags. */
export function buildSummarizationPrompt(
  messages: MemoryMessage[],
  previousSummary?: string,
  customInstructions?: string,
): string {
  const conversationText = serializeMemoryConversation(messages);
  let basePrompt = previousSummary ? UPDATE_SUMMARIZATION_PROMPT : SUMMARIZATION_PROMPT;
  if (customInstructions) {
    basePrompt = `${basePrompt}\n\nAdditional focus: ${customInstructions}`;
  }

  let promptText = `<conversation>\n${conversationText}\n</conversation>\n\n`;
  if (previousSummary) {
    promptText += `<previous-summary>\n${previousSummary}\n</previous-summary>\n\n`;
  }
  promptText += basePrompt;
  return promptText;
}

// =============================================================================
// Summarization call
// =============================================================================

export interface GenerateMemorySummaryParams {
  /** Messages to summarize (already cut). System messages are ignored. */
  messages: MemoryMessage[];
  previousSummary?: string;
  backend: LLMBackend;
  model: string;
  reserveTokens?: number;
  signal?: AbortSignal;
  customInstructions?: string;
}

/** Generate a structured summary of `messages` through the active backend. */
export async function generateMemorySummary(
  params: GenerateMemorySummaryParams,
): Promise<{ summary: string; usage?: LLMUsage }> {
  const conversation = params.messages.filter((message) => message.role !== 'system');
  const promptText = buildSummarizationPrompt(conversation, params.previousSummary, params.customInstructions);
  const reserveTokens = params.reserveTokens ?? DEFAULT_COMPACTION_SETTINGS.reserveTokens;
  const maxTokens = Math.max(512, Math.min(Math.floor(0.8 * reserveTokens), 8192));

  const response = await params.backend.complete({
    messages: [
      { role: 'system', content: SUMMARIZATION_SYSTEM_PROMPT },
      { role: 'user', content: promptText },
    ],
    temperature: 0.3,
    maxTokens,
    signal: params.signal,
  });

  if (response.finishReason === 'error' || response.finishReason === 'aborted') {
    throw new Error(`summarization ${response.finishReason}`);
  }

  const summary = (response.text ?? '').trim();
  if (!summary) {
    throw new Error('summarization returned an empty summary');
  }

  return { summary, usage: response.usage };
}

// =============================================================================
// Compact
// =============================================================================

export interface CompactMemoryParams {
  messages: MemoryMessage[];
  backend: LLMBackend;
  model: string;
  contextWindow?: number;
  settings?: CompactionSettings;
  signal?: AbortSignal;
  customInstructions?: string;
  /** Compact even when the context is below the threshold (manual `compact.`). */
  force?: boolean;
}

export type CompactMemoryReason =
  | 'applied'
  | 'disabled'
  | 'below-threshold'
  | 'nothing-to-summarize'
  | 'empty-summary'
  | 'no-reduction'
  | 'failed';

export interface CompactMemoryResult {
  applied: boolean;
  reason: CompactMemoryReason;
  messages: MemoryMessage[];
  summary?: string;
  tokensBefore: number;
  tokensAfter: number;
  usage?: LLMUsage;
  error?: string;
}

/**
 * Compact `messages` if needed (or `force`d). Returns the rewritten list: the
 * preserved system messages, the new checkpoint, then the recent tail.
 */
export async function compactMemory(params: CompactMemoryParams): Promise<CompactMemoryResult> {
  const settings = params.settings ?? DEFAULT_COMPACTION_SETTINGS;
  const messages = params.messages;
  const tokensBefore = estimateMemoryTokens(messages);

  if (!settings.enabled && !params.force) {
    return { applied: false, reason: 'disabled', messages, tokensBefore, tokensAfter: tokensBefore };
  }
  if (
    !params.force
    && params.contextWindow !== undefined
    && !shouldCompact(tokensBefore, params.contextWindow, settings)
  ) {
    return { applied: false, reason: 'below-threshold', messages, tokensBefore, tokensAfter: tokensBefore };
  }

  const cut = findCompactionCut(messages, settings.keepRecentTokens);
  if (cut.messagesToSummarize.length === 0) {
    return { applied: false, reason: 'nothing-to-summarize', messages, tokensBefore, tokensAfter: tokensBefore };
  }

  const previousSummary = findExistingSummary(messages);
  let generated: { summary: string; usage?: LLMUsage };
  try {
    generated = await generateMemorySummary({
      messages: cut.messagesToSummarize,
      previousSummary,
      backend: params.backend,
      model: params.model,
      reserveTokens: settings.reserveTokens,
      signal: params.signal,
      customInstructions: params.customInstructions,
    });
  } catch (error) {
    return {
      applied: false,
      reason: 'failed',
      messages,
      tokensBefore,
      tokensAfter: tokensBefore,
      error: error instanceof Error ? error.message : String(error),
    };
  }

  const rewritten = [...cut.keptMessages, buildSummaryMessage(generated.summary)];
  const tokensAfter = estimateMemoryTokens(rewritten);
  if (tokensAfter >= tokensBefore) {
    return {
      applied: false,
      reason: 'no-reduction',
      messages,
      tokensBefore,
      tokensAfter,
      summary: generated.summary,
      usage: generated.usage,
    };
  }

  return {
    applied: true,
    reason: 'applied',
    messages: rewritten,
    summary: generated.summary,
    tokensBefore,
    tokensAfter,
    usage: generated.usage,
  };
}

// =============================================================================
// Event helper
// =============================================================================

export interface MemoryCompactionEvent {
  scope: 'loop' | 'session';
  trigger: 'before_model_call' | 'before_user_message';
  action: 'applied' | 'skipped' | 'failed';
  beforeTokens: number;
  afterTokens: number;
  error?: string;
  usage?: LLMUsage;
}

/** Turn a `CompactMemoryResult` into the fields consumed by a `memory_compaction` event. */
export function toCompactionEvent(
  scope: MemoryCompactionEvent['scope'],
  trigger: MemoryCompactionEvent['trigger'],
  result: CompactMemoryResult,
): MemoryCompactionEvent {
  const action: MemoryCompactionEvent['action'] = result.applied
    ? 'applied'
    : result.reason === 'failed' || result.reason === 'empty-summary'
      ? 'failed'
      : 'skipped';
  return {
    scope,
    trigger,
    action,
    beforeTokens: result.tokensBefore,
    afterTokens: result.tokensAfter,
    error: result.error,
    usage: result.usage,
  };
}
