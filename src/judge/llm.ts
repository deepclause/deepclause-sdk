/**
 * LLM judge backend.
 *
 * Answers the same generic questions through an ordinary LLM. When an injected
 * `LLMBackend` is present (for example, pi's active model in the extension) it
 * is used directly; otherwise the SDK provider path is used. Answers are
 * labels or self-reported probabilities, never calibrated ones.
 */

import { generateLlmReply } from '../prolog/bridge.js';
import type { LLMBackend, MemoryMessage } from '../types.js';
import type {
  JudgeAnswer,
  JudgeBackend,
  JudgeBackendRequest,
  JudgeCapabilities,
  JudgeQuestion,
} from './types.js';

export interface LLMJudgeBackendOptions {
  llmBackend?: LLMBackend;
  model?: string;
  provider?: string;
  temperature?: number;
  maxTokens?: number;
  baseUrl?: string;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  providerOptions?: Record<string, Record<string, any>>;
}

const LLM_CAPABILITIES: JudgeCapabilities = {
  calibrated: false,
  probability: true,
  confidence: false,
  independentQuestions: false,
  batch: true,
  structuredCriteria: false,
  maxQuestions: 32,
  maxOptions: 64,
  maxLevels: 10,
  stateTokenBudget: 16_000,
};

function describeQuestion(question: JudgeQuestion): string {
  if (question.kind === 'choose') {
    const options = (question.options ?? [])
      .map((option) => (option.description ? `${option.id} (${JSON.stringify(option.description)})` : option.id))
      .join(', ');
    return `[choose] ${String(question.instruction)}\n  options: ${options}`;
  }
  if (question.kind === 'rate') {
    const levels = (question.levels ?? []).map((level) => JSON.stringify(level)).join(', ');
    return `[rate] ${String(question.instruction)}\n  levels: ${levels}`;
  }
  if (question.kind === 'verify') {
    return `[verify] ${String(question.instruction)}\n  answer yes, no, or unknown, with a probability 0..1`;
  }
  return `[probability] ${String(question.instruction)}\n  answer only with a probability 0..1`;
}

export function buildJudgePrompt(state: unknown, questions: JudgeQuestion[]): string {
  const questionLines = questions.map((question, index) => `${index + 1}. ${describeQuestion(question)}`).join('\n');
  return [
    'You are a precise judge. Answer every question using only the state and the allowed answers.',
    'Return strict JSON of the form {"answers":[...]} with one entry per question, in the same order.',
    'Each entry uses "value" for choose/rate/verify and "probability" for verify/probability.',
    '',
    `STATE:\n${JSON.stringify(state, null, 2)}`,
    '',
    `QUESTIONS:\n${questionLines}`,
  ].join('\n');
}

function extractJson(text: string): unknown {
  const fenced = /```(?:json)?\s*([\s\S]*?)```/i.exec(text);
  const candidate = fenced ? fenced[1] : text;
  const start = candidate.indexOf('{');
  const end = candidate.lastIndexOf('}');
  if (start === -1 || end === -1 || end <= start) throw new Error('LLM judge response did not contain JSON');
  return JSON.parse(candidate.slice(start, end + 1));
}

interface RawLlmAnswer {
  value?: unknown;
  probability?: unknown;
}

export function parseJudgeResponse(text: string, questions: JudgeQuestion[]): JudgeAnswer[] {
  const parsed = extractJson(text) as { answers?: RawLlmAnswer[] };
  const rawAnswers = Array.isArray(parsed.answers) ? parsed.answers : [];
  return questions.map((question, index) => {
    const raw = rawAnswers[index] ?? {};
    const probability = typeof raw.probability === 'number' ? raw.probability : undefined;
    const value = typeof raw.value === 'string' ? raw.value : undefined;
    const basis: JudgeAnswer['basis'] = probability !== undefined ? 'self_reported' : 'label';
    if (question.kind === 'probability') {
      return { id: question.id, kind: question.kind, probability: probability ?? 0, basis };
    }
    if (question.kind === 'verify') {
      return {
        id: question.id,
        kind: question.kind,
        value: value ?? (probability !== undefined ? (probability >= 0.5 ? 'yes' : 'no') : 'unknown'),
        probability,
        basis,
      };
    }
    return { id: question.id, kind: question.kind, value, basis };
  });
}

export function createLLMJudgeBackend(options: LLMJudgeBackendOptions = {}): JudgeBackend {
  return {
    id: 'llm',
    capabilities: LLM_CAPABILITIES,
    async complete(request: JudgeBackendRequest) {
      const messages: MemoryMessage[] = [
        {
          role: 'system',
          content: 'You are a precise judge. Reply with strict JSON and nothing else.',
        },
        { role: 'user', content: buildJudgePrompt(request.state, request.questions) },
      ];
      const { text, usage } = await generateLlmReply({
        messages,
        modelOptions: {
          provider: options.provider ?? 'openai',
          model: options.model ?? request.model ?? 'gpt-4o-mini',
          temperature: options.temperature ?? 0,
          maxOutputTokens: options.maxTokens,
          baseUrl: options.baseUrl,
          providerOptions: options.providerOptions,
        },
        signal: request.signal,
        llmBackend: options.llmBackend,
      });
      return {
        answers: parseJudgeResponse(text, request.questions),
        usage: usage
          ? {
              inputTokens: usage.inputTokens,
              outputTokens: usage.outputTokens,
              totalTokens: usage.totalTokens,
            }
          : undefined,
      };
    },
  };
}
