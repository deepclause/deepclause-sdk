/**
 * Jev (TypeSafe System One) judge backend.
 *
 * Calls `POST /v1/systemone` with the explicit state and the typed questions,
 * and maps the typed answers back to `JudgeAnswer`s. Calibration, full
 * distributions, and confidence are available from this backend.
 *
 * The API key is supplied by the caller (for example, the pi extension reading
 * it from an environment variable); the SDK never reads or stores credentials.
 */

import {
  type JudgeAnswer,
  type JudgeBackend,
  type JudgeBackendRequest,
  type JudgeCapabilities,
  type JudgeLevel,
  type JudgeQuestion,
} from './types.js';

export interface JevJudgeBackendOptions {
  apiKey?: string;
  baseUrl?: string;
  model?: string;
  maxRetries?: number;
  timeoutMs?: number;
  fetch?: typeof fetch;
}

const JEV_CAPABILITIES: JudgeCapabilities = {
  calibrated: true,
  probability: true,
  confidence: true,
  independentQuestions: true,
  batch: true,
  structuredCriteria: true,
  maxQuestions: 128,
  maxOptions: 255,
  maxLevels: 10,
  stateTokenBudget: 32_000,
};

function levelToText(level: JudgeLevel): unknown {
  if (level && typeof level === 'object' && !Array.isArray(level) && 'what' in level) {
    return level;
  }
  return level;
}

function toJevQuestion(question: JudgeQuestion): Record<string, unknown> {
  if (question.kind === 'choose') {
    const criteria: Record<string, unknown> = {};
    for (const option of question.options ?? []) criteria[option.id] = option.description ?? null;
    return { type: 'choice', instructions: question.instruction, criteria };
  }
  if (question.kind === 'rate') {
    return {
      type: 'score',
      instructions: question.instruction,
      criteria: (question.levels ?? []).map(levelToText),
    };
  }
  const noul: Record<string, unknown> = { type: 'noul', instructions: question.instruction };
  if (question.criteria) {
    noul.criteria = {
      true: question.criteria.true ?? null,
      false: question.criteria.false ?? null,
    };
  }
  return noul;
}

function distributionForQuestion(question: JudgeQuestion, probabilities: Record<string, number> | undefined): number[] | undefined {
  if (!probabilities) return undefined;
  if (question.kind === 'choose') {
    const optionIds = (question.options ?? []).map((option) => option.id);
    return optionIds.map((id) => Number(probabilities[id] ?? 0));
  }
  if (question.kind === 'rate') {
    const count = (question.levels ?? []).length;
    return Array.from({ length: count }, (_, index) => Number(probabilities[String(index)] ?? 0));
  }
  return undefined;
}

function levelLabel(question: JudgeQuestion, index: number): string {
  const level = question.levels?.[index];
  if (typeof level === 'string') return level;
  if (typeof level === 'number' || typeof level === 'boolean') return String(level);
  if (level && typeof level === 'object' && typeof (level as { what?: unknown }).what === 'string') {
    return (level as { what: string }).what;
  }
  return `level_${index}`;
}

function argmax(values: number[]): number {
  let best = 0;
  for (let index = 1; index < values.length; index++) {
    if ((values[index] ?? 0) > (values[best] ?? 0)) best = index;
  }
  return best;
}

interface JevRawAnswer {
  type: string;
  choice?: string;
  score?: number;
  noul?: number;
  probabilities?: Record<string, number>;
  confidence?: number;
}

function fromJevAnswer(question: JudgeQuestion, raw: JevRawAnswer): JudgeAnswer {
  const distribution = distributionForQuestion(question, raw.probabilities);
  if (question.kind === 'choose') {
    return {
      id: question.id,
      kind: 'choose',
      value: raw.choice,
      confidence: raw.confidence,
      distribution,
      basis: 'calibrated',
    };
  }
  if (question.kind === 'rate') {
    const index = distribution && distribution.length > 0 ? argmax(distribution) : Math.round(raw.score ?? 0);
    return {
      id: question.id,
      kind: 'rate',
      value: levelLabel(question, index),
      confidence: raw.confidence,
      distribution,
      basis: 'calibrated',
    };
  }
  const probability = typeof raw.noul === 'number' ? raw.noul : 0;
  if (question.kind === 'probability') {
    return { id: question.id, kind: 'probability', probability, basis: 'calibrated' };
  }
  return {
    id: question.id,
    kind: 'verify',
    value: probability >= 0.5 ? 'yes' : 'no',
    probability,
    basis: 'calibrated',
  };
}

function retryableStatus(status: number): boolean {
  return status === 429 || status === 529;
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function combineSignals(signal: AbortSignal | undefined, timeoutMs: number): { signal: AbortSignal; dispose: () => void } {
  const controller = new AbortController();
  const disposers: Array<() => void> = [];
  if (signal) {
    if (signal.aborted) controller.abort(signal.reason);
    else {
      const onAbort = (): void => controller.abort(signal.reason);
      signal.addEventListener('abort', onAbort, { once: true });
      disposers.push(() => signal.removeEventListener('abort', onAbort));
    }
  }
  const timer = setTimeout(() => controller.abort(new Error(`Jev request timed out after ${timeoutMs}ms`)), timeoutMs);
  disposers.push(() => clearTimeout(timer));
  return {
    signal: controller.signal,
    dispose: () => disposers.forEach((dispose) => dispose()),
  };
}

export function createJevJudgeBackend(options: JevJudgeBackendOptions = {}): JudgeBackend {
  const baseUrl = (options.baseUrl ?? 'https://api.typesafe.ai').replace(/\/$/, '');
  const model = options.model ?? 'jev-latest';
  const maxRetries = options.maxRetries ?? 2;
  const timeoutMs = options.timeoutMs ?? 30_000;
  const fetchImpl = options.fetch ?? fetch;

  return {
    id: 'jev',
    capabilities: JEV_CAPABILITIES,
    async complete(request: JudgeBackendRequest) {
      if (!options.apiKey) throw new Error('Jev backend requires an API key');
      const questions: Record<string, unknown> = {};
      for (const question of request.questions) questions[question.id] = toJevQuestion(question);
      const body = JSON.stringify({ state: request.state, model, questions });

      let lastError: unknown;
      for (let attempt = 0; attempt <= maxRetries; attempt++) {
        const { signal, dispose } = combineSignals(request.signal, timeoutMs);
        try {
          const response = await fetchImpl(`${baseUrl}/v1/systemone`, {
            method: 'POST',
            headers: {
              Authorization: `Bearer ${options.apiKey}`,
              'Content-Type': 'application/json',
            },
            body,
            signal,
          });
          if (!response.ok) {
            const text = await response.text().catch(() => '');
            const error = new Error(`Jev request failed (${response.status}): ${text.slice(0, 500)}`);
            if (retryableStatus(response.status) && attempt < maxRetries) {
              lastError = error;
              await sleep(250 * 2 ** attempt);
              continue;
            }
            throw error;
          }
          const payload = (await response.json()) as {
            model?: string;
            answers?: Record<string, JevRawAnswer>;
            usage?: { input_tokens?: number; output_tokens?: number };
          };
          const rawAnswers = payload.answers ?? {};
          return {
            answers: request.questions.map((question) => {
              const raw = rawAnswers[question.id];
              if (!raw) throw new Error(`Jev returned no answer for question ${question.id}`);
              return fromJevAnswer(question, raw);
            }),
            model: payload.model ?? model,
            usage: payload.usage
              ? {
                  inputTokens: payload.usage.input_tokens ?? 0,
                  outputTokens: payload.usage.output_tokens ?? 0,
                  totalTokens: (payload.usage.input_tokens ?? 0) + (payload.usage.output_tokens ?? 0),
                }
              : undefined,
          };
        } catch (error) {
          if (attempt < maxRetries && !request.signal?.aborted) {
            lastError = error;
            await sleep(250 * 2 ** attempt);
            continue;
          }
          throw error;
        } finally {
          dispose();
        }
      }
      throw lastError instanceof Error ? lastError : new Error('Jev request failed');
    },
  };
}
