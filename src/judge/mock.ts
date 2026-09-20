/**
 * Mock judge backend.
 *
 * Useful for development and tests: it returns deterministic, schema-valid
 * answers without any network access. `createMockJevJudgeBackend` configures it
 * with Jev-like calibrated capabilities so the real Jev path can be developed
 * and exercised before an API key is available.
 */

import {
  withJudgeCapabilities,
  type JudgeAnswer,
  type JudgeBackend,
  type JudgeBackendRequest,
  type JudgeCapabilities,
  type JudgeQuestion,
} from './types.js';

export type MockAnswerSource =
  | JudgeAnswer[]
  | ((request: JudgeBackendRequest) => JudgeAnswer[] | Promise<JudgeAnswer[]>);

export interface MockJudgeBackendOptions {
  id?: string;
  answers?: MockAnswerSource;
  capabilities?: Partial<JudgeCapabilities>;
  /** Default probability for verify / probability questions. */
  probability?: number;
  /** Which option a choose question selects. */
  choice?: 'first' | 'last';
  /** Simulated latency. */
  delayMs?: number;
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

function deriveAnswer(
  question: JudgeQuestion,
  probability: number,
  choice: 'first' | 'last',
  basis: JudgeAnswer['basis'],
): JudgeAnswer {
  if (question.kind === 'choose') {
    const optionIds = (question.options ?? []).map((option) => option.id);
    const index = choice === 'last' ? Math.max(optionIds.length - 1, 0) : 0;
    return {
      id: question.id,
      kind: question.kind,
      value: optionIds[index] ?? 'option',
      confidence: 1,
      distribution: optionIds.map((_, position) => (position === index ? 1 : 0)),
      basis,
    };
  }
  if (question.kind === 'rate') {
    const levels = question.levels ?? [];
    return {
      id: question.id,
      kind: question.kind,
      value: levelLabel(question, 0),
      confidence: 1,
      distribution: levels.map((_, position) => (position === 0 ? 1 : 0)),
      basis,
    };
  }
  if (question.kind === 'probability') {
    return { id: question.id, kind: question.kind, probability, basis };
  }
  return {
    id: question.id,
    kind: question.kind,
    value: probability >= 0.5 ? 'yes' : 'no',
    probability,
    basis,
  };
}

export function createMockJudgeBackend(options: MockJudgeBackendOptions = {}): JudgeBackend {
  const capabilities = withJudgeCapabilities(options.capabilities);
  const probability = options.probability ?? 0.5;
  const choice = options.choice ?? 'first';
  const basis: JudgeAnswer['basis'] = capabilities.calibrated ? 'calibrated' : 'mock';

  return {
    id: options.id ?? 'mock',
    capabilities,
    async complete(request) {
      if (options.delayMs && options.delayMs > 0) {
        await new Promise((resolve) => setTimeout(resolve, options.delayMs));
      }
      if (typeof options.answers === 'function') return { answers: await options.answers(request) };
      if (Array.isArray(options.answers)) return { answers: options.answers };
      return {
        answers: request.questions.map((question) => deriveAnswer(question, probability, choice, basis)),
        model: 'mock',
      };
    },
  };
}

/**
 * A mock that behaves like a calibrated Jev backend. Register it under `jev`
 * to exercise the Jev code path without network access or an API key.
 */
export function createMockJevJudgeBackend(options: MockJudgeBackendOptions = {}): JudgeBackend {
  return createMockJudgeBackend({
    ...options,
    id: options.id ?? 'jev',
    capabilities: {
      calibrated: true,
      probability: true,
      confidence: true,
      independentQuestions: true,
      batch: true,
      structuredCriteria: true,
      ...(options.capabilities ?? {}),
    },
  });
}
