/**
 * Marshalling between the Prolog wire shapes and the SDK judge types.
 *
 * The engine payload arrives as swipl-wasm values:
 *   - a compound `f(a, b)`  -> `{ $t: 't', f: [[a, b]] }`
 *   - a pair `K-V`          -> `{ $t: 't', '-': [[K, V]] }`
 *   - a dict `tag{k: v}`    -> `{ $tag: 'tag', k: v }`
 *   - a list                -> an array
 *   - atoms / strings / numbers / booleans arrive as plain JS values.
 */

import type { JsonValue, JudgeAnswer, JudgeLevel, JudgeOption, JudgeQuestion } from './types.js';

type RawObject = Record<string, unknown>;

function isRawObject(value: unknown): value is RawObject {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isCompound(value: unknown): value is RawObject {
  return isRawObject(value) && value.$t === 't';
}

function compoundParts(value: RawObject): { functor: string; args: unknown[] } {
  for (const key of Object.keys(value)) {
    if (key === '$t') continue;
    const payload = value[key];
    const args =
      Array.isArray(payload) && Array.isArray(payload[0]) ? (payload[0] as unknown[]) : [];
    return { functor: key, args };
  }
  return { functor: '', args: [] };
}

function isPairCompound(value: unknown): boolean {
  return isCompound(value) && compoundParts(value as RawObject).functor === '-';
}

/** Normalize an arbitrary Prolog value into JSON. A list of `Key-Value` pairs becomes an object. */
export function normalizePrologValue(value: unknown): JsonValue {
  if (value === null || value === undefined) return null;
  if (typeof value === 'string') {
    if (value === 'null') return null;
    if (value === 'true') return true;
    if (value === 'false') return false;
    return value;
  }
  if (typeof value === 'number' || typeof value === 'boolean') return value;
  if (Array.isArray(value)) {
    if (value.length > 0 && value.every(isPairCompound)) {
      const object: Record<string, JsonValue> = {};
      for (const entry of value) {
        const { args } = compoundParts(entry as RawObject);
        object[String(normalizePrologValue(args[0]))] = normalizePrologValue(args[1]);
      }
      return object;
    }
    return value.map(normalizePrologValue);
  }
  if (isCompound(value)) {
    const { functor, args } = compoundParts(value as RawObject);
    if (functor === 'arr') return normalizePrologValue(args[0] ?? []);
    if (functor === '-') {
      return { [String(normalizePrologValue(args[0]))]: normalizePrologValue(args[1]) };
    }
    return { [functor]: args.map(normalizePrologValue) } as JsonValue;
  }
  if (isRawObject(value)) {
    const object: Record<string, JsonValue> = {};
    for (const [key, entry] of Object.entries(value)) {
      if (key === '$tag' || key === '$t') continue;
      object[key] = normalizePrologValue(entry);
    }
    return object;
  }
  return null;
}

/** The author-facing state term, normalized to JSON. */
export function normalizeJudgeState(value: unknown): JsonValue {
  return normalizePrologValue(value);
}

function parseOptions(raw: unknown): JudgeOption[] {
  const list = Array.isArray(raw) ? raw : [];
  return list.map((entry) => {
    if (isPairCompound(entry)) {
      const { args } = compoundParts(entry as RawObject);
      return {
        id: String(normalizePrologValue(args[0])),
        description: normalizePrologValue(args[1]),
      };
    }
    return { id: String(normalizePrologValue(entry)) };
  });
}

function parseLevels(raw: unknown): JudgeLevel[] {
  const list = Array.isArray(raw) ? raw : [];
  return list.map((entry) => {
    if (isCompound(entry) && compoundParts(entry as RawObject).functor === 'level') {
      const { args } = compoundParts(entry as RawObject);
      return { what: normalizePrologValue(args[0]), examples: normalizePrologValue(args[1]) };
    }
    return normalizePrologValue(entry);
  });
}

function parseCriteria(raw: unknown): { true?: JsonValue; false?: JsonValue } | undefined {
  if (!isCompound(raw)) return undefined;
  const { functor, args } = compoundParts(raw as RawObject);
  if (functor !== 'criteria') return undefined;
  return { true: normalizePrologValue(args[0]), false: normalizePrologValue(args[1]) };
}

/** Parse the `questions` list yielded by the meta-interpreter into `JudgeQuestion[]`. */
export function parseJudgeQuestions(raw: unknown): JudgeQuestion[] {
  const list = Array.isArray(raw) ? raw : [];
  return list.map((entry, index) => {
    const dict: RawObject = isRawObject(entry) ? entry : {};
    const kind = String(normalizePrologValue(dict.kind));
    const question: JudgeQuestion = {
      id: `q${index + 1}`,
      kind: kind as JudgeQuestion['kind'],
      instruction: normalizePrologValue(dict.instruction),
    };
    if (kind === 'choose') question.options = parseOptions(dict.options);
    else if (kind === 'rate') question.levels = parseLevels(dict.levels);
    else if (kind === 'verify') question.criteria = parseCriteria(dict.criteria);
    return question;
  });
}

function levelId(level: JudgeLevel, index: number): string {
  if (typeof level === 'string') return level;
  if (typeof level === 'number' || typeof level === 'boolean') return String(level);
  if (isRawObject(level) && typeof level.what === 'string') return level.what;
  return `level_${index}`;
}

function validateAnswer(question: JudgeQuestion, answer: JudgeAnswer): JudgeAnswer {
  const basis = answer.basis ?? 'label';
  if (question.kind === 'choose') {
    const optionIds = (question.options ?? []).map((option) => option.id);
    const value = answer.value;
    if (!value || !optionIds.includes(value)) {
      throw new Error(
        `judge answer for '${question.id}' selected '${value ?? 'nothing'}', which is not one of: ${optionIds.join(', ')}`,
      );
    }
    return { ...answer, basis, distribution: alignDistribution(optionIds.length, answer.distribution) };
  }
  if (question.kind === 'rate') {
    const levels = question.levels ?? [];
    let value = answer.value;
    if (!value && typeof answer.probability === 'number') {
      value = levelId(levels[argmax(answer.distribution ?? [])] ?? '', 0);
    }
    const labels = levels.map(levelId);
    if (value && !labels.includes(value)) {
      const numeric = Number(value);
      if (Number.isInteger(numeric) && labels[numeric] !== undefined) value = labels[numeric];
      else throw new Error(`judge answer for '${question.id}' returned unknown level '${value}'`);
    }
    if (!value && labels.length > 0) value = labels[0];
    return { ...answer, value, basis, distribution: alignDistribution(levels.length, answer.distribution) };
  }
  if (question.kind === 'verify') {
    if (answer.value !== undefined && !['yes', 'no', 'unknown'].includes(answer.value)) {
      throw new Error(`judge answer for '${question.id}' returned '${answer.value}', expected yes/no/unknown`);
    }
    if (answer.value === undefined) {
      const probability = answer.probability;
      const value = typeof probability === 'number' ? (probability >= 0.5 ? 'yes' : 'no') : 'unknown';
      return { ...answer, value, basis };
    }
    return { ...answer, basis };
  }
  // probability
  if (typeof answer.probability !== 'number') {
    throw new Error(`judge answer for '${question.id}' did not return a probability`);
  }
  return { ...answer, basis };
}

function alignDistribution(length: number, distribution: number[] | undefined): number[] | undefined {
  if (!distribution) return undefined;
  if (distribution.length === length) return distribution;
  if (distribution.length > length) return distribution.slice(0, length);
  return [...distribution, ...Array<number>(length - distribution.length).fill(0)];
}

function argmax(values: number[]): number {
  let best = 0;
  for (let index = 1; index < values.length; index++) {
    if ((values[index] ?? 0) > (values[best] ?? 0)) best = index;
  }
  return best;
}

/** Validate backend answers against the questions and preserve question order. */
export function validateJudgeAnswers(
  questions: JudgeQuestion[],
  answers: JudgeAnswer[],
): JudgeAnswer[] {
  return questions.map((question, index) => {
    const answer = answers[index] ?? answers.find((candidate) => candidate.id === question.id);
    if (!answer) throw new Error(`judge backend returned no answer for question ${question.id}`);
    return validateAnswer(question, answer);
  });
}

/** The ordered primary values bound back into DML. */
export function judgeAnswersToValues(
  questions: JudgeQuestion[],
  answers: JudgeAnswer[],
): Array<string | number> {
  return questions.map((question, index) => {
    const answer = answers[index];
    if (!answer) throw new Error(`missing judge answer for ${question.id}`);
    if (question.kind === 'probability') {
      if (typeof answer.probability !== 'number') {
        throw new Error(`judge answer for '${question.id}' did not return a probability`);
      }
      return answer.probability;
    }
    if (question.kind === 'verify') return answer.value ?? 'unknown';
    return answer.value ?? '';
  });
}

/** Serialize answer values into a Prolog list, using atoms for identifiers. */
export function valuesToPrologList(values: Array<string | number>): string {
  return `[${values.map(prologValue).join(', ')}]`;
}

function prologValue(value: string | number): string {
  if (typeof value === 'number') return String(value);
  if (/^[a-z][a-zA-Z0-9_]*$/.test(value)) return value;
  return JSON.stringify(value);
}

function stableStringify(value: unknown): string {
  if (value === undefined) return 'undefined';
  if (value === null || typeof value !== 'object') return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map((entry) => stableStringify(entry)).join(',')}]`;
  const entries = Object.entries(value as Record<string, unknown>).sort(([a], [b]) =>
    a < b ? -1 : a > b ? 1 : 0,
  );
  return `{${entries.map(([key, entry]) => `${JSON.stringify(key)}:${stableStringify(entry)}`).join(',')}}`;
}

/** Cache key for a judgment: same backend/model/state/questions returns the same answers. */
export function judgeCacheKey(
  backendId: string,
  model: string,
  state: JsonValue,
  questions: JudgeQuestion[],
): string {
  return `${backendId}|${model}|${stableStringify(state)}|${stableStringify(questions as unknown as JsonValue)}`;
}
