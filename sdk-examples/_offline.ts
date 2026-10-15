/**
 * Shared support for the examples.
 *
 * Every example runs with no configuration. If a provider key is in the
 * environment it uses the live model; otherwise it injects a scripted LLM
 * backend, so the example still runs end to end (and deterministically). That is
 * what makes `npm run examples:smoke` meaningful on a machine with no keys.
 */
import { createDeepClause } from '../src/index.js';
import type {
  CreateOptions,
  DeepClauseSDK,
  LLMBackend,
  LLMBackendRequest,
  LLMBackendResponse,
} from '../src/index.js';

const PROVIDERS = [
  { provider: 'openai', env: 'OPENAI_API_KEY', model: 'gpt-4o-mini' },
  { provider: 'anthropic', env: 'ANTHROPIC_API_KEY', model: 'claude-3-5-haiku-latest' },
  { provider: 'google', env: 'GOOGLE_GENERATIVE_AI_API_KEY', model: 'gemini-2.0-flash' },
] as const;

type Provider = (typeof PROVIDERS)[number];

/** The first provider with a key in the environment, or undefined for offline mode. */
export function liveProvider(): Provider | undefined {
  return PROVIDERS.find((entry) => Boolean(process.env[entry.env]));
}

/** A one-line description of what an example will actually run against. */
export function modelLabel(): string {
  const live = liveProvider();
  return live ? `${live.provider}:${live.model}` : 'offline (scripted model)';
}

/** The output variable a `task/N` asks for, if the agent prompt names one. */
export function askedVariable(request: LLMBackendRequest): string | undefined {
  const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
  return /Variable must be one of: "([^"]+)"/.exec(system)?.[1];
}

/** The values of the tool results already returned to the model. */
export function toolResults(request: LLMBackendRequest): string[] {
  return request.messages.filter((m) => m.role === 'tool').map((m) => m.content);
}

/** Wrap a pure responder as an `LLMBackend`. */
export function backendFrom(respond: (request: LLMBackendRequest) => LLMBackendResponse): LLMBackend {
  return { complete: async (request) => respond(request) };
}

/** The `set_result` + `finish` reply that ends a `task/N`. */
export function textResponse(request: LLMBackendRequest, value: unknown): LLMBackendResponse {
  const variable = askedVariable(request) ?? 'Result';
  return {
    text: '',
    toolCalls: [
      { id: 'set', name: 'set_result', arguments: { variable, value } },
      { id: 'finish', name: 'finish', arguments: { success: true } },
    ],
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  };
}

/** A single DML tool call, as a model response. DML tool inputs are `arg1`, `arg2`, … */
export function toolResponse(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return {
    text: '',
    toolCalls: [{ id: name, name, arguments: args }],
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  };
}

/** A scripted backend that answers every `task/N` by calling `reply(request)`. */
export function scriptedBackend(reply: (request: LLMBackendRequest) => string): LLMBackend {
  return backendFrom((request) => textResponse(request, reply(request)));
}

/**
 * `createDeepClause` against the live provider, or against `scripts` when no key
 * is set. `scripts` is either a full backend or a `reply` function.
 */
export async function exampleSdk(
  scripts: LLMBackend | ((request: LLMBackendRequest) => string),
  options: Partial<CreateOptions> = {},
): Promise<DeepClauseSDK> {
  const live = liveProvider();
  if (!live) {
    const backend = typeof scripts === 'function' ? scriptedBackend(scripts) : scripts;
    return createDeepClause({ model: 'offline', llmBackend: backend, ...options });
  }
  return createDeepClause({
    model: live.model,
    provider: live.provider,
    apiKey: process.env[live.env],
    ...options,
  });
}
