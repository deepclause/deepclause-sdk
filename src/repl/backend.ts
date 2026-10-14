/**
 * Backend resolution for the REPL (M5).
 *
 * `env` uses the SDK's provider/API-key backend (unchanged). `pi` reuses pi's
 * own model registry and credential store so the REPL runs against the model
 * configured in pi, without asking for API keys or writing provider env vars.
 *
 * The pi packages are optional peers and loaded with a dynamic import only when
 * the pi backend is selected.
 */

import * as path from 'path';

import type {
  LLMBackend,
  LLMBackendMessage,
  LLMBackendRequest,
  LLMBackendResponse,
  ResolvedModel,
} from '../types.js';

export type BackendKind = 'env' | 'pi';

export interface PiBackendConfig {
  kind?: BackendKind;
  agentDir?: string;
  modelsPath?: string | null;
  authPath?: string | null;
  model?: string;
  /** Requested thinking level for reasoning models (see `ThinkingLevel`). */
  thinkingLevel?: string;
}

export interface ResolvedBackendPaths {
  agentDir: string;
  modelsPath: string;
  authPath: string;
  model?: string;
}

/** Minimal structural view of pi's ModelRegistry used by the adapter. */
export interface PiModelLike {
  provider: string;
  id: string;
  api?: string;
  /** Whether the model accepts a thinking/reasoning level. */
  reasoning?: boolean;
  /** Model compatibility flags (e.g. `supportsTemperature`). */
  compat?: { supportsTemperature?: boolean; [key: string]: unknown };
  /** Catalog price, USD per 1M tokens. */
  cost?: { input?: number; output?: number; cacheRead?: number; cacheWrite?: number };
  /** Model context window in tokens. */
  contextWindow?: number;
  /** Maximum output tokens. */
  maxTokens?: number;
  /** Maps pi thinking levels to provider values; `null` marks a level unsupported. */
  thinkingLevelMap?: Partial<Record<ThinkingLevel, string | null>>;
}

/** pi's thinking levels, in ascending effort order. */
export type ThinkingLevel = 'off' | 'minimal' | 'low' | 'medium' | 'high' | 'xhigh' | 'max';

export const THINKING_LEVELS: readonly ThinkingLevel[] = ['off', 'minimal', 'low', 'medium', 'high', 'xhigh', 'max'];

/** Levels a model accepts: reasoning models expose all but explicitly-unsupported ones. */
export function supportedThinkingLevels(model: PiModelLike): ThinkingLevel[] {
  if (!model.reasoning) {
    return ['off'];
  }
  return THINKING_LEVELS.filter((level) => model.thinkingLevelMap?.[level] !== null);
}

/**
 * Validate a requested thinking level against the model. Returns the level to
 * pass through (or `undefined` for the provider default) plus a warning when a
 * request had to be dropped.
 */
export function resolveThinkingLevel(
  model: PiModelLike,
  requested?: string,
): { level?: ThinkingLevel; supported: ThinkingLevel[]; warning?: string } {
  const supported = supportedThinkingLevels(model);
  if (!requested) {
    return { level: undefined, supported };
  }
  if ((THINKING_LEVELS as readonly string[]).includes(requested) && supported.includes(requested as ThinkingLevel)) {
    return { level: requested as ThinkingLevel, supported };
  }
  return {
    level: undefined,
    supported,
    warning: `thinking level '${requested}' is not supported by ${model.provider}/${model.id}; using the provider default`,
  };
}

export interface PiCompletionRequest {
  systemPrompt?: string;
  messages: unknown[];
  tools?: unknown[];
}

export interface PiCompletionResult {
  content: Array<
    | { type: 'text'; text: string }
    | { type: 'toolCall'; id: string; name: string; arguments: Record<string, unknown> }
  >;
  usage: {
    input: number;
    output: number;
    totalTokens: number;
    cacheRead?: number;
    cacheWrite?: number;
    reasoning?: number;
  };
  stopReason: 'stop' | 'toolUse' | 'length' | 'error' | 'aborted';
  errorMessage?: string;
}

export interface PiStreamEventLike {
  type: string;
  delta?: string;
}

export interface PiStreamLike extends AsyncIterable<PiStreamEventLike> {
  result(): Promise<PiCompletionResult>;
}

export interface PiRegistryLike {
  getAvailable(): PiModelLike[];
  find?(provider: string, modelId: string): PiModelLike | undefined;
  hasConfiguredAuth(model: PiModelLike): boolean;
  complete(model: PiModelLike, context: PiCompletionRequest, options: unknown): Promise<PiCompletionResult>;
  /** Streaming API; when present, `complete` forwards text/thinking deltas. */
  streamSimple?(model: PiModelLike, context: PiCompletionRequest, options: unknown): PiStreamLike;
}

/** Resolve pi's agent dir, models.json, and auth.json paths. */
export function resolvePiPaths(
  config: PiBackendConfig,
  defaults: { agentDir: string },
): ResolvedBackendPaths {
  const agentDir = config.agentDir ?? defaults.agentDir;
  return {
    agentDir,
    modelsPath: config.modelsPath ?? path.join(agentDir, 'models.json'),
    authPath: config.authPath ?? path.join(agentDir, 'auth.json'),
    model: config.model,
  };
}

function modelLabel(model: PiModelLike): string {
  return `${model.provider}/${model.id}`;
}

/** Find a pi model by provider+id (registry `find` when present, else the catalog). */
function findPiModel(registry: PiRegistryLike, target: ResolvedModel): PiModelLike | undefined {
  const found = registry.find?.(target.provider, target.id);
  if (found) {
    return found;
  }
  return registry.getAvailable().find((model) => model.provider === target.provider && model.id === target.id);
}

/** Select the pi model to use, honoring an explicit provider/model request. */
export function selectPiModel(registry: PiRegistryLike, requested?: string): PiModelLike {
  const available = registry.getAvailable();
  if (available.length === 0) {
    throw new Error('pi has no configured models; run pi or `deepclause set-backend env`');
  }

  if (requested) {
    const normalized = requested.replace(':', '/');
    const [provider, ...rest] = normalized.split('/');
    const modelId = rest.join('/');
    if (modelId && registry.find) {
      const found = registry.find(provider, modelId);
      if (found) return found;
    }
    const byId = available.find((model) => model.id === requested || modelLabel(model) === requested);
    if (byId) return byId;
    throw new Error(`pi model not found: ${requested}`);
  }

  return available[0];
}

function emptyUsage(): PiCompletionResult['usage'] {
  return { input: 0, output: 0, totalTokens: 0, cacheRead: 0, cacheWrite: 0, reasoning: 0 };
}

function toPiMessages(messages: LLMBackendMessage[], model: PiModelLike): unknown[] {
  return messages
    .filter((message) => message.role !== 'system')
    .map((message) => {
      if (message.role === 'assistant' && message.providerData) {
        return message.providerData;
      }
      if (message.role === 'user') {
        // Attach resolved media as pi image content blocks alongside the text.
        const content = message.media && message.media.length > 0
          ? [
              ...(message.content ? [{ type: 'text', text: message.content }] : []),
              ...message.media.map((media) => ({
                type: 'image',
                data: media.data,
                mimeType: media.mimeType,
              })),
            ]
          : message.content;
        return { role: 'user', content, timestamp: Date.now() };
      }
      if (message.role === 'tool') {
        return {
          role: 'toolResult',
          toolCallId: message.toolCallId ?? 'unknown',
          toolName: message.toolName ?? 'unknown',
          content: [{ type: 'text', text: message.content }],
          isError: false,
          timestamp: Date.now(),
        };
      }
      return {
        role: 'assistant',
        content: [
          ...(message.content ? [{ type: 'text', text: message.content }] : []),
          ...(message.toolCalls ?? []).map((call) => ({
            type: 'toolCall',
            id: call.id,
            name: call.name,
            arguments: call.arguments,
          })),
        ],
        api: model.api,
        provider: model.provider,
        model: model.id,
        usage: emptyUsage(),
        stopReason: message.toolCalls?.length ? 'toolUse' : 'stop',
        timestamp: Date.now(),
      };
    });
}

/**
 * Wrap a pi registry + model as an SDK `LLMBackend`. Reused by the extension
 * embedding, which passes its live registry.
 *
 * `thinkingLevel` is forwarded to the provider only for reasoning models and
 * non-`off` levels, so provider defaults are preserved for every other model.
 */
export function createPiAdapter(
  registry: PiRegistryLike,
  model: PiModelLike,
  maxTokens = 65536,
  thinkingLevel?: string,
): {
  backend: LLMBackend;
  registry: PiRegistryLike;
  modelId: string;
  provider: string;
  contextWindow?: number;
  maxTokens: number;
  reasoning: boolean;
  thinkingLevel?: ThinkingLevel;
  supportedThinkingLevels: ThinkingLevel[];
  warning?: string;
} {
  if (!registry.hasConfiguredAuth(model)) {
    throw new Error(`pi has no configured authentication for ${modelLabel(model)}`);
  }

  // Resolved per call rather than once: the reasoning level is a setting the user can
  // change while the session runs, and a captured value would ignore it.
  let currentThinkingLevel = thinkingLevel;
  const baseResolved = (): ReturnType<typeof resolveThinkingLevel> =>
    resolveThinkingLevel(model, currentThinkingLevel);

  // Per-call model overrides from `with_model/2`, resolved lazily and cached.
  const scoped = new Map<string, { model: PiModelLike; resolved: ReturnType<typeof resolveThinkingLevel> }>();
  const resolveScoped = (target: ResolvedModel): { model: PiModelLike; resolved: ReturnType<typeof resolveThinkingLevel> } => {
    const key = `${target.provider}/${target.id}`;
    const cached = scoped.get(key);
    if (cached) {
      return cached;
    }
    const found = findPiModel(registry, target);
    if (!found) {
      throw new Error(`pi model not found: ${key}`);
    }
    if (!registry.hasConfiguredAuth(found)) {
      throw new Error(`pi has no configured authentication for ${key}`);
    }
    const entry = { model: found, resolved: resolveThinkingLevel(found, target.reasoningLevel) };
    scoped.set(key, entry);
    return entry;
  };

  const completeWith = async (
    activeModel: PiModelLike,
    activeResolved: ReturnType<typeof resolveThinkingLevel>,
    defaultMaxTokens: number,
    request: LLMBackendRequest,
  ): Promise<LLMBackendResponse> => {
      const systemPrompt = request.messages
        .filter((message) => message.role === 'system')
        .map((message) => message.content)
        .join('\n\n');

      const context: PiCompletionRequest = {
        systemPrompt: systemPrompt || undefined,
        messages: toPiMessages(request.messages, activeModel),
        tools: request.tools?.map((tool) => ({
          name: tool.name,
          description: tool.description,
          parameters: tool.parameters,
        })),
      };
      const options: Record<string, unknown> = {
        signal: request.signal,
        maxTokens: request.maxTokens ?? defaultMaxTokens,
        cacheRetention: 'none',
      };
      // Model defaults the caller's temperature only when a scope explicitly set
      // one (pi itself never forwards an ambient temperature, and some models —
      // e.g. github-copilot — reject the parameter). `compat.supportsTemperature`
      // opts a model out when pi knows it rejects it.
      const scopedTemperature = request.target?.temperature;
      if (
        typeof scopedTemperature === 'number'
        && activeModel.compat?.supportsTemperature !== false
      ) {
        options.temperature = scopedTemperature;
      }
      if (activeModel.reasoning && activeResolved.level && activeResolved.level !== 'off') {
        // pi's `SimpleStreamOptions` enables thinking with `reasoning` (a
        // ThinkingLevel), not `thinkingLevel`.
        options.reasoning = activeResolved.level;
      }

      let response: PiCompletionResult;
      if (registry.streamSimple) {
        // Stream text/thinking deltas as they arrive; the final message carries
        // the full text, tool calls, and usage.
        const stream = registry.streamSimple(activeModel, context, options);
        for await (const event of stream) {
          if (event.type === 'text_delta') {
            if (event.delta) request.onText?.(event.delta);
          } else if (event.type === 'thinking_delta') {
            if (event.delta) request.onReasoning?.(event.delta);
          }
        }
        response = await stream.result();
      } else {
        response = await registry.complete(activeModel, context, options);
        const streamedText = response.content
          .filter((content): content is { type: 'text'; text: string } => content.type === 'text')
          .map((content) => content.text)
          .join('');
        if (streamedText) request.onText?.(streamedText);
      }

      if (response.stopReason === 'error' || response.stopReason === 'aborted') {
        throw new Error(response.errorMessage || `pi model request ${response.stopReason}`);
      }

      const text = response.content
        .filter((content): content is { type: 'text'; text: string } => content.type === 'text')
        .map((content) => content.text)
        .join('');

      return {
        text,
        toolCalls: response.content
          .filter((content): content is { type: 'toolCall'; id: string; name: string; arguments: Record<string, unknown> } => content.type === 'toolCall')
          .map((call) => ({ id: call.id, name: call.name, arguments: call.arguments })),
        usage: {
          inputTokens: response.usage.input,
          outputTokens: response.usage.output,
          totalTokens: response.usage.totalTokens,
          cacheReadTokens: response.usage.cacheRead || undefined,
          cacheWriteTokens: response.usage.cacheWrite || undefined,
          reasoningTokens: response.usage.reasoning || undefined,
        },
        finishReason: response.stopReason === 'toolUse'
          ? 'tool_use'
          : response.stopReason === 'length'
            ? 'length'
            : 'stop',
        providerData: response,
      };
  };

  const backend: LLMBackend = {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      if (request.target && (request.target.provider !== model.provider || request.target.id !== model.id)) {
        const entry = resolveScoped(request.target);
        return completeWith(entry.model, entry.resolved, request.target.maxTokens ?? maxTokens, request);
      }
      return completeWith(model, baseResolved(), maxTokens, request);
    },
    /** Change the reasoning level for subsequent calls. */
    setThinkingLevel(level: string): void {
      currentThinkingLevel = level;
    },
    getThinkingLevel(): string | undefined {
      return currentThinkingLevel;
    },
    supportedThinkingLevels(): ThinkingLevel[] {
      return baseResolved().supported;
    },
  };

  return {
    backend,
    registry,
    modelId: modelLabel(model),
    provider: model.provider,
    contextWindow: model.contextWindow,
    maxTokens,
    reasoning: Boolean(model.reasoning),
    thinkingLevel: baseResolved().level,
    supportedThinkingLevels: baseResolved().supported,
    warning: baseResolved().warning,
  };
}

interface PiModule {
  getAgentDir(): string;
  ModelRuntime: {
    create(options: { authPath?: string; modelsPath?: string; allowModelNetwork?: boolean }): Promise<unknown>;
  };
  ModelRegistry: new (runtime: unknown) => PiRegistryLike;
}

const PI_PACKAGE_NAME = '@earendil-works/pi-coding-agent';

/**
 * Import the pi coding agent. It is a regular dependency of this SDK, so the
 * bare specifier always resolves; a globally installed `pi` is deliberately
 * not consulted, which keeps the version deterministic.
 */
async function importPiModule(): Promise<PiModule> {
  try {
    return (await import(PI_PACKAGE_NAME)) as unknown as PiModule;
  } catch {
    throw new Error(
      `Backend 'pi' requires ${PI_PACKAGE_NAME} (a dependency of this SDK). `
      + 'Reinstall dependencies, or run `deepclause set-backend env` to use the SDK provider/API-key backend.',
    );
  }
}

export interface CreatePiBackendOptions extends PiBackendConfig {
  maxTokens?: number;
  /** Injected registry (used by tests and by a pi embedding). */
  modelRegistry?: PiRegistryLike;
}

/** Structural view of pi's runtime used for setup/catalog/auth operations. */
export interface PiRuntimeLike {
  getProviders(): Array<{
    id: string;
    name?: string;
    auth?: { apiKey?: unknown; oauth?: { isSubscription?: boolean } };
  }>;
  getProvider(providerId: string): { id: string; name?: string; auth?: { apiKey?: unknown; oauth?: { isSubscription?: boolean } } } | undefined;
  getModels(providerId?: string): PiModelLike[];
  hasConfiguredAuth(providerId: string): boolean;
  getProviderAuthStatus(providerId: string): { configured: boolean; label?: string; source?: string };
  isUsingOAuth(providerId: string): boolean;
  listCredentials(options?: unknown): Promise<Array<{ providerId: string; type: string }>>;
  login(providerId: string, type: string, interaction: unknown, options?: unknown): Promise<unknown>;
  logout(providerId: string, options?: unknown): Promise<void>;
  /** Image models whose provider has complete auth (ModelRuntime). */
  getAvailableOfType?(type: string, providerId?: string): Promise<Array<unknown>>;
  /** Runtime lookup of one image model. */
  getModelOfType?(type: string, providerId: string, modelId: string): unknown;
  /** Generate images with a resolved pi image model. */
  generateImages?(model: unknown, context: unknown, options?: unknown): Promise<{
    output?: Array<{ type: string; data?: string; mimeType?: string; text?: string }>;
    errorMessage?: string;
  }>;
}

export interface PiProviderInfo {
  id: string;
  name: string;
  apiKey: boolean;
  oauth: boolean;
  configured: boolean;
  source?: string;
  label?: string;
}

export interface PiModelInfo {
  provider: string;
  id: string;
  name: string;
  reasoning: boolean;
  contextWindow?: number;
  maxTokens?: number;
  /** Thinking levels the model accepts (empty/undefined for non-reasoning). */
  supportedThinkingLevels?: ThinkingLevel[];
}

/** Build the catalog `PiModelInfo` (with reasoning levels) from a raw model. */
export function toPiModelInfo(model: PiModelLike & { name?: string }): PiModelInfo {
  return {
    provider: model.provider,
    id: model.id,
    name: model.name ?? model.id,
    reasoning: Boolean(model.reasoning),
    contextWindow: model.contextWindow,
    maxTokens: model.maxTokens,
    supportedThinkingLevels: supportedThinkingLevels(model),
  };
}

export interface PiAuthPromptLike {
  signal?: AbortSignal;
  type: 'text' | 'secret' | 'select' | 'manual_code';
  message: string;
  placeholder?: string;
  options?: ReadonlyArray<{ id: string; label: string; description?: string }>;
}

export interface PiAuthEventLike {
  type: 'info' | 'auth_url' | 'device_code' | 'progress';
  message?: string;
  url?: string;
  instructions?: string;
  userCode?: string;
  verificationUri?: string;
  links?: ReadonlyArray<{ url: string; label?: string }>;
}

export interface PiAuthInteractionLike {
  signal?: AbortSignal;
  prompt(prompt: PiAuthPromptLike): Promise<string>;
  notify(event: PiAuthEventLike): void;
}

/** List providers from a pi runtime with auth capability/status. */
export function listPiProviders(runtime: PiRuntimeLike): PiProviderInfo[] {
  return runtime.getProviders().map((provider) => {
    const status = runtime.getProviderAuthStatus(provider.id);
    return {
      id: provider.id,
      name: provider.name ?? provider.id,
      apiKey: Boolean(provider.auth?.apiKey),
      oauth: Boolean(provider.auth?.oauth),
      configured: status?.configured ?? runtime.hasConfiguredAuth(provider.id),
      source: status?.source,
      label: status?.label,
    };
  });
}

/** List chat models from a pi runtime, optionally filtered by provider. */
export function listPiModels(runtime: PiRuntimeLike, providerId?: string): PiModelInfo[] {
  return runtime.getModels(providerId).map((model) => toPiModelInfo(model as PiModelLike & { name?: string }));
}

/** A pi runtime handle: the model registry plus catalog/auth operations. */
export interface PiRuntimeHandle {
  registry: PiRegistryLike;
  runtime: PiRuntimeLike;
  listProviders(): PiProviderInfo[];
  listModels(providerId?: string): PiModelInfo[];
  hasAuth(providerId: string): boolean;
  isUsingOAuth(providerId: string): boolean;
  listCredentials(): Promise<Array<{ providerId: string; type: string }>>;
  login(providerId: string, interaction: PiAuthInteractionLike, type?: 'api_key' | 'oauth'): Promise<void>;
  logout(providerId: string): Promise<void>;
}

/** Whether the pi dependency resolves (it is a required dependency of the SDK). */
export function isPiPackageAvailable(): boolean {
  try {
    const resolve = (import.meta as unknown as { resolve?: (specifier: string) => string }).resolve;
    return typeof resolve === 'function' && Boolean(resolve(PI_PACKAGE_NAME));
  } catch {
    return false;
  }
}

/**
 * Open a pi runtime using an existing install's credential store and model
 * catalog. This is the setup entry point used by `set-backend pi`; it does not
 * require the pi CLI or a pre-existing pi session.
 */
export async function openPiRuntime(
  options: CreatePiBackendOptions = {},
): Promise<PiRuntimeHandle> {
  const pi = await importPiModule();
  const paths = resolvePiPaths(options, { agentDir: pi.getAgentDir() });
  const runtime = (await pi.ModelRuntime.create({
    authPath: paths.authPath,
    modelsPath: paths.modelsPath,
    allowModelNetwork: false,
  })) as unknown as PiRuntimeLike;
  const registry = options.modelRegistry ?? new pi.ModelRegistry(runtime);

  return {
    registry,
    runtime,
    listProviders: () => listPiProviders(runtime),
    listModels: (providerId) => listPiModels(runtime, providerId),
    hasAuth: (providerId) => runtime.hasConfiguredAuth(providerId),
    isUsingOAuth: (providerId) => runtime.isUsingOAuth(providerId),
    listCredentials: async () => (await runtime.listCredentials()) ?? [],
    login: async (providerId, interaction, type) => {
      const authType = type ?? 'api_key';
      await runtime.login(providerId, authType, interaction, {});
    },
    logout: async (providerId) => {
      await runtime.logout(providerId);
    },
  };
}

/** Resolved pi backend plus the model capabilities the UI/config needs. */
export interface PiBackendInfo {
  backend: LLMBackend;
  /** The pi registry, for resolving `with_model/2` scopes. */
  registry: PiRegistryLike;
  modelId: string;
  provider: string;
  contextWindow?: number;
  maxTokens: number;
  reasoning: boolean;
  thinkingLevel?: ThinkingLevel;
  supportedThinkingLevels: ThinkingLevel[];
  /** Set when a requested thinking level was unsupported and dropped. */
  warning?: string;
}

/**
 * Build an `LLMBackend` from pi's credential store. Throws a clear error when
 * the pi packages are not installed.
 */
export async function createPiBackend(options: CreatePiBackendOptions = {}): Promise<PiBackendInfo> {
  let registry = options.modelRegistry;
  let requested = options.model;

  if (!registry) {
    const pi = await importPiModule();
    const paths = resolvePiPaths(options, { agentDir: pi.getAgentDir() });
    requested = requested ?? paths.model;
    const runtime = await pi.ModelRuntime.create({
      authPath: paths.authPath,
      modelsPath: paths.modelsPath,
      allowModelNetwork: false,
    });
    registry = new pi.ModelRegistry(runtime);
  }

  const model = selectPiModel(registry, requested);
  return createPiAdapter(registry, model, options.maxTokens, options.thinkingLevel);
}

export interface ResolveBackendOptions {
  kind?: BackendKind;
  model?: string;
  provider?: string;
  maxTokens?: number;
  agentDir?: string;
  modelsPath?: string | null;
  authPath?: string | null;
  thinkingLevel?: string;
}

export interface ResolvedBackend {
  kind: BackendKind;
  /** Injected backend for pi; undefined for env (the runner builds it). */
  llmBackend?: LLMBackend;
  provider: string;
  /** Bare model id. */
  model: string;
  /** `provider/model` (pi) or bare id (env), for display/persistence. */
  modelId: string;
  /** pi model capabilities (undefined for env). */
  contextWindow?: number;
  reasoning?: boolean;
  thinkingLevel?: ThinkingLevel;
  /** pi registry, for resolving `with_model/2` (pi backend only). */
  registry?: PiRegistryLike;
}

/**
 * Resolve the shared pi/env model backend used by both `deepclause run` and the
 * REPL. `pi` (the default) returns an injected `LLMBackend`; `env` returns the
 * provider/model so the runner builds the SDK provider backend.
 */
export async function resolveLlmBackend(options: ResolveBackendOptions = {}): Promise<ResolvedBackend> {
  const kind = options.kind ?? 'pi';
  if (kind === 'pi') {
    const pi = await createPiBackend({
      agentDir: options.agentDir,
      modelsPath: options.modelsPath ?? undefined,
      authPath: options.authPath ?? undefined,
      model: options.model,
      maxTokens: options.maxTokens,
      thinkingLevel: options.thinkingLevel,
    });
    return {
      kind: 'pi',
      llmBackend: pi.backend,
      registry: pi.registry,
      provider: pi.provider,
      model: pi.modelId.split('/').slice(1).join('/') || pi.modelId,
      modelId: pi.modelId,
      contextWindow: pi.contextWindow,
      reasoning: pi.reasoning,
      thinkingLevel: pi.thinkingLevel,
    };
  }
  const model = options.model ?? 'gpt-4o';
  return { kind: 'env', provider: options.provider ?? 'openai', model, modelId: model };
}
