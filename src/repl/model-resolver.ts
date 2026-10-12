/**
 * Resolve a `with_model/2` spec (string or dict) into a concrete model profile.
 *
 * Two sources, matching the chat backend:
 *   - `PiModelResolver`  — pi's live registry; authoritative for existence and
 *                          auth (the default backend).
 *   - `EnvModelResolver` — the bundled model database (the `env` backend).
 *
 * Resolution happens on first use of a scope and is cached by the runner.
 */

import type { ModelSpec, ResolvedModel } from '../types.js';
import { getAllModels, lookupModel, lookupProvider } from '../system/config/model-database.js';
import { supportedThinkingLevels, type PiModelLike, type PiRegistryLike } from './backend.js';

export class ModelResolutionError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'ModelResolutionError';
  }
}

/**
 * Parse a raw `model_spec` value (from the Prolog payload) into a `ModelSpec`.
 * Accepts `none`/`default`/empty (→ default), a string
 * (`provider:model`, `provider/model`, `model`, each with an optional `#level`),
 * or a `model{…}` dict.
 */
export function parseModelSpec(raw: unknown): ModelSpec {
  if (raw === null || raw === undefined) {
    return 'default';
  }
  if (typeof raw === 'object') {
    const obj = raw as Record<string, unknown>;
    const id = typeof obj.id === 'string' ? obj.id : undefined;
    if (!id) {
      return 'default';
    }
    return {
      provider: typeof obj.provider === 'string' ? obj.provider : undefined,
      id,
      temperature: typeof obj.temperature === 'number' ? obj.temperature : undefined,
      maxTokens: typeof obj.maxTokens === 'number' ? obj.maxTokens : undefined,
      reasoning: typeof obj.reasoning === 'string' ? obj.reasoning : undefined,
    };
  }
  const text = String(raw).trim();
  if (!text || text === 'none' || text === 'default') {
    return 'default';
  }
  let body = text;
  let reasoning: string | undefined;
  // Reasoning suffix uses the last `#` (provider/model ids may contain `:`/`/`).
  const hash = body.lastIndexOf('#');
  if (hash > 0) {
    reasoning = body.slice(hash + 1).trim() || undefined;
    body = body.slice(0, hash);
  }
  const sep = body.search(/[:/]/);
  if (sep === -1) {
    return { id: body, reasoning };
  }
  return { provider: body.slice(0, sep), id: body.slice(sep + 1), reasoning };
}

export interface ModelResolver {
  /** Resolve + validate a non-default spec. Throws `ModelResolutionError`. */
  resolve(spec: ModelSpec): Promise<ResolvedModel>;
  /** A short list of available models, for error messages. */
  describe(): string;
  /** The models available in this session, with catalog prices. */
  catalog(): ModelCatalogEntry[];
}

/** One entry of the session's model catalog, as shown to the agent. */
export interface ModelCatalogEntry {
  /** `provider:model` id, usable in `with_model/2`. */
  model: string;
  /** USD per 1M input tokens. */
  inputCost?: number;
  /** USD per 1M output tokens. */
  outputCost?: number;
  contextWindow?: number;
}

function trimCost(value: number): string {
  return String(Number(value.toFixed(3)));
}

/** Format a catalog entry as `provider:model ($in/$out per 1M)`. */
export function formatModelCatalogEntry(entry: ModelCatalogEntry): string {
  const hasCost = typeof entry.inputCost === 'number' || typeof entry.outputCost === 'number';
  if (!hasCost) {
    return entry.model;
  }
  const input = typeof entry.inputCost === 'number' ? `$${trimCost(entry.inputCost)}` : '?';
  const output = typeof entry.outputCost === 'number' ? `$${trimCost(entry.outputCost)}` : '?';
  return `${entry.model} (${input}/${output} per 1M)`;
}

/** Resolve against pi's live registry (existence + auth are authoritative). */
export class PiModelResolver implements ModelResolver {
  constructor(private readonly registry: PiRegistryLike) {}

  async resolve(spec: ModelSpec): Promise<ResolvedModel> {
    if (spec === 'default') {
      throw new ModelResolutionError('internal: default spec resolved by the runner');
    }
    const available = this.registry.getAvailable();
    const matches = available.filter(
      (model) => model.id === spec.id && (spec.provider ? model.provider === spec.provider : true),
    );
    const label = spec.provider ? `${spec.provider}:${spec.id}` : spec.id;
    if (matches.length === 0) {
      throw new ModelResolutionError(
        `unknown model "${label}" — available: ${this.describe() || '(none)'}`,
      );
    }
    if (matches.length > 1) {
      throw new ModelResolutionError(
        `"${spec.id}" is ambiguous (${matches.map((m) => m.provider).join(', ')}) — use "provider:model"`,
      );
    }
    const model = matches[0];
    if (!this.registry.hasConfiguredAuth(model)) {
      throw new ModelResolutionError(
        `provider "${model.provider}" is not signed in — run \`deepclause login\``,
      );
    }
    return {
      provider: model.provider,
      id: model.id,
      temperature: spec.temperature,
      maxTokens: spec.maxTokens ?? model.maxTokens,
      contextWindow: model.contextWindow,
      reasoning: Boolean(model.reasoning),
      reasoningLevel: spec.reasoning,
      supportedThinkingLevels: supportedThinkingLevels(model as PiModelLike),
    };
  }

  describe(): string {
    return this.registry
      .getAvailable()
      .map((model) => `${model.provider}:${model.id}`)
      .slice(0, 12)
      .join(', ');
  }

  catalog(): ModelCatalogEntry[] {
    return this.registry.getAvailable().map((model) => ({
      model: `${model.provider}:${model.id}`,
      inputCost: model.cost?.input,
      outputCost: model.cost?.output,
      contextWindow: model.contextWindow,
    }));
  }
}

/** Resolve against the bundled model database (the `env` backend). */
export class EnvModelResolver implements ModelResolver {
  constructor(private readonly sessionProvider: string) {}

  async resolve(spec: ModelSpec): Promise<ResolvedModel> {
    if (spec === 'default') {
      throw new ModelResolutionError('internal: default spec resolved by the runner');
    }
    const provider = spec.provider ?? this.sessionProvider;
    const entry = lookupModel(spec.id);
    const providerEntry = lookupProvider(provider);
    return {
      provider,
      id: spec.id,
      temperature: spec.temperature,
      maxTokens: spec.maxTokens ?? entry?.limit.output,
      contextWindow: entry?.limit.context,
      reasoning: entry?.reasoning ?? false,
      reasoningLevel: spec.reasoning,
      reasoningType: providerEntry?.reasoning_type ?? 'none',
      reasoningBudgetMap: providerEntry?.budget_map,
    };
  }

  describe(): string {
    return Object.keys(getAllModels()).slice(0, 12).join(', ');
  }

  catalog(): ModelCatalogEntry[] {
    return Object.keys(getAllModels()).map((id) => ({ model: id }));
  }
}
