/**
 * `deepclause set-backend pi|env|status`
 *
 * Switches the SDK between its own provider/API-key backend (`env`) and pi's
 * configured model registry (`pi`). The pi backend never asks for API keys on
 * its own, never writes provider environment variables, and never guesses a
 * provider — it reuses pi's model catalog and credential store.
 *
 * `set-backend pi` is also the setup flow: it discovers (or installs) the pi
 * package, lists pi's model catalog, lets the user pick a model, and runs pi's
 * own login flow when the provider is not yet authenticated. Neither the pi CLI
 * nor a pre-existing pi session is required.
 */

import type { Command } from 'commander';
import * as readline from 'readline';

import { loadConfig, updateConfig } from './config.js';
import {
  LLAMA_PROVIDER_ID,
  createPiBackend,
  isPiPackageAvailable,
  openPiRuntime,
  toPiModelInfo,
  type LlamaServerOptions,
  type PiModelInfo,
  type PiProviderInfo,
  type PiRuntimeHandle,
} from '../repl/backend.js';
import { createTerminalAuthInteraction, loginToProvider } from '../repl/auth.js';
import { pickFromList } from '../repl/picker.js';

export type BackendKind = 'env' | 'pi';

const PI_PACKAGE = '@earendil-works/pi-coding-agent';

export interface SetBackendOptions extends LlamaServerOptions {
  agentDir?: string;
  models?: string;
  auth?: string;
  model?: string;
  provider?: string;
  /** Thinking level for reasoning models (skips the picker). */
  thinking?: string;
  apiKey?: string;
  install?: boolean;
  list?: boolean;
  json?: boolean;
  login?: boolean;
}

export interface SetBackendResult {
  kind: BackendKind;
  model?: string;
  provider?: string;
  thinkingLevel?: string;
  info: string;
  /** Present when `--list` was used. */
  providers?: PiProviderInfo[];
  models?: PiModelInfo[];
}

function ask(question: string): Promise<string> {
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
  return new Promise((resolve) => {
    rl.question(question, (answer) => {
      rl.close();
      resolve(answer);
    });
  });
}

async function requirePiPackage(): Promise<void> {
  if (isPiPackageAvailable()) return;
  throw new Error(
    `pi backend needs ${PI_PACKAGE} (a dependency of this SDK). `
    + 'Reinstall dependencies, or run `deepclause set-backend env`.',
  );
}

/** Pick a model from pi's catalog (explicit, filtered, or interactive). */
export async function choosePiModel(
  handle: PiRuntimeHandle,
  options: SetBackendOptions,
): Promise<PiModelInfo> {
  const toInfo = (model: { provider: string; id: string; name?: string; reasoning?: boolean; contextWindow?: number; maxTokens?: number; thinkingLevelMap?: Partial<Record<string, string | null>> }): PiModelInfo =>
    toPiModelInfo(model);

  if (options.model) {
    const requested = options.model.replace(':', '/');
    const [provider, ...rest] = requested.split('/');
    const modelId = rest.join('/');
    const found = modelId ? handle.registry.find?.(provider, modelId) : undefined;
    if (found) {
      return toInfo(found as { provider: string; id: string; name?: string; reasoning?: boolean; thinkingLevelMap?: Partial<Record<string, string | null>> });
    }
    const models = handle.listModels(options.provider);
    const match = models.find((model) => `${model.provider}/${model.id}` === requested || model.id === options.model);
    if (match) return match;
    throw new Error(`pi model not found: ${options.model}`);
  }

  const models = handle.listModels(options.provider);
  if (models.length === 0) {
    throw new Error(`no pi models found${options.provider ? ` for provider '${options.provider}'` : ''}`);
  }
  if (models.length === 1) {
    return models[0];
  }
  if (!process.stdin.isTTY) {
    throw new Error(
      `multiple pi models are available (${models.length}); pass --model <provider/model> or --provider <id>`,
    );
  }

  const picked = await pickFromList({
    message: `Choose a pi model (${models.length} available)`,
    items: models.map((model) => ({
      id: `${model.provider}/${model.id}`,
      label: `${model.provider}/${model.id}${model.name && model.name !== model.id ? ` — ${model.name}` : ''}`,
      hint: model.reasoning ? '[reasoning]' : undefined,
    })),
  });
  if (!picked) {
    throw new Error('model selection cancelled');
  }
  const chosen = models.find((model) => `${model.provider}/${model.id}` === picked);
  if (!chosen) {
    throw new Error('invalid model selection');
  }
  return chosen;
}

/** Ensure pi has credentials for the chosen provider (using pi's login flow). */
export async function ensurePiAuth(
  handle: PiRuntimeHandle,
  provider: string,
  options: SetBackendOptions,
): Promise<void> {
  const server: LlamaServerOptions = {
    baseUrl: options.baseUrl,
    host: options.host,
    port: options.port,
  };
  // An explicit llama.cpp endpoint must re-login: the router URL lives in the
  // stored credential, so skipping this would silently keep the old one.
  const explicitServer = options.baseUrl !== undefined
    || options.host !== undefined
    || options.port !== undefined;
  if (handle.hasAuth(provider) && !(provider === LLAMA_PROVIDER_ID && explicitServer)) {
    return;
  }

  if (options.apiKey) {
    await loginToProvider(handle, provider, {
      interaction: createTerminalAuthInteraction(),
      apiKey: options.apiKey,
      ...server,
    });
    return;
  }

  const info = handle.listProviders().find((candidate) => candidate.id === provider);
  const canOAuth = Boolean(info?.oauth);
  const canApiKey = info?.apiKey !== false;

  if (options.login === false || !process.stdin.isTTY) {
    if (explicitServer && handle.hasAuth(provider)) {
      throw new Error(
        `Updating the llama.cpp endpoint requires re-login; `
        + `pass --api-key <key> or run 'deepclause login --provider llama.cpp' in a terminal.`,
      );
    }
    const hint = provider === LLAMA_PROVIDER_ID
      ? `Run 'deepclause login --provider llama.cpp', set LLAMA_BASE_URL, or pass --api-key <key>.`
      : `Run 'pi auth' to sign in, or pass --api-key <key>.`;
    throw new Error(`pi has no credentials for provider '${provider}'. ${hint}`);
  }

  let type: 'api_key' | 'oauth' = canOAuth && !canApiKey ? 'oauth' : 'api_key';
  if (canOAuth && canApiKey) {
    const answer = await ask(
      `\nAuthenticate '${provider}' with:\n  1. API key\n  2. OAuth / subscription\n> `,
    );
    type = answer.trim() === '2' ? 'oauth' : 'api_key';
  }

  console.log(`\nStarting pi login for '${provider}' (${type})…`);
  await loginToProvider(handle, provider, {
    interaction: createTerminalAuthInteraction(),
    type,
    ...server,
  });
}

/**
 * Resolve a thinking level for a reasoning model: explicit (validated) or an
 * interactive picker. Non-reasoning models resolve to `undefined`.
 */
export async function chooseThinkingLevel(
  model: PiModelInfo,
  requested?: string,
): Promise<string | undefined> {
  if (!model.reasoning) {
    return undefined;
  }
  const levels: string[] = model.supportedThinkingLevels ?? ['off', 'minimal', 'low', 'medium', 'high', 'xhigh', 'max'];
  if (requested) {
    if (levels.includes(requested)) {
      return requested;
    }
    throw new Error(`thinking level '${requested}' is not supported by ${model.provider}/${model.id}; supported: ${levels.join(', ')}`);
  }
  if (levels.length <= 1) {
    return levels[0];
  }
  if (!process.stdin.isTTY) {
    return undefined;
  }
  const picked = await pickFromList({
    message: `Thinking level for ${model.provider}/${model.id}`,
    items: levels.map((level) => ({ id: level, label: level })),
  });
  return picked ?? undefined;
}

async function setupPiBackend(
  workspaceRoot: string,
  options: SetBackendOptions,
): Promise<SetBackendResult> {
  await requirePiPackage();

  const handle = await openPiRuntime({
    agentDir: options.agentDir,
    modelsPath: options.models,
    authPath: options.auth,
    model: options.model,
  });

  if (options.list) {
    const providers = handle.listProviders();
    const models = handle.listModels(options.provider);
    return {
      kind: 'pi',
      info: `pi catalog: ${providers.length} providers, ${models.length} models`,
      providers,
      models,
    };
  }

  // Resolve credentials before the catalog lookup when the user named a
  // provider. Some providers (llama.cpp) only publish a catalog after auth.
  const normalizedModel = options.model?.replace(':', '/');
  const explicitServer = options.baseUrl !== undefined
    || options.host !== undefined
    || options.port !== undefined;
  const providerHint = options.provider
    ?? (normalizedModel?.includes('/') ? normalizedModel.split('/')[0] : undefined)
    ?? (explicitServer ? LLAMA_PROVIDER_ID : undefined);
  if (
    providerHint
    && handle.listProviders().some((provider) => provider.id === providerHint)
  ) {
    await ensurePiAuth(handle, providerHint, options);
  }

  // When the endpoint flags imply llama.cpp but no provider was named, keep
  // the picker scoped to that provider instead of every model in pi's catalog.
  const chooseOptions = explicitServer && !options.provider
    ? { ...options, provider: providerHint ?? LLAMA_PROVIDER_ID }
    : options;
  const model = await choosePiModel(handle, chooseOptions);
  await ensurePiAuth(handle, model.provider, options);
  const thinkingLevel = await chooseThinkingLevel(model, options.thinking);

  // Validate that the model is usable before persisting the choice.
  const probe = await createPiBackend({
    modelRegistry: handle.registry,
    model: `${model.provider}/${model.id}`,
    thinkingLevel,
  });

  await updateConfig(workspaceRoot, {
    backend: {
      kind: 'pi',
      agentDir: options.agentDir,
      modelsPath: options.models ?? null,
      authPath: options.auth ?? null,
      model: probe.modelId,
      thinkingLevel: probe.thinkingLevel,
    },
  });

  return {
    kind: 'pi',
    model: probe.modelId,
    provider: model.provider,
    thinkingLevel: probe.thinkingLevel,
    info: `backend: pi (model: ${probe.modelId}${probe.thinkingLevel ? `, thinking: ${probe.thinkingLevel}` : ''})`,
  };
}

export async function setBackend(
  workspaceRoot: string,
  kind: BackendKind,
  options: SetBackendOptions = {},
): Promise<SetBackendResult> {
  if (kind === 'env') {
    await updateConfig(workspaceRoot, { backend: { kind: 'env' } });
    return { kind, info: 'backend: env (SDK provider and API-key settings)' };
  }
  return setupPiBackend(workspaceRoot, options);
}

export async function getBackendStatus(workspaceRoot: string): Promise<string> {
  const config = await loadConfig(workspaceRoot);
  const backend = config.backend ?? { kind: 'env' };
  if (backend.kind === 'pi') {
    return [
      'backend: pi',
      `model: ${backend.model ?? '(first available)'}`,
      `thinking: ${backend.thinkingLevel ?? '(provider default)'}`,
      `agentDir: ${backend.agentDir ?? '(pi default)'}`,
      `pi package: ${isPiPackageAvailable() ? 'available' : 'NOT FOUND'}`,
    ].join('\n');
  }
  return ['backend: env', `model: ${config.models.run}`].join('\n');
}

export function registerSetBackendCommand(program: Command): void {
  program
    .command('set-backend <kind>')
    .description("Select the model backend: env (SDK keys), pi (pi's catalog/credentials), or status")
    .option('--agent-dir <path>', 'explicit pi agent dir (default: ~/.pi/agent)')
    .option('--models <path>', 'explicit models.json path')
    .option('--auth <path>', 'explicit auth.json path')
    .option('--model <provider/model>', 'pin the default model (skips the picker)')
    .option('--thinking <level>', 'thinking level for reasoning models (off|minimal|low|medium|high|xhigh|max)')
    .option('--provider <id>', 'restrict the catalog/picker to one provider')
    .option('--base-url <url>', 'llama.cpp server URL (overrides --host/--port)')
    .option('--host <host>', 'llama.cpp server host (default 127.0.0.1)')
    .option('--port <port>', 'llama.cpp server port (default 8080)')
    .option('--api-key <key>', 'store an API key for the provider in pi credentials')
    .option('--install', `(no-op) ${PI_PACKAGE} ships as a dependency`)
    .option('--list', 'list pi providers and models without changing the backend')
    .option('--json', 'machine-readable output (with --list or status)')
    .option('--no-login', 'do not start an interactive login; fail if unauthenticated')
    .action(async (kind: string, options) => {
      try {
        if (kind === 'status') {
          const config = await loadConfig(process.cwd());
          if (options.json) {
            console.log(JSON.stringify({
              kind: config.backend?.kind ?? 'env',
              model: config.backend?.model,
              thinkingLevel: config.backend?.thinkingLevel,
              agentDir: config.backend?.agentDir,
              piAvailable: isPiPackageAvailable(),
            }));
          } else {
            console.log(await getBackendStatus(process.cwd()));
          }
          return;
        }
        if (kind !== 'pi' && kind !== 'env') {
          console.error("❌ Unknown backend. Use 'pi', 'env', or 'status'.");
          process.exit(1);
        }

        const result = await setBackend(process.cwd(), kind, options);
        if (options.list) {
          if (options.json) {
            console.log(JSON.stringify({ providers: result.providers, models: result.models }, null, 2));
          } else {
            console.log(`\npi providers (${result.providers?.length ?? 0}):\n`);
            for (const provider of result.providers ?? []) {
              console.log(`  ${provider.id} (${provider.name})`
                + `${provider.configured ? ' — configured' : ' — not configured'}`
                + ` [${[provider.apiKey ? 'api-key' : null, provider.oauth ? 'oauth' : null].filter(Boolean).join(', ') || 'ambient'}]`);
            }
            console.log(`\npi chat models (${result.models?.length ?? 0}):\n`);
            for (const model of result.models ?? []) {
              console.log(`  ${model.provider}/${model.id}${model.reasoning ? ' [reasoning]' : ''}`);
            }          }
          return;
        }
        console.log(result.info);
      } catch (error) {
        console.error('❌ Error:', (error as Error).message);
        process.exit(1);
      }
    });
}
