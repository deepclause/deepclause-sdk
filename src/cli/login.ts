/**
 * `deepclause login` / `deepclause logout` — pi-backed authentication.
 *
 * This is the terminal port of pi's `/login`: pick a provider, pick an auth
 * method when the provider offers both, run pi's login (which stores the
 * credential), then pick the default model. Provider and model lists use the
 * readline picker, so a large catalog is filtered instead of scrolled.
 */

import type { Command } from 'commander';

import { updateConfig } from './config.js';
import {
  LLAMA_PROVIDER_ID,
  openPiRuntime,
  type LlamaServerOptions,
  type PiRuntimeHandle,
} from '../repl/backend.js';
import {
  createTerminalAuthInteraction,
  listLoginProviders,
  loginToProvider,
  logoutProvider,
} from '../repl/auth.js';
import { pickFromList, type PickItem } from '../repl/picker.js';
import { choosePiModel, chooseThinkingLevel, type SetBackendOptions } from './set-backend.js';

export interface LoginCliOptions extends LlamaServerOptions {
  agentDir?: string;
  models?: string;
  auth?: string;
  provider?: string;
  apiKey?: string;
  oauth?: boolean;
  model?: string;
  /** Thinking level for reasoning models (skips the picker). */
  thinking?: string;
  noModel?: boolean;
  json?: boolean;
}

function openHandle(options: LoginCliOptions): Promise<PiRuntimeHandle> {
  return openPiRuntime({
    agentDir: options.agentDir,
    modelsPath: options.models,
    authPath: options.auth,
  });
}

/** Providers that support login, deduped, with configured status. */
function providerChoices(handle: PiRuntimeHandle, oauthOnly: boolean): PickItem[] {
  const seen = new Map<string, PickItem>();
  for (const option of listLoginProviders(handle, oauthOnly ? { authType: 'oauth' } : {})) {
    if (seen.has(option.id)) continue;
    seen.set(option.id, {
      id: option.id,
      label: `${option.name} (${option.id})`,
      hint: option.configured ? '✓ configured' : option.source,
    });
  }
  return [...seen.values()];
}

async function resolveProvider(
  handle: PiRuntimeHandle,
  options: LoginCliOptions,
): Promise<{ id: string; type: 'api_key' | 'oauth' } | null> {
  let providerId = options.provider;
  if (
    !providerId
    && (options.baseUrl !== undefined || options.host !== undefined || options.port !== undefined)
  ) {
    providerId = LLAMA_PROVIDER_ID;
  }
  if (!providerId) {
    const items = providerChoices(handle, Boolean(options.oauth));
    if (items.length === 0) {
      throw new Error('no login providers available');
    }
    const picked = await pickFromList({ message: 'Choose a provider to sign in to', items, pageSize: 20 });
    if (!picked) return null;
    providerId = picked;
  }

  const types = listLoginProviders(handle)
    .filter((provider) => provider.id === providerId)
    .map((provider) => provider.authType);
  if (types.length === 0) {
    throw new Error(`provider '${providerId}' has no login method`);
  }
  if (options.apiKey || options.oauth) {
    return { id: providerId, type: options.oauth ? 'oauth' : 'api_key' };
  }
  if (types.length === 1) {
    return { id: providerId, type: types[0] };
  }
  const picked = await pickFromList({
    message: `Sign in to ${providerId} with`,
    items: [
      { id: 'api_key', label: 'API key' },
      { id: 'oauth', label: 'OAuth / subscription' },
    ],
  });
  if (!picked) return null;
  return { id: providerId, type: picked as 'api_key' | 'oauth' };
}

export async function loginCommand(
  workspaceRoot: string,
  options: LoginCliOptions,
): Promise<{ provider: string; model?: string }> {
  const inferredLlamaEndpoint = options.baseUrl !== undefined
    || options.host !== undefined
    || options.port !== undefined;
  if (options.apiKey && !options.provider && !inferredLlamaEndpoint) {
    throw new Error('--api-key requires --provider <id>');
  }
  const handle = await openHandle(options);
  const choice = await resolveProvider(handle, options);
  if (!choice) {
    throw new Error('login cancelled');
  }

  await loginToProvider(handle, choice.id, {
    interaction: createTerminalAuthInteraction(),
    type: choice.type,
    apiKey: options.apiKey,
    baseUrl: options.baseUrl,
    host: options.host,
    port: options.port,
  });
  console.log(`\n✓ Signed in to ${choice.id}.`);

  let model: string | undefined;
  let thinkingLevel: string | undefined;
  if (!options.noModel) {
    const chosen = await choosePiModel(handle, {
      provider: choice.id,
      model: options.model,
    } as SetBackendOptions);
    model = `${chosen.provider}/${chosen.id}`;
    thinkingLevel = await chooseThinkingLevel(chosen, options.thinking);
  } else if (options.model) {
    model = options.model;
  }

  await updateConfig(workspaceRoot, {
    backend: {
      kind: 'pi',
      model,
      thinkingLevel,
      agentDir: options.agentDir,
      modelsPath: options.models ?? null,
      authPath: options.auth ?? null,
    },
  });
  if (model) {
    console.log(`Default model set to ${model}${thinkingLevel ? ` (thinking: ${thinkingLevel})` : ''}.`);
  }
  return { provider: choice.id, model };
}

export async function logoutCommand(options: LoginCliOptions): Promise<string[]> {
  const handle = await openHandle(options);
  const credentials = await handle.listCredentials();
  if (credentials.length === 0) {
    return [];
  }
  let providerId = options.provider;
  if (!providerId) {
    const picked = await pickFromList({
      message: 'Choose a provider to sign out of',
      items: credentials.map((credential) => ({
        id: credential.providerId,
        label: credential.providerId,
        hint: credential.type,
      })),
    });
    if (!picked) return [];
    providerId = picked;
  }
  await logoutProvider(handle, providerId);
  return [providerId];
}

export function registerLoginCommands(program: Command): void {
  program
    .command('login')
    .description("Sign in to a model provider using pi's credential store, then pick a default model")
    .option('--provider <id>', 'provider to sign in to (skips the picker)')
    .option('--base-url <url>', 'llama.cpp server URL (overrides --host/--port)')
    .option('--host <host>', 'llama.cpp server host (default 127.0.0.1)')
    .option('--port <port>', 'llama.cpp server port (default 8080)')
    .option('--api-key <key>', 'store an API key for the provider (requires --provider)')
    .option('--oauth', 'use OAuth / subscription sign-in')
    .option('--model <provider/model>', 'set the default model without the picker')
    .option('--thinking <level>', 'thinking level for reasoning models (off|minimal|low|medium|high|xhigh|max)')
    .option('--no-model', 'do not change the default model')
    .option('--agent-dir <path>', "pi agent dir (default: pi's own)")
    .option('--models <path>', 'explicit models.json path')
    .option('--auth <path>', 'explicit auth.json path')
    .option('--json', 'machine-readable output')
    .action(async (options) => {
      try {
        const result = await loginCommand(process.cwd(), options);
        if (options.json) {
          console.log(JSON.stringify(result));
        } else if (!result.model) {
          console.log('Default model unchanged.');
        }
      } catch (error) {
        console.error('❌ Error:', (error as Error).message);
        process.exit(1);
      }
    });

  program
    .command('logout [provider]')
    .description("Remove a stored pi credential (pi's /logout)")
    .option('--agent-dir <path>', "pi agent dir (default: pi's own)")
    .option('--models <path>', 'explicit models.json path')
    .option('--auth <path>', 'explicit auth.json path')
    .action(async (provider: string | undefined, options) => {
      try {
        const removed = await logoutCommand({ ...options, provider });
        console.log(
          removed.length > 0
            ? `Removed credential for ${removed.join(', ')}.`
            : 'No stored credentials to remove.',
        );
      } catch (error) {
        console.error('❌ Error:', (error as Error).message);
        process.exit(1);
      }
    });
}
