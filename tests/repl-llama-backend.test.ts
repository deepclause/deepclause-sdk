/**
 * The pi backend registers pi's bundled llama.cpp provider, so a running
 * llama.cpp router can be reached through `LLAMA_BASE_URL` or `/login
 * llama.cpp` without a models.json entry.
 */
import { afterEach, describe, expect, it, vi } from 'vitest';
import { createServer, type Server } from 'node:http';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import * as path from 'node:path';

import {
  LLAMA_PROVIDER_ID,
  createPiBackend,
  openPiRuntime,
  resolveLlamaServerUrl,
} from '../src/repl/backend.js';
import { loginToProvider } from '../src/repl/auth.js';
import { PiModelResolver } from '../src/repl/model-resolver.js';

interface FakeRouter {
  baseUrl: string;
  port: number;
  chatBodies: Array<Record<string, unknown>>;
  close: () => Promise<void>;
}

const MODEL_ID = 'test-model.gguf';

type FakeMode = 'router' | 'single' | 'single-placeholder';

/** Minimal llama.cpp server: router mode, or single-model modes for the fallback. */
async function fakeRouter(mode: FakeMode = 'router'): Promise<FakeRouter> {
  const chatBodies: Array<Record<string, unknown>> = [];
  const server: Server = createServer((req, res) => {
    const url = new URL(req.url ?? '/', 'http://localhost');
    if (url.pathname === '/models') {
      res.writeHead(200, { 'content-type': 'application/json' });
      if (mode === 'router') {
        res.end(JSON.stringify({
          data: [{
            id: MODEL_ID,
            status: { value: 'loaded' },
            meta: { n_ctx: 8192, n_ctx_train: 32768 },
            architecture: { input_modalities: ['text'] },
            source: 'preset',
          }],
        }));
      } else if (mode === 'single') {
        res.end(JSON.stringify({ data: [{ id: MODEL_ID }] }));
      } else {
        // No id and no router status: force the placeholder path.
        res.end(JSON.stringify({ data: [{ object: 'model' }] }));
      }
      return;
    }
    if (url.pathname === '/v1/models') {
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({
        object: 'list',
        data: mode === 'single' ? [{ id: MODEL_ID, object: 'model' }] : [],
      }));
      return;
    }
    if (url.pathname === '/props') {
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify(mode === 'router'
        ? { chat_template: 'tool template', models_autoload: false }
        : {
            default_generation_settings: { n_ctx: mode === 'single' ? 4096 : 2048 },
            ...(mode === 'single' ? { model_path: `/models/${MODEL_ID}` } : {}),
          }));
      return;
    }
    if (url.pathname === '/v1/chat/completions') {
      let body = '';
      req.on('data', (chunk) => { body += chunk; });
      req.on('end', () => {
        chatBodies.push(JSON.parse(body || '{}') as Record<string, unknown>);
        const chunk = (delta: Record<string, unknown>, finish: string | null): string =>
          `data: ${JSON.stringify({
            id: 'chatcmpl-local',
            object: 'chat.completion.chunk',
            created: 1,
            model: MODEL_ID,
            choices: [{ index: 0, delta, finish_reason: finish }],
          })}\n\n`;
        res.writeHead(200, { 'content-type': 'text/event-stream' });
        res.write(chunk({ role: 'assistant', content: 'LOCAL-OK' }, null));
        res.write(chunk({}, 'stop'));
        res.write('data: [DONE]\n\n');
        res.end();
      });
      return;
    }
    res.writeHead(404, { 'content-type': 'application/json' });
    res.end('{}');
  });
  await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve));
  const port = (server.address() as { port: number }).port;
  return {
    baseUrl: `http://127.0.0.1:${port}`,
    port,
    chatBodies,
    close: () => new Promise<void>((resolve) => server.close(() => resolve())),
  };
}

let router: FakeRouter | undefined;
let dir: string | undefined;

afterEach(async () => {
  vi.unstubAllEnvs();
  await router?.close();
  router = undefined;
  if (dir) await rm(dir, { recursive: true, force: true });
  dir = undefined;
});

async function paths(): Promise<{ authPath: string; modelsPath: string }> {
  dir = await mkdtemp(path.join(tmpdir(), 'deepclause-llama-'));
  return {
    authPath: path.join(dir, 'auth.json'),
    modelsPath: path.join(dir, 'models.json'),
  };
}

describe('resolveLlamaServerUrl', () => {
  it('accepts a full URL, a host, or a host/port pair', () => {
    expect(resolveLlamaServerUrl({ baseUrl: 'http://192.168.1.5:9999/v1' }))
      .toBe('http://192.168.1.5:9999/v1');
    expect(resolveLlamaServerUrl({ host: '192.168.1.5', port: '9999' }))
      .toBe('http://192.168.1.5:9999');
    expect(resolveLlamaServerUrl({ host: 'gpu-box' })).toBe('http://gpu-box:8080');
    expect(resolveLlamaServerUrl({ host: 'http://gpu-box:1234' }))
      .toBe('http://gpu-box:1234');
    expect(resolveLlamaServerUrl({ port: 11434 })).toBe('http://127.0.0.1:11434');
    expect(resolveLlamaServerUrl()).toBeUndefined();
  });

  it('rejects an invalid port', () => {
    expect(() => resolveLlamaServerUrl({ host: 'localhost', port: 'nope' }))
      .toThrow(/invalid llama.cpp port/);
    expect(() => resolveLlamaServerUrl({ host: 'localhost', port: 70000 }))
      .toThrow(/invalid llama.cpp port/);
  });
});

describe('llama.cpp backend integration', () => {
  it('discovers the router from LLAMA_BASE_URL', async () => {
    router = await fakeRouter();
    const { authPath, modelsPath } = await paths();
    vi.stubEnv('LLAMA_BASE_URL', router.baseUrl);

    const handle = await openPiRuntime({ authPath, modelsPath });
    const provider = handle.listProviders().find((entry) => entry.id === LLAMA_PROVIDER_ID);

    expect(provider?.configured).toBe(true);
    expect(handle.listModels(LLAMA_PROVIDER_ID).map((model) => model.id)).toContain(MODEL_ID);
  }, 30_000);

  it('falls back to /v1/models for a single-model server', async () => {
    router = await fakeRouter('single');
    const { authPath, modelsPath } = await paths();
    vi.stubEnv('LLAMA_BASE_URL', router.baseUrl);

    const handle = await openPiRuntime({ authPath, modelsPath });
    expect(handle.listModels(LLAMA_PROVIDER_ID).map((model) => model.id)).toContain(MODEL_ID);
    expect(new PiModelResolver(handle.registry).catalog().map((entry) => entry.model))
      .toContain(`${LLAMA_PROVIDER_ID}:${MODEL_ID}`);

    const backend = await createPiBackend({
      authPath,
      modelsPath,
      model: `${LLAMA_PROVIDER_ID}/${MODEL_ID}`,
    });
    expect(backend.contextWindow).toBe(4096);

    const response = await backend.backend.complete({
      messages: [{ role: 'user', content: 'hi' }],
    });
    expect(response.text).toBe('LOCAL-OK');
    // A 4096-token window must not collapse max_tokens to 1 (pi-ai reserves
    // 4096 context tokens as a safety margin).
    expect(Number(router.chatBodies.at(-1)?.max_tokens)).toBeGreaterThan(1);
  }, 30_000);

  it('uses a placeholder model when single-model mode exposes no id', async () => {
    router = await fakeRouter('single-placeholder');
    const { authPath, modelsPath } = await paths();
    const handle = await openPiRuntime({ authPath, modelsPath });

    await loginToProvider(handle, LLAMA_PROVIDER_ID, {
      interaction: {
        prompt: async (prompt) => (prompt.type === 'secret' ? 'local-key' : router!.baseUrl),
        notify: () => undefined,
      },
    });

    expect(handle.listModels(LLAMA_PROVIDER_ID).map((model) => model.id)).toContain('local-model');
    expect(new PiModelResolver(handle.registry).catalog().map((entry) => entry.model))
      .toContain(`${LLAMA_PROVIDER_ID}:local-model`);

    const backend = await createPiBackend({
      authPath,
      modelsPath,
      model: `${LLAMA_PROVIDER_ID}/local-model`,
    });
    expect(backend.contextWindow).toBe(2048);

    const response = await backend.backend.complete({
      messages: [{ role: 'user', content: 'hi' }],
    });
    expect(response.text).toBe('LOCAL-OK');
    expect(Number(router.chatBodies.at(-1)?.max_tokens)).toBeGreaterThan(1);
  }, 30_000);

  it('accepts a bare host:port in the interactive URL prompt', async () => {
    router = await fakeRouter();
    const { authPath, modelsPath } = await paths();
    const handle = await openPiRuntime({ authPath, modelsPath });

    await loginToProvider(handle, LLAMA_PROVIDER_ID, {
      interaction: {
        prompt: async (prompt) => (prompt.type === 'secret'
          ? 'local-key'
          : `127.0.0.1:${router!.port}`),
        notify: () => undefined,
      },
    });

    expect(handle.hasAuth(LLAMA_PROVIDER_ID)).toBe(true);
    expect(handle.listModels(LLAMA_PROVIDER_ID).map((model) => model.id)).toContain(MODEL_ID);
  }, 30_000);

  it('uses an explicit host/port and skips the URL prompt during login', async () => {
    router = await fakeRouter();
    const { authPath, modelsPath } = await paths();
    const handle = await openPiRuntime({ authPath, modelsPath });
    const prompts: string[] = [];

    await loginToProvider(handle, LLAMA_PROVIDER_ID, {
      interaction: {
        prompt: async (prompt) => {
          prompts.push(prompt.type);
          return prompt.type === 'secret' ? 'local-key' : 'not-used';
        },
        notify: () => undefined,
      },
      host: '127.0.0.1',
      port: router.port,
    });

    expect(prompts).toEqual(['secret']);
    expect(handle.hasAuth(LLAMA_PROVIDER_ID)).toBe(true);
    expect(handle.listModels(LLAMA_PROVIDER_ID).map((model) => model.id)).toContain(MODEL_ID);
  }, 30_000);

  it('routes completions through the registered provider', async () => {
    router = await fakeRouter();
    const { authPath, modelsPath } = await paths();
    vi.stubEnv('LLAMA_BASE_URL', router.baseUrl);

    const backend = await createPiBackend({
      authPath,
      modelsPath,
      model: `${LLAMA_PROVIDER_ID}/${MODEL_ID}`,
    });
    const response = await backend.backend.complete({
      messages: [{ role: 'user', content: 'hi' }],
    });

    expect(response.text).toBe('LOCAL-OK');
    expect(response.finishReason).toBe('stop');
  }, 30_000);

  it('persists a login and can select the model without LLAMA_BASE_URL afterwards', async () => {
    router = await fakeRouter();
    const { authPath, modelsPath } = await paths();

    const handle = await openPiRuntime({ authPath, modelsPath });
    expect(handle.hasAuth(LLAMA_PROVIDER_ID)).toBe(false);

    await handle.login(LLAMA_PROVIDER_ID, {
      prompt: async (prompt) => (prompt.type === 'secret' ? 'local-key' : router!.baseUrl),
      notify: () => undefined,
    }, 'api_key');

    expect(handle.hasAuth(LLAMA_PROVIDER_ID)).toBe(true);
    expect(handle.listModels(LLAMA_PROVIDER_ID).map((model) => model.id)).toContain(MODEL_ID);
    expect(await handle.listCredentials()).toContainEqual({
      providerId: LLAMA_PROVIDER_ID,
      type: 'api_key',
    });

    // A fresh process-looking runtime must restore the stored credential and
    // the cached catalog without the environment variable.
    vi.unstubAllEnvs();
    const restarted = await openPiRuntime({ authPath, modelsPath });
    expect(restarted.hasAuth(LLAMA_PROVIDER_ID)).toBe(true);

    const backend = await createPiBackend({
      authPath,
      modelsPath,
      model: `${LLAMA_PROVIDER_ID}/${MODEL_ID}`,
    });
    expect(backend.modelId).toBe(`${LLAMA_PROVIDER_ID}/${MODEL_ID}`);
    expect(backend.contextWindow).toBe(8192);
  }, 30_000);
});
