/**
 * JavaScript-Prolog Bridge utilities
 */

import { randomUUID } from 'crypto';
import { request as httpsRequest } from 'https';
import { PassThrough, Readable } from 'stream';
import { google, createGoogleGenerativeAI } from '@ai-sdk/google';
import { anthropic, createAnthropic } from '@ai-sdk/anthropic';
import { createOpenAI } from '@ai-sdk/openai';
import { generateText } from 'ai';
import type { LanguageModel } from 'ai';
import type { MemoryMessage, LLMBackend, LLMUsage } from '../types.js';

export interface RawProviderResponseSnapshot {
  requestId: string;
  url: string;
  status: number;
  contentType: string | null;
  transport: 'https-one-shot' | 'undici';
  bodyText: string;
  captureError?: string;
}

export interface SampleSingleTokenOptions {
  prompt: string;
  allowedTokens?: string[];
  modelOptions: {
    provider: string;
    model: string;
    temperature: number;
    baseUrl?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    providerOptions?: Record<string, Record<string, any>>;
  };
  signal?: AbortSignal;
  debugLog?: (...args: unknown[]) => void;
  onRawResponse?: (snapshot: Promise<RawProviderResponseSnapshot>) => void;
  llmBackend?: LLMBackend;
}

export interface GenerateLlmReplyOptions {
  messages: MemoryMessage[];
  modelOptions: {
    provider: string;
    model: string;
    temperature: number;
    maxOutputTokens?: number;
    baseUrl?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    providerOptions?: Record<string, Record<string, any>>;
  };
  signal?: AbortSignal;
  debugLog?: (...args: unknown[]) => void;
  onRawResponse?: (snapshot: Promise<RawProviderResponseSnapshot>) => void;
  llmBackend?: LLMBackend;
}

function describeFetchError(error: unknown): string {
  if (!(error instanceof Error)) {
    return String(error);
  }

  const details: string[] = [`${error.name}: ${error.message}`];
  const topLevelCode = (error as Error & { code?: string }).code;
  if (topLevelCode) details.push(`code=${topLevelCode}`);

  const cause = error.cause;
  if (cause && typeof cause === 'object') {
    const causeRecord = cause as { name?: string; message?: string; code?: string; errno?: string | number; syscall?: string; address?: string; port?: number };
    const causeParts = [
      causeRecord.name,
      causeRecord.message,
      causeRecord.code ? `code=${causeRecord.code}` : undefined,
      causeRecord.errno ? `errno=${causeRecord.errno}` : undefined,
      causeRecord.syscall ? `syscall=${causeRecord.syscall}` : undefined,
      causeRecord.address ? `address=${causeRecord.address}` : undefined,
      causeRecord.port ? `port=${causeRecord.port}` : undefined,
    ].filter(Boolean);
    if (causeParts.length > 0) {
      details.push(`cause=${causeParts.join(' ')}`);
    }
  }

  if (error.stack) {
    details.push(`stack=${error.stack.split('\n').slice(0, 4).join(' | ')}`);
  }

  return details.join(' ');
}

function normalizeRequestBody(body: unknown): string | Uint8Array | undefined {
  if (body == null) return undefined;
  if (typeof body === 'string') return body;
  if (body instanceof URLSearchParams) return body.toString();
  if (body instanceof Uint8Array) return body;
  if (body instanceof ArrayBuffer) return new Uint8Array(body);
  if (ArrayBuffer.isView(body)) {
    return new Uint8Array(body.buffer, body.byteOffset, body.byteLength);
  }
  return undefined;
}

function createResponseHeaders(headers: Record<string, string | string[] | undefined>): Headers {
  const responseHeaders = new Headers();
  for (const [key, value] of Object.entries(headers)) {
    if (value == null) continue;
    if (Array.isArray(value)) {
      for (const item of value) {
        responseHeaders.append(key, item);
      }
      continue;
    }
    responseHeaders.set(key, value);
  }
  return responseHeaders;
}

function getIsolatedProxyAttemptTimeoutMs(): number {
  const raw = process.env.DC_PROXY_ATTEMPT_TIMEOUT_MS;
  if (raw != null) {
    const parsed = Number.parseInt(raw, 10);
    if (Number.isFinite(parsed) && parsed >= 0) {
      return parsed;
    }
  }
  return 300_000;
}

function getConfiguredTimeoutMs(envVar: string, fallback: number): number {
  const raw = process.env[envVar];
  if (raw != null) {
    const parsed = Number.parseInt(raw, 10);
    if (Number.isFinite(parsed) && parsed >= 0) {
      return parsed;
    }
  }
  return fallback;
}

function getIsolatedProxyFirstByteTimeoutMs(): number {
  return getConfiguredTimeoutMs('DC_PROXY_FIRST_BYTE_TIMEOUT_MS', getIsolatedProxyAttemptTimeoutMs());
}

function getIsolatedProxyBodyIdleTimeoutMs(): number {
  return getConfiguredTimeoutMs('DC_PROXY_BODY_IDLE_TIMEOUT_MS', getIsolatedProxyAttemptTimeoutMs());
}

function shouldUseIsolatedProxyTransport(baseUrl: string | undefined, requestUrl: string): boolean {
  if (!baseUrl || !requestUrl.startsWith(baseUrl)) {
    return false;
  }

  try {
    return new URL(requestUrl).protocol === 'https:';
  } catch {
    return false;
  }
}

async function isolatedHttpsFetch(
  url: string,
  init: RequestInit,
): Promise<Response> {
  const body = normalizeRequestBody(init.body);
  if (init.body != null && body == null) {
    throw new Error(`Unsupported request body type for isolated HTTPS fetch: ${typeof init.body}`);
  }

  return new Promise<Response>((resolve, reject) => {
    const target = new URL(url);
    const headers = new Headers(init.headers);
    headers.set('connection', 'close');
    if (body != null && !headers.has('content-length')) {
      const bodyByteLength = typeof body === 'string' ? Buffer.byteLength(body) : body.byteLength;
      headers.set('content-length', String(bodyByteLength));
    }
    const timeoutMs = getIsolatedProxyAttemptTimeoutMs();
    const abortSignal = init.signal;
    let settled = false;
    let timeoutId: NodeJS.Timeout | undefined;
    let abortRequest: () => void = () => {};

    const settle = (fn: () => void) => {
      if (settled) return;
      settled = true;
      if (timeoutId) clearTimeout(timeoutId);
      if (abortSignal) {
        abortSignal.removeEventListener('abort', abortRequest);
      }
      fn();
    };

    const fail = (error: unknown) => settle(() => reject(error));
    const succeed = (response: Response) => settle(() => resolve(response));

    const buildRetryableTimeoutError = (message: string) => {
      const timeoutError = new Error(message);
      (timeoutError as Error & { code?: string }).code = 'ETIMEDOUT';

      const fetchError = new TypeError('fetch failed') as TypeError & { cause?: unknown };
      fetchError.cause = timeoutError;
      return { timeoutError, fetchError };
    };

    const req = httpsRequest(
      {
        protocol: target.protocol,
        hostname: target.hostname,
        port: target.port ? Number.parseInt(target.port, 10) : 443,
        path: `${target.pathname}${target.search}`,
        method: init.method ?? 'GET',
        headers: Object.fromEntries(headers.entries()),
        agent: false,
      },
      (res) => {
        const status = res.statusCode ?? 500;
        const statusText = res.statusMessage ?? '';
        const responseHeaders = createResponseHeaders(res.headers);
        const contentLength = Array.isArray(res.headers['content-length'])
          ? res.headers['content-length'][0]
          : res.headers['content-length'];
        const hasNoBody = status === 204 || status === 304 || contentLength === '0';

        if (hasNoBody) {
          succeed(new Response(null, { status, statusText, headers: responseHeaders }));
          return;
        }

        const firstByteTimeoutMs = getIsolatedProxyFirstByteTimeoutMs();
        let firstByteSettled = false;
        let firstByteTimeoutId: NodeJS.Timeout | undefined;

        const settleFirstByte = (fn: () => void) => {
          if (firstByteSettled) return;
          firstByteSettled = true;
          if (firstByteTimeoutId) clearTimeout(firstByteTimeoutId);
          res.removeListener('data', onFirstData);
          res.removeListener('end', onEndBeforeFirstByte);
          res.removeListener('error', onFirstByteError);
          res.removeListener('aborted', onFirstByteAborted);
          fn();
        };

        const onFirstByteError = (error: Error) => {
          settleFirstByte(() => fail(error));
        };

        const onFirstByteAborted = () => {
          const abortedError = new Error('Upstream response aborted before first body byte');
          (abortedError as Error & { code?: string }).code = 'ECONNRESET';
          settleFirstByte(() => fail(abortedError));
        };

        const onEndBeforeFirstByte = () => {
          settleFirstByte(() => {
            succeed(new Response(null, { status, statusText, headers: responseHeaders }));
          });
        };

        const onFirstData = (firstChunk: Buffer) => {
          settleFirstByte(() => {
            const passthrough = new PassThrough();
            const forwardError = (error: Error) => {
              passthrough.destroy(error);
            };
            const forwardAbort = () => {
              const abortedError = new Error('Upstream response aborted before completion');
              (abortedError as Error & { code?: string }).code = 'ECONNRESET';
              passthrough.destroy(abortedError);
            };

            res.on('error', forwardError);
            res.on('aborted', forwardAbort);
            passthrough.on('close', () => {
              res.removeListener('error', forwardError);
              res.removeListener('aborted', forwardAbort);
            });

            passthrough.write(firstChunk);
            res.pipe(passthrough);
            succeed(new Response(Readable.toWeb(passthrough) as ReadableStream<Uint8Array>, {
              status,
              statusText,
              headers: responseHeaders,
            }));
          });
        };

        res.once('data', onFirstData);
        res.once('end', onEndBeforeFirstByte);
        res.once('error', onFirstByteError);
        res.once('aborted', onFirstByteAborted);

        if (firstByteTimeoutMs > 0) {
          firstByteTimeoutId = setTimeout(() => {
            const { timeoutError, fetchError } = buildRetryableTimeoutError(
              `Proxy response timed out waiting for first body byte after ${firstByteTimeoutMs}ms`,
            );
            res.destroy(timeoutError);
            fail(fetchError);
          }, firstByteTimeoutMs);
        }
      },
    );

    abortRequest = () => {
      const reason = abortSignal?.reason instanceof Error
        ? abortSignal.reason
        : Object.assign(new Error('This operation was aborted'), { name: 'AbortError' });
      req.destroy(reason);
      fail(reason);
    };

    if (abortSignal) {
      if (abortSignal.aborted) {
        abortRequest();
        return;
      }
      abortSignal.addEventListener('abort', abortRequest, { once: true });
    }

    req.on('socket', (socket) => {
      socket.setNoDelay(true);
    });
    req.on('error', fail);

    if (timeoutMs > 0) {
      timeoutId = setTimeout(() => {
        const { timeoutError, fetchError } = buildRetryableTimeoutError(
          `Proxy request timed out after ${timeoutMs}ms`,
        );
        req.destroy(timeoutError);
        fail(fetchError);
      }, timeoutMs);
    }

    if (body != null) {
      req.write(body);
    }

    req.end();
  });
}

/**
 * Create a model provider for the Vercel AI SDK
 */
export function createModelProvider(
  provider: string,
  model: string,
  baseUrl?: string,
  debugLog?: (...args: unknown[]) => void,
  onRawResponse?: (snapshot: Promise<RawProviderResponseSnapshot>) => void,
): LanguageModel {
  let proxyRequestSeq = 0;

  // Create an instrumented fetch that logs HTTP-level timing and body chunk delivery.
  const timedFetch: typeof globalThis.fetch | undefined = (debugLog || baseUrl)
    ? async (input: Parameters<typeof globalThis.fetch>[0], init?: Parameters<typeof globalThis.fetch>[1]) => {
        const url = typeof input === 'string' ? input : input instanceof URL ? input.toString() : input.url;
        const headers = new Headers(input instanceof Request ? input.headers : undefined);
        if (init?.headers) {
          const initHeaders = new Headers(init.headers);
          initHeaders.forEach((value, key) => headers.set(key, value));
        }

        proxyRequestSeq += 1;
        const requestId = `${process.env.SESSION_ID ?? 'no-session'}-${proxyRequestSeq}-${randomUUID().slice(0, 8)}`;
        headers.set('x-deepclause-request-id', requestId);
        headers.set('x-deepclause-llm-call-seq', String(proxyRequestSeq));
        if (process.env.SESSION_ID) headers.set('x-deepclause-exec-session-id', process.env.SESSION_ID);
        if (process.env.DC_SESSION_ID) headers.set('x-deepclause-dc-session-id', process.env.DC_SESSION_ID);
        if (process.env.SKILL) headers.set('x-deepclause-skill', process.env.SKILL);
        if (process.env.TASK_MODE) headers.set('x-deepclause-task-mode', process.env.TASK_MODE);

        const nextInit = { ...init, headers };
        const bodyLen = nextInit.body
          ? typeof nextInit.body === 'string'
            ? Buffer.byteLength(nextInit.body)
            : nextInit.body instanceof Uint8Array
              ? nextInit.body.byteLength
              : '(stream)'
          : 0;
        const fetchT0 = Date.now();
        const useIsolatedProxyTransport = shouldUseIsolatedProxyTransport(baseUrl, url);
        const transport = useIsolatedProxyTransport ? 'https-one-shot' : 'undici';
        debugLog?.(`[fetch] POST ${url} requestId=${requestId} seq=${proxyRequestSeq} transport=${transport} (body=${bodyLen})`);
        let response: Response;
        try {
          response = useIsolatedProxyTransport
            ? await isolatedHttpsFetch(url, nextInit)
            : await globalThis.fetch(input, nextInit);
        } catch (err) {
          const elapsed = Date.now() - fetchT0;
          debugLog?.(`[fetch] FAILED requestId=${requestId} seq=${proxyRequestSeq} after ${elapsed}ms: ${describeFetchError(err)}`);
          throw err;
        }
        const ttfb = Date.now() - fetchT0;
        debugLog?.(`[fetch] Response requestId=${requestId} seq=${proxyRequestSeq}: ${response.status} in ${ttfb}ms (TTFB) content-type=${response.headers.get('content-type')} content-encoding=${response.headers.get('content-encoding')}`);

        if (onRawResponse) {
          onRawResponse((async () => {
            try {
              return {
                requestId,
                url,
                status: response.status,
                contentType: response.headers.get('content-type'),
                transport,
                bodyText: await response.clone().text(),
              } satisfies RawProviderResponseSnapshot;
            } catch (error) {
              return {
                requestId,
                url,
                status: response.status,
                contentType: response.headers.get('content-type'),
                transport,
                bodyText: '',
                captureError: describeFetchError(error),
              } satisfies RawProviderResponseSnapshot;
            }
          })());
        }

        // Wrap response.body to track when chunks actually arrive
        if (response.body) {
          const originalBody = response.body;
          let chunkCount = 0;
          let totalBytes = 0;
          let firstChunkMs: number | null = null;
          const bodyIdleTimeoutMs = getIsolatedProxyBodyIdleTimeoutMs();
          const bodyAbortController = bodyIdleTimeoutMs > 0 ? new AbortController() : undefined;
          let bodyIdleTimer: NodeJS.Timeout | undefined;

          const clearBodyIdleTimer = () => {
            if (bodyIdleTimer) {
              clearTimeout(bodyIdleTimer);
              bodyIdleTimer = undefined;
            }
          };

          const scheduleBodyIdleTimer = () => {
            if (!bodyAbortController) return;
            clearBodyIdleTimer();
            bodyIdleTimer = setTimeout(() => {
              const idleError = new Error(`Proxy response body stalled for ${bodyIdleTimeoutMs}ms`);
              (idleError as Error & { code?: string }).code = 'ETIMEDOUT';
              debugLog?.(`[fetch] Body timeout requestId=${requestId} seq=${proxyRequestSeq} after ${bodyIdleTimeoutMs}ms without data`);
              bodyAbortController.abort(idleError);
            }, bodyIdleTimeoutMs);
          };

          const bodyTap = new TransformStream({
            transform(chunk, controller) {
              chunkCount++;
              const bytes = chunk.byteLength ?? chunk.length ?? 0;
              totalBytes += bytes;
              const elapsed = Date.now() - fetchT0;
              if (firstChunkMs === null) {
                firstChunkMs = elapsed;
                debugLog?.(`[fetch] First body chunk requestId=${requestId} seq=${proxyRequestSeq} at ${elapsed}ms (${bytes} bytes)`);
              } else if (chunkCount <= 10 || chunkCount % 50 === 0) {
                // Log first 10 chunks and every 50th after
                debugLog?.(`[fetch] Body chunk requestId=${requestId} seq=${proxyRequestSeq} #${chunkCount} at ${elapsed}ms (${bytes} bytes, total=${totalBytes})`);
              }
              scheduleBodyIdleTimer();
              controller.enqueue(chunk);
            },
            flush() {
              clearBodyIdleTimer();
              debugLog?.(`[fetch] Body complete requestId=${requestId} seq=${proxyRequestSeq}: ${chunkCount} chunks, ${totalBytes} bytes, ${Date.now() - fetchT0}ms total`);
            },
          });

          const bodyPipe = originalBody.pipeTo(
            bodyTap.writable,
            bodyAbortController ? { signal: bodyAbortController.signal } : undefined,
          );

          void bodyPipe.catch((err) => {
            clearBodyIdleTimer();
            const failure = bodyAbortController?.signal.aborted
              ? bodyAbortController.signal.reason ?? err
              : err;
            const kind = bodyAbortController?.signal.aborted ? 'aborted' : 'errored';
            debugLog?.(`[fetch] Body ${kind} requestId=${requestId} seq=${proxyRequestSeq} after ${Date.now() - fetchT0}ms: ${describeFetchError(failure)}`);
          });

          // Return a new Response with the instrumented body
          return new Response(bodyTap.readable, {
            status: response.status,
            statusText: response.statusText,
            headers: response.headers,
          });
        }
        return response;
      }
    : undefined;

  switch (provider) {
    case 'google': {
      if (baseUrl) {
        const g = createGoogleGenerativeAI({
          baseURL: baseUrl,
          apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
          ...(timedFetch ? { fetch: timedFetch } : {}),
        });
        return g(model);
      }
      return google(model);
    }

    case 'anthropic': {
      if (baseUrl) {
        const a = createAnthropic({
          baseURL: baseUrl,
          apiKey: process.env.ANTHROPIC_API_KEY,
          ...(timedFetch ? { fetch: timedFetch } : {}),
        });
        return a(model);
      }
      return anthropic(model);
    }

    case 'openai': {
      const openai = createOpenAI({
        apiKey: process.env.OPENAI_API_KEY,
        baseURL: baseUrl,
        ...(timedFetch ? { fetch: timedFetch } : {}),
      });
      // Use .chat() when a custom baseUrl is set to force the chat completions API.
      // Without this, @ai-sdk/openai defaults to the Responses API, which is not
      // supported by OpenRouter or any other proxy.
      return baseUrl ? openai.chat(model) : openai(model);
    }

    case 'openrouter': {
      const openrouter = createOpenAI({
        name: 'openrouter',
        baseURL: baseUrl ?? 'https://openrouter.ai/api/v1',
        apiKey: process.env.OPENROUTER_API_KEY ?? process.env.OPENAI_API_KEY,
        ...(timedFetch ? { fetch: timedFetch } : {}),
      });
      return openrouter.chat(model);
    }

    default: {
      // Default to OpenAI-compatible
      const defaultProvider = createOpenAI({
        apiKey: process.env.OPENAI_API_KEY,
        baseURL: baseUrl,
        ...(timedFetch ? { fetch: timedFetch } : {}),
      });
      // Use .chat() when a custom baseUrl is set to force chat completions API
      // instead of the Responses API (required for OpenRouter-compatible proxies).
      return baseUrl ? defaultProvider.chat(model) : defaultProvider(model);
    }
  }
}

function stripWrappingQuotes(token: string): string {
  if (token.length < 2) {
    return token;
  }
  const prefix = token[0];
  const suffix = token[token.length - 1];
  if ((prefix === '"' || prefix === "'") && prefix === suffix) {
    return token.slice(1, -1);
  }
  return token;
}

function stripTerminalPunctuation(token: string): string {
  if (token.length <= 1) {
    return token;
  }
  const suffix = token[token.length - 1];
  if (suffix === '.' || suffix === ',' || suffix === ';' || suffix === ':') {
    return token.slice(0, -1);
  }
  return token;
}

function normalizeSampleTokenResponse(rawToken: string): string {
  const trimmed = rawToken.trim();
  if (!trimmed) {
    throw new Error('Model returned an empty token response');
  }
  const [first] = trimmed.split(/\s+/);
  if (!first) {
    throw new Error('Model returned an empty token response');
  }
  return stripWrappingQuotes(first);
}

function chooseAllowedSampleToken(rawToken: string, allowedTokens: string[]): string {
  if (allowedTokens.length === 0) {
    throw new Error('Allowed token list must not be empty');
  }

  const candidate = normalizeSampleTokenResponse(rawToken);
  if (allowedTokens.includes(candidate)) {
    return candidate;
  }

  const punctuationStripped = stripTerminalPunctuation(candidate);
  if (punctuationStripped !== candidate && allowedTokens.includes(punctuationStripped)) {
    return punctuationStripped;
  }

  return allowedTokens[0];
}

function buildSampleTokenPrompt(prompt: string, allowedTokens?: string[]): string {
  if (!allowedTokens || allowedTokens.length === 0) {
    return prompt;
  }

  return `${prompt}\nAllowed tokens: ${JSON.stringify(allowedTokens)}`;
}

export async function sampleSingleToken(options: SampleSingleTokenOptions): Promise<{ token: string; usage?: LLMUsage }> {
  if (options.llmBackend) {
    const result = await options.llmBackend.complete({
      messages: [{ role: 'user', content: buildSampleTokenPrompt(options.prompt, options.allowedTokens) }],
      temperature: options.modelOptions.temperature,
      maxTokens: 1,
      signal: options.signal,
    });
    const token = options.allowedTokens && options.allowedTokens.length > 0
      ? chooseAllowedSampleToken(result.text, options.allowedTokens)
      : normalizeSampleTokenResponse(result.text);
    return { token, usage: result.usage };
  }

  const model = createModelProvider(
    options.modelOptions.provider,
    options.modelOptions.model,
    options.modelOptions.baseUrl,
    options.debugLog,
    options.onRawResponse,
  );

  const result = await generateText({
    model,
    prompt: buildSampleTokenPrompt(options.prompt, options.allowedTokens),
    temperature: options.modelOptions.temperature,
    maxOutputTokens: 1,
    abortSignal: options.signal,
    providerOptions: options.modelOptions.providerOptions,
  });

  const token = options.allowedTokens && options.allowedTokens.length > 0
    ? chooseAllowedSampleToken(result.text, options.allowedTokens)
    : normalizeSampleTokenResponse(result.text);

  const usage: LLMUsage | undefined = result.usage ? {
    inputTokens: result.usage.inputTokens ?? 0,
    outputTokens: result.usage.outputTokens ?? 0,
    totalTokens: result.usage.totalTokens ?? 0,
  } : undefined;

  return { token, usage };
}

export async function generateLlmReply(options: GenerateLlmReplyOptions): Promise<{ text: string; usage?: LLMUsage }> {
  if (options.llmBackend) {
    const result = await options.llmBackend.complete({
      messages: options.messages,
      temperature: options.modelOptions.temperature,
      maxTokens: options.modelOptions.maxOutputTokens,
      signal: options.signal,
    });
    return { text: result.text, usage: result.usage };
  }

  const model = createModelProvider(
    options.modelOptions.provider,
    options.modelOptions.model,
    options.modelOptions.baseUrl,
    options.debugLog,
    options.onRawResponse,
  );

  const result = await generateText({
    model,
    messages: options.messages,
    temperature: options.modelOptions.temperature,
    maxOutputTokens: options.modelOptions.maxOutputTokens,
    abortSignal: options.signal,
    providerOptions: options.modelOptions.providerOptions,
  });

  const usage: LLMUsage | undefined = result.usage ? {
    inputTokens: result.usage.inputTokens ?? 0,
    outputTokens: result.usage.outputTokens ?? 0,
    totalTokens: result.usage.totalTokens ?? 0,
  } : undefined;

  return { text: result.text, usage };
}

/**
 * Convert a JavaScript value to a Prolog term string
 */
export function jsToPrologTerm(value: unknown): string {
  if (value === null || value === undefined) {
    return 'null';
  }

  if (typeof value === 'string') {
    // Escape special characters and wrap in double quotes
    const escaped = value
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r')
      .replace(/\t/g, '\\t');
    return `"${escaped}"`;
  }

  if (typeof value === 'number') {
    return String(value);
  }

  if (typeof value === 'boolean') {
    return value ? 'true' : 'false';
  }

  if (Array.isArray(value)) {
    const items = value.map(jsToPrologTerm).join(', ');
    return `[${items}]`;
  }

  if (typeof value === 'object') {
    const entries = Object.entries(value)
      .map(([k, v]) => `${sanitizeAtom(k)}: ${jsToPrologTerm(v)}`)
      .join(', ');
    return `dict{${entries}}`;
  }

  return String(value);
}

/**
 * Convert a Prolog term to a JavaScript value
 */
export function prologTermToJs(term: unknown): unknown {
  if (term === null || term === undefined) {
    return null;
  }

  // Handle Prolog atoms/strings
  if (typeof term === 'string') {
    // Try to parse as JSON first
    try {
      return JSON.parse(term);
    } catch {
      return term;
    }
  }

  // Numbers pass through
  if (typeof term === 'number') {
    return term;
  }

  // Booleans pass through
  if (typeof term === 'boolean') {
    return term;
  }

  // Handle arrays (Prolog lists)
  if (Array.isArray(term)) {
    return term.map(prologTermToJs);
  }

  // Handle objects (Prolog dicts or structures)
  if (typeof term === 'object') {
    const result: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(term)) {
      // Skip the 'functor' property that represents the dict tag
      if (key !== 'functor' && key !== '_') {
        result[key] = prologTermToJs(value);
      }
    }
    return result;
  }

  return term;
}

/**
 * Sanitize a string to be a valid Prolog atom
 */
function sanitizeAtom(str: string): string {
  // If it starts with lowercase and contains only valid chars, it's already valid
  if (/^[a-z][a-zA-Z0-9_]*$/.test(str)) {
    return str;
  }

  // Otherwise, quote it
  const escaped = str.replace(/'/g, "''");
  return `'${escaped}'`;
}

/**
 * Parse a Prolog term string into arguments
 */
export function parsePrologArgs(termStr: string): unknown[] {
  // Simple parser for common cases
  const match = termStr.match(/\((.+)\)$/s);
  if (!match) {
    return [];
  }

  const content = match[1];
  const args: unknown[] = [];
  
  let current = '';
  let depth = 0;
  let inString = false;
  let stringChar = '';
  let escaped = false;

  for (const char of content) {
    if (escaped) {
      current += char;
      escaped = false;
      continue;
    }

    if (char === '\\') {
      current += char;
      escaped = true;
      continue;
    }

    if (!inString) {
      if (char === '"' || char === "'") {
        inString = true;
        stringChar = char;
        current += char;
      } else if ('([{'.includes(char)) {
        depth++;
        current += char;
      } else if (')]}'.includes(char)) {
        depth--;
        current += char;
      } else if (char === ',' && depth === 0) {
        args.push(parseArgValue(current.trim()));
        current = '';
      } else {
        current += char;
      }
    } else {
      current += char;
      if (char === stringChar) {
        inString = false;
      }
    }
  }

  if (current.trim()) {
    args.push(parseArgValue(current.trim()));
  }

  return args;
}

/**
 * Parse a single argument value
 */
function parseArgValue(str: string): unknown {
  // Quoted string
  if ((str.startsWith('"') && str.endsWith('"')) ||
      (str.startsWith("'") && str.endsWith("'"))) {
    return str.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\r/g, '\r')
      .replace(/\\t/g, '\t')
      .replace(/\\"/g, '"')
      .replace(/\\'/g, "'")
      .replace(/\\\\/g, '\\');
  }

  // Number
  const num = Number(str);
  if (!isNaN(num) && str !== '') {
    return num;
  }

  // Boolean atoms
  if (str === 'true') return true;
  if (str === 'false') return false;

  // Null
  if (str === 'null' || str === '[]') return null;

  // List (simplified - just return as string for now)
  if (str.startsWith('[')) {
    return str;
  }

  // Atom or variable
  return str;
}
