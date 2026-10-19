/**
 * An OpenAI-compatible endpoint — which is what Ollama (`/v1`) and llama.cpp's
 * server (`/v1`) expose — works through the plain `openai` provider plus a
 * `baseUrl`. The SDK forces the *chat completions* API whenever a base URL is
 * set, because those servers do not implement the Responses API.
 *
 * This also guards a bug this test would have caught: an AI-SDK assistant message
 * carries its text as content *parts*, and the memory builder only accepted a
 * string, so every assistant reply was dropped from a conversation's memory
 * (and `chat/2` bound an empty string). The injected-backend tests cannot see
 * that path.
 */
import { afterEach, describe, expect, it } from 'vitest';
import { createServer, type Server } from 'node:http';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK } from '../src/index.js';

interface FakeLocal {
  baseUrl: string;
  paths: string[];
  bodies: Array<Record<string, unknown>>;
  close: () => Promise<void>;
}

/** A minimal `/v1/chat/completions` server, streaming or not, as local servers do. */
async function fakeLocalServer(reply = 'LOCAL-OK'): Promise<FakeLocal> {
  const paths: string[] = [];
  const bodies: Array<Record<string, unknown>> = [];
  const server: Server = createServer((req, res) => {
    let body = '';
    req.on('data', (chunk) => { body += chunk; });
    req.on('end', () => {
      paths.push(String(req.url));
      const parsed = JSON.parse(body || '{}') as Record<string, unknown>;
      bodies.push(parsed);
      const chunk = (delta: Record<string, unknown>, finish: string | null): string =>
        `data: ${JSON.stringify({
          id: 'chatcmpl-local', object: 'chat.completion.chunk', created: 1, model: 'local',
          choices: [{ index: 0, delta, finish_reason: finish }],
        })}\n\n`;
      if (parsed.stream === true) {
        res.writeHead(200, { 'content-type': 'text/event-stream' });
        res.write(chunk({ role: 'assistant', content: reply }, null));
        res.write(chunk({}, 'stop'));
        res.write('data: [DONE]\n\n');
        res.end();
        return;
      }
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({
        id: 'chatcmpl-local', object: 'chat.completion', created: 1, model: 'local',
        choices: [{ index: 0, message: { role: 'assistant', content: reply }, finish_reason: 'stop' }],
        usage: { prompt_tokens: 3, completion_tokens: 2, total_tokens: 5 },
      }));
    });
  });
  await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve));
  const port = (server.address() as { port: number }).port;
  return {
    baseUrl: `http://127.0.0.1:${port}/v1`,
    paths,
    bodies,
    close: () => new Promise<void>((resolve) => server.close(() => resolve())),
  };
}

let sdk: DeepClauseSDK | undefined;
let local: FakeLocal | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
  await local?.close();
  local = undefined;
});

const TWO_TURNS = `
agent_main :-
    chat("first", R1),
    chat("second", R2),
    format(string(M), "R1=[~w] R2=[~w]", [R1, R2]),
    answer(M).
`;

describe('an OpenAI-compatible local model', () => {
  for (const streaming of [false, true]) {
    it(`runs a conversation through chat/2 (streaming=${streaming})`, async () => {
      local = await fakeLocalServer();
      sdk = await createDeepClause({
        model: 'local',
        provider: 'openai',
        baseUrl: local.baseUrl,
        apiKey: 'local-key-not-needed',
        streaming,
      });

      const answers: string[] = [];
      for await (const event of sdk.runDML(TWO_TURNS)) {
        if (event.type === 'answer' && event.content) answers.push(event.content);
        if (event.type === 'error') answers.push(`ERR ${event.content}`);
      }

      // Chat completions, not the Responses API — what Ollama/llama.cpp serve.
      expect(local.paths.every((path) => path.endsWith('/chat/completions'))).toBe(true);
      // `chat/2` binds the reply...
      expect(answers.join('\n')).toContain('R1=[LOCAL-OK] R2=[LOCAL-OK]');
      // ...and the second turn's memory carries the first assistant reply.
      const second = local.bodies[1]?.messages as Array<{ role: string; content: unknown }>;
      const assistant = second.filter((message) => message.role === 'assistant');
      expect(assistant).toHaveLength(1);
      expect(JSON.stringify(assistant[0]?.content)).toContain('LOCAL-OK');
    }, 40_000);
  }
});
