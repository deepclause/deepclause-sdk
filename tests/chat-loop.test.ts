/**
 * `chat/1` is a traditional agent loop: tools, no `finish`/`set_result`, and the
 * turn ends when the model stops calling tools. `input/1` is the turn boundary.
 * See docs/DML_CONVERSATION.md.
 */
import { afterEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createDeepClause } from '../src/index.js';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DeepClauseSDK, DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;
let repl: HarnessRepl | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
  await repl?.dispose();
  repl = undefined;
});

function reply(text: string): LLMBackendResponse {
  return { text, toolCalls: [], usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 } };
}

const LOOP = `
agent_main :-
    system("You are a chat assistant."),
    loop.

loop :-
    input(A),
    chat(A),
    loop.
`;

describe('chat/1 is a conversational turn', () => {
  it('offers tools but not finish/set_result, and ends when the model stops calling tools', async () => {
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        return reply('hello!');
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    const outputs: string[] = [];
    const prompts: string[] = [];
    let inputs = 0;
    for await (const event of sdk.runDML(LOOP, {
      onUserInput: async (prompt) => {
        prompts.push(prompt);
        inputs += 1;
        return (inputs === 1 ? 'hi' : undefined) as unknown as string;
      },
    })) {
      if (event.type === 'output' && event.content) outputs.push(event.content);
      if (event.type === 'input_required') prompts.push(event.prompt ?? '');
    }

    // Exactly one model call for the turn: no nudge after a reply without tools.
    expect(requests).toHaveLength(1);
    const names = (requests[0].tools ?? []).map((tool) => tool.name);
    expect(names).not.toContain('finish');
    expect(names).not.toContain('set_result');
    // The person's message was the turn's user message...
    expect(JSON.stringify(requests[0].messages)).toContain('hi');
    // ...the reply is shown...
    expect(outputs.join('\n')).toContain('hello!');
    // ...and `input/1` asks for nothing: the chat UI is already the prompt.
    expect(prompts.every((prompt) => prompt === '')).toBe(true);
  });

  it('carries the conversation forward across turns', async () => {
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        return reply(`reply ${requests.length}`);
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    const messages = ['first message', 'second message'];
    let inputs = 0;
    for await (const _event of sdk.runDML(LOOP, {
      onUserInput: async () => {
        const message = messages[inputs];
        inputs += 1;
        return message as unknown as string;
      },
    })) {
      // drain
    }

    expect(requests).toHaveLength(2);
    const second = JSON.stringify(requests[1].messages);
    expect(second).toContain('first message');
    expect(second).toContain('reply 1');
    expect(second).toContain('second message');
  });

  it('keeps going while the model calls tools, then ends on the reply', async () => {
    const DML = `
agent_main :-
    system("You are a chat assistant."),
    input(A),
    chat(A).

tool(echo(Text, Result), "Echo the text back.", [string]) :-
    Result = Text.
`;
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        if (requests.length === 1) {
          return {
            text: '',
            toolCalls: [{ id: 't1', name: 'echo', arguments: { arg1: 'ping' } }],
            usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
          };
        }
        return reply('echoed it');
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    const outputs: string[] = [];
    for await (const event of sdk.runDML(DML, {
      onUserInput: async () => 'say ping',
    })) {
      if (event.type === 'output' && event.content) outputs.push(event.content);
    }

    // One tool call, then the reply: two model calls, one turn, no nudge.
    expect(requests).toHaveLength(2);
    expect(JSON.stringify(requests[1].messages)).toContain('ping');
    expect(outputs.join('\n')).toContain('echoed it');
  });

  it('ends the loop when there is nobody to talk to', async () => {
    // `input/1` fails rather than returning an empty message, so the harness does
    // not run an empty turn.
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        return reply('should not happen');
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    const events: DMLEvent[] = [];
    for await (const event of sdk.runDML(LOOP, { onUserInput: async () => undefined as unknown as string })) {
      events.push(event);
    }

    expect(requests).toHaveLength(0);
    expect(events.some((event) => event.type === 'error')).toBe(false);
  });
});

describe('next/1: one inbox for the person and a worker', () => {
  it('receives a message a worker sent to the session', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-chat-event-'));
    const backend: LLMBackend = {
      async complete() {
        return reply('event handled');
      },
    };
    // Nobody types: the inbox closes at once, and the closed-inbox poll still
    // picks up the worker's message rather than swallowing it.
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
      onInput: async () => undefined,
    });

    await repl.session.beginTurn();
    await repl.session.loadCode(`
agent_main :-
    self(Me),
    spawn_opt(messenger(Me), _, [name(worker)]),
    next(Item),
    (   Item = message(ping)
    ->  chat("A worker pinged."),
        answer("handled event")
    ;   format(string(S), "unexpected: ~w", [Item]),
        answer(S)
    ).

messenger(To) :- send(To, ping).
`);
    await repl.session.endTurn();

    const answers: string[] = [];
    const errors: string[] = [];
    for await (const event of repl.query('agent_main')) {
      if (event.type === 'answer' && event.content) answers.push(event.content);
      if (event.type === 'error') errors.push(event.content ?? '');
    }

    expect(errors).toEqual([]);
    expect(answers.join('\n')).toContain('handled event');
  });

  it('keeps a worker message queued when input/1 is waiting', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-chat-queue-'));
    const backend: LLMBackend = {
      async complete() {
        return reply('ok');
      },
    };
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      llmBackend: backend,
      // The person speaks once; the worker's message was already in flight.
      onInput: (() => {
        let asked = 0;
        return async () => {
          asked += 1;
          return asked === 1 ? 'hello' : undefined;
        };
      })(),
    });

    await repl.session.beginTurn();
    await repl.session.loadCode(`
agent_main :-
    self(Me),
    spawn_opt(messenger(Me), _, [name(worker)]),
    sleep(50),                     % let the worker deliver first
    input(A),                      % takes the person's message, not the worker's
    next(Item),                    % which is still queued
    (   Item = message(ping)
    ->  format(string(S), "~w then event", [A]), answer(S)
    ;   format(string(S), "~w then ~w", [A, Item]), answer(S)
    ).

messenger(To) :- send(To, ping).
`);
    await repl.session.endTurn();

    const answers: string[] = [];
    for await (const event of repl.query('agent_main')) {
      if (event.type === 'answer' && event.content) answers.push(event.content);
    }
    expect(answers.join('\n')).toContain('hello then event');
  });
});

describe('a failed turn does not end the conversation', () => {
  it('reports the failure, keeps the conversation, and answers the next message', async () => {
    let calls = 0;
    const backend: LLMBackend = {
      async complete() {
        calls += 1;
        if (calls === 1) throw new Error('upstream 503');
        return reply('second time lucky');
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    const outputs: string[] = [];
    const errors: string[] = [];
    let inputs = 0;
    for await (const event of sdk.runDML(LOOP, {
      onUserInput: async () => {
        inputs += 1;
        return (inputs <= 2 ? `message ${inputs}` : undefined) as unknown as string;
      },
    })) {
      if (event.type === 'output' && event.content) outputs.push(event.content);
      if (event.type === 'error') errors.push(event.content ?? '');
    }

    // Two turns were attempted: the failed one, then the next message.
    expect(calls).toBe(2);
    // The person saw what happened...
    expect(outputs.join('\n')).toContain('model call failed');
    // ...and the conversation carried on.
    expect(outputs.join('\n')).toContain('second time lucky');
    expect(errors).toEqual([]);
  });
});
