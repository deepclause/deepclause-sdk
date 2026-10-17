/**
 * Forms inside a conversation. The model may render a form and then reply without
 * calling `wait_for_form_change` (it should not have to know the two-call
 * protocol); a chat turn must not end with that form unanswered, or the person's
 * submission has nowhere to go.
 */
import { afterEach, describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

const DML = `
agent_main :-
    system("You collect a name."),
    input(A),
    chat(A).
`;

const SPEC = {
  version: 1,
  title: 'Your name',
  fields: [{ name: 'subject', type: 'string' }],
};

function call(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return {
    text: '',
    toolCalls: [{ id: name, name, arguments: args }],
    usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
  };
}

describe('forms in a chat turn', () => {
  it('waits for a form the model rendered but did not wait for', async () => {
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        // 1: render the form. 2: reply without waiting (the easy mistake).
        // 3: the host has injected the submission by now.
        if (requests.length === 1) return call('render_form', { arg1: SPEC });
        return {
          text: requests.length === 2 ? 'Here is a form for you.' : 'Thanks — saved.',
          toolCalls: [],
          usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
        };
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    const outputs: string[] = [];
    let submitted = false;
    for await (const event of sdk.runDML(DML, {
      onUserInput: async () => 'collect my name',
      onForm: async (form) => {
        if (form.op !== 'open' || submitted) return;
        submitted = true;
        sdk!.submitForm(form.formId, { action: 'submit', values: { subject: 'Ada' } });
      },
    })) {
      if (event.type === 'output' && event.content) outputs.push(event.content);
    }

    expect(submitted).toBe(true);
    // render -> reply -> (host injects the submission) -> reply
    expect(requests).toHaveLength(3);
    // The host told the model what the person entered.
    expect(JSON.stringify(requests[2].messages)).toContain('Ada');
    expect(outputs.join('\n')).toContain('Thanks');
  });

  it('delivers a submission that arrives after the idle wait is registered', async () => {
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        if (requests.length === 1) return call('render_form', { arg1: SPEC });
        return {
          text: requests.length === 2 ? 'Here is a form for you.' : 'Thanks — saved.',
          toolCalls: [],
          usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
        };
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });
    const controller = new AbortController();

    let submitted = false;
    for await (const event of sdk.runDML(DML, {
      signal: controller.signal,
      onUserInput: async () => 'collect my name',
      onForm: async (form) => {
        if (form.op !== 'open' || submitted) return;
        submitted = true;
        // The person clicks *after* the turn went idle and registered its wait.
        // Submitting synchronously (inside onForm) misses the waiter path.
        setTimeout(() => {
          sdk!.submitForm(form.formId, { action: 'submit', values: { subject: 'Ada' } });
        }, 10);
      },
    })) {
      // drain
    }

    expect(submitted).toBe(true);
    expect(requests).toHaveLength(3);
    const third = JSON.stringify(requests[2].messages);
    expect(third).toContain('Ada');
    expect(third).not.toContain('cancelled');
  });

  it('does not wait twice when the model did call wait_for_form_change', async () => {
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        const results = request.messages.filter((message) => message.role === 'tool').map((message) => message.content);
        if (results.length === 0) return call('render_form', { arg1: SPEC });
        if (results.length === 1) {
          return call('wait_for_form_change', { arg1: JSON.parse(results[0]) as string });
        }
        return {
          text: 'Thanks — saved.',
          toolCalls: [],
          usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
        };
      },
    };
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend });

    let submitted = false;
    for await (const event of sdk.runDML(DML, {
      onUserInput: async () => 'collect my name',
      onForm: async (form) => {
        if (form.op !== 'open' || submitted) return;
        submitted = true;
        sdk!.submitForm(form.formId, { action: 'submit', values: { subject: 'Ada' } });
      },
    })) {
      // drain
    }

    // The model's own wait consumed the change, so the turn ends on its reply.
    expect(requests).toHaveLength(3);
  });
});
