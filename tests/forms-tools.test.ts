/**
 * The model-facing path: a `task/N` agent calls the built-in form tools, and
 * the tool engine resolves the primitives through the same registry.
 */
import { afterEach, describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

const DML = `

agent_main :-
    with_tools([render_form, wait_for_form_change, close_form],
        task("Collect a note.", string(R))),
    answer("done").
`;

function toolCall(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return { text: '', toolCalls: [{ id: name, name, arguments: args }], usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 } };
}

/** render_form → wait_for_form_change → finish. */
function backend(): LLMBackend {
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      const results = request.messages.filter((m) => m.role === 'tool').map((m) => m.content);
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'R';

      if (results.length === 0) {
        return toolCall('render_form', {
          arg1: {
            version: 1,
            title: 'Note',
            fields: [{ name: 'subject', type: 'string' }],
            actions: [{ name: 'submit', title: 'Save' }],
          },
        });
      }
      if (results.length === 1) {
        const formId = JSON.parse(results[0]) as string;
        return toolCall('wait_for_form_change', { arg1: formId });
      }
      return {
        text: '',
        toolCalls: [
          { id: 'set', name: 'set_result', arguments: { variable, value: 'done' } },
          { id: 'finish', name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
      };
    },
  };
}

describe('form tools inside task/N', () => {
  it('invokes the host interaction for a form opened by a tool', async () => {
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend() });
    const opened: string[] = [];
    const events: DMLEvent[] = [];

    for await (const event of sdk.runDML(DML, {
      onForm: async (form) => {
        opened.push(form.formId);
        sdk!.submitForm(form.formId, { action: 'submit', values: { subject: 'hi' } });
      },
    })) {
      events.push(event);
    }

    // The handler ran from the tool engine, not only the top-level step loop.
    expect(opened).toHaveLength(1);
    expect(events.some((event) => event.type === 'answer' && event.content === 'done')).toBe(true);
  });

  it('renders, waits and resumes when the UI submits', async () => {
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend() });
    const events: DMLEvent[] = [];
    let submittedForm: string | undefined;

    for await (const event of sdk.runDML(DML)) {
      events.push(event);
      if (event.type === 'form' && event.form?.op === 'open') {
        submittedForm = event.form.formId;
        sdk.submitForm(event.form.formId, { action: 'submit', values: { subject: 'hi' } });
      }
    }

    expect(submittedForm).toBeTruthy();
    expect(events.some((event) => event.type === 'form' && event.form?.op === 'open')).toBe(true);
    expect(events.some((event) => event.type === 'answer' && event.content === 'done')).toBe(true);
    // The change reached the model as a tool result.
    const toolResults = events.filter((event) => event.type === 'tool_call').map((event) => event.toolResult);
    expect(JSON.stringify(toolResults)).toContain('submit');
  });
});
