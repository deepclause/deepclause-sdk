/**
 * `advisories/1` — the soft-validation tool. The harness defines
 * `form_advisories/2`; the model calls the tool for advice that never blocks.
 */
import { afterEach, describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, DMLEvent, LLMBackend, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

const DML = `
:- capability(forms).

form_advisories(Values, Advisories) :-
    findall(warning(amount, "an unusually large claim"),
            ( get_dict(amount, Values, A), A > 100 ),
            Advisories).

agent_main :-
    with_tools([render_form, wait_for_form_change, advisories, close_form],
        task("Collect a claim.", string(R))),
    answer("done").
`;

function toolCall(name: string, args: Record<string, unknown>): LLMBackendResponse {
  return { text: '', toolCalls: [{ id: name, name, arguments: args }], usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 } };
}

/** render_form → wait_for_form_change → advisories(values) → finish. */
function backend(): LLMBackend {
  return {
    async complete(request): Promise<LLMBackendResponse> {
      const results = request.messages.filter((m) => m.role === 'tool').map((m) => m.content);
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'R';

      if (results.length === 0) {
        return toolCall('render_form', {
          arg1: { version: 1, title: 'Claim', fields: [{ name: 'amount', type: 'integer' }], actions: [{ name: 'submit', title: 'Go' }] },
        });
      }
      if (results.length === 1) {
        const formId = JSON.parse(results[0]) as string;
        return toolCall('wait_for_form_change', { arg1: formId });
      }
      if (results.length === 2) {
        const change = JSON.parse(results[1]) as { values: Record<string, unknown> };
        return toolCall('advisories', { arg1: change.values });
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

describe('advisories tool', () => {
  it('returns the harness advisories to the model', async () => {
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend() });
    const events: DMLEvent[] = [];

    for await (const event of sdk.runDML(DML)) {
      events.push(event);
      if (event.type === 'form' && event.form?.op === 'open') {
        sdk.submitForm(event.form.formId, { action: 'submit', values: { amount: 500 } });
      }
    }

    const advisoriesCall = events.find((event) => event.type === 'tool_call' && event.toolName === 'advisories' && event.toolState === 'completed');
    expect(JSON.stringify(advisoriesCall?.toolResult)).toContain('unusually large claim');
    // Advisory only: the submit still went through.
    expect(events.some((event) => event.type === 'answer' && event.content === 'done')).toBe(true);
  });
});
