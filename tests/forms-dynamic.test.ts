/**
 * Two ways a harness gets a form, both required:
 *  - a static spec in Prolog (deterministic: no model, no task/N), and
 *  - a spec produced at runtime by `task/N` (the shape a model designs).
 */
import { afterEach, describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, DMLEvent, LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

const SPEC = {
  version: 1,
  title: 'Generated intake',
  fields: [{ name: 'subject', type: 'string', required: true }],
  actions: [{ name: 'submit', title: 'Send' }],
};

const FROM_TASK = `

agent_main :-
    task("Design a one-field intake form and return the spec.", object(Spec)),
    form(Spec, F),
    await_form(F, Change),
    get_dict(action, Change, Action),
    close_form(F),
    answer(Action).
`;

/** The model answers the typed output with a spec object, then finishes. */
function backend(value: unknown): LLMBackend {
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'Spec';
      return {
        text: '',
        toolCalls: [
          { id: 'set', name: 'set_result', arguments: { variable, value } },
          { id: 'finish', name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 1, outputTokens: 1, totalTokens: 2 },
      };
    },
  };
}

async function run(code: string, values: Record<string, unknown>): Promise<DMLEvent[]> {
  const events: DMLEvent[] = [];
  for await (const event of sdk!.runDML(code)) {
    events.push(event);
    if (event.type === 'form' && event.form?.op === 'open') {
      sdk!.submitForm(event.form.formId, { action: 'submit', values: values as never });
    }
  }
  return events;
}

describe('forms created by task/N', () => {
  it('renders, validates and resumes a spec the model returned', async () => {
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend(SPEC) });

    const events = await run(FROM_TASK, { subject: 'hello' });

    const opened = events.find((event) => event.type === 'form' && event.form?.op === 'open');
    expect(opened?.form?.spec?.title).toBe('Generated intake');
    expect(opened?.form?.spec?.fields).toHaveLength(1);
    expect(events.some((event) => event.type === 'answer' && event.content === 'submit')).toBe(true);
  });

  it('enforces the model-authored spec\'s own constraints', async () => {
    sdk = await createDeepClause({ model: 'test-model', llmBackend: backend(SPEC) });

    // `subject` is required in the *generated* spec: the host rejects an empty submit.
    const events = await run(FROM_TASK, { subject: '' });
    expect(events.some((event) => event.type === 'answer' && event.content === 'invalid')).toBe(true);
  });
});

const DETERMINISTIC_LOOP = `

form_validate(Values, Errors) :-
    findall(error(amount, "must be positive"),
            ( get_dict(amount, Values, A), A =< 0 ),
            Errors).

agent_main :-
    form(_{version: 1, title: "Claim",
           fields: [_{name: amount, type: integer}],
           actions: [_{name: submit, title: "Go"}]}, F),
    loop(F).

loop(F) :-
    await_form(F, Change),
    get_dict(action, Change, Action),
    (   Action == invalid
    ->  get_dict(errors, Change, Errors),
        form_errors(F, Errors),
        loop(F)
    ;   close_form(F),
        answer(Action)
    ).
`;

describe('forms driven by a Prolog loop (no model)', () => {
  it('re-renders on invalid, then accepts', async () => {
    sdk = await createDeepClause({ model: 'test-model' });
    const events: DMLEvent[] = [];
    const opens: string[] = [];
    const answers: string[] = [];
    let submits = 0;

    for await (const event of sdk.runDML(DETERMINISTIC_LOOP)) {
      events.push(event);
      if (event.type === 'answer' && event.content) answers.push(event.content);
      if (event.type === 'form' && event.form?.op === 'open') opens.push(event.form.formId);
      if (event.type === 'form' && (event.form?.op === 'open' || event.form?.op === 'update')) {
        submits += 1;
        // First submit is invalid, the second is accepted.
        sdk.submitForm(event.form.formId, {
          action: 'submit',
          values: { amount: submits === 1 ? -1 : 5 } as never,
        });
      }
    }

    expect(answers).toContain('submit');
    expect(events.some((event) => event.type === 'form' && event.form?.op === 'update')).toBe(true);
  });
});
