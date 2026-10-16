/**
 * The forms, headless: a DML program renders a form, waits, and a
 * scripted consumer submits. No model, no UI, and no directive — forms are built
 * in.
 */
import { afterEach, describe, expect, it } from 'vitest';

import { createDeepClause } from '../src/index.js';
import type { DeepClauseSDK, DMLEvent } from '../src/index.js';

let sdk: DeepClauseSDK | undefined;

afterEach(async () => {
  await sdk?.dispose();
  sdk = undefined;
});

const WITH_CAPABILITY = `

agent_main :-
    form(_{version: 1, title: "Note",
           fields: [ _{name: subject, type: string},
                     _{name: body, type: string} ],
           actions: [ _{name: submit, title: "Save", kind: primary},
                      _{name: cancel, title: "Cancel"} ]}, F),
    await_form(F, Change),
    get_dict(action, Change, Action),
    get_dict(values, Change, Values),
    get_dict(subject, Values, Subject),
    format(string(Msg), "~w:~w", [Action, Subject]),
    answer(Msg).
`;

/** Drive a run, submitting the first opened form with `action`. */
async function run(
  code: string,
  action = 'submit',
  values: Record<string, unknown> = { subject: 'hello', body: 'world' },
): Promise<{ events: DMLEvent[]; submitted: boolean }> {
  sdk = await createDeepClause({ model: 'test-model' });
  const events: DMLEvent[] = [];
  let submitted = false;
  for await (const event of sdk.runDML(code)) {
    events.push(event);
    if (event.type === 'form' && event.form?.op === 'open' && !submitted) {
      submitted = sdk.submitForm(event.form.formId, { action, values: values as never });
    }
  }
  return { events, submitted };
}

/** Drive a run, submitting attempts in turn: a rejected submit re-shows and waits. */
async function runAttempts(
  code: string,
  attempts: Array<Record<string, unknown>>,
): Promise<{ events: DMLEvent[]; submissions: number }> {
  sdk = await createDeepClause({ model: 'test-model' });
  const events: DMLEvent[] = [];
  let submissions = 0;
  for await (const event of sdk.runDML(code, {
    onForm: async (form) => {
      const attempt = attempts[submissions];
      if (attempt === undefined) return;
      submissions += 1;
      sdk!.submitForm(form.formId, { action: 'submit', values: attempt as never });
    },
  })) {
    events.push(event);
  }
  return { events, submissions };
}

describe('forms', () => {
  it('renders a form and resumes when the UI submits', async () => {
    const { events, submitted } = await run(WITH_CAPABILITY);
    expect(submitted).toBe(true);

    const opened = events.find((event) => event.type === 'form' && event.form?.op === 'open');
    expect(opened?.form?.spec?.title).toBe('Note');
    expect(opened?.form?.values).toEqual({});   // no defaults declared, nothing filled

    expect(events.some((event) => event.type === 'answer' && event.content === 'submit:hello')).toBe(true);
  });

  it('carries the action through', async () => {
    const { events } = await run(WITH_CAPABILITY, 'cancel', { subject: '', body: '' });
    expect(events.some((event) => event.type === 'answer' && event.content === 'cancel:')).toBe(true);
  });

  it('needs no directive: the primitives work in a plain run', async () => {
    // The opt-in gate (`:- capability(forms).`) was removed; this is the guard
    // that it does not come back and silently make forms unavailable again.
    const { events, submitted } = await run(
      'agent_main :- form(_{version: 1, title: "X", fields: [_{name: n, type: string}]}, F), await_form(F, C), answer(C.action).',
    );
    expect(submitted).toBe(true);
    expect(events.some((event) => event.type === 'error')).toBe(false);
  });

  it('ignores a leftover capability directive', async () => {
    // An older file still loads; `capability/1` is benign, not an "unhandled
    // directive" warning the author has to decode.
    const { events } = await run(':- capability(forms).\nagent_main :- answer("ok").');
    expect(events.some((event) => event.type === 'answer' && event.content === 'ok')).toBe(true);
    expect(events.some((event) => event.type === 'error')).toBe(false);
  });
});

const HARD_RULES = `

form_validate(Values, Errors) :-
    findall(error(amount, "must be positive"),
            ( get_dict(amount, Values, A), A =< 0 ),
            Errors).

agent_main :-
    form(_{version: 1, title: "Claim",
           fields: [_{name: amount, type: integer}],
           actions: [_{name: submit, title: "Go"}]}, F),
    await_form(F, Change),
    get_dict(action, Change, Action),
    answer(Action).
`;

describe('host-enforced hard validation', () => {
  it('enforces the declared spec constraints without a form_validate/2', async () => {
    const code = `
agent_main :-
    form(_{version: 1, title: "T",
           fields: [_{name: subject, type: string, required: true}],
           actions: [_{name: submit, title: "Go"}]}, F),
    await_form(F, Change),
    get_dict(action, Change, Action),
    answer(Action).
`;
    // `required` is in the spec: the empty submit is refused by the host, so the
    // harness only ever sees the second (valid) attempt.
    const { events, submissions } = await runAttempts(code, [{}, { subject: 'ok' }]);
    expect(submissions).toBe(2);
    expect(events.some((event) => event.type === 'answer' && event.content === 'submit')).toBe(true);
    expect(events.some((event) => event.type === 'answer' && event.content === 'invalid')).toBe(false);
  });

  it('shows a rejected submit\'s errors on the form, then waits again', async () => {
    const { events, submissions } = await runAttempts(HARD_RULES, [{ amount: -1 }, { amount: 5 }]);
    expect(submissions).toBe(2);
    // The harness never saw `invalid`: only the submit the rules accepted.
    expect(events.some((event) => event.type === 'answer' && event.content === 'submit')).toBe(true);
    expect(events.some((event) => event.type === 'answer' && event.content === 'invalid')).toBe(false);
    // The refusal reached the person filling the form, with the message and the
    // value they typed (so they can correct it).
    const shown = events.find(
      (event) => event.type === 'form' && event.form?.op === 'update' && (event.form.issues?.length ?? 0) > 0,
    );
    expect(shown?.form?.issues?.[0]?.message).toContain('must be positive');
    expect(shown?.form?.values).toEqual({ amount: -1 });
    expect(shown?.form?.spec?.fields?.length).toBe(1);
  });

  it('passes a submit the rules accept', async () => {
    const { events } = await run(HARD_RULES, 'submit', { amount: 5 });
    expect(events.some((event) => event.type === 'answer' && event.content === 'submit')).toBe(true);
  });

  it('never validates a cancel', async () => {
    const { events } = await run(HARD_RULES, 'cancel', { amount: -1 });
    expect(events.some((event) => event.type === 'answer' && event.content === 'cancel')).toBe(true);
  });

  it('terminates when the host cancels a retry instead of answering again', async () => {
    // The CLI's `--answer` mode: it answers once, then cancels a re-presented
    // form. Without that, a refused submit would be re-asked forever.
    sdk = await createDeepClause({ model: 'test-model' });
    const answers: string[] = [];
    let presented = 0;
    for await (const event of sdk.runDML(HARD_RULES, {
      onForm: async (form) => {
        presented += 1;
        sdk!.submitForm(
          form.formId,
          presented === 1
            ? { action: 'submit', values: { amount: -1 } }
            : { action: 'cancel', values: form.values ?? {} },
        );
      },
    })) {
      if (event.type === 'answer' && event.content) answers.push(event.content);
    }
    expect(presented).toBe(2);
    expect(answers).toContain('cancel');
  });
});
