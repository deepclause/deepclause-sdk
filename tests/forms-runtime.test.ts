/**
 * The forms capability, headless: a DML program renders a form, waits, and a
 * scripted consumer submits. No model, no UI.
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
:- capability(forms).

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

describe('forms capability', () => {
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

  it('refuses the primitives without the capability directive', async () => {
    const { events, submitted } = await run(
      'agent_main :- form(_{version: 1, title: "X", fields: [_{name: n, type: string}]}, F), answer(F).',
    );
    expect(submitted).toBe(false);
    const error = events.find((event) => event.type === 'error');
    expect(error?.content).toContain('capability');
    expect(error?.content).toContain('not enabled');
  });

  it('rejects an unknown capability by name', async () => {
    const { events } = await run(':- capability(nonsense).\nagent_main :- answer("ok").');
    expect(events.some((event) => event.type === 'error' && /unknown capability/.test(event.content ?? ''))).toBe(true);
  });
});
