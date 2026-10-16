/**
 * The REPL path: a session goal renders a form, the host handler collects values
 * (the wizard the CLI installs), and the run resumes.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent, FormEventPayload } from '../src/index.js';

let workspace: string;
let repl: HarnessRepl | undefined;

const DML = `
:- capability(forms).

probe(Out) :-
    form(_{version: 1, title: "Note",
           fields: [ _{name: subject, type: string, title: "Subject", required: true},
                     _{name: count, type: integer, title: "Count"} ],
           actions: [ _{name: submit, title: "Save"} ]}, F),
    await_form(F, Change),
    get_dict(action, Change, Action),
    get_dict(values, Change, Values),
    get_dict(subject, Values, Subject),
    get_dict(count, Values, Count),
    format(string(Out), "~w:~w:~w", [Action, Subject, Count]).
`;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-forms-repl-'));
});

afterEach(async () => {
  await repl?.dispose();
  repl = undefined;
  await fs.rm(workspace, { recursive: true, force: true });
});

/** Stand-in for the CLI wizard: answer each field in order, then submit. */
function answersFor(scripted: Record<string, string>): {
  handler: (form: FormEventPayload) => Promise<void>;
  fields: string[];
} {
  const fields: string[] = [];
  const handler = async (form: FormEventPayload): Promise<void> => {
    const values: Record<string, unknown> = { ...(form.values ?? {}) };
    for (const field of form.spec?.fields ?? []) {
      if (field.type === 'object') continue;
      fields.push(field.name);
      const answer = scripted[field.name];
      if (answer === undefined) continue;
      values[field.name] = field.type === 'integer' ? Number(answer) : answer;
    }
    repl!.session.submitForm(form.formId, { action: 'submit', values });
  };
  return { handler, fields };
}

describe('forms in the REPL session', () => {
  it('renders a form and resumes with the submitted values', async () => {
    const { handler, fields } = answersFor({ subject: 'hello', count: '3' });
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      execution: 'inline',
      onForm: handler,
    });

    await repl.session.beginTurn();
    await repl.session.loadCode(DML);
    await repl.session.endTurn();

    const events: DMLEvent[] = [];
    for await (const event of repl.query('probe(Out)')) events.push(event);

    expect(fields).toEqual(['subject', 'count']);
    const solution = events.find((event) => event.type === 'solution');
    expect(JSON.stringify(solution?.solution)).toContain('submit:hello:3');
  });

  it('does not offer the form tools without the capability', async () => {
    repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
    await repl.session.beginTurn();
    await repl.session.loadCode('probe :- form(_{version: 1, title: "X", fields: [_{name: n, type: string}]}, F), close_form(F).');
    await repl.session.endTurn();

    const events: DMLEvent[] = [];
    for await (const event of repl.query('probe')) events.push(event);
    expect(events.some((event) => event.type === 'error' && /not enabled/.test(event.content ?? ''))).toBe(true);
  });
});
