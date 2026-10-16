# Forms: making the authoring agent aware (SDK fix instructions)

> **Status: done.** All six items below are implemented (see the commit that
> added `tests/forms-discoverability.test.ts`): the bundled `DML_FORMS.md` and a
> `reference("forms")` topic, a `reference("capabilities")` topic plus a
> `- capabilities:` manifest line, a Forms section in `DML_REFERENCE.md` (and so
> in `AGENT_PROMPT.md`, which is generated from it), a validated example in
> `HARNESS_EXAMPLES.md`, capability errors surfaced as tool results, and tests.
> One thing the instructions did not name, found while testing the deterministic
> loop: the host sent the change's `action` as a JSON **string**, so
> `Change.action == submit` (the idiom in `DML_FORMS.md` §3) was silently always
> false. Fixed with `form_change_atom_action/2`.

**Symptom.** In a live REPL the authoring agent does not know forms exist:

- `current_predicate(form/2)` → `no` (so it concludes "forms aren't available"),
- `reference("language" | "examples" | "patterns" | …)` returns nothing about forms,
- it burns turns grepping the workspace and probing.

**Diagnosis (two independent causes, both in the SDK).**

1. **Undocumented for the agent.** The forms subsystem is implemented —
   `src/forms/{spec,registry}.ts`, the MI primitives (`deepclause_mi.pl` §5067
   `form/2`, `form_update/2`, `form_errors/2`, `await_form/2,3`, `close_form/1`;
   `require_capability(forms)` at §5107), the model tools, host-enforced hard
   validation, the TUI panel — and there is a design doc `docs/DML_FORMS.md`.
   But **nothing in `src/system/assets/docs/` mentions forms**, and only those
   files are bundled and served by the `reference` tool
   (`src/repl/session.ts` `reference()`). So the agent can never discover it.
2. **The probe is misleading.** `form/2` is an **interpreter handler**, not a
   module clause — exactly like `exec/2`. `current_predicate(exec/2)` is also
   `no` in a working session. The agent has no reason to know that, so it
   concludes the opposite of the truth. There is no way to list the *gated
   primitives* or *capabilities*.

A third, structural point the agent must be told: **the capability is
file-scoped.** `:- capability(forms).` is a directive processed at load
(`process_directive/2`), so forms work in a harness **loaded from a file** (or a
runner), not in a module that was only built through REPL tools. Without the
directive, `form/2`… `close_form/1` throw "the `forms` capability is not
enabled".

---

## 1. Serve `DML_FORMS.md` through the `reference` tool

`DML_FORMS.md` lives at `docs/DML_FORMS.md`; the agent-facing docs live in
`src/system/assets/docs/`. Bundle the former and add a topic.

1. Copy the doc: `cp docs/DML_FORMS.md src/system/assets/docs/DML_FORMS.md`.
   (`package.json` `copy:assets` copies `src/system/assets/docs/*.md`, so it
   ships automatically — no script change.)
2. `src/system/assets/index.ts`
   - add `'dml-forms'` to the `WorkspaceDocAssetName` union;
   - add `case 'dml-forms': return 'DML_FORMS.md';` to `getWorkspaceDocFileName`.
3. `src/repl/session.ts`
   - widen `loadDocAsset`'s parameter union (≈ line 1308) to include
     `'dml-forms'`, and cache it like the others;
   - in `reference()` (≈ line 2100) add:
     ```ts
     case 'forms':
       return this.loadDocAsset('dml-forms');
     ```
   - add `forms` to the "unknown reference topic" message.
4. `src/repl/harness-tools.ts` — `tools.set('reference', …)` (≈ line 365): add
   `forms` to the description and to the `topic` property's help text.

## 2. Add a short Forms section to the prompt and the language reference

Add the same ~15-line block to `src/system/assets/docs/AGENT_PROMPT.md` (as a
paragraph in the "DML is Prolog extended with…" preamble) and to
`DML_REFERENCE.md` (near `exec/2`, since both are host handlers). Suggested text:

```markdown
### Forms (structured human input) — opt-in

A harness can ask the user a structured question and act on the answer. Forms
are **off by default**; a file must declare the capability at the top:

    :- capability(forms).

Then, in DML:

    Spec = _{ version: 1, id: "refund", title: "Refund request",
              description: "Review the charge.",
              fields: [ _{name: "amount", type: "number", title: "Amount", required: true},
                        _{name: "reason", type: "string", widget: "select", required: true,
                          enum: ["duplicate_charge", "not_received", "other"]},
                        _{name: "notes",  type: "string", widget: "textarea", maxLength: 500} ],
              actions: [ _{name: "submit", title: "Issue refund", kind: "primary"},
                         _{name: "cancel", title: "Cancel", kind: "link"} ] },
    form(Spec, F),                       % render; F is the form handle
    await_form(F, Change),               % blocks until the user acts
    close_form(F),
    (   get_dict(action, Change, "submit")
    ->  get_dict(values, Change, Values), ...   % Values is a dict keyed by field name
    ;   ...                                     % action is a string: "submit" | "cancel"
    ).

Primitives: `form/2`, `form_update/2`, `form_errors/2`, `await_form/2,3`,
`close_form/1`. Field `type` is one of `string | number | integer | boolean`
(plus `object`/`array` groups); `widget` is a presentation hint
(`text | textarea | number | slider | select | radio | multiselect | checkbox |
switch | date | time | datetime | hidden`); `enum` requires `string` fields;
`required` is enforced by the host on submit. Full reference:
`reference("forms")`.

Notes for the authoring agent:
- `form/2` and friends are **handlers, not clauses** — `current_predicate/1`
  reports `no` for them (as for `exec/2`); do not use it to test availability.
- The directive is **file-scoped**: forms work in a harness loaded from a file
  or run as a runner. A module assembled only via REPL tools has no place for a
  directive, so add the file (or export and run it) instead.
```

## 3. Make capabilities and handlers discoverable

Pick at least one; the probe problem (cause 2) will recur for every gated
feature (`forms`, future `http`/`sqlite`/`charts`).

- **Reference topic** — add `capabilities` to `reference()` returning a short
  list of host primitives (handlers + tools) and the capabilities that gate
  them, with the note that handlers don't appear in `current_predicate/1`.
- **Listing/manifest line** — in the manifest the agent sees each turn, add a
  `- capabilities: forms (disabled)` / `(enabled)` line derived from
  `repl_capabilities/2` (the MI already exposes it), so the agent sees the
  feature exists even before enabling it.
- **`reference("tools")`** — it returns `hostToolsCatalog`; mention that `exec/2`
  and other handlers share that mechanism but are not in the catalog.

## 4. Make the failure self-explanatory

`require_capability/1` already throws a precise message
("add `:- capability(forms).`"). Confirm it is surfaced as the *tool result* the
authoring agent sees (not swallowed), so a probe that calls `form/2` yields the
instruction rather than a bare failure. A test should assert the message text.

## 5. A bundled example (the agent learns from `reference("examples")`)

Add one complete forms harness to `HARNESS_EXAMPLES.md` (and keep `forms.dml`
in `sdk-examples/`), showing: the directive, a select + textarea, submit/cancel
actions, and handling `action: invalid` after host validation. This is the
fastest path to the agent producing valid specs.

## 6. Tests

- `reference('forms')` returns the bundled `DML_FORMS.md` (non-empty, contains
  `capability(forms)`).
- `AGENT_PROMPT.md` contains `capability(forms)` (guards against regressions).
- A harness with the directive can `form/2` + `await_form/2`; one without it
  fails with the capability message.

---

## One-line summary for a PR

> Forms are implemented but invisible to the authoring agent: bundle
> `DML_FORMS.md` and expose a `forms` reference topic, document the
> `:- capability(forms).` opt-in (and that `form/2` is a handler, not a clause)
> in `AGENT_PROMPT.md`/`DML_REFERENCE.md`, and surface enabled capabilities in
> the manifest.

## Note on the demo skill that failed to load

The syntax error (`syntax_error(operator_expected)`) came from the hand-written
`.dml` example, not from the runtime; once
`reference("forms")` exists, an example in `HARNESS_EXAMPLES.md` should be
validated by a smoke test so the agent copies a working shape.
