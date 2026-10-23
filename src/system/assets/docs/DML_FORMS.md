# Forms: structured human input

Form primitives are built in; the old activation gate was removed. No capability
directive/import is needed. Availability still requires a host: a foreground
query/run or Work foreground workflow with an interactive UI. Browser scratch
execute workers and worker-hosted processes cannot render forms. Do not infer
availability from `current_predicate(form/2)`; it is a handler, not a normal clause.

## Fixed forms

Known fields need no model. Use `form/2`, await an accepted action, then close:

```prolog
claim_form(Values) :-
    form(_{title: "Claim", fields: [_{name: vendor, type: string, required: true},
                                   _{name: amount, type: number, required: true, minimum: 0}]}, F),
    await_form(F, Change),
    close_form(F),
    ( Change.action == submit -> Change.values = Values ; Values = _{} ).
```

`form(+Spec, -Handle)` opens a form; `form_update(Handle, Patch)` updates it;
`form_errors(Handle, Issues)` displays issues; `await_form(Handle, Change)` waits;
`await_form(Handle, Change, TimeoutMs)` adds a timeout; `close_form(Handle)` closes.
Close the form before committing any subsequent long-running work or returning.
On cancel/unavailable, do not save a record or invent field values. A reusable
workflow binds outputs and returns rather than calling answer/1.

## Dynamic forms

If fields depend on the request, let a scoped task design them:

```prolog
collect_details(Context, Summary) :-
    with_tools([render_form, wait_for_form_change, close_form],
        task("Collect exactly what this case needs; ask the person with a form, wait for their action, close the form, and report only entered values. Context: {Context}",
             string(Summary))).
```

Model tools: `render_form(spec)`, `update_form(formId, patch)`,
`set_form_issues(formId, issues)`, `wait_for_form_change(formId)`,
`validate_form`, `advisories`, `close_form(formId)`.
Internal result/finish tools belong to task execution; do not write wrappers.
The model must wait for actual user input rather than claiming a submission.
Custom DML chat loops may keep a turn waiting implicitly, but normal Work assistants
use shell/1 and invoke a foreground workflow instead of building a custom loop.

## Field and action specification

A spec is an inert dict with `title`, optional `description`, `fields`, optional
`initial`, `actions` and `layout`. Version defaults to 1. Each field has a unique
atom name and type (default string). Common fields:

- string: required, minLength, maxLength, pattern, format; enum list for choices.
- number/integer: minimum, maximum, multipleOf.
- boolean: checkbox/switch presentation.
- object with `properties` or array with `items` for structured values; renderer
  editing support may be more limited than the type permits.

`widget` is presentation, not validation: text, textarea, number, slider, select,
radio, multiselect, checkbox, switch, date/time, hidden. Unknown widget behavior is
renderer-specific; do not promise unimplemented repeaters/tabs.
Actions default to submit/cancel and may have name, title, kind and validation
policy. Keep irreversible domain actions AFTER a validated submit, not hidden in
presentation metadata. Use explicit confirmation where policy requires it.

## Change values and cancellation

DML `Change` contains formId, revision, action, values, changed and errors.
`Change.action` is an ATOM: submit, cancel, cancelled, closed, unavailable or a
custom action. Model-facing JSON represents the action as a string.
`values` is a dict keyed by field names; read controlled keys with get_dict.
Revisions prevent stale updates. Submit does not mean the user wants every later
irreversible action; domain policy remains the harness's responsibility.

## Validation

The host checks spec constraints and `form_validate(+Values, -Errors)` on submit.
A rejected submit is re-presented with errors; the wait continues. It is NOT
returned as an accepted action. Do not build a second rejected-submit loop.
`form_advisories(+Values, -Warnings)` is soft guidance and never blocks submission.
Validation hooks must be deterministic; compare field value types correctly
(e.g. form string values are not arbitrary Prolog atoms).
A cancel/unavailable path must be separate from saving any values.

## Layout

Fields define data and stable paths; layout is a separate tree referring to those
paths. Common nodes: row, column, grid, box, field, text, divider, spacer.
Unreferenced fields still render; layout must not rename data paths. Only promise
widgets/layouts supported by the actual renderer. UI host implementation/design
history is not part of this author API.

## Host and testing

A host receives form events and submits with session.submitForm(formId, change).
The UI routes by session/run owner; ids may repeat across sessions. Headless
hosts return unavailable/cancel or use a deliberately configured submission;
never treat that as real user data. Abort must close/resolve pending forms.

Test known fields deterministically, wrong/missing values, cancellation, repeated
runs and the foreground workflow path. Pure validator tests can use execute;
an actual browser form cannot be tested inside its disposable execute worker.
See `examples` fixed-form/dynamic-form, and `debugging` execution-contexts.
Historical implementation discussions remain in repository docs/DML_FORMS.md,
not in the runtime contract served to the authoring model.
