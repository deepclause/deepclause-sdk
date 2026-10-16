# Auto-generated forms

A harness can ask the human a **structured question** — a form — and act on the
answer, over several rounds, with the UI living outside the model. The agent
declares a form, waits for a change, calls tools with the updated values, updates
the form, and waits again.

> **Status: implemented.** Forms are always available — there is no directive and
> nothing to enable (§4 records why the original opt-in gate was removed). This
> document is both the reference and the design record.
>
> **Built:** the primitives
> `form/2`, `form_update/2`, `form_errors/2`, `await_form/2,3`, `close_form/1`;
> the model tools `render_form`, `update_form`, `set_form_issues`,
> `wait_for_form_change`, `validate_form`, `advisories`, `close_form`;
> host-enforced hard validation (the spec's own constraints *and* the harness's
> `form_validate/2`) — a refused submit is re-shown by the host and the wait
> continues; soft `form_advisories/2`;
> the `form` event and `submitForm`; a TUI panel and a plain-TTY widget;
> `object`/`array` groups, `enum`, and the `row`/`column`/`grid`/`box` layout.
>
> **Not built:** in-place field navigation in the TUI panel (the plain-TTY widget
> has it); forms owned by a subagent (an L2 worker cannot forward a form to the
> foreground UI); repeaters (`array` of `object` renders but has no add/remove UI);
> `tabs`; cross-turn surfaces.
>
> **Try it:** `reference("examples")` has a complete harness, and
> `sdk-examples/forms.dml` runs with no model (`deepclause repl -l
> sdk-examples/forms.dml` then `/goal agent_main`).

Forms generalize two things that came first: `media` events (a program declares
host-rendered content — [`DML_MEDIA.md`](DML_MEDIA.md)) and `input/2` (a blocking
host request that suspends an engine and resumes it).

---

## 1. Why

Most "generative UI" streams markup and re-renders. That is fine for a read-out,
but a form is a **state machine with a decision at the end**: the human edits,
presses an action, and the agent must do something with the result. Modelling it
as data the host owns — rather than text the model streams — gives four things:

- **A stable contract.** The agent references fields by name; the presentation
  can change under it without breaking its tools.
- **A place for validation.** Shape is checked before the agent ever sees values.
- **A round trip.** The agent acts between updates, not just re-renders.
- **Renderer independence.** The same form works in a TUI, a web embed, and
  `--json`, because it is data.

## 2. The model

A form is **host-owned data**, not model state and not UI state:

```
Form = { id, revision, spec, values, state }
```

- The **agent** (via tools) or a **DML program** (via primitives) declares and
  updates it, then waits for a change.
- The **UI** renders it and pushes changes back.
- A **change** is `{ formId, revision, action, values, changed }`.

In DML the `action` is an **atom**, so a harness tests it with `==`:

```prolog
await_form(F, Change),
(   Change.action == submit -> ...
;   Change.action == cancel -> ...
).
```

One of `submit`, `cancel`, `closed`, `cancelled`, `timeout`, `unavailable`, or an
action the spec declared. (Under `:>`, a `task/N` output wrapped in a dict, the
change reaches the model as JSON, where the same value is the string `"submit"` —
the atom is a DML-side convenience, applied by `form_change_atom_action/2`.)

**A refused submit never appears here.** The host validates on submit (the spec's
constraints and the harness's `form_validate/2`); when they reject it, the host
attaches the errors to the form and the wait continues — see §9. So the loop above
has no error branch, and a caller only ever sees an action the rules accepted.

The loop:

```
render_form(spec)            → form event; returns { formId, revision }
wait_for_form_change(formId) → blocks
        ▲                          │  user edits, presses "Submit"
        │                          ▼
   ← { action:'submit', values, revision }
   → refund(values)          (any tool: the submit handler)
   → update_form(formId, patch)   re-render results into the form
   → wait_for_form_change(formId) wait again … or finish
```

## 3. Where it lives: primitives first, tools as sugar

The mechanism is a host-side **form registry**. It is reached two ways, and the
primitive is the core — so it works with no model and no `task/N`. None of it is
active by default; §4 is how a harness opts in:

```
                    ┌────────────────────────────────────────────┐
   model-facing     │ tool(render_form/…) · wait_for_form_change │  tool/2 wrappers
                    ├────────────────────────────────────────────┤
   author-facing    │ form/2 · form_update/2 · await_form/2,3    │  mi_call primitives
                    ├────────────────────────────────────────────┤
   host             │ FormRegistry + `form` events + submitForm  │  one mechanism
                    └────────────────────────────────────────────┘
```

The model-facing tools are ordinary DML wrappers:

```prolog
tool(render_form(Spec, FormId), "Open (or replace) a form.") :-
    form(Spec, FormId).

tool(wait_for_form_change(FormId, Change), "Wait for the user to act on a form.") :-
    await_form(FormId, Change).
```

This makes the agent flow a special case of the primitive, and lets a harness
drive a form with **no model at all**:

```prolog
agent_main :-
    form(Spec, F),
    await_form(F, Change),
    (   Change.action == submit
    ->  get_dict(values, Change, V),
        refund(V),
        form_update(F, _{ notes: "Refund issued" }),
        await_form(F, _),
        answer("refunded")
    ;   close_form(F),
        answer("cancelled")
    ).
```

`form/2` is a *display* primitive (like `output/1`); `await_form/2` is the
request. A program may render a form and never await it.

## 4. Activation: none — forms are always available

There is nothing to enable. A harness calls `form/2` and it works, in the REPL, in
`execute`, and in a `deepclause run` file.

This section is the record of why. Forms were first built behind an opt-in
capability directive (`:- capability(forms).`). The gate was **removed**: it cost
more than it bought, because the feature was invisible and the probe for it was
actively misleading.

- **A gated primitive is a handler, not a clause.** `current_predicate(form/2)`
  reports `no` in a session where forms work — exactly as it does for `exec/2`.
  An agent that checks availability the obvious way concludes the opposite of
  the truth, and nothing in the language tells it otherwise.
- **The directive was file-scoped.** It was processed at load, so it applied to a
  harness loaded from a file, not to a module assembled through the edit tools
  (`begin_turn`/`define`). A form could not be used until the author noticed and
  moved to a file — a second, unrelated concept to learn.
- **The gate guarded nothing dangerous.** A form is inert data rendered by the
  host; the operations a model performs *after* reading it are gated by the
  ordinary tool policy. Opting in bought no isolation.

So the `tool/N` wrappers are now **installed into every session** by
`install_builtin_tools/1` (`deepclause_mi.pl`), through the same `process_clause/2`
path an authored tool takes. They land in `session_user_tools/2` /
`session_user_tool_schema/3`, so `task/N` offers them, `with_tools/2` scopes them,
`listing(tools)` shows them, and an export carries their source (a standalone run
of a harness that uses them keeps working). The install is idempotent —
`parse_clauses/2` runs on every load, and a duplicated schema would show the model
the same tool twice.

The primitives themselves (`form/2`, `await_form/2`, …) are `mi_call` handlers and
were always defined; only the gate in front of them was removed.

A file written against the old directive still loads: `capability/1` is a benign
directive, ignored rather than reported as an unhandled one.

---

## 5. The representation

Four rules:

1. **Data, not code.** A spec is inert JSON — no expressions, no handlers, no HTML.
2. **One tree, with the data/presentation split inside each node** (rather than a
   separate schema + UI schema, which duplicates and is tedious for a model).
3. **`type` ≠ `widget`.** Validation reads only the data half; `widget` is a hint.
4. **JSON-native.** The same object is the TypeScript type, the wire format, and
   (via a JSON Schema) what the model is constrained to produce.

### 5.1 Fields

```ts
interface FieldBase {
  name: string;             // unique within its parent — the contract
  type: FieldType;          // data type (drives validation)
  required?: boolean;
  default?: Json;
  title?: string;           // human label
  description?: string;     // help text
  widget?: Widget;          // presentation hint only
}
```

Data types, grouped by **type** rather than by widget:

```ts
{ type: 'string',  minLength?, maxLength?, pattern?, format?: 'date'|'time'|'datetime'|'email'|'uri'|'media' }
{ type: 'number' | 'integer', minimum?, maximum?, multipleOf? }
{ type: 'boolean' }
{ type: 'string', enum: string[], options?: { value: string; title?: string; description?: string }[] }
{ type: 'object', properties: Field[] }                                  // a group
{ type: 'array', items: Field, minItems?, maxItems?, uniqueItems? }      // a repeater
{ type: 'null', widget: 'hidden' }
```

`format: 'media'` aligns with the media model: a file field's value is a
workspace `media{path, mime}` reference, not bytes.

The `widget` vocabulary is renderer-facing and advisory: `text`, `textarea`,
`number`, `slider`, `select`, `radio`, `multiselect`, `checkbox`, `switch`,
`date`, `time`, `datetime`, `hidden`, `tabs`, `fieldset`. A renderer that does not
know one falls back to the default for its type.

### 5.2 Paths

Fields form a tree for structure and values, but the **canonical address is the
path**:

```
customer.name
lines[0].sku
```

Paths are what validation errors, patches, and `changed` refer to. `name` is the
contract; `title`/`widget`/order/layout are presentation and may change freely.

### 5.3 Layout

Layout is a **separate tree over field paths**:

> Moving a field between two columns must never change its path, its value, or
> its validation.

Nesting fields inside columns would silently rename `customer.name` and break the
agent's tools. So `fields` describes the data; `layout` describes the
arrangement.

```ts
type LayoutNode =
  | { kind: 'row';    children: LayoutNode[]; gap?: number; align?: 'start'|'center'|'end'|'stretch'; wrap?: boolean }
  | { kind: 'column'; children: LayoutNode[]; gap?: number }
  | { kind: 'grid';   columns: number; gap?: number; children: LayoutNode[] }
  | { kind: 'box';    title?: string; description?: string; variant?: 'plain'|'card'|'fieldset'; children: LayoutNode[] }
  | { kind: 'tabs';   tabs: { title: string; children: LayoutNode[] }[] }
  | { kind: 'field';  field: string; span?: number; widget?: string; label?: string; help?: string }
  | { kind: 'text';   text: string; variant?: 'body'|'caption'|'heading' }
  | { kind: 'divider' }
  | { kind: 'spacer'; size?: number };

interface FormSpec {
  version: 1;
  id?: string;
  title?: string;
  description?: string;
  fields: Field[];              // the data tree
  layout?: LayoutNode[];        // the presentation tree, over field paths
  initial?: Record<string, Json>;
  actions?: Action[];
  ui?: { submitOnEnter?: boolean; dense?: boolean };
  meta?: Record<string, Json>;  // namespaced, ignored by the protocol
}
```

Choices worth stating:

- **`grid` + `span`, not columns as nesting.** `span` is in grid columns, so
  `columns: 12, span: 6` is CSS-familiar and `columns: 2, span: 2` means full width.
- **`box` subsumes "sections."** A titled, bordered container *is* a section.
- **`text`/`divider`/`spacer`** cover labels and breathing room that aren't fields.
- **No conditions.** The agent re-renders instead; a condition language drags in a
  client/server evaluation mismatch.

**Precedence.** Two rules keep partial layouts safe and avoid a second source of
truth:

1. Fields **not referenced** by the layout render afterwards, in `fields` order,
   in a default single-column flow. Nothing disappears.
2. A `field` leaf may **override presentation only** — `widget`, `label`, `help`,
   `span`. The field's `type`, constraints, and `name` always win. Renderers
   dedupe by path, first occurrence winning.

**Responsive / stacking.** Character grids have no media queries, so the rule is
structural and each renderer applies it:

- A `row` stacks when `availableWidth < sum(child.minWidth)`, or when `wrap: true`.
- A `grid` reduces its column count by the same rule; `span` clamps to it.
- Each leaf has an implicit `minWidth` (a text input ~16, a select ~12) that a
  renderer may override.

The spec stays width-agnostic; only the adapters know about cells and pixels.

### 5.4 Values

Values **mirror the tree** and are JSON:

```json
{ "customer": { "name": "Ana", "vip": true },
  "lines": [ { "sku": "X-1", "qty": 2 } ],
  "reason": "duplicate_charge" }
```

- **Defaults are resolved, not stored.** `applyDefaults(spec, values)` fills
  missing paths deepest-first, so the UI and the agent always get a complete value
  set — "blank" and "never rendered" never become the same question.
- **Coercion is defensive.** `"2"` for an integer is coerced where unambiguous and
  recorded as an error where not; a string is never silently stored in a number.

### 5.5 Actions and changes

```ts
interface Action {
  name: string;                          // 'submit' | 'cancel' | custom
  title: string;
  kind?: 'primary' | 'default' | 'danger' | 'link';
  confirm?: string;
  validates?: 'all' | string[];          // paths checked before the action is allowed
}

interface FormChange {
  formId: string;
  revision: number;                      // monotonic; the race-free cursor
  action: string;                        // submit | cancel | custom | timeout | unavailable | cancelled
  values: Record<string, Json>;          // full values, defaults resolved
  changed?: string[];                    // paths that differ from the previous revision
}
```

Full values, not a delta: forms are small, it is idempotent, and the agent never
replays history. `changed` lets the agent react to "the user just edited the
order lines."

### 5.6 Patching

`update_form(formId, patch)` takes a **JSON Merge Patch** (RFC 7386): simpler than
JSON Patch and sufficient for "fill a computed field / add options." Arrays
replace wholesale. Every apply bumps `revision`.

## 6. Validation

Two functions, deliberately separated, both returning **data** so the same errors
drive UI messages and the tool result the agent sees:

- `validateSpec(spec): SpecError[]` — static: unknown widget, `enum` on a
  non-string, `minimum > maximum`, duplicate path, layout referencing an unknown
  field, `span > columns`, depth/size limits.
- `validateValues(spec, values): FieldError[]` — dynamic: `{ path, code, message }`.

Cross-field rules (`end > start`) are **not** in the spec; that is logic, and the
agent (or a tool) owns it — the same line DML already draws elsewhere.

**Who acts on a rejection.** The host does, and only the host. On a submit it runs
both checks; when they produce errors it attaches them to the form (so the person
filling it in sees what is wrong, next to the value they typed) and keeps the
wait open. `await_form/2` loops internally: it returns the next *accepted* change,
or a cancel. So a harness reads

```prolog
await_form(F, Change),          % Change.action is submit or cancel — never a refusal
```

with no error branch, and a model driving the loop with
`wait_for_form_change/1` likewise never sees a refusal. A caller that wants the
raw errors before submitting can ask: `form_validate/2` (the `validate_form`
tool) runs the same checks without touching the form.

## 7. Protocol

**Host → consumer** is the existing event stream; one new `DMLEvent`:

```ts
{ type: 'form'; formId: string; revision: number;
  op: 'open' | 'update' | 'close';
  form?: FormSpec; patch?: JsonMergePatch; values?: Record<string, Json>;
  issues?: FormIssue[];                       // errors to show against fields
  state?: 'open' | 'submitted' | 'cancelled';
  source?: { name?: string; pid?: string }   // attribution when a subagent owns it
}
```

`spec` travels with an `update` too: a renderer that only receives the update
still needs the fields to draw, and the `issues` to show against them.

**Consumer → host** mirrors `provideInput`:

```ts
sdk.submitForm(formId, { action, values, revision });   // SDK
session.submitForm(formId, { action, values, revision }); // REPL / DMLSession
```

The generator is host→UI; `submitForm` is UI→host. In headless runs (`-g`,
`--json`, CI) the wait resolves `unavailable` by default (configurable to
auto-`submit` with defaults), so a form never hangs a build — the same policy
`input/2` uses for its cancel path.

## 8. Registry and routing

- `FormRegistry`: `Map<formId, { spec, values, revision, waiters, state }>`.
- **Monotonic revisions.** `await_form(FormId, Change, Since?)` returns the first
  change with `revision > Since`, else blocks. A submit that lands *before* the
  wait starts is still delivered; a retried wait cannot double-consume.
- One change resolves all outstanding waiters for that form.
- **Cancellation:** on turn abort, pending waits resolve `{ action: 'cancelled' }`
  and the form closes.
- **Timeout:** `await_form(FormId, Change, TimeoutMs)` fails on timeout.
- **Subagents.** Once a process can render a form, ids must be *routable*, not
  merely unique:

| Mode | Behaviour |
|---|---|
| L1 (inline) | child sessions share the root registry (they are built from the parent), so `submitForm` reaches the waiting child |
| L2 (worker) | the worker emits `form` events to the main like process events, and `submitForm` is proxied back through the capability broker |

Namespace form ids by owner (session/pid, as pids already are) and carry `source`
on the event so the UI shows `⚙ delivery  [form]`.

## 9. Rendering adapters

A capability the host implements, like `setImageSink`:

```ts
interface FormHost {
  open(formId, spec, values): void;
  update(formId, patch, values, revision): void;
  close(formId, reason): void;
}
```

| Layout | TUI (pi-tui) | Web |
|---|---|---|
| `row` | `HStack` (stack → `VStack` when narrow) | `display:flex; flex-wrap:wrap` |
| `column` | `VStack` | `flex-direction:column` |
| `grid` / `span` | rows of `HStack` with computed cell widths | `grid-template-columns:repeat(N,1fr)`; `span` → `grid-column:span K` |
| `box` | `Box` (+ title rule) | `<fieldset>` / `<section>` |
| `tabs` | *no component today* — degrade to stacked boxes (or drive with `SelectList`) | `<details>` / tab strip |
| `text` / `divider` | `Text` / a rule line | `<p>` / `<hr>` |
| `spacer` | `Spacer` | margin |
| `field` | `Input` / `SelectList` / `SettingsList` | `<input>` / `<select>` / … |

pi-tui gives `Box`, `HStack`, `VStack`, `Spacer`, `ScrollView`, `Input`,
`SelectList`, `SettingsList`, `Text` — everything except a tab widget, which is
why `tabs` degrades in v1 rather than introducing a component.

- **Web embedding** renders HTML from the `form` event and POSTs back via
  `submitForm`. Ship a small reference renderer so every embed does not reinvent
  it — that is what makes a protocol useful twice.
- **`--json`** carries the spec as data; a driver decides what to do.
- **Plain / headless** prints a summary and resolves `unavailable`.

## 10. Security and limits

- **No gate.** The surface is installed into every session (§4). A form is
  inert data rendered by the host; the operations a model performs *after*
  reading it are gated by the ordinary tool policy, which is where the real
  boundary sits.
- Specs are **inert data**; renderers sanitize. Rendering is not a capability, so
  there is nothing further to gate once the capability is on.
- **Submitting** leads the agent to call tools, and those already pass through the
  tool policy. A host may also gate the `render_form` tools themselves.
- Field values are untrusted input to the agent — treat like `user/1`.
- Cap field count, options, nesting depth, payload size, and revision count; a
  looping model must not allocate unbounded UI state.

## 11. Worked example

```json
{
  "version": 1,
  "id": "refund",
  "title": "Refund request",
  "description": "Review the charge and decide how to proceed.",
  "fields": [
    { "name": "customer", "type": "object", "properties": [
      { "name": "name",  "type": "string", "title": "Customer" },
      { "name": "email", "type": "string", "format": "email", "title": "Email" }
    ]},
    { "name": "amount", "type": "number", "minimum": 0, "required": true },
    { "name": "reason", "type": "string", "widget": "select",
      "enum": ["duplicate_charge", "not_received", "other"],
      "options": [
        { "value": "duplicate_charge", "title": "Charged twice" },
        { "value": "not_received",     "title": "Never arrived" },
        { "value": "other",            "title": "Other" }
      ], "required": true },
    { "name": "notes", "type": "string", "widget": "textarea", "maxLength": 500 }
  ],
  "initial": { "customer": { "name": "Ana" }, "reason": "duplicate_charge" },
  "layout": [
    { "kind": "box", "title": "Customer", "variant": "fieldset", "children": [
      { "kind": "grid", "columns": 12, "children": [
        { "kind": "field", "field": "customer.name",  "span": 7 },
        { "kind": "field", "field": "customer.email", "span": 5 }
      ]}
    ]},
    { "kind": "row", "gap": 2, "children": [
      { "kind": "field", "field": "amount", "widget": "number" },
      { "kind": "field", "field": "reason", "widget": "select" }
    ]},
    { "kind": "text", "variant": "caption", "text": "Notes are shown to the billing team." },
    { "kind": "field", "field": "notes", "widget": "textarea" }
  ],
  "actions": [
    { "name": "submit",   "title": "Issue refund", "kind": "primary", "validates": "all" },
    { "name": "escalate", "title": "Escalate to billing", "kind": "default" },
    { "name": "cancel",   "title": "Cancel", "kind": "link" }
  ]
}
```

On a wide terminal the customer block is two columns and amount/reason sit side by
side; on a narrow one everything stacks. Same events, same values, same
validation.

## 12. What is built, and what is next

**Built** (the status note at the top is the full list): the primitives
`form/2`, `form_update/2`, `form_errors/2`, `await_form/2,3`, `close_form/1` plus
the built-in `tool/2` wrappers; field types `string`, `number`,
`integer`, `boolean`, plus `object`/`array` groups; `enum`, `required`,
`minLength`/`maxLength`, `minimum`/`maximum`, `pattern`; layout
`row`/`column`/`grid`/`box`/`field`/`text`/`divider`; one active form per session;
`form`/`form_update`/`form_errors`/`await_form`/`close_form` plus the `tool/2`
wrappers; `form` events + `submitForm`; a TUI panel and a plain-TTY widget;
headless `unavailable`; host-enforced hard validation and harness advisories.

**Next:** in-place field navigation in the TUI panel; forms owned by a subagent
(forwarding an L2 worker's form to the foreground UI); repeaters (add/remove UI
for an `array` of `object`); `tabs`; a web reference renderer; cross-turn
**surfaces**; layout-widget overrides beyond `widget`/`label`.

**Out of scope:** conditional layout, computed fields, cross-field validation,
behavior in actions, styling beyond `columns`/`span`/`gap`/`align`.

## 13. Decisions taken

1. **Tools and primitives** — both. The primitive is the core (a form works with
   no model at all); the tools are ordinary `tool/2` wrappers, so the
   model-facing path is a special case rather than a second implementation.
2. **Blocking, within a turn** — `await_form/2` blocks the engine and the turn.
   A form does not outlive its turn; on abort, pending waits resolve
   `{ action: cancelled }`.
3. **Repeaters deferred** — values are genuinely nested only under an `array` of
   `object`, which is also where merge-patch's "arrays replace wholesale" rule
   bites. Declaring one is fine; there is no add/remove UI yet.
4. **Headless** — a missing UI resolves `unavailable` rather than guessing a
   submit. The CLI's `--answer` is the scripted equivalent.
5. **Actions are atoms in DML** — `Change.action == submit`; the host converts at
   the boundary (§2).

---

## Appendix A. Worked examples: the model drives the loop

In these two cases the **loop lives in the model**, inside `task/N`, not in a
Prolog `review_loop/2`. The DML owns policy; the model owns flow.

| | Provided by | |
|---|---|---|
| Loop: when to generate, validate, finish | the model, inside `task/N` | tool calls |
| Form tools | built in | `render_form`, `update_form`, `close_form`, `wait_for_form_change`, `validate_form`, `advisories` |
| **Hard** rules | the harness | `form_validate(Values, Errors)` — Prolog |
| **Soft** rules | the harness | `form_advisories(Values, Warnings)` — Prolog |
| Domain facts | the harness | plain facts |

Two properties make a model-driven loop safe:

1. **Hard validation is host-enforced on submit, and stays in the host.**
   `form_validate/2` runs when a `validates: hard` action fires; if it returns
   errors the host puts them on the form and keeps waiting (`await_form/2` loops —
   §9), so neither the model nor the harness ever receives a `submit` it can act
   on, and neither has to write the re-show loop. "The model decides when to
   validate" cannot mean "the model may skip validation".
2. **`validate_form` / `advisories` are affordances, not the gate.** The model
   calls them for live feedback (`wait_for_form_change` returns `changed`), but
   the authoritative check is the host's.

### A.1 Expense claim (mostly-fixed fields)

```prolog
vendor(acme). vendor(globex). vendor(initech).
category(travel). category(meals). category(supplies). category(software).
currency(eur). currency(usd). currency(gbp).
claim_limit(travel, 2000). claim_limit(meals, 150).
claim_limit(supplies, 500). claim_limit(software, 1000).
today(D) :- get_time(T), format_time(string(D), '%Y-%m-%d', T).

%% HARD — deterministic, host-enforced on submit.
form_validate(Values, Errors) :-
    findall(error(Path, Msg), claim_error(Values, Path, Msg), Errors).

claim_error(V, vendor, "is required") :- \+ get_dict(vendor, V, _).
claim_error(V, vendor, Msg) :-
    get_dict(vendor, V, N), \+ vendor(N),
    format(string(Msg), "unknown vendor: ~w", [N]).
claim_error(V, category, "is required") :- \+ get_dict(category, V, _).
claim_error(V, category, Msg) :-
    get_dict(category, V, C), \+ category(C),
    format(string(Msg), "unknown category: ~w", [C]).
claim_error(V, currency, Msg) :-
    get_dict(currency, V, C), \+ currency(C),
    format(string(Msg), "unsupported currency: ~w", [C]).
claim_error(V, amount, "is required") :- \+ get_dict(amount, V, _).
claim_error(V, amount, "must be positive") :-
    get_dict(amount, V, A), \+ (number(A), A > 0).
claim_error(V, date, "is in the future") :-
    get_dict(date, V, D), today(T), D @> T.
claim_error(V, amount, Msg) :-
    get_dict(category, V, Cat), get_dict(amount, V, A),
    claim_limit(Cat, Limit), A > Limit,
    format(string(Msg), "exceeds the ~w limit for ~w", [Limit, Cat]).
claim_error(V, receipt, "is required over 50") :-
    get_dict(amount, V, A), A > 50,
    ( get_dict(receipt, V, R) -> R == "" ; true ).

%% SOFT — advisory; exposed as the `advisories` tool.
form_advisories(Values, Advisories) :-
    findall(warning(Path, Msg), claim_advisory(Values, Path, Msg), Advisories).

claim_advisory(V, notes, "the note is thin for an amount this size") :-
    get_dict(notes, V, N), atom_length(N, L), L < 15,
    get_dict(amount, V, A), A > 100.
claim_advisory(V, amount, Msg) :-
    get_dict(category, V, C), get_dict(amount, V, A), claim_limit(C, Limit),
    A > Limit * 0.8, A =< Limit,
    format(string(Msg), "within 20%% of the ~w limit for ~w", [Limit, C]).

agent_main(Context) :-
    system(
        "You are a data-entry assistant collecting an expense claim.\n\
         Loop:\n\
         1. Design the form and call render_form (vendor, category, amount, \
            currency, date; add a receipt/notes field when the context implies it).\n\
         2. Call wait_for_form_change. It returns {action, values, changed}.\n\
         3. action=submit: the rules passed. Optionally call advisories(values) \
            and surface anything material before finishing.\n\
         4. action=cancel: close_form and finish.\n\
         A refused submit is handled for you: the host shows the errors on the \
         form and wait_for_form_change keeps waiting, so you never see it.\n\
         Never invent values. End with set_result, then finish."),
    with_tools([render_form, update_form, close_form, wait_for_form_change,
                advisories, set_result, finish],
        task("Collect an expense claim. Context: {Context}", string(Summary))),
    answer(Summary).
```

### A.2 Customer onboarding (context-dependent fields)

Same shape, but the fields depend on the situation: an individual needs name,
email and country; a business also needs a legal name and, in the EU, a VAT id.
The model decides what to render; the hard rules do not bend.

```prolog
country(de). country(fr). country(gb). country(us).
eu(de). eu(fr).
vat_prefix(de, "DE"). vat_prefix(fr, "FR"). vat_prefix(gb, "GB").

%% HARD
form_validate(Values, Errors) :-
    findall(error(Path, Msg), onboard_error(Values, Path, Msg), Errors).

onboard_error(V, type, "is required") :- \+ get_dict(type, V, _).
onboard_error(V, name, "is required") :- \+ get_dict(name, V, _).
onboard_error(V, email, "is required") :- \+ get_dict(email, V, _).
onboard_error(V, email, "is not a valid email") :-
    get_dict(email, V, E), \+ sub_atom(E, _, _, _, "@").
onboard_error(V, country, "is required") :- \+ get_dict(country, V, _).
onboard_error(V, country, Msg) :-
    get_dict(country, V, C), \+ country(C),
    format(string(Msg), "unsupported country: ~w", [C]).
onboard_error(V, vat_id, "is required for an EU business") :-
    get_dict(type, V, business), get_dict(country, V, C), eu(C),
    ( get_dict(vat_id, V, X) -> X == "" ; true ).
onboard_error(V, vat_id, Msg) :-
    get_dict(type, V, business), get_dict(vat_id, V, Vat), Vat \== "",
    get_dict(country, V, C), vat_prefix(C, P),
    \+ sub_atom(Vat, 0, 2, _, P),
    format(string(Msg), "a ~w VAT id begins with ~w", [C, P]).

%% SOFT
form_advisories(Values, Advisories) :-
    findall(warning(Path, Msg), onboard_advisory(Values, Path, Msg), Advisories).

onboard_advisory(V, email, "a free-mail address on a business account") :-
    get_dict(type, V, business), get_dict(email, V, E),
    ( sub_atom(E, _, _, _, "@gmail.com") ; sub_atom(E, _, _, _, "@outlook.com") ).
onboard_advisory(V, name, "an unusually short legal name") :-
    get_dict(type, V, business), get_dict(name, V, N), atom_length(N, L), L < 4.

agent_main(Context) :-
    system(
        "You onboard a customer. What the form needs depends on the context: an \
         individual needs name, email and country; a business also needs a legal \
         name and, in the EU, a VAT id.\n\
         Loop with render_form -> wait_for_form_change. The host enforces the \
         rules on submit: a refused submit is shown on the form and the wait \
         continues, so you only ever see action=submit or cancel. On submit, call \
         advisories(values) and surface anything material before finishing. Offer \
         `country` as a select. \
         Never invent values."),
    with_tools([render_form, update_form, close_form, wait_for_form_change,
                advisories, set_result, finish],
        task("Onboard a customer. Context: {Context}", string(Summary))),
    answer(Summary).
```

Both loops are the model's — `agent_main` contains a *prompt* and a *tool scope*,
not a review loop. The rules are not the model's: `form_validate/2` and
`form_advisories/2` are ordinary Prolog, unit-testable and deterministic, and the
hard half is not skippable because the host runs it before a `submit` is
delivered. Generated and fixed forms are the same mechanism: the model can add or
rename fields, but a core name it reuses (`amount`, `vat_id`) is what the rules
read, so a form cannot be shaped to escape them.

---

**Lineage.** This is recognisably JSON Schema + a UI schema (react-jsonschema-form,
JSON Forms) collapsed into one tree, wired to an agent that *acts* on changes
rather than only re-rendering — which is where it departs from the
streaming-render "generative UI" approach. The `type`/`widget` split,
path-addressed values, merge-patch updates, and a `version` field are the four
decisions that keep it small and evolvable.
