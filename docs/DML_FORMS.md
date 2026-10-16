# Auto-generated forms

A harness can ask the human a **structured question** — a form — and act on the
answer, over several rounds, with the UI living outside the model. The agent
declares a form, waits for a change, calls tools with the updated values, updates
the form, and waits again.

> **Status: design — not implemented.** This is the specification to build
> against; the surface below does not exist yet. It generalizes two things that
> do exist: `media` events (a program declares host-rendered content —
> [`DML_MEDIA.md`](DML_MEDIA.md)) and `input/2` (a blocking host request that
> suspends an engine and resumes it).

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

## 4. Activation: opt-in capability libraries

Nothing above is active by default. A harness opts in once, at the top:

```prolog
:- use_module(library(forms)).
```

`library(forms)` is not a SWI module. It is a **DeepClause capability**: a host
feature a harness declares it needs. The directive is intercepted where
`library(clpfd)` is processed (`process_directive/2` in `deepclause_mi.pl`) and
does three things:

1. **Records it transactionally** as `session_capability(SessionId, forms)`. It
   participates in `repl_snapshot_save/restore`, `rollback`, and `reset`, so a
   failed turn that enabled forms leaves nothing behind — the same discipline as
   `session_user_tools`.
2. **Loads the DML surface through the normal clause path.** The `tool/2`
   wrappers (`render_form`, `wait_for_form_change`, `update_form`, `close_form`)
   and any helpers go through `process_clause/2`, so they land in
   `session_user_tools/2` / `session_user_tool_schema/3` exactly like authored
   tools. `task/N` therefore offers them, `with_tools/2` scopes them, and
   `listing(tools)` shows them — with no special case in the agent loop.
3. **Signals the host to activate its half.** After a load/commit, the session
   reads `repl_capabilities(SessionId, Caps)` and registers the host tools and
   the form host (registry + adapters).

The primitives are always *defined* but **gated**: every `mi_call(form(...))` /
`mi_call(await_form(...))` clause begins with `require_capability(forms)`, which
throws a precise error when the directive is missing:

```
error: the `forms` capability is not enabled:
       add `:- use_module(library(forms)).` to the harness
```

so a call cannot half-work, and a missing directive does not read like a typo.

**Deactivation is the inverse.** `reset`/`rollback` retract
`session_capability/2` and the capability's tool facts; the host re-syncs and
drops the registered tools and the registry. Capability-provided tools are marked
(`session_capability_tool/3`) so `export_harness` does **not** inline them —
otherwise an exported harness that also carries the directive would load them
twice.

### Why a directive, not a config flag

- **The requirement travels with the code.** A harness that needs forms says so;
  its behaviour does not depend on ambient session config.
- **It is transactional.** A turn that adds the directive and then fails rolls it
  back, unlike a flag set once.
- **It is the security story.** Enabling a capability is an explicit act by the
  author, so there is no "forms are on and nobody noticed".

### Refusing a capability

A host may decline one — a headless run with no UI, or a locked-down embedding.
The session exposes `onCapability(name)`; returning false either makes
`await_form` resolve `unavailable` or refuses the load, the host's choice.
Rendering is inert data either way; the tools the agent calls *afterwards* still
pass the normal tool policy.

### Generalization

`forms` is one capability. The mechanism is deliberately generic so `http`,
`sqlite`, `charts`, or a host-specific `approvals` library plug in the same way:
each declares a name, a DML surface (tool declarations + helpers), and a host
surface, and defaults to off. `library(clpfd)` keeps its SWI meaning; only names
the host knows are capabilities.

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

## 7. Protocol

**Host → consumer** is the existing event stream; one new `DMLEvent`:

```ts
{ type: 'form'; formId: string; revision: number;
  op: 'open' | 'update' | 'close';
  form?: FormSpec; patch?: JsonMergePatch; values?: Record<string, Json>;
  state?: 'open' | 'submitted' | 'cancelled';
  source?: { name?: string; pid?: string }   // attribution when a subagent owns it
}
```

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

- **Off by default.** None of the surface exists unless the harness declares
  `:- use_module(library(forms)).` (§4). That opt-in is the primary gate; there is
  no ambient "forms are on".
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

## 12. MVP and later

**MVP:** the `:- use_module(library(forms)).` capability directive (opt-in,
§4); field types `text`/`textarea`/`number`/`boolean`/`select`/`multiselect`;
layout `row`/`column`/`grid`/`box`/`field`/`text`/`divider`; one active form per
session; `form`/`form_update`/`await_form`/`close_form` primitives plus the
`tool/2` wrappers; `form` events + `submitForm`; a basic TUI renderer; JSON and
headless-`unavailable`; tests for registry revisions/waiters/cancel and a
scripted UI driving a full render → wait → submit → update → wait loop.

**Later:** `tabs`/`spacer`, repeaters, per-field validation feedback, a web
reference renderer, cross-turn **surfaces**, and layout-widget overrides beyond
`widget`/`label`.

**Explicitly out:** conditional layout, computed fields, cross-field validation,
behavior in actions, styling beyond `columns`/`span`/`gap`/`align`.

## 13. Open questions

1. **Tools vs. primitives** — ship both (primitive core + wrappers), or tools first?
2. **Blocking vs. persistent** — block within a turn (recommended v1), or let a
   form outlive the turn and have a submit start a new one?
3. **Repeaters in v1** — the only construct that makes values genuinely nested, and
   the only place merge-patch's "arrays replace wholesale" rule bites.
4. **Headless default** — `unavailable` or auto-`submit` with defaults?
5. **Capability spelling** — `use_module(library(forms))` (idiomatic, matches
   `library(clpfd)`) or a DeepClause-native `:- capability(forms).` that does not
   pretend to be a SWI module?

---

**Lineage.** This is recognisably JSON Schema + a UI schema (react-jsonschema-form,
JSON Forms) collapsed into one tree, wired to an agent that *acts* on changes
rather than only re-rendering — which is where it departs from the
streaming-render "generative UI" approach. The `type`/`widget` split,
path-addressed values, merge-patch updates, and a `version` field are the four
decisions that keep it small and evolvable.
