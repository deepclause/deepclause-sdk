# Implementation plan: forms (`:- capability(forms)`)

Status: **in progress.** Design lives in [`DML_FORMS.md`](DML_FORMS.md); this is
how it gets built, in an order where every step is testable before the next one.

Guiding rule: **build the vertical slice that lets us drive a form in the REPL,
then widen.** No phase depends on a later one.

---

## Phase 0 — docs (done)

- `docs/DML_FORMS.md`: representation, layout, protocol, registry, adapters,
  opt-in capability, worked examples (Appendix A).
- `:- capability(forms)` chosen over `use_module(library(...))`.

## Phase 1 — the host core, headless (no capability, no UI)

The smallest thing that can be tested: a form registry driven from `runDML`,
with a scripted "UI" that submits.

- `src/forms/spec.ts` — `FormSpec`, `Field`, `LayoutNode`, `Action`, `FormChange`;
  `applyDefaults`, `validateSpec`, `validateValues`. Pure functions, no deps.
- `src/forms/registry.ts` — `FormRegistry`: `open/update/close`, monotonic
  `revision`, `wait(formId, since)` returning a promise, `submit(formId, change)`
  resolving waiters, `onEvent` sink. Cancellation + timeout.
- `src/forms/errors.ts` — the `SpecError` / `FieldError` shapes.

**Done when:** a unit test opens a form, `wait()`s, `submit()`s, and the change
comes back with the right revision — and a stale `since` is delivered, not lost.

## Phase 2 — the event + `submitForm` transport

- `src/types.ts` — `DMLEvent` gains the `form` variant; `RunOptions` gains
  `onForm?`; `DeepClauseSDK` gains `submitForm`.
- `src/sdk.ts` — wire the registry to `runDML`: turn a yield into a `form` event;
  `submitForm` forwards to the registry.
- Headless policy: a wait with no UI resolves `{ action: 'unavailable' }` (config
  to auto-submit defaults later).

**Done when:** `runDML` that calls the form primitive emits a `form` event, and
the scripted consumer calls `submitForm` and the run resumes.

## Phase 3 — the primitives (MI ↔ runner)

- `deepclause_mi.pl` — `mi_call(form/2)`, `form_update/2`, `form_errors/2`,
  `await_form/2,3`, `close_form/1`; `process_engine_result` clauses for
  `request_form` / `wait_form`; `session_form_result/2` + `post_form_change/2`.
- `src/runner.ts` — handle the two statuses in `stepLoopInner`, resolved from the
  registry; post the result and signal the engine.
- **Same three call sites as `request_proc`:** top-level run, the nested agent
  loop, and `executeToolIsolatedInner` (so a `tool/2` body can call the
  primitives — the same fix class as the policy bypass).

**Done when:** `agent_main :- form(Spec, F), await_form(F, C), answer(C.action).`
runs under `runDML` with a scripted UI, **with no model**.

## Phase 4 — the capability + the model tools

- `deepclause_mi.pl` — `capability_library/1`, `process_directive(capability/1)`,
  `session_capability/2`, `require_capability/1`, `repl_capabilities/2`; add to
  `repl_snapshot_save/restore` and `clear_session`.
- The capability's DML surface: the `tool/2` wrappers (`render_form`,
  `update_form`, `close_form`, `wait_for_form_change`, `validate_form`,
  `advisories`) loaded through `process_clause/2` so `task/N` and `with_tools/2`
  see them.
- `src/repl/session.ts` — `syncCapabilities()` after load/commit/rollback: read
  `repl_capabilities/2`, register/unregister the host side; expose `submitForm`.
- Host enforcement: a `validates: hard` action runs the harness's
  `form_validate/2`; on errors the change is `action: 'invalid'` + `errors`.

**Done when:** a harness with `:- capability(forms).` sees the form tools in
`task/N`, and one *without* it gets the precise "not enabled" error.

## Phase 5 — model-driven end-to-end (the Appendix A shape)

- The two worked examples as `sdk-examples/expense-claim.dml` and
  `sdk-examples/onboarding.dml` (or one file with two entries).
- A test that drives the whole loop with a scripted model: render → wait →
  invalid → update → wait → submit → advisories → finish.

**Done when:** the loop completes headlessly, hard rules block a bad submit, and
`form_advisories/2` reaches the model.

## Phase 6 — the REPL form (live)

- `src/repl/tui/form.ts` — a `FormView` from pi-tui `Box`/`VStack`/`HStack`/
  `Input`/`SelectList`/`Text`: focus ring, values, errors/warnings, actions.
- `ReplEventRenderer` gains `formOpen/formUpdate/formClose`; the TUI renders it
  in the transcript, and an action calls `session.submitForm`.
- JSON mode: `form` events pass through; plain mode: a text summary.
- `/goal` goal mode already routes events, so it works there too.

**Done when:** `deepclause repl -l sdk-examples/expense-claim.dml`, then
`/goal agent_main("...")`, renders a form we can fill, submit, see errors on, and
finish — and the same in `--json`.

---

## Risks / decisions already made

| Risk | Decision |
|---|---|
| Model skips hard validation | Host runs `form_validate/2` on submit; `invalid`, not `submit`, comes back |
| Export duplicates capability tools | Mark them `session_capability_tool/3`; excluded from `export_harness` |
| Capability leaks on a failed turn | `session_capability/2` is transactional (snapshot/rollback/reset) |
| No UI (headless/JSON/CI) | Wait resolves `unavailable`; never hangs |
| Tool-engine path | Handle the yields there too (Phase 3), or tool bodies break silently |
| Subagent-owned forms | v1: single registry on the root session; L2 forwarding deferred |

## Out of scope for v1

Repeaters (`array`), `tabs`, cross-turn surfaces, a web reference renderer,
`onCapability` refusal, per-field live validation.
