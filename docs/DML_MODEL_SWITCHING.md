# Switching models during DML execution (`with_model`)

Status: **implemented** (Phases 1–5). See the decisions table below; the runner
resolves the scope, the pi adapter honors `LLMBackendRequest.target`, and the
`run` and REPL paths both install a resolver.

### Decisions locked (from review)

1. **Spec grammar:** both a **string** (with and without a reasoning suffix) and a
   `model{…}` **dict**.
2. **Validation:** the **simpler first-call** check (no scope-entry host call).
3. **Backend:** extend **`LLMBackendRequest.target`**; the pi adapter resolves the
   model per call (recommended option).
4. **Compaction:** host summarization stays on the **session** model; only nested
   thresholds follow the scope.
5. **Judgments are _not_ affected by `with_model`** — they keep their own judge
   selection. (No `with_judge_model` for now.)
6. **`task/N` semantics are unchanged** — no per-call model options; `with_model`
   only scopes a dynamic extent.

Goal: let a DML harness choose **which model** (and reasoning level) an individual
LLM call uses, so we can build harnesses optimized for a specific model and
orchestrate work across several models from one program.

```prolog
triage(Message, Verdict) :-
    with_model("openai:gpt-4o-mini",
        prompt("Classify as billing/orders/account: {Message}", Verdict)).

research(Question, Report) :-
    with_model("anthropic:claude-sonnet-4",
        task("Research {Question}; write a sourced brief.", Report)).

code_review(Diff, Notes) :-
    with_model("google:gemini-2.5-pro",
        with_reasoning(high,
            task("Review this diff and list defects:\n{Diff}", Notes))).
```

---

## 1. Motivation & use cases

| # | Use case | Needs |
|---|----------|-------|
| 1 | **Model-specialized harness** — prompts/tools tuned to one model | set the model for a whole skill |
| 2 | **Router / cascade** — cheap model for extraction, strong model only when needed | set the model per sub-call |
| 3 | **Cross-model orchestration** — planner A, workers B, verifier C | nested scopes |
| 4 | **Reasoning-level control per call** | compose with `with_reasoning/2` |
| 5 | **Ensembles / A-B within a run** — N models, vote/compare | set model per branch |
| 6 | **Local + remote mix** | provider selection |
| 7 | **Evaluation harnesses** — measure a harness across models | per-model usage |

The current model is a **session-level** setting (`repl.model` / `--model` /
`/model`). `with_model` makes it a **per-call** setting scoped by the program.

---

## 2. Existing mechanism we reuse: `with_reasoning/2`

`with_reasoning/2` already implements precisely this shape, so `with_model/2` is
"the same mechanism with a richer value":

- **Prolog** (`deepclause_mi.pl`): `mi_call(with_reasoning(Goal, Effort))` stores
  `reasoning_effort` in the threaded **state dict**, runs `Goal`, then clears the
  key. Helpers: `set_reasoning_effort_state/3`, `get_reasoning_effort/2`,
  `clear_reasoning_effort_state/2`.
- The **request builders** read the scoped value from the state and place it in
  the `engine_yield(...)` payload:
  `get_reasoning_effort(StateIn, E)` → `request_agent_loop(…, E, …)` (line ~3176).
- **`process_engine_result/4`** in Prolog turns the yielded term into a step
  payload (`payload{reasoningEffort: …}`), and the **TypeScript runner** reads
  `rawPayload.reasoningEffort` and applies it to the model call
  (`buildReasoningProviderOptions`, runner.ts ~909–919).
- `with_tools/without_tools/with_recipe/with_judgment` all use the same
  state-scoping pattern, and `transform_task_calls/3` **recurses into arbitrary
  compound terms** (line ~288), so `task/N` inside `with_model(Goal, …)` is still
  transformed without special handling.

`with_model/2` therefore only needs: a new state key, a new `with_model` clause,
a new payload field in each request, and the runner-side model resolution.

---

## 3. Surface & grammar

### 3.1 Accepted forms

```prolog
with_model("provider:model", Goal).              % canonical
with_model("provider/model", Goal).              % '/' accepted
with_model("model", Goal).                       % bare id → resolve provider
with_model("provider:model#reasoning", Goal).    % reasoning in the string, e.g. "openai:gpt-4o#high"
with_model("model#reasoning", Goal).             % bare id + reasoning, e.g. "gpt-4o#low"
with_model(model{provider: "google", id: "gemini-2.5-pro",
                 temperature: 0.2, maxTokens: 8192, reasoning: high}, Goal).
```

`#reasoning` is optional. Reasoning levels are `off|minimal|low|medium|high|
xhigh|max` (same set as `with_reasoning/2`).

Dict keys (all optional except `id`):

| key | type | meaning |
|-----|------|---------|
| `provider` | atom/string | provider id (`openai`, `anthropic`, …) |
| `id` | atom/string | model id |
| `temperature` | number | per-scope temperature override |
| `maxTokens` | integer | per-scope max output tokens |
| `reasoning` | atom | default reasoning effort for the scope (like `with_reasoning`) |

### 3.2 Parsing rules

- Strings are split once for reasoning on the **last `#`** (if present); the
  remainder is split once for provider/model on the first `:` or `/`.
- `provider:model` → provider + id. No separator → bare id (provider resolved at
  first call; ambiguity is an error).
- `model{…}` dicts normalize to `{provider?, id, temperature?, maxTokens?,
  reasoning?}`.
- `default` (or an empty spec) clears the scope → the session model.
- Reasoning precedence: an **inner `with_reasoning/2` overrides** the model
  spec's `reasoning` for its extent; otherwise the spec's reasoning is the scope
  default.

### 3.3 Scope

- Affects every **chat** LLM call in the dynamic extent of `Goal`: `task/N`,
  `prompt/N`, and `sample_token/2,3`.
- **Not** affected: the authoring agent's own calls, host-owned
  compaction/summarization, **judgments** (`judge/choose/rate/verify/probability`
  keep their own backend selection), `gen_image` (image model), `exec` (no model).
- **Nesting:** innermost `with_model` wins; sibling scopes don't leak.
- **Inheritance:** nested `task/N` runs under the same state → inherits the scope.
- **Composition with `with_reasoning`:** independent; the effort is applied via
  the scoped model's reasoning mapping. If the scoped model does not support
  reasoning, the effort is ignored with a debug log.

---

## 4. Prolog design

All in `src/prolog-src/deepclause_mi.pl`.

### 4.1 State helpers

```prolog
%% Mirrors set/get/clear_reasoning_effort_state.
set_model_state(StateIn, Spec, StateOut)    :- StateOut = StateIn.put(model_spec, Spec).
get_model_spec(State, Spec)                 :- ( get_dict(model_spec, State, Spec) -> true ; Spec = default ).
clear_model_state(StateIn, StateOut)        :-
    ( get_dict(model_spec, StateIn, _) -> del_dict(model_spec, StateIn, _, StateOut) ; StateOut = StateIn ).
```

### 4.2 The scope predicate

```prolog
%% mi_call(with_model(Goal, Spec), +StateIn, -StateOut)
mi_call(with_model(Goal, Spec), StateIn, StateOut) :-
    !,
    set_model_state(StateIn, Spec, ScopedState),
    mi_call(Goal, ScopedState, StateWithScope),
    clear_model_state(StateWithScope, StateOut).
```

Add `is_mi_special_predicate(with_model(_,_)).` next to
`is_mi_special_predicate(with_reasoning(_,_))` (line ~4584). Optionally add a
symmetric `transform_task_calls(with_model(Goal, Spec), …)` clause for clarity,
though the generic compound clause already covers it.

### 4.3 Payload plumbing (add `modelSpec` beside `reasoningEffort`)

Read the scope in each request builder and add it to the yielded payload:

| Builder | Location | Change |
|---|---|---|
| `task/N` | `mi_call(task(Desc, …), …)` ~3176 | `get_model_spec(StateIn, MS)` → add `modelSpec: MS` to the `request_agent_loop` payload dict |
| `task/N` (second variant) | ~3560 | same |
| `prompt/N` | `request_llm(Messages)` ~3519 | `request_llm(Messages, ModelSpec)` (keep an arity-1 fallback) |
| `sample_token` | `request_sample_token/2` ~3311 | `request_sample_token(Prompt, Allowed, ModelSpec)` |

Judgments (`request_judgment`) are intentionally **not** threaded with a model
spec (decision 5).

And in `process_engine_result/4`, thread `modelSpec` into the `payload{…}` for
each arity variant, defaulting legacy variants to `none`.

### 4.4 Documentation

- `doc(with_model/2, "…")` and a `DML_REFERENCE.md` entry; the bundled
  `AGENT_PROMPT.md` gets a short "choosing models" subsection.

---

## 5. TypeScript design

### 5.1 Types

```ts
/** Parsed from the Prolog `model_spec` payload. */
type ModelSpec =
  | 'default'
  | { provider?: string; id: string; temperature?: number; maxTokens?: number; reasoning?: string };

/** Fully resolved capability profile for one call. */
interface ResolvedModel {
  provider: string;
  id: string;
  temperature?: number;            // from the spec, else session default
  maxTokens?: number;              // from the spec, else model default
  contextWindow?: number;          // for nested compaction thresholds
  reasoning: boolean;              // model supports reasoning
  reasoningType?: ReasoningType;   // 'effort' | 'budget_tokens' | 'thinking_config' | 'none'
  reasoningBudgetMap?: Record<string, number>;
  supportedThinkingLevels?: string[];
}
```

### 5.2 Resolver

```ts
interface ModelResolver {
  /** Resolve + validate; throws a ModelResolutionError with an actionable message. */
  resolve(spec: ModelSpec): Promise<ResolvedModel>;
}
```

Two implementations, selected by backend kind (same source as chat resolution):

- **`PiModelResolver`** — authoritative for existence **and auth**:
  `ModelRuntime.getModelOfType('chat', provider, id)` to resolve,
  `getAvailableOfType('chat')` to verify the provider is signed in.
- **`EnvModelResolver`** — the bundled `model-database.json`
  (`getAllModels()`), used with the `env` backend.

Caching: `Map<specKey, ResolvedModel>`; models are effectively static per run.
Resolution happens **on first use of the scope** (cheap, and avoids a host
round-trip in `with_model/2`); errors are clear (see §7).

### 5.3 Backend: per-call model target

The **pi adapter is currently bound to one model** at construction and ignores
per-request model options (`registry.streamSimple(model, …)` uses the captured
`model`). Add a per-call target:

```ts
interface LLMBackendRequest {
  // …existing…
  /** Per-call model override (from with_model). */
  target?: ResolvedModel;
}
```

Pi adapter changes:
- Refactor the current `complete` body into `completeWith(model, resolvedThinkingLevel, request)`.
- `adapterFor(target)` resolves the `PiModelLike` via the registry and caches by
  `provider:id` (including the model's own `contextWindow`, `maxTokens`,
  reasoning type, and thinking-level map).
- `complete(request)` = `completeWith(baseModel, baseResolved, request)` when no
  target, else `completeWith(resolved.model, resolved.thinking, request)`.
- Reasoning options on the target use the **target model's** `reasoning` flag and
  supported levels.

Env backend: already builds everything from `modelOptions`, so it simply uses the
scoped `provider`/`model`/`temperature`/`maxTokens`.

**Temperature and unsupported parameters.** The pi adapter **never forwards the
ambient session temperature** (pi itself does not, and some models — e.g.
github-copilot — reject the parameter with a 400). It forwards a temperature only
when a `with_model` scope sets one explicitly *and* the model does not opt out via
`compat.supportsTemperature === false`. The `env` backend honors the scoped
temperature normally.

### 5.4 Runner

The scoped spec arrives in the step payload (no `InternalRunOptions` change). In
each handler, replace the hard-coded `this.options.{provider,model,…}` with the
resolved scope:

| Handler | Location | Change |
|---|---|---|
| agent loop (`task`) | `handleAgentLoop`, modelOptions ~1015 | resolve `payload.modelSpec`; build `modelOptions` and `effectiveProviderOptions` (reasoning) from it; pass to `runAgentLoop` |
| `llm` (`prompt`) | `handleLlm` ~1235 | resolve; use for `generateLlmReply` `modelOptions` + `llmBackend` target |
| `sample_token` | `handleSampleToken` ~1147 | same |

`completeJudgment` is **unchanged** (decision 5).

Reasoning: `buildReasoningProviderOptions(effort, reasoningType, budgetMap)`
already takes the mapping; feed it the **scoped** model's
`reasoningType`/`reasoningBudgetMap` (falling back to `this.options`). If the
scoped model is non-reasoning, skip.

Compaction/context window: `handleAgentLoop` should pass the scoped
`contextWindow` into the nested-task compaction path (today the host callback
`compactMessages` uses the session's `contextWindow`); thread the scoped window
through `onBeforeModelCall`/`applyHostCompaction` so a small-model scope doesn't
inherit a large-model threshold.

Usage: include `usageModel` (the resolved model id) on `usage` events from
`task`/`prompt`/`sample_token`.

### 5.5 Wiring the resolver

`RunnerOptions` gains `modelResolver?: ModelResolver`; `DMLSession` builds one
from the backend/config and passes it through `createHarnessRepl` →
`runnerOptions`. When absent, fall back to `EnvModelResolver`.

---

## 6. Interaction matrix

| Area | Behaviour |
|------|-----------|
| `with_reasoning/2` | composes; effort mapped through the scoped model; dropped if unsupported (debug log) |
| judgments | **not affected** — keep their own backend selection (`with_model` does not change the judge model) |
| budgets (`maxModelCalls`) | every call counts, regardless of model — unchanged |
| image generation | unaffected (image model); `with_model` is chat/vision only |
| compaction | host summarization stays on the session model; **nested** thresholds use the scoped window |
| `execute` scratch | scopes apply inside `execute` like anywhere else |
| nested `task/N` | inherits the scope from the state |
| export/listing | `with_model` is runtime-only; exports keep the calls verbatim |
| usage/status | `usage` events carry the model id; per-model rollup is a later add |

---

## 7. Failure semantics

Resolution errors surface as a normal DML call failure (the goal fails with a
`log`/error), not a crash:

| Case | Message (sketch) |
|------|------------------|
| unknown model | `with_model: unknown model "acme:foo" — try deepclause list-models` |
| provider not authenticated | `with_model: provider "anthropic" is not signed in — run deepclause login` |
| ambiguous bare id | `with_model: "gemini" matches 3 providers (google, google-vertex, openrouter) — use "provider:model"` |
| unsupported reasoning level | debug log; the call proceeds without reasoning |

Validation is **first-call** (decision 3): the first time a scoped model is used,
resolution either succeeds (and is cached) or fails the call with the message
above. There is no scope-entry host round-trip.

---

## 8. Test matrix

**Unit (Prolog + runner), canned backend recording `request.target` per call:**
1. `with_model("provider:model", task(...))` → only the inner call gets the target; an outer call keeps the session model.
2. Nesting: inner `with_model` overrides outer; sibling scopes independent.
3. Dict overrides (`temperature`, `maxTokens`) reach the backend request.
4. `with_model` + `with_reasoning(high, …)`: effort passed for a reasoning model; dropped for a non-reasoning one.
5. Nested `task/N` inside a `with_model` scope inherits it.
6. `prompt/N` and `sample_token` inside a scope use it; a judgement inside the scope does **not** (unchanged).
7. Unknown model → actionable error; unauthenticated → actionable error.
8. `usage` events carry the resolved model id.
9. Compaction threshold inside a scope uses the scoped `contextWindow`.
10. `with_model` outside any call has no effect; `default` clears the scope.

**Integration:** the authoring agent can create, `execute`, and commit a skill
that uses `with_model` (canned backend).

**E2E (slow, optional):** one real harness on two pi models (e.g. a cheap and a
strong model) producing different usage records per model.

---

## 9. Migration & compatibility

- Purely additive: no existing predicate/API changes. `reasoningEffort` handling
  is untouched.
- `request_llm(Messages, ModelSpec)` / `request_sample_token(_,_,ModelSpec)` are
  new arities; keep the old arities in `process_engine_result/4` for one release.
- The env backend and `LocalRuntimeHost` work without the worker work; the
  resolver is backend-agnostic.
- Existing harnesses are unaffected (empty/default spec everywhere).

---

## 10. Phasing & acceptance criteria

| Phase | Deliverable | Done when |
|-------|-------------|-----------|
| **1. Core scope** | `with_model/2` + state helpers + payload plumbing; runner applies `provider/model` for `task` + `prompt` | a canned backend sees the scoped model for inner calls only; tests 1,2,6 pass |
| **2. Backend target** | `LLMBackendRequest.target`; pi adapter per-call resolution + cache; env passthrough | switching models within one run works on the pi backend; test 1 passes with a real backend stub |
| **3. Capabilities** | scoped `temperature`/`maxTokens`, reasoning mapping, nested `contextWindow` | tests 3,4,9 pass |
| **4. Usage** | `usageModel` on all `usage` events from `task`/`prompt`/`sample_token` | test 8 passes |
| **5. Docs & examples** | `DML_REFERENCE.md`, `AGENT_PROMPT.md` guidance, router/cascade examples | docs reviewed |

---

## 11. Resolved decisions

| # | Question | Decision |
|---|----------|----------|
| 1 | Spec syntax | **string (with/without `#reasoning`) + `model{}` dict** |
| 2 | Bare model ids | **allowed**, ambiguity → error |
| 3 | Validation timing | **first call** (simple) |
| 4 | Backend | extend **`LLMBackendRequest.target`**; pi adapter resolves per call |
| 5 | Compaction | session model for summarization; scoped window for nested thresholds |
| 6 | Judgments | **not affected** |
| 7 | Per-call `task(…, Options)` | **not in scope** |

---

## Appendix A — exact touch points

**Prolog (`src/prolog-src/deepclause_mi.pl`)**
- new: `set_model_state/3`, `get_model_spec/2`, `clear_model_state/2`, `mi_call(with_model/2)`.
- read `get_model_spec/2` in: `task/N` (~3176, ~3560), `prompt/N` (~3519), `sample_token` (~3311).
- `process_engine_result/4`: thread `modelSpec` for `request_agent_loop` (all arities), `request_llm`, `request_sample_token`.
- `is_mi_special_predicate(with_model(_,_))` (~4584); optional `transform_task_calls(with_model/2, …)`; `doc(with_model/2, …)`.

**TypeScript**
- `src/types.ts` — `ModelSpec`, `ResolvedModel`, `LLMBackendRequest.target`.
- `src/repl/model-resolver.ts` (new) — `ModelResolver`, `PiModelResolver`, `EnvModelResolver`.
- `src/repl/backend.ts` — pi adapter `adapterFor(target)` + cache; honor `request.target`.
- `src/runner.ts` — `handleAgentLoop` (~1015), `handleLlm` (~1235), `handleSampleToken` (~1147), `completeJudgment` (~1335): resolve the scope and build `modelOptions`/target; scoped reasoning + context window; `usageModel` on usage events.
- `src/repl/index.ts` / `src/repl/session.ts` — build the resolver and pass it in `runnerOptions`.

**Docs**
- `src/system/assets/docs/DML_REFERENCE.md`, `src/system/assets/docs/AGENT_PROMPT.md`, `docs/REPL.md`.

## Appendix B — example harness shapes

```prolog
% Router: cheap triage, strong answer only when needed.
handle(Request, Reply) :-
    with_model("openai:gpt-4o-mini",
        prompt("Is this a simple FAQ? reply simple/detailed.\n{Request}", Complexity)),
    ( Complexity == simple ->
        with_model("openai:gpt-4o-mini", task("Answer: {Request}", Reply))
    ;   with_model("anthropic:claude-sonnet-4", task("Answer carefully: {Request}", Reply))
    ).

% Cascade: cheap extraction, strong reasoning.
extract_and_reason(Text, Out) :-
    with_model("openai:gpt-4o-mini", prompt("Extract the numbers as JSON: {Text}", Facts)),
    with_model("google:gemini-2.5-pro",
        with_reasoning(high,
            task("Reason over these facts and give a conclusion: {Facts}", Out))).
```
