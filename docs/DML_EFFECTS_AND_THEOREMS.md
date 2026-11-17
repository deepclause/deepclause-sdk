# DML Effects, Obligations, and Theorems

Status: **design draft — not implemented.**

This document defines a minimal, DML-native verification layer:

- effects are **formal declarations**;
- preconditions are code guards or `requires/2` facts;
- the user/system supplies protected **obligations** the model must prove;
- the model supplies **theorems** about properties it believes matter;
- the verifier proves or refutes both;
- `define` accepts code only after verification.

No new syntax. No `tool/4`. No `where` blocks. No dicts or contract lists.

## 1. Goals and non-goals

### Goals

- Let a model generate DML plus a small set of formal declarations.
- Let the model run `verify` in a loop until the verifier accepts.
- Let the user/system define must-prove conditions.
- Let the model propose additional theorems for user review.
- Accept code into the REPL module only when verification succeeds.
- Keep effects and theorems pure, declarative, and analysable.

### Non-goals

- Proving model correctness or calibration.
- Proving arbitrary natural-language prompt semantics.
- A full theorem prover or a new language.
- Runtime enforcement inside `effect/2`; runtime guards are separate.
- Silently accepting unsupported claims.

## 2. Formal layer vs runtime layer

| Layer | Purpose | Examples |
|---|---|---|
| **Formal declarations** | Analysis and proof | `effect/2`, `requires/2`, `ensures/2`, `obligation/2`, `theorem/2` |
| **Program code** | Execution | predicates, `tool/2`, `tool/3`, `task/N`, `exec/2` |
| **Runtime enforcement** | Guards and policy | code guards, tool engine, MI precondition/approval checks |
| **Verification** | Proof or counterexample | `verify/1`, tool/effect tree, CLP/SMT conditions |
| **Acceptance** | Commit gate | `define/2`, `end_turn` |

Effects are **not runtime actions**. They are facts used by the verifier. A runtime guard is a separate mechanism.

## 3. Minimal primitives

| Primitive | Written by | Meaning |
|---|---|---|
| `effect(Call, Effect)` | model | Formal effect declaration: the call may produce `Effect`. |
| `requires(Call, Condition)` | model or trusted manifest | Formal precondition. Required for opaque/host calls; optional when the DML code has visible guards. |
| `obligation(Name, Condition)` | **protected spec** | Must-prove condition. The model cannot edit it. |
| `theorem(Name, Condition)` | model | Proposed property. The verifier proves or refutes it. |
| `tool(Head, Desc, Types)` | model | Existing model-facing tool annotation. |
| `verify(Report)` | model tool | Run the verifier on the current candidate. |
| `define(Head, Clauses)` | model tool | Commit only if verification accepts. |

Optional metadata, only when needed:

```prolog
ensures(Call, Condition).
reads(Call, Resource).
writes(Call, Resource).
capability(Call, Capability).
approval(Call, Mode).
idempotent(Call, Key).
effect_class(Effect, Class).
```

`effect_class/2` is normally predefined, not model-written.

## 4. Declarations

### 4.1 `effect/2`

```prolog
effect(issue_refund(ClaimId, Amount),
       financial(refund(ClaimId, Amount))).
```

- A fact.
- No body. No side effects. No runtime semantics.
- Multiple facts for the same call are alternatives.
- No `effect/2` means no declared effect.
- The verifier compares declared effects with effects inferred from the body and from called predicates.

### 4.2 `requires/2` and `ensures/2`

For visible DML code, the code guards are the precondition:

```prolog
tool(issue_refund(ClaimId, Amount),
     "Issue a refund after verification.",
     [claim_id, number]) :-
    claim_verified(ClaimId),
    claim_status(ClaimId, open),
    Amount > 0,
    Amount =< 1000,
    issue_refund_impl(ClaimId, Amount).

effect(issue_refund(ClaimId, Amount),
       financial(refund(ClaimId, Amount))).
```

For an opaque host tool, use formal declarations:

```prolog
host_tool(email_send(Recipient, Subject, Body, Token)).

effect(email_send(Recipient, Subject, Body, Token),
       external(email(Recipient))).

requires(email_send(Recipient, Subject, Body, Token),
         (allowlisted(Recipient),
          body_labels(Body, Labels),
          subset(Labels, [public, internal]),
          release_token(Token, Hash, Recipient),
          hash(Body, Hash))).

ensures(email_send(Recipient, Subject, Body, Token),
        email_sent(Recipient)).
```

- `requires/2` is a formal condition, not an executable body.
- The tool engine may use it as a runtime guard for opaque tools, but that is a separate policy decision.
- Host tool contracts come from a trusted manifest; generated code cannot weaken them.

### 4.3 `obligation/2`

Protected spec module; the model cannot edit it:

```prolog
obligation(no_unverified_refund,
    forall(ClaimId,
        (may_effect(financial(refund(ClaimId, _))) ->
         previously(effect(claim_verified(ClaimId)))))).

obligation(no_over_limit_refund,
    forall(Amount,
        (may_effect(financial(refund(_, Amount))) -> Amount =< 1000))).

obligation(no_closed_claim_refund,
    forall(ClaimId,
        (may_effect(financial(refund(ClaimId, _))) ->
         claim_status(ClaimId, open)))).
```

- Obligations are must-prove.
- They are quantified over inputs/state; adding a convenient fact does not prove them.
- The model satisfies them by adding guards, contracts, fallbacks, or by changing the program.

### 4.4 `theorem/2`

Model-proposed properties:

```prolog
theorem(bounded_refund,
    forall(ClaimId, Amount,
        (may_effect(financial(refund(ClaimId, Amount))) ->
         (claim_verified(ClaimId),
          claim_status(ClaimId, open),
          Amount =< 1000)))).

theorem(refund_path_reachable,
    exists_path(effect(financial(refund(ClaimId, Amount))),
                [claim_verified(ClaimId),
                 claim_status(ClaimId, open),
                 Amount =< 1000])).
```

- The model proposes theorems; the verifier reports `proved`, `counterexample`, `unknown`, `trivial`, or `unsupported`.
- The user sees the theorems and their statuses.
- A theorem is not a license to weaken an obligation or a host contract.

## 5. Theorem vocabulary

The model composes a small, verifier-supported vocabulary. It does not invent arbitrary predicates.

### 5.1 `may_effect/1`

```prolog
may_effect(Effect)
```

**Meaning:** there exists at least one reachable abstract execution path on which `Effect` occurs, under the declared tool scopes, guards, and preconditions.

- It is a **verifier query**, not a runtime predicate.
- It is a may-over-approximation: if it succeeds, the verifier returns a witness path and argument constraints; if it fails, no path in the abstraction produces the effect.
- It is used for safety (`may_effect(E) -> Condition`) and liveness (`Condition -> may_effect(E)`).
- It is not assertible by the model.
- It does not mean the model will call the tool; it means a path exists in the abstraction.
- It does not mean the effect definitely happens; use `must_effect/1` if that is ever supported.

### 5.2 `must_effect/1` (optional)

```prolog
must_effect(Effect)
```

**Meaning:** every reachable terminal path produces `Effect`. This is optional. If unsupported, the verifier reports `unsupported`, not `proved`.

### 5.3 `effect/1` (trace event)

```prolog
effect(Effect)
```

**Meaning:** inside `always/1` or `previously/1`, `effect(Effect)` is true at the point the effect occurs on the current trace.

Distinguish:

- `effect(Call, Effect)` — declaration fact (arity 2).
- `effect(Effect)` — trace event predicate (arity 1), used only in formulas.
- `may_effect(Effect)` — verifier reachability query.
- `must_effect(Effect)` — universal effect query (optional).

### 5.4 `call/1` (attempt event)

```prolog
call(Head)
```

**Meaning:** a tool or predicate call was attempted on the path. A denied call is a `call/1` event but not an `effect/1` event.

This distinction is essential:

- `call(issue_refund(ClaimId, Amount))` can occur even when the guard denies it.
- `effect(financial(refund(ClaimId, Amount)))` only occurs if the call succeeded and produced the effect.

### 5.5 `always/1` and `previously/1`

```prolog
always(Formula)
previously(Formula)
```

- `always(F)` — `F` holds on every reachable path/terminal state.
- `previously(F)` — on the current path, `F` held at some earlier point.
- `eventually(F)` — optional; `F` holds at some point on every path or some path, depending on the verifier definition. Document the exact semantics before use.

### 5.6 `exists_path/2`

```prolog
exists_path(Goal, Condition)
```

**Meaning:** there is a reachable path on which `Goal` holds and `Condition` holds. Used for wanted effects and coverage.

### 5.7 `forall/2`

```prolog
forall(Vars, Formula)
```

**Meaning:** `Formula` holds for all values of `Vars` in their finite/typed domains. The verifier must know the domains. Unbounded domains make the claim `unknown`, not `proved`.

### 5.8 Data and cost predicates

```prolog
labels(Value, Labels)
no_flow(Source, Sink)
bounded(Cost, Limit)
at_most_once(Effect)
```

- `labels/2` — data labels on a value.
- `no_flow/2` — a source label/value does not reach a sink.
- `bounded/2` — a cost (tokens, time, tool calls, bytes) is bounded.
- `at_most_once/1` — optional; if unsupported, report `unsupported`.

### 5.9 Status vocabulary

| Status | Meaning |
|---|---|
| `proved` | The verifier proved the claim under its assumptions. |
| `counterexample` | The verifier found a path/arguments that violate the claim. |
| `unknown` | Outside the decidable subset, timeout, or insufficient information. Never treated as proved. |
| `trivial` | Tautology, direct restatement, or a claim with no effect/tool/path/label/bound reference. Rejected. |
| `unsupported` | The claim uses a primitive or construct the verifier does not implement. |
| `assumed` | The user explicitly accepted an unproven claim as an assumption. Recorded, not proved. |

## 6. The verifier loop

1. The model writes a candidate DML module.
2. It calls `verify`.
3. The verifier returns a compact report.
4. The model repairs and re-runs `verify`.
5. When `verified(true)`, the model calls `define`.
6. `define` re-runs `verify` on the exact source and commits only if accepted.
7. Evidence is stored with the module.

Example report:

```
verified: false

obligations:
  proved no_unverified_refund
  counterexample no_over_limit_refund
    Amount = 1500
    path = [issue_refund]
  proved no_closed_claim_refund

theorems:
  proved bounded_refund
  proved refund_path_reachable
  unsupported no_double_refund
    primitive at_most_once/1 is not implemented

missing_effects: none
assumptions:
  task may call any tool in its scope
  host tool contracts are trusted
fix:
  add Amount =< 1000 to the issue_refund guard
  remove or restate no_double_refund
```

Accepted report:

```
verified: true

obligations:
  proved no_unverified_refund
  proved no_over_limit_refund
  proved no_closed_claim_refund

theorems:
  proved bounded_refund
  proved refund_path_reachable

evidence: sha256:...
```

## 7. Acceptance policy

`define` accepts only when:

- every `obligation/2` is `proved`;
- every `theorem/2` has no `counterexample`, `unsupported`, or `trivial` status;
- `unknown` theorems are either removed or explicitly accepted by the user as `assumed`;
- every declared effect is consistent with inferred effects;
- every opaque tool call has a trusted `requires/2`;
- no verified-subset violation exists;
- evidence is recorded.

`define` never accepts a weaker effect declaration silently. Trusted host-tool effects and preconditions cannot be overridden by generated code.

## 8. Anti-gaming

- **Protected obligations.** The model cannot edit `obligation/2`.
- **Trusted host contracts.** Host tool effects and `requires/2` come from a manifest; generated code cannot weaken them.
- **Inferred vs declared effects.** Undeclared effects are errors.
- **Quantified obligations.** `forall` over inputs prevents “add a convenient fact” proofs.
- **Triviality filter.** Reject `true`, `X = X`, direct restatement of a fact, or a theorem with no effect/tool/path/label/bound reference.
- **Unknown is not proved.** Unsupported or timeout claims cannot satisfy acceptance.
- **User review.** All model theorems are shown with status; the user can accept an assumption or demand a proof.
- **No self-certification.** The model cannot mark its own theorem as important or accepted.

## 9. Example candidate and loop

Protected obligations:

```prolog
obligation(no_unverified_refund,
    forall(ClaimId,
        (may_effect(financial(refund(ClaimId, _))) ->
         previously(effect(claim_verified(ClaimId)))))).

obligation(no_over_limit_refund,
    forall(Amount,
        (may_effect(financial(refund(_, Amount))) -> Amount =< 1000))).
```

Candidate:

```prolog
tool(issue_refund(ClaimId, Amount),
     "Issue a refund after verification.",
     [claim_id, number]) :-
    claim_verified(ClaimId),
    claim_status(ClaimId, open),
    Amount > 0,
    Amount =< 1000,
    issue_refund_impl(ClaimId, Amount).

effect(issue_refund(ClaimId, Amount),
       financial(refund(ClaimId, Amount))).

theorem(bounded_refund,
    forall(ClaimId, Amount,
        (may_effect(financial(refund(ClaimId, Amount))) ->
         (claim_verified(ClaimId),
          claim_status(ClaimId, open),
          Amount =< 1000)))).
```

The first `verify` call proves both obligations. The model adds `bounded_refund` as its own theorem. The user sees it with `proved`.

If the model removes `Amount =< 1000`, the verifier returns:

```
counterexample no_over_limit_refund:
  Amount = 1500
  path = [issue_refund]
```

The model restores the guard and re-verifies.

## 10. Integration points

| Area | Change |
|---|---|
| `src/validate.ts` | Read `effect/2`, `requires/2`, `ensures/2`, `obligation/2`, `theorem/2`, optional metadata. Return report with statuses. |
| `src/prolog-src/deepclause_analysis.pl` | Extract effects, infer summaries, build the tool/effect tree, solve conditions. |
| `src/repl/session.ts` | `define`/`end_turn` gate; store evidence. |
| `src/repl/harness-tools.ts` | Add `verify` tool; `define` calls it. |
| `src/cli/index.ts` | `deepclause check <file> [--json] [--strict]`. |
| `src/runner.ts` | Refuse unverified by default; `--allow-unverified` override. |
| Docs | This document; `DML_REFERENCE.md` effect section; authoring prompt. |
| Tests | Effect inference, obligations, theorems, statuses, acceptance, anti-gaming. |
| Pi extension | `dc-check` uses the SDK verifier; `dc-run` refuses unverified. |

## 11. Open questions

1. Exact syntax/grammar for `forall`, `exists_path`, `always`, `previously`, `eventually`.
2. Which numeric domains use CLP(FD) vs CLP(Q) vs SMT.
3. Bounded string/path/URL abstraction for `no_flow` and host allowlists.
4. `must_effect/1`, `eventually/1`, `at_most_once/1`: implement or leave unsupported.
5. Triviality heuristics and coverage policy (does each effect need a theorem?).
6. How the user accepts `unknown` theorems as assumptions.
7. Tool-tree bounds and partial-order reduction.
8. How `requires/2` for host tools is trusted and versioned.
9. Evidence format and storage location.
10. Whether `define` can accept a module with only obligations and no model theorems.

## 12. Summary

The model needs only a few formal primitives:

```prolog
effect(Call, Effect).
requires(Call, Condition).     % optional for visible code
theorem(Name, Condition).      % model-proposed properties
```

The user/system provides:

```prolog
obligation(Name, Condition).   % protected must-prove conditions
```

The verifier proves or refutes both over the effect/tool tree. The model loops on the report. `define` commits only when obligations and theorems are accepted.

`may_effect/1` is a verifier reachability query, not a runtime predicate. `effect/1` is a trace event; `effect/2` is a declaration. `call/1` is an attempt; `effect/1` is an actual effect after guards. Those distinctions keep the proof vocabulary precise.
