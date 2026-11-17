# Lean as a Substrate for DeepClause

Status: **analysis and design sketch — not implemented.** This document is based
on the implemented code in `deepclause-sdk` and `deepclause-pi`, not on the
aspirational parts of the docs. In particular,
`docs/DML_EFFECTS_AND_THEOREMS.md` says it is a design draft, and the code
confirms that `obligation/2`, `theorem/2`, `may_effect/1`, and the proposed
verifier do not exist yet.

## Bottom line

- **Replacing DML with Lean as the execution and authoring language is technically
  possible, but a poor fit for this product today.** It would require
  reimplementing a logic-programming runtime (unification, backtracking, cut,
  state threading, dynamic modules) inside a functional proof assistant, while
  adding a heavy toolchain to an extension whose runtime is currently an
  in-process WebAssembly binary.
- **Lean is an excellent *semantic and verification substrate*.** It is the right
  place to define the language's typing/effect rules, the reference interpreter,
  the effect/trace semantics, and the obligations/theorems checker.
- **Recommended architecture: one typed effect IR, two frontends, three execution
  modes.** Keep a small deterministic interpreter as the execution substrate; make
  Lean the place where the IR's semantics and proofs live. Offer:
  1. a small, LLM-authorable surface language (**DCL**, working name);
  2. an optional Lean EDSL for high-assurance/authoring experts;
  3. DML as a legacy frontend that is translated to the same IR for analysis and
     execution.
- **Do not rewrite the runtime first.** Add the IR and a TypeScript structural
  checker first; add Lean verification behind `dc-check`; only then consider a new
  surface language.

**If logic programming is explicitly dropped, the verdict changes materially.**
The main objection to Lean as the primary language is the logic-programming
semantic mismatch. Without it, Lean becomes a credible primary substrate. See
[§12](#12-addendum-if-logic-programming-is-dropped).

The rest of this document justifies the conditional conclusion and sketches the
language.

---

## 1. What DML actually is today

DML is not "Prolog with a few LLM calls" in the implementation. It is a
**meta-interpreter over SWI-Prolog compiled to WebAssembly**, with an explicit
operational semantics for effects, state, cut, and scopes.

### 1.1 Runtime shape

| Component | Implementation | Consequence |
|---|---|---|
| Engine | SWI-Prolog WASM (`src/prolog/loader.ts`) | In-process, portable, no native Prolog; no OS processes |
| Core semantics | `src/prolog-src/deepclause_mi.pl`, ~9.6k lines, 164 `mi_call/3` clauses | Explicit dispatch table, state threading, custom cut lowering |
| State | `state{memory, params, context_stack, depth, gas_remaining, tool_scope, model_spec, …}` | Memory/scopes are backtrackable; effects are not |
| Host loop | `src/runner.ts`, ~3.2k lines | Yields `request_agent_loop`, `request_exec`, `request_proc`, `request_form`, `request_llm`, `request_judgment`, … |
| Events | `DMLEvent` in `src/types.ts` | `stream`, `tool_call`, `task_activity`, `input_required`, `usage`, `answer`, `error`, … |
| Authoring | `src/repl/*`; `repl_define/4`, `repl_commit/3`, `repl_rollback/3` | Living module, snapshots, replace-by-predicate, versions |
| Validation | `src/validate.ts` | Prolog parse + lint only; **no effect/obligation verifier** |
| Analysis prototype | `src/prolog-src/deepclause_analysis.pl`, 208 lines | Capability/taint warnings; not a sound verifier |
| Typed workflows | `src/work/index.ts` | Shell/workflow inputs/outputs, runtime type checks |
| pi integration | `deepclause-pi/src/planner.ts`, `runtime.ts`, `index.ts` | Typed plan spec, deterministic DML assembly, `dc_run` default-off |

### 1.2 The properties that matter

These are the properties the substrate must preserve, because the product depends
on them:

1. **Deterministic orchestration with probabilistic leaves.** LLM calls are typed
   leaves; sequencing, branching, retry, fallback, and validation are explicit.
2. **Backtracking and logical variables.** Used for search, parsing, and
   relational glue. `member/2`, `findall/3`, `->`, `;`, `\+`, and `!` are part of
   the authoring vocabulary.
3. **Constraint logic programming.** `clpfd`/`clpq`/`clpr` are available for
   bounded search.
4. **Backtrackable conversation state.** A failed branch restores memory, tool
   scope, and context; host effects are *not* undone, and the docs warn about
   replaying effects by backtracking.
5. **A living module.** The REPL edits clauses at runtime, snapshots, tests, and
   commits replace-by-predicate.
6. **A typed host boundary.** `task/N` outputs are validated against schemas;
   `exec/2` calls host tools; forms/processes/media are host-owned.
7. **A small, portable runtime.** SWI-Prolog WASM starts in-process; no native
   toolchain is required from the user.
8. **Analysobility.** Homoiconicity and an explicit meta-interpreter make static
   analysis unusually tractable; the effects/theorems draft is an attempt to
   exploit that.

Any replacement substrate must be judged against all eight, not only against the
verification goal.

---

## 2. What problem is Lean supposed to solve?

The effects/theorems draft is essentially a request for a **program logic**:

- declarations of effects;
- preconditions and postconditions;
- protected obligations;
- model-proposed theorems;
- proof/counterexample/unknown statuses;
- a commit gate that accepts code only after verification.

Lean is a natural fit for that request. It offers:

- a small trusted kernel and machine-checked proof terms;
- dependent types and precise data invariants;
- a rich logic for specifications (`Prop`, quantifiers, inductive relations);
- tactics and automation (`simp`, `omega`, `linarith`, `aesop`, `grind`, `decide`);
- metaprogramming for generating verification conditions;
- a way to state and prove metatheory about the language itself (type safety,
  effect soundness, checker soundness).

Lean does **not** solve the semantic gap at the model/host boundary. A proof can
show that *if* the model returns a `Decision` satisfying the runtime-checked
schema and *if* the host tool honors its contract, *then* the plan never issues a
refund above 1000. It cannot show that the model is correct, that the prompt is
not injected, or that the payment provider behaved. Those remain assumptions or
runtime policy.

That distinction is the key to evaluating the proposal.

---

## 3. Three readings of "Lean as the substrate"

### Option A — Lean as the execution and authoring language

Every skill is a Lean module; `dc-run` compiles/elaborates it and executes compiled
Lean; effects are `IO` or a custom monad; tools are FFI.

**Pros:** direct proofs, one language, excellent metatheory.
**Cons:** dynamic authoring is hostile (Lean is compiled and its environment is
immutable), backtracking/CLP must be reimplemented or abandoned, the toolchain is
heavy, and LLM authoring of Lean proofs is unreliable. This is the option that
sounds like "replace DML with Lean" and is the least attractive.

### Option B — Lean as the semantic/verification substrate for a new surface language

A small surface language (DCL) is parsed and type-checked; it lowers to a typed
effect IR. Lean defines the IR's semantics, effect analysis, and obligation
checker. The IR is executed by a small interpreter (TypeScript for speed and
portability; Lean AOT for a reference/high-assurance mode).

**Pros:** keeps the good runtime properties, gets Lean's proof power, keeps the
LLM authoring surface small, avoids a hard Lean dependency for `dc-run`.
**Cons:** two languages/artifacts to keep in sync; the interpreter is in the TCB
unless verified/extracted.

### Option C — Lean EDSL plus reified IR

DCL is a Lean 4 EDSL (`Plan`/`Step` monad); `do` notation builds an effect tree;
`#verify` generates obligations; the tree is extracted to the same IR and executed
by the runtime. This is the most literal "Lean as substrate" that still separates
execution from proof.

**Pros:** direct Lean tooling, dependent types where wanted, proofs and code in
one place.
**Cons:** expert-only; LLM authoring is harder; Lean elaboration is required at
authoring time; runtime still needs an interpreter.

**Recommendation: B, with C as an optional expert frontend.** Lean should be the
place where the language is *defined and verified*, not the place where everyday
skills are *executed*.

---

## 4. Feasibility

### 4.1 Toolchain and runtime facts (as of October 2026)

These are the practical constraints that determine feasibility:

- The current Lean release is **4.35.0**. Native Lean via `elan`/`lake` is the
  supported path.
- There is **no first-class, officially supported WASM target** in the release
  toolchain that this analysis could verify. The working options are third-party
  or experimental:
  - **Lasm** (`@lasm/cli` 0.3.5): compiles Lean 4 AOT to WASM for Node, Bun, Deno,
    edge, WASI, and the browser. It requires **Node ≥24**, at least **512 MB** for
    deployed Node apps, a first build of roughly **250–275 MB** of toolchain
    downloads (about 19 s on a CI runner), and about **1–2 s** for incremental
    builds. Crucially, deployed programs **cannot elaborate, evaluate, or compile
    new Lean code, load `.olean` files, or use Lean's kernel at runtime**.
  - **cauli/lean4-wasm-in-browser**: a fork of Lean that compiles the compiler to
    WASM and runs it in a browser worker; useful for an in-browser `dc-check`, but
    experimental and not the official toolchain.
  - **T-Brick/lean2wasm**: an early experimental compiler, last updated in 2024;
    not a production dependency.
- Lean's `IO` is opaque to the kernel. If a program performs effects in `IO`,
  Lean can prove nothing about those effects. To get the effects/theorems draft,
  effects must be **reified as data** (a free monad or command tree) and
  interpreted by a host; the Lean proofs reason about the tree, not about `IO`.
- Lean proofs are erased at runtime. Proofs do not make the compiler or runtime
  correct; they guarantee that the source satisfies the spec under the stated
  assumptions, modulo the trusted kernel/compiler/runtime.
- `sorry`, `axiom`, `unsafe`, `partial`, `@[implemented_by]`, `@[extern]`,
  `opaque`, custom elaborators, and `native_decide` are all ways to weaken or
  bypass the guarantee. An acceptance policy must inspect the environment and
  proof terms, not just the source text.

**Implication:** Lean can be a hard dependency for `dc-check` on developer
machines, but it should not be a hard dependency for `dc-run` in a pi extension.
The extension currently targets Node 22 and starts in-process; adding a Node 24
Lean toolchain with hundreds of megabytes of cache would be a product regression.

### 4.2 Semantic fit

| DML property | Lean fit | What a Lean runtime would need |
|---|---|---|
| Horn clauses, unification, SLD resolution | Poor | A Prolog-like runtime or an embedded logic monad; Lean has expression unification only in `MetaM` |
| Backtracking with trail | Poor | An explicit search monad and state snapshots; cut must be redesigned or emulated |
| Cut / negation as failure | Poor | Non-logical control; better replaced by explicit `commit`/`first` |
| CLP(FD/Q/R) | Poor | External solver or bounded enumeration; Lean's `omega`/`linarith` prove, they do not search at runtime |
| Mutable living module (`assertz`/`retract`) | Very poor | Lean's environment is immutable; require re-elaboration/compilation |
| Backtrackable memory/state | Good | Immutable `State` threaded through a free monad |
| Typed LLM leaves and schemas | Excellent | `Schema`/`Decidable` validators, `Plan` commands |
| Effects and capabilities | Excellent if reified | Effect rows, capability tokens, host manifest |
| Processes/concurrency | Medium | `Task`/`IO` or a process effect; verification of concurrency is a research project |
| Streaming/cancellation/usage | Medium | Host interpreter handles them; model them as explicit events/outcomes |
| Static analysis | Excellent | Reified IR, effect inference, trace logic |

The pattern is clear: **Lean is a great fit for the *analysis* half and a poor fit
for the *logic-programming execution* half.** A wholesale replacement throws away
the latter.

### 4.3 Engineering and product constraints

- **Rewrite cost.** The meta-interpreter is ~9.6k lines with 164 dispatch clauses,
  custom cut lowering, frames, dynamic session predicates, gas accounting, and a
  large host event surface. Reimplementing that in Lean is a multi-quarter effort
  before feature parity.
- **Dynamic authoring.** DML's REPL edits clauses and commits transactions. Lean
  AOT WASM cannot elaborate at runtime; a native Lean server could, but that is a
  separate long-running toolchain and a new failure mode. A source-based workflow
  with `dc-check` is feasible; an interactive `define`/`execute` loop is not.
- **Distribution.** Pi extensions are installed from GitHub and run under Node 22.
  Native Lean binaries per platform, or a Node 24 + 512 MB WASM runtime, are a
  significant install/CI/support burden.
- **LLM authoring.** DML is already a stretch for models; Lean proof scripts are
  much harder. A restricted surface with automatic proof search is essential;
  requiring the model to write `by` proofs will produce `sorry` and long error
  loops.
- **Proof automation.** `decide`/`omega`/`linarith` handle finite/bounded
  obligations well. Unbounded loops, string/path reasoning, concurrency, and
  semantic model properties will return `unknown` or need invariants. That is
  acceptable if the status vocabulary is honest, but it limits the "automatic"
  value.
- **Trusted base.** Lean kernel + compiler + IR interpreter + host + tool
  implementations + model provider. Proofs do not remove the host from the TCB.
- **Version skew.** The pi extension currently depends on a published SDK version
  (`deepclause-sdk@0.0.89`) while the local SDK is 0.5.0. A substrate change must
  be coordinated across packages and versions.

### 4.4 Feasibility scorecard

Judgment, not measurement; 1 = poor, 5 = strong.

| Criterion | A: Lean runtime | B: Lean verifier + IR | C: Lean EDSL |
|---|---:|---:|---:|
| Effect/contract verification | 5 | 5 | 4 |
| Dynamic authoring / fast REPL | 1 | 4 | 2 |
| Backtracking / CLP compatibility | 1 | 3 (legacy DML retained) | 1 |
| Portability / install cost | 2 | 4 | 3 |
| LLM authoring | 2 | 4 | 2 |
| Time to first value | 1 | 4 | 2 |
| Long-term ceiling | 5 | 4 | 5 |
| Risk | Very high | Medium | High |

**Verdict:** A is a research rewrite, B is an engineering project with a clear
incremental path, C is an expert-tier feature. B is the recommended first step.

---

## 5. Usefulness

### 5.1 What Lean genuinely buys

- **Machine-checked structural safety.** Effect declarations, capability use,
  typed I/O, path confinement, budget bounds, and dataflow labels can be stated
  and proved for the IR.
- **Protected obligations.** A protected spec module can state properties the
  model cannot edit; the kernel checks the proof, so "the model marked it proved"
  is not evidence.
- **Proof-carrying plans.** A plan can carry a proof term or a verifier report;
  `dc-run` can refuse unverified plans or warn, with a hash of the source/IR/
  manifest/checker.
- **Precise counterexamples.** For finite plans, a checker can return a concrete
  trace and inputs; for integer constraints, an SMT oracle can propose a witness
  that Lean validates.
- **Metatheory.** Lean can prove the type system sound, the effect analysis
  over-approximates, and the checker accepts only valid plans. This is valuable
  for a language, not just for one skill.
- **A clear assumption ledger.** The effects draft already emphasizes
  `assumed`; Lean makes assumptions explicit as axioms/parameters, which is a
  better user experience than hidden trust.

### 5.2 What Lean does not buy

- **Model correctness.** No proof can establish that an LLM's semantic judgment is
  true. You can prove "if the model returns a value satisfying P, then Q"; you
  cannot prove the model returns such a value without a trusted oracle or
  constrained decoding.
- **Host correctness.** `bash`, network, filesystem, process, and provider
  behavior are outside the proof. Runtime policy and approvals remain necessary.
- **Prompt-injection resistance.** Type/label systems can help, but the semantic
  content of untrusted text is not a theorem.
- **A better authoring experience.** Lean is harder to write and read than DML for
  most users and models. The surface language must hide it.
- **A smaller runtime.** Lean is much heavier than SWI-Prolog WASM for this use
  case.

### 5.3 Where the value is highest

Ordered by expected payoff:

1. **Typed capabilities and effects.** Replace stringly-typed `exec/2` with typed
   tools and effect rows. This alone removes a large class of policy/analysis
   ambiguity.
2. **Schema/contract checking for LLM leaves.** Prove that validated outputs have
   the expected type; make retry/fallback explicit and exhaustive.
3. **Dataflow/label checks.** Secrets and untrusted session content flowing into
   tools/prompts is a real risk; a label checker over the IR is tractable.
4. **Path and workspace invariants.** A `Path` smart constructor with a proof of
   confinement; runtime checks remain.
5. **Budget/termination for deterministic glue.** Bounded loops and model/tool
   call counts are easy to state and often easy to prove.
6. **Authorization/business invariants** (the refund example). High value where
   the domain is formalizable and the tools are typed.
7. **Proof-carrying plans for regulated workflows.** Narrow but high-assurance.

### 5.4 Where the cost is highest

- Proof engineering for unbounded loops, string/path reasoning, concurrency, and
  external APIs.
- Keeping the surface language, IR, Lean semantics, and runtime interpreter in
  sync.
- Toolchain distribution and CI.
- Teaching models and users to write DCL, not just DML.
- Reviewing assumptions: a proof with a bad `assume` is worse than no proof
  because it looks authoritative.

### 5.5 Alternatives worth considering

| Approach | Strength | Weakness | Fit here |
|---|---|---|---|
| Keep DML, add IR + effect checker in TS | Fast, no new toolchain | No machine-checked proofs | Best Phase 0 |
| Lean verifier over IR | Proofs, metatheory | Toolchain, proof automation | Best Phase 1 |
| Dafny/F* contracts | Automatic verification, imperative effects | Different language/toolchain, less metatheory | Strong alternative for contract-heavy code |
| SMT (Z3) + runtime monitor | Counterexamples, linear arithmetic | No general proofs/metatheory | Complement to Lean |
| TLA+/Alloy | Concurrency/protocol design | Not an execution language | Use for process protocols |
| Lean EDSL for experts | Direct proofs, one language | Hard for LLMs/users | Optional high-assurance tier |

Lean is the best fit for *metatheory + dependent types + proof-carrying plans*.
Dafny may be a better fit if the priority is *automatic contract verification of
imperative effectful code*. The proposal should not assume Lean is the only
reasonable proof substrate.

---

## 6. Recommended architecture

```
┌──────────────────────────────────────────────────────────────┐
│ pi extension / CLI                                           │
│  /dc-plan  /dc-check  /dc-run  /dc-list  /dc-tool            │
└───────────────┬──────────────────────────────┬───────────────┘
                │                              │
      ┌─────────▼─────────┐          ┌─────────▼─────────┐
      │ DCL frontend      │          │ DML frontend      │
      │ parse/typecheck   │          │ existing loader   │
      └─────────┬─────────┘          └─────────┬─────────┘
                │                              │
                └──────────────┬───────────────┘
                               ▼
                    ┌─────────────────────┐
                    │ Typed effect IR     │  (JSON / binary, versioned)
                    │ plans, types,       │
                    │ effects, contracts  │
                    └──────────┬──────────┘
                               │
              ┌────────────────┼────────────────┐
              ▼                ▼                ▼
      ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
      │ TS fast      │  │ Lean verifier│  │ Runtime      │
      │ checks       │  │ (dc-check)   │  │ interpreter  │
      │ effects,     │  │ obligations, │  │ (dc-run)     │
      │ schemas,     │  │ theorems,    │  │ host effects │
      │ paths, budget│  │ counterexamples│ │ events       │
      └──────────────┘  └──────────────┘  └──────┬───────┘
                                                  │
                                        ┌─────────▼─────────┐
                                        │ pi / host tools   │
                                        │ LLM, bash, forms, │
                                        │ processes, media  │
                                        └───────────────────┘
```

### 6.1 Components

1. **IR (TypeScript package, also a Lean inductive type).** A stable, serializable
   representation of types, plans, effects, contracts, obligations, and
   manifests. Both frontends produce it; both the checker and interpreter consume
   it.
2. **DCL frontend.** A small surface language, parsed/type-checked in TypeScript.
   Produces IR with source spans and diagnostics. No Lean dependency.
3. **DML translator.** Best-effort translation of DML clauses to IR for analysis
   and (eventually) execution. Unsupported constructs are reported as `unsupported`
   or conservatively over-approximated. This preserves the existing investment and
   gives immediate value.
4. **TS fast checks.** Effect inference, capability checks, schema validation,
   path confinement, budget/termination heuristics, label/taint analysis. These
   run on every edit and are the default `dc-check`.
5. **Lean verifier.** A Lean package that imports the IR (or receives a generated
   Lean module), defines the semantics, and checks obligations/theorems. Runs as
   an optional native sidecar, or as an AOT-compiled checker for the decidable
   subset.
6. **Runtime interpreter.** Executes IR, emits the existing `DMLEvent` stream,
   enforces budgets/cancellation/policy. TypeScript for production; Lean AOT for a
   reference/high-assurance mode and differential testing.
7. **Evidence store.** Hash of source + IR + manifest + checker version +
   assumptions + statuses. Stored under `.pi/deepclause/evidence/` (never
   `.deepclause/`).

### 6.2 Deployment modes

| Mode | `dc-check` | `dc-run` | Dependency |
|---|---|---|---|
| Fast | TS structural checks | TS interpreter | None |
| Verified | Native Lean sidecar | TS interpreter + evidence | Lean/elan optional |
| High-assurance | Native Lean | Lean AOT interpreter (or TS + evidence) | Lean toolchain |
| Browser | Lean WASM compiler (optional) | TS interpreter | Large WASM payload |

The default should be **Fast**; **Verified** is opt-in per workspace. `dc-run`
never requires Lean unless the workspace asks for strict proof checking.

### 6.3 Trust boundaries

- **Proved:** typing, effect rows, capability use, schema validation,
  path construction, budget bounds, contract implications, effect/trace
  properties, checker soundness.
- **Assumed / runtime-enforced:** model output semantics, host tool behavior,
  network, filesystem, processes, provider guarantees, user decisions.
- **Runtime policy:** approvals, workspace isolation, concurrency, cancellation,
  redaction, usage accounting. Proofs are an additional layer, not a replacement.

---

## 7. DCL: a language sketch

**DCL** (working name; "DeepClause Lean") is a small, typed, effect-annotated
successor frontend. It is deliberately *not* full Lean: no user-facing dependent
types in executable code, no tactics in every skill, no implicit backtracking in
effectful code. Its semantics and checker are defined in Lean; its surface is
optimized for LLM authoring and review.

### 7.1 Goals and non-goals

**Goals**

- Deterministic plans with typed, bounded LLM/tool leaves.
- Explicit effects and capabilities; no stringly-typed host calls.
- Exhaustive handling of failure, cancellation, and human input.
- Machine-checkable contracts, obligations, and evidence.
- A surface a model can author reliably, with good diagnostics.
- A runtime that stays fast, in-process, and pi-hosted.

**Non-goals**

- A general-purpose programming language.
- Full dependent types for everyday skills.
- Implicit Prolog-style backtracking in effectful code.
- Proving model semantics or host behavior.
- Replacing DML's search/CLP strengths in v1.

### 7.2 Surface syntax

Lean-shaped but restricted: expressions, `let`, `if`/`match`, explicit blocks,
no user tactics in normal code. Indentation is significant; braces are allowed.

```dcl
module refund

type ClaimId = String

enum Status { open, closed, disputed }

record Claim {
  id: ClaimId,
  amount: Int,
  status: Status,
}

record Decision {
  approve: Bool,
  amount: Int,
  reason: String,
}

enum Outcome {
  refunded(amount: Int),
  rejected(reason: String),
}

plan issue_refund(claim: Claim) -> Outcome
  requires claim.status == Status.open
  requires claim.amount > 0
  effects { llm, refund_payment }
  budget { model_calls: 1, tool_calls: 1, wall_ms: 30_000 }
{
  let d: Decision = llm(
    prompt: "Review refund claim {claim.id} for {claim.amount}.",
    schema: Decision,
    checked: d => d.amount >= 0,
  )

  if d.approve {
    require d.amount <= 1000
    let r: Receipt = refund_payment(claim.id, d.amount)
    ensure r.status == ReceiptStatus.accepted
    return Outcome.refunded(d.amount)
  } else {
    return Outcome.rejected(d.reason)
  }
}
```

The surface should preserve DML's good ideas: explicit inputs/outputs, typed
leaves, short plans, and `require`/`ensure` guards. It should remove the parts
that make verification hard: implicit clause search, dynamic predicates, and
stringly-typed tools.

### 7.3 Types and schemas

- Base types: `String`, `Int`, `Nat`, `Float`, `Bool`, `Bytes`, `Json`, `Path`,
  `Secret<T>`, `Untrusted<T>`.
- Aggregates: records, enums, lists, options, results.
- `Schema<T>` is a runtime-validatable description derived from a type; the
  runtime validates model/tool output and returns `Result<T, ValidationError>`.
- Refinements belong in contracts, not executable types:
  `require x > 0`, `ensure result.amount <= 1000`.
- A `Path` is constructed only by `Path.workspace("relative/path")`; the checker
  proves the smart constructor confines to the workspace, and the runtime still
  checks.
- `Secret<T>` cannot be interpolated into prompts or logs without `declassify`;
  `Untrusted<T>` cannot be passed to a privileged tool without an explicit
  policy. This is roadmap, not v1.

### 7.4 Effects and capabilities

```dcl
effect llm
effect human
effect fs
effect net
effect proc
effect time
effect rand
effect fail
effect state
effect nondet
effect tool(name: String)
effect opaque(reason: String)
effect domain(name: String, args: List<Value>)   // e.g. financial.refund(claim, amount)
```

The exact effect syntax is provisional; the important property is that domain effects
are declared by the trusted tool manifest, not invented by skill code.

- A plan's type carries an effect row: `Plan {llm, tool "bash"} Outcome`.
- Subtyping: `{llm} < {llm, fs}`. A plan may declare more than it uses; the
  checker rejects uses not declared.
- `with_tools`, `with_model`, `with_budget`, `with_context`, `with_fresh_memory`
  are handlers that introduce/discharge scoped effects.
- `tool` declarations live in a **trusted manifest**, not in skill code:

```dcl
tool refund_payment(claim: ClaimId, amount: Int) -> Receipt
  requires amount > 0 && amount <= 1000
  effect financial.refund(claim, amount)
```

- `bash` is a typed tool with an `opaque` effect. A plan that uses `bash` cannot
  prove a domain obligation that depends on the command's meaning; the checker
  reports `assumed` or `unsupported`. This is the honest boundary that DML's
  `exec(bash(...))` currently hides.

### 7.5 Plans and commands

Core commands (the IR's instruction set):

| Command | Meaning | Effect |
|---|---|---|
| `llm(prompt, schema, checked?)` | typed model leaf | `llm` |
| `prompt(...)` | fresh-memory model leaf | `llm` |
| `call tool(args)` / `tool(args)` | typed host tool | `tool name` |
| `ask(form)` | structured human input | `human` |
| `emit(event)` | progress/stream display | none/host |
| `return value` | terminal result | none |
| `fail(error)` | explicit failure | `fail` |
| `retry(policy, block)` | bounded retry | inherits |
| `fallback(block, block)` | explicit alternative | inherits |
| `spawn(plan)` / `send` / `receive` | process primitives | `proc` |
| `search { ... }` | pure bounded search | `nondet` |
| `assert P` / `require P` / `ensure P` | runtime/contract check | `fail` |
| `with_* { ... }` | scoped handler | scoped |

Notably absent: `assertz`/`retract`, implicit clause choice, `!`, `\+`, and
`exec` with an arbitrary string. If a skill needs a dynamic store, it uses an
explicit `Store` effect with typed keys, not code mutation.

### 7.6 Contracts, obligations, theorems

```dcl
obligation no_over_limit_refund {
  forall claim: Claim, amount: Int,
    effect(financial.refund(claim.id, amount)) -> amount <= 1000
}

theorem bounded_refund(claim: Claim)
  requires claim.status == Status.open
  ensures forall amount: Int,
    effect(financial.refund(claim.id, amount)) -> amount <= 1000
{
  -- proof script, or `by` automation; may be omitted in favor of a verifier
  -- report for the bounded/decidable subset
}
```

- `obligation` is protected: it lives in a separate spec module and cannot be
  edited by the skill.
- `theorem` is model/user-proposed; it may not weaken an obligation or a trusted
  tool contract.
- Statuses: `proved`, `counterexample`, `unknown`, `trivial`, `unsupported`,
  `assumed` (the same vocabulary as the effects draft).
- `assume P` is allowed only with an explicit user-visible record; it does not
  count as `proved`.
- The checker must reject trivial claims (no effect/tool/path/budget/label
  reference), direct restatements, and `True`.

### 7.7 Search, logic, and CLP

DML's implicit backtracking is valuable for pure search and painful for effects.
DCL splits them:

- `search { ... }` is a **pure** block with finite domains or a `decreases`
  measure. It can use `choose`, `require`, `match`, and bounded enumeration.
  Effects are forbidden inside `search`.
- `retry`/`fallback` are explicit, bounded, and visible in the trace. They do not
  rewind host effects.
- Large integer/constraint problems call a typed `solve` capability (SMT/CLP), with
  a certificate or `assumed` status. The language does not promise a built-in
  general CLP solver.
- Unification is available for pattern matching and small logic fragments; it is
  not the default execution model.
- If a skill truly needs DML-style SLD search with effects, keep it in DML (legacy
  frontend) rather than weakening DCL's guarantees.

### 7.8 Memory and context

- `State` is immutable: memory, budget, trace, model, tools, context.
- `llm` reads state memory; `prompt` uses fresh memory; `with_fresh_memory` scopes
  a group and restores the outer memory.
- `with_context(mode)` imports pi session context (`turn`/`branch`/`isolated`);
  pi remains the sole session owner.
- `remember`/`recall` are explicit; there is no hidden authoring transcript.
- Backtracking in `search` snapshots `State`; effectful steps are linear.

### 7.9 Processes, forms, media

- `spawn(plan) -> Proc<T>`, `send`, `receive`, `await`, `cancel`, `select`.
  Start with typed mailboxes and unverified concurrency; session types are later.
- `ask(form) -> Submitted<T> | Cancelled | Unavailable`; the type forces handling
  all outcomes. This is stricter than DML's `Change.action` checks.
- `read_media`, `attach_media`, `generate_image`, `output(media)` become typed
  commands/values.
- `emit` and `stream` are presentation events; they do not affect plan semantics.

### 7.10 Budgets, cancellation, streaming, usage

- `budget { model_calls, tool_calls, tokens, wall_ms }` is enforced by the
  interpreter and checkable statically for bounded plans.
- Cancellation is an outcome (`Cancelled`), not a silent failure; plans must
  handle it or propagate it.
- Streaming is an event stream attached to an LLM call; it does not change the
  result type.
- Usage is recorded per call and aggregated in the trace; `dc-run` maps it to pi's
  usage metadata.

### 7.11 Modules, imports, versions

- `module`/`import` with explicit exports and version pins; imports are read-only.
- No runtime `use_harness` cycle or dynamic redefinition.
- A plan has a source hash and an IR hash; evidence is versioned with both.
- Bundled documentation and examples are versioned; initialization is
  non-destructive, as in the current pi extension.

### 7.12 Diagnostics and acceptance

`dc-check` output should be compact and actionable:

```
verified: false
effects: declared {llm, refund_payment}; inferred {llm, refund_payment}
obligations:
  proved no_over_limit_refund
  counterexample no_duplicate_refund
    claim.id = "C-17"; amount = 250; path = [issue_refund, refund_payment]
theorems:
  proved bounded_refund
  unsupported no_double_refund (at_most_once/1 not implemented)
assumptions:
  llm returns a Decision satisfying the schema
  refund_payment honors its manifest contract
fix:
  add `require !already_refunded(claim.id)` before `refund_payment`
```

Acceptance:

1. Every obligation is `proved`.
2. Every theorem is not `counterexample`, `unsupported`, or `trivial`; `unknown`
   requires explicit user acceptance as `assumed`.
3. Inferred effects ⊆ declared effects.
4. Every tool call has a trusted manifest contract.
5. No forbidden Lean constructs in harness code.
6. Evidence hash covers source, IR, manifest, checker version, assumptions.
7. `dc-run` refuses unverified plans only in strict mode; otherwise warns and
   records the report.

### 7.13 The IR

The IR is the contract between frontends, checker, and runtime. It should be
versioned, serializable, and small. Sketch:

```json
{
  "version": 1,
  "module": "refund",
  "types": [
    {"kind": "record", "name": "Claim", "fields": [
      {"name": "id", "type": "ClaimId"},
      {"name": "amount", "type": "Int"},
      {"name": "status", "type": "Status"}
    ]}
  ],
  "plans": [
    {
      "name": "issue_refund",
      "params": [{"name": "claim", "type": "Claim"}],
      "result": "Outcome",
      "effects": ["llm", {"tool": "refund_payment"}],
      "requires": ["claim.status == Status.open", "claim.amount > 0"],
      "body": [
        {"op": "llm", "prompt": "...", "schema": "Decision", "bind": "d"},
        {"op": "if", "cond": "d.approve", "then": [
          {"op": "require", "cond": "d.amount <= 1000"},
          {"op": "call", "tool": "refund_payment",
           "args": ["claim.id", "d.amount"], "bind": "r"},
          {"op": "ensure", "cond": "r.status == ReceiptStatus.accepted"},
          {"op": "return", "value": "Outcome.refunded(d.amount)"}
        ], "else": [
          {"op": "return", "value": "Outcome.rejected(d.reason)"}
        ]}
      ]
    }
  ],
  "obligations": [
    {"name": "no_over_limit_refund",
     "formula": "forall claim amount, effect(financial.refund(claim.id, amount)) -> amount <= 1000"}
  ],
  "theorems": []
}
```

The IR should be the *only* thing the runtime executes. DCL and DML are frontends;
Lean is a checker over the IR. This avoids a three-way semantic drift between
surface, Lean, and runtime.

### 7.14 What the Lean EDSL looks like

For experts, the same IR can be built with Lean `do` notation:

```lean
import DeepClause

open DeepClause

def issueRefund (claim : Claim) : Plan [.llm, .tool "refund_payment"] Outcome := do
  let d ← llm
    (prompt := s!"Review refund claim {claim.id} for {claim.amount}.")
    (schema := Decision)
    (checked := fun d => d.amount ≥ 0)
  if d.approve then
    require (d.amount ≤ 1000)
    let r ← call "refund_payment" (claim.id, d.amount)
    ensure (r.status = .accepted)
    return .refunded d.amount
  else
    return .rejected d.reason
```

This is where Lean's dependent types and proof terms are most useful. It is not
the default authoring surface.

---

## 8. Lean formalization sketch

The following is illustrative, not compilable as written. It shows the shape of
the Lean side.

### 8.1 Effects, capabilities, commands

```lean
inductive Eff where
  | llm | human | fs | net | proc | time | rand | fail | state | nondet
  | tool (name : String)
  | domain (name : String)
  deriving DecidableEq, Repr

abbrev EffRow := List Eff

structure Capability where
  name : String
  Args : Type
  Result : Type
  contract : ToolContract

inductive Cmd (ε : EffRow) : Type → Type
  | llm : Prompt → Schema α → Cmd ε α
  | call : (c : Capability) → c.Args → Cmd ε c.Result
  | ask : FormSpec → Cmd ε Answer
  | emit : Event → Cmd ε Unit
  | fail : Error → Cmd ε α
  | choose : List (Cmd ε α) → Cmd ε α
  | commit : Cmd ε α → Cmd ε α
```

`Plan ε α := Free (Cmd ε) α` (or an equivalent free monad). `do` notation builds
a reified tree. Nothing in a plan is executed by Lean's `IO`; the host interprets
the tree.

### 8.2 Semantics and traces

```lean
inductive Event where
  | call (name : String) (args : Value)
  | effect (e : Eff) (args : Value)
  | guard (ok : Bool)
  | branch (i : Nat)
  | answer (v : Value)
  | fail (msg : String)

structure Trace where
  events : List Event

structure State where
  memory : List Message
  budget : Budget
  model : ModelSpec
  tools : List String
```

The reference interpreter is a total function over fuel (or a relational
semantics):

```lean
def interpret (oracle : Oracle) (fuel : Nat) :
    Plan ε α → State → Outcome α × State × Trace
```

`Oracle` supplies `llm` and `call` results. It is the only place where model/host
behavior enters; proofs are parameterized by its contract.

### 8.3 Effect soundness

```lean
def effects : Plan ε α → EffectSet

theorem effects_sound
    (h : interpret oracle fuel p s = (ok v, s', tr)) :
    ∀ e ∈ tr.effectSet, e ∈ effects p := by
  ...
```

This is the formal counterpart of the effects draft's "declared vs inferred
effects" check. It is a property of the IR, not of any particular model.

### 8.4 Obligations, theorems, statuses

```lean
inductive Status where
  | proved | counterexample (w : Witness) | unknown
  | trivial | unsupported | assumed

structure Report where
  obligations : List (Name × Status)
  theorems : List (Name × Status)
  inferredEffects : EffectSet
  assumptions : List Assumption
  evidence : Hash

def accepted (r : Report) : Bool :=
  r.obligations.all (·.2 = .proved) &&
  r.theorems.all (fun (_, s) => s ≠ .counterexample &&
                               s ≠ .unsupported &&
                               s ≠ .trivial) &&
  r.inferredEffects ⊆ r.declaredEffects &&
  r.noForbiddenConstructs
```

The real checker generates verification conditions from the IR, applies Lean
tactics/decision procedures, and returns a report. For finite plans, a symbolic
executor can enumerate paths; for integer constraints, `omega`/`linarith` prove or
an SMT oracle proposes a witness that Lean validates.

### 8.5 Anti-gaming checks

The acceptance policy must inspect more than source text:

- reject `sorryAx` and any custom `axiom`;
- reject `unsafe` definitions in executable code;
- reject `partial` in plans (allow in explicitly trusted libraries only);
- reject `@[implemented_by]` and `@[extern]` in harness code;
- reject `opaque` definitions whose behavior matters;
- reject custom `elab`/`macro`/`initialize` in harness code (trusted prelude only);
- allow only standard axioms (`propext`, `Classical.choice`, `Quot.sound`);
- mark `native_decide`/`Lean.ofReduceBool` as a trusted assumption or forbid it in
  strict mode;
- check that every theorem mentions an effect/tool/path/budget/label from the plan;
- require counterexamples to include a reproducible trace and inputs.

This is the Lean-specific counterpart of the effects draft's anti-gaming section.

---

## 9. Migration plan

### Phase 0 — IR and TypeScript checks (no Lean)

- Define the IR and JSON schema; add a DML→IR translator for the supported subset.
- Implement effect inference, capability/path/schema/budget checks, and a
  structured report in `src/validate.ts`/a new package.
- Add `dc-check` output and evidence storage.
- Add runtime monitoring and trace events.
- **Value:** the effects/theorems draft's structural half, without a toolchain.

### Phase 1 — Lean reference semantics and checker

- Create a Lean package with the IR types, reference interpreter, effect
  soundness theorem, and obligation checker.
- Run it as an optional native sidecar for `dc-check`.
- Add proof acceptance/anti-gaming checks and evidence.
- Differential-test the Lean interpreter against the TS interpreter on the IR
  corpus.
- **Value:** machine-checked obligations/theorems; a formal spec for the language.

### Phase 2 — AOT/WASM checker

- Compile the fixed checker (not arbitrary user Lean) AOT to WASM with Lasm or a
  comparable toolchain.
- Bundle it optionally; fall back to TS checks when unavailable.
- If arbitrary proof checking is required at runtime, use native Lean or the
  in-browser Lean compiler; document the trade-off.
- **Value:** in-process verification without a native toolchain.

### Phase 3 — DCL frontend

- Implement the surface parser/typechecker and lower to IR.
- Add `dc-plan` generation of DCL/IR, `dc-check`, and `dc-run`.
- Keep DML as a legacy frontend; translate DML to IR for analysis where possible.
- Benchmark LLM authoring: DML vs DCL vs Lean EDSL; measure time-to-green, proof
  attempts, and human review.
- **Value:** a cleaner, typed authoring language with the existing runtime.

### Phase 4 — Optional Lean EDSL / high-assurance tier

- Provide a Lean library for expert users; share the IR and checker.
- Add strict mode: `dc-run` refuses plans without evidence.
- Keep the default path fast and Lean-free.
- **Value:** proof-carrying skills for regulated/high-assurance workflows.

### What to keep from DML

- The host boundary and event mapping (`DMLEvent`, `runner.ts` request/response).
- Workspace initialization and path isolation.
- pi as the model/credential/session/cancellation/usage owner.
- The `dc_run` opt-in gate, `/dc-plan`, `/dc-list`, `/dc-cancel`.
- DML as a legacy/authoring frontend for search/CLP-heavy skills.
- The typed plan spec in `deepclause-pi/src/planner.ts` as the seed of the IR.

### What not to keep

- Dynamic clause mutation as the primary authoring model.
- Implicit effectful backtracking.
- Stringly-typed `exec(bash(...))` as a verifiable capability.
- A second session store or `.deepclause/` artifacts.

---

## 10. Validation plan

Before claiming the design works, test:

**Language/IR**

- IR round-trip and schema-version compatibility.
- DML→IR translation for supported constructs; `unsupported` for dynamic
  predicates, higher-order goals, and opaque shell commands.
- Effect inference: declared vs inferred, missing/extra effects.
- Capability checks: a plan cannot call a tool outside its effect row.
- Path checks: `Path.workspace` rejects `..`, absolute paths, symlink escapes.
- Schema checks: required/wrong-typed model outputs fail and trigger the declared
  fallback; no silent defaults.
- Budget checks: model/tool call limits, wall-clock, cancellation.
- Label checks: untrusted session content cannot reach a privileged tool without
  declassification.
- Search: finite domains terminate; unbounded search is rejected or returns
  `unknown`.

**Verification**

- Obligations `proved` only with a valid proof; `sorry`/`axiom`/`unsafe` rejected.
- Counterexamples are reproducible and include inputs/trace.
- `unknown`/`unsupported` are not accepted as proved.
- Trivial theorems are rejected.
- Trusted manifests cannot be overridden by skill code.
- Evidence hash changes when source/IR/manifest/checker changes.
- Differential test: Lean reference interpreter vs TS interpreter on a corpus.

**pi integration**

- No `.deepclause/` files are created.
- `dc_run` is absent by default, appears after `/dc-tool enable`, disappears after
  disable, and rejects concurrent runs.
- Context modes `turn`/`branch`/`isolated`, forks, compaction.
- Model switching, streaming, cancellation, usage.
- Invalid IR/DCL, missing skills, path traversal, and unverified plans.
- Existing `.pi/deepclause/` files are never overwritten silently.
- LLM authoring benchmark: can the model produce valid DCL from the bundled docs?

---

## 11. Recommendation

**Adopt Lean as the semantic/verification substrate; do not replace the DML
runtime with Lean.**

Concretely:

1. **Now:** define a typed effect IR and a TS structural checker; translate the
   supported DML subset; add `dc-check` reports. This delivers much of the effects
   draft without a new toolchain.
2. **Next:** formalize the IR in Lean, prove effect soundness, implement
   obligations/theorems, and run Lean as an optional `dc-check` sidecar. Add
   anti-gaming checks and evidence.
3. **Later:** add DCL as a typed surface over the IR; keep DML as a legacy
   frontend. Offer a Lean EDSL for high-assurance skills.
4. **Never:** make Lean a hard dependency of `dc-run` or of extension startup;
   never execute model/host effects inside Lean's `IO` and claim proofs about
   them; never let a model mark its own obligations as proved.

### Conditions that would change this recommendation

- Lean gains a first-class, officially supported, lightweight WASM target with
  runtime elaboration and a stable kernel, and the pi extension can bundle it
  without a Node-version or memory penalty.
- The product pivots to high-assurance, proof-carrying workflows where every skill
  is authored by formal-methods engineers, not by an LLM in a REPL.
- DML's backtracking/CLP usage turns out to be marginal in real skills, so the
  cost of abandoning it is low.
- A different substrate (Dafny/F*/SMT) proves to be a better fit for the
  contract-heavy, imperative parts; Lean would then be reserved for metatheory.

Until then, **the winning move is to keep DML's execution strengths and add Lean
where it is strongest: the semantics, the checker, and the proof-carrying
evidence.**

---

## 12. Addendum: if logic programming is dropped

The analysis above assumes DML's logic-programming semantics are non-negotiable.
If they are not — if the product can give up unification, backtracking, cut, CLP,
dynamic clauses, and the living-module REPL — then the central objection to Lean
as the primary language disappears. This is a materially different project, and
it is worth evaluating on its own terms.

### 12.1 What disappears

- Unification, SLD resolution, backtracking, and cut.
- `clpfd`/`clpq`/`clpr` constraints.
- `assertz`/`retract`, dynamic predicates, and runtime clause editing.
- The homoiconic meta-interpreter and custom cut lowering.
- Most of `src/prolog-src/deepclause_mi.pl` (the 9.6k-line semantics), the
  SWI-Prolog WASM engine, and a large part of the runner's state/signal plumbing.
- The hardest cases in the effects/theorems draft: dynamic predicates,
  higher-order goals, cut interaction, and constraint search.
- DML's identity as a logic language.

### 12.2 What remains

- Deterministic orchestration with probabilistic, typed leaves.
- Typed LLM calls, schemas, and validation.
- Sequencing, branching, recursion, retries, fallbacks, and validation.
- Conversation memory, context scopes, and model/tool scoping.
- Host effects: tools, forms/human input, processes, media, streaming,
  cancellation, usage.
- Budgets, workspace isolation, approvals, and pi session ownership.
- Verification: effects, contracts, obligations, theorems, evidence.
- Authoring — but source-based rather than a live clause-editing REPL.

### 12.3 Revised feasibility

| Criterion | With logic programming (previous verdict) | Without logic programming |
|---|---:|---:|
| Semantic fit for Lean | 2/5 | 5/5 |
| Verification tractability | 3/5 | 5/5 |
| Runtime simplicity | 1/5 | 4/5 |
| Dynamic authoring | 1/5 | 2/5 (source/check/run) |
| Toolchain/install cost | 2/5 | 2/5 (unchanged) |
| LLM authoring | 2/5 | 3/5 (typed functional is easier than Prolog for many models) |
| Time to first value | 1/5 | 3/5 |

Dropping logic programming does not fix the toolchain, distribution, or dynamic
elaboration problems. It does remove the semantic mismatch, which was the deepest
problem. The remaining blockers are engineering and product constraints, not
"Lean cannot express this."

### 12.4 Execution options

**Option A — Lean all-in (AOT).** The plan is a Lean `Plan` value; `dc-check`
builds/proves it; `dc-run` executes a compiled artifact (native Lean or Lasm
WASM). Host effects are requested over a small JSON-line/stdio protocol; the host
(pi) performs LLM, tool, form, process, and media operations. Pure functions are
ordinary Lean functions and run in the compiled artifact.

- **Pros:** one language; full Lean pure functions; direct proofs; no IR/runtime
  semantic drift; no SWI-Prolog.
- **Cons:** Lean toolchain at authoring time; per-edit rebuild (native Lean or
  Lasm, ~1–2 s incremental after a heavy first build); Node 24/512 MB for Lasm,
  or platform-specific native binaries; source/check/run instead of a live REPL.

**Option B — Lean surface, IR execution.** Lean typechecks/proves the plan and
serializes an IR; a small TypeScript interpreter executes the IR. This avoids a
Lean runtime but forces a choice: either restrict pure functions to an
IR-compilable subset, or compile them separately (WASM) and call them from the
interpreter. Arbitrary Lean pure functions cannot be executed by a TS IR
interpreter without such a bridge.

- **Pros:** light runtime, zero-install `dc-run`, Lean proofs at authoring time.
- **Cons:** two representations; restricted pure language or a WASM bridge; the
  interpreter is in the TCB unless verified/extracted.

**Option C — DCL surface + Lean checker.** Keep a small, product-friendly surface
and TS runtime; Lean defines/verifies the IR. This is the previous recommendation.
It remains the lowest-risk path, but Lean is not the authoring language.

**Recommendation:**

- If "Lean as the language" is a hard requirement, choose **A**. Accept the
  toolchain and the source/check/run workflow. Do not emulate Prolog in Lean —
  that is the worst of both worlds.
- If "zero-install pi extension, fast in-process runtime" is a hard requirement,
  choose **C**.
- Choose **B** only if you are willing to maintain a restricted pure language or
  a WASM bridge and a separate IR interpreter.

### 12.5 What the Lean language sketch looks like

With logic programming gone, the language is a deterministic effect language with
Lean as its host. The core is a reified `Plan` free monad:

```lean
inductive Eff where
  | llm | human | fs | net | proc | time | rand | fail | state
  | tool (name : String)
  | domain (name : String)
  deriving DecidableEq, Repr

abbrev EffRow := List Eff

inductive Cmd (ε : EffRow) : Type → Type
  | llm : Prompt → Schema α → Cmd ε α
  | call : (c : Capability) → c.Args → Cmd ε c.Result
  | ask : FormSpec → Cmd ε Answer
  | emit : Event → Cmd ε Unit
  | fail : Error → Cmd ε α
  | retry : Policy → Cmd ε α → Cmd ε α
  | fallback : Cmd ε α → Cmd ε α → Cmd ε α
  | spawn : Cmd ε α → Cmd ε (Proc α)
  | send : Proc α → α → Cmd ε Unit
  | receive : Proc α → Cmd ε α
```

A plan is `Free (Cmd ε) α` (or an equivalent free monad). User code uses `do`
notation:

```lean
def issueRefund (claim : Claim) : Plan [.llm, .tool "refund_payment"] Outcome := do
  let d ← llm
    (prompt := s!"Review refund claim {claim.id} for {claim.amount}.")
    (schema := Decision)
  if d.approve then
    require (d.amount ≤ 1000)
    let r ← call "refund_payment" (claim.id, d.amount)
    ensure (r.status = .accepted)
    return .refunded d.amount
  else
    return .rejected d.reason
```

Pure logic becomes ordinary Lean:

```lean
def groupByCustomer (xs : List Order) : List (CustomerId × List Order) :=
  ...
```

This replaces most of DML's relational glue. For constraints, call a typed `solve`
capability (SMT/CLP) or use Lean's `omega`/`linarith` at proof time; do not build a
new CLP runtime.

### 12.6 Verification becomes easier

Without dynamic predicates, cut, higher-order goals, and constraint search, the
effect tree is a deterministic program tree. That means:

- `effects : Plan ε α → EffectSet` is a straightforward structural function;
- `may_effect`/`must_effect` are functions over the tree, not a meta-interpreter;
- `wp`/verification conditions are standard program logic;
- symbolic execution, `decide`, `omega`, and bounded model checking cover more
  properties;
- counterexamples are easier to produce and validate;
- the checker's soundness theorem is much easier to state and prove.

The remaining hard cases are the honest ones: unbounded loops/recursion,
concurrency, string/path reasoning, and the model/tool oracle boundary. Lean does
not make those disappear, but it no longer has to model Prolog first.

### 12.7 What you lose and must replace

| Lost | Replacement |
|---|---|
| Concise relational search | Lean functions, `List`/`Std` algorithms, or a typed `solve` tool |
| CLP(FD/Q/R) | External SMT/CLP capability, or proof-time `omega`/`linarith` |
| Dynamic facts / living module | Explicit typed `Store` effect, source edits, versioned modules |
| `assertz`/`retract` | Typed state/DB operations, not code mutation |
| Cut/backtracking | Explicit `retry`/`fallback`/`Result`; no effectful replay |
| Live REPL `define`/`execute` | Source edit → `dc-check` → `dc-run`; optional pure-expression REPL |
| Homoiconic analysis | Lean AST/IR analysis (still strong; `Plan` is reified) |
| Existing DML skills | Rewrite or translate; keep DML as a legacy frontend |
| SWI-Prolog WASM | Lean AOT artifact, or a small IR interpreter |

### 12.8 Revised verdict

Dropping logic programming changes the verdict from **"Lean is a poor execution
substrate"** to **"Lean is a credible primary language, if you accept the
toolchain."** The technical feasibility is much higher; the product trade-offs are
still real:

- **High-assurance developer tool / expert tier:** Lean all-in is now a coherent
  design. It gives the strongest verification story and the simplest semantics.
- **Zero-install pi extension for everyday skills:** still not the right default.
  Use a small surface + IR runtime, with Lean as the checker (Option C).
- **Hybrid:** start with C (IR + TS checker), build the Lean semantics, then add
  A as an opt-in high-assurance mode. This preserves the migration path and lets
  the team measure LLM authoring and proof automation before committing.

The one thing not to do is build a Lean runtime that emulates Prolog's search and
dynamic module. If logic programming is not valuable enough to keep, do not pay
its cost inside Lean.

---

## Appendix A — DML to DCL mapping

| DML | DCL | Notes |
|---|---|---|
| `agent_main/N` | `plan name(params) -> Result` | Explicit typed parameters/results |
| `task/N` | `llm(prompt, schema)` | Memory-reading leaf |
| `prompt/N` | `prompt(prompt, schema)` | Fresh-memory leaf |
| `llm/2` | `llm(messages)` | Raw text; no memory write |
| `chat/1,2` | `conversation` plan | Explicit loop |
| `exec/2` | typed `call tool(args)` | No stringly-typed tool names |
| `tool/N` | `tool` manifest + typed function | Tool contract is trusted |
| `system/1`, `user/1` | `remember(role, text)` | Typed memory |
| `with_fresh_memory` | `with_fresh_memory { ... }` | Handler |
| `with_model` | `with_model(spec) { ... }` | Handler |
| `with_tools` | `with_tools([...]) { ... }` | Capability scope |
| `with_reasoning` | `with_reasoning(level) { ... }` | Handler |
| `choose/rate/verify/probability` | `judge` command | Typed result; backend handler |
| `form/await_form` | `ask(form)` | Exhaustive outcome |
| `spawn/send/receive` | `spawn/send/receive` | Typed mailboxes; unverified initially |
| `output/answer/log` | `emit/return/fail` | Events/results |
| `assertz/retract` | `Store` effect | Explicit state, no code mutation |
| `!`, `;`, `\+` | `commit`, `fallback`, `not` | No implicit effectful backtracking |
| CLP | `search` + `solve` capability | Bounded/pure; external solver |
| `format/2` | string interpolation | Typed formatting |
| `schema/2` | `record`/`enum` + `Schema<T>` | Derived JSON schema |
| `script_file/run_script` | typed `call` to a script tool | Manifest contract |
| `use_harness` | `import` with version pin | Read-only |

## Appendix B — What a Lean proof can and cannot say

**Can say**

- "Every reachable path that calls `refund_payment` is guarded by
  `amount <= 1000`."
- "The declared effect set over-approximates the trace effects."
- "This plan never calls a tool outside its capability row."
- "This path stays inside the workspace."
- "This loop terminates under the given measure."
- "If the model returns a `Decision` satisfying the runtime-checked schema and
  `checked` predicate, then the plan's postcondition holds."
- "The checker accepts only plans whose obligations are proved."

**Cannot say**

- "The model's decision is correct."
- "The prompt is injection-free."
- "`bash` did what its command string says."
- "The payment provider accepted the refund."
- "The user's approval was informed."
- "The compiler/runtime is correct" (unless separately verified).

## Appendix C — Glossary

- **IR**: the typed effect tree shared by frontends, checker, and runtime.
- **Effect row**: the set of effects a plan may perform.
- **Capability**: a typed permission to call a host tool.
- **Oracle**: the model/host boundary that supplies `llm`/tool results.
- **Obligation**: a protected must-prove property.
- **Theorem**: a model/user-proposed property checked by the verifier.
- **Evidence**: the report/hash proving what was checked and under what
  assumptions.
- **Reified effect**: an effect represented as data in a command tree, not as an
  opaque `IO` action.
- **Proof-carrying plan**: a plan accompanied by evidence that its obligations
  were proved.
