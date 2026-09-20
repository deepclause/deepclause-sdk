# Known issue: judge predicate names shadow user-defined predicates

**Status:** open. Affects 0.0.89 and later.

## Summary

The judgment layer registers its DML predicates globally as special predicates,
and `is_mi_special_predicate/1` is consulted before user dispatch. Any
user-defined DML predicate with the same name and arity is therefore shadowed:
the meta-interpreter routes the goal to the judge handler instead of the user's
clauses, usually silently.

Registered judge special predicates (`src/prolog-src/deepclause_mi.pl`):

```prolog
is_mi_special_predicate(judge(_,_)).
is_mi_special_predicate(choose(_,_,_,_)).
is_mi_special_predicate(rate(_,_,_,_)).
is_mi_special_predicate(verify(_,_,_)).
is_mi_special_predicate(probability(_,_,_)).
is_mi_special_predicate(holds(_,_)).
is_mi_special_predicate(holds(_,_,_)).
is_mi_special_predicate(with_judgment(_,_)).
is_mi_special_predicate(require_judgment(_,_)).
```

The batch spec functors (`choose/2`, `rate/2`, `verify/1`, `probability/1`)
are data and do not collide by arity; the hazard is the one-off predicates,
whose names are common enough to appear in ordinary DML.

## Impact

- `deepclause-pi` `src/assets/apply.dml` defined a local `verify/3`. On SDK
  0.0.89 the apply flow called the judge `verify/3` instead, sending a
  judgment request and failing to produce an answer. Fixed by renaming the user
  predicate to `verify_task/3`.
- Any user skill that defines `verify/3`, `choose/4`, `rate/4`,
  `probability/3`, `holds/2`, `holds/3`, `judge/2`, `with_judgment/2`, or
  `require_judgment/2` is shadowed, often with no diagnostic.

## Root cause

`is_mi_special_predicate/1` is a global dispatch table. The catch-all
`mi_call/3` skips user dispatch for any goal matching it, and it is not scoped
to a module or to sessions that actually use judgments. There is no check that
a same name/arity predicate is defined by the user.

## Proposed fixes

### Option A — namespace the judge predicates (preferred)

Use a reserved prefix for the executable predicates:

```prolog
judge_batch(State, Specs)          % was judge/2
judge_choose(State, Question, Options, Choice)
judge_rate(State, Question, Levels, Level)
judge_verify(State, Question, Truth)
judge_probability(State, Question, P)
judge_holds(State, Question)
judge_holds(State, Question, Threshold)
judge_with(Backend, Goal)          % was with_judgment/2
judge_require(Capability, Goal)    % was require_judgment/2
```

This cannot collide with realistic user predicates. It is a breaking change to
the DML surface and needs a minor release plus updated docs, examples, and
consumers (including `deepclause-pi`).

### Option B — shape-aware dispatch

Only treat a name as a judge call when the arguments match the judge shape
(e.g. `verify/3` with an unbound third argument and a state-like first
argument). Ambiguous and fragile; not recommended.

### Option C — session-scoped opt-in

Require the program or workspace to enable the judge predicates explicitly
(e.g. a directive or a `use_judgments` call), so they never shadow by default.
Adds friction but removes the surprise.

### Option D — detect and warn (immediate mitigation)

When consulting a user clause whose name/arity matches a registered special
predicate, emit a clear warning. Cheap and complementary to A.

## Acceptance criteria

- A user-defined predicate named `verify/3` (or any judge name) is callable and
  returns its own result unless the program explicitly opts into the judge
  surface.
- The judge surface remains available under names that cannot collide with
  ordinary DML.

## Tests to add

- Regression: a user predicate `verify/3` runs its own clauses and is not
  intercepted by the judge layer.
- A warning test for the shadowing case (Option D).
- `deepclause-pi` apply flow (`tests/apply.test.ts`) as an integration check.

## Related

- `docs/META_INTERPRETER_CUTS.md` — the other meta-interpreter limitation
  exposed by the 0.0.88 backtracking fix.
- `is_mi_special_predicate/1` and `mi_call/3` in
  `src/prolog-src/deepclause_mi.pl`.
- `deepclause-pi` `src/assets/apply.dml` (`verify/3` -> `verify_task/3`).
