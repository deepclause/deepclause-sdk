# Known issue: `!` (cut) in user-defined DML predicates is not honored

**Status:** open. Affects all current SDK versions. Exposed by the
meta-interpreter backtracking fix shipped in 0.0.88 and later.

## Summary

The DML meta-interpreter does not implement Prolog cut semantics for
user-defined predicates. A `!` inside a DML predicate body is executed by
`mi_call(!, ...)`, so it cuts choice points of `mi_call/3` rather than the
clause choice created by `clause(SessionId:Goal, Body)` in
`mi_call_dispatch/3`. As a result the predicate behaves as if the cut were
absent: on backtracking it still tries later clauses and can return multiple
answers where a cut was intended to commit to one.

## Impact

- Any user-defined DML predicate that relies on `!` for determinism becomes
  nondeterministic.
- `findall/3`, `bagof/3`, and `setof/3` collect duplicate or spurious
  solutions because `mi_call/3` reimplements `findall` over `mi_call(Goal, ...)`.
- Recursive parsers that use cuts to stop can blow up combinatorially. This is
  how the issue was found: the `deepclause-pi` spec library (`specs.dml`)
  produced 256 duplicate parses and 1048 spurious errors on SDK 0.0.89.
- Code that was developed against earlier SDKs can silently change behavior on
  upgrade.

Built-in and imported predicates are unaffected because `mi_call_dispatch/3`
calls them directly with `call(Goal)`; only user-defined predicates go through
`clause/2` + `mi_call(Body, ...)`.

## Root cause

`mi_call/3` is the interpreter entry point for every goal. User-defined
predicates are dispatched in `mi_call_dispatch/3` (`src/prolog-src/deepclause_mi.pl`):

```prolog
mi_call_dispatch(Goal, StateIn, StateOut) :-
    get_session_id(SessionId),
    callable(Goal),
    predicate_property(SessionId:Goal, defined),
    (   (predicate_property(SessionId:Goal, imported_from(_)) ; predicate_property(SessionId:Goal, foreign))
    ->  StateOut = StateIn,
        call(SessionId:Goal)
    ;   get_depth(StateIn, Depth),
        Goal =.. [Functor|Args],
        add_trace_entry(SessionId, call, Functor, Args, Depth),
        clause(SessionId:Goal, Body),          % <-- clause choice point lives here
        NewDepth is Depth + 1,
        set_depth(StateIn, NewDepth, State1),
        mi_call(Body, State1, State2),         % <-- body is interpreted here
        add_trace_entry(SessionId, exit, Functor, Args, Depth),
        set_depth(State2, Depth, StateOut)
    ).
```

and the cut handler is:

```prolog
mi_call(!, StateIn, StateIn) :-
    !.
```

`clause(SessionId:Goal, Body)` creates the choice point over the predicate's
clauses. `Body` is then interpreted by a recursive `mi_call/3` call. A cut in
`Body` reaches `mi_call(!, ...)`, whose `!` cuts alternatives of that
`mi_call/3` invocation. The `clause/2` choice point belongs to
`mi_call_dispatch/3`, a different predicate invocation, so it is not cut and
prolog will still try the remaining clauses on backtracking.

## Reproduction

```prolog
first(X) :- X = 1, !.
first(X) :- X = 2.

agent_main :-
    findall(X, first(X), Xs),
    answer(Xs).
```

- Standard Prolog: `Xs = [1]`.
- Current meta-interpreter: `Xs = [1, 2]`.

A test for this lives in `tests/meta-interpreter-cuts.test.ts` and is marked
with `it.fails` so the suite stays green until the issue is fixed. Flip it to a
normal `it` as part of the fix.

## History

- Before 0.0.88, `mi_call_dispatch/3` wrapped the interpreted body in an
  if-then-else that committed to the first solution:
  ```prolog
  (   mi_call(Body, State1, State2)
  ->  add_trace_entry(SessionId, exit, Functor, Args, Depth),
      set_depth(State2, Depth, StateOut)
  ;   add_trace_entry(SessionId, fail, Functor, Args, Depth),
      fail
  )
  ```
  This made every user predicate deterministic and hid all latent cut reliance,
  but it also broke genuine backtracking: `findall/3` over a helper rule
  returned only the first solution.
- The 0.0.88 backtracking fix removed that wrapper, restoring backtracking and
  exposing this cut limitation.
- 0.0.89 inherited the fix. `deepclause-pi` hit the issue when it upgraded.

Note: a soft cut (`*->`) instead of the plain body does not help; the cut
inside `Body` is still interpreted by `mi_call/3`. Both forms fail the same way.

## Workaround for library authors

Rewrite predicates that rely on `!` to use if-then-else, which `mi_call/3`
implements itself (and therefore commits correctly):

```prolog
% Instead of:
take([line(Line, _)|Rest], [], [line(Line, _)|Rest]) :-
    heading(Line, Level), Level >= 1, Level =< 3, !.
take([Item|Rest], [Item|Body], Tail) :- take(Rest, Body, Tail).

% Write:
take([], [], []).
take([Item|Rest], Body, Tail) :-
    (   Item = line(Line, _), heading(Line, Level), Level >= 1, Level =< 3
    ->  Body = [], Tail = [Item|Rest]
    ;   Body = [Item|Body1], take(Rest, Body1, Tail)
    ).
```

This is the workaround applied to `deepclause-pi` `src/assets/specs.dml`
(PR `feat/judge-integration`): `sp_skip_plain`, `sp_take_body`,
`sp_take_plain`, `sp_take_until_level4`, `sp_drop_to_op`, and
`sp_take_until_op` were rewritten from cut-based to if-then-else.

`once/1` is also usable as a blunt commit for "first solution of this
predicate" cases, because it is a built-in and calls its goal directly.

## Proposed fixes

### Option A — implement cut semantics in the meta-interpreter (preferred)

The classic meta-interpreter cut problem. Faithful approaches:

1. **Continuation / exception unwinding.** Preprocess each user clause so that
   cuts become an explicit signal (e.g. a distinguished exception) that is
   caught at the clause-dispatch boundary, where it commits to the current
   clause by cutting the `clause/2` choice point. This requires respecting cut
   scope: a cut commits to the current clause and the goals to its left, and
   must not escape `\+`, disjunction alternatives, or a nested clause.
2. **`call/1` for pure predicates.** Mark clauses/predicates that contain `!`
   (statically, or on first use) and execute those bodies with real `call/1`
   semantics instead of `mi_call/3`, so native cut behavior applies. The
   trade-off is that special predicates (`task/N`, `prompt/N`, `llm/2`,
   `exec/2`, state-threading list predicates) inside such predicates would not
   be intercepted. Predicates that use cuts are usually pure (parsers,
   helpers), so this is often acceptable, but it is a real semantic
   restriction and must be documented and enforced (e.g. reject if the body
   contains a special predicate).

### Option B — compile special predicates instead of interpreting them

Rewrite `task/N`, `exec/2`, etc. into ordinary wrapper calls that can be
executed by `call/1`, then execute user bodies with native Prolog semantics.
This gives correct cut behavior for free and is the robust long-term fix, at
the cost of a larger change to how DML is loaded/executed.

### Option C — document and warn (immediate mitigation)

Add a static analysis warning when a user-defined predicate contains `!`
("cut is not honored in user predicates; use if-then-else or once/1"). Cheap
and prevents silent surprises while A/B are scheduled.

### Rejected — restore the first-solution `->` wrapper

This would make every predicate deterministic again but reintroduces the
backtracking bug the wrapper was removed to fix, and it makes
`findall/3` over helper rules return only one solution. Not acceptable.

## Acceptance criteria

When fixed, these must hold (standard Prolog semantics):

- `findall/3` over a predicate with a cut returns only the solutions up to the
  cut.
- A cut commits to the current clause and does not leak out of `\+` or a
  disjunction branch.
- A cut in an if-then-else condition behaves as in Prolog.
- `once/1`, `->`, `*->`, and `\+` keep working.
- Special predicates (`task/N`, `prompt/N`, `llm/2`, `exec/2`, and the
  state-threading `findall/3`) still thread state.

## Tests to add

- `tests/meta-interpreter-cuts.test.ts` — currently `it.fails`; flip to `it`
  when fixed.
- Additional cases for cut scope: `( X = a, ! ; X = b )` followed by another
  clause; `\+ (member(X, [1,2]), !)`; a cut inside a nested helper predicate.
- A regression that `findall/3` over a helper rule still backtracks through all
  solutions (guards against reintroducing the old `->`).

## Related

- `mi_call/3`, `mi_call_dispatch/3`, `is_mi_special_predicate/1`,
  `mi_call(!, ...)` in `src/prolog-src/deepclause_mi.pl`.
- `docs/DML_REFERENCE.md` — "Cut" section.
- `deepclause-pi` `src/assets/specs.dml` and PR `feat/judge-integration`
  (workaround).
