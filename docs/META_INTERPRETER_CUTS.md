# Known issue: `!` (cut) in user-defined DML predicates is not honored

**Status:** fixed. Cuts are honoured in both pure and interpreted predicates (see
[Resolution](#resolution)); the interpreted case compiles the cut instead of
running the body natively. Exposed by the meta-interpreter backtracking fix
shipped in 0.0.88 and later.

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
- Before the fix: `Xs = [1, 2]`.
- Now: `Xs = [1]`.

`tests/meta-interpreter-cuts.test.ts` covers this case as a normal `it`, plus
clause commit (an effectful clause does not replay), `findall/3`, `call/1`,
`not/1`, disjunction, a cut typed as a query, and the lowered (interpreted)
variants that call `output/1`.

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

## Resolution

Implemented: **native execution for pure cut predicates** (Option A.2).

`mi_call_dispatch/3` asks `mi_native_cut_predicate/2` before interpreting a
session clause. When the predicate contains a cut and is *pure*, the dispatch
calls `call(SessionId:Goal)` instead of decomposing the body, so the cut is a
real Prolog cut: bindings are kept, the clause commits, and `\+`, `call/1`,
`findall/3` and if-then-else scope it exactly as Prolog does. A pure goal typed
directly as a query (or passed to `execute`) is run the same way, so
`p(X), !` commits the query.

"Pure" is computed transitively (`mi_body_is_pure/2`, `mi_predicate_purity/3`):
a predicate is impure if it — or anything it calls — needs state threading
(`task/N`, `prompt/N`, `llm/2`, `exec/2`, forms, processes, `system/1`, `output/1`,
…). Native-safe constructs (`;`, `->`, `\+`, `!`, `findall/3`, `call/N`,
`once/1`, `catch/3`, `assertz/1`, …) do not make a predicate impure; their
arguments are still checked.

Both decisions are cached (`mi_cut_class/3`, `mi_purity/3`) and cleared whenever
the harness changes (`load_code/3`, `repl_define/4`, `repl_delete_predicate/4`,
`repl_rollback/3`, `repl_snapshot_restore/1`, `assert[a]`/`retract[all]`).
Measured overhead on cut-free recursive code is negligible (slightly faster than
before in the current build).

### Interpreted predicates: cut lowering

A predicate that needs the interpreter (it calls `task/N`, `exec/2`, `output/1`,
forms, processes, …) cannot run natively, so its cut is **compiled away** before
the body is interpreted:

```prolog
!         ->  '$dc_cut_flag'                    % commit the clause
( L, !, R ) ->  '$dc_cut_commit'(L), R          % once(L), then commit
( L ; R )   ->  ( L' ; '$dc_cut_guard'(R') )    % pruned when L cut
\+ G, call(G), findall(_,G,_), (C->T;E)  ->  G keeps its own cut scope
```

`'$dc_cut_commit'(L)` is `once(L)` plus the commit marker. `once/1` is already
interpreter-aware, so bindings and the threaded state behave exactly as with a
real cut. The marker itself is a per-call, non-backtrackable fact
(`mi_cut_done/1`) because "do not try later clauses" must survive backtracking,
which the threaded state cannot do; the call id travels in the state so nested
calls and scopes cannot collide.

Measured behaviour is now ISO for: clause commit, effectful clauses (no replay),
`findall/3`, `call/1`, `\+`/`not/1`, disjunction, if-then-else, mid-body cuts
(post-cut goals still enumerate), and cuts typed directly as a query. Tool bodies
are lowered the same way. Cut-free interpreted calls take the old path unchanged,
so there is no measurable overhead.

Remaining known gap: a cut is only visible where a *clause body* is interpreted
or a body/query is lowered, so a cut inside a hand-written closure passed to
`call/N` at runtime is still inert. Use `once/1` there.

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
