# A DML swarm: simple API, BEAM model underneath

Status: **design target / worked example**. Supersedes the earlier
process-level draft. The high-level `swarm` combinator and the process
primitives it is built on are **not shipped**; `task/N`, `verify/3`,
`maplist/3`, `format/2` are real DML.

The lesson of the first draft was that we were asking *authors* (and agents) to
think about pids, mailboxes, links, child specs, restart intensity and attempt
plumbing. Authors should not. They should write one worker predicate and one
call. The process model belongs in a bundled library, exactly as OTP belongs
under `gen_server`.

---

## 0. The whole swarm

```prolog
:- use_module(library(swarm)).

agent_main(Topic, Report) :-
    task("Break {Topic} into 3 to 6 independent subtopics. Each must stand alone.",
         list(string(Subtopics))),

    swarm(research_subtopic, Subtopics, Results,
          [concurrency(4), attempts(2), verify(well_sourced)]),

    task("Write a concise report answering {Topic}. Use only these findings, "
         "attribute each to its subtopic, and flag any failed subtopic. "
         "Findings: {Results}",
         string(Report)),
    answer(Report).

research_subtopic(Subtopic, Finding) :-
    task("Research '{Subtopic}'. Give 2 to 4 concrete, checkable claims, each "
         "with a named source. State only what you can support.",
         string(Finding)).

well_sourced(_Subtopic, Finding) :-
    verify(Finding,
           "Does this finding contain at least one specific, checkable claim "
           "and at least one named source?",
           yes).
```

That is the whole program: one planner `task`, one `swarm`, one synthesis
`task`. No pids. No `send`/`receive`. No supervisor. No `exit`. No `catch`. No
attempt threading. The author writes **one predicate per unit of work** and one
quality gate; the runtime does the rest.

---

## 1. The `swarm` API

```prolog
swarm(+Worker, +Items, -Results)                  % defaults
swarm(+Worker, +Items, -Results, +Options)
pmap(+Worker, +Items, -Values)                    % fail-on-any-error sugar
```

- **`Worker`** is a closure called as `call(Worker, Item, Value)`. It runs in its
  **own process**, with **fresh memory** and a **private snapshot** of the current
  module. It may call `task/N`, `prompt/N`, `exec/2`, judgments — anything — as
  if it were the only code running.
- **Results** is aligned with `Items`, in input order, regardless of completion
  order:
  ```prolog
  [ ok(Item, Value) | failed(Item, Reason) ]
  ```
  The call itself always succeeds in the default mode, so a partial swarm is data,
  not an exception.
- **Failure is per item.** A worker that fails, throws, or fails its `verify`
  produces `failed(Item, Reason)`; siblings are unaffected.

### Options

| Option | Default | Meaning |
|---|---|---|
| `concurrency(N)` | `4` | at most `N` workers doing model/tool work at once |
| `attempts(N)` | `1` | tries per item before it is reported `failed` |
| `verify(Closure)` | none | `call(Closure, Item, Value)` after the worker; failure ⇒ this attempt failed |
| `retry(Closure)` | none | `call(Closure, Item, Attempt, Reason, Value)` used for attempts ≥ 2 (informed retry) |
| `item_timeout(Ms)` | `300000` | per-attempt budget |
| `total_timeout(Ms)` | `600000` | whole-swarm budget; stragglers are cancelled |
| `on_error(collect\|fail\|throw)` | `collect` | `collect` returns `failed(...)`; `fail` fails the call; `throw` raises |
| `usage(U)` | — | total model usage across all items and retries |

### Why this shape

- **Agents compose better with data than with control.** `ok/failed` is
  inspectable and partial, so the synthesis step can be told "flag any failed
  subtopic" instead of a human writing error plumbing.
- **`verify` is the common case.** Most agentic fan-out needs a quality gate.
  Making it an option removes the single largest chunk of the first draft.
- **Retries are attempts, not restarts.** An author does not need a supervisor
  process to retry a failed subtask; the library loops internally. Supervision is
  only needed for long-lived *services*, which is a different pattern (§5).
- **The module stays a single writer.** Each worker gets a snapshot; only the
  coordinator mutates. No locks, no races, no `with_fresh_memory` boilerplate.

---

## 2. The library (so the simplicity is real)

`library(swarm)` is ordinary DML built on the process primitives. This is the
only place pids and mailboxes appear. It is a sketch, but every operation it uses
is from the model surface.

```prolog
:- use_module(library(lists)).

%% swarm/3,4 -----------------------------------------------------------
swarm(Worker, Items, Results) :- swarm(Worker, Items, Results, {}).

swarm(Worker, Items, Results, Options) :-
    opt(concurrency,   Options, Conc,    4),
    opt(attempts,      Options, Attempts, 1),
    opt(verify,        Options, Verify,  none),
    opt(on_error,      Options, OnError, collect),
    self(Parent),
    semaphore(Conc, Sem),                       % bound concurrent WORK, not spawns
    Ctx = job{worker: Worker, verify: Verify, attempts: Attempts,
              sem: Sem, parent: Parent},
    maplist(spawn_job(Ctx), Items, _Pids),
    length(Items, N),
    collect(N, Pairs),
    reorder(Items, Pairs, Ordered),
    apply_on_error(Ordered, OnError, Results).

%% One process per item. Cheap because most of its life is suspended on I/O.
spawn_job(Ctx, Item, Pid) :-
    spawn_opt(swarm_job(Ctx, Item), Pid, [memory(fresh)]).

swarm_job(Ctx, Item) :-
    self(Self),
    sem_acquire(Ctx.sem),
    swarm_attempt(Ctx, Item, 1, Outcome),
    sem_release(Ctx.sem),
    send(Ctx.parent, {swarm_done, Self, Item, Outcome}).

%% The retry loop is INSIDE the process, so it keeps one identity and one
%% attempt counter. A failure is caught, never propagated as a crash.
swarm_attempt(Ctx, Item, Attempt, Outcome) :-
    catch(call(Ctx.worker, Item, Value), Error, Value = nil),
    (   var(Error),
        ( Ctx.verify == none ; call(Ctx.verify, Item, Value) )
    ->  Outcome = ok(Value)
    ;   Attempt < Ctx.attempts
    ->  Next is Attempt + 1,
        swarm_attempt(Ctx, Item, Next, Outcome)
    ;   Outcome = failed(reason_of(Error, Ctx.verify, Item, Value))
    ).

collect(0, []) :- !.
collect(N, [Item-Outcome|Rest]) :-
    receive_match({swarm_done, _, Item, Outcome}, _, 0),   % 0 = wait for the next
    N1 is N - 1,
    collect(N1, Rest).

reorder([], _, []).
reorder([Item|Items], Pairs, [Outcome|Results]) :-
    selectchk(Item-Outcome, Pairs, Rest),
    reorder(Items, Rest, Results).

apply_on_error(Ordered, collect, Ordered).
apply_on_error(Ordered, fail,    Ordered) :- forall(member(R, Ordered), R = ok(_,_)).
apply_on_error(Ordered, throw,   Ordered) :-
    (   member(failed(Item, Reason), Ordered) -> throw(swarm_failed(Item, Reason))
    ;   true
    ).

opt(Key, Options, Value, Default) :-
    (   is_dict(Options), get_dict(Key, Options, Value) -> true
    ;   Value = Default
    ).

%% pmap/3: values only; fails if any item failed.
pmap(Worker, Items, Values) :-
    swarm(Worker, Items, Results, {on_error: fail}),
    findall(V, member(ok(_, V), Results), Values).
```

A counting semaphore (a small process) bounds *work* while `spawn` stays cheap,
so `concurrency(4)` over `N=50` items does not mean 50 simultaneous model calls.
`spawn_opt(..., [memory(fresh)])` is the entire isolation story.

### What moved out of author code

| First draft (author) | Now (library) |
|---|---|
| `self/1`, `spawn_link`, child specs | `spawn_opt` in `spawn_job` |
| supervisor process + restart intensity | `attempts` loop in `swarm_attempt` |
| `exit(verification_failed(...))` | `catch/3` + `verify` option |
| attempt counter threaded through closures | `swarm_attempt` recursion |
| gather loop + dedup by subtopic | `collect/2` + `reorder/2` |
| planning/synthesizing pids | none — planner and synthesizer are plain `task/N` |

The author's surface went from ~120 lines and six concepts to ~15 lines and two
concepts (`swarm`, a quality gate).

---

## 3. Common patterns

**Retry with feedback.** The retry can see *why* the previous attempt failed:

```prolog
swarm(research_subtopic, Subtopics, Results,
      [attempts(3), verify(well_sourced), retry(refine_subtopic)]).

refine_subtopic(Subtopic, Attempt, Reason, Finding) :-
    task("Attempt {Attempt} at '{Subtopic}' failed because '{Reason}'. "
         "Retry with narrower, better-sourced claims.", string(Finding)).
```

**Best-of-N.** Ask for N independent answers and let a judge choose; the extra
processes are just `swarm` over a list of copies.

```prolog
best_of(N, Question, Answer) :-
    length(Samples, N),
    swarm(prompt_answer(Question), Samples, Results),
    findall(A, member(ok(_, A), Results), Answers),
    choose("Which answer is best?", Answers, Answer).

prompt_answer(Question, _Sample, Answer) :-
    task("Answer carefully: {Question}", string(Answer)).
```

**Recursive fan-out.** A worker may itself call `swarm` — each level is a fresh
snapshot, so recursion is safe; a depth argument is the only guard.

```prolog
research_deep(Subtopic, Depth, Finding) :-
    task("Is '{Subtopic}' narrow enough to answer directly?", boolean(Simple)),
    (   Simple == true ; Depth >= 3
    ->  research_subtopic(Subtopic, Finding)
    ;   task("Split '{Subtopic}' into 2 to 4 sub-questions.", list(string(Subs))),
        swarm(research_deep_1(Depth), Subs, Results),
        task("Combine {Results} into one finding about '{Subtopic}'.", string(Finding))
    ).
```

**A supervised service (the OTP case).** Long-lived processes that must restart
on crash still use explicit supervision. That is a different, rarer pattern and
is covered in §5.

---

## 4. `{a,b,c}` and message shapes

Short answer: **yes, `{a,b,c}` is a valid term**, but it is not what it looks
like.

Verified on the shipped engine:

```prolog
?- functor({a,b,c}, F, A).
F = {}, A = 1.

?- {a,b,c} = {X}, X = (A,B,C).
A = a, B = b, C = c.
```

`{a,b,c}` is the standard brace term — `'{}'(','(a,','(b,c)))` — i.e. functor
`{}` of arity **1** wrapping the conjunction `(a,b,c)`. Consequences:

- Pattern matching is symmetric, so `{finding, S, F}` matches `{finding, S, F}`
  and destructures as expected. The first draft's messages worked.
- But `functor/3` reports `{}/1`, `=/2` sees a conjunction inside, and it is easy
  to confuse with **dicts**, which are a different thing (`point{x:1}`).
- `a{1}` is a **syntax error** — dict keys require `key:value` — which makes the
  brace forms look more uniform than they are.

**Recommendation.** Do not use bare braces as tuples in a public API. Use one of:

```prolog
finding(Subtopic, Text)                       % tagged term: self-describing, arity real
finding{subtopic: Subtopic, text: Text}       % dict: named fields, get_dict/3 is readable
```

In `library(swarm)` this point is moot: the author never sees message terms at
all. The library is free to use dicts internally; `swarm`'s results are the
small, explicit `ok/2` / `failed/2` terms.

---

## 5. When you need raw processes

`swarm` covers fan-out-over-data with a quality gate. Reach for the process
primitives only when the shape is genuinely a *service* or a *custom protocol*:
long-lived loops, request/response without a terminating item list, or a
dependency graph between processes.

The essentials (unchanged from the model sketch):

```prolog
self(Pid)                       spawn(Goal, Pid)        spawn_link(Goal, Pid)
spawn_opt(Goal, Pid, Opts)      spawn_monitor(Goal, Pid, Ref)
send(Pid, Msg)                  receive(Msg[, TimeoutMs])
receive_match(Template, Msg, TimeoutMs)
exit(Reason)  exit(Pid, Reason)  exit(Pid, kill)
link(Pid)  unlink(Pid)  monitor(Pid, Ref)  demonitor(Ref)
process_flag(trap_exit, Bool)
register(Name, Pid)  whereis(Name, Pid)
process_info(Pid, Info)  self_usage(U)
code_version(V)  reload
```

A supervised worker service, condensed:

```prolog
%% child(Id, Goal, Restart, ShutdownMs); Goal is called as call(Goal) and
%% should run IN the supervised process. Restart: permanent | transient |
%% temporary. (Attempt-aware retry is a `swarm` concern, not a supervisor
%% concern: a supervised process owns its own retry loop.)
supervisor_start(Specs, supervision{strategy: one_for_one,
                                    max_restarts: 6, max_seconds: 90}, SupPid).
supervisor_stop(SupPid, Reason).
```

Rule of thumb for the authoring guide:

> To run work over a list, use `swarm`/`pmap`. Use processes directly only when a
> unit of work does not have an item to return from, or must stay alive.

### A service loop, if you must

```prolog
%% A process that serves requests forever; the caller uses monitor to know
%% when it is gone.
counter_service() :-
    process_flag(trap_exit, true),
    counter_loop(0).

counter_loop(N) :-
    receive(Msg),
    handle_counter(Msg, N, NNext),
    ( NNext == stopped -> true ; counter_loop(NNext) ).

handle_counter({inc, A, From, Ref}, N, NNext) :-
    NNext is N + A, send(From, {value, Ref, NNext}).
handle_counter({stop}, _, stopped).
handle_counter({'EXIT', _, _}, _, stopped).
```

---

## 6. What the runtime must provide for `swarm`

The combinator only needs:

1. **`spawn_opt`** — new session/module snapshot/frame/mailbox, fresh memory,
   reusing `create_frame`/`createNestedFrame`.
2. **Copy-on-send** — serialize terms (`write_canonical`/`read_term`) even
   in-process, so isolation is real, not accidental.
3. **`receive_match/3`** — selective receive with a timeout, unifying in the
   owner's context.
4. **`catch/3` isolation** — an error inside one process is a term, not a crash of
   the run.
5. **A scheduling quantum** — `call_with_inference_limit/3` between yields, so one
   greedy item cannot starve the others.
6. **A concurrency budget** — the semaphore above, or a runtime `concurrency`
   limit shared with `maxBackground`.

Items 1–4 are the process table, mailbox, and exit/link machinery from the
design doc; `library(swarm)` is a thin, portable DML layer on top. No SWI changes
are required.

---

## 7. L0 / L1 / L2

`swarm` is written once and runs at any isolation level; only the pid's node
differs.

| | L1 (cooperative, one WASM) | L2 (worker / process) |
|---|---|---|
| `spawn_opt` | new session + frame | new worker instance |
| `send` | copied term | serialized over the worker protocol |
| host I/O | yielded to the main loop | proxied to the main thread |
| CPU-bound item | blocks the instance | truly parallel |
| `exit(kill)` | best-effort between steps | `worker.terminate()` |
| cost | cheap | one WASM instance per worker |

For a swarm that is almost entirely model I/O — which agent swarms are — L1 is
enough and gives the overlap the responder wants. L2 is for CPU-heavy or
hard-cancellable items.

---

## 8. Gotchas

- **Unbounded model calls.** `concurrency` bounds work, but `spawn` is still O(N).
  For very large `N`, window the spawns or cap `maxBackground`.
- **Deterministic retries.** A verification that always fails will retry `attempts`
  times and then report `failed`. That is correct, but prefer `retry/1` for
  informed second attempts.
- **Copy cost.** Large items/values are copied into and out of processes. Pass
  paths/findings, not documents.
- **`receive` without a timeout** is how services hang; every request/response in
  a service loop needs one.
- **Orphans.** A process not linked to the session root outlives cancellation.
  `swarm` links its jobs to the run; hand-written services must do the same.
- **Braces.** Use tagged terms or dicts for anything public; do not ship
  `{a,b,c}` as a record.

---

## 9. Summary

The BEAM model is the right substrate for DML: agentic work is many independent,
I/O-bound, individually failable subtasks. But authors should meet that model as
**one combinator over a list**, not as pids and mailboxes. `swarm/4` gives them
concurrency, isolation, retries, a quality gate, ordered partial results, and
usage accounting — while the process model powers it underneath and stays
available for the rare service-shaped problem.
