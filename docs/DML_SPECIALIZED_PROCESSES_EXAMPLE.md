# A DML desk: specialized processes

Status: **design target / worked example**. Companion to
[`DML_BEAM_SWARM_EXAMPLE.md`](./DML_BEAM_SWARM_EXAMPLE.md). The process
primitives and `library(service)` are **not shipped**; `task/N`, `verify/3`,
`maplist/3` are real DML.

The swarm example was *homogeneous*: one worker predicate over a list. This
example is the other shape — a small, fixed set of **specialized, long-lived
processes**, each with its own role, state, and message protocol, wired into a
pipeline. There is exactly one fan-out (the research pool), and it is a **pool
with a queue**, not a `maplist`.

Reach for this shape when the work is not "map a function over data" but "a
system of roles": a planner, a quality gate, a single writer, a recorder, a
dispatcher.

---

## 1. What the desk does

```
editor (root, coordinator)
│
├── supervisor (one_for_one, 10 restarts / 60 s)
│   ├── planner    permanent   state: style guide          call {plan, Brief} -> plan(Sections, Style)
│   ├── checker    permanent   stateless quality gate      call {check, Notes} -> [checked(Note, ok|rejected)]
│   ├── scribe     permanent   state: style + memory        call {draft, …} / {assemble, …}
│   └── ledger     permanent   state: events + usage        call usage; cast {record, …}
│
└── research_pool   (custom dispatcher; traps exits; owns a queue)
    ├── researcher  linked loop ── task/N ──► notes
    ├── researcher  linked loop
    └── researcher  linked loop
```

Data flow, stage by stage, for each section:

```
editor ──{request, Ref, Question}──► research_pool ──{research, Q}──► researcher
researcher ──task/N──► notes
researcher ──{done, Self, Notes}──► pool ──{reply, Ref, Notes}──► editor
researcher ──{add_usage, U}──► ledger
editor ──{check, Notes}──► checker ──verify/3──► checked notes
editor ──{draft, Title, Checked}──► scribe ──task/N──► prose
editor ──{assemble, Drafts}──► scribe ──► article
```

Each arrow is a copied message. No process shares a variable, a module, or a
memory with another. The only shared *name* is the registry (`planner`,
`checker`, `scribe`, `ledger`, `research_pool`).

### Why each one is a process

| Process | Role | Why a process, not a predicate |
|---|---|---|
| `planner` | plan + style | stable name; single owner of the style the scribe needs; restartable |
| `checker` | verification | serializes judge calls (rate limits) and centralizes the quality policy |
| `scribe` | drafting | **single writer**: one process gives a consistent voice and shared style memory across sections |
| `ledger` | recording + usage | accumulating state that outlives any single call |
| `research_pool` | dispatch + workers | owns concurrency, a work queue, and worker lifecycle |
| `editor` | orchestration | the root; owns the brief and the final assembly |

The planner and checker are the weakest cases for a process (they are close to
pure functions); they earn their place through a stable name and centralized
policy. The scribe and ledger are the strongest: their state is exactly why the
process must persist.

---

## 2. `library(service)`: a minimal `gen_server`

Most of these processes are plain request/response services. One small library
makes them readable; the dispatcher writes its own loop because it is not a
request/response service.

```prolog
%% =====================================================================
%%  library(service) — a minimal gen_server for the process model
%% =====================================================================
%%  service_init(+Name, +InitState, +Handler)   % run IN the service process
%%    registers Name, then loops. Handler is
%%      call(Handler, Message, State0, Reply, State1)
%%    where Reply = reply(Value) sends {reply, Ref, Value} to a caller of
%%    {request, From, Ref, Message}, and Reply = noreply sends nothing.
%%
%%  service_call/3,4   synchronous request -> Value (service_timeout/2 on expiry)
%%  service_cast/2     fire-and-forget
%% =====================================================================

service_init(Name, Init, Handler) :-
    self(Pid),
    register(Name, Pid),
    format("[service ~w/~w] up~n", [Name, Pid]),
    service_loop(Name, Handler, Init).

service_start(Name, Init, Handler) :-
    spawn(service_init(Name, Init, Handler), _Pid).

service_loop(Name, Handler, State) :-
    receive(Msg),
    (   Msg = {request, From, Ref, Request}
    ->  call(Handler, Request, State, Reply, State1),
        (   Reply = reply(Value) -> send(From, {reply, Ref, Value}) ; true ),
        service_loop(Name, Handler, State1)
    ;   Msg = {cast, Cast}
    ->  call(Handler, Cast, State, _, State1),
        service_loop(Name, Handler, State1)
    ;   Msg = {'EXIT', _, _}
    ->  service_loop(Name, Handler, State)          % if trapping exits
    ;   service_loop(Name, Handler, State)
    ).

service_call(Name, Request, Reply) :-
    service_call(Name, Request, Reply, 600000).

service_call(Name, Request, Reply, Timeout) :-
    self(Me),
    make_ref(Ref),
    send(Name, {request, Me, Ref, Request}),
    (   receive_match({reply, Ref, Reply}, _, Timeout)
    ->  true
    ;   Reply = service_timeout(Name, Request)
    ).

service_cast(Name, Message) :- send(Name, {cast, Message}).
```

A stale reply (one that arrives after a `service_call` timed out) is harmless:
its ref never matches again. That is the point of per-call refs.

---

## 3. The desk: `desk.dml`

```prolog
:- use_module(library(lists)).
:- use_module(library(service)).
:- use_module(library(supervisor)).

doc(agent_main/2, "Write an article from a brief using a desk of specialist processes.").
doc(editor_section/2, "Research, check and draft one section.").

%% ======================= editor (root) ===============================

agent_main(Brief, Article) :-
    start_desk(),

    %% 1. Plan: the planner owns sections and the style guide.
    service_call(planner, {plan, Brief}, plan(Sections, Style)),
    service_call(scribe, {set_style, Style}, _),
    service_cast(ledger, {record, planned(Sections)}),

    %% 2. Produce each section.
    maplist(editor_section, Sections, Drafts),

    %% 3. Assemble with the single writer, report usage.
    service_call(scribe, {assemble, Drafts}, Article),
    service_call(ledger, usage, Usage),
    service_cast(ledger, {record, assembled(Usage)}),
    format("[editor] done; usage ~w~n", [Usage]),
    answer(Article).

editor_section(Section, Draft) :-
    get_dict(title, Section, Title),
    get_dict(questions, Section, Questions),
    research_all(Questions, Notes),
    service_call(checker, {check, Notes}, Checked),
    service_call(scribe, {draft, Title, Checked}, Draft).

%% Fan a section's questions out to the pool and collect replies BY REF.
research_all(Questions, Notes) :-
    self(Me),
    maplist(ask_research(Me), Questions, Refs),
    maplist(collect_note, Refs, Notes).

ask_research(Me, Question, Ref) :-
    make_ref(Ref),
    send(research_pool, {request, Me, Ref, Question}).

collect_note(Ref, Note) :-
    (   receive_match({reply, Ref, Note}, _, 180000)
    ->  true
    ;   Note = failed(timeout)
    ).

%% ======================= desk startup ================================

start_desk() :-
    supervisor_start([
        child(planner, service_init(planner, planner(none), planner_handler), permanent, 5000),
        child(checker, service_init(checker, ok, checker_handler), permanent, 5000),
        child(scribe,  service_init(scribe, scribe(none), scribe_handler), permanent, 5000),
        child(ledger,  service_init(ledger,
                          ledger([], usage{input: 0, output: 0, total: 0}),
                          ledger_handler), permanent, 5000)
    ], supervision{strategy: one_for_one, max_restarts: 10, max_seconds: 60}, _Sup),
    pool_start(3).

%% ======================= planner =====================================

planner_handler({plan, Brief}, _State, reply(plan(Sections, Style)), planner(Style)) :-
    format(string(PlanPrompt),
        "Plan an article answering '~w'. Return 3 to 5 sections; each section "
        "needs a title and 1 to 3 specific research questions.", [Brief]),
    task(PlanPrompt, list(object(Sections))),
    format(string(StylePrompt),
        "Write a one-sentence style guide for an article answering '~w'.", [Brief]),
    task(StylePrompt, string(Style)).

%% ======================= checker (quality gate) ======================

checker_handler({check, Notes}, State, reply(Checked), State) :-
    maplist(check_note, Notes, Checked).

check_note(Note, checked(Note, ok)) :-
    verify(Note, "Is this a specific, checkable claim with a named source?", yes),
    !.
check_note(Note, checked(Note, rejected)).

%% ======================= scribe (single writer) ======================

scribe_handler({set_style, Style}, _State, reply(ok), scribe(Style)) :-
    !,
    format(string(Sys), "Write in this style: ~w", [Style]),
    system(Sys).                       % private to the scribe's memory

scribe_handler({draft, Title, Checked}, State, reply(Prose), State) :-
    format(string(Prompt),
        "Draft the section '~w' from these checked notes. Use only notes "
        "marked ok; 1 to 2 paragraphs. Notes: ~w", [Title, Checked]),
    task(Prompt, string(Prose)).

scribe_handler({assemble, Drafts}, State, reply(Article), State) :-
    format(string(Prompt),
        "Assemble these section drafts into one coherent article. Remove "
        "repetition and add transitions. Drafts: ~w", [Drafts]),
    task(Prompt, string(Article)).

%% ======================= ledger (recorder) ===========================

ledger_handler({record, E}, ledger(Es, U), reply(ok), ledger([E|Es], U)) :- !.
ledger_handler({add_usage, U2}, ledger(Es, U), reply(ok),
               ledger(Es, add_usage(U, U2))) :- !.
ledger_handler(events, State, reply(Es), State) :- State = ledger(Es, _), !.
ledger_handler(usage,  State, reply(U),  State) :- State = ledger(_, U).

add_usage(usage{input: I, output: O, total: T},
          usage{input: I2, output: O2, total: T2},
          usage{input: I + I2, output: O + O2, total: T + T2}).

%% ======================= research pool (dispatcher + workers) ========
%%  Custom loop, not library(service): it dispatches, queues, and must
%%  react to worker crashes by replying to the in-flight caller.

pool_start(Size) :-
    spawn_link(pool_init(Size), Pool),
    register(research_pool, Pool).

pool_init(Size) :-
    process_flag(trap_exit, true),
    self(Pool),
    make_workers(Size, Pool, Workers),
    pool_loop(pool(Workers, [])).

make_workers(0, _, []) :- !.
make_workers(N, Pool, [idle(Pid)|Rest]) :-
    spawn_link(research_worker(Pool), Pid),
    N1 is N - 1,
    make_workers(N1, Pool, Rest).

pool_loop(State) :-
    receive(Msg),
    (   pool_handle(Msg, State, Next) -> pool_loop(Next) ; pool_loop(State) ).

%% New work: hand it to an idle worker, or queue it.
pool_handle({request, From, Ref, Question}, pool(Workers, Queue), Next) :-
    (   selectchk(idle(Worker), Workers, Rest)
    ->  send(Worker, {research, Question}),
        Next = pool([busy(Worker, From, Ref, Question)|Rest], Queue)
    ;   append(Queue, [{From, Ref, Question}], Queue1),
        Next = pool(Workers, Queue1)
    ).

%% A worker finished: forward its result, free the worker, start the
%% oldest queued request.
pool_handle({done, Worker, Notes}, pool(Workers, Queue), Next) :-
    selectchk(busy(Worker, From, Ref, _), Workers, Rest),
    !,
    send(From, {reply, Ref, Notes}),
    next_request([idle(Worker)|Rest], Queue, Next).

%% A worker died: answer whoever was waiting, replace the worker.
pool_handle({'EXIT', Worker, Reason}, pool(Workers, Queue), Next) :-
    !,
    (   selectchk(busy(Worker, From, Ref, _), Workers, Rest)
    ->  send(From, {reply, Ref, failed(Reason)})
    ;   selectchk(idle(Worker), Workers, Rest) -> true
    ;   Rest = Workers
    ),
    self(Pool),
    spawn_link(research_worker(Pool), NewWorker),
    next_request([idle(NewWorker)|Rest], Queue, Next).

next_request(Workers, [], pool(Workers, [])) :- !.
next_request(Workers, [{From, Ref, Q}|Rest], Next) :-
    (   selectchk(idle(Worker), Workers, WorkersRest)
    ->  send(Worker, {research, Q}),
        next_request([busy(Worker, From, Ref, Q)|WorkersRest], Rest, Next)
    ;   Next = pool(Workers, [{From, Ref, Q}|Rest])
    ).

%% A researcher loops; it keeps no state between questions. A crash is
%% observed by the pool through the link.
research_worker(Pool) :-
    self(Self),
    receive({research, Question}),
    research_question(Question, Notes),
    self_usage(U),
    service_cast(ledger, {add_usage, U}),
    send(Pool, {done, Self, Notes}),
    research_worker(Pool).

research_question(Question, Notes) :-
    task("Research this question and return 2 to 4 checkable notes, each with "
         "a named source: {Question}", list(string(Notes))).
```

---

## 4. Walkthrough

Assume the planner returns two sections. `editor`, `pool`, `w1..w3` are the
processes that matter; the services sit waiting on `receive`.

```
t0    editor      start_desk
t1    sup         spawn planner, checker, scribe, ledger (permanent, linked)
t2    editor      pool_start(3) -> spawn_link pool; pool spawn_link w1,w2,w3
t3    editor      service_call planner {plan, Brief}
t4    planner     task(plan)          yield -> model call
t5    planner     task(style)         yield -> model call
t6    planner -> editor  plan([S1,S2], Style)
t7    editor      service_call scribe {set_style, Style}
t8    scribe      system(Style)  (memory now holds the style)
t9    editor      service_cast ledger {record, planned([S1,S2])}

      section S1 = section{title: "…", questions: [QA, QB]}

t10   editor      ask_research QA -> ref RA; ask_research QB -> ref RB
t11   pool        QA -> w1 busy; QB -> w2 busy; w3 idle
t12   w1          task(research QA)   yield -> model call
t13   w2          task(research QB)   yield -> model call      (concurrent)
t14   w2          done; self_usage(U2) -> ledger cast; pool {done, w2, NotesB}
t15   pool -> editor  {reply, RB, NotesB}; w2 idle
t16   w1          done; pool {done, w1, NotesA}
t17   pool -> editor  {reply, RA, NotesA}
t18   editor      service_call checker {check, [NotesA, NotesB]}
t19   checker     verify(NoteA, …); verify(NoteB, …)           (judge batch)
t20   checker -> editor  [checked(NoteA, ok), checked(NoteB, rejected)]
t21   editor      service_call scribe {draft, TitleS1, Checked}
t22   scribe      task(draft)         yield -> model call
t23   scribe -> editor  ProseS1
t24   editor      … repeats for S2 …
t25   editor      service_call scribe {assemble, [ProseS1, ProseS2]}
t26   scribe      task(assemble)      yield -> model call
t27   scribe -> editor  Article
t28   editor      service_call ledger usage -> Usage
t29   editor      answer(Article)
```

What this shows that `swarm` does not:

- **Two concurrent model calls from one section**, routed through a pool that
  also serves *other* callers and holds a queue. A `maplist` would have blocked
  on the first section before starting the second.
- **A single writer (`scribe`)**. Two `draft` calls from different sections
  cannot interleave inside the model, because the scribe's mailbox serializes
  them. Its memory carries the style, so every section sounds the same.
- **Stateful services.** The ledger accumulates across the whole run; the scribe
  accumulates memory; the planner owns the style.
- **Directional protocols.** `{plan, …}`, `{check, …}`, `{draft, …}`,
  `{request, …}` are different shapes for different roles, not one uniform item.

---

## 5. Failure and restart

### A researcher crashes

```
w2 dies inside task/N
  pool <- {'EXIT', w2, Reason}
  pool sees w2 was busy with {editor, RB, QB}
  pool -> editor : {reply, RB, failed(Reason)}          % nobody hangs
  pool spawn_link w2' -> idle; dispatches the next queued request, if any
editor: collect_note(RB) binds failed(Reason)
checker: checked(failed(Reason), rejected)
scribe:  drafts the section with that note flagged
```

The important part is the middle line: a pool that owns in-flight requests
**must** reply on behalf of a crashed worker, or its callers hang forever. The
`busy(Pid, From, Ref, Question)` record exists precisely so the pool can do that.

### A service crashes

```
scribe dies
  sup <- {'EXIT', scribe, Reason}; permanent -> restart
  service_init(scribe, scribe(none), scribe_handler) re-registers `scribe`
  BUT the new scribe has no style memory.
  editor's in-flight {draft, …} -> service_timeout(scribe, _)
  editor re-sends {set_style, Style} and retries the draft
```

Two lessons: (1) a supervisor restarts the *process*, not its state, so stateful
services need re-hydration; (2) a caller must treat `service_timeout` as
recoverable, because a restart means the reply may never come.

### The whole desk

The supervisor is `permanent`, so it restarts planner/checker/scribe/ledger on
any exit. The pool traps exits and supervises its own workers. The editor is the
root: if it dies, its link to the pool tears the workers down, and the
supervisor's link to its parent stops the services.

---

## 6. Shutdown and cancellation

- **Normal:** `answer(Article)` ends the editor's run; its link to the pool ends
  the pool and its workers; `supervisor_stop(SupPid, done)` (or the parent-exit
  path) stops the services.
- **Cancellation** (pi `/dc-cancel`, session close): the root frame is aborted,
  the editor unwinds, and the same links cascade. At L2 this is
  `worker.terminate()`.
- **In-flight requests:** a cancelled call leaves `service_timeout` with the
  caller; the service may still finish its `task/N`. If that matters, a service
  should check `process_info(self, state)` or accept a `{cancel, Ref}` message.

Unlike `swarm`, where cancellation means "stop the batch", here it means
"unwind a graph". The link discipline is what makes that deterministic.

---

## 7. Gotchas

- **Unbounded pool queue.** The pool's `Queue` grows without limit under load.
  Add a cap and reply `failed(overloaded)` when full.
- **State loss on restart.** The scribe's style and the ledger's events vanish
  when the process restarts. Keep authoritative state in the caller, persist it,
  or re-hydrate; the supervisor will not do it for you.
- **A service is a serialization point.** The scribe being one process is a
  feature (consistent voice) *and* a throughput limit. Two slow drafts cannot
  overlap.
- **Stale replies.** Ref-keyed selective receive makes late replies harmless, but
  they still occupy the mailbox until matched or drained. Drain with
  `receive_match` timeouts or a `flush_mailbox/0`.
- **Name reuse on restart.** `register/2` fails if the name is still held; SWI
  releases a name when the process dies, so restart is fine, but a client call in
  the gap times out.
- **Do not service-ify pure functions.** If a helper has no state, no rate limit
  to enforce, and no need for a stable name, call it directly. The planner and
  checker sit near that line; the scribe, ledger, and pool are clearly on the
  right side of it.

---

## 8. Summary

`swarm` answers "run this predicate over that list". The desk answers "these
roles, in this topology". The primitives are the same — `spawn`, `send`,
`receive`, links, a registry — but the shape is a system, not a map. The
specialization is what buys:

- a **single writer** with consistent state (scribe),
- a **stateful recorder** (ledger),
- a **dispatcher** that owns concurrency and a queue (pool),
- **crash recovery that answers in-flight callers** and keeps going.

Use `swarm` for fan-out; build a desk only when the roles genuinely differ and
must stay alive.
