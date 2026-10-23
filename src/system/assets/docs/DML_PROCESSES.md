# Processes and concurrency

A process is a session-scoped DML execution with private harness/memory snapshot
and a mailbox. Parallelism is real; do not infer from Prolog's normal sequential
conjunction that DML lacks `swarm`/processes. Read before first process use.

## Choosing a process pattern

Use one `prompt/N` or scoped `task/N` for a short focused model step.
Use `maplist/3` for sequential mapping; `swarm/3,4` for independent bounded work.
Use `spawn/2` when work outlives the current turn; use a service for long-lived
message/state logic or single-writer coordination. Avoid a process per tiny
operation; every worker costs a session and often a model call.

## Parallel mapping

`swarm(Worker, Items, Results[, Concurrency])` calls Worker(Item, Value) in an
isolated process per item; default concurrency 4. Results preserve input order,
each `ok(Value)` or `failed`. Inspect failures; do not assume every item succeeded.
`pmap(Worker, Items, Values)` returns bare values and fails if any worker fails.
Bound input size and concurrency; live process limits and provider limits apply.

```prolog
double(N, R) :- R is N * 2.
parallel_values(Items, Results) :- swarm(double, Items, Results, 2).
```

Keep worker outputs structured/plain values and their effects deterministic.
Do not make independent workers overwrite the same file; last sync may win.

## Spawn and result messages

`spawn(Goal, Pid)` returns immediately; `spawn_opt(Goal, Pid, Options)` adds name,
priority, timeout and code options. Child variables are COPIES: no child output
variable binds a variable in the parent. Use messages or a terminal answer.

```prolog
worker(Parent, Input) :-
    prompt("Summarise: {Input}", string(Summary)),
    send(Parent, done(Summary)).
start_summary(Input, Pid) :- self(Me), spawn(worker(Me, Input), Pid).
```

A separate foreground goal can receive the result. `wait(Pid, Outcome)` reports
`ok(Text)`, `failed(Error)` or `killed(Reason)`; use a message for structured
results. A worker's terminal result is text, not a shared output argument.
Commit harness edits before spawning; snapshots already running do not update.
Module/memory isolation does NOT imply a copied filesystem. In the current
browser, session/child/Python workers mount one live OPFS-backed /workspace.
Completed writes are shared; use messages to order producer/consumer operations
and coordinate concurrent writers. Other embeddings must state their own file
visibility guarantees. Create required artifacts before their first use.

## Mailboxes

`send(Target, Term)` copies a message. Target can be a pid or registered name.
`receive(Message[, TimeoutMs])` consumes mailbox messages; use bounded waits.
`receive_match(Template, Message, TimeoutMs)` selects a matching message and
keeps unrelated messages. For custom request/reply protocols use correlation
refs (`make_ref/1`) so replies cannot be confused.
`self(Pid)`, `register(Name, Pid)` and `whereis(Name, Pid)` identify processes.
A simple foreground `input/1` does not consume a worker's message; `next/1`
dispatches `user(Text)` versus `message(Term)` for custom event-driven conversations.

## Services

`serve(Name, Loop[, Pid])` starts and registers a process.
`service_call(Target, Request, Reply[, TimeoutMs])` sends
`request(From, Ref, Request)` and waits for `reply(Ref, Reply)`.
`service_cast(Target, Message)` sends without waiting. The service loop receives
requests, sends the correlated reply, then recurses with its new state.

```prolog
counter(N) :-
    receive(M),
    ( M = request(From, Ref, read)
    -> send(From, reply(Ref, N)), counter(N)
    ; M = request(From, Ref, bump)
    -> Next is N + 1, send(From, reply(Ref, Next)), counter(Next)
    ; M = stop -> true
    ; counter(N)
    ).
```

Use a registered service name for cross-turn reuse, `ps`/`process_info` to inspect
liveness, and `ps(code: name)` to inspect its snapshot. Model-token usage can be
zero for a deterministic service; that does not mean it is stopped.

## Lifecycle and cleanup

`wait/1` succeeds only for successful termination. `exit(Reason)` terminates the
current process; `exit(Target, Reason)` stops a peer. The authoring host tool
`stop` is NOT a DML `stop/1` predicate. `sleep(Ms)` yields; `now_ms/1` gives time.
Stop/reap test workers on success, error or timeout. An execute call can leave
session-scoped processes running; a scratch module is not a scratch world.

Browser worker-hosted processes cannot render forms. Their host can proxy explicit
`input/2` where supported, but structured form workflows belong in foreground.
`suspend(Continuation)` is an advanced safe-point operation; close streams/forms
before suspending and verify host resume support. Do not suspend inside a host call.
