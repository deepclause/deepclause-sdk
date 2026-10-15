# Subagents and concurrency

DeepClause harnesses can run **subagents**: session-scoped DML processes that
live alongside the main module, talk to each other by messages, and outlive the
turn that started them. This document explains the model, the surface, and the
design — and where the ideas come from.

> Status: implemented. `spawn`, `send`/`receive`, `serve`/`service_call`,
> `swarm`/`pmap`, `wait`, the harness tools, the `/ps --watch --logs` view, and
> the L1/L2 execution modes are all in the runtime (`src/repl/processes.ts`,
> `src/repl/process-executor.ts`, `src/repl/worker-host.ts`, the
> `request_proc` primitives in `deepclause_mi.pl`). Links, monitors, and a
> supervisor library are designed but not built — see
> [`DML_REPL_PROCESSES_SPEC.md`](DML_REPL_PROCESSES_SPEC.md).

---

## 1. What a subagent is

A subagent is an ordinary DML execution with four things of its own:

- **a private module snapshot** — the harness as it was when the process started;
- **private memory** — its own `system/1`, `user/1`, and task history;
- **a mailbox** — a FIFO of copied terms;
- **a lifecycle** — it runs, may wait, and finishes (or crashes).

It runs *in the same session*, not as a separate harness: the same workspace,
the same tools, the same model, the same credentials. The main agent keeps
editing the harness; a subagent just does not see those edits.

```prolog
%% a one-shot subagent
summarise(Topic) :-
    task("Summarise {Topic} in one sentence.", string(Summary)),
    answer(Summary).

%% start it; it keeps running after this goal returns
?- spawn(summarise("quantum error correction"), Pid).
Pid = proc(main, sess_abc, p1).
```

The goal decides the shape:

| Shape | The goal… |
|---|---|
| **one-shot** | calls `output/1` / `answer/1` and returns |
| **message-waiting** | loops on `receive/1,2`, handling terms from other processes or the user |
| **interactive** | calls `input/2` and blocks until the user answers in the editor |

---

## 2. Inspiration: the BEAM / actor model

The model is deliberately Erlang-shaped, because Erlang's concurrency model fits
agentic work almost exactly: many independent, I/O-bound, individually failable
subtasks that spend most of their life waiting on the network.

| Erlang / OTP | DeepClause |
|---|---|
| process (own heap, mailbox) | process (private snapshot, private memory, mailbox) |
| `spawn/1`, `spawn_link/1` | `spawn/2`, `spawn_opt/3` |
| `Pid ! Msg`, `receive` | `send/2`, `receive/1,2`, `receive_match/3` |
| links, monitors | designed (`link/1`, `monitor/2`) — see the spec |
| OTP `supervisor` | designed (a DML `supervise/…` library) — see the spec |
| hot code loading | the living module; `spawn` snapshots, a code server owns edits |
| "let it crash" | a worker that fails is `failed(Reason)`; a supervisor restarts |
| value semantics (copy on send) | `spawn` and `send` copy terms |

The actor model (Hewitt) supplies the one rule that makes it safe: **processes
do not share memory; they communicate by messages.** DeepClause makes that
concrete — `spawn` and `send` serialise terms, so even two processes in the same
WASM instance cannot share a variable. That is not an implementation detail; it
is the property that lets the same program run in one thread (L1) or in worker
threads (L2) without changing a line of DML.

The DML-specific twist is the **living module**. In Erlang, code is immutable and
shared, and state lives in processes. In DML, the module *is* code and state at
once. The resolution is the same as OTP's code server: the primary session owns
the module and serialises mutations; a subagent runs a **snapshot** and can only
affect the world through messages. A subagent that `assert`s changes only
itself.

---

## 3. The surface

### Processes

```prolog
spawn(+Goal, -Pid)                    % start a process; returns immediately
spawn_opt(+Goal, -Pid, +Options)      % name, priority, timeout, code
self(-Pid)                            % this process's pid
send(+Target, +Message)               % copy a term into a mailbox (pid | name | label)
receive(?Message[, +TimeoutMs])       % pop the oldest message; fails on timeout
receive_match(+Template, ?Msg, +Ms)   % selective receive; requeues non-matches
exit(+Reason) / exit(+Pid, +Reason)   % terminate
process_info(+Pid, -Info)             % a dict of state/priority/mailbox/usage
register(+Name, +Pid) / whereis(+Name, -Pid)
sleep(+Ms) / now_ms(-T) / make_ref(-Ref)
wait(+Pid[, -Outcome])                % block; ok(Result) | failed(Error) | killed(Reason)
```

A pid is `proc(Node, Session, Id)` and is location-transparent. `send/2`
also accepts a registered name or a process's default label (its goal's functor),
so `send(keeper, stop)` reaches an unregistered `keeper/…` process.

### Services

```prolog
serve(+Name, +Goal)                          % spawn + register
service_call(+Target, +Request, -Reply[, +Timeout])   % request/reply
service_cast(+Target, +Message)              % fire-and-forget
```

A service is a `receive` loop that answers `request(From, Ref, Request)` with
`send(From, reply(Ref, Reply))`. `service_call` ignores unrelated messages while
it waits.

### Fan-out

```prolog
swarm(+Worker, +Items, -Results[, +Concurrency])   % ok(Value)/failed per item, ordered
pmap(+Worker, +Items, -Values)                     % bare values; fails on any failure
```

`Worker` is `call(Worker, Item, Value)`. Each item runs in its own process, at
most `Concurrency` (default 4) at once, and the results come back **in input
order**.

---

## 4. Two execution modes

The same DML runs at either isolation level; only `--execution` changes. `worker`
is the default; `inline` is the in-process fallback.

| | **L1 — `inline`** | **L2 — `worker` (default)** |
|---|---|---|
| Where | one WASM instance, cooperative scheduler | a `worker_threads` thread with its own WASM |
| Concurrency | interleaving at `task`/`exec`/`receive` yield points | true parallelism |
| Cancellation | between steps (best-effort) | `worker.terminate()` |
| CPU-bound Prolog | blocks the session | runs off the main thread |
| Model/tools/input | direct | proxied to the main thread |

The seam is two interfaces (`ProcessHost` and `CapabilityBroker` in
`src/repl/processes.ts` and `worker-host.ts`). L2 never holds credentials or
policy: a worker asks the main thread for a model call, a tool run, or user
input. That is what makes the switch a mode, not a rewrite, and it is enforced
by the value semantics above.

A per-session **provider semaphore** (default 4, `providerConcurrency`) bounds
model calls across the main run and every subagent, so a fan-out cannot trip
rate limits.

---

## 5. In the REPL

**Agent tools** (`src/repl/harness-tools.ts`): `spawn`, `ps`, `send`, `call`,
`stop`, `swarm`, `wait`.

**Slash commands**:

| Command | What |
|---|---|
| `/spawn <goal>` (`/serve`) | start a subagent |
| `/ps` | print the process table |
| `/ps --watch` | pin the summary panel above the editor |
| `/ps --watch --logs` | pin a larger follow of every subagent's streamed output, reasoning, and tool calls |
| `/send <name> <term>` (`/cast`) | message a process |
| `/call <name> <term>` | request/reply with a service |
| `/stop <name>` | stop a process |

Lifecycle, output, and answers render into the transcript as `⚙ name …` lines;
the logs panel adds model text, reasoning, tool calls, and exit states. The
status bar shows `⚙ N svc`.

---

## 6. Use cases

- **Fan-out over a list** — summarise N files, classify a batch, research
  subtopics: `swarm(Worker, Items, Results)`.
- **Best-of-N / ensembles** — run the same prompt N times, judge the results.
- **Background a long job** — `spawn(Goal)`, report the id, keep talking;
  `/ps --watch --logs` follows it.
- **A long-lived service** — a watcher, poller, or accumulator that keeps state
  across turns and answers `service_call`s.
- **Interactive delegation** — a subagent asks the user (`input/2`) while the
  main agent continues.
- **Multi-role collaboration** — a planner, a checker, a single writer, a
  ledger, wired by a coordinator.
- **Recursive decomposition (RLM)** — a subagent that spawns subagents.
- **Isolation** — run generated or experimental DML in a subagent whose
  `assert` cannot corrupt the harness.
- **Hedging and verification** — start two approaches and cancel the loser;
  one verifier per artifact.

Anti-cases: a single model call (`task/N`), work that needs the harness's shared
state, or work whose answer is needed right now — keep those in the foreground.

---

## 7. Examples

- [`DML_REPL_SUBAGENTS.md`](DML_REPL_SUBAGENTS.md) — the three shapes
  (one-shot, message-waiting, interactive) and a runnable `.dml`/`.ts`.
- `sdk-examples/debate.{dml,ts}` — two **LLM-backed** subagents that debate each
  other and are judged.
- [`DML_SPECIALIZED_PROCESSES_EXAMPLE.md`](DML_SPECIALIZED_PROCESSES_EXAMPLE.md)
  — a "desk" of specialised roles with a dispatcher pool.
- [`DML_BEAM_SWARM_EXAMPLE.md`](DML_BEAM_SWARM_EXAMPLE.md) — the `swarm`
  combinator and the supervisor sketch.

## 8. Design history

The design was worked out across a series of documents:

- [`DML_PARALLEL_BACKGROUND.md`](DML_PARALLEL_BACKGROUND.md) — the original
  brainstorm: background execution, `parallel_maplist`, parallel tool calls.
- [`DML_REPL_WORKER_EXECUTION.md`](DML_REPL_WORKER_EXECUTION.md) — the
  worker-thread plan (out-of-process execution, pool, timeouts).
- [`DML_REPL_PROCESSES_SPEC.md`](DML_REPL_PROCESSES_SPEC.md) — the normative
  spec: lifetimes, primitives, the L1↔L2 seam, scheduling, persistence.
- [`DML_REPL_PROCESSES_IMPLEMENTATION_PLAN.md`](DML_REPL_PROCESSES_IMPLEMENTATION_PLAN.md)
  — the phased plan and status.
- [`DML_REPL_SERVICES_UX.md`](DML_REPL_SERVICES_UX.md) — the REPL UX.
