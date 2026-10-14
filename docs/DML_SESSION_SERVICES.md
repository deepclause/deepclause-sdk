# Session-scoped services and swarms

Status: **design / feasibility**. Runtime-level, with one adapter per host.

> **Correction.** An earlier revision of this document conflated the DeepClause
> **REPL** with **pi**. They are different hosts with different owners, UIs,
> authoring agents, sessions, and storage. This revision is host-neutral at the
> runtime layer and gives a separate adapter for each. Where a detail is
> host-specific it is labelled `[REPL]` or `[pi]`.

**Question.** Could the authoring agent (or a predicate in the harness) start
services or swarms so that the whole system state is visible in the session, and
so that something keeps running while the user continues with the session?

**Short answer.** Yes. The runtime feature is the same in both hosts — a
**process owner** with a **session-scoped lifetime** — but *who owns it*, *where
the state is shown*, and *what "the session" means* differ per host. This document
first defines the host-neutral feature, then wires it into the REPL and into pi.

---

## 0. Two hosts — do not conflate

| | **DeepClause REPL** | **pi + `deepclause-pi`** |
|---|---|---|
| Host process | the `deepclause` CLI / an embed of `createHarnessRepl()` | pi, loading the extension |
| Runtime | `DMLSession` + `DMLRunner` + `swipl-wasm` | `createDeepClause` + `DMLRunner` + `swipl-wasm` |
| **Authoring agent** | `HarnessAgent` (TypeScript, `src/repl/agent.ts`) | **pi's own model** |
| Authoring tools | `begin_turn`, `define`, `assert`, `execute`, `end_turn`, `listing`, `reference`, … | `dc_run`, `dc_plan_commit`, `dc_diagram`, … |
| User input | plain English, `/goal <dml>`, or a slash command | chat with pi, or `/dc-*` |
| UI surface | REPL TUI: transcript + fixed status bar + editor (`src/repl/tui/`) | pi TUI via `ctx.ui` (`setWidget`/`setStatus`/dialogs) |
| **"Session"** | REPL session under `.deepclause/repl/sessions/<id>/`, resumed with `--session` | pi session; extension state under `.pi/deepclause/` |
| Storage rule | `.deepclause/` is **correct** here | **must not** create `.deepclause/` (root `AGENTS.md`) |
| Cancellation | `Esc` interrupts a turn, Ctrl-C exits | `ctx.signal`, `/dc-cancel` |
| Long-lived resource hooks | the REPL process / `DMLSession` lifetime | `session_start` / `session_shutdown` |

The runtime work — the process model, the job registry, `library(swarm)` and
`library(service)` — is **shared**. The ownership, UI, and persistence are
**per host**. Everything below is organised that way.

---

## 1. The host-neutral feature

A service is not a longer run. Add a third lifetime tier:

| Tier | Owner | Ends when | Example |
|---|---|---|---|
| **Run-scoped** | one DML run | the entry predicate returns | a `swarm` inside `agent_main` |
| **Turn-scoped** | a foreground request | the request ends / is cancelled | a `/goal` or `/dc-run` |
| **Session-scoped** | a **process owner** | the session closes, or an explicit stop | a desk, a poller, a queue consumer |

The owner is the same abstraction in both hosts:

```ts
interface ProcessOwner {
  // start a DML process that outlives the current run/turn
  serve(name: string, skill: string, args: unknown[], seed?: Seed): Promise<Pid>;
  spawn(goal: string, opts?: SpawnOpts): Promise<Pid>;
  ps(): ProcessInfo[];                       // the whole system state
  call(name: string, msg: unknown, ms?: number): Promise<unknown>;
  cast(name: string, msg: unknown): void;
  stop(name: string): Promise<void>;

  signal: AbortSignal;                       // SESSION-scoped, not a turn's
  step(): void | Promise<void>;              // scheduler tick
}
```

The DML surface is host-neutral:

```prolog
serve(Name, Handler, InitState).        % session-scoped process
serve_supervisor(Name, Specs, Options). % a supervision subtree
stop_service(Name).
ps(Pids).
```

Three rules make it correct in both hosts:

1. **The owner has a session-scoped abort signal.** A turn ending must not cancel
   a service. `[REPL]` the per-turn abort (`Esc`) is separate from the
   REPL-session signal. `[pi]` `ctx.signal` is turn-owned and must never reach a
   service; the host has its own.
2. **The owner resolves the active model lazily.** `[REPL]` the injected
   `LLMBackend` is process-scoped already, so this is easy. `[pi]` `ctx` is only
   valid inside a handler and a reload replaces the runtime, so the host holds a
   refreshed context provider and reads it at call time.
3. **Teardown is idempotent.** `[REPL]` `/halt`, Ctrl-C, process exit, and an
   embedder calling `dispose()` all converge. `[pi]` `session_shutdown`, reload,
   session replacement, and cancellation all converge.

The process model itself (spawn/send/receive/links/supervisors) and the two
libraries are specified in the companion docs and are unchanged here:
[`DML_BEAM_SWARM_EXAMPLE.md`](./DML_BEAM_SWARM_EXAMPLE.md) and
[`DML_SPECIALIZED_PROCESSES_EXAMPLE.md`](./DML_SPECIALIZED_PROCESSES_EXAMPLE.md).

---

## 2. `[REPL]` adapter

### 2.1 Owner and lifetime

The owner is the **REPL process**, concretely `HarnessRepl` / `DMLSession`
(`src/repl/index.ts`, `src/repl/session.ts`). A `ProcessHost` is added to
`DMLSession` and lives as long as the session object. There is no
`session_start`/`session_shutdown`; the lifecycle is:

- created when the REPL session is created (or on first `/serve`),
- survives turns, plain-English requests, and `/goal` queries,
- ended by `/halt`, Ctrl-C exit, or the embedder disposing the REPL,
- on `--session` resume: **declarations and history are replayed, live processes
  are not** (a process is not serializable); they are marked `orphaned`.

### 2.2 Scheduler

The REPL already has the substrate: a cooperative loop that steps a frame and
services `engine_yield` requests from TypeScript (`DMLRunner.stepLoop`), with
`DMLSession.executeGoal` running nested frames. Extend it to a **run queue**:

- the foreground turn's frame and each service frame are entries;
- when the foreground awaits a model/tool/input call, the loop steps a ready
  service;
- when a message arrives or I/O completes, the loop wakes the target;
- a status-bar repaint is requested at most once per tick.

This is the same scheduler as the host-neutral core; the REPL is where it is
cheapest to build, because the loop already exists and `swipl-wasm` is
single-threaded and cooperative.

### 2.3 Visibility

The REPL TUI has a transcript and a fixed status bar, not pi's generic
`setWidget`. So the surface is:

- a **status-bar segment** via `ReplStatusTracker` / `formatStatusLine`
  (`src/repl/status.ts`): e.g. `⧗ desk 2/3 · 41k tok`;
- **transcript lines** for lifecycle transitions (started, crashed, restarted,
  stopped), rendered by the existing renderer;
- a **`/ps` command** that prints the process tree into the transcript, reflowed
  and highlighted like `/listing`.

```
⧗ desk (one_for_one, 8 restarts)
  ├─ planner   ready     idle
  ├─ checker   ready     idle
  ├─ scribe    busy      drafting §2 (12s, 8.1k tok)
  ├─ ledger    ready     events=37 usage=41k
  └─ pool      running   w1 busy  w2 idle  w3 restarted×1  queue=0
```

### 2.4 Durable record

The REPL session is the record. Write service **declarations** and **lifecycle
events** under the session directory (`sessions/<id>/`), alongside the existing
`harness_*` history — e.g. a `services.jsonl` (declarations + start/stop/restart
events) that `--session` replays. Do **not** put process state into the harness
module; it is not harness logic.

### 2.5 Authoring and commands

- `HarnessAgent` gains a `serve` tool through `HarnessToolController`
  (`src/repl/harness-tools.ts`), beside `execute` and `define`. It must be
  non-blocking: the agent starts a service, reports it, and continues the turn.
- Slash commands: `/serve <skill> [args]`, `/ps [name]`, `/call <name> <req>`,
  `/cast <name> <msg>`, `/stop <name>`.

### 2.6 "The user continues"

This already works in spirit: the REPL accepts input while a turn runs (steering
interrupts the in-flight model call and injects the message). With session-scoped
services, the user's next plain-English request or `/goal` is the foreground,
while service frames advance in the cooperation loop. `Esc` cancels the
*foreground*; `/stop` ends a service; Ctrl-C exits everything.

### 2.7 Model

The REPL's injected `LLMBackend` (pi-backed or env-backed) is process-scoped, so
services reuse it directly. Attribute usage per process and roll it into the
status bar's existing token counters.

---

## 3. `[pi]` adapter

### 3.1 Owner and lifetime

The owner is a **`DcSessionHost`** created by the `deepclause-pi` extension, one
per pi session, started on `session_start` or on the first `/dc-serve`, and
stopped idempotently on `session_shutdown`. Pi explicitly sanctions this shape:

> *"Start long-lived resources from `session_start` … Close session-scoped
> resources from an idempotent `session_shutdown` handler."* — `docs/extensions.md`

```
pi session
├── DcSessionHost                      (session_start / first /dc-serve)
│   ├── session abort signal           ← distinct from ctx.signal
│   ├── process registry + supervision
│   ├── background scheduler           (steps service frames between turns)
│   ├── live widget + status           (ctx.ui.setWidget / setStatus)
│   └── session record                 (pi.appendEntry / sendMessage)
│
├── foreground run  ── turn-scoped ──  claims the one foreground slot
└── services/swarms ── session-scoped ─ survive turns
```

`claimExecution`/`activeController` (`deepclause-pi/src/index.ts`) stays the
foreground guard; services are a **different tier**, not more claimants of that
slot.

### 3.2 Scheduler

A driver loop independent of any turn steps ready service frames and wakes on
message arrival, host-call completion, and timers. Pi's model calls are async, so
services progress while the user or pi is thinking. **Services default to L2
workers** so "background" is not a lie: a CPU-bound L1 service would freeze pi's
UI.

### 3.3 Visibility

- Live: `ctx.ui.setWidget("dc-ps", …)` — a component factory in TUI mode for an
  interactive tree, plain lines in RPC mode, nothing in JSON/print; plus
  `ctx.ui.setStatus("dc", "⧗ desk 2/3")`.
- Durable: two pi facilities with a strict split —

| API | Stored | Sent to model | Use for |
|---|---|---|---|
| `pi.appendEntry()` | yes | **no** | process snapshots, event logs, restart history |
| `pi.sendMessage()` | yes | yes (custom) | a compact status notice the agent should see |
| `pi.sendUserMessage()` | yes | yes (user turn) | a service that genuinely needs attention |

Detailed state is **excluded from model context**; only a one-line summary ever
reaches the model. The agent pulls more with `dc_ps` when it wants it.

### 3.4 Durable record and resume

On `session_start`, the host reads recorded declarations/history from the branch
(`ctx.sessionManager.getBranch()`), re-renders the panel, and marks recorded
services `orphaned`. Fork = new session, new host; services are never shared
across forks. Reload (`ctx.reload()`) stops the old host and recreates it from
declarations; live processes are lost.

### 3.5 Authoring and commands

- The authoring agent is **pi's model**, via opt-in tools: `dc_serve` (starts,
  non-blocking, like `dc_run`), `dc_ps`, `dc_call`, `dc_cast`, `dc_stop`.
- Slash: `/dc-serve`, `/dc-ps`, `/dc-call`, `/dc-cast`, `/dc-tail`, `/dc-stop`,
  `/dc-cancel [name|all]`.

### 3.6 "The user continues"

The user keeps chatting with pi. Services advance in the background driver. Pi
learns about a meaningful state change on its next turn via a one-line
`sendMessage` (or not at all, if nothing changed). Approval prompts are the sharp
edge: `ctx.ui.confirm` is a single dialog, so a service must not seize the editor
— grant an explicit **capability set** at `dc_serve`, queue anything else, and
present only when pi is idle.

### 3.7 Model

Model calls are tagged per process; usage rolls up to the process, its
supervisor, and pi's session totals. A service **pins its model** at `dc_serve`
(deterministic) unless it opts into following the session. Foreground and
services share a per-provider semaphore, and services run under a budget cap.

---

## 4. Shared concerns

| Concern | `[REPL]` | `[pi]` |
|---|---|---|
| Isolation default | L1 is natural (single process, cooperative) | L2 by default for services |
| Approvals | the `bash` human channel; serialized through the REPL editor | `ctx.ui.confirm`; capability grants at `dc_serve` |
| Context isolation | services run with fresh memory; the REPL is their UI | services are `isolated`; the widget is their UI |
| Budget/usage | per-process counters into the status bar | per-process, rolled into session totals + caps |
| Termination | `/halt`, Ctrl-C, `dispose()` | `session_shutdown`, reload, `/dc-stop` |
| Storage | `.deepclause/repl/sessions/<id>/` | pi session + `.pi/deepclause/` (**never `.deepclause/`**) |

---

## 5. Phasing

1. **Runtime core** (host-neutral): process owner interface, process table,
   session-scoped signal, job registry, scheduler tick, `serve`/`ps`/`stop`.
   *Neither host changes yet.*
2. **REPL adapter**: owner inside `DMLSession`, run-queue extension of the
   existing loop, status segment, `/ps`, `serve` harness tool.
3. **pi adapter**: `DcSessionHost`, background driver, `dc_*` widget/commands,
   session entries, resume/fork semantics.
4. **Governance**: capability grants, approval queue, per-provider semaphore,
   budgets, budget-exhaustion behavior.
5. **Background `swarm` (`swarm_bg`)** and DML-native supervisors across turns,
   on top of the same owner.

Building the runtime core first is what keeps the two hosts from drifting: the
REPL and pi are then two owners of one scheduler and one process model, not two
implementations.

---

## 6. Open questions

1. **Target host.** Is this work for the REPL, for pi, or for both via the shared
   core? The runtime core is the same either way; the adapters are independent.
2. **`[pi]` resolving `ctx` at idle time.** The host must call models while no
   turn is active and across handler boundaries. Is a refreshed context provider
   enough, or does pi need an explicit idle-time model facility?
3. **`[REPL]` run-queue integration.** Can the existing `stepLoop` become a
   multi-frame scheduler without disturbing steering, the solution pager, and
   `ask_user` on the shared editor?
4. **L1 vs L2 default** per host, and the resource cost of L2 on small machines.
5. **Background approvals**: reach the user without stealing the editor; timeout
   and deny defaults.
6. **Wake policy `[pi]`**: when a service may `sendUserMessage` (trigger a turn)
   versus only light the status chip.
7. **Context budget**: exactly what is injected per turn for a running desk, and
   how it stays out of compaction.
8. **Reload/resume semantics**: is declaration-replay enough, or is a
   persist-and-resume model worth the complexity?
9. **Determinism and testing**: a session with background processes needs a
   seeded scheduler and a way to quiesce services in tests.
