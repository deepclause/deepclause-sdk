# Subagents and session-scoped processes in the REPL — specification

Status: **spec (v1 targets L1; L2 seam designed in)**.
Scope: `deepclause repl` only.

Related:
[`DML_REPL_SUBAGENTS.md`](./DML_REPL_SUBAGENTS.md) (the simple example),
[`DML_REPL_SERVICES_UX.md`](./DML_REPL_SERVICES_UX.md) (the UX),
[`DML_SPECIALIZED_PROCESSES_EXAMPLE.md`](./DML_SPECIALIZED_PROCESSES_EXAMPLE.md)
and [`DML_BEAM_SWARM_EXAMPLE.md`](./DML_BEAM_SWARM_EXAMPLE.md) (the model),
[`DML_REPL_WORKER_EXECUTION.md`](./DML_REPL_WORKER_EXECUTION.md) (the worker plan
this spec plugs into).

---

## 0. Scope, goals, non-goals

### 0.1 Goal

Let the authoring agent or the user start **subagents** in the REPL from ordinary
DML. A subagent is a DML process that runs a goal: it may **run and exit**, **wait
for messages** (from the user or other processes), or **ask the user and wait**.
Processes are **session-scoped**: they survive the turn that started them, are
visible in the UI, and are stoppable. The whole system state is queryable.

### 0.2 The L1/L2 requirement

- **v1 runs at L1**: one `swipl-wasm` instance, cooperative scheduling on the
  REPL thread. No worker threads, no new processes.
- **The architecture must make L2 a mode switch**, not a rewrite:
  `deepclause repl --execution worker` moves execution behind the same interfaces.
  This is a hard architectural constraint on v1, not a later optimization. In
  particular, v1 **MUST NOT** depend on any behavior that cannot cross a process
  boundary (shared variables, live closures, direct object references, shared
  filesystem handles).

Everything below is specified so that this holds.

### 0.3 Non-goals (v1)

- No distributed REPL (multiple machines). L2 is local workers.
- No AgentVM-per-process (see the worker plan, phase 6).
- No persistent processes across REPL restarts. Only declarations/history.
- No hot code upgrade of a running process (a process keeps its snapshot).
- No remote/`pi` host. This is the REPL.

---

## 1. Terminology

| Term | Meaning |
|---|---|
| **process** | an isolated DML execution with its own module snapshot, memory, mailbox, and lifecycle. The unit of a subagent. |
| **pid** | `{ node, session, id }` — a globally unique, node-qualified process identifier. |
| **node** | where a process runs: `"main"` (L1, same thread) or a worker id (L2). |
| **host** | `ProcessHost` — the owner of processes: spawn, send, stop, inspect, schedule. |
| **broker** | `CapabilityBroker` — model, tools, human channel, events, usage. Always executes on the main thread. |
| **scheduler** | the run queue that steps process frames and services their yields. |
| **session** | a REPL session (`.deepclause/repl/sessions/<id>/`). |
| **run** | one foreground `/goal` or authoring turn. |
| **snapshot** | a process's private copy of the living module at spawn time. |

---

## 2. Architecture

### 2.1 Layering

```
┌──────────────────────────────────────────────────────────────────────┐
│ REPL UI: transcript · services panel · status bar · editor            │
├──────────────────────────────────────────────────────────────────────┤
│ Harness tools: spawn · ps · send/call · stop · tail   (authoring agent)│
├──────────────────────────────────────────────────────────────────────┤
│ DML surface: spawn/2 send/2 receive/2 exit/1 ...      (built-ins)     │
│ libraries: service · swarm · supervisor                               │
├──────────────────────────────────────────────────────────────────────┤
│ ProcessHost        (LocalProcessHost | WorkerProcessHost)   ← SEAM    │
│ CapabilityBroker   (DirectBroker       | RpcBroker)         ← SEAM    │
├──────────────────────────────────────────────────────────────────────┤
│ Scheduler (run queue + timers)   ·   swipl-wasm engines   ·   MI      │
└──────────────────────────────────────────────────────────────────────┘
```

The two **seam** interfaces are the entire L1→L2 migration surface. v1 implements
both with local classes; L2 supplies RPC implementations and a worker pool.

### 2.2 `ProcessHost`

```ts
type NodeId = 'main' | string;              // L2: 'w0', 'w1', …
interface Pid { node: NodeId; session: string; id: string }

interface SpawnOptions {
  name?: string;                            // label; defaults to goal functor
  memory?: 'fresh' | 'parent';              // default 'fresh'
  priority?: 'foreground' | 'interactive' | 'background';  // default 'background'
  timeoutMs?: number;                       // wall-clock, enforced at yield points
  maxGas?: number;                          // inference budget
  code?: string;                            // module version; default current
  link?: Pid;                               // link at spawn
  monitor?: boolean;
}

type ProcessState =
  | 'runnable' | 'running' | 'waiting'      // waiting: on I/O, timer, or mailbox
  | 'input'                                  // waiting on the human channel
  | 'done' | 'crashed' | 'killed';

interface ProcessInfo {
  pid: Pid; name: string; goal: string; state: ProcessState;
  priority: string; mailboxDepth: number; links: Pid[]; monitors: Pid[];
  usage: LLMUsage; startedAt: number; restarts: number;
  currentActivity?: string; parent?: Pid; codeVersion: string;
}

interface ProcessHost {
  spawn(goal: string, options?: SpawnOptions): Promise<Pid>;
  send(to: Pid, message: unknown): void;                 // copied, fire-and-forget
  call(to: Pid, message: unknown, timeoutMs: number): Promise<unknown>;
  stop(pid: Pid, reason: unknown): Promise<void>;
  monitor(pid: Pid): { ref: string; down: Promise<unknown> };
  processInfo(pid: Pid): ProcessInfo | undefined;
  processes(): ProcessInfo[];                            // whole system state
  register(name: string, pid: Pid): void;
  whereis(name: string): Pid | undefined;
  shutdown(): Promise<void>;                             // idempotent
  tick(): number;                                        // step runnable work; returns steps done
}
```

`send`, `monitor`, `processInfo`, `processes`, `register`/`whereis` are the same
operations the DML built-ins expose. The built-ins are thin wrappers over the host.

### 2.3 `CapabilityBroker`

```ts
interface CapabilityBroker {
  completeModel(req: LLMBackendRequest, ctx: { pid: Pid; signal: AbortSignal }): Promise<LLMBackendResponse>;
  runTool(name: string, args: Record<string, unknown>, ctx: { pid: Pid; signal: AbortSignal }): Promise<unknown>;
  requestInput(prompt: string, ctx: { pid: Pid; signal: AbortSignal }): Promise<string | undefined>;
  emit(event: DMLEvent, ctx: { pid: Pid }): void;
  recordUsage(usage: LLMUsage, ctx: { pid: Pid }): void;
  codeVersion(): string;
}
```

**Invariant B (broker centralization).** A worker/process never talks to a model
provider, the filesystem, the terminal, or the network directly. All side effects
go through the broker, which applies the tool policy, path isolation, and
approvals **on the main thread**. This is what makes L2 safe without moving
credentials or policy into workers.

### 2.4 Pids are location-transparent

A pid carries its `node`. `send(to, msg)` routes: local enqueue, or (L2) a
message to `to.node` via the main thread. Calling code never branches on L1/L2.
This is the single most important L2 enabler, and it costs nothing in v1.

---

## 3. Lifetimes and ownership

| Tier | Owner | Ends | Example |
|---|---|---|---|
| run | the foreground turn | the entry goal returns | a `swarm` inside a `/goal` |
| session | the `ProcessHost` | `/stop all`, `/halt`, REPL exit, dispose | a `spawn`ed subagent |

- `spawn` **MUST** create a **session-scoped** process. It keeps running after the
  goal that spawned it returns.
- The `ProcessHost` **MUST** be created when the REPL session is created and
  disposed idempotently on `/halt`, Ctrl-C exit, and `HarnessRepl.dispose()`.
- A library that wants run-scoped children (e.g. `swarm`) **MUST** explicitly
  await or stop them; it cannot rely on the run ending.
- `/stop all` and session teardown **MUST** stop every process, in reverse spawn
  order, and wait a bounded grace period before killing.

---

## 4. DML surface

### 4.1 Core built-ins (v1)

| Predicate | Semantics |
|---|---|
| `spawn(Goal, Pid)` | start a process running `call(Goal)`; returns immediately |
| `spawn_opt(Goal, Pid, Options)` | as `spawn` with `SpawnOptions` (a dict) |
| `self(Pid)` | this process's pid |
| `send(PidOrName, Msg)` | copy `Msg` into the target mailbox; fallible if the target is gone |
| `receive(Msg)` / `receive(Msg, TimeoutMs)` | FIFO pop from the mailbox; blocks; fails on timeout |
| `receive_match(Template, Msg, TimeoutMs)` | selective receive; see §4.4 |
| `exit(Reason)` | terminate self with `Reason` |
| `exit(Pid, Reason)` | signal an exit to another process |
| `exit(Pid, kill)` | untrappable terminate |
| `link(Pid)` / `unlink(Pid)` | bidirectional exit propagation |
| `monitor(Pid, Ref)` / `demonitor(Ref)` | deliver `{'DOWN', Ref, process, Pid, Reason}` |
| `process_flag(trap_exit, Bool)` | receive `{'EXIT', Pid, Reason}` as a message |
| `process_info(Pid, Info)` | one process's state, or the whole map when unbound |
| `processes(Pids)` | all live pids in the session |
| `register(Name, Pid)` / `whereis(Name, Pid)` | session-scoped naming |
| `make_ref(Ref)` | unique reference term |
| `now_ms(T)` | monotonic milliseconds |
| `sleep(Ms)` | yield for at least `Ms` |
| `self_usage(U)` | this process's accumulated LLM usage |
| `code_version(V)` | the module version this process was spawned with |

### 4.2 Value semantics (mandatory, even at L1)

**Invariant A (copying).** `spawn` copies its goal; `send` copies its message.
Variables in a copied term are **fresh in the receiver**. There is no shared
variable binding between processes.

- A result returns only through a message (`send`) or the process's terminal
  `answer/1` observed by a monitor. The parent's variables are never bound by a
  child.
- At L1 this is implemented by serializing with `write_canonical/1` and
  `read_term/2` across the process boundary. This is not "free", and that is the
  point: it forces the code to be L2-portable.
- Non-ground, non-serializable terms (live streams, JS handles) **MUST** be
  rejected with a clear error at `spawn`/`send`.

`library(swarm)` and `service_call/3` already obey this; the spec makes it
explicit so that v1 code does not accidentally rely on sharing.

### 4.3 Process-local isolation

**Invariant C (snapshot).** A process runs against a **private module snapshot**
taken at spawn: the current living-module text, loaded under a process-local
namespace. Consequences:

- `assert/1`, `retract/1`, `define` inside a process affect only that process.
- A process does not see predicates committed after its spawn. `reload` adopts
  the current version explicitly.
- Memory (`system/1`, `user/1`, task history) is private; `memory:'fresh'`
  (default) starts empty; `memory:'parent'` copies the spawner's conversation.
- The implementation reuses the MI's per-session namespacing already used for
  nested frames and tool engines; each process gets its own `SessionId`.

### 4.4 `receive` and selective receive

- `receive(Msg)` pops the **oldest** message. `receive(Msg, T)` fails after `T` ms.
  This is the recommended v1 form: pop anything, then dispatch with `=/2`.
- `receive_match(Template, Msg, T)` scans the mailbox in order, unifying each
  candidate **in the receiving process** (so its bindings apply). Non-matching
  messages are requeued in order. On timeout it fails.
- Requeue preserves global order among non-matching messages. A pathological
  pattern that matches nothing but is retried in a loop is the sender's bug; the
  mailbox bound (§13.3) makes it visible rather than fatal.
- The `{request, From, Ref, Msg}` / `{reply, Ref, Value}` protocol in
  `library(service)` is the encouraged shape, so selective matching is limited to
  a shallow header.

### 4.5 `input/2`, `output/1`, `answer/1` inside a process

- `input(Prompt, Answer)` yields `wait_input`. The host enqueues a human-channel
  request on the **main thread** and suspends the process (`state: 'input'`).
  When answered, the process resumes with `Answer`. A cancelled request fails the
  goal (existing semantics).
- `output/1`, `yield/1`, `log/1` emit events attributed to the process; they are
  routed to the service sink (§8.4), never interleaved into a foreground stream.
- `answer/1` terminates **this process** and records a terminal result. It does
  not terminate the REPL or sibling processes. The result is surfaced as a
  lifecycle line and, for a monitored process, as the DOWN reason
  (`normal` + `{answer, Value}`).

### 4.6 Libraries (on top of the built-ins)

```prolog
%% library(service): a named request/reply process. NOTE: this is sugar over
%% spawn + register + a receive loop, not a distinct primitive.
service_start(Name, Init, Handler).
service_call(Name, Request, Reply [, TimeoutMs]).
service_cast(Name, Message).

%% library(swarm): fan-out over a list (see the swarm doc).
swarm(Worker, Items, Results [, Options]).
pmap(Worker, Items, Values).

%% library(supervisor): OTP-style supervision.
supervisor_start(Specs, Options, SupPid).
supervisor_stop(SupPid, Reason).
```

The authoring guide **MUST** present `spawn` as the primitive and `serve` as a
convenience, so the mental model stays "spawn a subagent; the goal decides its
shape".

---

## 5. Scheduling at L1

This is the main new runtime component.

### 5.1 The run queue

```
Scheduler
├── runnable:  priority buckets  [foreground, interactive, background]
├── waiting:   pid -> { model | tool | input | mailbox | timer | monitor }
├── timers:    min-heap of (deadline, pid)
└── steps this tick, inference budget per step
```

`tick()` picks the highest-priority runnable pid, steps its frame **once**
(`DMLRunner.stepFrameOnce`), handles the yielded request, and repeats until a
bounded step budget for the tick is spent or nothing is runnable. The REPL calls
`tick()` from a driver (see §5.4).

### 5.2 Yield points and fairness

A step runs until the process yields:

- `request_agent_loop` / `request_llm` / `request_judgment` / `request_sample_token`
- `request_exec` (a tool)
- `wait_input`
- `receive` with an empty/again-unmatched mailbox
- `sleep`
- `spawn` / `send` / `exit` / link/monitor bookkeeping
- an **inference-limit** quantum, so a greedy Prolog goal cannot starve the queue

The per-step inference limit uses `call_with_inference_limit/3` (already used in
the MI). A process that exceeds its `maxGas` or `timeoutMs` at a step boundary
terminates with `budget_exhausted` / `timeout`.

### 5.3 Priorities and responsiveness

- `foreground` — the user's current `/goal` or authoring turn. Strict highest.
- `interactive` — a process awaiting or producing human input the user asked for.
- `background` — services and subagents.
- Strict priority can starve background; the scheduler uses weighted
  round-robin (e.g. 4 foreground : 1 background) once the foreground is I/O-bound.
- A tick **MUST** return to the event loop frequently (bounded steps and a soft
  time budget) so the TUI redraws and keystrokes stay responsive. A single
  synchronous Prolog step cannot be preempted in L1; the inference limit bounds
  it.

### 5.4 The driver

The REPL's main loop already awaits user input. The scheduler runs alongside it:

- a `setImmediate`/`setInterval(0)` driver calls `tick()` while the host has work;
- when the editor is awaiting a line, ticks still run (background progress);
- when `input/2` is requested, the request is enqueued and the editor shows it at
  the next idle point (§8.5);
- when nothing is runnable and no timer/IO is pending, the driver parks (no busy
  loop) and wakes on `send`, timer expiry, or I/O completion.

The foreground turn is registered as a `foreground` process and driven by the same
scheduler, so the CLI awaits its terminal state rather than running its own inline
loop. This is the largest refactor in v1 and is isolated behind the scheduler
module.

---

## 6. The L2 seam (designed now, built later)

### 6.1 What L2 changes

- `LocalProcessHost` → `WorkerProcessHost`: a pool of workers, each with its own
  `swipl-wasm` instance and scheduler. Pids get `node: 'w0' | 'w1' | …`.
- `DirectBroker` → `RpcBroker`: worker-initiated `need(model|tool|input)` calls
  are messages to the main thread, serviced there (credentials, policy, UI).
- Cross-node `send` is routed through the main thread (a broker), preserving pid
  transparency.

### 6.2 What must already be true in v1 for this to work

1. **Node-qualified pids** (§2.4) — implemented in v1, always `'main'`.
2. **All process ops behind `ProcessHost`** — no direct engine manipulation from
   the UI or tools.
3. **All capabilities behind `CapabilityBroker`** — no direct model/FS/terminal
   access from process code.
4. **Value semantics for goals and messages** (§4.2).
5. **Serializable goals**: a spawn goal is a Prolog term that round-trips through
   `write_canonical`/`read_term`. Closures are terms with their bound arguments.
6. **The scheduler is a standalone module** with no dependency on the TUI.
7. **`process_info` and events are plain data**, not live objects.

### 6.3 Worker protocol (sketch; see the worker plan for the full form)

```
main → worker:  init { workspace, codeText, limits, node }
                spawn { goal, options, processId }
                send { to, message }
                stop { pid, reason }
                info { pid } / list {}
                snapshot {}        (code server)
                dispose {}
worker → main:  ready {}
                event { pid, event }
                need { requestId, pid, kind: model|tool|input, payload }
                resolved { requestId, value | error }
                exited { pid, reason, usage }
```

The worker runs the same scheduler over its own processes. Host calls suspend the
process and send `need`; the main thread resolves them through the
`CapabilityBroker`. A kill is `worker.terminate()` for every process on that node.

### 6.4 The mode switch

```
deepclause repl                       # --execution inline (default)
deepclause repl --execution worker    # L2
deepclause repl --execution worker --workers 4
```

Config equivalent: `.deepclause/config.json` → `repl.execution.mode`. **The DML
surface, libraries, commands, and UX are identical in both modes.** A shared
conformance test suite (§14) runs against both.

---

## 7. Session record and resume

### 7.1 Record

Under the existing session directory,
`session/<id>/services.jsonl` is an append-only log:

```jsonc
{"type":"declare","pid":{"node":"main","session":"s","id":"p1"},
 "name":"summarise","goal":"summarise(\"qec\")","options":{...},
 "codeVersion":"v12","parent":null,"startedAt":1712}
{"type":"state","pid":"p1","state":"running","at":1713,"activity":"task(...)"}
{"type":"result","pid":"p1","answer":"bullets(...)","usage":{...}}
{"type":"exit","pid":"p1","reason":"normal","at":1719}
```

Declarations are what make resume possible; states/results are the history.

### 7.2 Resume

- On `--session`, the host replays declarations and marks them `recorded`, never
  `running`. A process is not serializable, so no live state is claimed.
- `/ps` shows recorded processes dimmed; `/spawn --resume <name>` restarts one
  from its declaration (same goal/options/code version), and replays its prior
  result line so the story continues.
- `library(service)` state is not restored; a resumed service starts from its
  `Init` unless the declaration names a persisted seed.

### 7.3 Fork and rollback

- **Fork** creates a new session with a new host and a new record; service
  declarations may be copied, live processes are not shared.
- **`/rollback`** affects the harness transaction history only. Services are
  runtime state and are untouched; the spec is explicit about this so users are
  not surprised.

---

## 8. REPL UX

(This consolidates [`DML_REPL_SERVICES_UX.md`](./DML_REPL_SERVICES_UX.md); that
document is now the rationale, this is the normative behavior.)

### 8.1 Status segment

The one-row status bar gains one segment after the run state: `⚙ <name> done/total`
for a single service, `⚙ N svc` for several, `⚙ name !` when attention is needed.
Omitted when no process exists. It **MUST** respect the existing whole-segment
drop rule.

### 8.2 Services panel

A collapsible dock component above the editor. Collapsed: one line per process.
Expanded (`/ps --watch`): the process tree with per-process state, activity,
mailbox/queue depth, restarts, and usage. Polled at a low rate (~600 ms) like the
harness view. On narrow terminals it collapses to the status segment.

### 8.3 Transcript lines

Lifecycle is one dim line per event, with a `⚙` prefix; a terminal result is an
attributed answer block. Events are written **only when no foreground stream is
open** (deferred otherwise), reusing the renderer's block-separation state.

### 8.4 The service sink

A second output channel alongside the renderer's document sink. Process events
are written to it. It **MUST** defer writes while a foreground stream or tool pane
is open, and **MUST** prefix focused-service output with `⚙<name>`. This is the
one new plumbing component.

### 8.5 The input queue

Human-channel requests from processes (`input/2`) are queued. When the editor is
idle, the request takes over the editor border (as `ask_user` does). While a turn
is running, the request waits and the panel shows a pending indicator. A request
**MUST NOT** interrupt a running stream or steal focus.

### 8.6 Commands

| Command | Behavior |
|---|---|
| `/ps [name] [--watch]` | tree snapshot / toggle the pinned panel |
| `/send <name> <text>` | deliver `{text, Text}` |
| `/call <name> <term>` | request/reply; prints the reply |
| `/cast <name> <term>` | fire-and-forget |
| `/tail <name> [n]` | last `n` buffered events |
| `/focus <name>` / `/unfocus` | stream a process's output into the transcript |
| `/stop <name>` / `/stop all` | graceful stop; `/kill <name>` immediate |
| `/spawn --resume <name>` | restart a recorded declaration |
| `/answer <name> <text>` | resolve a pending input request |

### 8.7 Interrupts

- **Esc** cancels the foreground turn only; processes are untouched.
- **Ctrl-C**: if idle with processes running, confirm `stop N processes? [y/N]`;
  otherwise exit. `/halt` stops all processes then exits, flushing the record.
- A stopped/killed process emits a final `exit` line and is removed from the
  panel.

---

## 9. Authoring agent integration

### 9.1 Harness tools (added to `HarnessToolController`)

| Tool | Purpose |
|---|---|
| `spawn` | start a subagent: `{goal, options}` → `{pid, name}` |
| `spawn_opt` | as above with explicit options |
| `ps` | list processes (whole system state) |
| `send` / `cast` | message a process |
| `call` | request/reply with a timeout |
| `stop` | stop a process |
| `tail` | recent events from a process |
| `serve` | **alias** for the common named request/reply process; documented as sugar over `spawn` |

### 9.2 Prompt guidance (added to the authoring agent's system prompt)

- To run work that outlives the current turn, `spawn` a subagent.
- The goal's shape decides the lifecycle: exit = one-shot, `receive` loop =
  service, `input/2` = interactive.
- Do **not** await a subagent unless the user asked for its result; report its
  name and continue.
- Inspect with `ps`; never busy-wait.
- Subagent code is isolated: `assert`/`retract` inside it are private; results
  come back by message.

### 9.3 Recursion and safety

- A `spawn` is not a self-recursion hazard the way a DML tool is (the existing
  `toolCallStack` guard is about tools). Recursive subagents are a feature (RLM),
  bounded by `maxProcesses` and optional `maxDepth`.
- The host **MUST** reject a spawn when `maxProcesses` is reached, with a clear
  error, rather than accepting unbounded work.

---

## 10. Configuration and CLI

```jsonc
// .deepclause/config.json
"repl": {
  "execution": {
    "mode": "inline",        // "inline" (L1) | "worker" (L2, later)
    "workers": 2,            // L2 only
    "workerTimeoutMs": 300000
  },
  "services": {
    "enabled": true,
    "maxProcesses": 16,
    "maxMailbox": 1000,
    "defaultTimeoutMs": 300000,
    "sessionBudgetTokens": 500000,
    "perProcessBudgetTokens": 100000,
    "receiveDefaultTimeoutMs": 0   // 0 = block forever; timeouts still recommended
  }
}
```

| Flag | Effect |
|---|---|
| `--execution inline\|worker` | select L1/L2 (default `inline`) |
| `--workers N` | L2 worker pool size |
| `--max-processes N` | override `maxProcesses` |
| `--no-services` | disable `spawn` (foreground only) |

`--execution worker` **MUST** present the same UX; a warning is printed when a
configured feature is not yet available at L2.

---

## 11. Observability

- `process_info(Pid, Info)` and `processes(Pids)` expose the tree; the panel and
  `/ps` render exactly these.
- Each process has a bounded ring buffer of its events; `/tail` reads it.
- Usage is recorded per process via the broker and rolled into the status bar.
- An optional trace mode emits `tool_call`/`model_call` events per process for the
  harness view.
- On crash, `process_info` retains the last state and reason for one "reap"
  window so `/ps <name>` can show `crashed: verification_failed`.

---

## 12. Security and approvals

- Subagents use the **same tool policy** as the foreground. The broker applies it;
  a worker cannot bypass it (it has no direct tool access).
- Approval-gated tools (`bash`) invoked by a subagent enqueue a human request. It
  is presented at idle (§8.5); the subagent stays suspended meanwhile.
- Path isolation is enforced by the broker on the main thread; a process sees the
  same workspace confinement as the foreground.
- A process cannot register a new tool, change the model provider, or read
  credentials. Those are host capabilities.

---

## 13. Errors, limits, edge cases

### 13.1 Unknown / dead target

`send` to a dead pid **MUST** fail with `noproc` (a Prolog failure with a
diagnostic), not crash the sender. `whereis` returns nothing for a dead name.
`register` on a taken name throws `permission_error`.

### 13.2 Timeouts and deadlock

- `receive(T)` and `call(T)` time out. `input/2` can be cancelled from the UI.
- The host **SHOULD** maintain a wait-for graph (process → process it is blocked
  on) and surface a `/ps` warning for a cycle. It **MUST NOT** kill automatically.

### 13.3 Mailbox bound

`maxMailbox` is enforced. `send` to a full mailbox **MUST** either block the
sender (yielding) or fail with `mailbox_full`, per a documented policy; the
default is to fail the sender's `send` so a runaway producer is visible.

### 13.4 Budget

- `maxGas` and `timeoutMs` are enforced at step boundaries.
- Per-process and session token budgets are enforced by the broker before a model
  call; exceeding them terminates the process with `budget_exhausted` and emits a
  lifecycle line. A budget-stopped process is *not* restarted.

### 13.5 Code versioning

- `spawn` snapshots the current version; the record stores `codeVersion`.
- A commit (`end_turn`) does not alter running processes. A new `spawn` sees the
  new version.
- `reload` inside a process adopts the current version explicitly; the panel
  shows the version so drift is visible.

### 13.6 Nondeterminism

Scheduling introduces nondeterministic interleavings. This is intended for
services; tests and reproducible runs use a **deterministic scheduler mode**
(manual `tick`, FIFO delivery, seeded refs) exposed to the test harness, not to
users.

### 13.7 Name/pid stability

Names are session-scoped; on process exit the name is released. A pid is never
reused. Recorded pids are historical only.

---

## 14. Testing strategy

1. **Primitive unit tests**: spawn/send/receive/receive_match/exit/link/monitor/
   register/process_info, each with a deterministic scheduler.
2. **Isolation tests**: a child's `assert` does not affect the parent; messages
   are copied (mutating a received term does not affect the sender).
3. **Shape tests**: the three subagent shapes from the simple example (one-shot,
   message-waiting, interactive).
4. **UX tests**: status segment, panel rendering, service-sink deferral (no
   interleaving with a foreground stream), input queue at idle.
5. **L2 conformance suite (the migration gate)**: every primitive and shape test
   runs under `--execution inline` and, once available, `--execution worker`, with
   identical observable results. This suite is the definition of "easy migration".
6. **Resume tests**: declarations replay, recorded processes are not `running`.

---

## 15. Phasing

Ordered to keep the seam honest.

1. **Seam + primitives (L1).** `ProcessHost`/`CapabilityBroker` interfaces,
   `LocalProcessHost`, `DirectBroker`, node-qualified pids, value semantics, the
   core built-ins, and the standalone `Scheduler`. Foreground still inline;
   background processes driven by a tick.
2. **REPL UX.** Service sink, status segment, panel, `/ps`/`/send`/`/call`/
   `/stop`/`/tail`, input queue, Ctrl-C policy.
3. **Libraries.** `service`, then `swarm`/`pmap`, then `supervisor`.
4. **Authoring agent.** `spawn`/`ps`/`send`/`call`/`stop` tools and prompt text.
5. **Persistence.** `services.jsonl`, resume, fork semantics.
6. **L2 seam.** Worker protocol + `RpcBroker`, then `--execution worker` with a
   single worker (whole-runtime), then a pool with distributed pids.
7. **Governance.** Budgets, approval queue, deadlock warning, trace mode.

Steps 1–2 are the feature the user asked for. Step 6 is the mode switch, and it is
cheap **only if** steps 1–5 respect §6.2.

---

## 16. Open questions

1. **Foreground scheduler refactor.** Do we unify the foreground into the
   scheduler in step 1, or keep it inline and tick background processes around it?
   (Recommended: unify, but it is the riskiest change.)
2. **`memory:'parent'` cost.** Copying the whole conversation into a child is
   expensive; is it needed in v1, or only `fresh` + explicit seeds?
3. **Selective receive cost.** Is `receive_match` worth it in v1 given the
   `{request, From, Ref, Msg}` convention already tags messages?
4. **Process-local snapshot cost.** Loading the module per process may be
   expensive; can static clauses be shared read-only while dynamic ones stay
   process-local?
5. **Ctrl-C default.** Confirm-before-stop vs stop-foreground-only.
6. **Resume policy.** Trusted-workspace auto-restart vs always explicit.
7. **Mailbox-full policy** default (fail vs block).
8. **L2 granularity.** Whole-runtime worker first, or per-process workers from the
   start? (Whole-runtime is simpler and delivers UI responsiveness; per-process
   delivers true subagent parallelism.)

---

## Appendix A — Primitive quick reference

```prolog
spawn(Goal, Pid).
spawn_opt(Goal, Pid, opts{name: _, memory: _, priority: _, timeoutMs: _}).
self(Pid).
send(PidOrName, Msg).
receive(Msg).            receive(Msg, TimeoutMs).
receive_match(Template, Msg, TimeoutMs).
exit(Reason).            exit(Pid, Reason).        exit(Pid, kill).
link(Pid).               unlink(Pid).
monitor(Pid, Ref).       demonitor(Ref).
process_flag(trap_exit, Bool).
process_info(Pid, Info). processes(Pids).
register(Name, Pid).     whereis(Name, Pid).
make_ref(Ref).           now_ms(T).                sleep(Ms).
self_usage(U).           code_version(V).
```

## Appendix B — The three shapes, minimal

```prolog
one_shot(Topic) :- task("Summarise '{Topic}' in 3 bullets.", list(string(B))), answer(bullets(Topic, B)).

waiter(Acc) :- receive(M),
    ( M = {add,X,From,Ref} -> send(From,{ok,Ref}), waiter([X|Acc])
    ; M = {text,Text}      -> output("noted: {Text}"), waiter([Text|Acc])
    ; M = stop             -> answer(Acc)
    ; waiter(Acc) ).

interactive(Q) :- input(Q, A), answer(got(A)).

start(T) :- spawn(one_shot(T), _), spawn(waiter([]), _), spawn(interactive("Which deadline?"), _).
```

## Appendix C — L1 vs L2 at a glance

| Aspect | L1 `--execution inline` | L2 `--execution worker` |
|---|---|---|
| session ownership | same thread, same process | main thread + worker pool |
| process identity | frame/engine in one swipl | frame in a worker's swipl |
| messages | copied via serialization | serialized over the worker protocol |
| model/tool/input | direct broker call | RPC to main thread |
| cancel | abort signal / kill signal at yields | `worker.terminate()` |
| CPU parallelism | none (cooperative) | real |
| UI | identical | identical |
| DML surface | identical | identical |
