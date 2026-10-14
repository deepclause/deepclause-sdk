# Parallel and background execution

Status: **design / discussion**. Companion to
[`DML_REPL_WORKER_EXECUTION.md`](./DML_REPL_WORKER_EXECUTION.md) (out-of-process
workers, parallel executes, background jobs). This document narrows that broad
plan to three concrete primitives and works out their exact semantics:

1. `execute_background/2` — run an `execute`-style DML goal in the background,
   stream its output to a buffer/log, return a job id immediately.
2. `parallel_maplist/3` (and friends) — fan a goal over a list concurrently.
3. Parallel tool calls inside `task/N` — run the several tools one model
   response asks for concurrently.

It describes what exists today, where the hard constraints are, the design
options, and a recommended shape per feature.

---

## 1. What exists today (the constraints that shape everything)

### 1.1 One cooperative Prolog engine, no preemption

- `swipl-wasm` is a **per-JS-realm singleton** (`loadProlog()` in
  `src/prolog/loader.ts`, `swiplInstance`). `mountWorkspace` is likewise global.
- DML execution is a **cooperative step loop**: `DMLRunner.stepLoop` calls
  `deepclause_mi:step_frame/4` repeatedly; a frame suspends at an `engine_yield`
  and the runner services the yielded request (model call, tool call, input)
  from TypeScript, then resumes. See `src/runner.ts` (`stepLoop`, `stepFrame`)
  and `src/prolog-src/deepclause_mi.pl` (`mi_call/3`, `engine_yield`).
- Frames are **strictly depth-first**. The runner's own comment is explicit:

  > Frames are strictly depth-first: a parent frame is suspended at an
  > `engine_yield` while a child frame runs to completion, so the per-session
  > result/signal slots (`session_agent_result/2`, `session_pending_signal/2`, …)
  > are never concurrently pending. This is what makes keying those slots by
  > `SessionId` … safe. — `src/runner.ts`

  The slots (`session_agent_result/2`, `session_exec_result/2`,
  `session_pending_signal/2`) are declared in `deepclause_mi.pl` and keyed only by
  session, not by frame.
- **There is no interrupt inside a Prolog step.** The abort signal is checked
  *between* steps (`stepLoopInner`) and inside host tools (bash), but a runaway
  `label/1` or a deep `findall/3` cannot be preempted. There is no per-goal
  timeout (`docs/STATUS.md`, "Not implemented / known gaps").
- `engine_create/3`, `engine_yield`, `engine_next/2` are SWI **engines**
  (coroutines in one thread), not `thread_create`. They give interleaving at
  explicit yield points, not CPU parallelism.

**Consequence.** Concurrency (I/O-bound overlap: model calls, shell, network) is
achievable in-process by interleaving frames at yield points. True parallelism
(CPU-bound Prolog) and hard cancellation require separate workers/processes.

### 1.2 The `execute` primitive

`execute` is the REPL authoring tool that runs a goal against the scratch
harness:

- `DMLSession.executeGoal()` (`src/repl/session.ts`) calls
  `deepclause_mi:repl_execute_frame/4`, registers the returned frame, drives it
  with `DMLRunner.runFrame`, collects solutions/output, then restores harness
  memory. It is budgeted by frame depth (`maxDepth`, default 3) and a shared
  model-call count for the turn.
- `execute` is exposed to the authoring agent in `src/repl/harness-tools.ts`.
- The pi extension has no `execute` tool today; its equivalent is `/dc-run`
  and the opt-in, model-callable `dc_run` tool (`deepclause-pi/src/index.ts`).
  Everything in feature 1 applies to a future `execute_background` and to
  background `/dc-run`/`dc_run`.

### 1.3 Host tools are already out-of-process

`exec(bash(...))` yields `request_exec` to TypeScript, which runs the command
through `ShellManager`/`HostShellManager` (`src/system/runtime/shell-manager.ts`)
using `child_process.spawn`. It already takes an `AbortSignal`, an observer for
streamed stdout/stderr, a wall-clock `timeoutMs`, and an idle timeout. So a
**background shell command is essentially free**: the process is already
separate and cancellable.

### 1.4 `task/N` tool calls are sequential (in one path) and shared-slot bound

There are two agent loops in `src/agent.ts`:

- `runInjectedAgentLoop` (used with an injected `LLMBackend`, i.e. the pi
  extension's `createPiBackend`) executes the tool calls of one assistant
  message in an explicit sequential loop:

  ```ts
  for (const call of toolCalls) { … await tool.execute(call.arguments, …) }
  ```

- `runAgentLoop` (AI SDK path) builds `aiTool(...)` definitions with `execute`
  and lets the AI SDK drive them. The AI SDK may run a step's tool calls
  concurrently, but the runtime provides no ordering, concurrency cap, or
  shared-state guarantees around that.

DML-defined tools are routed through `DMLRunner.executeToolIsolated`, which
creates an isolated Prolog engine (`create_tool_engine`) precisely so that
concurrent execution does not corrupt the parent module. However, when a nested
tool itself calls `exec(...)`, the result is posted with
`post_exec_result(this.sessionId, …)` and read from the single
`session_exec_result/2` slot. The isolation solves *module state* races but not
*per-session result-slot* races. A parallel design must address this.

### 1.5 pi's host model already parallelizes

Pi's own agent may run tool calls from one assistant message in parallel, and
`pi.on("tool_execution_*")` handlers must tolerate out-of-order siblings. Any
nested work a tool starts must use `ctx.signal` (pi's docs, "Extensions →
Events and concurrency"). The extension's single-execution guard
(`claimExecution`/`activeController` in `deepclause-pi/src/index.ts`) is
deliberately one-slot today; background jobs and parallel branches need a
separate budget that does not fight it.

---

## 2. Shared substrate: a job registry and an isolation level

All three features are the same machine with different front doors. Define one
job model first.

### 2.1 Job record

```ts
interface DcJob {
  id: string;                       // "job_<ts>_<rand>"
  kind: 'shell' | 'goal' | 'task' | 'maplist';
  label: string;                    // human/model-readable
  state: 'queued' | 'running' | 'done' | 'failed' | 'cancelled' | 'timeout';
  createdAt: number;
  startedAt?: number;
  finishedAt?: number;
  parentJobId?: string;             // for nested/fan-out jobs
  scope: { sessionId: string; harnessVersion?: string; workspace: string };
  usage?: LLMUsage;                 // rolled up
  result?: unknown;                 // final answer/value
  error?: string;
  logPath: string;                  // buffered output (see 2.3)
  cancel(): void;                   // job-level cancellation
}
```

A **job registry** (per pi session / per SDK instance) owns the records, the
concurrency budget, and cancellation. This is the same registry the prior worker
plan calls the "job registry"; building it now pays off for feature 5 of that
plan.

### 2.2 Isolation levels (the in-process vs out-of-process decision)

| Level | Runs where | Cancellation | Parallelism | Cost |
|---|---|---|---|---|
| **L0 host shell** | `child_process` (already) | real (`SIGKILL`) | real (OS) | ~0 |
| **L1 cooperative frame** | same WASM instance, own session/frame, interleaved at yields | soft (abort between steps) | only across yield points; CPU-bound serializes | low |
| **L2 worker** (`worker_threads`/child) | own WASM instance | hard (`terminate`) | real | one WASM instance + RSS per worker |

Recommendation: **L0 for `exec(bash)` backgrounding**, **L1 as the default for
background goals and `parallel_maplist`** (it is cheap and sufficient for the
common case where the goal is I/O-bound through `task/N`/`exec`), and **L2
behind config/opt-in for anything that must be truly preemptible or CPU-parallel**.
This mirrors the prior plan's `LocalRuntimeHost` → `WorkerRuntimeHost` seam: L1 is
the local host, L2 is the worker host, and the job API is identical above the
seam.

The honest caveat, which the UI must not hide: **an L1 background goal that is
CPU-bound in Prolog will freeze the foreground.** That is acceptable for a
"background a slow report" use case only if the slowness is model/shell I/O, not
CLP. L2 is the answer when it is not. The `WorkerRuntimeHost` prototype (does
`swipl-wasm` load under `worker_threads`? — flagged risk in the prior plan) is
the gating spike.

### 2.3 Output buffering and the "tmp log file"

The user's proposed mechanism — *return an id and a tmp log file* — is the right
shape for an agent that frequently loses event streams. Design:

- Each job gets a directory: `${os.tmpdir()}/deepclause-jobs/<sessionId>/<jobId>/`
  with `events.jsonl` (one runtime event per line, bounded fields; image payloads
  replaced by paths) and `result.json` (terminal state + answer/error + usage).
- The registry also keeps a bounded in-memory tail (e.g. last 200 events) for
  instant `job_status`/UI reads.
- **Do not** write job logs under `.pi/deepclause/` by default. The root
  `AGENTS.md` says "Avoid persistent execution logs initially; pi's session is the
  execution record." A tmp dir honors that and still gives a path the agent can
  `read`/`tail` if it wants. Provide `config.jobs.logDir` to opt into a
  workspace-visible location, and always clean up on session shutdown.
- The returned job handle is small and model-friendly:

  ```json
  { "id": "job_...", "state": "running", "log": "/tmp/deepclause-jobs/.../events.jsonl",
    "result": "/tmp/deepclause-jobs/.../result.json" }
  ```

### 2.4 Cancellation

- **L0**: abort → `SIGKILL` the child (already implemented).
- **L1**: set the frame's `AbortSignal`; the runner notices between steps and
  yields an `error`/`cancelled` terminal. Best-effort.
- **L2**: `worker.terminate()` then respawn from the pool. Reliable.
- Job cancellation cascades to children (`parentJobId`) and to the job's shell
  children.
- `/dc-cancel` with no argument today cancels `activeController`; extend it to
  accept a job id, and keep the bare form as "cancel everything in this session".

### 2.5 Approvals and security

Background/parallel work must not become an approval-bypass. Keep the existing
model: capability tools are registered on the host and whitelisted
(`registerPiRuntimeTools` + `setToolPolicy` in `deepclause-pi/src/runtime.ts`).
- Approvals are requested on the **pi host thread**, with the job id in the
  prompt; parallel jobs may present several prompts, so approvals are serialized
  through a small queue and cancellation withdraws pending prompts.
- Each job is confined to the same workspace and path rules; no job may register
  or reveal new tools.
- `dc_run`/`execute_background` remain opt-in and must reject recursion,
  concurrent foreground execution, and contextual plans (as `dc_run` already
  does).

### 2.6 Usage accounting

Roll each job's `usage` events into the `DcJob` record and into the pi session
totals. Surface per-job and aggregate tokens/cost, exactly as the prior plan's
"per-job usage accounting" requires. The pi extension already accumulates usage
in `executeDml`; the registry should own the same accumulator for jobs.

---

## 3. Feature 1 — `execute_background`

### 3.1 Semantics

```prolog
% Start a goal in the background; succeed immediately with a handle.
execute_background(Goal, JobId).

job_status(JobId, Status).        % queued/running/done/failed/cancelled/timeout
job_tail(JobId, Lines).           % recent events as text
job_wait(JobId, TimeoutMs, Result). % block the current goal until done/timeout
job_cancel(JobId).
```

`Goal` is the same kind of goal `execute`/`repl_execute_frame` accepts — it runs
against the scratch harness and may itself call `task/N`, `exec/2`, etc. The
difference is *when* and *where*: `execute_background` returns a handle before
the goal finishes; the caller (agent or DML) polls/waits later.

Note the distinction from `exec_background(bash(...), JobId)`: that is just L0
(background the shell tool) and needs no DML frame. Both should exist, and both
go through the registry.

### 3.2 What "background" means per isolation level

- **Shell job** (`exec_background`): spawn now, return job id. The foreground
  DML goal continues; the goal can `job_wait` when it needs the result. No frame
  is involved. This is the highest-value, lowest-risk half and should ship first.
- **L1 goal job**: create a new session scope (`sess_...`) or a distinct frame
  keyed so it does not collide with the foreground session slots, register it in
  the registry, and drive it from a scheduler that also drives the foreground
  frame. The scheduler steps the foreground; whenever the foreground awaits a
  host call, it steps the background frame; and vice-versa. Because the runner
  currently drives one frame per `stepLoop`, this needs either:
  - a **multi-frame scheduler** in `DMLRunner` that owns all runnable frames, or
  - a **separate `DMLRunner` bound to a separate session id** on the same WASM
    instance (simplest, and avoids the shared `session_*` slots entirely), with
    a host-level scheduler interleaving `stepFrameOnce` calls.
  The separate-session approach is recommended: it reuses the existing machinery,
  keeps session slots isolated, and the module text is copied/loaded per job
  (snapshot isolation). It re-introduces the RAM/`mountWorkspace` global caveat:
  the workspace mount is process-global, so all in-process jobs must share the
  same workspace (they do).
- **L2 goal job**: hand the snapshot to a worker; the worker runs the goal and
  streams events back; cancellation is `terminate`.

### 3.3 Snapshot isolation

A background goal sees the harness **as of submission** (version-pinned), per the
prior plan. In L1 this means capturing `{harnessText, memory}` and loading it into
the job's session/module. Commits made after submission are not visible to the
running job. Document this; it is the price of not sharing a mutable module.

### 3.4 UI element

Pi exposes `ctx.ui.setWidget(key, lines | componentFactory)` and
`ctx.ui.setStatus(key, text)` (`docs/extensions.md`, `docs/rpc-extension-ui.md`;
see `examples/extensions/widget-placement.ts`, `status-line.ts`).

- A widget keyed `dc-jobs` renders one line per active job: state glyph, kind,
  label, elapsed, and a cancel affordance. A **component factory** (not just
  string lines) gives keyboard/mouse interaction for cancel/expand; fall back to
  plain lines in RPC/non-interactive mode.
- `ctx.ui.setStatus("dc-jobs", "⧗ 2 jobs")` gives the compact status-bar form.
- A `/dc-jobs` command prints the full table; `/dc-jobs show <id>` tails the
  buffered log; `/dc-jobs cancel <id>` cancels.
- Because pi itself may be running a turn, the widget is *informational*; the
  authoritative interactive cancel path is the command **and** a model-callable
  tool (below).

### 3.5 Model-facing tools

Two tools, both opt-in like `dc_run`:

- `dc_run` gains a `background: boolean` parameter (or a sibling
  `dc_run_background`) that executes an existing skill as a job and returns the
  job handle. Enabling rules are unchanged; contextual plans still cannot be
  backgrounded because they must be started by the user.
- `dc_jobs` with actions `list | status | tail | cancel | wait` for the model to
  inspect and manage jobs. This is the "cancel tool" the question asks about.
  It must respect the same recursion/isolation rules and never expose jobs from
  another workspace/session.

The agent-facing convenience wrapper `execute_many(goals, {parallel:true})` from
the prior plan is the natural bridge to feature 2.

### 3.6 API summary

| Surface | Addition |
|---|---|
| DML | `execute_background/2`, `exec_background/2`, `job_status/2`, `job_wait/3`, `job_tail/2`, `job_cancel/1` |
| pi tools | `dc_run` `background` option; new `dc_jobs` tool |
| Slash | `/dc-run <skill> --background`, `/dc-jobs [list\|show\|cancel\|wait]` |
| Config | `jobs: { enabled, maxConcurrent, maxBackground, defaultTimeoutMs, logDir? }` |

---

## 4. Feature 2 — `parallel_maplist`

### 4.1 Semantics

```prolog
parallel_maplist(Goal, List).            % Goal = closure over one element
parallel_maplist(Goal, List, Results).   % collect Goal's last arg per item
parallel_maplist(Goal, List, Results, Options).
```

`Options` (a dict or list): `concurrency(N)`, `timeout(Ms)` (per item),
`on_error(fail|throw|collect)`, `preserve_order(true|false)`.

Desired behaviour: results preserve input order by default; per-item failures do
not abort siblings under `on_error(collect)`; the whole call has a bounded
concurrency and a bounded aggregate timeout.

### 4.2 The closure problem

Prolog closures (`call/N`) are not portable across process boundaries. Two
shapes avoid that:

1. **Data + named goal**: `parallel_map(GoalName, List, Results)` where
   `GoalName` is a callable term and the item is applied as one argument, e.g.
   `parallel_map(summarize, [F1,F2], Results)`. Serializes cleanly to a worker.
2. **Template goal with a hole**: `parallel_maplist(G, List, R)` where `G` uses a
   distinguished variable; serialized as the goal term with the item
   substituted at fan-out time. This is just `call(G, Item)` and is what a
   Prolog user expects.

Recommendation: implement (2) in-process and define it as **`call(G, Item)` per
item**, documented as such. For L2 workers, only (1) is guaranteed portable;
document the restriction and reject (or fall back to L1 for) non-portable
closures.

### 4.3 Implementation

`parallel_maplist` is a thin Prolog built-in that:
1. creates N jobs in the registry (one per item, up to `concurrency`),
2. each job runs `call(G, Item)` in its isolated session (L1) or worker (L2),
3. collects results/errors, waits for all, unifies `Results` in input order,
4. fails/throws according to `on_error`.

Because each branch is a job, it inherits isolation, timeout, cancellation, and
usage accounting for free. This is exactly the prior plan's parallel executes +
`execute_many`, expressed as a list primitive.

### 4.4 Rules and gotchas to document

- **Side effects are unordered.** Two branches writing the same file/harness fact
  race. Provide no ordering guarantee; advise "map over data, not over state".
- **Memory writes are per-branch and discarded** (like `execute`'s scratch
  memory), unless a branch explicitly returns a value. There is no shared mutable
  harness.
- **CPU-bound Prolog serializes** at L1 (cooperative). State this beside the
  predicate so authors do not expect speedups from `label/1` fan-out.
- **Nondeterminism**: `parallel_maplist` should be deterministic itself (one
  solution) — wrap collection in `once/1` semantics; a nondeterministic closure
  takes its first solution.
- **Budget**: fan-out counts against the turn's model-call budget and the
  session's job budget; a `concurrency` default (e.g. 4) and a per-provider
  model-call semaphore protect rate limits (prior plan §5.2).

---

## 5. Feature 3 — parallel tool calls in `task/N`

### 5.1 Where to change

- **Injected backend loop** (`runInjectedAgentLoop`, `src/agent.ts`): replace the
  sequential `for (const call of toolCalls)` with a bounded parallel executor.
  This is the path the pi extension uses (`createPiBackend`), so it is the one
  that matters for `dc_run`/`/dc-run`.
- **AI SDK loop** (`runAgentLoop`): make the choice explicit and capped rather
  than relying on the AI SDK's default. Give each `aiTool.execute` a shared
  semaphore so a step cannot fan out unboundedly.

### 5.2 Ordering and result correlation

- Execute calls concurrently, but **append `{role:'tool'}` messages in the
  original call order**, matching each result to its `toolCallId`. This keeps the
  replayed transcript deterministic regardless of completion order.
- Emit `tool_call` events with the call id so the UI can nest out-of-order
  activity (pi already supports parallel sibling tool events).

### 5.3 Control tools must stay ordered

`finish` and `set_result` are special (they mutate shared loop state):
- `set_result` writes `state.variables`. Run it **after** any parallel data
  tools complete, then merge in call order.
- `finish` must be **last**; if present, run remaining tools first, then finish.
- Simplest safe rule: partition a step into `[data tools…]` (parallel) and
  `[control tools…]` (serial, ordered, after the parallel batch). This preserves
  today's semantics exactly for the tools where order matters.

### 5.4 The DML-tool result-slot race

`executeToolIsolated` solves module-state races but posts nested `exec` results
to the single `session_exec_result/2` slot keyed by `this.sessionId`
(`src/runner.ts` `postExecResult`, `executeToolIsolatedInner`). Two DML tools
running concurrently and each calling `exec()` will clobber each other's slot.

Options:
- **A. Per-call correlation.** Key the result slots by a `CallId` (tool-call id /
  frame id) in addition to session, e.g. `session_exec_result(Session, Call, R)`.
  Requires touching the MI and `engine_fetch` path; the cleanest fix but the
  most invasive.
- **B. Distinct session scope per concurrent tool.** Give each concurrent DML
  tool engine a child session id so its slots are private. Moderate change;
  aligns with the separate-session approach in §3.2.
- **C. Serialize DML-defined tools; parallelize only host/TS tools.** Zero
  runtime change; still covers the common case (a model asking for two `bash`
  calls or two `read`s at once).

Recommendation: **C first, then B** (B also unblocks L1 background goals), and A
only if in-process fan-out becomes a core workload. Explicitly exclude
`pi_agent_step` from parallel execution: it toggles pi's global active-tool set
and has a single `pendingAgentStep` slot (`deepclause-pi/src/index.ts`).

### 5.5 Safety and configuration

- `maxToolConcurrency` (default **1**, to preserve today's behaviour; per-task
  opt-in or global default of 2–4).
- Per-tool classification: `readOnlyHint`/`destructiveHint` (pi annotations) or a
  runtime allowlist decides what may run concurrently. Never parallelize two
  destructive tools that touch the same resource; use `withFileMutationQueue()`
  on the pi side for file-mutating tools (pi docs).
- Cancellation: `Promise.allSettled` + the task's `AbortSignal`; an aborted task
  cancels in-flight tools.
- Approvals: a parallel batch may need several confirmations. Serialize the
  prompt queue; do not present concurrent dialogs (pi's `ctx.ui` dialog is a
  single interactive channel — the same one-slot hazard the extension's
  `claimExecution` comment describes).

---

## 6. Cross-cutting concerns

### 6.1 Events and UI mapping

Job events must map onto the existing `DMLEvent` union and the pi renderer:
`task_activity` → progress, `stream` → live text, `tool_call` → nested activity,
`input_required` → pi input prompt, `usage` → usage metadata, `answer` → result,
`error` → concise error. Background job events are **buffered, not interleaved**
into the foreground transcript; they surface via the `dc-jobs` widget, the
`/dc-jobs show` view, or `job_tail`. Add a `jobId` field to `DMLEvent` (alongside
the existing `frameId`) so the renderer can attribute and group.

### 6.2 Configuration (proposed)

```jsonc
// .pi/deepclause/config.json  (pi extension)  / CreateOptions (SDK)
"execution": {
  "mode": "inline",            // inline (L1) | worker (L2)
  "maxToolConcurrency": 1,     // task/N parallel tool calls
  "jobs": {
    "enabled": true,
    "maxConcurrent": 4,
    "maxBackground": 8,
    "defaultTimeoutMs": 300000,
    "logDir": null             // null => OS tmpdir
  },
  "parallel": {
    "defaultConcurrency": 4,
    "providerConcurrency": 4   // per-provider model-call semaphore
  }
}
```

### 6.3 Relationship to the worker plan

This document is a refinement and a first slice of
`DML_REPL_WORKER_EXECUTION.md`:
- §2 job registry = that plan's §5 job model/scheduler.
- §3.2 L1/L2 = `LocalRuntimeHost`/`WorkerRuntimeHost` seam.
- §4 parallel_maplist = that plan's parallel executes + `execute_many`.
- §6.1 buffered events / widget = that plan's §5.4 background UX.
Doing the shell-job half and the `task/N` ordering work now delivers value
without waiting for the worker spike.

---

## 7. Recommended phasing

1. **Job registry + shell backgrounding** (`exec_background`, tmp log,
   `job_status/wait/tail/cancel`). L0 only. No Prolog-engine change.
   *Delivers the lowest-risk half of feature 1.*
2. **`task/N` parallel tool calls, host/TS tools only, `maxToolConcurrency=1`
   default, ordered result assembly, control tools serialized.** *Feature 3,
   option C.*
3. **pi UI: `dc-jobs` widget/status, `/dc-jobs`, `dc_jobs` tool, `dc_run
   --background`.** *Completes feature 1's UX and the cancel tool.*
4. **Concurrency seam + separate-session goal jobs (L1)** and
   `parallel_maplist/3` on top of the registry. *Feature 2.*
5. **Worker host spike** (`swipl-wasm` under `worker_threads`), then L2 for
   true preemption/parallelism; per-call result-slot correlation (5.4 option B).
6. Optional: DML `spawn/await/gather`, per-job model overrides, AgentVM-per-job.

## 8. Open questions / spikes

1. **`swipl-wasm` in `worker_threads`** — does it load without DOM/`importScripts`
   issues, and does a per-worker `mountWorkspace` work? *Gates L2. Prototype
   first.*
2. **L1 scheduler** — can two `DMLRunner` sessions on one WASM instance be
   interleaved safely through `stepFrameOnce`, or does any MI global
   (`current_memory`, signal slots) leak across sessions? Audit the
   `session_*` predicates for anything not session-keyed.
3. **Result-slot correlation** — is option B (child session per concurrent
   tool) or A (CallId-keyed slots) less invasive in the MI?
4. **CPU-bound background** — do we document L1's non-preemption loudly, or
   require L2 whenever a job may run Prolog computation?
5. **Closure portability** for `parallel_maplist` at L2.
6. **Approval UX** for a fan-out that needs many confirmations.
7. **Log retention/cleanup** policy and whether any job artifacts should survive
   session shutdown.
8. **Usage/cost attribution** for fan-out (per job and per branch), and the
   default per-provider semaphore size.
