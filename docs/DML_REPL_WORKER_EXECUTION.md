# Out-of-process execution: workers, parallel executes, background jobs

Status: **plan** (Tier 2 of the REPL responsiveness work). Companion to the
ad-hoc Tier 1 items (cooperative yields, per-event throttling, incremental
transcript rendering).

Today the Prolog runtime (`swipl-wasm`) and the runner execute **synchronously
on the Node main thread**, so one heavy `execute`/goal/`exec` freezes the whole
UI (no redraw, no spinner, no Ctrl-C). This plan moves execution out of the UI
thread, adds true cancellation, and unlocks parallelism and background work.

---

## 1. Use cases

These are the concrete things we want to be able to do; the requirements and the
plan below are derived from them.

### 1.1 Responsiveness (the original bug)
- A slow `execute` (CLP `label`, deep recursion, big `findall`, string work) or a
  sandboxed command must **not** freeze the TUI. The spinner, keystrokes, and
  Ctrl-C must keep working.

### 1.2 Hard timeouts / watchdog
- Any goal can be **killed after a budget** (e.g. 5 s) with a clear error,
  regardless of what the author wrote. This is the backstop against the
  unbounded-loop class we already saw (46k solutions / 110 s turns).

### 1.3 Agent authoring loop: parallel tests
- The authoring agent writes a skill, then runs **several test goals in parallel**
  against the same (scratch) harness and gets a combined pass/fail report. This is
  the single most valuable speed-up for the agent's edit→test→commit loop.

### 1.4 Harness test suites
- A module with `test_<name>/N` predicates runs its whole suite in parallel, and
  re-runs the suite after every edit (regression check).

### 1.5 Batch / map over data
- Process N files/rows/URLs by fanning out one job per item and collecting
  results. Currently this is a sequential loop.

### 1.6 Ensembles / best-of-N
- Run the same prompt/goal **N times in parallel** and choose with a judge
  (self-consistency, majority vote, best-of-N). Very natural for LLM harnesses.

### 1.7 Branch exploration
- Explore several plan branches / search directions concurrently and keep the
  best — the agentic analogue of CLP search.

### 1.8 Long background jobs
- Kick off a slow report, eval, or benchmark as a **background job** and keep
  using the REPL; collect the result later.

### 1.9 Model comparison / harness A-B tests
- Run the same harness against **different models/providers in parallel** and
  compare outputs and cost. Requires per-job model overrides and per-job usage.

### 1.10 Sandboxed isolation per job
- Each `--sandbox` job gets its own AgentVM, so jobs can't interfere and killing a
  job kills its VM.

### 1.11 Recursive (RLM) fan-out from DML
- A harness can `spawn(Goal, JobId)` sub-goals and `await/2` them — parallel
  delegation instead of a sequential fold.

### 1.12 Evaluation harness (RSI)
- Run a candidate harness over a task set **in parallel**, score, and keep the
  best. Parallel executes + per-job model overrides + background jobs are exactly
  the substrate for the bounded self-improvement loop.

### 1.13 pi extension
- `/dc-run` and `/dc-plan` long skills / parallel `/dc-run`s should not block
  pi's UI.

---

## 2. Requirements derived from the use cases

| Req | From | Notes |
|-----|------|-------|
| UI thread never runs Prolog/AgentVM | 1.1 | core |
| True preemption (`terminate`) | 1.1, 1.2 | only reliable interrupt |
| Per-job timeout | 1.2 | default + per-call |
| Snapshot isolation per job | 1.3, 1.4, 1.5, 1.6 | many jobs on one harness |
| **Parallel executes on the same harness** | 1.3–1.7 | snapshot per job; no shared mutable state |
| Collect/aggregate results | 1.4, 1.5, 1.6, 1.7 | `gather`-style primitive |
| Bounded concurrency + queue | all | + provider rate limits |
| Per-job overrides (model, tools, sandbox, timeout) | 1.6, 1.9, 1.10 | |
| Background submit + poll/cancel | 1.8, 1.12 | job registry |
| Per-job usage accounting | 1.6, 1.9, 1.12 | tokens/cost rolled up |
| Harness version pinning | 1.12, 1.9 | a job sees the harness "as of submit" |
| DML `spawn`/`await` | 1.11 | optional later phase |
| Job artifacts on disk | 1.8, 1.12 | inspect/resume |

---

## 3. Architecture

### 3.1 Topology

```
main thread (UI: TUI, renderer, status, history, input, approvals, credentials)
│
├─ Primary worker  ── owns the living module: begin/define/assert/retract/end_turn,
│                     listing, export, snapshot. (mutations serialized here)
│
├─ Job worker 1 ── execute/goal against a harness snapshot
├─ Job worker 2 ── execute/goal against a harness snapshot     (pool)
└─ Job worker N ── …
        each: own swipl-wasm instance + DMLRunner + session module
```

`swipl-wasm` is a process/worker-wide singleton and cannot be shared, so **one
instance per worker**. Job workers are disposable; `terminate()` is the cancel.

### 3.2 The seam: `RuntimeHost`
Abstract the current `DMLSession ↔ DMLRunner ↔ SWIPLModule` coupling behind a
`RuntimeHost`:
- `LocalRuntimeHost` — today's in-process behavior. **Default for the SDK and all
  unit tests** (no behavior change).
- `WorkerRuntimeHost` — message-passing to a worker. **CLI/TUI opt-in.**

Everything below is behind `WorkerRuntimeHost`, so the ~500 tests keep passing
and the embedder is unaffected until it opts in.

### 3.3 Worker protocol

**main → worker**
- `init { workspacePath, sessionId, harnessText, memory, backend, tools, limits }`
- `load { code }` · `beginTurn` · `define` · `assert` · `assertMany` · `retract` ·
  `delete` · `endTurn` · `rollback`
- `run { jobId, kind:'goal'|'prompt', goal?, args?, overrides?, signal? }`
- `cancel { jobId }`
- `snapshotModule {}` → `{ harnessText, memory, head }`
- `dispose {}`

**worker → main**
- `ready {}` · `event { jobId, event }`
- `need { requestId, jobId, kind:'model'|'tool'|'input'|'judgment', payload }`
- `chunk { requestId, kind, chunk }` (streamed model text / tool stdout)
- `usage { jobId, usage }`
- `result { jobId, result }` · `error { jobId, error }`

**main → worker (responses)**: `resolve { requestId, value | error }`,
`resolveDone { requestId }`; plus `flowPause`/`flowResume` backpressure.

### 3.4 Capability proxy (keep side effects on the main thread)
The worker's `DMLRunner` gets **proxy** implementations that RPC back:
- `LLMBackend` proxy → `need(model)`; streamed tokens via `chunk`.
- host tools (`exec`, `image`, `save_media`, …) → `need(tool)`; stdout via `chunk`.
- `input/2` / `ask_user` → `need(input)`.
- judgments → `need(judgment)`.

So the worker runs **only Prolog + orchestration**; credentials, workspace
policy, approvals, and UI stay centralized. One local round-trip per model/tool
call is negligible next to real I/O.

---

## 4. Parallelism levels

Three levels, shipped incrementally:

1. **Parallel executes** (`execute`/goals) on the same harness snapshot — use
   cases 1.3–1.5, 1.7. The core of Tier 2.
2. **Parallel goals/entry points** (`/run`, `agent_main`, prompts) — 1.6, 1.8,
   1.9. Same machinery, `kind:'prompt'`.
3. **DML fan-out** (`spawn`/`await`/`gather`) — 1.11, 1.12. Optional later phase.

### 4.1 Snapshot isolation (how "same harness, parallel" is safe)
The **primary worker** owns the living module (including uncommitted transaction
edits). Every `execute` first calls `snapshotModule` → `{harnessText, memory,
head}`; the main thread hands that to a pooled **job worker**, which loads it and
runs the goal. Consequences:
- each `execute` is fully isolated (own module, own memory, own scratch);
- N parallel executes share nothing mutable → **no locking**;
- the snapshot includes uncommitted edits, so `begin_turn → define → execute →
  end_turn` still works;
- mutations remain serialized on the primary worker (no write/write races);
- a background job sees the harness **as of submission** (version-pinned).

---

## 5. Job model, scheduler, background tasks

### 5.1 Job record (main thread)
`{ id, kind, goal/entry, overrides, state: queued|running|done|failed|cancelled|timeout,
workerId, version, submittedAt, startedAt, durationMs, usage, result|summary, error }`

### 5.2 Scheduler
- Worker pool (default `min(2, cpus-1)`, configurable) of warm job workers.
- FIFO queue with two priorities: **foreground** (agent waiting) > **background**.
- `maxConcurrent` jobs; the rest queue.
- `jobTimeoutMs` (default e.g. 300 s) → `cancel` → `terminate`.
- **Provider rate limiting**: a per-provider semaphore for model calls across all
  jobs, so parallel jobs don't trip API limits.

### 5.3 Background submission
- Agent tool: `execute(goal, { background: true })` → `{ jobId }` immediately.
- REPL: `/run-bg <goal>` (and `/run <entry> --background`).
- DML: `spawn(Goal, JobId)` (phase 6).

### 5.4 Background UX
- Status bar: `⧗ 2 jobs`.
- `/jobs` — table (id, kind, state, duration, usage).
- `/jobs show <id>` — buffered output/result.
- `/jobs cancel <id>` · `/jobs wait <id>`.
- Background events are **buffered** (not interleaved into the foreground
  transcript) and surfaced on demand / as a collapsed one-line progress row.
- Job artifacts under `.deepclause/repl/jobs/<id>/` (`events.jsonl`, `result.json`).

### 5.5 Agent-facing API
- `execute_many(goals[], { parallel: true })` → combined report (per-goal
  pass/fail + output). Directly serves the authoring loop (1.3) and test suites
  (1.4) without the agent juggling N tool calls.

---

## 6. Cancellation & timeouts
- `cancel(jobId)` → `worker.terminate()` + mark cancelled + respawn from the pool.
  This is the only reliable interrupt with synchronous WASM.
- A foreground `execute` runs in a **job** worker, so cancelling it never touches
  the primary session — the living module and its transaction stay safe.
- Ctrl-C → `cancel(activeJobId)`; second Ctrl-C exits. A timeout is the same path
  with an error instead of "cancelled".
- The existing temp-`.dml` save on Ctrl-C remains the backstop for a hung
  *primary* turn.

---

## 7. AgentVM / sandbox in the worker (F)
- Run `--sandbox` execution in the **job worker**, so sandboxed commands can't
  block the UI and cancelling the job kills its VM.
- Shell/image capabilities still route through the proxy (policy/approvals
  centralized); only the VM process moves.

---

## 8. Flow control & event volume (overlaps Tier 1)
- Worker→main events are unbounded by nature; add `flowPause`/`flowResume`
  backpressure when the main queue exceeds a threshold.
- Coalesce `solution`/`stream`/`log` bursts per job into ≤1 renderable per frame.
- History: sample high-volume types (record a count), drop base64 image bodies
  (keep paths). History stays on the main thread.

---

## 9. Persistence & state
- Main thread owns all files: `harness.dml`, versions, `changes.jsonl`,
  `history.dml`, `events.jsonl`, `jobs/<id>/*`.
- Workers are pure compute: load snapshot → run → emit events/result. No FS
  writes except through proxied tools.
- On commit the primary returns the new module text; the main thread persists,
  versions, and records history.

---

## 10. Configuration

```json
"repl": {
  "execution": {
    "mode": "worker",            // "inline" | "worker"  (SDK/tests default: inline)
    "workers": 2,                // job-worker pool size
    "maxBackgroundJobs": 8,
    "jobTimeoutMs": 300000,
    "stepInferenceLimit": 200000,
    "providerConcurrency": 4     // per-provider model-call semaphore
  }
}
```

CLI: `--execution inline|worker` (debug/escape hatch).

---

## 11. Migration & compatibility
- Phase 1 lands the `RuntimeHost` seam with `LocalRuntimeHost` only — no behavior
  change, all tests green.
- The CLI switches to `WorkerRuntimeHost` behind config; tests/SDK stay inline.
- Existing `execute`/goal/`HarnessRepl` signatures are unchanged; only the
  transport changes underneath.
- The pi extension can opt in later (`createHarnessRepl({ execution: 'worker' })`).

---

## 12. Phasing (ordered by use-case value)

1. **Seam** — `RuntimeHost` + `LocalRuntimeHost`; `DMLSession` refactor. No
   behavior change. *(unblocks everything; keeps tests green)*
2. **Worker host + timeout cancel** — worker entrypoint (`loadProlog` + runner);
   protocol; capability proxy (model/tool/input); primary owns the module.
   **Delivers 1.1 and 1.2** (UI never freezes; hard timeout kills runaways).
3. **Snapshot pool + parallel executes** — `snapshotModule`, job workers,
   `execute_many`. **Delivers 1.3, 1.4, 1.5, 1.7.**
4. **Per-job overrides + usage** — model/tools/sandbox/timeout per job; per-job
   usage roll-up and rate limiting. **Delivers 1.6, 1.9.**
5. **Background jobs** — registry, `background:true`, `/jobs*`, status bar, job
   artifacts. **Delivers 1.8, 1.12 (basis).**
6. **AgentVM in job workers (F)** — **Delivers 1.10.**
7. **DML `spawn`/`await`/`gather`** and optional scheduling — **Delivers 1.11,
   completes 1.12.**

---

## 13. Risks / open questions
- **swipl-wasm under `worker_threads`** — verify the Emscripten build loads in a
  worker (no DOM deps) and the workspace FS mount works there. *Prototype first
  (Phase 2 spike).*
- **Memory** — each worker is a full swipl instance; keep the pool small (2–4)
  and measure RSS per worker.
- **Snapshot cost** — serializing module+memory per `execute`; cache the
  primary's snapshot and invalidate on mutation.
- **Proxy re-entrancy** — nested `task → tool → model` needs correlated
  `requestId`s; partial cancellation must settle all outstanding requests.
- **Determinism** — parallel jobs are version-pinned; document that later commits
  aren't visible to a running job.
- **Debuggability** — stack traces span workers; keep `--execution inline` and
  per-job logs.
- **SDK embedder default** — inline for now; opt-in worker later.
- **Provider limits** — parallel model calls need the per-provider semaphore from
  day one of Phase 4.
