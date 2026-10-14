# Implementation plan — Subagents and session-scoped processes in the REPL

Status: **in progress**. Branch: `feat/repl-subagents` (from `main` after
`feat/harness-side-view`).

Normative spec: [`DML_REPL_PROCESSES_SPEC.md`](./DML_REPL_PROCESSES_SPEC.md).
UX: [`DML_REPL_SERVICES_UX.md`](./DML_REPL_SERVICES_UX.md).
Simple example: [`DML_REPL_SUBAGENTS.md`](./DML_REPL_SUBAGENTS.md).

The spec's §6.2 lists the invariants that make `--execution worker` a mode switch.
Every phase below preserves them.

---

## Status

| Phase | State |
|---|---|
| 1. Process core + seam | ✅ `src/repl/processes.ts`, 18 tests |
| 2. DML surface + executor | ✅ MI primitives, runner `request_proc`, `process-executor.ts`, e2e tests |
| 3. REPL integration | ✅ harness tools, `/spawn` `/ps` `/send` `/call` `/stop`, event sink, status segment |
| 4. Libraries | ✅ `serve/2`, `service_call/3,4`, `service_cast/2` built-ins (`library(swarm)` deferred) |
| 5. Persistence | ✅ `services.jsonl` declarations/exits; resume replays declarations |
| 4. Libraries | ✅ `serve/2`, `service_call/3,4`, `service_cast/2`, `swarm/3,4`, `pmap/3`, `receive_match/3`, `receive/1,2` |
| 5. Persistence | ✅ `services.jsonl` declarations/exits; resume replays declarations |
| 6. L2 seam | ✅ `--execution worker` is real: a worker thread runs the process host with its own `swipl-wasm`, capabilities are proxied to the main thread, and the panel reads a synchronous cache |

### Remaining

- **Non-modal background input presentation.** A subagent's `input/2` is served
  through the shared, serialized human channel and reuses the editor; there is
  no separate queued/notification surface beyond the panel's `needs input`
  marker.
- **Enforced `swarm` concurrency.** `swarm/4` accepts a concurrency argument but
  does not yet throttle spawns (the host's process limit still bounds it).
- **Cross-node links/monitors.** Links and monitors work within a backend; a
  process on one node cannot link to a process on another. The pid carries the
  node, so the routing is a later addition.
- **Real-worker test coverage.** The worker cannot load a TypeScript entry under
  vitest, so `worker-host.ts` is unit-tested against a scripted fake worker and
  the real worker is verified end-to-end against `dist` (rebuild + manual run).

## Phase 1 — Process core

The host bookkeeping and the L1↔L2 seam, independent of Prolog. Fully unit-tested.

| Task | File | Status |
|---|---|---|
| Pid, ProcessInfo, SpawnOptions, ProcessContext types | `src/repl/processes.ts` | ✅ |
| `ProcessHost` + `CapabilityBroker` interfaces | `src/repl/processes.ts` | ✅ |
| `LocalProcessHost`: process table, mailbox, registry, links, monitors, limits | `src/repl/processes.ts` | ✅ |
| value-copy helper for messages/goals (L2 discipline) | `src/repl/processes.ts` | ✅ |
| lifecycle events for the service sink | `src/repl/processes.ts` | ✅ |
| unit tests | `tests/repl-processes.test.ts` | ✅ |

Deferred to a later phase (documented in the module):

- priority-aware *stepping* (the DML scheduler drives frames; the host here owns
  lifecycle and messaging);
- `trap_exit` mailbox delivery (spec §4.1) — the host propagates abnormal exits to
  links as stops for now; the DML layer will deliver `{'EXIT',…}` to trappers;
- worker-backed host (`WorkerProcessHost`) — same interface, RPC bodies.

## Phase 2 — DML surface over the core

| Task | Where |
|---|---|
| MI `mi_call` clauses: `spawn/2`, `send/2`, `receive/1,2`, `receive_match/3`, `self/1`, `exit/1,2`, `link/1`, `monitor/2`, `process_info/2`, `register/2`, `whereis/2`, `make_ref/1`, `now_ms/1`, `sleep/1` | `src/prolog-src/deepclause_mi.pl` |
| dynamic slots + `is_mi_special_predicate/1` entries | `src/prolog-src/deepclause_mi.pl` |
| runner step-status handling (`request_spawn`, `request_send`, `request_receive`, …) via host callbacks in `InternalRunOptions` | `src/runner.ts` |
| `DmlProcessExecutor`: goal → a `ProcessBody` driving a per-process `DMLRunner` frame | `src/repl/process-executor.ts` |
| per-process session snapshot (`Invariant C`) | reuse the MI session namespacing used by tool engines |

## Phase 3 — REPL integration

| Task | Where |
|---|---|
| `ProcessHost` bound to `DMLSession`; created on open, disposed on `dispose()` | `src/repl/session.ts` |
| `spawn` harness tool (+ `ps`, `send`, `call`, `stop`, `tail`, `serve` alias) | `src/repl/harness-tools.ts` |
| service sink in the renderer; deferred writes | `src/repl/tui/renderer.ts` |
| status segment | `src/repl/tui/status-bar.ts`, `src/repl/status.ts` |
| services panel component | `src/repl/tui/services-panel.ts` |
| commands `/spawn`, `/ps`, `/send`, `/call`, `/stop`, `/tail`, `/focus` | `src/cli/repl.ts` |
| input queue for process `input/2` requests | `src/cli/repl.ts` |
| Ctrl-C policy | `src/cli/repl.ts` |

## Phase 4 — Libraries & guidance

`library(service)`, `library(swarm)`, `library(supervisor)`; authoring prompt
section; the three-shape example as a fixture.

## Phase 5 — Persistence

`services.jsonl` declarations/lifecycle; resume replays declarations; fork.

## Phase 6 — L2 seam

`WorkerProcessHost` + `RpcBroker` + worker protocol; `--execution inline|worker`;
conformance suite runs against both (the migration gate).

---

## Invariants checklist (must hold after every phase)

- [ ] pids are node-qualified (`node: 'main'` in v1)
- [ ] all process ops go through `ProcessHost`
- [ ] all side effects go through `CapabilityBroker`
- [ ] goals and messages are copied (value semantics)
- [ ] `process_info`/events are plain data
- [ ] the host has no TUI dependency
