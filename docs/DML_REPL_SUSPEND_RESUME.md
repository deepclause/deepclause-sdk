# Suspending and resuming a DML session

A turn can **stop at a safe point** and write down the goal that continues it, so
`--session` picks the program up where it left off instead of dropping the user
into a half-finished state.

Status: **cooperative suspension is implemented**; the rest of this document is
design, marked per section. What exists, what is designed, and what needs a
different mechanism:

| Capability | State | Where |
|---|---|---|
| `suspend/1` written by the program, resumed by `--session` | **shipped** | §1–§5 |
| never suspending while holding a stream or form | **shipped** | §3 |
| the turn's memory in the record and in `harness.json` | **shipped** (was broken, see §4) | §4 |
| *host*-triggered suspension (`/suspend`, SIGINT) at the program's next checkpoint | **designed** — needs atomic record writes first | §8 |
| suspending across a `task/N`/`chat/N` turn boundary | **shipped for `chat`** (a `chat` is already one turn of a DML loop); **needs a primitive for `task`** | §9 |
| suspending *inside* a `task/N` or mid-goal | **needs the CPS machine** | §7, §9 |
| `kill -9` / power loss | **up to the last *persisted* checkpoint** — needs eager checkpoints + atomic writes | §10 |


Related: [`DML_REPL_SESSION_HISTORY.md`](./DML_REPL_SESSION_HISTORY.md) §10
(resume of a session's harness and memory), [`SUBAGENTS.md`](./SUBAGENTS.md)
(processes), [`DML_FORMS.md`](./DML_FORMS.md) (forms).

## 1. What a suspension is

```prolog
loop(N) :-
    (   N >= 2
    ->  suspend(loop(N))          % stop here; resume with loop(N)
    ;   M is N + 1, loop(M)
    ).
```

`suspend(+Continuation)` stops the turn and hands the host:

- the **continuation goal** (`loop(2)`), which is what a resume runs;
- the **turn memory** at the stop, so the resumed turn sees the same conversation;
- the **module version + hash** it was running.

The turn then ends exactly like `answer/1` does — a clean stop, no error, no
"no solution". The host writes `sessions/<id>/suspend.json` and, on a later
`--session`, runs the continuation once the harness and memory are restored.

The **program** supplies the continuation. That is the whole contract, and it is
why this works without touching the interpreter: the state you need to resume is
the state you already wrote as the goal's argument — the same discipline the
runtime asks for elsewhere ("a service's state lives in its loop argument").

## 2. A turn suspends only between host calls

Every host interaction (`task/N`, `exec/2`, `await_form/2`, `input/2`,
`receive/2`) already hands control to the host and waits for a signal. `suspend/1`
is one of those leaves, so by construction it runs **between** host calls: no
model call, shell command or form wait is ever cut in half. The host half of a
turn is therefore either finished or not started, and the snapshot only has to
carry Prolog state.

## 3. The guard: never suspend while holding something

A snapshot cannot carry an open C stream, and a resumed session must never inherit
a half-written file. So two things are tracked in a per-session ledger and
`suspend/1` **refuses** while either is held:

| Held | Acquired by | Released by |
|---|---|---|
| `file(Path, Stream)` | `open/3`, `open/4` | `close/1` |
| `form(FormId)` | `form/2` | `close_form/1` |

```prolog
probe :-
    open("/workspace/report.csv", write, S),
    write(S, "partial"),          % ← a suspend here is refused
    close(S),
    suspend(probe).               % ← safe: nothing is held
```

The refusal is an ordinary Prolog error, so the harness can react:

```
error: cannot suspend while holding file("/workspace/report.csv", …): close it first
```

Two consequences worth knowing:

- **A suspend request is a *point*, not an interrupt.** There is no host-initiated
  "suspend now"; the program decides where it is safe. That is deliberate: an
  interrupt would have to cut a goal in half, which is exactly what §7 is about.
- **A turn that was aborted while holding a stream leaves the handle open.** Each
  run starts by closing whatever the previous turn left open and clearing the
  ledger (`release_stale_resources/1`), so one abandoned turn cannot block every
  later suspension.

## 4. What is written, and when

`sessions/<id>/suspend.json` (atomic write, next to `harness.json`):

```json
{
  "schemaVersion": 1,
  "sessionId": "repl_m9x1_1",
  "at": "2026-10-19T12:30:00.788Z",
  "continuation": "loop(2)",
  "memory": "[message{role: user, content: …}]",
  "turn": 7,
  "moduleVersion": "12",
  "moduleHash": "…"
}
```

Alongside it, `harness.json` is rewritten with the turn's memory (so the resumed
session starts from exactly this module *and* conversation), and the turn is
recorded in `turns.jsonl` with `outcome: "suspended"`.

Resume consumes it: the record is cleared *before* the continuation runs, so a
program that suspends again writes a fresh one rather than resuming a stale
position.

## 5. Resume

- `deepclause repl --session <id>` restores the harness and memory (as it always
  has) and then runs the continuation, logging
  `resuming the suspended continuation: loop(2)`.
- An explicit `-g goal` does **not** auto-resume: the user asked for that goal.
  Run it with `/goal`/`-g` yourself if that is what you want.
- The SDK: `session.pendingSuspension()` → `{ continuation, memory, at }`, then
  run it with `query(continuation)` and `session.clearSuspension()`.

If the harness changed since the suspension, the continuation runs against the
*current* module — the recorded version is there so a host can warn or refuse. A
resume never re-runs a completed host call: `suspend/1` sits between leaves, so
the tool result or model answer it already produced is final.

## 6. What is *not* carried

| Not carried | Why | What happens |
|---|---|---|
| an open stream | not serializable | `suspend/1` refuses |
| an open form | host state | `suspend/1` refuses until `close_form/1` |
| a `task/N` or `exec/2` in flight | the host owns it | cannot happen: a suspension is between leaves |
| subagent mailboxes / mid-flight service state | process state, not a term | `services.jsonl` records each `serve` declaration (name, goal, code version) — see §8 |
| model, credentials, tool policy | must not be persisted | rebuilt from config on resume, which is what makes a snapshot portable |

## 7. Mid-execution suspension (designed, not built)

The interpreter's control layer is **five clauses**: conjunction, if-then-else,
soft-cut, disjunction and cut (`mi_call/3`). Everything else is a *leaf* — a host
call or a builtin that returns — so the continuation between two leaves is the
only thing Prolog keeps on its native stack:

```prolog
mi_call((A, B),       S0, S) :- mi_call(A, S0, S1), mi_call(B, S1, S).    % 5583
mi_call((C -> T ; E), S0, S) :- …                                          % 5590
mi_call((C -> T),     S0, S) :- …                                          % 5599
mi_call((A ; B),      S0, S) :- ( mi_call(A, S0, S) ; mi_call(B, S0, S) ). % 5607
mi_call(!,            S,  S) :- !.                                         % 5618
```

Defunctionalising exactly those gives a machine whose continuation is **data**:

```prolog
%% cont = [g(Goal)|Rest] | [alt(ContA, ContB)|Rest] | [barrier(Tag)|Rest]
run([],            State, State).
run([g((A,B))|C],  S0, S) :- run([g(A), g(B)|C], S0, S).
run([g((A;B))|C],  S0, S) :- run([alt([g(B)|C])|C], S0, S) ; run([g(B)|C], S0, S).
run([g(!)|C],      S0, S) :- prune_to_barrier(C, C1), run(C1, S0, S).
run([g(G)|C],      S0, S) :- mi_call(G, S0, S1), run(C, S1, S).
```

Then a suspension is `cont{ goals, env, state }` — `copy_term` + `numbervars` +
`term_string`, the same trick the exporter and `spawn` already use — and resume
rebuilds it. Nice side effects: resumable backtracking, per-goal gas/time
accounting, and `answer/1` becoming an ordinary frame instead of a throw.

Two costs, both real:

- **The variable environment.** The remaining goals close over bindings, so a
  suspension must capture `env` (a `$VAR(N)=Value` list) and re-unify it.
- **Choicepoints and cut.** `(A ; B)` and `member/2` leave alternatives in the
  native stack. They become `alt` nodes (so, serializable — and resumable), and
  `!` needs a barrier tag that prunes only to its own frame. v1 of that work
  should instead **refuse** to suspend when the continuation holds unvisited
  alternatives ("not at a safe point"), which costs nothing for deterministic DML.

This is the most load-bearing code in the engine (`context_stack`, `catch/3`,
`consume_gas`, `$answer_commit`, every engine-yield special), so it belongs behind
a flag with the native path preserved, and with the current suite as the net.

An exact alternative — snapshotting the whole SWI-WASM instance inside
`agentvm` — is much cheaper to build, but pins the runtime and still cannot
capture the host half (model calls, forms).

## 8. Host-triggered suspension (designed, not built)

Today the *program* decides where it stops. The other direction — the host says
"suspend now" (a `/suspend` command, SIGINT, a pool reaper) — needs two pieces,
and only one of them is missing.

**The trigger point already exists.** Every goal that is not control structure
runs through the `mi_call/3` catch-all, which is the atomic-step boundary (control
structure recurses through `mi_call`; host leaves have their own clauses and skip
it). A flag checked there fires at the next step that is not inside a host call.

**The continuation is the missing half.** At that point the rest of the body is on
the native Prolog stack, so the cheap, correct mechanism is a *program-supplied
anchor*:

```prolog
checkpoint(+Continuation)          % records where to resume, and persists it
    ...
    suspend(Continuation)          % the host-flag case takes this at the next step
```

with the machine checking, at each step:

```prolog
    host_suspend_requested(SessionId),
    \+ session_resource(SessionId, _),          % the §3 guard defers it
    session_checkpoint(SessionId, Depth, Cont) % scoped to this frame
```

What that buys: the host decides *when*, the program decides *where*, at most one
iteration away; the ledger defers a request that arrives mid-file; and the
guarantee is **at-least-once since the checkpoint**, so the rule is *checkpoint
after the effect* (`task(…)` then `checkpoint(agent(Memory))`, never the reverse).
Scope the checkpoint by frame depth, or an `execute` child's checkpoint will
hijack the parent's suspension.

*Prerequisite:* `HistoryStore.writeJson` is `fs.writeFile`, not temp-file +
`rename`. A crash mid-write truncates `suspend.json`/`harness.json`, which is the
one failure mode that turns "resume" into "lose the session".

## 9. Agent loops inside `task/N` and `chat/N` (partly designed)

An agent loop's own state is small and already state-shaped: `messages` (system,
user/assistant, tool results), the tool scope, `variables`, `outputs`, usage.

- **`chat/N` needs nothing.** `loop :- input(A), chat(A), loop` runs one `chat/N`
  per message, so a suspension between turns loses nothing: the conversation *is*
  `state.memory`, which the record already carries (§4).
- **Between `task/N` iterations** is the only seam (a model call is a leaf, so it
  can never be interrupted). Reaching it needs a primitive whose `State` *is* the
  inner history, because a `task/N` is a single goal and its continuation is the
  rest of the body:

  ```prolog
  agent_loop(State) :-
      agent_step(State, Next),              % one model call + its tool calls
      (   Next == done(Reply) -> answer(Reply)
      ;   checkpoint(agent_loop(Next)),     % ← the suspension anchor
          agent_loop(Next)
      ).
  ```

  with `runAgentLoop` gaining a `resumeMessages` entry (today it rebuilds
  `[system, …history, {user: "Subtask: …"}]`). Then a suspension saves *only the
  inner messages* — which is what §8's checkpoint mechanism needs, and much
  smaller than a Prolog frame.
- **Inside a `task/N`** — mid-thinking, mid-tool — remains out of reach without
  the CPS machine (§7): the DML continuation is the rest of the body.

## 10. Sudden interruptions (designed, not built)

| Interruption | Reachable? | How |
|---|---|---|
| SIGINT / SIGTERM | yes, fully | set the §8 flag, take the suspension at the next checkpoint (bounded grace, then abort as today); a second Ctrl-C kills |
| `kill -9`, power loss | up to the **last persisted checkpoint** | nothing in-process runs, so `checkpoint/1` must write its record when taken (one small atomic file write), not at suspend time |
| an L2 worker crash | yes | the main host survives, the dead worker's mailbox waiter is dropped (already fixed), the process shows `crashed`; the same checkpoint resume applies |

Already in place and helping: the transcript is written through every 250 ms, and
the turn record is written at *start* with `outcome: "running"`, so an interrupted
turn is detectable. Still needed: **atomic record writes** (see §8) and a
**heartbeat + lock**, so a cron waker and a human cannot resume the same agent
twice and a stale lock is taken over safely.

The honest guarantee is **at-least-once**: work between the last persisted
checkpoint and the crash is repeated on resume, never skipped. Exactly-once would
require the effect and the checkpoint to be one transaction, which a Prolog goal
and a file write are not.

## 11. Follow-ups

- **Re-spawn services on resume.** `services.jsonl` already records each `serve`
  declaration (name, goal, priority, code version); `recordedProcesses()` reads it
  and nothing consumes it. A `repl.resume.services: none | list | restart` switch
  would make a resumed session whole again — `restart` is exact only for services
  that keep their state in the goal argument.
- **`hibernate/1`** for processes: a process may suspend returning its
  continuation goal, which is what makes a service's suspension exact.
- **Host-initiated suspend** (§8), which starts with atomic record writes.
- **Resume a turn interrupted by a crash** (the `running` outcome in
  `turns.jsonl`): replay to the last recorded tool result and continue, never
  re-running a leaf that already has a result.
- **`--session` clone**: a snapshot you can fork without consuming it.
