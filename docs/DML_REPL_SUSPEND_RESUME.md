# Suspending and resuming a DML session

A turn can **stop at a safe point** and write down the goal that continues it, so
`--session` picks the program up where it left off instead of dropping the user
into a half-finished state.

Status: **implemented** (cooperative suspension). Mid-execution suspension — a
continuation reified by the meta-interpreter — is designed but *not built*; §7
records the shape and why it is a separate step.

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

## 8. Follow-ups

- **Re-spawn services on resume.** `services.jsonl` already records each `serve`
  declaration (name, goal, priority, code version); `recordedProcesses()` reads it
  and nothing consumes it. A `repl.resume.services: none | list | restart` switch
  would make a resumed session whole again — `restart` is exact only for services
  that keep their state in the goal argument.
- **`hibernate/1`** for processes: a process may suspend returning its
  continuation goal, which is what makes a service's suspension exact.
- **Host-initiated suspend**: a `/suspend` request that takes effect at the
  program's next `suspend/1` (or throws a clear "the program has no safe point").
- **Resume a turn interrupted by a crash** (the `running` outcome in
  `turns.jsonl`): replay to the last recorded tool result and continue, never
  re-running a leaf that already has a result.
- **`--session` clone**: a snapshot you can fork without consuming it.
