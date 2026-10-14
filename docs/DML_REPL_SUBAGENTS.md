# Subagents in the REPL — a simple example

Status: **simple example first**. REPL-only. The deep process-tree, supervision,
and scheduler details live in [`DML_REPL_SERVICES_UX.md`](./DML_REPL_SERVICES_UX.md)
and [`DML_SPECIALIZED_PROCESSES_EXAMPLE.md`](./DML_SPECIALIZED_PROCESSES_EXAMPLE.md);
this document is the smallest thing that shows the idea.

## 0. `spawn` is the primitive. `serve` is not a separate concept.

The question was: *"the authoring agent creates the DML code and then runs the
serve tool on it — shouldn't that be `spawn`?"* **Yes.** There is one primitive:

```prolog
spawn(Goal, Pid).            % start a subagent running Goal; return its pid
spawn_opt(Goal, Pid, Opts).  % + name / memory / timeout
```

`serve` is just `spawn` + `register/2` + a standard request loop. It is a
**library convenience**, not a second kind of process. The goal decides the shape:

| The subagent… | Its goal… |
|---|---|
| runs a task and exits | calls `task/N`, then `answer/1` |
| waits for messages | loops on `receive/1` |
| asks the user and waits | calls `input/2` (which blocks until the UI answers) |

So "start a subagent that does task X" is `spawn`, full stop.

`spawn` creates a **session-owned** process: it keeps running after the goal that
spawned it returns, and it is visible in the process panel until it exits or is
stopped. That is what makes "start it, then keep working" work.

---

## 1. Three subagents, three shapes

`helpers.dml`:

```prolog
:- use_module(library(lists)).

%% ---- shape 1: one-shot. Runs, reports a result, exits. ----------
summarise(Topic) :-
    task("Summarise '{Topic}' in three bullets.", list(string(Bullets))),
    answer(bullets(Topic, Bullets)).          % the subagent's result

%% ---- shape 2: message-waiting. Stays alive. ---------------------
note_keeper(Notes) :-
    receive(Msg),
    (   Msg = {add, Note, From, Ref} ->       % a term from another process
        send(From, {ok, Ref}),
        note_keeper([Note|Notes])
    ;   Msg = {list, From, Ref} ->
        send(From, {notes, Ref, Notes}),
        note_keeper(Notes)
    ;   Msg = {text, Text} ->                 % text sent by the user
        output("noted: {Text}"),
        note_keeper([Text|Notes])
    ;   Msg = stop ->
        answer(notes(Notes))
    ;   note_keeper(Notes)
    ).

%% ---- shape 3: interactive. Asks the user, waits, then acts. -----
confirm_and_run(Question, Action) :-
    input(Question, Answer),                  % blocks this process; the
                                              % REPL shows the question in
                                              % the editor and waits
    (   Answer = "yes" ->
        task("Do this now: {Action}", Result),
        answer(Result)
    ;   answer(declined(Answer))
    ).
```

Start them:

```prolog
start_helpers(Topic) :-
    spawn(summarise(Topic), Summariser),
    spawn(note_keeper([]), Keeper),
    spawn(confirm_and_run("Deploy now?", "run the deploy script"), Guard),
    answer("Started 3 helpers.").
```

A process's label defaults to its goal's predicate name, so the panel already
reads well without explicit names.

---

## 2. How the authoring agent starts them

The agent does what it already does, in two steps:

1. **Author the code** — `define`/`assert` the predicates (committed with
   `end_turn`), or write a `.dml` file with `write_file`.
2. **Spawn** — call the `spawn` tool with the predicate and args:

```
spawn(summarise("quantum error correction"))
spawn(note_keeper([]))
spawn(confirm_and_run("Deploy now?", "run the deploy script"))
```

The tool returns immediately with a pid and label. The agent reports and
continues the turn. It only waits if it explicitly wants the result.

> A `serve` tool, if it exists at all, is an alias for the common case of a
> named request/reply process. It is not needed for these three examples.

The agent may also just run a starter goal — `spawn` inside it still creates
session-owned processes:

```
/goal start_helpers("quantum error correction").
```

---

## 3. What it looks like

```
> start three helpers: one to summarise quantum error correction, one to keep
  notes, and one to confirm before deploying

  ● authoring agent
    ⚙ define summarise/1, note_keeper/1, confirm_and_run/2
    ⚙ spawn summarise("quantum error correction")      → summarise
    ⚙ spawn note_keeper([])                            → note_keeper
    ⚙ spawn confirm_and_run("Deploy now?", "run the deploy script")
                                                       → confirm_and_run
    Started 3 helpers. confirm_and_run is asking you a question.

  ⚙ services (3)
    summarise        running   task(...)
    note_keeper      waiting   0 notes
    confirm_and_run  input     "Deploy now?"

  ~/proj  ⚙ 3 svc  12k↑ 2k↓ · 24%                                        claude-sonnet-4
```

The question from `confirm_and_run` appears in the **editor**, like `ask_user`:

```
┌─ confirm_and_run asks ───────────────────────────────────────────┐
│ Deploy now?                                                      │
> yes▊
└──────────────────────────────────────────────────────────────────┘
```

Answer `yes`, and the process resumes and runs its action. You can ignore it and
keep working; it stays suspended.

Talking to `note_keeper` from the prompt:

```
> /send note_keeper the migration finished
  ⚙ note_keeper ← "the migration finished"

> /send note_keeper
  ⚙ note_keeper › notes(["the migration finished"])
```

- `/send <name> <text>` delivers `{text, Text}` — the "text from the user" case.
- Another process would `send(KeeperPid, {add, Note, self, Ref})` — the
  "terms/messages from other services" case.
- `/call <name> <term>` does request/reply when you want the answer inline.

`summarise` finishes on its own:

```
  ⚙ summarise done — bullets("quantum error correction", [...]) · 3.1k tok
```

`note_keeper` keeps waiting. `/stop note_keeper` ends it (and delivers its final
`answer/1` as a result).

---

## 4. Commands used above

| Command | Meaning |
|---|---|
| `/ps [--watch]` | show / pin the process panel |
| `/send <name> <text>` | deliver `{text, Text}` to a subagent |
| `/call <name> <term>` | request/reply; prints the answer |
| `/stop <name>` | stop a subagent (graceful) |
| `/tail <name>` | last events from a subagent |

The agent-side tools are the same: `spawn`, plus `ps`, `send`/`cast`, `call`,
`stop`.

---

## 5. Why this is the right first example

- **One primitive.** `spawn` covers all three shapes; there is no `serve` vs
  `spawn` distinction to learn.
- **The goal expresses the lifecycle.** Exit = one-shot, `receive` loop =
  service, `input/2` = interactive. No configuration.
- **`input/2` already exists.** A subagent asking the user and waiting is the
  existing human channel, just running in its own process. The REPL reuses the
  editor, exactly as `ask_user` does today.
- **Nothing blocks the session.** The spawner returns immediately; the user and
  the agent keep working; the panel shows the rest.

Everything else — supervision strategies, pools, budgets, resume — is layered on
top and can come later. The minimal mental model is: **`spawn` a subagent; the
code it runs decides whether it exits, waits, or asks.**
