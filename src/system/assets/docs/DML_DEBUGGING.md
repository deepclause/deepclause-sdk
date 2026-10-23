# Debugging DML

Reproduce the smallest failing goal, inspect the actual result, then change the
smallest relevant predicate. A supported contract is cheaper to read than guesses.

## Zero solutions versus exceptions

`solutions: []` means the goal failed; `catch/3` will not intercept ordinary
failure. Inspect each boundary: input type, missing field, guard, host tool, output
binding. Use `(Goal -> output("succeeded") ; output("failed here"))` and temporary
`output/1` calls with intermediate values. Remove noisy diagnostics afterwards.
Do not turn a failed fetch or cancelled form into invented success data.
An empty predicate and a handler primitive are different: `current_predicate`
can report false for built-in DML handlers such as `form/2` or `exec/2`.

## Model calls

Read `tasks` for memory/type/tool scope errors. A bare output is a string, not a
number; use type wrappers. A task sees harness memory, not the authoring request.
A prompt sees only its own description and no attached memory images.
Pass exact inputs, include them in the description, and guard empty data before
a model can fill gaps from prior knowledge. A tool named in prose is not registered.

## Host failures

Read the CURRENT schema via `tools` before retrying a parameter error. Inspect
host failure, `exitCode`, `stderr`, `stdout` separately; a nonzero Python exit may
still return a dict. `run_script/3` checks exit and parses JSON for you.
In the browser, Python has no stdin option and DML/Python private tmp directories
are different. Use workspace argv/files, not source-code interpolation.
For script-name conflicts, rename the link (`foo_py`) or predicate; use
`delete("script_file:old_name")` for a stale link.

## Execution contexts

| Context | Structured forms | Test isolation |
|---|---|---|
| Foreground query/run with UI | yes, via session handler | normal session/run |
| Work workflow/tool foreground runner | yes, via session handler | not disposable test worker |
| Browser authoring execute worker | no | disposable engine; host/file effects real |
| Browser worker-hosted process | no | private code/memory snapshot; live shared workspace |

The same goal can be valid but unsupported in a particular host. Interactive form
flows must be tested in foreground, not by repeatedly trying scratch execute.
Test pure validation separately; cancellation/unavailable is NOT a submitted record.
No capability directive is necessary. Close forms on every exit path.

## Edits and annotations

Read `authoring` for transaction/target contracts. `define` takes a head matching
the actual predicate arity. For one doc use `doc(foo/2, _)`, NOT `doc(foo/2)`.
Arity changes add a different predicate; remove the old predicate/doc key explicitly.
`delete("doc:foo/2")`, `delete("schema:Name")`, `delete("script_file:Name")`
remove one entry, never all docs/schemas/scripts. Missing keys are errors.
Read the end_turn result, not your intent; verify `listing` after changes.

## Process failures

Use `ps`, process state/errors, and the process harness version. A service keeps
its spawn-time snapshot; an edit does not hot-update it. Children cannot bind
parent outputs. Use messages or wait outcomes, inspect `failed` swarm items, bound
receives, and stop/reap test processes. Do not equate zero model tokens with a dead
service. See `processes` for exact signatures.

## Logs and bounds

Read only a short tail of `.deepclause/repl/sessions/<id>/events.jsonl` using the
available host capability when normal tool results are insufficient. Logs can
contain user data, prompts and arguments. Do not dump them wholesale to the model.
Use execute's `timeoutMs`, result limits and reduced bindings for scratch tests;
foreground goals/processes have different cancellation guarantees. Bound finite
search and builtin inference separately; gas alone cannot preempt every builtin.
After a couple of failed attempts, report the evidence and remaining uncertainty.
