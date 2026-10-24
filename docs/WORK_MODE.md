# Work mode — shared by TUI and web

Work **uses** a harness; Develop **authors** it. A Work conversation has its own
prompt, declared tool list, memory, snapshot and history. It is not a frozen
Develop agent and is not a sandbox for arbitrary DML: host grants, filesystem and
sandbox boundaries still apply to the capabilities the harness runs.

## Terminal

```bash
# Develop remains the default
 deepclause repl

# Use a committed harness slot
 deepclause repl --mode work --harness writing

# Run one exact declared workflow and exit (no assistant decision)
 deepclause repl --mode work --harness writing \
   --workflow "Uppercase" --inputs '{"Text":"hello"}' --json

# Or explicitly load a DML file for a named workflow
 deepclause repl --mode work -l writing.dml \
   --workflow "Uppercase" --inputs '{"Text":"hello"}'
```

The normal model backend/auth settings still apply. Pure workflows do not call
an assistant. For deterministic/headless use without credentials, `--backend env
--no-tools` can be used when the program itself needs neither models nor host
capability tools. If the DML workflow calls a model, that model still needs its
normal credentials.

| Command | Meaning |
|---|---|
| `/mode` | TUI mode/harness chooser; current mode in plain text |
| `/mode work [harness]` | Choose a separate Work conversation |
| `/mode develop [harness]` | Return to a separate authoring conversation |
| `/workflows` or `/workflow` | TUI workflow chooser; list in plain mode |
| `/workflow "Uppercase" {"Text":"hello"}` | Execute that exact named workflow |
| `/workflow "Uppercase"` | Ask for the declared inputs through the human channel |
| `/details [on|off]` | Show/hide technical events, never errors/results |

On a wide terminal the Work index shows workflows and assistant tools; on a narrow
terminal use the picker. Tools are informational, not directly runnable actions.
The footer identifies purpose, harness and loaded snapshot. `/harness` retains
its Develop source-view meaning.

Switches happen while idle and refuse to dispose active services silently. When
changing purpose, the matching conversation may be resumed; it is never converted
in place. Repeating `/mode work <name>` in the same conversation opens a fresh
snapshot, useful after edits. Work does not accept raw goals or edit/rollback
commands; switch to Develop for those operations.

`--session <id>` restores recorded purpose/harness. Legacy sessions without purpose
metadata require an explicit `--mode develop|work`; specify their original harness
if not known. Mismatched explicit purpose/harness is refused rather than silently
promoting a Work conversation to authoring.

## Declare a shell

Save as `writing.dml`, or author it in Develop:

```prolog
uppercase(Text, Upper) :- string_upper(Text, Upper).
length_of(Text, N) :- string_length(Text, N).

shell(_{
    kind: agent,
    system: "You are a concise writing assistant.",
    host_tools: [],
    tools: [_{name: "text_length", goal: "length_of",
              description: "Count characters in text.",
              inputs: [_{name: "Text", type: string}],
              outputs: [_{name: "N", type: integer}]}],
    workflows: [_{name: "Uppercase", goal: "uppercase",
                  inputs: [_{name: "Text", type: string}],
                  outputs: [_{name: "Upper", type: string}]}],
    welcome: "Ask about text, or choose Uppercase."
}).
```

`kind: workflow` provides a workflow-only surface, without a chat assistant.
Inputs precede outputs in actual predicate argument order. Arity must match the
sum exactly. An `agent_main` entry is not required for a shell library.

**No shell means no inferred Work interface.** Web offers Develop or an explicit
Run entry; the TUI explains how to open Develop/Run. Helpers are not exported, and
last arguments are not guessed to be outputs. Old shell syntax is still decoded,
but legacy `args` is input data only. Add explicit outputs when migrating it.
`host_tools: [execute]` is obsolete/ignored with a diagnostic.

## Types, execution and outcomes

Supported Work boundary types are `string`, `integer`, `number`, `boolean`, `file`.
Objects/arrays are rejected until their full boundary codec is supported; general
DML schemas and nested task tools continue to support complex values.

- Missing/unknown inputs, fractional or unsafe integers, non-finite numbers, bad
  booleans and unsupported types fail before execution. No silent zero/false.
- File values are workspace-relative strings, selected/uploaded/completed by the
  host. Absolute/URL/traversal paths are refused. Paths and strings are quoted as
  data; user input is never inserted as DML source.
- Inputs are bound; outputs are generated variables. Return bound values with
  the declared types. Use zero outputs for a successful side-effect-only goal.
- No solution, unbound/mistyped output, exception, cancellation and suspension
  are not success. A completed iterator alone does not make a successful run.
- Manual starts and assistant `run_workflow` use the same codec and foreground
  executor. Manual starts make **zero assistant calls**. Their results become
  labelled conversation data, not fabricated provider tool messages.
- Foreground workflow memory is retained; authoring scratch execution remains
  isolated. Root forms/input are available in foreground. Browser scratch workers
  and background worker processes cannot render forms.
- Same-session operations are serialized/reject overlap. Stop aborts the selected
  owner, propagates to browser model/MCP/input proxies, closes operation-owned
  forms, and releases the input queue. It does not stop another session.
- Work defaults to a 1,000,000-step gas budget; `--gas-limit` or `repl.gasLimit`
  can override it (`-1` is an explicit unlimited budget). Gas is not a wall-clock
  bound for every native/backtracking Prolog call. A browser session that cannot
  acknowledge Stop within 1.5 seconds is terminated; reopen its saved snapshot
  afterward. Node foreground SWI remains in-process, so native CPU calls are not
  universally preemptible; root-worker isolation is separate follow-up work.

## Persistence and refresh

SDK `session.json` schema version 2 records purpose, harness, session/run kind,
title, model, shell kind and snapshot identity. Workflow turns carry invocation
metadata; `workflow_result` events carry typed results. TUI/web use one replay
decoder, and replay never reopens a historical input prompt.

Work stays pinned to its initial program/interface and linked-script hashes.
Private DML facts and conversation memory may evolve and are saved separately
from program identity; starting a fresh conversation does not inherit those facts.
Develop can commit newer source independently. Refresh starts a new coherent snapshot; changed linked
scripts are detected instead of silently used with the old program. Shell-editor
saves are validated transactions in an idle Develop session with an expected base
hash, not edits to a running Work session. Program hashes use canonical DML rather
than incidental anonymous-variable ids. Missing embedded scripts are materialized
at Work startup; existing workspace files are not intentionally overwritten.

Web workflow-only runs are durable SDK sessions (`kind: workflow-run`), not an
unrelated log store or synthetic `runner` harness. Restoring one opens its run
transcript in Work. `suspend/1` has an explicit Resume action for these runs;
conversations can use `/resume`. Merely viewing/listing history never executes it.
CLI `--session` continues its saved continuation, as an explicit resume intent.
A pending continuation blocks new foreground calls in that same Work session;
resume it or deliberately open a new conversation. Transient services/forms/handles
are not restored.

OPFS and terminal disk remain separate stores. The metadata travels in workspace
files; no automatic synchronization, credential transfer or common native package
environment is implied. Existing per-host workspace export/import can transfer
files, while richer cross-host transfer UX is deferred. Stale sequential writers
are rejected, but distributed multi-tab/process writer leases and fully atomic
OPFS transactions remain follow-up work.

## Embedding API

```typescript
const repl = await createHarnessRepl({
  model, provider, llmBackend,
  workspacePath, harness: 'writing', history: {}, mode: 'work',
});
const spec = await repl.workSpec();
for await (const event of repl.runWorkflow('Uppercase', { Text: 'hello' }, { signal })) {
  if (event.workflow) console.log(event.workflow.result);
}
```

The browser RPC implements the same async Work methods: `workSpec`, `sessionInfo`,
`runWorkflow`, `suspension`, `resumeSuspension`. `setTitle` writes SDK metadata.
`updateWorkShell(spec, expectedHash)` is an explicit Develop operation. `workMode:
true` remains a compatibility alias, but callers no longer need to supply a web
Work prompt themselves. Extra authoring tools do not get merged into Work.

The optional third argument of `onInput(prompt, choices, signal)` lets a host close
its prompt when its owner is cancelled. Old one/two-argument handlers still work;
the SDK releases its own await/queue on cancellation even if such a handler does
not resolve.

## Runtime policy and build provenance

Both hosts read the non-secret `repl.gasLimit`, `maxDepth`, `maxModelCalls`,
`providerConcurrency`, and `compaction` settings. Model defaults are distinct from
the effective model/context of existing sessions. Compaction retention/reserve is
bounded for small context windows. Browser root/child judge selections are both
forwarded; capabilities still differ deliberately (bash/sandbox vs Pyodide/CORS).
`run_script/3` is the existing portable Python/script path.

```bash
# Sibling development build, in this order
cd deepclause-sdk
npm run build
npm run check:runtime
cd ../deepclause-web
npm run build
```

SDK builds stamp `dist/runtime-identity.json` with source/artifact digests, revision,
version and Work contract version. Web sync checks it before copying assets;
dev/build and worker handshakes reject incompatible builds. Web Help displays the
identity. Package version alone is not enough to identify an unreleased branch.
No package publication or Git push is performed by these commands.
