# Work shell contract

Use a normal shell assistant for ordinary conversation, even with a custom
persona, welcome or tools. Write a DML chat loop only for explicitly custom
turn-by-turn control logic. A shell library needs no `agent_main`.

## Agent shell

```prolog
length_of(Text, N) :- string_length(Text, N).
label(Text, Label) :- prompt("Give a short title for: {Text}", string(Label)).
shell(_{
    kind: agent,
    system: "You are a concise writing assistant.",
    host_tools: [],
    tools: [_{name: "text_length", goal: "length_of", description: "Count characters.",
              inputs: [_{name: "Text", type: string}], outputs: [_{name: "N", type: integer}]}],
    workflows: [_{name: "Create title", goal: "label",
                  inputs: [_{name: "Text", type: string}], outputs: [_{name: "Label", type: string}]}],
    welcome: "Ask about text, or create a title."
}).
```

The Work assistant gets real typed tools plus `run_workflow` if workflows exist.
It does not get DML/authoring tools such as `execute`, `listing` or `define`.
Describe persona/domain in `system`; put capability semantics in tool descriptions.
`welcome` is Markdown. Users can edit this contract in the web Shell editor or in
Develop. TUI Work is entered with `deepclause repl --mode work --harness NAME`;
`/workflows` opens its chooser. Without a shell, use Develop or an explicit Run;
internal predicates are never automatically exported.

## Metadata names must be text, not variables

Write `name: "Query"`, `name: "Url"`, `name: "Report"` (or quoted atoms).
Bare capitalized `Query`, `Url`, `Report` are Prolog variables, not labels. They
produce unbound/empty runtime parameter names. The visual editor quotes names
for you; when authoring DML directly, include the quotes.

`end_turn` refuses an invalid shell before publishing a version. Keep the
transaction open, correct the declarations, then commit again. Full-source
editor saves are checked too. This validates the Work interface; executing a
backing predicate alone is not a substitute for validating its tool declaration.

## Inputs and outputs

For EVERY tool/workflow, actual predicate arguments are inputs followed by outputs,
in declared order. Arity = input count + output count. Manual starts ask only
inputs, leaving outputs unbound. Bind outputs and return; do not call `answer/1`
in place of returning them. An output cannot be left unbound just because text
was printed. Use zero outputs explicitly for a side-effect-only workflow.

Supported Work boundary types are string, integer, number, boolean and file.
A `file` input is a workspace-relative path supplied by the picker/upload, not raw
bytes or a raw Prolog term. Object/array boundary types are currently rejected;
use a JSON string or a file instead. General task/schema outputs still support
complex values. Runtime tool schemas are authoritative, not a wish list.
Old `args` is legacy input data only; author new shells with `inputs`/`outputs`.

## Workflows versus tools

A workflow is a recurring user task with a prescribed result, e.g. intake,
reporting or a review. Users can start it from the sidebar; the assistant can use
`run_workflow(name, inputs)`. A tool is a composable capability the ASSISTANT
chooses. Tools appear for discovery but cannot be run directly from the sidebar.
Do not export internal helpers, docs, schemas or all predicates indiscriminately.

`shell.tools` is NOT `tool/2`: the latter exposes a predicate to a nested agent
inside `task/N`, with a single last output and input types in its annotation.
A predicate may participate in both interfaces if needed, with one implementation.
Host tools are a third surface (`exec/2`); `host_tools: [show_image]` allows the
Work image-display helper, not arbitrary DML/host execution.

## Workflow-only shell

```prolog
uppercase(Text, Upper) :- string_upper(Text, Upper).
shell(_{kind: workflow, system: "", host_tools: [], tools: [],
        workflows: [_{name: "Uppercase", goal: "uppercase",
                      inputs: [_{name: "Text", type: string}],
                      outputs: [_{name: "Upper", type: string}]}], welcome: "Choose a task."}).
```

There is no assistant loop. A manual start executes exactly the selected workflow
with validated inputs and no intervening assistant call, for BOTH shell kinds.
Outputs are named typed values; no solution/unbound output/cancel/suspend is not
success. Active Work conversations are pinned to their program/script snapshot;
refresh opens a new one instead of mixing an old shell with new code.
Interactive workflows must run through foreground form/input handlers. Browser
scratch `execute` workers and worker-hosted processes do not support forms.
Test pure validation/headless paths separately from actual foreground form submits.

## Custom conversation

A custom DML `input/1` + `chat/1` loop is for a required protocol or per-turn
state, e.g. staged intake that validates each stage before advancing. Merely
asking for a conversational agent or changing its persona does not require it.
Use `next/1` only if the conversation must also react to process messages.
Such loops must run in foreground with an input host, not inside scratch execute.
See `examples` section `custom-intake` for a minimal custom staged protocol.

## Authoring and validation

Define the shell together with the predicates it exposes in one transaction.
Validate goal names/arities, unique tool/workflow names, input/output types and
bound output values. Keep the shell small and its descriptions specific.
A shell change refreshes Work; an already-running process still has its old
harness snapshot. Do not assume a live process updates with edits.
