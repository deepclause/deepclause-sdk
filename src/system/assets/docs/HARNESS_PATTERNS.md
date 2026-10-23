# Harness design patterns

Choose the smallest pattern that matches a real requirement. DML coordinates
exact logic, capabilities, model work and human interaction. It supports real
process concurrency; consecutive tasks accumulate harness memory unless scoped.

## Normal agent

Use `shell/1` kind agent with a few typed capabilities in `shell.tools` and recurring
user tasks in `workflows`. No agent_main/chat loop is needed. Persona/welcome are
shell fields, not a reason to implement a custom protocol. Tools are discovered
in Work but not directly executed from the sidebar. Read `shell` and copy the
`examples` section `work-agent`.

## Isolated transform

A string/schema extraction, translation or classification without tools is a
fresh `prompt/N`. Include all input and instructions explicitly. Use deterministic
DML for exact arithmetic/formatting; do not call a model for a known calculation.
For a fixed set of labels, use `choose/4` with explicit evidence instead.

## Tool-using task

Define a small deterministic capability and a delegating `tool/N` annotation.
Use `with_tools([capability], task(..., Type(Result)))`. This allows a bounded
model-owned plan INSIDE one step without exposing the whole host registry.
A task does not automatically see authoring or Work chat. Supply context.
Do not advertise the task itself as a tool that it can call recursively.

## Pipeline

Pass values explicitly across a short chain. Use `with_fresh_memory` when a group
should share context internally but not with the caller. Guard failed/empty fetch
results BEFORE semantic synthesis. A second `task/N` is not fresh; choose prompt
or a scope when isolation is intended.

```prolog
brief(Text, Report) :-
    prompt("Extract the central claim: {Text}", string(Claim)),
    Claim \== "",
    prompt("Explain only this claim, using {Text}: {Claim}", string(Report)).
```

## Routing and verification

Use `choose(State, Question, Options, Choice)` then parenthesized deterministic
branches. For several independent questions use `judge/2` batching.
Distinguish `unknown` from `no`. A model's assertion is not proof that an external
operation occurred; use observed tool results and hard checks for side effects.

## Parallel and background work

Use `swarm` for the same independent skill over a bounded list (ordered ok/failed
results), `pmap` when any failure should fail the batch. Use `spawn` for work that
outlives the turn; child outputs do not bind parent variables. Use `serve` for
mailbox/state loops or a single writer. Commit before spawn, pass messages/refs,
inspect results and clean up. Read `processes` before adding the first worker.

## Human intake

Known fields: a fixed `form` / `await_form` / `close_form` workflow.
Context-dependent fields: a scoped task with built-in form tools. Hard validation
is host-enforced; never save a rejected, cancelled or unavailable submission.
Run in foreground with an interactive form host, not a disposable execute worker.
Use a custom input/chat loop only for an explicitly required staged protocol.
Read `forms`, then the matching example.

## Files and semantic analysis

Mechanical parsing, filesystem work and substantial algorithms belong in a script
with a distinct script link name and a small result contract. Use only host
capabilities from the current catalog. Browser Python has no shell/CLI binaries;
examples must not assume desktop browser drivers or native wheels. Emit JSON,
check exit status and pass data via argv/workspace files, never source injection.
Then use a typed semantic step if meaning is required.

## Bounded refinement

If quality can be evaluated, use an explicit small attempt counter plus a judgment
or deterministic validator. Stop after the bound and report uncertainty; do not
use backtracking to repeat irreversible effects. Do not add retries/caching or
an optimizer unless the task actually needs them.

## Media and miniapps

Analysis: attach a file reference to a vision task. Display: output media/HTML via
the host. Generation: check configured image backend first. Keep bytes out of
clauses and context. Miniapp callback predicates must be bounded and deterministic.
Read `media` before using a new media/rendering surface.
