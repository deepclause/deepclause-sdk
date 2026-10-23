# Model calls, schemas and nested tools

Choose by tools, memory and output contract, not by which name sounds familiar.

## Selecting a model operation

| Operation | Context | Tools | Result |
|---|---|---|---|
| `prompt(Description, Type(Out))` | fresh; no memory read/write | no callable harness tools | typed output |
| `task(Description, Type(Out))` | DML harness memory; reply accumulates | declared nested tools, scoped explicitly | typed output |
| `llm(Messages, Reply)` | explicit message list (or supported text input) | none | raw text; no memory writeback |
| `chat(Message[, Reply])` | ongoing DML conversation | nested tools | conversational reply |
| `choose/rate/verify/judge` | explicit evidence/state | none | constrained semantic judgment |

For a normal Work assistant, use `shell/1`; `chat` is only for an explicitly
custom conversation protocol. `task/1` and `prompt/1` discard structured outputs;
prefer output-bearing forms when a later step needs the value.

## Typed outputs

A bare output variable is a string. Supported common wrappers:
`string(X)`, `integer(X)`, `number(X)`, `float(X)`, `boolean(X)`,
`list(string(Xs))`, `object(D)`, `schema(person, D)`,
`list(schema(person, Ds))`. Types are checked by the runtime, not just prose.
`media(X)` is image generation, not a general file output, and needs an image backend.

```prolog
schema(person, [field(name, string, required), field(age, integer, default(0))]).
extract_person(Text, Person) :-
    prompt("Extract a person from: {Text}", schema(person, Person)).
```

Schema field types include primitives, `list(Type)` and nested `schema(Name)` /
`schema(Name, _)`. Defaults fill missing fields; missing required/wrong-typed
fields fail validation; undeclared fields are retained. `schema_fill/3` applies a
schema to an existing dict. Keep one structured result per conceptual step.
Multiple typed outputs are possible; use the actual argument names consistently.

## Memory

Authoring conversation, Work chat and DML harness memory are different stores.
A task sees ONLY harness memory (prior DML `system`, `user`, `task` messages),
not the authoring user's whole conversation. Pass the input explicitly.
Consecutive tasks accumulate conversation memory; prompts neither read nor change
it. Do not assume a typed result is restated in assistant prose or all tool results
remain in memory; pass the actual bound outputs explicitly to the next step.

```prolog
explain(Text, Report) :-
    with_fresh_memory((
        task("Find the main claim: {Text}", string(Claim)),
        task("Explain the claim using only {Text}: {Claim}", string(Report))
    )).
```

`with_fresh_memory/1` isolates a group: its tasks share memory WITH EACH OTHER,
then the original memory is restored. `system/1` and `user/1` changes are
backtrackable. A task inside a nested `tool/N` engine starts fresh: context must
be an input or supplied with `system/1` inside that body. A process has its own
memory snapshot. Attached media is harness memory, so a fresh `prompt/N` will
not see an image attached for a task.

## Nested tools

Host registration is NOT nested-agent exposure. `exec(python(...), R)` calls a
host capability from DML. To offer a capability to a nested task, define the
predicate AND its thin annotation:

```prolog
length_of(Text, N) :- string_length(Text, N).
tool(length_of(Text, N), "Count text characters.", [string]) :- length_of(Text, N).
inspect(Text, Reply) :-
    with_tools([length_of], task("Use length_of on {Text}; report its result.", string(Reply))).
```

The annotation and delegate share name AND arity. Exactly one output, LAST;
all earlier arguments are inputs. The optional type list describes input types
only (default string). A write capability needs e.g. `(Path, Content, Result)`,
not `(Path, Content)`. Several result fields belong in a dict.
Use names in `with_tools`, not indicators. `without_tools/2` excludes names.
Tool-free tasks can use `with_tools([], task(...))`; task result/finish machinery
is internal, not a harness tool you must write. Built-in form tools are otherwise
available to tasks, so scope non-form work.

Do not implement the work ONLY inside an annotation: DML callers invoke the real
predicate. Do not advertise an LLM step as the very tool offered inside its own
task; keep it a skill or explicitly exclude that tool to prevent self-delegation.
`shell.tools` is a DIFFERENT, explicit Work-agent interface; no annotation is
needed unless a nested task needs the same capability.

## Data and evidence

Interpolate bound DML data with `{Var}` in task/prompt descriptions. Include the
actual input in every step; variable names alone do not supply their values.
Use typed extraction, then deterministic guards. A successful model response is
not evidence that a file was saved or an API action completed. Inspect the host
result, check `exitCode` for Python, and do not ask a model to fill missing data.
Use `dict_field/3,4` for varying model keys, `get_dict/3` for keys you control,
and `json_get/3,4` / `json_gets/3` for paths and projections.

## Judgments and models

`choose(State, Question, Options, Choice)` returns an option atom.
`rate(State, Question, Levels, Level)` returns a supplied level.
`verify(State, Question, Truth)` returns `yes | no | unknown`; `probability/3`
returns a number; `holds/2` succeeds for yes. `judge(State, Specs)` batches
questions such as `choose(Question, Options)-Choice` in one request.
Judgments do not see task memory or images. Supply observed evidence as State;
`unknown` is not no, and neither is a failed goal.
`with_model(Spec, Goal)` changes task/prompt/llm model selection, not judgment
backend selection; use only model ids available in this session. Read the exact
scopes and backend capabilities in the language reference before relying on them.
