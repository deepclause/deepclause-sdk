# Runnable canonical examples

Generated from scripts/canonical-examples.mjs; do not hand-copy into the system prompt.
Pick ONE matching section. Each skill binds its outputs; agent_main is only used
for the explicitly custom conversation. Host-dependent examples list prerequisites.

## work-agent

**Default Work agent with a tool and workflow**

The default conversation is supplied by shell/1. A custom persona does not need a DML chat loop. Inputs precede outputs; tools are not directly run from the sidebar.

Requires: Work host; title workflow requires a text model. The character-count test is pure DML.

```prolog
length_of(Text, N) :- string_length(Text, N).
title(Text, Label) :- prompt("Give a short title for: {Text}", string(Label)).
shell(_{
    kind: agent, system: "You are a concise writing assistant.", host_tools: [],
    tools: [_{name: "text_length", goal: "length_of", description: "Count characters.",
              inputs: [_{name: "Text", type: string}], outputs: [_{name: "N", type: integer}]}],
    workflows: [_{name: "Create title", goal: "title",
                  inputs: [_{name: "Text", type: string}], outputs: [_{name: "Label", type: string}]}],
    welcome: "Ask about text, or create a title."
}).
```

Test goal: `length_of("Ada", N), write(N)`. Expected: `3`.

Failure/cancel check: Do not replace output binding with answer/1 or add a custom conversation loop.

## basic-relation

**Arguments, arithmetic and a deterministic branch**

N is an input; Label is an output. is/2 evaluates arithmetic, = binds a returned value. A failed input guard is not an exception.

Requires: No host tools or model.

```prolog
size_label(N, Label) :-
    integer(N), N >= 0,
    Twice is N * 2,
    ( Twice > 10 -> Label = large ; Label = small ).
```

Test goal: `size_label(2, Label), write(Label)`. Expected: `small`.

Failure/cancel check: size_label(-1, _) has zero solutions; catch/3 does not catch a false guard.

## typed-pipeline

**Fresh typed transformation and explicit data flow**

Each prompt includes its input. No tool or previous-memory dependency. Reject empty intermediate data before the next semantic step.

Requires: Text model; tests supply deterministic typed completions.

```prolog
brief(Text, Report) :-
    prompt("Extract the central claim from: {Text}", string(Claim)),
    Claim \== "",
    prompt("Explain this claim using only the source {Text}: {Claim}", string(Report)).
```

Test goal: `brief("A source document", Report), write(Report)`. Expected: `A grounded summary.`.

Failure/cancel check: An empty Claim fails before generating a report.

## task-tools

**A scoped task calls one declared capability**

Host tools, nested tool/N annotations and shell tools are different surfaces. This task may only call length_of; the real predicate remains callable directly.

Requires: Text model with tool calling; test checks the capability really executes.

```prolog
length_of(Text, N) :- string_length(Text, N).
tool(length_of(Text, N), "Count characters.", [string]) :- length_of(Text, N).
inspect(Text, Reply) :-
    with_tools([length_of], task("Call length_of for {Text}; report the observed count.", string(Reply))).
```

Test goal: `inspect("Ada", Reply), write(Reply)`. Expected: `3 characters`.

Failure/cancel check: Naming length_of in prose without registering its annotation would not expose it.

## task-memory

**Task memory accumulates; a fresh prompt does not inherit it**

Task memory is DML harness memory, not the authoring chat. The tasks share a fresh group, then a separate prompt receives only explicitly supplied data.

Requires: Text model; tests inspect actual request memory, not only returned strings.

```prolog
memory_demo(Out) :-
    with_fresh_memory(with_tools([], (
        task("Return marker alpha.", string(First)),
        task("Report the earlier marker. Explicit value: {First}", string(Second))
    ))),
    prompt("Report only this explicit result: {Second}", string(Out)).
```

Test goal: `memory_demo(Out), write(Out)`. Expected: `Memory scopes checked.`.

Failure/cancel check: Do not assume a prompt sees a prior task or a nested tool inherits the Work transcript.

## judgment-routing

**A constrained semantic choice with explicit evidence**

Choose returns one option atom; deterministic branching follows it. A semantic judgment is not proof that an external action was performed.

Requires: Configured judgment backend (the test uses the session model with a deterministic judge response).

```prolog
route(Message, Team) :-
    choose(Message, "Which team handles this?", [billing, orders, account], Choice),
    ( Choice == billing -> Team = billing
    ; Choice == orders -> Team = orders
    ; Team = account
    ).
```

Test goal: `route("I was charged twice", Team), write(Team)`. Expected: `billing`.

Failure/cancel check: Unknown evidence is not a reason to claim that a real refund happened.

## schema-output

**A declared schema rather than guessed dict keys**

The schema is an output contract, not merely a suggested JSON shape. Missing required fields and invalid types fail validation.

Requires: Text model; deterministic schema-aware test output.

```prolog
schema(person, [field(name, string, required), field(age, integer, default(0))]).
extract_person(Text, Person) :-
    prompt("Extract a person from: {Text}", schema(person, Person)).
```

Test goal: `extract_person("Ada, age 36", Person), get_dict(name, Person, Name), write(Name)`. Expected: `Ada`.

Failure/cancel check: Missing name must not be silently converted to an empty string.

## python-files

**Python, files and argv**

Write/link the helper during authoring, not on each run. Script link normalise_py is distinct from the normalise/2 predicate. run_script checks exit status and parses JSON.

Requires: Python host capability (or a compatible script execution embedding). Files below are fixtures to create before calling the skill.

```prolog
script_file(normalise_py, "scripts/normalise.py").
normalise(Path, Text) :-
    run_script(normalise_py, [Path], Data),
    get_dict(text, Data, Text).
```

Test goal: `normalise("canonical/input.txt", Text), write(Text)`. Expected: `HELLO`.

Failure/cancel check: A missing input file produces a script_failed error; no model fills the missing text.

Fixture `canonical/input.txt` (create before run):

```text
hello
```

Fixture `scripts/normalise.py` (create before run):

```python
import json, sys
from pathlib import Path
print(json.dumps({'text': Path(sys.argv[1]).read_text().upper()}))
```

## swarm

**Bounded parallel mapping with visible failures**

Results preserve input order. Every item has its own process; failed items remain visible. Use pmap only if any failure should fail the whole batch.

Requires: Process-capable session; no model.

```prolog
double(N, R) :- R is N * 2.
parallel_values(Items, Results) :- swarm(double, Items, Results, 2).
```

Test goal: `parallel_values([1,2,3], Results), write(Results)`. Expected: `[ok(2),ok(4),ok(6)]`.

Failure/cancel check: Inspect failed items rather than assuming all results succeeded.

## spawn-python

**Spawn a worker and return by message**

Create required files before spawn. Child output variables do not bind the parent; send a result, receive it with a bound, then reap the worker.

Requires: Python and process host. Test fixture canonical/input.txt exists before spawn.

```prolog
read_in_worker(Path, Out) :-
    self(Me), spawn(file_worker(Path, Me), Pid),
    receive(done(Out), 30000), wait(Pid, _).
file_worker(Path, Parent) :-
    exec(python(code: "from pathlib import Path\nimport sys\nprint(Path(sys.argv[1]).read_text().upper())", argv: [Path]), R),
    get_dict(exitCode, R, 0), get_dict(stdout, R, Out),
    send(Parent, done(Out)).
```

Test goal: `read_in_worker("canonical/input.txt", Out), write(Out)`. Expected: `HELLO`.

Failure/cancel check: A failed worker may never send; the receive is bounded. Inspect the worker and stop/reap it after a timeout.

Fixture `canonical/input.txt` (create before run):

```text
hello
```

## service

**A stateful service with correlated replies**

A service is for message-driven state, not a single model call. Registered names survive across turns while the process is alive. Stop/reap test services.

Requires: Process-capable session.

```prolog
service_demo(Text) :-
    serve(counter, counter_loop(0), Pid),
    service_call(counter, bump, First, 5000),
    service_call(counter, bump, Second, 5000),
    service_call(counter, read, Final, 5000),
    service_cast(counter, stop), wait(Pid, _),
    format(string(Text), "~w ~w ~w", [First, Second, Final]).
counter_loop(Count) :-
    receive(Msg),
    ( Msg = request(From, Ref, read)
    -> send(From, reply(Ref, Count)), counter_loop(Count)
    ; Msg = request(From, Ref, bump)
    -> Next is Count + 1, send(From, reply(Ref, Next)), counter_loop(Next)
    ; Msg = stop -> true
    ; counter_loop(Count)
    ).
```

Test goal: `service_demo(Text), write(Text)`. Expected: `1 2 2`.

Failure/cancel check: Duplicate service names are rejected; callers use bounded service_call timeouts.

## vision

**Vision input is not display or generation**

Attach a workspace reference, then use a vision task. A fresh prompt does not read the attached image. Display is a separate output operation.

Requires: Vision-capable model and supported image file; tests provide a tiny PNG and assert media is actually in the model request.

```prolog
describe_image(Path, Text) :-
    read_media(Path, Image), attach_media(Image),
    with_tools([], task("Describe only the attached image.", string(Text))).
show_image_file(Path) :- output(media(Path)).
```

Test goal: `describe_image("canonical/pixel.png", Text), write(Text)`. Expected: `An observed image.`.

Failure/cancel check: A text-only model cannot analyse pixels. Image generation additionally needs a configured image backend.

Binary fixture: provide a valid supported image before running (test suite includes one).

## fixed-form

**Fixed foreground form with a cancellation branch**

Known fields need no model. The host enforces hard validation; a refused submit is not delivered. Never save on cancel or unavailable.

Requires: Foreground form host. Browser disposable execute/process workers do not render forms.

```prolog
claim_form(Values) :-
    form(_{title: "Quick check-in", fields: [_{name: name, type: string, required: true}]}, F),
    await_form(F, Change), close_form(F),
    ( Change.action == submit -> Change.values = Values ; Values = _{} ).
```

Test goal: `claim_form(V), get_dict(name, V, Name), format(string(S), "Hello ~w", [Name]), write(S)`. Expected: `Hello Ada`.

Failure/cancel check: Cancel/unavailable returns an empty dict; a caller must not create a record from it.

## dynamic-form

**Context-dependent fields designed by a scoped task**

The nested model designs the form, waits for actual values and closes it. Result/finish machinery is internal, not a user tool annotation.

Requires: Foreground form host and text model with tools.

```prolog
collect_details(Context, Summary) :-
    with_tools([render_form, wait_for_form_change, close_form],
        task("Collect {Context} using a form, wait for the user action, close the form, and report only what they entered.", string(Summary))).
```

Test goal: `collect_details("name and favorite color", Summary), write(Summary)`. Expected: `Ada likes blue.`.

Failure/cancel check: A form rendered without a real submission is not evidence that intake completed.

## custom-intake

**Custom staged conversation (advanced, not the default)**

Only use a custom loop for a required protocol. This example validates project intake BEFORE allowing conversation. A normal assistant with a persona/tools uses shell/1 instead.

Requires: Foreground conversation/input host and text model; file fixture exists. First input is the required project name, followed by chat messages.

```prolog
agent_main :-
    input("Project name?", Project), Project \== "",
    system("You are a concise assistant for project {Project}."),
    conversation.
conversation :- input(Message), chat(Message), conversation.
read_text(Path, Text) :- read_file(Path, Text).
tool(read_text(Path, Text), "Read a workspace text file.", [string]) :- read_text(Path, Text).
```

Test goal: `agent_main`. Expected: `I remember the earlier turn.`.

Failure/cancel check: An empty project fails before chat starts; no custom loop should be added for ordinary conversation.

Fixture `canonical/input.txt` (create before run):

```text
hello
```
