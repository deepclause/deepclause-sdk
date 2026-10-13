# DML Language Reference

**DML** (DeepClause Meta Language) is a simplified Prolog dialect for programming AI agents. It combines declarative logic programming with LLM-powered task execution.

## Table of Contents

- [Program Structure](#program-structure)
- [Entry Point](#entry-point)
- [Core Predicates](#core-predicates)
  - [task/1, task/2, task/3, task/4](#task-predicates)
  - [exec/2](#exec2)
  - [Tool Definitions](#tool-definitions)
  - [Tool Scoping](#tool-scoping)
  - [Model Selection](#model-selection)
  - [system/1, user/1](#memory-predicates)
  - [output/1, yield/1, log/1](#output-predicates)
  - [answer/1](#answer1)
- [Control Flow](#control-flow)
- [String Interpolation](#string-interpolation)
- [Backtracking](#backtracking)
- [Full Prolog Support](#full-prolog-support)

---

## Program Structure

A DML program is a collection of Prolog clauses. Comments use `%` for single lines or `/* */` for blocks:

```prolog
% This is a single line comment

/* 
   This is a 
   multi-line comment 
*/

% Define helper predicates
greet(Name, Greeting) :-
    format(string(Greeting), "Hello, ~w!", [Name]).

% Main entry point
agent_main :-
    greet("World", Msg),
    answer(Msg).
```

---

## Entry Point

Every DML program must define `agent_main` as the entry point. It can have 0, 1, or 2 arguments:

### agent_main/0
```prolog
agent_main :-
    task("Do something"),
    answer("Done").
```

### agent_main/1
Receives the first argument:
```prolog
agent_main(Name) :-
    format(string(Msg), "Hello, ~w!", [Name]),
    answer(Msg).
```

### agent_main/2
Receives the first two arguments:
```prolog
agent_main(Name, Task) :-
    system("You are a helpful assistant."),
    task(Task),
    answer("Complete").
```

---

## Core Predicates

### `sample_token(+Prompt, -Token)`

Run a fresh prompt and normalize the result down to a single raw token string.
This is useful when the model should emit one grammar symbol, label, or keyword
instead of a full sentence.

```prolog
agent_main :-
    sample_token("Pick a SQL keyword for starting a query.", Token),
    answer(Token).
```

### `sample_token(+Prompt, +Allowed, -Token)`

Same as `sample_token/2`, but constrains the result to an allowed token list.
If the model returns something outside that list, DML falls back deterministically
to the first allowed token. The runtime still samples just one model token, so
the prompt can stay terse and the grammar frontier can live in `Allowed`.

```prolog
agent_main :-
    sample_token("Pick the next token for a query.", ["select", "update"], Token),
    answer(Token).
```

### Task Predicates

#### `task(+Description)`

Invoke an LLM to perform a task. The description is sent to the model.

```prolog
agent_main :-
    system("You are a coding assistant."),
    task("Write a function to calculate factorial in Python."),
    answer("Code generated!").
```

#### `task(+Description, -Var1)`

Invoke LLM and bind output to a variable. Mention the variable name in the description:

```prolog
agent_main :-
    task("Calculate 2 + 2 and store the result in Answer.", Answer),
    format(string(Msg), "The answer is: ~w", [Answer]),
    answer(Msg).
```

#### `task(+Description, -Var1, -Var2)`

Bind multiple variables:

```prolog
agent_main :-
    task("Generate a name and age for a character. Store name in Name and age in Age.", Name, Age),
    format(string(Msg), "Character: ~w, Age: ~w", [Name, Age]),
    answer(Msg).
```

#### `task(+Description, -Var1, -Var2, -Var3)`

Bind up to three variables.

> **Note — `task/N` is load-time sugar.** `task/1,2,3,4` and `prompt/2,3,4` are
> rewritten by the DML loader into `task_named/3` / `prompt_named/3` with typed
> output variables. The rewrite runs whenever code is loaded (`deepclause run`,
> the REPL `-l` and commit paths, `consult`, and the REPL `assert`/`assert_many`
> tools). The untransformed form has no runtime handler,
> so code that somehow reaches the runtime without the rewrite raises
> `domain_error(unrewritten_task_form, Goal)` instead of failing silently.
> `task/1`/`prompt/1` are handled directly and are unaffected.

> **Internal note.** At load time `task/N`/`prompt/N` are rewritten to
> `task_named/3`/`prompt_named/3` (which carry the output-variable names) and
> `{Var}` descriptions are hoisted into a `format/3`. That is why a description
> like `task("Answer: {Question}", Answer)` sometimes *appears* as
> `format(string(C), "Answer: ~w", [Question]), task_named(C, [Answer], ...)`.
> `listing` and `export` reverse this, so you always read/write `task/N` and
> `prompt/N` — never author `task_named/3` yourself.

### Type-Safe Output Variables

You can wrap output variables with type specifiers to enforce strict validation on the LLM's output. The runtime will ensure the value matches the specified type.

| Wrapper | Description | Zod Equivalent |
|---------|-------------|----------------|
| `string(Var)` | Default string type | `z.string()` |
| `integer(Var)` | Integer number | `z.number().int()` |
| `number(Var)` | Any number (float or int) | `z.number()` |
| `float(Var)` | Alias for number | `z.number()` |
| `boolean(Var)` | Boolean value | `z.boolean()` |
| `list(Type(Var))` | Array of specified type | `z.array(Type)` |
| `object(Var)` | Dictionary/Object | `z.record(z.unknown())` |

**Examples:**

```prolog
% Enforce integer result
task("Calculate 10 + 5", integer(Result)).

% Enforce boolean result
task("Is this true?", boolean(IsTrue)).

% Enforce list of strings
task("List 3 colors", list(string(Colors))).

% Enforce object result
task("Extract user details", object(User)).
```

**Note:** The LLM during `task()` can only call tools defined in DML (see [Tool Definitions](#tool-definitions)). Registered TypeScript tools are not automatically available - this gives you control over what the LLM can do.

---

### `exec(+ToolCall, -Output)`

Execute a registered TypeScript tool directly and capture its output.

```prolog
% Assuming 'calculator' tool is registered in TypeScript
agent_main :-
    exec(calculator(expression: "15 * 23"), Result),
    format(string(Msg), "Calculation result: ~w", [Result]),
    answer(Msg).
```

Tool arguments use `key: value` syntax:

```prolog
exec(fetch_url(url: "https://example.com"), Response)
```

Or positional if the tool supports it:

```prolog
exec(get_time, CurrentTime)
```

The `bash` tool accepts an optional wall-clock `timeoutMs` and always returns the
wall-clock duration as `timeMs`:

```prolog
exec(bash(command: "sleep 30", timeoutMs: 5000), R),
get_dict(timeMs, R, Took),
(   get_dict(exitCode, R, 124)
->  format(string(Msg), "timed out after ~w ms", [Took])
;   format(string(Msg), "finished in ~w ms", [Took])
).
```

A command is also killed when it produces no output for `DC_HOST_SHELL_IDLE_TIMEOUT_MS`
(default 180 000 ms); `DC_HOST_SHELL_TIMEOUT_MS` sets a default wall-clock cap
(0 = disabled). Exit code `124` means a timeout.

---

### Tool Definitions

`tool/2` is an **annotation**, not an implementation. It exposes an existing
harness predicate to the nested agent that runs inside `task/N`. Define the
predicate normally, then add a `tool/2` clause that delegates to it:

```prolog
% 1. The capability: a normal predicate, callable from `execute` and `/goal`.
count_words(File, Count) :-
    script_file(count_words, Script),
    format(atom(Cmd), "python3 '~w' '~w'", [Script, File]),
    exec(bash(command: Cmd), Result),
    get_dict(stdout, Result, Count).

% 2. The annotation: exposes it to the nested agent. Thin body only.
tool(count_words(File, Count), "Count words in a workspace file.") :-
    count_words(File, Count).
```

The head and its delegate must have the **same name and the same arity**. The
tool body is what runs when the nested agent calls the tool, so a body that
calls a different arity either calls nothing (`existence_error`) or calls the
tool again and recurses forever:

```prolog
% WRONG: declares /2 but calls /3, so the tool fails when a nested agent uses it.
tool(bibliography(Query, Max), "…") :-
    bibliography(Query, Max, Entries).

% WRONG: calls itself; a nested agent recurses until the budget is gone.
tool(citations(Text, Citations), "…") :-
    citations(Text, Citations).
```

Nothing is checked when the clause is loaded, so both mistakes look fine until a
nested agent calls the tool. `end_turn` reports them as contract warnings; treat
them as errors, not style notes.

A predicate defined *only* inside a `tool/2` clause is invisible to
`execute` / `/goal` — the loader asserts it as `tool(Head)`, never as `Head`. Split
the two so one implementation is reachable from both surfaces:

```prolog
/goal count_words("report.txt", C).          % authoring agent
task("Count the words in report.txt.", N).   % nested agent calls the tool
```

The only tools a nested agent can call are the harness's `tool/2` predicates;
registered host tools (for example `bash`) are reachable from DML code through
`exec/2`, not as tools by name. So a task description that says "use the
`fs_read` tool" works only if the module itself defines `fs_read/2` **and**
annotates it.

The tool head's **last argument is the output**; the description is shown to the
nested agent. Keep the `tool/2` body a single delegation — put no logic in it.

Logic lives in the predicate, not the annotation:

```prolog
safe_divide(A, B, Result) :-
    (   B =:= 0
    ->  Result = error("Division by zero")
    ;   Result is A / B
    ).

tool(safe_divide(A, B, Result), "Safely divide two numbers.") :-
    safe_divide(A, B, Result).
```

#### Using `task()` Inside Tools

Tools can use `task()` and `prompt()` internally to combine Prolog logic with LLM reasoning:

```prolog
% A predicate that computes then explains.
% The nested task is told to work from the given figures — it has no tools it
% needs, so it cannot try to re-enter this predicate.
explain_calculation(A, B, Explanation) :-
    Sum is A + B,  % Prolog computation
    format(string(Desc),
           "Explain ~w + ~w = ~w to a child, using only these figures.",
           [A, B, Sum]),
    task(Desc, Explanation).

tool(explain_calculation(A, B, Explanation), "Calculate and explain the result.") :-
    explain_calculation(A, B, Explanation).
```

**Do not wrap an LLM step in a `tool/2` that advertises the same job.** The tool
is declared to the agent running *inside* that same `task/N`, so the description
invites the agent to do the very thing it was asked, by calling the tool:

```prolog
% WARNING: this shape self-delegates.
extract_authors(Text, Authors) :-
    task("Extract every author and their affiliation(s) from: {Text}", list(object(Authors))).

tool(extract_authors(Text, Authors), "Extract every author and their affiliation(s).") :-
    extract_authors(Text, Authors).
```

Called as a predicate (`/goal extract_authors(…, A)`, `execute`, or from another
predicate), `extract_authors` runs its `task/N`, and that agent is offered
`extract_authors` — a tool described as doing exactly its job. The runtime refuses
the re-entrant call (a tool whose predicate is already running is withheld, and a
call that slips through is refused with a reason), so it cannot recurse forever.
But the agent still spends a turn on it, is told the tool failed, and then does
the work by hand — a confusing transcript and a wasted call.

Two ways to avoid it, in order of preference:

**Keep the LLM step a skill, and give `tool/2` only to capabilities.** A skill is
called from DML (`/goal`, `execute`, another predicate) and does not need to be a
tool. Annotate the operations a nested agent should *use* — deterministic or
side-effecting work such as reading a PDF, searching an index, or shelling out:

```prolog
% Skill: an LLM step. Call it from DML; no tool/2.
extract_authors(Text, Authors) :-
    task("Extract every author and their affiliation(s) from: {Text}", list(object(Authors))).

% Tools: capabilities for nested agents. No task/N, so no self-reference.
read_pdf(Path, Text) :-
    script_file(pdf_to_text, Script),
    format(atom(Cmd), "python3 '~w' '~w'", [Script, Path]),
    exec(bash(command: Cmd), R), get_dict(stdout, R, Text).

tool(read_pdf(Path, Text), "Convert a workspace PDF to text.") :- read_pdf(Path, Text).
```

**Or scope the nested task's tools so the tool cannot be reached.** If a `task/N`
inside a predicate must not use a tool, say so with `without_tools/2` (or list
exactly what it may use with `with_tools/2`):

```prolog
extract_authors(Text, Authors) :-
    without_tools([extract_authors],
        task("Extract every author and their affiliation(s) from: {Text}",
             list(object(Authors)))).
```

Scoping is the narrower fix: it keeps the tool advertised to *other* agents (which
is usually why it exists) while removing it from this one.

**Memory Isolation:** When `task()` or `prompt()` runs inside a tool, the nested agent starts with **fresh memory**. It does NOT inherit the parent's accumulated memory from `system()`, `user()`, or previous `task()` calls. If you need context in a nested task, pass it explicitly as a tool argument or add it with `system()` inside the tool body.

**Tool Access:** A nested agent's only callable tools are the harness's `tool/2`
predicates; host tools such as `bash` are reachable from DML code via `exec/2`,
never by name from a task description. A tool that is **already executing** is
withheld from the agent inside it, and a re-entrant call is refused, so a tool
cannot recurse into itself.

That exclusion is keyed on *tool* execution, which has a consequence worth knowing:
a predicate that is also a tool does **not** count as "executing" when it is called
as a predicate (`/goal`, `execute`, or from another predicate). So for

```prolog
extract_authors(Text, Authors) :- task("…", list(object(Authors))).
tool(extract_authors(Text, Authors), "…") :- extract_authors(Text, Authors).
```

`/goal extract_authors(Text, A)` still offers `extract_authors` to the agent inside
`task/N`. Use `with_tools/2` / `without_tools/2` to scope it, or keep the LLM step
out of the tool set entirely (see **Using `task()` Inside Tools** above).

```prolog
% Example: passing context explicitly.
smart_grep(BuildContext, Pattern, Summary) :-
    system(BuildContext),            % add context to the nested task's memory
    format(atom(Cmd), "grep -rn '~w' .", [Pattern]),
    exec(bash(command: Cmd), Results),
    format(string(Desc), "Summarize these matches: ~w", [Results]),
    task(Desc, Summary).

tool(smart_grep(BuildContext, Pattern, Summary), "Grep with context.") :-
    smart_grep(BuildContext, Pattern, Summary).
```

---

### Tool Scoping

You can control which tools are available to nested `task()` calls using `with_tools/2` and `without_tools/2`.

#### `with_tools(+ToolList, +Goal)`

Run Goal with only the specified tools available to nested tasks. `ToolList`
holds tool *names* (atoms), not predicates:

```prolog
% The implementation defines what the tool does; the annotation exposes it.
restricted_work(Input, Output) :-
    with_tools([search_papers, read_paper], (
        format(string(Desc), "Process '~w' using the available tools", [Input]),
        task(Desc, Output)
    )).

tool(restricted_task(Input, Output), "Runs with limited tools") :-
    restricted_work(Input, Output).
```

#### `without_tools(+ToolList, +Goal)`

Run Goal with the specified tools excluded from nested tasks:

```prolog
cheap_work(Input, Output) :-
    without_tools([expensive_tool], (
        format(string(Desc), "Process '~w' cheaply", [Input]),
        task(Desc, Output)
    )).

tool(cheap_task(Input, Output), "Runs without expensive tools") :-
    cheap_work(Input, Output).
```

#### Using Tool Scoping in `agent_main`

You can also use `with_tools/2` and `without_tools/2` directly in `agent_main`:

```prolog
agent_main :-
    system("You are a safe assistant."),
    
    % Run task with only safe tools
    with_tools([search, fetch_page], (
        task("Analyze the files in the workspace.", Analysis)
    )),
    
    output(Analysis),
    answer("Done").
```

**Automatic call stack exclusion:** When a tool calls `task()`, the calling tool is automatically excluded from the nested agent's available tools to prevent infinite recursion. This happens in addition to any manual scoping via `with_tools/2` or `without_tools/2`.

---

### Model Selection

#### `with_model(+Spec, +Goal)`

Run `Goal` with a different model — and optionally a reasoning level — for every
`task/N`, `prompt/N` and `sample_token/N` call inside it. Judgments
(`choose/rate/verify/probability`) are not affected.

```prolog
% a cheap model for classification
with_model("openai:gpt-4o-mini",
    prompt("Classify as billing/orders/account: {Message}", Team)).

% a strong model with high reasoning, only for the hard step
with_model("anthropic:claude-sonnet-4#high",
    task("Review this diff and list defects:\n{Diff}", Notes)).

% full control with a dict
with_model(model{provider: "google", id: "gemini-2.5-pro",
                 temperature: 0.2, maxTokens: 8192}, Goal).
```

`Spec` is a string — `"provider:model"`, `"provider/model"`, `"model"`, each with
an optional `#reasoning` suffix (`off|minimal|low|medium|high|xhigh|max`) — or a
`model{provider, id, temperature, maxTokens, reasoning}` dict. The innermost
`with_model` wins, and a nested `with_reasoning/2` overrides the spec's reasoning.
The default is the session's active model. An unknown or unsigned-in model fails
with a clear error.

---

### Memory Predicates

Memory predicates build the conversation context sent to the LLM.

#### `system(+Text)`

Add a system message (instructions for the LLM):

```prolog
agent_main :-
    system("You are a helpful cooking assistant."),
    system("Always provide measurements in metric units."),
    task("Suggest a recipe for pasta."),
    answer("Done").
```

#### `user(+Text)`

Add a user message to the context:

```prolog
agent_main :-
    system("You are a helpful assistant."),
    user("I want to learn about machine learning."),
    task("Explain the basics of machine learning."),
    answer("Explained!").
```

---

### Media Predicates

Images are **workspace files**; DML carries only a reference. See
`docs/DML_MEDIA.md`.

#### `read_media(+Path, -Media)`

Bind `Media = media(Path, Mime)` for a workspace-relative image (PNG, JPEG, GIF,
WebP). The bytes are not read into Prolog.

#### `attach_media(+Media)`

Append the image to conversation memory (reference only), so the next `task/N`
call sends it to the model as an image block:

```prolog
agent_main :-
    read_media("photos/fox.png", Img),
    attach_media(Img),
    task("Describe the image in one sentence.", Description),
    answer(Description).
```

Memory stores `media{path, mime}` — never the image bytes. Judgments
(`choose`/`rate`/`verify`/`probability`) stay text-only.

---

### File I/O and paths — prefer the shell

**Prefer `exec(bash(...))` for file operations.**
Reading, writing, copying, or converting files is usually clearer and more
robust as a shell command; reach for the DML predicates below only when the
content must be bound and reasoned over inside Prolog. Pulling a file into DML
just to hand it back to the shell is a smell (`DML_AUTHORING.md` §2).

If you do use them: every path in DML is **workspace-relative** and confined to
the workspace. The
engine mounts the workspace at `/workspace`, but you must not write
`/workspace/...` in DML — passing an absolute path is rejected, as is `..`. File
predicates all take a relative path:

```prolog
read_file("images/a.png", Bytes),
write_file("out/report.txt", Text),
make_directory_path("out/nested"),          % create a directory (and parents)
append_file("out/log.txt", Line),
delete_file("out/report.txt"),
directory_files("images", Names),        % bare entry names: ["a.png", ...]
exists_file("images/a.png"),
size_file("images/a.png", Size),
time_file("images/a.png", Modified).
```

`write_file/2` does **not** create parent directories — call
`make_directory_path/1` first. `directory_files/2` returns bare entry names (no
directory), so build a path with `atom_concat('images/', Name, Path)` when you
need one. `bash` runs with a wider view of the sandbox than DML has; convert
anything from the shell to a workspace-relative path before handing it to DML.

These predicates are **real clauses in the session module**, not only
interpreter handlers, so DML code, a nested `task/N`, and a direct host query
(the SDK's `runDML`, or `deepclause repl`) all resolve the same definition and
the same workspace-relative path. `read_file/2` in particular has no other
definition, which is why it once worked from DML and failed from a host query.

`open/3`, `close/1`, `read_string/3` and `write/2` are the exception. They are SWI
builtins and cannot be shadowed with a workspace-resolving version, so they stay
**stream-only** and take an absolute path. Use `read_file/2` and `write_file/2`
for whole-file I/O — they resolve the relative path and close their own streams.

---

### Output Predicates

#### `output(+Text)`

Emit an output event to the caller. Does NOT affect the LLM conversation. Useful for progress updates:

```prolog
agent_main :-
    output("Starting analysis..."),
    task("Analyze the data"),
    output("Analysis complete!"),
    answer("Done").
```

#### `yield(+Text)`

Alias for `output/1`.

#### `log(+Text)`

Emit a log event. Similar to output but typed as 'log' for filtering:

```prolog
agent_main :-
    log("Debug: entering main"),
    task("Process request"),
    log("Debug: task completed"),
    answer("Done").
```

---

### `answer(+Text)`

Emit the final answer and **commit** - no backtracking occurs after `answer/1`:

```prolog
agent_main :-
    task("Generate a poem"),
    answer("Poem complete!").  % Execution stops here
```

**Important:** `answer/1` commits the execution. After answering, the program completes and does not try alternative clauses.

---

## Control Flow

DML supports standard Prolog control flow:

### Conjunction (and)
```prolog
goal1, goal2, goal3
```

### Disjunction (or)
```prolog
(goal1 ; goal2)
```

### If-Then-Else
```prolog
(Condition -> Then ; Else)
```

### If-Then (soft cut)
```prolog
(Condition -> Then)
```

### Negation as Failure
```prolog
\+ goal
```

### Cut
```prolog
!
```

### Catch/Throw
```prolog
catch(Goal, Error, Recovery)
throw(Error)
```

---

## String Interpolation

Text arguments support `{variable}` interpolation with bound variables:

```prolog
agent_main(Topic) :-
    system("You are a research assistant."),
    task("Research the topic: {Topic}"),  % Topic is interpolated
    answer("Research on {Topic} complete!").
```

---

## Backtracking

DML supports full Prolog backtracking, **including across LLM calls**. This enables powerful retry logic:

### Multiple Clauses
```prolog
% Try first approach
agent_main :-
    output("Trying approach 1..."),
    task("Solve using method A"),
    fail.  % Force failure to try next clause

% Fallback approach
agent_main :-
    output("Trying approach 2..."),
    task("Solve using method B"),
    answer("Solved!").
```

### State Restoration

When backtracking occurs, the memory state is automatically restored:

```prolog
agent_main :-
    system("Context A"),  % This is in memory
    output("Trying path A"),
    fail.  % Memory is rolled back!

agent_main :-
    system("Context B"),  % Fresh memory
    output("Trying path B"),
    answer("Done").
```

### Disjunction Backtracking

```prolog
agent_main :-
    (
        task("Try difficult approach"),
        fail  % Backtrack
    ;
        task("Try easier approach")
    ),
    answer("Completed").
```

---

## Full Prolog Support

DML is built on SWI-Prolog, so you have access to the full Prolog language — and
to the constraint libraries (`clpfd`, `clpq`, `clpr`). Many questions are best
answered by a plain Prolog or CLP goal run with `execute`, with no model call at
all: arithmetic, counting, ordering, membership, `findall/3`, `aggregate_all/3`,
list/set operations, and constraint or optimisation problems. Reach for `task/N`
only when the step genuinely needs language understanding.

### Lists
```prolog
process_items([]).
process_items([H|T]) :-
    format(string(Msg), "Processing: ~w", [H]),
    output(Msg),
    process_items(T).

agent_main :-
    process_items([apple, banana, cherry]),
    answer("All items processed").
```

Not all builtin SWI Prolog predicates are guaranteed to work at this stage!

### Arithmetic
```prolog
agent_main :-
    X is 10 + 5 * 2,
    format(string(Msg), "Result: ~w", [X]),
    answer(Msg).
```

### String Formatting
```prolog
agent_main :-
    format(string(S), "Hello ~w, you are ~d years old", [alice, 30]),
    answer(S).
```

### Findall
```prolog
color(red).
color(green).
color(blue).

agent_main :-
    findall(C, color(C), Colors),
    format(string(Msg), "Colors: ~w", [Colors]),
    answer(Msg).
```

### Bounding a goal (inference limit)

The interpreter has **no wall-clock timeout for a single builtin call** — the
session `gas` budget counts meta-interpreter steps, and a runaway `findall/3`,
`aggregate_all/3`, or `label/1` runs inside one step. Wrap such a call in
`call_with_inference_limit/3` (core SWI-Prolog; available in this runtime, which
has no threads) to bound it by inference count:

```prolog
bounded_findall(Goal, Xs) :-
    call_with_inference_limit(findall(X, call(Goal, X), Xs), 1000000, Result),
    (   Result == inference_limit_exceeded
    ->  throw(error(inference_limit_exceeded, context(bounded_findall/2, Goal)))
    ;   true
    ).
```

`Result` is `inference_limit_exceeded` when the limit is hit (otherwise the
normal call result). Use it for `findall/3`, `aggregate_all/3`, and CLP
`label/1` over a large or under-specified domain:

```prolog
sudoku_solve(Puzzle, Solution) :-
    ...
    call_with_inference_limit(label(Solution), 500000, Result),
    (   Result == inference_limit_exceeded
    ->  throw(error(inference_limit_exceeded, context(sudoku_solve/2, label)))
    ;   true
    ).
```

System libraries imported with `:- use_module(library(...)).` (e.g. `clpfd`) are
also visible inside meta-calls, so `label/1`, `all_distinct/1`, and `ins/2` work
unqualified through the wrapper. If a library predicate is somehow not found
inside a wrapper (`existence_error`), qualify it: `clpfd:label(Xs)`.

For shell commands, pass `exec(bash(command: Cmd, timeoutMs: N), R)` instead.

### Assert/Retract (within session)
```prolog
agent_main :-
    assertz(fact(42)),
    fact(X),
    format(string(Msg), "Stored fact: ~w", [X]),
    answer(Msg).
```

### Constraint Logic Programming (CLP) — advanced, use sparingly

Use CLP only for a **genuine constraint problem** (scheduling, assignment,
puzzles, exact search). It is the wrong tool for ordinary arithmetic and
transformations — prefer a `task/N`/`prompt/N` step when the numbers or the
operation are open-ended (`DML_AUTHORING.md` §0). CLP also regresses easily: an
under-specified model, or one that is simply **unsatisfiable**, can search
essentially forever, so a buggy skill hangs the turn instead of failing fast.
If you use it:

- **Bound every domain first.** For one variable use `X in 0..100`; for a list use
  `[X,Y,Z] ins 0..100`. (`X ins 0..100` on a bare variable is an
  `instantiation_error` — use `in` for a single variable.)
- `all_distinct/1` / `all_different/1` over the list constrains it pairwise.
- Call `label/1` (or `labeling/2` with options such as `ff`) only when you need
  concrete values; wrap it in `once/1` when a single witness is enough.
- To bound a search that can run away, wrap it in `call_with_inference_limit/3`
  (see *Bounding a goal* above):
  `call_with_inference_limit(label(Xs), 500000, R)` — `R == inference_limit_exceeded`
  when it gave up. This works for CLP(FD) predicates in this runtime.
- Keep the model small; a big model is a sign the step belongs in a `task/N`.

- **CLP(FD)**: finite domains (integers). `:- use_module(library(clpfd)).`
- **CLP(Q)**: rationals. `:- use_module(library(clpq)).`
- **CLP(R)**: reals. `:- use_module(library(clpr)).`

```prolog
:- use_module(library(clpfd)).

% Small, fully bounded search; one witness.
solve_sum_product(Total, X, Y) :-
    [X, Y] ins 0..100,      % bound the domains; never leave them open
    X + Y #= Total,
    X * Y #= 2400,
    once(label([X, Y])).

% A search bounded by inference count instead of by the domain.
bounded_label(Xs, Result) :-
    call_with_inference_limit(label(Xs), 500000, Result).
```

#### Optimisation (LP / MIP)

The CLP family here is **CLP(FD)** (integers), **CLP(Q)** (exact rationals) and
**CLP(R)** (reals) — there is no `clpfq`. `library(simplex)` loads but returns
unreliable answers in this runtime (its minimum/maximum come back wrong); do not
build on it.

**Continuous LP** — `clpr` (reals) / `clpq` (rationals), same interface. `{...}`
posts the constraints; `maximize/1` / `minimize/1` find the optimum:

```prolog
:- use_module(library(clpr)).

lp(X, Y) :-
    {X >= 0, Y >= 0, X =< 10, Y =< 10, X + 2*Y =< 14},
    maximize(X + Y).               % X = 10.0, Y = 2.0
```

> **Caveat:** `maximize/1` / `minimize/1` bind the variables only when the optimum
> is a *unique vertex*. If the optimum is a face (a family of optima), they succeed
> and bind nothing. Use `sup/2` / `inf/2` to get the optimum **value** reliably —
> `{X >= 0, Y >= 0, X + Y =< 10, 2*X + Y =< 15}, sup(X + Y, S)` → `S = 10.0`.
> (`inf/3` / `sup/3` do not exist in this build.)

**Integer optimum** — negate the objective and use `bb_inf/5`; the 4th argument
comes back as the integer vertex:

```prolog
:- use_module(library(clpr)).

mip(Inf, Vertex) :-
    {X >= 0, Y >= 0, X + Y =< 4, X + 3*Y =< 6},
    bb_inf([X, Y], -(3*X + 2*Y), Inf, Vertex, 0.001).
    % Inf = -12.0, Vertex = [4,0]     (negate the objective to maximise)
```

**Exact integer optimum** — CLP(FD) with a `max(Expr)` label option. Prefer this
when the problem is genuinely integer: it is exact, bounded, and well-tested.
Bound every domain first and keep `label`/`labeling` inside
`call_with_inference_limit/3`:

```prolog
:- use_module(library(clpfd)).

exact_max(X, Y) :-
    [X, Y] ins 0..10,
    X + Y #=< 4,
    X + 3*Y #=< 6,
    call_with_inference_limit(labeling([max(3*X + 2*Y), down], [X, Y]), 500000, _).
    % X = 4, Y = 0
```

### Dictionaries (SWI-Prolog)
```prolog
agent_main :-
    D = point{x: 10, y: 20},
    X = D.x,
    format(string(Msg), "X coordinate: ~w", [X]),
    answer(Msg).
```

---

## Complete Example

```prolog
% research_assistant.dml
% A multi-step research assistant

% Helper to log progress
step(N, Desc) :-
    format(string(Msg), "Step ~w: ~w", [N, Desc]),
    output(Msg).

% Main entry point; the topic is an argument.
agent_main(Topic) :-
    % Set up the assistant
    system("You are a thorough research assistant."),
    system("Provide detailed, accurate information."),
    
    % Step 1: Overview
    step(1, "Getting overview"),
    format(string(Task1), "Provide a brief overview of ~w", [Topic]),
    task(Task1),
    
    % Step 2: Key concepts
    step(2, "Identifying key concepts"),
    task("List the 3 most important concepts related to this topic. Store them in Concepts.", Concepts),
    
    % Step 3: Deep dive
    step(3, "Deep dive analysis"),
    format(string(Task3), "Explain these concepts in detail: ~w", [Concepts]),
    task(Task3),
    
    % Complete
    format(string(FinalMsg), "Research on '~w' complete!", [Topic]),
    answer(FinalMsg).
```

Run with:
```typescript
for await (const event of dc.runDML(code, { 
  args: ['quantum computing'] 
})) {
  console.log(event.type, event.content);
}
```

---

## Semantic Judgment Predicates

Semantic judgments answer bounded, typed questions about an explicit state.
They are not generative: a `choose` picks one of your options, a `rate` picks
one of your levels, and a `verify` returns a probability / yes-no answer. They
never read or write DML memory (`system/1`, `user/1`, `task/N`), and one `judge/2`
call sends every question in a single request.

### `judge(+State, +Specs)`

Each element of `Specs` is `Question - Output`. The output variable is bound
when the batch completes.

```prolog
agent_main(Message) :-
    judge(Message, [
        choose("Which team should handle this?",
               [billing-"Charges and refunds", orders-"Delivery and returns",
                account-"Login and security"]) - Team,
        rate("How frustrated is the customer?", [calm, frustrated, angry]) - Frustration,
        verify("Does the message ask for a refund?") - Refund,
        probability("Is this urgent?") - Urgency
    ]),
    (   Refund == yes
    ->  answer("Escalating to the refund queue.")
    ;   format(string(R), "Routing to ~w (frustration ~w, urgency ~w)", [Team, Frustration, Urgency]),
        answer(R)
    ).
```

Question forms:

- `choose(Instruction, Options)` - options are atoms or `Option-"description"`.
- `rate(Instruction, Levels)` - an ordered list of level descriptions.
- `verify(Instruction)` / `verify(Instruction, criteria(True, False))`.
- `probability(Instruction)`.

### One-off predicates

Each is a single request for a single judgment:

```prolog
choose(State, Question, Options, Choice)
rate(State, Question, Levels, Level)
verify(State, Question, Truth)          % yes | no | unknown
probability(State, Question, P)         % number
holds(State, Question)                  % semidet: succeeds when verify == yes
holds(State, Question, Threshold)       % semidet: probability >= Threshold
```

### Backends and capabilities

Judgments are backend-agnostic. Backends are registered on the SDK
(`createDeepClause({ judgeBackends, defaultJudge })`) and can be selected per
run or per scope:

```prolog
with_judgment(jev, choose("state", "Which team?", [billing, orders], Team)).
require_judgment(calibrated, verify("state", "Is this urgent?", Truth)).
```

> **In the DML REPL**, an `llm` backend is always registered (it reuses the
> active model), and `jev` is registered when `judgment.jev.enabled` is set in
> `.deepclause/config.json` and its API key is available. `with_judgment(jev,
> ...)` fails if `jev` is not registered.

`require_judgment/2` fails the wrapped goal before it runs when the active
backend does not provide the capability, so a skill that depends on calibrated
probabilities cannot silently run on an uncalibrated backend. Available
capabilities include `calibrated`, `probability`, `confidence`, and
`independentQuestions`.

Observations are memoized per run by backend, model, state, and questions, so
backtracking reuses an answer instead of re-querying the model.

### Notes

- `verify` is three-valued: `yes`, `no`, or `unknown`. Never read `\+` as
  "the model said no"; use the returned truth value.
- Values are constrained to the options/levels you supply. A `choose` answer is
  always one of your option atoms.
- Prefer one `judge/2` batch over several one-off calls when you need more than
  one or two judgments.
## The DML REPL (`deepclause repl`)

DML skills can also be grown interactively in a persistent **living module**.

- `/goal <dml>` runs a goal and enumerates its solutions (the legacy dotted
  `goal: <dml>.` form still works).
- Plain English builds the module: the **authoring agent** inspects and edits it
  through tools (`listing`, `begin_turn`, `define`/`assert`/`assert_many`,
  `retract`/`delete`, `execute`, `end_turn`/`rollback`). Edits are transactions:
  `begin_turn` is required before any edit, `end_turn` commits a version.
  Nothing is auto-run; the agent uses `execute` to test during a turn.
- There is **no implicit `agent_main` requirement** in the REPL. Run any
  predicate with `/goal <name>(…)`. Define `agent_main` in the module when it
  should also run with `deepclause run` / `/dc-run`.
- `freeze.` makes the module read-only; `export <file>.` serializes it.
  `export harness.zip` bundles the module with its linked script files;
  `export harness.dml` inlines them into one portable file.
- `input/2` / `exec(ask_user(...))` (module) ask the human. They never block in
  non-interactive runs (`-g`, piped stdin, CI).

**Self-containment.** The harness is the `.dml` **plus the files it links**.
Materialize each program with a DML goal (`make_directory_path(...)`,
`write_file(...)`), link it with `script_file(Name, Path)`, and run it with
`exec(bash(...))`. The
exporter bundles the linked files, so they travel with the harness. Pass
untrusted data through files or stdin, never interpolated into the shell
command.

See [`docs/REPL.md`](../../../../docs/REPL.md) for the full guide.
