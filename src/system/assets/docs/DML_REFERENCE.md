# DML language API

DML is Prolog plus model, host, process, media and human-interaction handlers.
A harness is a library; it needs no agent_main unless used as a positional run.
Use focused topics (basics/tasks/shell/python/processes/media/forms) before an
unfamiliar feature. Topic-only calls give indexes; request a section id and follow
returned offsets. Runtime catalog/capabilities override generic examples.

## Program structure

Facts and rules use Prolog syntax and end with periods. Predicates are identified
by name/arity (ALL inputs and outputs). `agent_main/N` is an OPTIONAL positional
run entry point; all its arguments are inputs. Libraries and explicit workflow
or goal calls can name any predicate. Normal Work conversation uses shell/1.
`doc(Name/Arity, Text)` describes modes and an example; `module_doc(Text)` describes
purpose; `schema/2` defines model data shapes; `script_file/2` links bundled files.
Use distinct script link/predicate names, e.g. extract_py versus extract/2.

## Model operations

`task(Description, Outputs...)`: bounded model agent with DML harness memory and
scoped nested tools. Typed wrappers are load-time sugar, preserving variable names.
The loader supports task/1..5 and prompt/1..4; do not author internal task_named
or prompt_named forms. The no-output form is for work whose output is not consumed.

`prompt(Description, Outputs...)`: fresh model step, no harness tools, no memory
read/write. `with_fresh_memory(Goal)` isolates a group of accumulating tasks.
`llm(Messages, Reply)` accepts explicit messages (or supported text input), returns
raw text and leaves harness memory unchanged. Prefer typed prompt/task normally.
`chat(Message[, Reply])` is for a custom ongoing DML conversation, not a default
Work shell. Full memory/tool/type contracts are in `tasks`.

Output wrappers: string, integer, number/float, boolean, list(Type), object,
schema(Name, Var), list(schema(Name, Vars)). Bare variables default to string.
`media(Var)` asks an image backend to generate an image path; it is not ordinary
text/vision and fails if no image backend is configured.

## Memory and scopes

`system(Text)` and `user(Text)` add backtrackable DML memory messages.
`get_memory(Messages)` binds current DML memory; it is not the authoring transcript.
`clear_memory` empties it. `push_context`/`pop_context` save/restore;
`push_context(clear)` also starts empty. Use with_fresh_memory for normal scopes.
A nested task in a tool starts fresh; explicit arguments carry context. Processes
have independent memory. A fresh prompt does not see attached images.

`with_tools([Name,...], Goal)` limits nested task tools; `without_tools` excludes
names. `with_model(Spec, Goal)` uses a different available model for scoped calls;
string specs use provider:model and optional #reasoning, or a model dict.
`with_reasoning(Goal, Level)` (GOAL FIRST) scopes supported reasoning effort.
`with_recipe(Goal, Query)` adds transient recipe context when the embedding has
configured a recipe library; it is not persistent memory. These scopes do not
change judgment backend; see judgments section for with_judgment/require_judgment.
Never invent a model id or infer credentials. Use current models/catalog.

## Sampling

`sample_token(Prompt, Token)` requests a fresh single-token text result;
`sample_token(Prompt, AllowedStrings, Token)` constrains the allowed frontier.
An out-of-set result is normalized/falls back to the first allowed entry; do not
use that fallback as evidence that an external condition was verified.
This is for a grammar/label frontier, not a replacement for typed task outputs.
`with_model` applies to it. For a semantic bounded choice prefer choose/4.

## Host calls and scripts

`exec(Tool(key: Value,...), Result)` invokes a CURRENT registered host capability.
Do not copy an arbitrary tool name from another embedding. Read topic tools for
actual schemas; topic python is the browser Python contract.

`run_script(Name, Args, Value)` resolves script_file(Name, Path), stringifies atomic
arguments, checks exitCode and parses JSON stdout (else returns stdout).
Nonzero exit throws script_failed. Define/script link names must not overlap:

```prolog
script_file(count_py, "scripts/count.py").
count_words(File, Count) :- run_script(count_py, [File], Stats), get_dict(words, Stats, Count).
```

`Args` is a list. A script should print one JSON result with newline; diagnostics
belong on stderr. Export bundles LINKED files. A successful host call returning an
error/nonzero dict is not necessarily success of the requested operation.

## Nested tools

Define a real predicate and a same-name/same-arity delegating annotation:

```prolog
double(N, R) :- R is N * 2.
tool(double(N, R), "Double a number.", [number]) :- double(N, R).
```

Last argument is the ONLY output, earlier ones are inputs. The optional type list
labels inputs; missing types default to string. Multi-field results go in one dict.
Do not call tool/N as an ordinary DML goal; the loader registers the annotation.
Scope each tool-using task. Avoid an LLM-backed step advertising itself as the
same tool to its own nested task. Work shell tools are a separate explicit
interface with inputs/outputs; see shell topic.

## Output and entry results

`output(Text)` emits progress without model-memory changes; `yield/1` aliases it.
`log(Text)` emits a diagnostic event. `answer(Text)` emits a final one-shot answer
and terminates execution via the answer commit; it does NOT commit code edits.
Workflow/helper/tool outputs must be bound arguments and normal return, not answer.

`output(media(Path))`, `output(media{svg: Svg})` display images; `save_image/2`
saves a reference. Browser `output(html(Path))` renders a workspace miniapp.
Read media for distinctions between display, vision and generation.

## Data and schemas

`schema(Name, Fields)` declares fields with `field(Name, Type, required)` or
`default(Value)`; types include primitives, list(Type), schema(Name) and nested
schema(Name, _). `schema_fill(Name, Raw, Filled)` fills defaults and checks types.
Task schema outputs fail if required/wrong-typed fields are invalid. Extra fields
are retained, not silently dropped.
Dict keys are atoms. `get_dict(Key, Dict, Value)` is strict; use dict_field/3,4
for model-varying keys and optional defaults.

JSON helpers: `json_get(D, Path, V)`, `json_get(D, Path, Default, V)`,
`json_gets(D, "rows.*.name", Values)`, `json_put(D, KeyPath, V, Updated)`,
`json_text(D, Text)`, `atom_json_dict(Text, D, [])` in either direction.
Missing json_get path fails; explicit-default arity supplies a default.

## Strings and interpolation

The loader interpolates bound `{Variable}` in supported DML text positions such
as task/prompt descriptions and output/answer. For arbitrary strings use
`format(string(S), "~w ~d ~n", [Term, Number])`; ~q quotes a read-back-safe term,
~s writes text, ~w writes values. Strings escape newline/quotes/backslashes.
`pattern(Template, Text, Bindings)` builds/parses small anchored templates;
$name slots use `name=Value` pairs, $name:int converts numbers, $$ is a dollar.
Do not build a large custom parser where a script fits.

## Grammars and parameters

DCGs (`-->`) are expanded by the DML loader. A grammar needs a list input;
`library(dcg/basics)` normally expects character CODES. Do not hand a Prolog
string to phrase/2 and assume it is a code list.

```prolog
:- use_module(library(dcg/basics)).
number_value(N) --> integer(N).
parse_number(Text, N) :- string_codes(Text, Codes), phrase(number_value(N), Codes).
```

For session inputs/configuration, `param(Name, Value)` reads declared parameter
facts; `:- param(Name, Value).` is a supported load directive. Positional run
inputs are normally predicate arguments. Never invent undeclared free variables
or use a parameter name as assignment syntax. Read current host configuration
before relying on optional limits, recipe libraries or resume behavior.

## Control and deterministic glue

Conjunction `A,B`, alternatives `(A;B)`, committed `(C->T;E)`, negation `\+ G`,
unification `=`, arithmetic `is`/`=:=`, identity `==`, disequality dif/2,
`once/1`, cut `!`, `catch/3` and `throw/1` are supported. Read basics for modes.
A cut commits its clause and the goals to its left, and is scoped by `\+`,
`call/1`, `findall/3` and if-then-else — but only in a *pure* predicate (one that
calls no host/model predicate). A clause that also calls `task/N`, `exec/2`,
`output/1`, forms or processes is interpreted, so its cut is ignored there: use
`once/1` or `(Cond -> Then ; Else)` instead.
Backtracking restores logical bindings/memory; it does NOT undo host/file actions.
Select one intended effectful path. Pure relations may intentionally enumerate.

Lists/library glue: maplist, include, exclude, foldl, member/memberchk, findall,
aggregate_all, sum_list, sort/msort, keysort, nth0/3, append, reverse. Availability
of a particular library builtin should be checked with a pure bounded query;
handlers such as form/exec do not appear as ordinary current_predicate clauses.
See snippets for focused recipes. Do not use LLMs for exact arithmetic/counting.

## Files and persistence

The runtime environment states workspace paths and isolation. Browser DML/Python
share /workspace; private tmp filesystems differ. DML write_file/2 needs existing
parent directories; the authoring write_file TOOL creates parents. read_file/2,
write_file/2, append_file/2, directory_files/2, exists_file/1, exists_directory/1,
size_file/2 and time_file/2 are common helpers. directory_files returns bare names.
Use scripts for substantial byte/file operations. Do not assume native shell
binaries, symlink policy or host paths from another environment.
Authoring commits, runtime session facts, process snapshots and workspace records
have different lifetimes. Explicitly choose/test storage; a runtime assert is not
an authoring end_turn. Never bake secrets into facts/history/exported scripts.

## Forms and human input

No forms activation directive is required, but the execution host must support
interaction. Foreground Work/query/run paths can render forms; browser scratch
execute and worker-hosted processes cannot. input(Prompt, Answer) asks a question
where a host is attached. input(Message) waits for the next conversational turn;
next(Item) dispatches user(Text)/message(Term) for a custom event-driven loop.

Fixed form lifecycle: form(Spec, F), await_form(F, Change), close_form(F).
Change.action is an atom such as submit/cancel/unavailable; values is a dict.
Hard validation is host-enforced; rejected submits stay waiting. Never treat
cancel/unavailable as saved values. Read forms before implementing this path.

## Processes

spawn/2, spawn_opt/3, serve/2,3, send/2, receive/1,2, receive_match/3,
self/1, register/2, whereis/2, service_call/3,4, service_cast/2,
wait/1,2, exit/1,2, process_info/2, sleep/1 and make_ref/1 are process APIs.
Copies are by value: child output variables never bind parent variables.
Use bounded messages/wait outcomes and stop/reap test workers.
swarm/3,4 returns ordered ok(Value)/failed per independent item; pmap/3 returns
bare values, failing if any worker fails. Default concurrency is 4; bound it.
Use processes topic for exact message envelopes, snapshot and filesystem rules.
`suspend(Continuation)` needs a supported resume host and closed streams/forms;
suspend only at a safe point between calls, never inside a model/host wait.

## Judgments

`choose(State, Question, Options, Choice)`, `rate(State, Question, Levels, Level)`,
`verify(State, Question, Truth)`, `probability(State, Question, P)`, holds/2,3.
`judge(State, Specs)` batches specs such as choose(Question, Options)-Choice.
Choose returns a supplied atom; verify is yes/no/unknown. Supply real evidence;
judgments do not see task memory/images and are not a guarantee of external state.
`with_judgment(Backend, Goal)` selects a configured backend;
`require_judgment(Capability, Goal)` requires calibrated/probability/etc support.
`with_model` does not select the judgment backend. Inspect actual registration.

## Constraint search

Use CLP only for a genuine small bounded search. Library directives must be
TOP LEVEL so operators exist at parse time. Use clpfd for finite integers,
clpq for rationals, clpr for reals. There is no clpfq. Avoid relying on simplex
in this build. Bound domains BEFORE labeling, and bound inference separately.

```prolog
:- use_module(library(clpfd)).
solve(Total, X, Y) :-
    [X,Y] ins 0..20, X + Y #= Total, X #< Y,
    once(label([X,Y])).
```

A single builtin may not yield to the interpreter's gas counter;
call_with_inference_limit(Goal, Bound, Result) can report inference_limit_exceeded.
A scratch execute worker has wall-time limits but foreground goals differ.
Do not replace exact search with invented model output if the constraint model
fails. Read the available SWI API before advanced optimization options.

## Editing and directives

Authoring tools: listing, begin_turn, define, delete, end_turn, rollback, execute,
reference and available file/media/process tools. Define/delete are the only
predicate edit operations; each requires a transaction. Keyed annotation delete
forms are doc:Name/Arity, schema:Name and script_file:Name; tool:Name removes the
nested annotation only. An arity change needs deleting the old predicate/doc key.

System library imports use `:- use_module(library(...)).` Unknown directives are
reported ignored. consult/load on user DML are user load actions, not authoring
edits; do not bypass tracked transactions. The user controls sandbox requirements.
No compiler translates Markdown to DML. Author valid DML and test it directly.
