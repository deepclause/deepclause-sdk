# DML basics

DML is Prolog with host-interpreted operations for models, tools, forms and
processes. A harness is a library; `agent_main/N` is optional for positional runs.
Read these contracts when syntax or failure behavior is uncertain.

## Terms and arguments

Variables start uppercase or `_`; lowercase words are atoms, and double quotes
are strings. `'two words'` is a quoted atom, not a string. `true` and `false`
are atoms. A bare `_` is a fresh anonymous variable on every occurrence.
Lists are `[a,b,c]` or `[Head|Tail]`; dicts are `_{name: "Ada", age: 36}` with
ATOM keys. Read controlled keys with `get_dict(name, D, X)`, varying model keys
with `dict_field(D, name, X)`.

Predicates are relations, not functions. `greet("Ada", Greeting)` supplies an
input and leaves the output variable to be bound. `greet/2` counts BOTH
arguments. `greet(+Name, -Text)` is documentation for intended modes.
Do not call `Text = greet("Ada")` expecting evaluation.

## Clauses and goals

A fact states a relation; a rule gives a body that must succeed:

```prolog
team(billing).
team(orders).
greet(Name, Text) :- format(string(Text), "Hello ~w", [Name]).
```

Every fact/rule ends with a period, including `doc/2`, `schema/2`, `script_file/2`
and `shell/1`. Newlines alone do not separate clauses. `%` starts a line comment.
A query such as `greet("Ada", X)` runs the body and returns `X = "Hello Ada"`.
A predicate may have several clauses; their written order matters.

## Unification and arithmetic

`X = Y` unifies terms, binding variables if possible. `X = 1 + 2` binds a TERM,
not 3. `X is 1 + 2` evaluates arithmetic; inputs must be bound numbers.
`A =:= B` compares evaluated numbers. `A == B` tests identical terms without
binding; `A \== B` tests non-identity now. `dif(A, B)` constrains them to remain
different. A string `"yes"` and atom `yes` are not identical.

Use `number_string/2` or `atom_string/2` deliberately at a type boundary; do not
sprinkle conversions blindly. Model type wrappers bind actual numbers/booleans.

## Control flow

`A, B` is conjunction, `A ; B` disjunction. Parenthesize branching:

```prolog
size_label(Text, Label) :-
    string_length(Text, N),
    ( N > 100 -> Label = long ; Label = short ).
```

`\+ Goal` means the goal cannot be proved under current bindings. It is not
semantic truth or a model judgment. `catch(Goal, E, Recovery)` catches EXCEPTIONS,
not ordinary failure. `throw(Error)` raises an exception.

## Failure and determinism

A false guard or missing relation yields zero solutions; it may not throw.
`findall(X, Goal, Xs)` yields `[]` for no solutions, while `setof`/`bagof` fail.
`get_dict` fails for a missing key and throws for an invalid key/dict type.

Pure relations can enumerate intentionally. `once(Goal)` chooses the first
solution, `!` discards local alternatives, and `(C -> T ; E)` commits the
condition. Select a deterministic input BEFORE a model/file/network effect.
An earlier choicepoint can replay a later effect on backtracking. A trailing
cut only commits paths that reached it. Do not retry irreversible actions by
backtracking; use explicit, bounded, safe retry policies.

## Values and output

A reusable skill, nested tool or shell workflow binds its output arguments and
returns. The caller composes those values.
`output(Text)` emits progress/display without adding model memory.
`answer(Text)` emits the final one-shot answer and terminates the goal; it is NOT
`end_turn` and does not assign workflow outputs. Use it at a run entry point,
not in a helper whose caller must continue.

`system/1`/`user/1` add DML harness memory, not authoring conversation.
See `reference(topic: "tasks", section: "memory")` before relying on context.

## Standard glue

Use `maplist/3` for sequential mapping; it passes each item plus an output to a
predicate. Use `include/3`, `exclude/3`, `memberchk/2`, `findall/3`, `sort/2`,
`msort/2`, `sum_list/2`, JSON helpers and `format/3` for small exact operations.
`sort/2` removes duplicates; `msort/2` keeps them. `directory_files/2` gives bare
entry names. Do not hand-build a large parser/list algorithm when a script fits.
`reference(topic: "snippets")` has precise short recipes.
