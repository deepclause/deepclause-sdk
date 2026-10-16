# Idiomatic harness examples

Copy-paste shapes for REPL harnesses. Keep each skill small and compose them.
(The snippets are tested against the bundled SWI-Prolog.)

Most skills should look like §2–§3: a short sequence of `task/N`/`prompt/N` calls
with thin guards between them. §4–§7 (facts, pure logic, judgments, CLP) are the
exception — reach for them only when the task is genuinely data- or
logic-shaped, not as the default style. DML is the orchestration layer over model
calls, not the logic library (`DML_AUTHORING.md` §0). For file work, shell out
(`exec(bash(...))`) instead of reading files into DML (§2).

Every skill carries a `doc(Name/Arity, Text)` annotation and the harness a
`module_doc(Text)` line (`DML_AUTHORING.md` §4). `module_doc/1` is the harness's
**skill description** — what it does and when to use it; it is the first line of
the manifest, which the agent reads at session start and whenever it changes. A
file with only `module_doc/1` is still a valid skill (an advertised capability
with an empty body). Keep both terse.

```prolog
module_doc("Example harness: a few small skills used as patterns. Use when you need a copy-paste starting point.").
```

## 1. Wrap a workspace script as a tool

Materialize the program as a workspace file (run a DML goal with
`make_directory_path("scripts"), write_file("scripts/count_words.py", "…")`),
link it with `script_file/2`, and define a predicate for it. `tool/2` is only an
annotation that exposes an existing predicate to `task/N`; keep it a one-line
delegation so the predicate is also callable from `execute` / `/goal`:

```prolog
script_file(count_words, ".dc-repl/scripts/count_words.py").

% The capability: callable from `execute` / `/goal` and from `task/N`.
count_words(File, Count) :-
    script_file(count_words, Script),
    format(atom(Cmd), "python3 '~w' '~w'", [Script, File]),
    exec(bash(command: Cmd), Result),
    get_dict(stdout, Result, Count).

% The annotation exposing it to the nested agent (delegation only).
tool(count_words(File, Count), "Count words in a workspace file.") :-
    count_words(File, Count).
```

Untrusted data goes through files/stdin, never interpolated into the shell
command. `script_file/2` is just a fact (the exporter bundles the file); the
write lands on disk immediately, so `bash` can run the script in the same turn.

## 2. A skill that calls the model

`task/N` runs a bounded agentic turn. Name each output variable in the
description so the model fills it:

```prolog
doc(classify/2, "Route a support message. classify(+Message, -Team). Example: /goal classify(\"I was charged twice\", T).").

classify(Message, Team) :-
    task("Route this support message to exactly one of billing, orders, or account. Message: {Message}. Store the choice in Team.", Team).

demo :-
    classify("I was charged twice for the same order", Team),
    format(string(Out), "Routed to ~w", [Team]),
    answer(Out).
```

## 3. Multi-step pipeline: task -> task -> answer

```prolog
report(Topic, Result) :-
    task("List three risks of using an LLM for arithmetic. Store them in Risks.", Risks),
    format(string(Desc), "Rank these risks and explain the top one: ~w", [Risks]),
    task(Desc, Top),
    format(string(Result), "Top risk: ~w", [Top]).
```

Compose the steps and let the model do the thinking; keep only thin guards
between them — an if-then-else for a branch, `once/1` to commit the first
answer, or a simple check that fails a step:

```prolog
summarise(Doc, Summary) :-
    task("Extract the document's main claim. Store it in Claim.", Claim),
    (   Claim == ""
    ->  Summary = "no claim found"
    ;   task("Summarise this claim in one sentence: {Claim}", Summary)
    ).
```

## 4. Data as facts (data = code)

Small/medium data belongs in the module and is queried exactly like code:

```prolog
parent(alice, bob).
parent(alice, carol).
parent(bob, dave).
parent(carol, eve).

doc(ancestor/2, "Transitive parent. ancestor(+A, -B) enumerates descendants. Example: /goal ancestor(alice, X).").

ancestor(A, B) :- parent(A, B).
ancestor(A, B) :- parent(A, C), ancestor(C, B).
```

```text
/goal ancestor(alice, X).
X = bob ; X = carol ; X = dave ; X = eve
```

## 5. Pure logic + a test predicate

```prolog
doc(tally/2, "Count occurrences of each item. tally(+List, -Counts) yields Key-N pairs. Example: /goal tally([a,b,a,c,a], C).").

tally(List, Counts) :-
    list_to_set(List, Items),
    findall(Item-N, (member(Item, Items), aggregate_all(count, member(Item, List), N)), Counts).

test_tally :-
    tally([a,b,a,c,a], C),
    C == [a-3, b-1, c-1].
```

`/goal test_tally.` succeeds when the behavior is correct. When a skill changes,
update or add its test and run it (`execute` during a turn, or `/goal`).

## 6. Semantic judgments (the model decides, Prolog enforces)

```prolog
route(Message, Team) :-
    choose(Message, "Which team should handle this?", [billing, orders, account], Team).

urgent(Message) :-
    holds(Message, "Is this urgent?").
```

`choose/4` always returns one of your options; `verify/3` is three-valued
(`yes`/`no`/`unknown`); `holds/2` is the semidet wrapper.

## 7. Constraints with CLP(FD)

```prolog
:- use_module(library(clpfd)).

doc(solve/3, "X+Y=Total with X<Y over 0..100. solve(+Total, -X, -Y). Example: /goal solve(10, X, Y).").

solve(Total, X, Y) :-
    [X, Y] ins 0..100,
    X + Y #= Total,
    X #< Y,
    label([X, Y]).
```

The `:- use_module(...)` directive must be **top-level** (an inline
`use_module` inside a body is not run at read time, so `#=`/`ins` are unknown).

## 8. Vision: describe a workspace image

```prolog
doc(describe_image/2, "Describe an image file. describe_image(+Path, -Text). Example: /goal describe_image(\"photos/fox.png\", T).").

describe_image(Path, Text) :-
    read_media(Path, Image),         % media(Path, Mime) — no bytes
    attach_media(Image),             % reference-only; sent by the next task/N
    task("Describe the attached image in one sentence.", Text).
```

Images are files; `media(Path, Mime)` is the reference. The bytes are read from
the workspace at call time, only PNG/JPEG/GIF/WebP are accepted, and they never
enter clauses or memory. Judgments stay text-only.

## 9. Forms: structured input from the user (opt-in)

Use a form when the program needs **typed, validated** input rather than free
text. Forms are off by default. The directive goes at the top of a **file** — the
capability is processed at load, so it cannot be added through the edit tools:

```prolog
:- capability(forms).

doc(collect_claim/0, "Collect an expense claim and file it. Example: /goal collect_claim.").

collect_claim :-
    Spec = _{ version: 1, title: "New expense claim",
              fields: [ _{name: vendor, type: string,  title: "Vendor", required: true},
                        _{name: amount, type: integer, title: "Amount", required: true, minimum: 0},
                        _{name: notes,  type: string,  title: "Notes",  widget: textarea} ],
              actions: [ _{name: submit, title: "Submit", kind: primary},
                         _{name: cancel, title: "Cancel", kind: link} ] },
    form(Spec, F),
    claim_loop(F).

%% The host refuses a submit the rules reject, so `invalid` is the only way in
%% here — never act on values the rules did not accept.
claim_loop(F) :-
    await_form(F, Change),
    (   Change.action == invalid
    ->  form_errors(F, Change.errors),        % show them on the form
        claim_loop(F)
    ;   Change.action == cancel
    ->  close_form(F), answer(cancelled)
    ;   close_form(F),
        get_dict(values, Change, Values),
        format(string(Msg), "filed: ~w", [Values]),
        answer(Msg)
    ).

%% Domain rules on top of the spec's own constraints; both block a submit.
form_validate(Values, Errors) :-
    findall(error(amount, "the amount looks implausible"),
            ( get_dict(amount, Values, A), A > 100000 ),
            Errors).
```

`Change.action` is an **atom** (`submit`, `cancel`, `invalid`) — compare with
`==`, never against `"submit"`. `current_predicate(form/2)` reports `no` even
when forms work (it is a handler, like `exec/2`); check
`reference("capabilities")`. The loop above is deterministic — no model. To let
the **model** choose the form, drive the same primitives from `task/N` with
`with_tools([render_form, wait_for_form_change, set_form_issues, advisories,
close_form, set_result, finish], task(…))`; either way the host validates. See
`reference("forms")` and `sdk-examples/forms.dml`.
