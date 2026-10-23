# Authoring a living harness

Start small, then improve. Use the smallest end-to-end design that satisfies the
request, verify it on real input and show the user. Reuse existing predicates;
do not build speculative abstractions, duplicate helpers or a predicate per one-off
request. A harness is reusable capabilities, not a log of everything performed.

## Design and inspect

Call `listing(filter: "all")` first, then relevant `Name/Arity` entries.
Clarify input, output, execution surface and evidence of correctness. If ambiguous,
ask one question before editing; once settled, do not reopen the same decision.
Normal conversational agents use `shell/1` plus tools/workflows, not a custom loop.
Use explicit argument modes and typed model outputs from the first small version.

Simple exact logic stays in DML; substantial parsing/file algorithms belong in
scripts; semantic work uses typed prompt/task calls. Choose by actual need, not
by a blanket ban on Prolog or a blanket preference for models. Read `patterns`
for a matching small shape and `examples` for a complete tested one.

## Transactions

`begin_turn` opens a snapshot; call it once, not before every define. `define`
and `delete` are refused without an open transaction. `end_turn` validates and
commits a version; `rollback` discards the open edit. After committing, open a
new transaction before editing again. Check per-call results and end_turn warnings.
`committed: false` or no changed predicates means the intended edit may not have
landed. Verify `listing` rather than asserting it worked.

Change the module only through those tools. Do not consult/load a workspace DML
file to bypass edits. A system library directive is different from importing a
user DML file. The user can load a harness as a separate action.

## Define targets

`define(head, clauses)` adds a NEW or replaces EXISTING complete predicate block.
Head is an indicator `"foo/2"` or actual head `"foo(A, B)"`, not a function name
whose arity you hope the tool infers. The block must define the target and may
include related predicates/annotations. All clauses end with their own period.
Preserve ordering; replacing a recursive predicate requires all its clauses.

For keyed annotations, use the actual two-argument head:
`define("doc(foo/2, _)", "doc(foo/2, \"...\").")`.
`doc(foo/2)` would name doc/1 and is rejected. `doc/2`, `schema/2`, `script_file/2`
merge by key so other entries survive. Changing foo/3 to foo/2 creates a different
predicate: delete the old one/doc key and define the new one in one transaction.

## Delete targets

- `delete("foo/2")`: whole predicate and its associated nested tool annotation.
- `delete("tool:foo/2")`: nested tool annotation, keeping delegate.
- `delete("doc:foo/2")`: one documentation entry.
- `delete("schema:person")`: one schema key.
- `delete("script_file:extract_py")`: one script link, not the file.

Missing annotation keys are errors. No assertion/retraction tools are offered.
Use define with the complete remaining clause set to remove one clause.
Script link names must differ from all user predicate names; prefer `_py` or
`script_` link names, not the predicate they implement.

## Documentation and scripts

`module_doc(Text)` describes purpose/when to use the harness. `doc(Name/Arity, Text)`
records inputs/outputs and an example goal; count every actual predicate argument.
`script_file(Name, Path)` links a workspace file for export. Write a script once
with the authoring write_file tool, then link/call it. Do not repeatedly overwrite
helper source every time a workflow runs. Unlinked files are not promised portable.
Do not hardcode user-specific host paths or secrets in code/facts.

## Test and hand over

A small pure validation test, actual bounded input, error case and cancellation
case where relevant are enough for the first version. Side effects must be
intentional and single-solution. Debug intermediate values with output/1 rather
than adding more model steps. Foreground form workflows cannot be exercised in
browser scratch execute; ask for a foreground run and test pure rules separately.

End_turn warnings are contract evidence, not a reason to add bogus alias predicates
to satisfy stale docs. Correct the annotation or arity. Clean up fixtures and test
processes you created, but never delete unrelated user files. Summarize what was
observed, what is still unverified and how to run it.
