# DML authoring guide (REPL)

How to think about a REPL harness. For code, see `HARNESS_EXAMPLES.md` (idiomatic
harness shapes) and `PROLOG_SNIPPETS.md` (tested building blocks).

## 0. What a harness is

A harness is a **living library of reusable DML skills** for the workspace —
not a single program. A skill is a predicate; name them descriptively and run
them with `execute` (during a turn) or `/goal`. Skills are built from:

- **helper predicates** (plain Prolog/CLP),
- **semantic judgments** (`choose`/`rate`/`verify`/`probability`),
- **host tools** via `exec/2`, and
- **model calls** via `task/N` — a bounded agentic turn that may itself call
  other skills.

The module is **data = code**: small/medium structured data belongs in it as
facts (`country(ch, "Switzerland").`), inspected and queried exactly like
predicates (`listing`, `/goal`, `execute`). Add data as facts when it is small
enough to reason over; keep large datasets in a file or a linked script rather
than bloating the module.

A harness may also carry **test predicates** that pin the intended behavior of a
skill — for example `test_<name>`, or a small `check/2` runner over
input/expected pairs. When you change a skill, update or add its test and run it
so the change is verified, not assumed.

So a harness evolves like a codebase through **composition and reuse**: prefer
an existing predicate over a near-duplicate. Change it **only when the user
asks** — create on a fresh workspace, and afterwards touch only what the request
requires; never refactor or extend it on your own initiative.

**Generalize, don't specialize.** Prefer a few small, composable primitives over
a predicate per request, and express one-off requests by composing them (or a
direct `/goal`) instead of naming a predicate for every phrasing. A hard-coded
"latest image in `~/Downloads`" is wrong; even `find_latest(Dir, Pattern, Path)`
is fairly narrow — the reusable parts (list a directory, filter, pick a
best/latest element) are shared by many tasks. Ask whether another caller would
plausibly reuse the predicate with different arguments; if only this request
would, do not add it. A harness of many task-shaped helpers is as bad as one
hard-coded script.

**Declare the shape of structured model output.** `list(object(B))` does not say
what is in the dicts, so every consumer re-derives defaults and tolerates missing
fields by hand. `schema(Name, [field(Field, Type, Option)])` is checked when the
result binds: defaults filled, a missing `required` field or a wrong type reported
against the field name, an undeclared field kept so a mistake stays visible. Name it
in the output — `task(…, list(schema(author, Authors)))` — or apply it by hand with
`schema_fill(author, Raw, Author)`.

**Know which conversation the model sees.** `task/N` is sent the memory built so
far and writes its reply back, so consecutive `task/N` calls accumulate history —
pass values as explicit arguments instead of relying on the model to remember them.
`prompt/N` is fresh (no memory, no tools) and is the right choice for a classifier,
extractor, or transformation that must not be swayed by the conversation;
`with_fresh_memory(Goal)` does the same for a group of steps, and restores the
caller's memory on exit. A `task/N` inside a `tool/2` body starts empty: give it the
context as an argument.

**Start small, then improve.** Write the smallest harness that answers the actual
request end to end — often a single predicate — run it on a real input, and show
the user the result *before* extending it. Say what it does, what it does not do
yet, and what you would add next, then let the next request decide. A harness grows
in the direction the user actually takes it.

Adding capability speculatively is the expensive mistake, and it costs more here
than in a normal codebase: every predicate is part of the harness the next turn has
to reason about, and every helper is one more thing the model can be offered as a
tool. Do not write an error taxonomy before an error has occurred, retry and
fallback policies before something has failed, caching or configuration layers
before there is a reason, or a predicate per case for cases you have not seen.
Prefer the smallest change that makes the failing input work.

Signs you are overengineering:

- a predicate with one caller whose arguments are all literals from the request;
- an abstraction (a schema, an options list, a dispatch table) built for a single
  shape or a single value;
- a helper "framework" written before the first end-to-end run;
- generalizing across cases that no one has asked for yet;
- a harness that does not run yet, because the parts are still being made tidy.

A small change is also easier to trust: each commit is one reviewable unit, the
`end_turn` probes report on a small surface, and `rollback` stays cheap. A harness
that is incomplete but working is a better answer than one that is complete and
unverified.

**Do not parse in DML — shell out or ask the model.** Prolog string handling is
the fiddliest thing here, and a hand-rolled parser (`sub_string/5`,
`split_string/4`, `atom_concat/3`, `format/2`, an accumulator) is the easiest way
to write DML that looks right and is quietly wrong. Split the work by what it is.
Mechanical work — pull a field, change a format, count, filter — belongs in a
**helper script** (`script_file/2` + `exec(bash(argv: [...]))`), which is testable
outside DML and written in a language made for text; have it emit **JSON** and read
it with `json_get/3`/`json_gets/3`/`json_text/2` instead of splitting stdout.
Work that needs *understanding* — find the field named X, classify, summarise —
belongs in a **model call** with a typed output (`task("…", list(string(Items)))`,
`schema/2`, `choose`/`rate`/`verify`). DML sequences and checks; it does not parse.

**Prefer orchestration over algorithms.** A skill should read as a short
sequence of steps. Describe each step with `task/N` (which sees the conversation
memory) or `prompt/N` (fresh context) and let the model extract, classify,
summarise, or decide; keep the Prolog between the steps to simple guards
(`->`, `;`, `once/1`, `==`, `\==`), `member/2`, `between/3`, `format/2`, and
small arithmetic. Reach for deep recursion, hand-rolled list algorithms, CLP, or
`assert`/`retract` only when the task is genuinely algorithmic — and prefer a
one-shot `execute` goal over committing it as a skill. DML is the orchestration
layer, not the logic library.

## 1. Skills vs one-shot work

A harness is a library of reusable **capabilities**, not a record of what you
did. Most requests mix two things, and you must separate them:

- **A reusable capability** — worth a named, documented, tested predicate. It
  takes its inputs as arguments and works for any caller (`describe_image(Path,
  Text)`, `tally(List, Counts)`).
- **A one-off action** — the concrete step for *this* request. Run it now with
  an `execute` goal or `bash`; do not commit it as a predicate.

Example — "find the latest image in `~/Downloads` and tell me what's on it":
run the "find/copy the latest file" step once in the turn, and add only
`describe_image(Path, Text)` as the skill. Finding a latest file, copying files
around, moving data, or any single concrete invocation is a task, not a skill.

Never bake environment-specific values into a predicate: no host paths
(`/home/...`), machine-specific directories, or fixed filenames — a skill takes
them as arguments. A predicate that exists only to be called once, with literals
frozen in, is a script in disguise.

## 2. Only use tools from the catalog

The HOST TOOL CATALOG above is the complete set of `exec(...)` capability tools.
Do not invent names — there is no `calculator`, `fetch_url`, `web_search`, or
`vm_exec` unless the catalog lists it. By default the only capability tool is
**`bash`**.

**Prefer the shell for file I/O.** Use `bash` for reading, writing, copying, and
converting files. The DML file predicates (`read_file/2`, `open/3`,
`read_string/3`, `write_file/2`, `make_directory_path/1`, `directory_files/2`, …)
are for when the content must be bound and reasoned over **inside Prolog** — not
for moving bytes around. Reading a file into DML just to shell it out again is a
smell.

## 3. Scripts are files, linked from the harness

Materialize a script by running a DML goal with the `execute` tool — for example
`make_directory_path("scripts"), write_file("scripts/x.py", "…")` — then link it
with `script_file/2` and run it with `bash`. `write_file/2` writes to the
workspace immediately and does **not** create parent directories.
`script_file/2` is just a fact: it tells `export` which files belong to the
harness. Use a literal workspace-relative path and one fact per name. `export harness.zip` bundles the
`.dml` plus every linked file; `export harness.dml` inlines them so the single
file stays portable.

Untrusted data (URLs, file contents, user text) goes through a file or `stdin`,
never interpolated into the shell command string. See `HARNESS_EXAMPLES.md` §1.

A linked script is for a genuinely external capability (a library, `ffmpeg`, an
API) — not a substitute for a shell command you run once, and not one script per
request. When a script is the capability, it is a component: parameterize it and
document it with `doc/2`.

## 4. Annotate the harness (self-describing)

Keep the harness self-describing. `module_doc/1` is the harness's **skill
description** — the one line that says what the harness does and when to use it,
the same role a skill's `description` plays for routing. Write it like
frontmatter, not like a title:

```prolog
module_doc("Support triage: classify a customer message and route it to the right team. Use when a message needs a team.").

doc(classify/2, "Route a support message to a team. classify(+Message, -Team). Example: /goal classify(\"I was charged twice\", T).").
doc(tally/2, "Count occurrences of each item. tally(+List, -Counts) yields Key-N pairs. Example: tally([a,b,a], C).").
```

- `module_doc(Text)` — the skill description. It is the **first line of the
  manifest** (`- skill: ...`), so **read it at the start of the session and
  re-read it whenever the harness changes**. A file with only `module_doc/1` and
  no predicates is still a valid skill: it advertises the capability while the
  body is empty.
- `tool(Head, "desc") :- Body.` — **an annotation, not an implementation**: it
  exposes an existing predicate to the nested agent in `task/N`. Define the
  predicate normally and delegate
  (`tool(count_words(File, Count), "…") :- count_words(File, Count).`), so the
  same predicate is callable from `execute` / `/goal` **and** from `task/N`. Logic
  placed only inside `tool/2` is unreachable from `execute` (the loader asserts
  `tool(Head)`, never `Head`).
  - Annotate **capabilities** (file I/O, search, a script) that a nested agent
    should use — not the LLM steps themselves. A `tool/2` whose implementation
    calls `task/N` and advertises the same job is offered to the agent inside that
    `task/N`, which then calls it. Absent the tool call stack, wrapper predicates
    are the same job twice; keep the step a skill, or scope the task with
    `without_tools([Head], task(…))`.
- `doc(Name/Arity, Text)` — one fact per skill, helper, or tool; terse, with
  arguments (`+in` / `-out`) and an example goal.

They are shown in the manifest (so the next turn sees them), in `listing`, and
survive `export`. When you change a predicate, update its `doc/2`; when the
harness's purpose changes, update `module_doc/1`.

## 5. Media (images)

Images are **workspace files**, referenced by `media(Path, Mime)`; the bytes
never enter Prolog or conversation memory. Read a reference with `read_media/2`
and put it in context with `attach_media/1`; the next `task/N` sends it to the
model as an image block:

```prolog
describe_image(Path, Text) :-
    read_media(Path, Image),
    attach_media(Image),
    task("Describe the attached image in one sentence.", Text).
```

Only PNG/JPEG/GIF/WebP are accepted. This needs a vision-capable model; keep a
textual description in memory when it does not. Semantic judgments stay
text-only — never attach media to them.

## 6. Guard rails

- Read first: call `listing`.
- **Paths: use `/workspace/...`.** The workspace is the working directory, so
  `out/report.txt` and `/workspace/out/report.txt` are the same file — but write
  the `/workspace` form, because it means the same thing from DML, from
  `exec(bash(...))`, and from the shell tool on every backend. Everything outside
  `/workspace` is the session's ephemeral VM filesystem: fine for scratch, never
  for a result, because **only files under `/workspace` reach the host**. Scratch
  goes in `$TMPDIR` (per-session in every backend), not a bare `/tmp`.
  `directory_files/2` returns **bare names**, so build one
  (`atom_concat('images/', Name, Path)`). Prefer `bash`/host file tools for file
  operations; use the DML file predicates only for content you must reason over
  in Prolog.
- **Side-effecting skills must be deterministic.** Any skill that calls
  `task/N`, `prompt/N`, `exec/2`, `read_media/2`, `attach_media/1`, or writes a
  file must yield exactly one solution with no choicepoint left across the side
  effect (`once/1`, `->`, or a cut). Otherwise backtracking re-runs the model
  call: slow, costly, and a different answer each time.
- Replace a predicate with `define(head, clauses)` (or `retract` then
  `assert_many`). `assert` appends, so asserting a clause twice duplicates the
  predicate; never patch it clause by clause.
- Clause order matters: base case before a recursive rule; define a whole
  predicate with `assert_many` (clauses in the order written).
- Verify after changing a predicate: `listing(Name/Arity)`.
- Bounded iteration only: finite recursion or an explicit bound. `execute` is
  gas-limited and the turn has a model-call budget, but a single builtin call
  (e.g. an unbounded `label/1`) is not interrupted.
- **CLP is advanced — use it only for a real constraint problem.** Bound every
  domain (`X ins 0..N`) before `label/1`, prefer `once/1`, and keep the model
  small: an under-specified or unsatisfiable model searches forever, and there
  is no per-goal timeout. Never reach for CLP as a shortcut for arithmetic.
- **Know whether you are sandboxed.** The session's environment block says which
  backend is in effect. With a sandbox (`bwrap`, AgentVM, `sandbox-exec`),
  `bash` is isolated and writes outside the workspace are confined. With
  `clean-room` there is **no** sandbox: commands run as the user, writes outside
  the workspace are real files that persist, and a `rm` is a real `rm`. Explore
  what is available (`python3 --version`, `pip list`, `which jq`) and install a
  dependency when a skill genuinely needs one; installs are sandbox/session-local
  and do not travel with an exported harness, so prefer the standard library.
- If a harness genuinely requires isolation, the **user** can add
  `:- require_sandbox.` (through `/set-require-sandbox`). It makes the load fail
  rather than run unsandboxed. Do not add it yourself.
- Do **not** use `agent_main` unless the user asks for a `deepclause run` /
  `/dc-run` skill.
- Media: reference images with `media(Path, Mime)` (`read_media/2` +
  `attach_media/1`); never inline bytes.
