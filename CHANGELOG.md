# Changelog

## Unreleased

### Added

- **`pattern/3` — one template that builds a string or parses it.** `format/3` had
  no inverse; `pattern("Name: $name, age: $age", Text, [name="Ada"])` renders, and
  the same template with `Text` bound parses to `[name="Ada", age="36"]`. Slots are
  `$name` (so a template can carry JSON verbatim and cannot collide with the
  `{Var}` interpolation in `task/N`), `$name:int` converts, a slot consumes up to
  the next literal, and an unanchored `"$a$b"` is rejected rather than guessed.
- **`schema/2` — declared shapes for structured model output.**
  `task(…, list(schema(author, Authors)))` fills defaults, reports a missing
  `required` field or a wrong-typed field against the field name, and keeps an
  undeclared field visible instead of dropping it.
- **JSON paths:** `json_get/3,4` (`"results.0.authors.1.name"` or a list),
  `json_gets/3` (`*` collects and flattens), `json_put/4`, `json_text/2`. A missing
  path fails, never throws and never yields `""`.
- **`with_fresh_memory/1`** scopes a group of steps to an empty conversation that
  accumulates within the scope and is discarded on exit — `prompt/N` for several
  calls. `prompt/N` itself was undocumented and now has a section.
- **`exec(bash(argv: [...]))`** takes the command and arguments as a list and quotes
  every element, so an argument containing a space, a quote, `$` or `;` is inert.
- **`dict_field/3,4`** reads a model-produced dict field with an atom or string key,
  any case, ignoring `_`/`-`/space.

### Fixed

- **`--harness` was inert.** The REPL action handler never forwarded it, so the
  harness always resolved to `main` and no named slot was ever created.
- **A tool that failed by returning an error rendered as a tick.** Only
  `success: false` counted as a failure, so the runtime's own "already running
  further up the call chain" refusal — and any `{ error: "…" }` — was reported as a
  completed call. Re-entrant tool calls are now refused at dispatch and rendered as
  failures with the reason.
- **`:- repl_delete(a/1).` silently did nothing**, and so did every other
  unrecognised directive: the loader's directive catch-all discarded them. The
  directive now removes the predicate, and an unknown directive is reported after
  the load.
- **`doc(Name/Arity, …)` with no clauses is reported at `end_turn`.** Such a
  predicate fails silently when called and its name still appears in the manifest.
- **Tool contract violations are reported at `end_turn`** — a `tool/2` whose body
  calls a different arity, calls itself, or names a predicate that does not exist
  failed only inside a nested agent, where it burned the model-call budget.
- **`read_file/2` had no definition**, so it worked from DML and raised
  `existence_error` from a host query. The workspace file predicates are real
  session clauses now, reachable from DML and from a host query alike. Host queries
  run in the session module. `open/3`, `close/1`, `read_string/3` and `write/2`
  cannot be shadowed and stay stream-only.
- **Tool output printed the last three lines of its captured output** — for JSON,
  the one part with no information. The panel now reports the size and kind and
  shows the head (stderr's tail), and a non-zero exit appears in the header.
- **The docs taught self-delegating tools** and never mentioned that `doc/2` creates
  nothing, that `prompt/N` exists, or that `atom_json_dict/3` writes JSON and DCGs
  work — but `phrase/2` needs `string_chars/2` first.


## 0.1.1 - 2026-10-12

### Fixed

- **`execute` no longer floods on a nondeterministic goal.** It stops enumerating
  at the solution limit instead of streaming every solution (a runaway CLP search
  previously wrote tens of thousands of events and hung the turn).
- **The pi adapter no longer forwards an ambient temperature** — pi itself does
  not, and some models (e.g. github-copilot) reject the parameter. A `with_model`
  temperature is forwarded only when a scope sets one *and* the model does not
  opt out via `compat.supportsTemperature`.
- **The model-call budget is interactive.** When the authoring budget is
  exhausted the agent asks whether to continue (naming `repl.maxModelCalls` /
  `--max-model-calls`; `0` = unlimited) instead of aborting silently.
- **Media mime is inferred from the extension.** `/show` and the agent's image
  tools no longer misread an SVG as a PNG.

- **Harness-scoped history facts.** `harness_change`/`harness_changed`/
  `harness_head`/`harness_rollback` now live in `harnesses/<name>/history.dml`
  (one per harness); session/turn facts stay in the workspace `history.dml`.
- **Reasoning now actually streams.** The pi adapter passed the thinking level
  as `options.thinkingLevel`, but pi's `SimpleStreamOptions` enables thinking
  with `options.reasoning` (`ThinkingLevel`). The key was ignored, so providers
  never emitted thinking deltas; reasoning models now stream reasoning.

### Changed

- **Docs + README refreshed.** Added a worked example session (create a skill
  transaction → inspect/rollback → export → load as a new harness → multiple
  harnesses per project), and updated the history layout/arity references.
- **Authoring agent tool set trimmed.** `ask_user` and the loop-level `finish`
  are no longer offered to the authoring model. Clarifying questions go in the
  final message, and a turn ends when the model replies without a tool call.
  (`ask_user`/`input/2` remain available to DML as host tools.)
- **Markdown rendering and grey reasoning.** Assistant text is rendered as
  markdown (headings, fenced/inline code, bold/italic, links, lists,
  blockquotes). Model reasoning is now italic grey (like pi) and wired to
  `repl.showReasoning` / `--show-reasoning` (`off` | `summary` | `full`, default
  `summary`); previously the setting was unused and reasoning was faint/dimmed.
- **Status bar `↑` counts total input.** The input figure now includes cache
  reads/writes (`↑` = fresh + cache reads + cache writes), so it reflects the
  real prompt size instead of only the uncached remainder; `R`/`W` still show
  the cache split and `CH%` the hit rate.
- **Nested activity is indented, not labelled.** The `nested` word is gone;
  `execute` child events are shown by indentation plus a hanging `│`, and task
  activity renders as `▸ Task  <description>` / `✔ Task  done` / `✖ Task  failed`
  instead of a spinner frame.
- **The system prompt is one file.** `AGENT_PROMPT.md` is now a single,
  self-contained prompt: DML intro, module/skill model, inspect/execute/edit,
  host tools (`bash`, `ask_user`), the `tool/2` annotation, file I/O, the
  **complete DML reference** (mirrored verbatim from `DML_REFERENCE.md`), typical
  **deep-research and coding-agent patterns**, and the editing rules. It also
  states the design principle: keep DML simple, treat it as orchestration +
  verification, and avoid algorithmic Prolog. Template substitution is gone:
  `{{TOOLS}}`, `{{CATALOG}}`, `{{MANIFEST}}`, `{{REFERENCE}}`, `{{AUTHORING}}`
  were removed, along with `repl.agentDocs` / `--agent-docs`. `listing` now
  prints the `module_doc/1` skill description first.
- **Enforced edit transactions.** `define`/`assert`/`assert_many`/`retract`/
  `delete` are refused while no transaction is open — the agent must call
  `begin_turn` first — and after `end_turn` a new `begin_turn` is required to edit
  again. When the agent loop ends with an open transaction the host nudges the
  agent to `end_turn`/`rollback`; if it still doesn't, the host commits as a last
  resort and shows `commit transaction (host)`. Every change is now an explicit
  `begin_turn` … `end_turn` unit.
- **`tool/2` is documented as an annotation, not an implementation.** It only
  exposes an existing harness predicate to the nested `task/N` agent, so the
  docs now define a normal predicate and have `tool/2` delegate to it
  (`tool(count_words(F, C), "…") :- count_words(F, C).`). Logic placed only
  inside `tool/2` is unreachable from `execute`/`goal:` — the loader asserts
  `tool(Head)`, never `Head`. Updated `DML_REFERENCE.md`, `DML_AUTHORING.md`,
  `HARNESS_EXAMPLES.md`, `HARNESS_PATTERNS.md`, and `AGENT_PROMPT.md`.
- **The authoring agent has no file or shell tools.** `read_file`, `write_file`,
  `list_files`, and `delete_file` were removed; the agent's file I/O now goes
  through DML (`make_directory_path/1` + `write_file/2`, …) or `bash` via its
  `execute` tool. Its tool set is now only harness editing (`listing`, `begin_turn`,
  `end_turn`, `rollback`, `define`, `assert`, `assert_many`, `retract`, `delete`,
  `execute`), plus `ask_user` and `reference` (and the loop-level `finish`).
  `write_file/2` does not create parent directories.
- **`module_doc/1` is now the harness's skill description.** The manifest leads
  with `- skill: <module_doc>` (previously a trailing `- harness:` line), and the
  authoring prompt tells the agent to read it at the start of the session and
  re-read it whenever the harness changes. A file with only `module_doc/1` and no
  predicates is documented as a valid skill (an advertised capability with an
  empty body).

### Added

- **`with_model/2` — per-call model switching.** Scope `task/N`, `prompt/N` and
  `sample_token/N` inside a goal to a different model:
  `with_model("provider:model[#reasoning]", Goal)` or
  `with_model(model{provider, id, temperature, maxTokens, reasoning}, Goal)`.
  Composes with `with_reasoning/2`; judgments keep their own backend; nested
  `task/N` inherits the scope. Unknown or unsigned-in models fail clearly.
- **Model catalog in the prompt.** The authoring agent sees the session's
  credentialed models with catalog prices (USD per 1M tokens, in/out) in an
  `AVAILABLE MODELS` prompt section, plus a `models` tool for the live list.
- **Media display and generation.** `output(media("path"))` and
  `output(media{svg: Svg})` render an image inline (Kitty/Ghostty/WezTerm/iTerm2)
  or print a path to open; `task("…", media(X))` generates one and binds its
  path; `save_image/2` writes a reference; `/show <path>` and `/gen <prompt>` run
  from the REPL; `--json` emits a `media` event with base64 PNG. `image-models`
  lists image-capable models.
- **`/viz` terminal diagrams.** `/viz [entry] [--spec]` draws a skill inline
  (`--graphics=auto|kitty|iterm2|ascii|off`); the agent gets a `visualize` tool.
- **Browser-free `visualize`.** `deepclause visualize -o out.svg|out.png` now
  generates the SVG directly and rasterizes with the bundled `@resvg/resvg-js`
  (no `mermaid-cli`, no Chromium); `--ascii` prints a Unicode tree.
- **New authoring tools**: `models`, `write_file` (verbatim content — no DML/shell
  escaping), `show_image`, `generate_image`.
- **Harness in the banner.** The startup banner shows the active harness, whether
  it is new, and its predicate count.
- **`end_turn` nondeterminism probe.** Committed predicates are probed and the
  agent is warned (`⚠`) when a helper yields many solutions on fresh variables
  (a missing `once/1`/`!`), with the warning surfaced in the transcript.

- **Multiple harnesses per project.** A project now holds one directory per
  harness under `.deepclause/repl/harnesses/<name>/` (`harness.dml`,
  `baseline.dml`, `HEAD.json`, `changes.jsonl`, `versions/`). `--harness <name>`
  (default `main`, `repl.harness`) selects one; `-l <file>` targets the harness
  named after the file, creating it if needed, and the loaded file becomes that
  harness's **baseline** — so a harness brought in from elsewhere works with no
  prior history. A fresh session loads its harness's current content (it no
  longer re-creates existing skills and piles up versions), and `/rollback`
  walks only the current harness, floored at its baseline. The old single
  `harness/` store is migrated to `harnesses/main/` once.
- **Shell wall-clock timeout.** `exec(bash(command: Cmd, timeoutMs: N), R)`
  bounds a command by wall clock (exit code 124 on timeout); `DC_HOST_SHELL_TIMEOUT_MS`
  and `shell.timeoutMs` set the default (0 = off). Shell results now carry
  `timeMs`, and `shell.idleTimeoutMs` overrides the idle timer. Applies to the
  host shell and the AgentVM sandbox.
- **Execution timing.** The `execute` tool result now carries `elapsedMs`
  (wall-clock), so the agent can see which goals are slow, and completed/failed
  tool-call lines in the transcript show a human-readable duration (` · 1.2s`) —
  measured for every authoring tool call, and for `bash` from its `timeMs`.
- **Harness patterns reference** (`HARNESS_PATTERNS.md`, `reference("patterns")`):
  how a harness typically looks for a coding assistant, a research agent, an SOP
  runner, and a tool collection (browser use), mapped to the Anthropic/OpenAI
  workflow and orchestration patterns, with guardrails and a default skeleton.
- **TypeScript authoring agent**: the REPL's main agent is a normal LLM agent
  with a system prompt and harness tools (`begin_turn`, `end_turn`, `rollback`,
  `listing`, `define`, `assert`, `assert_many`, `retract`, `delete`, `execute`,
  `models`, `write_file`, `show_image`, `generate_image`, `visualize`,
  `reference`). It can run several
  committed turns in one request; `end_turn` validates and records a version, and
  it is asked to commit larger changes incrementally. The DML `task_named/3`
  meta-frame authoring path was **removed**.
- **pi-style TUI** (`repl.tui: auto|on|off`, on a TTY by default): a scrolling
  transcript, a fixed status bar (tokens in/out, cache, context usage for the
  active frame, model + thinking level), and a fixed editor.
- **Steering**: submit while a turn is running to interrupt the in-flight model
  call and inject the message, rendered as a full-width `Steering:` block.
- **Slash commands**: `/goal`, `/listing`, `/help`, `/viz`, `/show`, `/gen`,
  `/reset`, `/rollback`, `/compact`, `/export`, `/model`, `/freeze`/`/unfreeze`,
  `/params`, `/trace`/`/notrace`, `/halt` (the legacy `goal:`/`listing.` forms
  still work).
- **Editor input history**: recalled for the session and seeded from the target
  session on startup (goal turns re-prefixed with `/goal`).
- **Reasoning levels**: `backend.thinkingLevel` (off|minimal|low|medium|high|
  xhigh|max), selectable in `login`/`set-backend`/`set-model` and via `--thinking`.
- **TypeScript compaction** ported from pi (agent conversation + in-loop), with
  a `compact.` command. The DML compactor (bindings, assets, and config) was
  removed.
- **Listing syntax highlighting** and reflow (doc merged onto the header,
  predicates separated, long clause bodies split).
- **Workspace `AGENTS.md`** appended to the authoring agent's prompt. The prompt
  itself is one file (`AGENT_PROMPT.md`, used verbatim); the DML reference,
  authoring guide and pattern library are read on demand via the `reference` tool.
- **`execute` child events** stream (model text, tool calls, `output/1`) under a
  nested label.

### Removed

- The DML `task_named/3` meta-frame authoring mode (and its `META_PROMPT.md`,
  meta tools, and `repl.authoring` config) — the TypeScript agent replaced it.
- The DML compactor (`src/compaction.ts`, the bundled compactor DML, the
  `compaction` config section, and `createDeepClause`'s `compaction` option). The
  TypeScript compaction is the only compactor.

### Fixed

- `define` now replaces every predicate in its clause block (it used to append
  the non-head predicates, creating duplicates); `delete` abolishes the
  predicate instead of leaving an empty phantom.
- The REPL no longer writes host nudges into the authoring conversation.

## 0.1.0

A ground-up simplification. The SDK is now three surfaces: **repl**, **run**,
and the **TypeScript SDK**.

### Added

- **Living-module REPL** (`deepclause repl`): a persistent DML/Prolog harness.
  Plain English is the authoring mode — a meta task writes, tests, and commits
  DML — while `goal: <dml>.` queries it directly.
- **Edit transactions**: every turn snapshots the module and commits
  replace-by-predicate, so a failed turn leaves the harness unchanged.
- **Auditability**: sessions, turns, traces, and versions are recorded under
  `.deepclause/repl/` and exposed to DML as `harness_*` facts; `rollback.`
  restores a previous version.
- **Session resume** (`deepclause repl --session <id|last>`): reopen an earlier
  session with its harness, conversation memory, and turn numbering restored. A
  durable `sessions/<id>/harness.json` snapshot is written on every change and
  again at session end, so the harness survives process exit; a module+scripts
  hash flags any drift since the session was saved.
- **pi backend by default** (`backend.kind: "pi"`): reuses pi's model catalog,
  credentials, streaming, cancellation, and usage accounting. `deepclause
  login`/`logout` port pi's OAuth/API-key flow. `env` remains an opt-in backend.
- **`run` uses the REPL engine**: load a `.dml`/`.zip` harness, call
  `agent_main/N` or `--goal`, and stream through the REPL renderer.
- **Media input** (`read_media/2` + `attach_media/1`): attach a workspace image
  to the next `task/N`, reference-only (no bytes in clauses). See
  [`docs/DML_MEDIA.md`](docs/DML_MEDIA.md).
- **`deepclause visualize`**: draw a harness skill as a Mermaid diagram
  (`--presentation` or `--spec`) from its `listing` + docs, rendering a PNG via
  `@mermaid-js/mermaid-cli` when available.
- **Tool-call rendering**: full bash scripts and `execute` goals (wrapped,
  colour-coded panels), a live model-call spinner, and a short bash output preview.
- `--no-stream` / `--no-events` REPL and run flags; harness auto-save on exit.

### Changed

- **Host tools reduced to `bash`** (plus the human-in-the-loop channel).
  Built-in `web_search`, `vm_exec`, `url_fetch`, and `validate_dml` runtime
  tools are gone; non-Prolog steps go through `bash` or a linked `script_file/2`.
- README rewritten around the REPL; docs updated. Authoring guidance now favours
  short `task/N`/`prompt/N` compositions with thin Prolog guards, the shell for
  file I/O, and CLP only for bounded constraint problems.

### Fixed

- `once/1`, `ignore/1`, `call/2..4`, and `not/1` now route through the
  meta-interpreter — previously a wrapped `task/N`/`prompt/N` ran in raw Prolog and
  failed with `task_named/3` undefined.
- Ctrl-C terminates a running `bash`: the turn's abort signal reaches
  `tool.execute` (the shell managers already honoured a signal; nothing passed one).
- `listing`/`export` show `task/N`/`prompt/N`; the internal `task_named/3` rewrite
  and hoisted `format/3` are reversed for display (export round-trip is stable).
- `deepclause run --trace <file>` enables the engine trace and writes the file (the
  CLI previously printed “Trace saved” without writing anything).

### Removed

- The Markdown→DML **compiler** (use `deepclause-sdk/validate` for validation).
- The **TUI**, conductor/planner/skill-creator DMLs, recipes, the skill catalog,
  MCP tooling, and the old examples/benchmarks.

## 0.0.89 - 2026-09-20

- Include the meta-interpreter correctness fixes: `assertz/1`, `asserta/1`,
  `retract/1`, and `retractall/1` now operate on the session module, and
  user-defined predicates backtrack through every solution again (previously
  an if-then-else committed to the first).
- Add generic semantic judgment predicates: `judge/2` (batch), `choose/4`,
  `rate/4`, `verify/3`, `probability/3`, `holds/2,3`, `with_judgment/2`, and
  `require_judgment/2`. Answers are constrained to the supplied options and
  levels, and `verify` is three-valued (`yes` / `no` / `unknown`).
- Add a `JudgeBackend` extension point next to `LLMBackend`, with `llm`, `jev`
  (TypeSafe System One), and `mock` implementations. `llm` is the default, so
  judgment predicates work without new credentials; Jev is opt-in and
  calibrated.
- Memoize judgments per run by backend, model, state, and questions, so
  backtracking reuses an answer instead of re-querying the model.
- Emit judge activity (`task_activity`) and usage (`usageSource: "judge"`)
  events; support `with_judgment/2` scoping, per-run backend overrides, and
  `require_judgment/2` capability gating.

## 0.0.87 - 2026-08-19

- Add an injectable `LLMBackend` for host-managed models, credentials, cancellation, tool calls, streaming callbacks, and usage.
- Preserve opaque provider-native assistant data across multi-step task loops.
- Route `llm/2`, token sampling, `task/N`, `prompt/N`, and nested execution through the injected backend.
- Support structured typed task results through injected backend tool calls.
- Keep DML-defined tool predicates visible inside task loops while enforcing runtime policy on their nested `exec/2` calls.
- Export backend, usage, and memory types from the package entry point.
