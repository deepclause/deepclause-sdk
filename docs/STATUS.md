# Current state

A snapshot of what DeepClause SDK is today.

## Surfaces

- **`deepclause repl`** — a living-module REPL: plain English authors DML via the
  TypeScript authoring agent, `/goal <dml>` queries it, slash commands control it.
- **`deepclause run <file>`** — one-shot execution of a `.dml`/`.zip` harness via
  `agent_main/N` or `--goal`.
- **`deepclause visualize <file> -g P/A`** — a Mermaid diagram of a harness skill.
- **TypeScript SDK** — `createDeepClause` (embed the runtime), `createHarnessRepl`
  (embed the REPL), judge backends, the pi/env backend helpers.

CLI commands: `init`, `login`, `logout`, `set-backend`, `set-model`, `show-model`,
`list-models`, `image-models`, `run`, `repl`, `visualize`.

## Runtime

- **Engine**: SWI-Prolog compiled to WebAssembly (`vendor/swipl-wasm`, loaded via
  `src/prolog/`). In-process, no native Prolog.
- **Backend**: `pi` by default (reuses pi's model catalog/credentials/streaming/
  cancellation/usage); `env` (provider API keys) is opt-in.
- **Host tools**: `bash` only, plus the human channel (`ask_user` / `input/2`).
  Everything else is DML, a script run through `bash`, or a model call.
- **Paths**: workspace-relative and confined; `bash` sees more, DML does not.
- **Sandboxing**: default host shell with a wrapper (`clean-room`/`bwrap`/
  `sandbox-exec`); `--sandbox` uses the AgentVM WASM micro-VM; network off in the
  VM by default.

## REPL

- **Author mode** (plain English): a TypeScript authoring agent writes, tests,
  and commits DML through harness tools
  (`begin_turn`/`end_turn`/`rollback`/`listing`/`define`/`assert`/`retract`/
  `delete`/`execute`/`reference`/`models`/`write_file`/`show_image`/
  `generate_image`/`visualize`), and can run several
  committed turns in one request.
- **Transactions**: each turn snapshots the module and commits
  replace-by-predicate; a failed turn leaves the harness unchanged.
- **Auditability**: `.deepclause/repl/` holds `history.dml` (append-only
  `harness_*` facts) and, per harness, `harnesses/<name>/{harness.dml,
  baseline.dml,HEAD.json,changes.jsonl,versions/*.json}`, plus
  `sessions/<id>/{session.json,harness.json,turns.jsonl,events.jsonl}`.
  `rollback.` restores the previous version, floored at the harness baseline.
- **Multiple harnesses per project**: `--harness <name>` (default `main`) selects
  an independent harness with its own content and history; `-l file.dml` uses/creates
  a harness named after the file, whose loaded content becomes its baseline.
- **Resume**: `deepclause repl --session <id|last>` restores the harness, memory,
  and turn numbering from the durable `sessions/<id>/harness.json`, and replays the
  prior transcript (`events.jsonl`, bounded) so the session continues instead of
  starting blank.
- **Media**: `read_media/2` + `attach_media/1` attach a workspace image to the next
  `task/N` (reference-only; no bytes in clauses). Images can also be **displayed**
  (`output(media(...))`, `output(media{svg: …})`, `/show <path>`) and **generated**
  (`task("…", media(X))`, `/gen <prompt>`) — rendered inline on Kitty/
  Ghostty/WezTerm/iTerm2, else a path (plus `xdg-open`/`open`). `save_image/2`
  writes a reference to a file. See [`DML_MEDIA.md`](./DML_MEDIA.md).
- **Model switching**: `with_model/2` scopes `task/N`/`prompt/N`/`sample_token/N`
  inside `Goal` to a different model — string `"provider:model"` (optional
  `#reasoning`) or a `model{provider,id,temperature,maxTokens,reasoning}` dict.
  The authoring agent sees the session's credentialed models and their prices
  (an `AVAILABLE MODELS` prompt section plus the `models` tool). Judgments keep
  their own backend. See [`DML_MODEL_SWITCHING.md`](./DML_MODEL_SWITCHING.md).
- **Subagents / processes**: session-scoped DML processes, each with its own
  module snapshot, memory, and mailbox. Built-ins: `spawn`/`spawn_opt`, `send`/
  `receive`/`receive_match`, `serve`/`service_call`/`service_cast`, `swarm`/`pmap`,
  `wait`, `exit`, `register`/`whereis`, `process_info`. The REPL runs them with
  the `spawn`/`ps`/`send`/`call`/`stop`/`swarm`/`wait` tools and the `/spawn`,
  `/ps --watch --logs`, `/send`, `/call`, `/stop` commands. Execution is `inline`
  (L1, cooperative, in-process) or `worker` (L2: a `worker_threads` thread with
  its own WASM, true parallelism and hard cancellation), selected by
  `--execution`; value semantics (copy on `spawn`/`send`) let the same DML run in
  both. See [`SUBAGENTS.md`](./SUBAGENTS.md).
- **Commands**: `/goal <dml>`, `/help`, `/listing [filter]`, `/viz [entry] [--spec]`,
  `/show <path>`, `/gen <prompt>`, `/spawn <goal>`, `/ps [--watch|--logs]`,
  `/send <name> <term>`, `/call <name> <term>`, `/stop <name>`, `/login`,
  `/reset`, `/rollback`, `/compact [notes]`, `/export [file]`, `/model [id]`,
  `/reasoning [level]`, `/show-reasoning [mode]`, `/harness [width]`,
  `/freeze`/`/unfreeze`, `/params`, `/trace`/`/notrace`, `/halt` (aliases
  `/exit`, `/quit`; the legacy `goal:`/`listing.` forms still work).
- **UI**: on a TTY the default is a pi-style TUI (`repl.tui`): a scrolling
transcript, a fixed status bar, and a fixed editor. The status bar shows
cumulative tokens in/out (plus cache and hit rate), context usage for the active
frame slot (`meta` while authoring, `run` while a child frame runs), and the
current model with its reasoning level. `ask_user`/`input/2` reuse the same
editor, so input prompts never fight the main prompt. `--plain`/`--no-tui` fall
back to the line renderer, which tints tool calls (green for shell, blue for
`execute`), shows a model-call spinner, previews a few lines of bash output, and
shows the full bash script / `execute` goal wrapped. `--no-stream`,
`--no-events`, `--json`.
- **Reasoning models**: `backend.thinkingLevel` (off|minimal|low|medium|high|xhigh|max)
is selectable in `deepclause login`, `set-backend`, `set-model`, and `repl`/`run`
(`--thinking`) and is forwarded to pi's provider only for reasoning-capable
models. The bundled catalog marks `[reasoning]` models in every picker.
- **Compaction**: TypeScript compaction ported from pi. It sizes context with
  the chars/4 estimate, cuts on user boundaries, and summarizes with the
  structured pi prompt through the active pi backend. The agent conversation
  compacts in-loop before a model call and on demand with `compact.`
  `memory_compaction` events drive the status bar.
- **Cancellation**: Ctrl-C aborts the current turn and terminates a running `bash`.
- **Steering**: submitting while a turn runs interrupts the in-flight model call
  and injects the message, rendered as a full-width `Steering: …` block. During a
  `goal:` query the line is buffered for the solution pager instead (Enter/`;`
  pages, `a` prints all, `.`/`q` stops).
- **Input history**: the editor recalls every submitted line for the session;
  on startup it is seeded from the target session (`--session` or the latest),
  goal turns re-prefixed as `/goal …`.
- **Listing**: `listing`/`/listing` output is reflowed (doc merged onto the
  header, predicates separated, long clause bodies split) and syntax-highlighted.
- **Project context**: a workspace `AGENTS.md` is appended to the agent's system
  prompt and shown in the banner. The prompt itself is one file
  (`AGENT_PROMPT.md`); the full DML reference and pattern library are read on
  demand via the `reference` tool.
- **`execute` events**: a goal run by the agent's `execute` tool streams its
  model text, tool calls, and `output/1` indented under the parent tool call.

## Authoring model

The authoring agent's prompt is the bundled `AGENT_PROMPT.md` (plus the embedded
DML reference/authoring guide, the tool list, the live manifest, and any
workspace `AGENTS.md`); the shorter guides (`DML_AUTHORING.md`,
`HARNESS_EXAMPLES.md`, `PROLOG_SNIPPETS.md`, `DML_REFERENCE.md`) are served by the
`reference` tool. The guides encode the current philosophy:

- Skills are **short sequences of `task/N`/`prompt/N` calls** with thin Prolog
  guards; no hand-rolled algorithms, and CLP only for genuine constraint problems
  (bounded domains — an unsatisfiable model can search forever, and there is no
  per-goal timeout).
- **Generalize, don't specialize**; distinguish reusable capabilities from one-off
  actions; never hardcode host paths or other environment-specific values.
- **Prefer the shell** for file I/O; use DML file predicates only for content that
  must be reasoned over inside Prolog.
- **Determinism**: a skill that calls `task/N`, `exec/2`, `read_media/2`, or writes
  a file must be deterministic (`once/1`, `->`, `!`).
- **Incremental commits**: for anything larger than a couple of predicates, the
  agent is instructed to `execute`, `end_turn`, and `begin_turn` per working
  piece rather than save one giant edit for the end.
- `listing`/`export` show `task/N`/`prompt/N` (the internal `task_named/3` rewrite
  is reversed for display).

## Tests

`npm test`: **832 passed / 26 skipped / 0 failed**. The skipped 26 are the
`llm-integration` suite, gated behind real provider API keys, and one opt-in
real-worker test.

## Not implemented / known gaps

- **Subagent links, monitors, and a supervisor library** — designed in
  [`DML_REPL_PROCESSES_SPEC.md`](./DML_REPL_PROCESSES_SPEC.md) but not built;
  `link/1`, `monitor/2`, `demonitor/1`, `processes/1` and `self_usage/1` are not
  implemented.
- **`.deepclause/lib/` libraries** — designed but not built; see
  [`DML_LIBS_PROPOSAL.md`](./DML_LIBS_PROPOSAL.md).
- **Video** — image input, inline display, and image generation all work, but
  video (frame sampling) is not built; see [`DML_MEDIA.md`](./DML_MEDIA.md).
- **No per-goal wall-clock timeout** — a runaway goal (e.g. unbounded CLP) hangs.
- **`visualize` images** render without a browser (SVG generated directly, PNG via
  the bundled `@resvg/resvg-js`); a `.mmd` output always works.
- **`deepclause-sdk/compiler`** is retained as a thin `validateWithProlog` alias
  for the sibling apps until the next publish, then remove it.
- **`dist/`** is untracked build output (gitignored).
- The pi extension (`dc_run`, slash commands) lives in a separate repo.

## Docs map

- Language/runtime: `DML_REFERENCE.md`, `DML_AUTHORING.md`, `HARNESS_EXAMPLES.md`,
  `PROLOG_SNIPPETS.md` (bundled; also seeded to `.deepclause/docs/`).
- REPL: `REPL.md`, `DML_REPL_PROPOSAL.md`, `DML_REPL_IMPLEMENTATION_PLAN.md`,
  `DML_REPL_SESSION_HISTORY.md`, `DML_REPL_SCRIPT_BUNDLES.md`.
- Subagents / concurrency: `SUBAGENTS.md` (overview), `DML_REPL_SUBAGENTS.md`
  (the three shapes), `DML_REPL_PROCESSES_SPEC.md` (spec),
  `DML_REPL_PROCESSES_IMPLEMENTATION_PLAN.md` (phases),
  `DML_REPL_SERVICES_UX.md` (UX), `DML_PARALLEL_BACKGROUND.md`,
  `DML_BEAM_SWARM_EXAMPLE.md`, `DML_SPECIALIZED_PROCESSES_EXAMPLE.md`.
- Other: `JUDGMENT_BACKENDS.md`, `JUDGE_EXAMPLES.md`, `DML_MEDIA.md`,
  `DML_MODEL_SWITCHING.md`, `DML_REPL_WORKER_EXECUTION.md` (worker plan; L2 now
  implemented in `worker-host.ts`), `DML_LIBS_PROPOSAL.md`, `REFACTOR_CLEANUP.md`,
  this file.
