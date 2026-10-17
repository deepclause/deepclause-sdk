# Changelog

## Unreleased

### Changed

- **A refused submit is now the host's business, not the caller's.** The host
  enforced the rules on submit but returned `action: invalid` with the errors, so
  every harness and every model-driven loop carried a reject-and-re-show branch
  (`form_errors` / `set_form_issues`, then wait again). `await_form/2` now loops
  internally: on a rejection the host attaches the errors to the form (which is
  re-presented, with the values the user typed) and keeps waiting. A caller only
  ever sees an action the rules accepted — so the loop is
  `form` → `await_form` → `close_form`, with no error branch, and
  `wait_for_form_change` likewise returns only `submit`/`cancel`. A caller that
  wants the raw errors up front can still call `form_validate/2` (the
  `validate_form` tool).
- **A form spec no longer needs `version`, and a field's `type` defaults to
  `string`.** A model asked for "a form with an amount and a reason" can answer
  `{title: "Refund", fields: [{name: "amount", type: "number"}, {name: "reason"}]}`,
  and a bare string field (`"reason"`) is shorthand. `normalizeSpec` fills the
  gaps; genuinely broken specs are still reported by `validateSpec`.
- **`spec` travels with every `form` event, and `update` events re-present the
  form.** Previously an update (a model's `update_form`, or the host showing
  errors) carried no `spec`, so a renderer could not draw it — the errors were
  effectively invisible.

### Added

- **`chat/1` and `input/1`: conversational agents.** `input/1` waits for the
  person's next message with **no prompt** (the embedding UI is already a chat
  interface) and **fails** when there is nobody to talk to, which ends
  `loop :- input(A), chat(A), loop.` `chat/1` runs one turn of a **traditional
  agent loop** — the harness's `tool/N`s, but no `finish`, no `set_result` and no
  nudging: the model calls tools, replies, and the turn ends when it stops calling
  tools. The reply is streamed and kept in memory (memory *is* the conversation),
  and `chat/2` also binds it. `agent.ts` gained a `conversational` mode over the
  existing loop; `CHAT_PROMPT.md` is its system prompt.
  [`docs/DML_CONVERSATION.md`](docs/DML_CONVERSATION.md) documents the contract and
  **Events: `next/1`.** The runner merges the person's input channel and the
  session's mailbox into **one inbox**, so a conversation can also react to a
  worker's message: `next(Item)` returns `user(Text)` or `message(Term)` and fails
  when the inbox is empty, and `input/1` is the same read restricted to the
  person. A queued message beats a closed inbox (the mailbox is polled once more
  before giving up), items are queued rather than dropped, and an event turn is a
  normal turn in the transcript. Declarative `on/1` handlers are deliberately not
  built — they would need re-entrancy and hide control flow; `next/1` + a
  `handle/1` clause covers the same ground. A handler can still be added at
  runtime with `assertz/1`, which works inside a harness.
- **A failed turn no longer ends a conversation.** A thrown model error used to
  abort the whole run; in a chat that dropped the person out of it. `chat/1` now
  reports the failure, leaves memory as it was, and the next message starts a
  fresh turn. `post_agent_result/6` carries the error so the message can say what
  went wrong (it also fixes `task/N`'s long-dead `error` lookup).
- **`reference("forms")`.** The bundled `DML_FORMS.md`, so the authoring agent can
  discover the feature at runtime. `HARNESS_EXAMPLES.md` gained a runnable forms
  example (§9) and a conversational one (§10), and the prompt teaches the
  three-call model loop first, with the DML primitives second.
  `docs/DML_FORMS.md` §4 records why the gate was removed.

### Removed

- **The `:- capability(forms).` directive.** Forms are always available; the
  primitives and the `tool/N` wrappers are installed into every session. An older
  file that still carries the directive loads unchanged (`capability/1` is
  ignored). The built-in tools stay out of a harness's own surface — `listing`,
  the manifest's predicates, the export, the counts, and the `end_turn` probes —
  but appear in `listing("tools")` and are offered to `task/N` / `with_tools/2`.

### Fixed

- **A form's `action` is an atom in DML.** The host sent it as a JSON string, so
  `Change.action == submit` — the documented idiom, and what every example used —
  was silently always false, and a no-model loop fell through and answered
  `invalid`. It is converted at the boundary; the model-facing tools re-serialize
  to JSON, where it is a string again.
- **`--json` output is now pure JSON.** `repl --json -g` wrote a raw `false.`
  line when a goal produced no solution, corrupting the one-object-per-line
  stream for any tooling that reads it. It now emits `{"type":"no_solution"}`.
  A goal that committed with `answer/1` is no longer reported as a failure, and
  the interactive goal path no longer leaks its `.` terminator in JSON mode.

## 0.2.2 - 2026-10-15

### Fixed

- **The tool policy now governs `exec/2` inside a DML tool.** The top-level path
  checked the policy; the tool-engine path did not, so a `tool/2` body could reach
  a host capability the session had blocked. Both paths now check, and
  `tests/tool-policy-exec.test.ts` covers each.

### Added

- **A reloaded session says so.** Resuming a session with `--session` appends a
  short system note to the restored context stating that the session was reloaded
  and its transient state was reset (subagents/services from the previous run are
  gone). It reaches both the runtime (`task/N`) memory and the authoring memory,
  and is replaced rather than stacked across repeated resumes.

### Changed

- **Curated `sdk-examples/`.** Replaced the stale grab-bags (`basic-usage`,
  `no-api-key`, `deep-research`, `judge`) with a set where every example runs with
  no credentials and demonstrates one idea: `quick-start`, `tools` (tools +
  policy), `neurosymbolic` (prose -> declared schema -> exact logic), `judgments`,
  and the `subagents`/`debate` examples. A shared scripted-model fallback makes
  offline runs deterministic, and `examples:smoke` exercises the whole set.
- **`sdk-examples/subagents.dml` is now a six-pattern tour** of the concurrency
  features: fan-out (`swarm`), best-of-N plus a judge, a stateful service,
  agents that route work between processes through tools, a hedged race with
  cancellation, and CPU-bound CLP search in subagents — plus the three process
  lifecycle shapes. A test loads the file and runs the examples against a
  scripted model.
- **`deepclause-agentvm` 0.0.5 → 0.4.0.** The sandbox API the SDK uses
  (`new AgentVM({ network, mounts })`, `start`/`exec`/`stop`, and the
  `{ stdout, stderr, exitCode }` result) is unchanged. 0.4.0 adds
  `persistentRoot`/`persistentRootDir`, adds a `blitFrame` export, and raises
  the default network rate limit from 512 KiB/s to 2 MiB/s. It requires Node
  ≥ 20 (0.0.5 already did), so the SDK's `engines.node` moves from `>=18` to
  `>=20.0.0`.

### Docs

- The bundled language reference now documents `input/2`, `push_context` /
  `pop_context`, `clear_memory`, `get_memory/1`, `save_image/2`, and the host tool
  policy. `AGENT_PROMPT.md` is regenerated from it.

## 0.2.1 - 2026-10-15

### Fixed

- **A worker no longer runs stale harness code.** A worker host loaded the
  harness once at worker start, so every process spawned later in a long REPL
  run ran the code from the worker's first spawn: committing an edit changed
  nothing for new subagents until the whole session restarted. The current
  snapshot is now pushed with each spawn, so a new process always runs the
  committed module while an already-running process keeps the code it started
  with.
- **A DML tool can use process primitives.** `process_tool_engine_result/4` had
  no `request_proc` clause, so a tool calling `self/1` (or `send`/`receive`/
  `serve`/`service_call`/`spawn`) failed with "Unknown tool engine result:
  request_proc(self)" and the nested model retried in a loop.

### Added

- **Harness snapshot inspection.** Every process carries the version of the
  module it runs; `/ps` shows it (`code <version>`, marked `(old)` when it
  predates the latest commit), `/ps --code` prints the current version, and
  `/ps --code <name>` prints the full harness source a process is running. The
  `ps` agent tool takes the same `code` argument.

### Changed

- **`worker` is the default execution mode** (`--execution inline` to opt back
  into the in-process host). Subagents now run in a worker thread with its own
  WASM: true parallelism, `worker.terminate()` cancellation, and no CPU-bound
  Prolog on the REPL thread.

## 0.2.0 - 2026-10-15

### Added

- **Subagents: session-scoped DML processes.** A process has its own module
  snapshot, private memory, a mailbox, and a lifecycle; `spawn/2` starts one and
  returns immediately, and it keeps running after the turn that started it.
  Built-ins: `spawn`/`spawn_opt`, `send`/`receive`/`receive_match`, `exit`,
  `register`/`whereis`, `process_info`, `wait`, `sleep`/`now_ms`/`make_ref`.
  Services via `serve/2` + `service_call/3,4` + `service_cast/2`; bounded
  fan-out via `swarm/3,4` and `pmap/3`. `spawn` and `send` copy terms (value
  semantics), so a child can never bind a parent's variable and a result comes
  back by message or as the child's `answer/1`.
- **Two execution modes.** `--execution inline` (default; cooperative, one WASM
  instance) or `--execution worker` (a `worker_threads` thread with its own
  WASM: real parallelism and hard cancellation). Model calls, host tools and
  user input are proxied to the main thread, so a worker never holds credentials
  or policy; the DML is identical in both.
- **A REPL process surface.** The authoring agent gets `spawn`, `ps`, `send`,
  `call`, `stop`, `swarm`, and `wait` tools; users get `/spawn` (`/serve`),
  `/ps` (`--watch` / `--logs`), `/send` (`/cast`), `/call`, and `/stop`. A status
  segment and a services panel show the process tree, and `--logs` follows every
  subagent's streamed output, reasoning, and tool calls.
- **`/login` in the REPL**, and the `deepclause login` / `set-backend` pickers
  now scroll with the arrow keys and filter as you type.
- **Resume replays the transcript.** `--session <id|last>` restores the harness
  and re-renders the prior events, instead of starting blank.
- **A per-session model-call semaphore** (`providerConcurrency`, default 4)
  bounds concurrent provider calls across the main run and every subagent.
- **Docs and examples**: `docs/SUBAGENTS.md` (model and BEAM/OTP inspiration), a
  *Processes and concurrency* section in the bundled DML reference and the
  authoring prompt, plus `sdk-examples/subagents.{dml,ts}` and
  `sdk-examples/debate.{dml,ts}` (two LLM-backed agents debating).

### Changed

- The worker coalesces streamed `stream`/`reasoning` chunks before crossing the
  worker boundary, so a long response is a handful of IPC messages, not hundreds.

### Fixed

- Slash commands are no longer injected as steering while a turn runs. UI toggles
  (`/ps`, `/harness`) and read-only snapshots act immediately; commands that do
  work (`/send`, `/call`, `/spawn`, `/stop`) run when the turn finishes.
- The dock spinner is cleared when a turn ends, errors, or is interrupted, so an
  aborted run no longer leaves it animating.
- Streamed reasoning keeps its frame indentation across an embedded newline, and
  a multi-line tool argument no longer leaks a stray line outside the tree.
- The REPL banner and the editor autocomplete advertise the process commands.

## 0.1.3 - 2026-10-14

### Changed

- **The REPL TUI is fullscreen.** It runs on pi-tui's alternate screen with an
  application-owned viewport: the transcript scrolls inside the app — scrollbar,
  mouse wheel, text selection, clipboard, search — while the editor and status bar
  stay pinned in a dock. `ScrollView`'s follow-end and wheel support only exist on
  that path, which is why the transcript previously scrolled with the terminal's
  scrollback and the layout was fragile when the terminal was resized. `--plain`,
  `--json` and non-TTY output are unchanged and still use the line renderer.

- **Esc interrupts a turn; Ctrl-C exits.** Ctrl-C used to abort the turn and need a
  second press to leave, which made it ambiguous whether a session was still alive.
  Escape now interrupts (leaving its usual meaning alone while a completion list or
  a search overlay owns it), and Ctrl-C exits the REPL — aborting a running turn
  first so nothing keeps writing after the UI is gone. The readline path exits on
  Ctrl-C too, since it has no raw key to tell Escape apart from an editing sequence.

### Added

- **Tool calls render as panes.** Each `execute`/`bash`/`listing` call is a
  component with its own status, duration and captured output — collapsed by
  default, expanded with `ctrl+o`. Unlike the old line panel it can be re-rendered
  and reopened, and its content is markdown when expanded.
- **Streamed answers render as markdown as they arrive** (pi's markdown theme),
  instead of the per-line ANSI markdown renderer on the TUI path.
- **A working indicator in the dock**, driven by turn state rather than by whichever
  event happened to arrive last. With reasoning off and streaming quiet, the
  transcript used to go silent with no sign that anything was running.
- **Slash-command autocomplete** in the editor (pi's combined provider), with file
  completion.
- **Pi's theme is adopted** for the component layer: markdown and editor themes come
  from pi, and colours go through pi-tui's styler, so the terminal's colour mode is
  handled for us. A fallback palette is used if pi's theme cannot be resolved.

- **Model and reasoning pickers in the TUI**, over the transcript: the list takes the
  keys while it is up, Escape closes it, and focus returns to the editor.
  - `/model` with no argument — the models this session can switch to, with prices or
    context size. Sets the workspace default; it says "takes effect on the next
    start", because the session has no live model-switch API (`with_model/2` scopes
    one call, it does not change the session).
  - `/reasoning [level]` — the model's reasoning **level** (its thinking effort:
    `off`…`max`), offering the levels that model accepts. Applies immediately: the
    backend resolves the level per call instead of capturing it.
  - `/show-reasoning` (alias `/reasoning-summary`) — how much of that reasoning is
    **displayed**: `off` / `summary` / `full`. Also applies immediately.
  - A setting is applied even when it cannot be saved: `updateConfig` needs an
    existing config file, and letting that failure abort the picker made the
    selection do nothing at all.
  - Both appear in the editor's command completion, along with every other command the
    parser accepts — a test now checks that the two lists agree in both directions,
    which is how `/reasoning` and `/show-reasoning` were found to be missing.

### Fixed

- **A turn's streamed reply rendered *above* the prompt that asked for it.** The
  streamed-text block was never closed at the end of a turn, so the next turn's text
  was appended to the previous turn's block — which sits earlier in the transcript.
  What looked like "streaming does not continue after the prompt line" was the reply
  appearing before it, joined to the previous answer with no separation
  (`…touching the module.Not much — just a blank harness…`). The block is now closed
  when a run finishes, and by the host when a prompt is submitted (which also covers
  an interrupted turn that never reports `finished`).
- **Wrapped lines lost their place in the tree.** A long line emitted by the line
  renderer (a solution value, a log line) carries its frame's stem (`run │ `) inside
  the text; when the document wrapped it to the terminal width, the continuation lines
  carried no stem, so a wrapped value read as one indented line followed by an
  unindented one, outside the frame. The stem is now kept out of the wrap and reattached
  to every line.
- **Wrapping an already-indented line produced a blank line and a bare stem.**
  `wrapTextWithAnsi` splits on whitespace, so handing it `    │       text` emitted a
  line containing only the stem, and an indented paragraph emitted an empty first
  line. The indent is stripped before wrapping and reattached after.
- **The transcript was crowded.** Output ran edge to edge with no separation: a
  tool call, its result and the next event were adjacent lines. There is now a blank
  row above the transcript and between it and the editor (fixed spacers, so they do
  not scroll away), and a blank line between any text block and a component — a
  streamed answer, a tool pane, a solution — in either order. Events that render
  nothing (a model call, a usage update) add no gap, and consecutive chunks of one
  streamed answer stay one block.
- **The status bar is one row.** Where the harness lives and what it is doing on the
  left, the model on the right, the token/context counters between them; anything
  that does not fit is dropped whole. The workspace is shortened to `…/project`
  rather than spending half the row on an absolute path, and `running` spells out
  `(type to steer)` only when there is room.
- **The status bar cut segments in half and lost the useful ones.** It truncated the
  last thing on the line to fit, so a narrow terminal showed `deepseek-flash · ` — a
  separator with nothing after it — and the absolute workspace path won the space
  fight, crowding out the model, session and running state. It now shows `~`-relative
  paths (`…/gh/project` when long) and drops whole segments in priority order
  (workspace, running, frozen, harness, backend, session; counters before the model),
  so every row states what it can rather than trailing off. The model is always kept,
  right-aligned, and is the last thing truncated.
- **Tool panes broke the tree.** Nesting is expressed by the frame's prefix
  (`run │ ` at top level, `    │ ` for an `execute` child), and the panes ignored it
  while also adding a stem of their own — so a nested call showed two stems at
  different columns, and its body did not line up with the surrounding output. Panes
  now hang off the same prefix with plain indentation, as the line renderer always
  did, and a pane only advertises `ctrl+o to expand` when something is actually
  hidden.
- **The TUI drew a 24-row frame in a taller terminal.** pi-tui asks the stream for
  its size (`process.stdout.rows || LINES || 24`), and when the stream reports none
  the fallback is a hard-coded 24, so the UI occupied the top of the screen and
  left the rest untouched. It now asks the tty directly (`stty size`) when the
  stream cannot answer, refreshed on resize.
- **Constructing a pi-tui `Loader` started a timer that kept the process alive.**
  The REPL never exited after `/halt` because of it. The indicator is now parked
  until there is work, and stopped on exit.
- **`getMarkdownTheme()` styles throw on *use* when pi's theme was never
  initialized**, not at lookup, so a lookup-time fallback never fired. The theme is
  initialized first and probed by calling one of its functions.

## 0.1.2 - 2026-10-14

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

- **`execute(goal, bindings: "brief" | "none")`** trims what a test run returns when
  one bound value is huge: `brief` keeps the names and shortens each value,
  `none` returns no bindings. Both report what was left out and the original
  length, so a shortened value cannot be mistaken for a short one, or an omitted
  binding for a goal that bound nothing.

### Changed

Harness-visible behaviour that this release tightens. Each of these used to
*succeed quietly* while doing something other than what it said; they now do what
they claim, or refuse. Check existing harnesses and prompts for the old shapes.

- **`retract` removes only the clauses matching its pattern.** It used to discard
  the pattern's arguments and remove every clause of the predicate. A pattern
  that matches nothing is now an error rather than a silent `ok`.
- **`assert`/`assert_many` refuse a predicate that already has clauses.** They
  appended, which left the old and the new version live together.
- **`define`'s head argument is the target**, and must be a predicate the clause
  block defines. It was parsed as a division term and silently ignored.
- **Paths resolve against `/workspace`**, which is the working directory; there is
  no rewriting layer. `/workspace/x` is the file `x` (it used to become
  `/workspace/workspace/x`), and other absolute paths are the session's ephemeral
  VM filesystem instead of being re-rooted into the workspace. `directory_files("")`
  is now an error — use `"."`.
- **An unknown tool parameter is an error.** `exec(bash(command: …, stdin: …))`
  used to drop `stdin` and then block until the timeout; the call is refused, with
  the accepted parameters named.
- **Prolog's own stdout/stderr is captured and rendered as events** rather than
  printed to the terminal, so a reader warning no longer corrupts an interactive
  frame. A warning or error is shown without `--verbose`.

### Fixed

- **A failed `define` used to destroy the predicate it was replacing.** Abolish
  ran before the commit, so a block with one reserved name in it removed the
  target, reported an error, and a following `end_turn` committed the deletion as
  a version. Everything that can fail is now checked first, and the clauses being
  replaced are copied so a failure mid-commit still restores them.
- **`define`'s head argument was ignored.** `'f/1'` reads as the operator term
  `/(f, 1)`, so the target was parsed as the functor `//2` and the clause block
  alone decided what got replaced. The head is now the target, and `define`
  refuses a target the block does not define.
- **`assert`/`assert_many` silently mixed versions** — appending to an existing
  predicate left both live with their clauses interleaved. Refused now, naming the
  clause count and the remedy.
- **`retract` ignored its pattern** and removed every clause of the predicate,
  reporting `ok`. It now removes the clauses that match, reports how many, and
  fails when none did.
- **A nested `begin_turn` clobbered the snapshot**, so `rollback` restored the
  uncommitted state instead of the last commit. Refused now.
- **Prolog's own stdout/stderr went straight to the terminal**, so a reader
  warning landed in the middle of an interactive frame. Captured and rendered as
  events instead: a warning or error is shown without `--verbose`.
- **DML paths no longer go through a rewriting layer.** `/workspace` is mounted
  and is the working directory, so `x` and `/workspace/x` are one file; anything
  else is the session's ephemeral VM filesystem. The old layer turned
  `/workspace/x` into `/workspace/workspace/x` and re-rooted `/etc/passwd` into
  the workspace instead of rejecting it, as the docs claimed.
- **`exec(bash(...))` has no `stdin`** — passing one was silently dropped, and
  then the command blocked until the timeout. An unknown tool parameter is now an
  error that names the accepted ones.

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

### Known gaps

- **The nested-tool exclusion is unverified.** A tool whose body calls `task/N` is
  withheld from the agent that `task/N` spawns (declared with
  `(unavailable in this task)`, and refused at dispatch if called anyway), and the
  construction sites are unit-tested. What is *not* verified is the live nested
  request: no test drives a tool call through the agent loop, because the DML path
  goes through `runAgentLoop` and the AI SDK, and the existing SDK mocks call
  `tools.X.execute` directly rather than exercising the loop's dispatch. Confirming
  it needs either a faithful `generateText` tool-call fixture or a real model call.
- **`schema/2` fields are described to the model, but a *changed* schema does not
  invalidate a task that names it.** Adding a field affects the next evaluated
  clause, not a session's existing prompt.

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
