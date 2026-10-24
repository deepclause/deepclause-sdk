# The DML REPL

`deepclause repl` opens the workspace's current harness (blank for a new slot)
and lets you grow a *living harness* — either by running DML goals
directly or by describing what you want in plain English and letting the model
write the DML.

Design reference: [`docs/DML_REPL_PROPOSAL.md`](./DML_REPL_PROPOSAL.md).
Implementation plan: [`docs/DML_REPL_IMPLEMENTATION_PLAN.md`](./DML_REPL_IMPLEMENTATION_PLAN.md).

## Quick start

```bash
# One-shot goal (like `swipl -g`)
deepclause repl -g "member(X, [1,2,3])"

# Load a harness, freeze it, run one entry, exit
deepclause repl -l harness.dml -f -g "explain_factorial"

# Scripted session over stdin
printf '/goal fact(5, F).\nlisting.\nhalt.\n' | deepclause repl -l fact.dml --no-ask

# Interactive
deepclause repl
```

`deepclause-repl` is a thin alias for `deepclause repl`.

## Develop and Work

Develop is the default authoring conversation. Work is a separate, restricted
conversation over a declared `shell/1`, shared with the web Work UI:

```bash
deepclause repl --mode work --harness example
deepclause repl --mode work --harness example --workflow "Summarise text" --inputs '{"Text":"hello"}' --json
```

`/mode` opens the TUI mode/harness chooser; `/workflows` chooses a task;
`/workflow "Name" {"Input":"value"}` runs that exact task; `/details` toggles
technical events. Work never guesses workflows from internal predicates, and
manual starts do not ask an assistant to decide what to run. Work and Develop
retain separate conversation memory and persisted identity. See
[Work mode](./WORK_MODE.md) for types, migration, snapshots and embedding APIs.

## Two input modes

The following input classification applies to **Develop**. Work conversation uses
its declared agent/tools; a workflow-only shell uses its workflow picker.

The classifier is intentionally trivial and unambiguous. **Slash commands** are
the primary surface; the bare forms (`goal:`, `listing.`, …) remain for
compatibility and for non-TTY/scripted use.

| Input | Mode |
|---|---|
| `/goal member(X, [a,b,c])` (or `goal: member(X, [a,b,c]).`) | **goal mode** — run the DML goal |
| `add a rule that computes factorial` | **prompt mode** — English → DML |
| anything else | prompt mode |

DML input always carries the explicit goal marker. A goal line whose payload
fails to parse is reported as a syntax error — it is never silently treated as
a prompt.

Commands (slash form; the whole-line dotted form still works):

| Command | Effect |
|---|---|
| `/halt` (or `halt.`, `/exit`, `/quit`) | exit |
| `/help` | help |
| `/listing [filter]` | list the living module (`all`, `Name/Arity`, `tools`, `scripts`) |
| `/reset` | empty the harness (refused when frozen) |
| `/rollback` | undo the last harness change (requires history) |
| `/compact [notes]` | compact the authoring memory |
| `/model [provider/id]` | show / set the default model |
| `/export [file]` | write a self-contained `.dml` harness |
| `/viz [entry] [--spec]` | draw a diagram of a skill inline (`--graphics=auto\|kitty\|iterm2\|ascii\|off`) |
| `/show <path>` | display a workspace image inline |
| `/gen <prompt>` | generate an image and display it |
| `/freeze` / `/unfreeze` | read-only lock |
| `/trace` / `/notrace` | toggle verbose logging |
| `/params` | show session parameters |
| `/reasoning [level]` | set the model reasoning level (`off|minimal|low|medium|high|xhigh|max`) |
| `/show-reasoning [mode]` | how much reasoning to display (`off|summary|full`) |
| `/harness [width]` | toggle the live harness side view |
| `/spawn <goal>` (`/serve`) | start a session-scoped subagent |
| `/ps [--watch\|--logs]` | list subagents; `--watch` pins the panel, `--logs` follows their output |
| `/ps --code [name]` | show the harness version, or the full snapshot a subagent runs |
| `/send <name> <term>` (`/cast`) | send a DML term to a subagent |
| `/call <name> <term>` | request/reply with a service subagent |
| `/stop <name>` | stop a subagent |
| `/login` | sign in to a provider from inside the REPL |

An unknown `/command` prints `unknown command: /… — try /help.` instead of being
treated as a prompt.

### Iterating over solutions

After each solution of a `/goal` the REPL pauses; in the TUI the editor border
shows `? Enter/; more · a all · . stop`. Press **Enter** (or `;`) for the next
solution, `a` for all, and `.`/`q` to stop. Lines typed while the goal is still
running are buffered and used for the next pager prompt instead of being lost.
Up/down recall earlier inputs from the session — and, when history is enabled,
from the last session (the one `--session last` would resume).

## What a harness is

A harness is a **living library of reusable DML skills** for the workspace, not
a single program. A skill is a predicate; the ones a user should call are
named descriptively and run with the authoring agent's `execute` tool or `/goal`. Skills compose:

- **helper predicates** (plain Prolog/CLP),
- **semantic judgments** (`choose`/`rate`/`verify`/`probability`),
- **host tools** via `exec/2`, and
- **model calls** via `task/N` — a bounded agentic turn that may itself invoke
  other skills.

Because the module is a live Prolog program, **data = code**: small/medium
structured data belongs in it as facts (`country(ch, "Switzerland").`),
inspected and queried exactly like predicates via `listing`, `/goal`, and
`execute`. Keep large datasets in a file or a linked script rather than bloating
the module.

A harness may also carry **test predicates** that pin the intended behavior of a
skill (for example `test_<name>`, or a small `check/2` runner). When a skill is
changed, its test is updated or added and run, so the change is verified rather
than assumed.

The authoring agent therefore treats the module like a codebase: it **reuses and
composes** existing skills rather than duplicating them, and it **changes the
harness only when the request asks for it**. On a fresh workspace it creates
what was requested; on an existing harness it changes only what the request
requires (a plain question changes nothing). A frozen harness is a live agent
that simply cannot be modified.

## Goal mode

```text
dml> /goal member(X, [a, b, c]).
X = a ;
X = b ;
X = c.
```

Each solution is rendered as `Name = value`; unbound variables show as `_`.
The pager accepts **Enter** or `;` (next), `a` (all), and `.`/`q` (stop). Use
`--first` to print only the first solution.

## Prompt mode

```text
dml> add a rule that computes factorial and a tool that formats integers

Let me list the harness first.
  ⚙ listing(filter=all)
  ⚙ define(fact(N, F), ...)
  ⚙ execute(fact(5, F))
I'll commit it now.
  ⚙ end_turn()
committed fact/2, demo/0
```

A prompt is an **edit transaction**:

1. the authoring agent opens it with `begin_turn` (snapshots the living module);
   the editing tools (`define`, `assert`/`assert_many`, `retract`, `delete`) are
   **refused** while no transaction is open;
2. the agent edits through those tools and tests with `execute`;
3. `end_turn` commits the change and records a version; `rollback` discards it.
   After `end_turn` the transaction is closed, so further edits need a new
   `begin_turn`;
4. if the agent loop ends while a transaction is still open, the host **nudges**
   the agent to `end_turn` or `rollback` (a few times). As a last resort it
   commits so nothing is lost, showing `commit transaction (host)`;
5. a failed or aborted run restores the harness unchanged.

This makes every change an explicit, visible `begin_turn` … `end_turn` unit.

For a larger change the agent is asked to commit working pieces incrementally,
so a late failure does not lose earlier progress.

### Cancelling a turn

Press **Esc** while a turn is running to abort it. (Escape keeps its usual
meaning while a completion list or a search or overlay is open.) The turn stays
transactional: the harness is restored to its last committed state, and the
*current* module (including any scratch `assert`/`execute` edits) is written to
a temp `.dml` file so nothing is lost:

```text
esc
(interrupted — Ctrl-C exits)
ERROR: turn aborted; current harness saved to /tmp/deepclause-repl-<id>-<ts>.dml
```

**Ctrl-C** exits the REPL (aborting a running turn first, so nothing keeps
writing after the UI is gone). In the plain/readline path there is no raw key to
distinguish Escape from an editing sequence, so Ctrl-C exits there too.

Load that file with `deepclause repl -l <file>` to resume. The same applies to
`/goal` queries and `-g` runs.

### Saving on exit

When you leave the REPL — `halt.`, **Ctrl-D**, or **Ctrl-C** — the
harness (module + linked scripts, as a `.zip` bundle) is written to a temp file
and the path is printed, so the work is preserved:

```text
dml> halt.
harness saved to /tmp/deepclause-repl-<id>-<ts>.zip
```

It is skipped when the harness has no predicates, when `--export <file>` was
given, or with `--no-save-on-exit`. Reload a bundle with
`deepclause repl -l <file>.zip`.

A turn that ends **without a commit** — iteration cap, exhausted model-call
budget, or a model error — is rolled back, so the harness is left unchanged and
the turn reports the error.

### Authoring agent

Plain-English requests are authored by a normal TypeScript agent with a system
prompt and harness tools. It opens a transaction with `begin_turn`, edits the
live harness through `listing` / `define` / `assert` / `assert_many` / `retract`
/ `delete`, tests with `execute`, and commits with `end_turn` (or discards with
`rollback`). Editing is refused while no transaction is open, and the host
nudges the agent if the run ends with one still open; it can run several
committed turns in one request.

#### Steering a running turn

While a turn is running you can keep typing. Submitting a line **steers** the
running agent (pi-style): the message is injected before the next model call,
and an in-flight model call is interrupted so the agent sees it immediately. The
status bar shows `running (type to steer)`, and the message is echoed as a
full-width `Steering: …` block. (While an `input/2` prompt is open,
submitting answers that prompt instead.)

#### Workspace `AGENTS.md`

If the working directory contains an `AGENTS.md`, its contents are appended to
the agent's system prompt under `PROJECT INSTRUCTIONS (AGENTS.md)`, so
repo-specific rules apply automatically. The startup banner shows
`project context AGENTS.md (N bytes)` when one is loaded.

#### Customizing the system prompt

The agent's system prompt is **one file**: the bundled
`src/system/assets/docs/AGENT_PROMPT.md`. It is used verbatim — there is no
template substitution or separate embedded reference. It covers what DML is, the
module/skill model, how to inspect/execute/edit a module, the host tools
(`bash` via `exec`), the `tool/2` annotation, file I/O, a condensed DML
reference, common patterns, and the editing rules. The full reference and the
pattern library are available at runtime through the `reference(topic)` tool
(`language`, `authoring`, `forms`, `patterns`, `examples`, `snippets`, `tools`).

To override the prompt **without rebuilding the SDK**, create
`.deepclause/system/AGENT_PROMPT.md` in the workspace; the runtime prefers it
over the bundled file (used verbatim). It is an opt-in override (never seeded),
so upgrading the SDK keeps the bundled prompt current for everyone who has not
created one.

To change the built-in prompt itself, edit
`src/system/assets/docs/AGENT_PROMPT.md` and run `npm run build`.

### Guard rails

The authoring agent is restricted to `listing`, `begin_turn`, `end_turn`,
`rollback`, `define`, `assert`, `assert_many`, `retract`, `delete`, `execute`,
and `reference`. It has no file or shell tools: all file I/O goes
through DML (`write_file/2`, `make_directory_path/1`, …) or `bash`. The
system prompt is the single bundled `AGENT_PROMPT.md` (see above). The catalog
pairing rule is explicit: **only the `exec(...)` names in the catalog
exist — by default just `bash`** — so the agent never invents
`calculator`/`fetch_url`/`web_search`/`vm_exec`. The rules that matter most:

* **Read the reference first** — before writing or changing DML, consult the
  DML language reference and the authoring guide (embedded in the prompt by
  default, or via `reference("language")`/`reference("authoring")`). Never write
  DML from memory.
* **Read the skill description first** — `module_doc/1` is the harness's skill
  description, surfaced as the first manifest line (`- skill: ...`). The agent is
  told to read it at the start of the session and re-read it whenever the harness
  changes. A file with only `module_doc/1` is still a valid skill.
* **Read before writing** — call `listing` first.
* **Clause order matters** — a base case must precede a recursive rule. Define a
  predicate with one `define`/`assert_many` call, which asserts the whole block in
  the written order, rather than one clause at a time.
* **Verify** — call `listing(Name/Arity)` after changing a predicate.
* **Commit incrementally** — `end_turn` each working piece before moving on.
* **Propose follow-ups** — after one-off work (e.g. `bash`/`curl` research),
  suggest the reusable predicate it implies; commit it only when the user agrees.
* **Bounded iteration** — no unbounded loops/assert/execute; use finite
  recursion, CLP(FD)/CLP(Q), or an explicit bound. `execute` is gas-limited and
  the turn has a model-call budget.

DML `input/2` / `exec(ask_user(...))` prompts in the editor (the question shows
in its top border) and reads through the REPL's own input line. The authoring
agent has no `ask_user` tool — it asks clarifying questions in its final
message:

```text
dml> ask me for my name
I can do that — what name should I store?

dml> call it "Andreas"
```

### Direct questions

For a simple question that can be answered directly — "what did we just do?",
"what can you do?", "how do I …?" — the agent answers **without touching the
harness**: it replies in its final message and does not `end_turn`. The reply is
rendered as streamed text and the living module is left byte-identical.
Build/commit only happens when the request asks you to create, change, or run
logic.

```text
dml> what can you do?
I'm the DeepClause REPL harness author. I grow and maintain a living DML module …
dml> listing.
% (empty harness)
```

### Shell timeouts and timing

`bash` runs out-of-process, so it can be bounded by wall clock:
`exec(bash(command: Cmd, timeoutMs: N), R)` kills the command after `N` ms
(SIGTERM, then SIGKILL) and returns exit code `124`. `DC_HOST_SHELL_TIMEOUT_MS`
or `shell.timeoutMs` sets the default (0 = disabled); `DC_HOST_SHELL_IDLE_TIMEOUT_MS`
or `shell.idleTimeoutMs` sets the no-output (idle) timeout (default 180 s). A
shell result carries `timeMs` (its wall-clock duration), and the authoring
agent's `execute` result carries `elapsedMs`.

### Reasoning and markdown

Assistant text is rendered as **markdown** — headings, fenced and inline code,
bold/italic, links, lists, and blockquotes (disable with `markdown: false`).
Model **reasoning** is streamed as italic grey; `repl.showReasoning` or
`--show-reasoning` controls it:

* `off` — hide it;
* `summary` (default) — a bounded preview (~600 chars per block);
* `full` — everything.

`--no-stream` hides both streamed text and reasoning.

### Entry points

There is **no `agent_main` requirement** in the REPL. Name predicates
descriptively and run any of them directly with `/goal <name>(…)`:

```prolog
weekly_report :-
    gather_data(Rows),
    summarize(Rows, Summary),
    answer(Summary).
```

Running is the authoring agent's job: while authoring a turn it calls the
`execute` tool to run and verify a goal; a commit only changes the module
(nothing is auto-run). If the same file should also run with `deepclause run` /
`/dc-run`, define `agent_main` in the harness.

### Self-containment and bundles

The harness is the `.dml` **plus the files it links**. Materialize each program
with a DML goal (the `execute` tool on `make_directory_path(...), write_file(...)`),
link it with a `script_file/2` fact, and run it with `exec(bash(...))`:

```prolog
script_file(url_reader, ".dc-repl/scripts/url_reader.py").

tool(url_text(Url, Text), "Fetch a URL and strip HTML.") :-
    script_file(url_reader, Script),
    make_directory_path(".dc-tmp"),
    write_file(".dc-tmp/url.txt", Url),
    format(atom(Cmd), "python3 '~w' '.dc-tmp/url.txt'", [Script]),
    exec(bash(command: Cmd), Result),
    get_dict(stdout, Result, Text).
```

The untrusted URL is passed through a file rather than interpolated into the
shell command. `script_file/2` has no runtime semantics — it tells the exporter
which files belong to the harness. `write_file/2` writes to the workspace
immediately and does not create parent directories, so the agent can run a
script with `bash` in the same turn.

`export harness.zip` writes the self-contained bundle (`harness.dml` + every
linked file + a manifest); `-l harness.zip` extracts and loads it.
`export harness.dml` auto-inlines the linked files as `embedded_file/2` facts so
a single file stays portable. See
[`DML_REPL_SCRIPT_BUNDLES.md`](./DML_REPL_SCRIPT_BUNDLES.md).

## Memory

Two conversations are kept apart:

| Memory | Owner | Contents |
|---|---|---|
| **Harness memory** | object-level `task` predicates and goal runs | the harness's runtime conversation |
| **Agent memory** | the authoring agent | your requests and the agent's replies |

Keeping them apart is a core design decision: the agent authoring the harness
does not accumulate the harness's runtime conversation, and the harness runtime
never contains authoring chatter.

The agent's conversation is a clean `messages[]`: it appends `user: <your raw
request>` and the assistant's replies, and is persisted in
`sessions/<id>/harness.json` so `--session` resumes it. The system prompt
(`AGENT_PROMPT.md` plus the workspace `AGENTS.md`) is **ephemeral**.
`execute` runs against a snapshot of harness memory and discards its writes, and
a goal run is the only place harness memory advances.

## Harnesses

A project can hold several **harnesses**, each with its own content and history:

- `--harness <name>` selects one (default `main`); `repl.harness` sets the default.
- `-l <file>` targets the harness named after the file (`research.dml` →
  `research`), creating it if needed: the loaded file becomes that harness's
  **baseline**, so a harness brought in from elsewhere works without prior history.
- A fresh session loads its harness's `harness.dml`, so it continues what is there
  (the agent does not re-create existing skills).
- `/rollback` walks the current harness only and floors at its baseline.

Each harness lives in `.deepclause/repl/harnesses/<name>/`: `harness.dml` (current
content), `baseline.dml` (version 0 / rollback floor), `HEAD.json`,
`changes.jsonl`, and `versions/`.

## History

Every session, turn, event trace, and harness version is persisted under
`.deepclause/repl/` (configurable via `repl.history`). The durable links live in
the harness itself as `harness_*` facts, so a skill can query what happened
before:

```prolog
recent(N, Cs) :-
    findall(V-Ts, harness_change(V, Ts, commit, _, _), All),
    last_n(All, N, Cs).
```

`harness_session/5`, `harness_session_end/2`, `harness_turn/5`,
`harness_change/5`, `harness_changed/4`, `harness_head/4`,
`harness_rollback/5`, and `harness_trace/3` are system-owned: queryable from
DML, but never committed, exported, or listed as harness predicates
(`listing(history)` shows a tail). A new session loads the persisted
`history.dml` at startup. See
[`DML_REPL_SESSION_HISTORY.md`](./DML_REPL_SESSION_HISTORY.md). Set
`repl.history.enabled: false` (or omit `history` when embedding) to disable it.

`rollback.` restores the previous version (moving `harness_head` back one), so
repeating it walks the harness history backwards; it is refused when frozen and
reports `no harness changes to roll back` when there is nothing to undo.

## Export, freeze, and `-l`

**`export`** serializes the living module into a self-contained, human-readable
`.dml` file: directives first, then tools, then embedded scripts, then user
predicates sorted by `name/arity`. Exported files are deterministic
(variables number as `A`, `B`, …) so `export → load → export` is byte-stable.

**`freeze`** is a runtime read-only lock, not a serializer. When frozen:

* `assert`/`retract`, `consult`/`[file]`, prompt commits, and `reset` are
  refused;
* read-only goals and `execute` (including nested `task` calls) keep working, so
  a frozen harness is still a live agent — just immutable.

`-f`/`--freeze` starts frozen; `--locked` additionally forbids `unfreeze.`.
`-l file` loads a DML file (repeatable) before the loop or `-g`.

Freeze is an **integrity lock, not a sandbox**. It prevents mutation of the
in-memory module; it does not stop `bash` from writing files in the workspace.

## Asking the human

Both levels share one input channel:

* the authoring agent asks clarifying questions in its final message (it has no
  `ask_user` tool);
* the harness may call `input/2` or `exec(ask_user(prompt: …))` while running.

In an interactive TTY the question is prompted. In non-interactive runs (`-g`,
piped stdin, CI) `input/2` fails instead of blocking; `--no-ask` fails
immediately, and `--answer "…"` supplies a canned reply. `ask_user` is exempt
from the capability tool policy: it only relays a question to the human.

## Backends

The **`pi` backend is the default**. It does not run an LLM of its own: it
borrows pi's **model catalog and credential store**.
`@earendil-works/pi-coding-agent` and `@earendil-works/pi-ai` are regular
dependencies of the SDK, so **no global `pi` install is needed** — a globally
installed `pi` is deliberately ignored so the version is deterministic. The
`env` backend (the SDK's own provider/API-key path) is still supported.

```bash
# Sign in (the terminal port of pi's /login) and pick a default model.
deepclause login
deepclause login --provider anthropic                 # skip the provider picker
deepclause login --provider openai --api-key sk-…      # non-interactive API key
deepclause login --model anthropic/claude-sonnet-4     # skip the model picker
deepclause login --no-model                            # authenticate only

deepclause logout [provider]                           # pi's /logout

# Inspect / switch:
deepclause set-backend status
deepclause set-backend pi --list [--json]              # providers + chat models
deepclause set-backend env                             # SDK provider/API-key backend
```

Login flow (`deepclause login`, `set-backend pi`):

1. **Provider picker.** The readline picker filters instead of listing the whole
   catalog; entries show the auth method (API key / OAuth/subscription) and
   whether the provider is already configured.
2. **Auth method.** Chosen automatically when a provider offers only one; a
   short prompt picks between API key and OAuth/subscription otherwise.
3. **pi login.** pi's own flow runs (API-key prompt, OAuth URL, or device code)
   and stores the credential through pi (`~/.pi/agent/auth.json` by default;
   `backend.agentDir` overrides it).
4. **Model picker.** Always shown (unless `--model`/`--no-model`), filtered by
   the picker; the choice is written to `.deepclause/config.json` under
   `backend` (`kind`, `model`, `agentDir`, `modelsPath`, `authPath`).

With `backend = pi`, the REPL never asks for API keys, never writes provider
environment variables, and never guesses a provider. `deepclause login --json`
is available for scripted use.

### Judgment backends

Semantic judgments (`judge/2`, `choose/4`, `rate/4`, `verify/3`,
`probability/3`, `holds/2,3`) answer bounded, typed questions about an explicit
state and never read or write conversation memory. The REPL registers two
backends:

- **`llm`** (default): reuses the active model and backend — the pi model and
  credentials when `backend = pi`, otherwise the configured provider.
- **`jev`** (TypeSafe System One): opt-in, because it is a separate service.
  Enable it and provide its key:

```json
"judgment": {
  "default": "llm",
  "jev": { "enabled": false, "model": "jev-latest", "apiKeyEnv": "TYPESAFE_API_KEY" }
}
```

`--judge <name>` (or `judgment.default`) selects the default backend. If the
chosen backend is not registered the REPL fails at startup with a clear error;
if `jev` is enabled but its key is missing the REPL warns and uses `llm`.
`require_judgment(Capability, Goal)` gates a goal on backend capabilities
(`calibrated`, `probability`, `confidence`, `independentQuestions`).

## Host tools

The default capability host tool is exactly **`bash`**, registered through the
SDK's shell manager with the configured sandbox. `--tools a,b` or
`--no-tools` change the set; MCP servers contribute their own tools when
configured. **File reading and writing are DML language, not host tools** — use
`open/3`, `close/1`, `read_string/3`, `write/2`, `write_file/2`, `read_file/2`,
`append_file/2` (paths resolve under the workspace).

`bash` runs in the workspace shell, or a sandbox (`--sandbox` uses the AgentVM;
`agentvm.network` controls networking). The authoring agent may explore that
environment and install a dependency when a skill genuinely needs one — installs
are sandbox/session-local and do **not** travel with an exported harness, so
prefer the standard library.

The meta prompt carries the live host tool catalog, and `listing(tools)`
returns it, so generated `exec(...)` calls use real names and argument shapes.

## CLI reference

```text
deepclause repl [options]

  -g, --goal <goal>        run one goal and exit (repeatable)
  -l, --load <file>        load a DML file before starting (repeatable)
  -f, --freeze             start frozen (read-only lock)
      --locked             like -f, but forbid unfreeze.
      --export <file>      export the harness on exit
      --first              with -g, print only the first solution
      --plain              no TUI formatting (auto when stdout is not a TTY)
      --tui <mode>         interactive UI: auto|on|off
      --json               emit results as JSON lines
      --workspace <path>   workspace root (default ./)
      --harness <name>     harness to edit (default main; a project can hold several)
      --backend <kind>     override the configured backend (env|pi)
      --judge <name>       default judgment backend (llm|jev)
      --model <model>      override the configured run model
      --provider <provider>
      --thinking <level>   reasoning level (off|minimal|low|medium|high|xhigh|max)
      --temperature <n>
      --no-stream          hide streamed model text/reasoning
      --show-reasoning <m> how much model reasoning to show: off|summary|full
      --no-events          hide activity events
      --no-save-on-exit    do not save the harness on exit
      --sandbox            run shell tools in AgentVM
      --tools <list>       capability host tools (default: bash)
      --no-tools           register no capability host tools
      --no-ask             make ask_user / input/2 fail instead of prompting
      --answer <text>      canned answer for any input request
      --interactive-input  allow input requests in non-TTY/--json mode
      --gas-limit <n>      per-frame step limit
      --depth <n>          maximum nested frame depth (default 3)
      --max-model-calls <n> maximum model calls per turn (default 100; 0 = unlimited)
      --param key=value    session parameter (repeatable)
      --verbose / --debug
      --no-color
```

### `--json`

In JSON mode each runtime event is emitted as one JSON object:

```json
{"type":"stream","content":"Let me look at the harness."}
{"type":"tool_call","toolName":"listing","toolArgs":{"filter":"all"}}
{"type":"solution","solution":{"bindings":[{"name":"F","value":"120"}],"deterministic":false}}
{"type":"answer","content":"120"}
```

### Budgets

A prompt turn is bounded by `--max-model-calls` (default 100) and `--depth`
(default 3 nested frames). When the model-call budget is exhausted in an
**interactive** session, the authoring agent asks whether to continue for another
`maxModelCalls` calls (and reminds you of the setting); answering no — or running
without an input channel (piped/`--no-ask`) — aborts the turn with an error and
leaves the harness unchanged, and the counter resets on the next prompt. Raise the
limit with `"repl": { "maxModelCalls": N }` in `.deepclause/config.json` or
`--max-model-calls N` (`0` = unlimited).

```bash
deepclause repl --max-model-calls 100        # raise it for a turn
deepclause repl --max-model-calls 0          # unlimited
```

Or persistently in `.deepclause/config.json` (`0` = unlimited):

```json
"repl": { "maxModelCalls": 100 }
```

### Scripted sessions

```bash
cat prompts.txt | deepclause repl -l seed.dml --export out.dml --no-ask
```

In non-TTY mode input is read line by line until EOF; `input/2` never
blocks.

## Configuration

```json
"repl": {
  "exportOnExit": false,
  "exportPath": "harness.dml",
  "frozen": false,
  "locked": false,
  "editMode": "transactional",
  "maxDepth": 3,
  "maxModelCalls": 100,
  "writeDepth": 20,
  "repairRounds": 1,
  "promptPrefix": "dml> ",
  "tui": "auto",
  "render": "rich",
  "showReasoning": "summary",
  "showToolArgs": true,
  "maxToolResultChars": 2000,
  "color": true,
  "tools": null,
  "compaction": {
    "enabled": true,
    "auto": true,
    "reserveTokens": 16384,
    "keepRecentTokens": 20000
  },
  "history": {
    "enabled": true,
    "dir": ".deepclause/repl",
    "trace": "summary",
    "maxEventBytes": 8192,
    "maxSessions": 50,
    "maxVersions": 200,
    "includeInExport": false
  }
},
"judgment": {
  "default": "llm",
  "jev": { "enabled": false, "model": "jev-latest", "apiKeyEnv": "TYPESAFE_API_KEY" }
}
```

## Embedding

```ts
import { createHarnessRepl } from 'deepclause-sdk';

const repl = await createHarnessRepl({ model: 'gpt-4o', provider: 'openai' });
for await (const event of repl.query('fact(5, F)')) {
  if (event.type === 'solution') console.log(event.solution?.bindings);
}
await repl.dispose();

// Judgment: an `llm` backend is built from the injected LLMBackend; pass
// `judgeSelection` to register custom/`jev` backends, or `jev: { enabled, apiKey }`.
```

The SDK also exports `DMLSession`, `createPiBackend`/`createPiAdapter`,
`createReplToolset`/`buildToolCatalog`, and the export helpers for embedding.

## Self-hosting demo

A harness whose only tooling is a linked script file survives a clean
workspace, because the file travels inside the bundle:

```bash
# 1. Author a harness whose program is a linked file.
mkdir -p .dc-repl/scripts
printf 'the script file ran\n' > .dc-repl/scripts/writer.sh
cat > seed.dml <<'DML'
script_file(writer, ".dc-repl/scripts/writer.sh").
run_writer :-
    script_file(writer, Script),
    format(atom(Cmd), "sh '~w'", [Script]),
    exec(bash(command: Cmd), R),
    get_dict(stdout, R, Out),
    output(Out).
DML

# 2. Export the self-contained bundle.
deepclause repl -l seed.dml --export harness.zip --no-ask -g "true"

# 3. Clean the workspace and run from the bundle.
rm -rf .dc-repl seed.dml
deepclause repl -l harness.zip -f -g "run_writer"
```

The second run prints the script's output even though `.dc-repl/` and `seed.dml`
are gone: `export` bundled the linked file, and `freeze` guarantees the loaded
harness cannot rewrite itself.

The equivalent automated round-trip lives in
`tests/repl-freeze-export.test.ts` (script-file bundle round-trip).
