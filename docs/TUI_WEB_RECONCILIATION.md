# TUI / Web reconciliation — implementation status and original proposal

**Status: core P0/P1 reconciliation implemented locally; not published or pushed.**
See [Work mode](./WORK_MODE.md) for current commands, APIs and migration.

Implemented: shared Work validation/prompt/allowlist/codecs, typed truthful results,
deterministic named invocation, durable session purpose/harness/title and workflow
runs, pinned snapshots, transactional shell-editor saves, TUI Work/pickers/panel,
web migration, shared replay, suspension controls, cancellation ownership and
source/artifact build compatibility checks. The accepted no-shell and manual-start
decisions below are enforced, not merely documented.

Validation includes shared Node fixtures, real headless CLI starts/mode changes,
and the actual browser-worker engine path, including foreground forms/input,
structured history, Work-safe resume, suspension and cancellation recovery.

Deferred P2: a full terminal visual Shell editor, multiple simultaneous live TUI
conversations, richer cross-host workspace transfer UI, and a complete typed
replacement of the legacy Develop-session proxy, cross-process writer leases,
and root-worker isolation for preempting native CPU calls in the Node TUI. Browser
Stop has a bounded hard-termination fallback; Node foreground remains in-process.
New Work APIs use an explicit
async facade; old authoring/UI methods retain compatibility shims. Object/array
Work fields and background-worker forms remain explicitly unsupported. Automatic
storage/credential synchronization is not added. Release/push requires approval.
The hero/intro previews remain separate and unintegrated.

The remainder records the original audit and design; descriptions of pre-change
defects are historical, not statements about the completed implementation.

Audit baseline: SDK `feat/authoring-reference-overhaul` at `f7a3dc2`, web at
`17abee6`. Local code and history were inspected; no remote fetch or merge was
performed. The separate intro/hero previews are outside this proposal and remain
unintegrated.

## 1. Recommendation

Build **one shared Work contract and execution service in the SDK**, with TUI and
web as adapters. Do not port the web controller wholesale, implement Work by
freezing an authoring agent, or maintain a second set of Work semantics in the TUI.

The missing TUI interface is only part of the gap. Fix identity, argument and
execution semantics before making the same inconsistencies available in a second UI.

Keep three distinctions explicit:

- **Session purpose:** Develop or Work; immutable for a conversation.
- **Work presentation:** agent or workflow-only (`shell.kind`).
- **Execution context:** authoring scratch test, foreground workflow, or background
  process. Form availability, memory, cancellation and limits depend on this.

## 2. What already exists

| Area | Current implementation | Reconciliation needed |
|---|---|---|
| Shell language | SDK `ShellSpec`, Prolog normalization of `shell/1` and legacy `shell/3`, `session.shell()` | Central validation, compatibility diagnostics and effective-shell resolution |
| Work model tools | `createWorkAgentTools`, `workMode: true`, typed DML wrappers and `run_workflow` | Complete policy, codecs, outputs and default Work prompt |
| Foreground interaction | `executeForeground` and root form/input callbacks; foreground form test passes | A shared operation API, explicit state semantics and host parity tests |
| TUI | Authoring REPL, goal runner, pickers, forms, process panel, history | Work entry, harness/workflow selection, Work prompt routing and presentation |
| Web | Work chooser, agent/workflow views, workflow argument dialog, shell editor, stale-version notice | Remove duplicated runtime rules and persist Work identity/runs |
| Authoring context | Latest SDK/web changes already share compact prompt cores, focused references and canonical fixtures | Preserve this work; correct remaining stale status/help text rather than redo it |
| Persistence | Same `.deepclause/repl` file layout | Same layout is not yet sufficient for portable session identity or mode |

Relevant code:

- SDK: `src/repl/index.ts`, `session.ts`, `harness-tools.ts`, `history.ts`;
  `src/types.ts`; `src/prolog-src/deepclause_mi.pl`.
- TUI: `src/cli/repl.ts`, `src/repl/tui/{autocomplete,status-bar,renderer}.ts`.
- Web: `src/state/controller.ts`, `src/components/{WorkMode,ShellEditor}.tsx`;
  `src/platform/dml/{engine,history-query}.ts`;
  `src/platform/fs-service/{runtime.worker,runtime-client}.ts`.

### Confirmed correctness gaps

1. **CLI never enables Work.** Its `createHarnessRepl` call does not pass
   `workMode`; its parser, help, completion and status have no Work mode.
2. **Work prompt is web-owned.** `workMode` switches tools in the SDK, but the
   browser separately builds `workSystemPrompt`. An SDK caller setting only
   `workMode: true` still gets the normal authoring prompt by default.
3. **Additional tools bypass the Work declaration.** `getHarnessAgent()` merges
   `authoringTools` even in Work. The browser supplies granted MCP tools here.
   MCP grants still apply, but the resulting list is broader than `shell.tools`.
4. **Two argument builders disagree.** Web `workflowArgValue` allows a file path
   to fall through as raw DML text. SDK `workArgValue` quotes files, but silently
   substitutes invalid numeric values and stringifies complex values.
5. **False success.** SDK `workResult` considers absence of an error sufficient
   for success, even with no solutions; it substitutes empty strings for missing
   outputs. Browser runs likewise should not infer success merely because an
   event iterator ended.
6. **User workflow starts are not uniform.** Agent-shell buttons send a natural
   language instruction to the model; workflow-only buttons build and run a goal.
7. **Missing shells expose too much.** The web synthesizes workflows from almost
   every ordinary predicate and guesses that the last argument is an output. This
   conflicts with the new authoring reference's explicit Work boundary.
8. **Session identity is incomplete.** SDK `session.json` has no harness/mode;
   the browser uses localStorage plus source matching to infer the harness. Work
   identity is live UI state only. Generic resume does not reconstruct a Work
   session and its restricted tool/prompt configuration.
9. **Workflow runs are transient in the browser.** `openRunnerSession` deliberately
   omits history, so the Work run list is not a durable cross-host run history.
10. **Version selection can drift.** The web remembers the selected shell/version,
    but workflow-only starts load current harness source unless a version is
    explicitly passed. A stale shell can therefore describe a different program
    from the one that runs.

A read-only probe against the current built SDK produced:

```text
Advertised inputs: Count: number (described as integer), Payload: object
Supplied values:   Count = 1.5, Payload = { topic: "test" }
Generated goal:    probe(0, "[object Object]", Out1)
No-solution result: { ok: true, outputs: { Result: "" }, output: "" }
```

The existing shell/tools/foreground test suites pass (11 tests); their coverage
currently does not catch these failure cases.

## 3. Shared SDK Work layer — first implementation slice

Create a browser-safe Work module, with an async session/host interface rather
than relying on a cast of a browser RPC client to synchronous `DMLSession`.
Names below are illustrative, not a request to add every helper as public API.

### Contract and policy

- Resolve one normalized Work shell from the actual loaded harness snapshot.
- Validate kind, goal existence and **exact arity**, supported types, unique names,
  reserved/provider-compatible tool names, and input/output declarations.
- Generate the Work prompt from the shell persona and shared Work rules. It must
  not inherit the authoring manual just because an embedding forgot an override.
- Construct one Work tool allowlist. Never append arbitrary authoring tools.
  If direct MCP access is desired, make it an explicit Work policy and require
  both declaration and the existing host grant. DML-internal host grants remain.
- Keep the three tool surfaces distinct: `shell.tools`, nested-agent `tool/2`,
  and `exec/2` host capabilities. `host_tools: [show_image]` is not a promise to
  expose the entire host registry. Diagnose obsolete/ignored entries such as
  legacy `execute` rather than silently implying they are active.
- Work restricts the assistant's model-facing interface; it is not a sandbox for
  arbitrary DML. Declared workflows still require the host's workspace, sandbox,
  network and approval boundaries.

### Arguments and results

- One codec for manual starts, assistant tools and both frontends.
- Treat strings and file paths as **data**, never raw Prolog source. Handle quotes,
  whitespace and Unicode; apply the host's workspace path policy.
- Validate integers, numbers and booleans; reject invalid/missing inputs instead
  of silently producing zero, false or an empty string.
- Either implement real JSON object/array conversion end to end or reject/hide
  these types until supported. Do not advertise an object schema and stringify it.
- Return named, typed output values. Distinguish no solution, exception, unbound
  output, invalid output type, cancellation and suspension.
- Zero-output side-effect workflows are valid when the goal actually succeeds.
- Preserve old shell forms through a tested compatibility normalizer. Keep old
  arity heuristics out of the canonical execution path and offer explicit migration.

### Invocation

Provide one foreground `runWorkflow(name, inputs, options)` operation usable by
UI buttons, slash commands and the assistant's `run_workflow` tool.

- A **manual start is deterministic**: execute exactly the selected workflow and
  inputs without asking a model to decide whether/how to run it.
- An agent may select a workflow in conversation through its typed tool.
- A manual run may optionally be summarized by the assistant afterward. Record
  the external result in conversation using a supported context mechanism, not
  fabricated provider tool-result messages without matching tool calls.
- Carry structured outputs, events, usage, status, owner and snapshot identity.
- Serialize operations against the same foreground session; cancel only the
  selected operation. Do not let an authoring query and workflow share an engine
  concurrently or close another session's form.

### Execution context

Retain the browser's hard-bounded scratch worker, but make the seam an explicit
host execution adapter rather than a monkey-patched `session.execute` method.

`executeForeground` already makes root forms/input available, but currently calls
`executeGoal`, which uses a nested frame and restores harness memory. Decide and
document foreground workflow memory/state semantics explicitly, then test them
on both hosts. Do not assume the method name means it already has independent
session/history semantics.

## 4. TUI Work experience

### Minimal entry and navigation

- Preserve the existing default: `deepclause repl` opens **Develop**.
- Add `deepclause repl --mode work --harness example`.
- Add `/mode work|develop [harness]`; show a chooser when necessary.
- Add `/workflows` (list/picker), `/workflow <name>` (start/prompt for inputs), and
  `/details` (toggle technical transcript detail).
- Do **not** repurpose `/harness`: it already toggles/resizes the source side view.
- Help, completion, plain-TTY handling and JSON output use the same command
  registry/dispatch semantics. Noninteractive missing inputs fail clearly rather
  than hanging on an invisible picker.

### Agent shell

Show the welcome Markdown, Work chat and declared workflows. Tools can be listed
for discovery but are not individual sidebar actions. Ordinary text goes to the
Work assistant, never to the authoring agent.

### Workflow-only shell

Show the workflow chooser and a run transcript, not a pretend chat assistant.
Typed inputs use the existing form/picker infrastructure. A file input uses path
completion/selection under the workspace; both hosts pass the same relative path
value, despite having different file picker UIs.

### Presentation and lifecycle

- Footer: **Work/Develop**, harness, loaded version/hash, current operation/model,
  cancellation state and usage. Errors are never hidden by the Details setting.
- Wide terminal: workflow/session panel. Narrow terminal: chooser overlay; do not
  require a web-like permanent sidebar.
- Reuse existing forms, input channel, process status and transcript renderers.
- Treat mode changes as choosing another conversation, not toggling a boolean on
  a cached agent. Keep Develop and Work conversation memory/tool maps separate.
- MVP can require idle state or an explicit cancel before switching. Do not
  silently dispose running processes. Full multiple-live-session tabs can follow.
- Raw goal/edit/rollback operations require an explicit Develop/advanced action;
  use the same operator-command policy in web Work instead of merely hiding UI.

A full terminal clone of the visual Shell editor is **not** required initially.
Users can edit `shell/1` through Develop. The web editor should save through the
same SDK validation/transaction API, with an expected base hash, rather than
mutating whichever same-harness session it happens to find or directly writing
unvalidated source when none is open.

## 5. Durable identity, versions and resume

Extend SDK-owned per-session metadata, backward compatibly, to record:

- harness name; session purpose (Develop/Work); shell kind;
- session/run kind; loaded harness version and content hash;
- title; parent conversation/run linkage where applicable; host of origin;
- effective model identity and relevant execution profile.

Keep session purpose separate from the existing turn `mode: goal|prompt`. Workflow
turns need named invocation metadata without losing compatibility with old logs.

Then:

- Both hosts resume the recorded mode/harness, never their current global UI mode.
- Legacy sessions use explicit fallback/confirmation where identity is unknown;
  do not silently elevate a Work conversation to authoring or infer the harness
  solely by comparing mutable source strings.
- Move browser-only harness identity out of localStorage. Migrate titles and
  legacy hints non-destructively into per-session metadata where possible.
- Persist workflow runs through SDK history without creating a synthetic
  `runner` harness slot or changing the selected harness's HEAD.
- Capture shell, source and linked-script identity together. A running Work
  session stays on that snapshot. Refresh opens a new coherent snapshot; do not
  run an old shell against the newest source or hot-swap an active operation.
- Expose pending suspension/resume through the typed async host facade. The CLI
  resumes `suspend/1` continuations today; the browser has no equivalent flow.
- Restore transcript/state consistently, and explain that live processes, forms
  and file handles are not restored. Browsing history must not execute a pending
  continuation; an explicit resume action may do so.

The on-disk format can be shared; the actual storage is not: browser OPFS and a
terminal workspace are separate stores. Define explicit binary-safe workspace
export/import for transfer. Do not imply automatic synchronization or include
credentials and host caches in a generic export.

## 6. Other reconciliation

| Area | Proposed alignment | Intentional host differences |
|---|---|---|
| Runtime capabilities | Publish the actual capability catalog; diagnose unsupported calls; test portable harnesses on both hosts | TUI bash/sandbox, browser Pyodide/CORS and granted remote MCP are not identical |
| Python/scripts | Prefer the existing `run_script/3` portability seam: it already tries Python then falls back to `bash(argv: [python3, ...])` for `.py` files. Test argv, JSON output, exit status and files on both | Direct `exec(python(...))`, package/native wheel availability, browser egress and host environment need explicit requirements; a Node Python adapter is a separate decision, not required to duplicate the existing script fallback |
| Models and limits | Track effective model/context per session, separately from a newly chosen default. Forward limits, concurrency, judge selection and compaction deliberately | Credentials stay in each host's appropriate store; no automatic secret synchronization |
| Judge/config plumbing | Audit every forwarded option. Browser engine passes `jev/defaultJudge`, but the root worker's `createHarnessRepl` currently omits them; child paths do forward them | Backend availability may differ, but a selected option must not be silently ignored |
| Foreground interaction | Same form validation/cancellation, owner IDs, stop semantics and structured run outcome | Terminal widgets versus browser controls; background process forms remain explicitly unsupported until implemented |
| Events and artifacts | One event interpretation for success/failure/cancel/suspend, usage, media and transcript replay | Terminal images/file links versus browser inline HTML/media; rendering need not be identical |
| Type-safe host API | Async, browser-safe Work/session facade with a typed RPC protocol and compatibility handshake | Node session may be local; browser remains worker-hosted. Avoid casting a partial async proxy to synchronous `DMLSession` |
| Authoring/reference | Keep the newly shared prompt core, focused reference topics, small browser overlay and canonical examples | Small host capability overlays; no duplicated full manuals or promises of unavailable tools |
| Build/release | Record SDK revision, Work contract version and generated asset digest; check at build/worker startup and in diagnostics | SDK `dist` and copied browser Prolog/docs need a coordinated build, not just matching package version `0.4.0` |
| Documentation | Rewrite contradictory Work/status/help descriptions and add a shared parity matrix | Host-specific setup and limitations remain separate |

Current copied Prolog source hashes match across SDK source, SDK `dist`, and web
generated assets; this audit did **not** establish a current Prolog sync failure.
The remaining risk is build provenance: web imports SDK TypeScript output through
a sibling package link while `sync:runtime` copies other assets from source. Web
prebuild already runs sync; it should also verify that the linked SDK build and
all workers belong to the same runtime/contract revision.

## 7. Delivery order

1. **P0 — shared correctness:** Work prompt/allowlist, shell validation, one codec,
   truthful results and deterministic foreground invocation. Add failure tests
   before moving browser helpers into the SDK.
2. **P0 — durable identity and pinning:** session metadata, coherent snapshots,
   Work-safe resume, and persistent workflow runs. Provide non-destructive legacy
   readers/migration. This also fixes existing non-Work named-harness resume risks.
3. **P1 — migrate web to the shared API:** remove local prompt/goal builders and
   implicit predicate export; make shell-editor saves transactional; preserve the
   current UI while correcting execution/history semantics.
4. **P1 — ship TUI Work MVP:** mode entry, chooser, workflow inputs/run transcript,
   agent chat, Details, status and interaction/cancellation. Include plain/JSON.
5. **P1 — host contract conformance:** runtime/config propagation, browser RPC,
   resume/suspension, supported script/capability paths and build stamping.
6. **P2 — UX polish and coordinated release:** optional terminal Shell editor,
   multiple live TUI sessions, workspace transfer UX and richer previews. Release
   the reconciled SDK and web against an identifiable compatible contract.

Keep commits small. Do not merge/release/push either repository as part of merely
approving this design.

## 8. Acceptance tests

Run the same canonical Work fixtures through local Node and real browser-worker
adapters, not only stub controllers:

- Agent shell exposes only declared tools/approved helpers; authoring tools and
  undeclared extra MCP tools do not leak in.
- Workflow-only manual starts execute exactly once with **zero assistant model
  calls**; agent-triggered starts invoke the same executor.
- Strings, quotes, whitespace/Unicode file paths, booleans, fractional integers,
  scientific numbers, unsupported objects, missing inputs, overloaded predicates,
  zero/multiple outputs, unbound outputs and no-solution results.
- Foreground form submit/reject/retry/cancel and `input/2`; cancellation resolves
  pending UI promises and leaves no orphan frame/form/worker.
- Background/scratch form attempts fail clearly rather than hang.
- Resume Develop and Work after restart/transfer; preserve harness/mode/title,
  version, transcript and suspension. Do not silently restart services.
- Edit in Develop while Work is open: stale notice, pinned active run, explicit
  refresh, no mixed old shell/new source, and no cross-mode conversation reuse.
- Persist workflow outcomes correctly: succeeded, failed, cancelled, suspended.
- Config/model/judge options and effective capabilities agree with what the UI
  reports; incompatible workers/assets fail clearly.
- Narrow/wide TUI, web/mobile, plain-TTY and JSON modes; unsupported rendering has
  a useful fallback rather than pretending it succeeded.

## 9. Decisions recommended for approval

1. **No shell:** show “No Work shell defined” with Develop/Run actions by default.
   Preserve an explicitly labelled advanced legacy path, but stop exporting every
   helper and guessing outputs automatically.
2. **Manual workflow starts are deterministic**, even for agent shells; any
   assistant explanation is secondary to successful execution.
3. **Mode belongs to the conversation**, not just the visible tab or a mutable
   `workMode` flag. Both hosts read it from shared session metadata.
4. **Parity means shared semantics, not identical widgets or identical host
   powers.** Keep capabilities explicit and unsupported paths honest.
