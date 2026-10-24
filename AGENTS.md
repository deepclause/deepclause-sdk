# DeepClause: Guide for AI Agents

DeepClause is a **neuro-symbolic runtime for self-modifying agent harnesses**. A
harness is a living **DML** program — Prolog with model calls, semantic
judgments, and constraints. You (an LLM agent) can read it, extend it, test it,
and run it from an interactive **REPL**; the `run` command executes a harness
one-shot; the **TypeScript SDK** embeds the runtime.

There is **no Markdown→DML compiler**. DML is authored directly (by you), and a
harness is a normal, inspectable Prolog module.

Three surfaces: **repl**, **run**, **SDK**.

---

## 1. The CLI

```bash
deepclause init                 # write .deepclause/config.json
deepclause login                # sign in via pi, pick a default model
deepclause repl                 # interactive living-module REPL
deepclause repl --session last  # resume recorded purpose/harness
deepclause repl --mode work --harness example  # use a declared Work shell
deepclause run skill.dml "arg"  # run one entry of a DML file / .zip bundle
deepclause visualize s.dml -g router/2 --presentation -o router.png
```

`run` opens the file as a library, calls `agent_main/N` (arity = number of
positional args) or an explicit `--goal`, and streams events. `visualize` draws a
skill (`--presentation` or `--spec`) as a Mermaid diagram from its `listing` and
docs. Other commands: `set-model`, `show-model`, `list-models`, `set-backend`,
`logout`.

---

## 2. The REPL: a living harness

Two input modes:

- **Plain English** (default) — a meta task writes/test DML and commits it
  (replace-by-predicate). Use this to add skills, add tools, change logic, or
  ask about the harness.
- **`goal: <dml>.`** — query the current harness directly with Prolog/DML.

Every prompt turn is an **edit transaction**: the module is snapshotted, the
meta task edits a scratch copy and tests it with `execute`, and the result is
committed only if it succeeds. Failed turns leave the harness unchanged.

Sessions, turns, traces, and versions are recorded under `.deepclause/repl/`
and exposed to DML as `harness_*` facts (`harness_session/5`, `harness_turn/5`,
`harness_change/5`, `harness_changed/4`, `harness_head/4`, `harness_trace/3`).
`rollback.` restores the previous version.

Useful commands: `help.`, `listing.`, `listing(Name/Arity).`, `reset.`,
`rollback.`, `export <file>.`, `freeze.`/`unfreeze.`, `model.`, `trace.`,
`params.`, `halt.`.

---

### Develop versus Work

The default REPL is Develop (authoring). Work uses an explicit `shell/1`; it never
exposes all predicates or guesses outputs. `/mode` switches conversations;
`/workflows` chooses a declared task. Manual starts execute exactly that task with
validated inputs and typed outputs, without an assistant decision. Work agents do
not receive authoring tools. See `docs/WORK_MODE.md` before changing this boundary.

## 3. DML Reference for Agents

DML combines LLM calls with Prolog's hard logic.

### Core predicates

- `task(Description, ...Vars)` — model call **with** the current memory.
- `prompt(Description, ...Vars)` — model call with **fresh** memory (isolated).
- `exec(bash(command: Cmd), Result)` — run a host capability (only `bash`).
- `system(Text)` / `user(Text)` — backtrackable conversation memory.
- `answer(Text)` — terminate and commit the final result.
- `tool(Head, Description) :- Body.` — declare a DML tool for nested tasks.
- `script_file(Name, Path)` — link a workspace-relative script file (bundled on
  export; no runtime semantics of its own).

Type wrappers make model output reliable: `integer(X)`, `number(X)`,
`boolean(Y)`, `list(string(Z))`, `string(S)`, etc.

### Logic and math (CLP)

Use constraint logic programming instead of hand-rolled arithmetic:

```prolog
:- use_module(library(clpfd)).   % integers
:- use_module(library(clpq)).    % rationals
:- use_module(library(clpr)).    % reals
```

### Judgments

Semantic judgments return **typed** values backed by a judge backend:
`choose/3`, `rate/3`, `verify/3`, `probability/3` (see
`src/system/assets/docs/DML_REFERENCE.md`).

---

## 4. Using as a Library (SDK)

```typescript
import { createDeepClause } from 'deepclause-sdk';

const dc = await createDeepClause({ model: 'gpt-4o', provider: 'openai' });

for await (const event of dc.runDML(dmlCode, { args: ['topic'] })) {
  if (event.type === 'answer') console.log('Result:', event.answer ?? event.content);
}

await dc.dispose();
```

To reuse pi's model and credentials, pass an injected `LLMBackend`
(`createPiBackend`) and judge backends (`createLLMJudgeBackend`,
`createJevJudgeBackend`). The REPL is embeddable via `createHarnessRepl`.

---

## 5. Best practices

1. **Orchestrate, don't algorithm.** A skill is a short sequence of `task/N` /
   `prompt/N` calls with thin Prolog guards (`->`, `;`, `once/1`, `member/2`,
   `format/2`). Let the model extract, classify, summarise, or decide.
2. **Type-safe tasks.** Wrap `task/N` outputs (`integer(Count)`,
   `list(string(Tags))`) so the model returns valid data.
3. **Determinism.** A skill that calls `task/N`, `exec/2`, `read_media/2`, or
   writes a file must yield exactly one solution (`once/1`, `->`, `!`); otherwise
   backtracking re-runs it.
4. **Shell out for files.** Prefer `exec(bash(...))` and the host file tools over
   the DML file predicates, which are for content you must reason over in Prolog.
5. **Generalize; no environment.** Parameterize skills and never hardcode host
   paths or one-off values; add or reuse a capability rather than a one-off action.
6. **Keep the harness inspectable.** Add `doc(Name/Arity, Text)` facts (and a
   `module_doc/1`) so the manifest, `listing`, and `visualize` describe it.
7. **CLP is advanced.** Use it only for a genuine, bounded constraint problem;
   bound every domain before `label/1` — an unsatisfiable model can search forever.
8. **Author, don't compile.** There is no compiler — write valid DML directly and
   let the REPL's transaction/test/commit loop verify it.

See `docs/STATUS.md` for what exists today, and the bundled docs for the language
reference and reviewed shapes: `DML_REFERENCE.md`, `DML_AUTHORING.md`,
`HARNESS_EXAMPLES.md`, `PROLOG_SNIPPETS.md`.
