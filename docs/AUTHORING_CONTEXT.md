# Authoring context and references

The instruction budget is for a usable language mental model, not an entire
manual. Do not append another reference book to fix a missed concept.

## Sources and generation

- `src/system/assets/authoring-core.md`: shared normal/local authoring semantics.
  `local:omit` regions omit worked examples/detailed repetition, not different
  safety rules. `scripts/sync-agent-prompt.mjs` builds `AGENT_PROMPT.md` and
  `LOCAL_AGENT_PROMPT.md` with character budgets; it no longer embeds the language
  reference. Build/check fails on stale generated docs.
- `scripts/canonical-examples.mjs`: one registry of code, prerequisites, fixtures,
  expected results and failure checks. It generates `HARNESS_EXAMPLES.md`; web
  sync copies the same registry into browser tests. Do not copy it into a prompt.
- `src/system/assets/docs/DML_*.md`: focused author-facing contracts. The forms
  API source is `docs/reference/DML_FORMS.md`; `docs/DML_FORMS.md` is design history,
  no longer blindly copied to the authoring agent.
- Web: `src/platform/dml/web-authoring.md` is a small capability overlay.
  `scripts/authoring-docs.mjs` composes it with SDK profiles, without broad regex
  surgery. Other references are platform-neutral; executable browser examples
  are linted. Task/chat subagents do not receive authoring instructions.
- Model discovery uses a bounded four-entry preview; the models tool exposes the
  full configured catalog on demand rather than injecting up to 60 entries.
  Runtime/clock, actual tool schemas and user-owned project context
  are additional live context. They are not counted as source prompt text; record
  provider token usage when measuring the complete request. Existing user project
  instruction files are not migrated/overwritten by this work.

Run `npm run sync:agent-prompt` and `npm run sync:docs` after changing sources.
`npm run build` checks both. Web `sync:runtime` checks generated SDK documents,
composes browser docs and copies runtime assets. Web build runs this sync.

## Reference API

`reference(topic)` returns an index, not a truncated whole book. Optional section
selects a stable heading id, query searches sections, offset follows a returned
continuation. Keep each section example bounded and each code fence copyable.
The reference reader reserves room for metadata and measures the serialized
`{text}` response, not only source characters, below the agent's 8,000-character
result cap. Oversized individual blocks fail explicitly instead of silently
cutting code. Missing/unknown docs/sections report errors, not empty text.

Topics: index, basics, language, tasks, shell, python, processes, media, forms,
authoring, debugging, patterns, examples, snippets, tools. Old dml/dml-forms/
harness-patterns aliases remain. tools is the CURRENT catalog plus actual host
parameter schemas; it is not a promise that an example capability is registered.

The prompt's routing table tells the model to read one relevant contract before
an unfamiliar feature and after a contract error, not to read every guide each
turn. There is no need for extra documentation tools.

## Validation

- `tests/reference.test.ts`: all topic sections through bounded pages; actual
  HarnessAgent wire serialization; final-section reachability; no split fences;
  search, aliases, missing sections and continuations.
- `tests/authoring-prompt.test.ts`: shared invariant/budget/generation/routing checks.
- `tests/documentation-contracts.test.ts`: loader/annotation contracts for every
  module-shaped Prolog fence. Standalone queries use `prolog-goal` fences.
- `tests/canonical-examples.test.ts`: all canonical fixtures run with deterministic
  model/host stubs; schema, tool execution, memory, process and cancellation checks.
- Browser `spike:engine`: same registry/backend with real Pyodide, workers,
  filesystem, form submissions and image-reference transport.

These tests prove runtime/documentation contracts, not that a real LLM follows
instructions. Never label deterministic test completions an LLM quality benchmark.

## Opt-in authoring-model evaluations

`npm run eval:authoring` only lists the 10 cases; it makes NO network calls.
Build the SDK, then deliberately opt in against an OpenAI-compatible endpoint:

```text
DML_EVAL_ENDPOINT=<full chat/completions URL>
DML_EVAL_MODEL=<chosen model>
DML_EVAL_KEY=<optional explicit key>
npm run eval:authoring -- --run --prompt-file=../deepclause-web/src/generated/docs/AGENT_PROMPT.md
```

Add `--case=process-result` to test one scenario. Compare the same model/cases
using a saved baseline prompt via `--prompt-file`. Use a representative remote
model and the smaller local profile. Output reports automatic contract checks,
pass rate, model/tool calls, request token usage, system characters and duration.
No provider/credentials are inferred. Each case uses a disposable workspace; host
Python is schema-only and not executed. This benchmark evaluates authoring
contracts, not arbitrary generated programs or a general sandbox.

Review nuanced model outputs as well as automatic rubrics; regex artifact checks
are not semantic proofs. Do not publish a quality improvement percentage without
running a real model and reporting its configuration and case-level results.
