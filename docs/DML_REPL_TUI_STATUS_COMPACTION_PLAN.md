# Plan — pi-style TUI, status bar, reasoning models, and compaction

> **Implementation status (this branch).** P0–P5 are implemented and tested:
> `@earendil-works/pi-tui` dependency, renderer sink/event hooks, the headless
> `ReplStatusTracker`, the TUI (transcript + fixed status bar + editor, with
> `ask_user` routed through the editor), pi-style TypeScript compaction
> (session + loop, `compact.`), and reasoning-level plumbing through
> `login`/`set-backend`/`set-model`/`repl`. P6 follow-ups (`/dc-*` in the pi
> extension, upstream `formatTokens` export) remain.

Status: **proposal** · Scope: `deepclause-sdk` (`deepclause repl`, `deepclause login`,
`deepclause set-model`, `deepclause set-backend`) · Branch: `feat/dml-repl`

This plan covers four things:

1. A **pi-style TUI** for the interactive REPL (fixed status bar + input editor).
2. A **status bar** with total tokens in/out, per-frame context usage
   (`meta` and child frames), and the current model.
3. **Reasoning models**: selecting and persisting a thinking level in
   `deepclause login` / `deepclause set-model` / `set-backend`, and passing it
   through the pi backend.
4. **Compaction** of REPL meta/harness memory and of in-loop prompt history.

The guiding constraint is to reuse `@earendil-works/pi-tui` and
`@earendil-works/pi-coding-agent` wherever their public APIs allow, and to reuse
the SDK's own runtime (`ReplRenderer`, `compaction.ts`, `token-usage.ts`) where pi
is coupled to its own `AgentSession`/`AgentMessage` model.

---

## 1. Where we are today

| Area | Current implementation | Gap |
|---|---|---|
| Interactive input | `readline` in `src/cli/repl.ts` (`runInteractive`); single-line `dml> ` prompt | No fixed regions; cannot host a persistent status bar |
| Rendering | `src/repl/render.ts` `ReplRenderer` — line-oriented rich/plain/json; `\r`-based spinner | No persistent footer, no live component tree |
| Usage | `usage` events emitted by the runner with `frameKind`/`depth`; `run` records them via `system/runtime/token-usage.ts`; REPL ignores them unless `--verbose` | No counters, no context accounting, no display |
| Context | Runner knows `lastInputTokens` (`onBeforeModelCall`) and `contextWindow` (`RunnerOptions.contextWindow`) | REPL never sets `contextWindow`; meta vs harness slot usage is untracked |
| Reasoning | `PiModelInfo.reasoning` is shown as `[reasoning]` in the picker; `createPiAdapter` has no thinking level; `modelOptions.*.reasoningEffort` exists but is not wired | No selection, persistence, or forwarding |
| Models | `set-model` only edits the bundled env model DB; `repl` uses `config.backend.model` only for the pi backend | `set-model` is a no-op with the default `pi` backend |
| Compaction | Full DML compactor machinery (`src/compaction.ts`, `default-session-compactor.dml`, `default-loop-compactor.dml`, `resolveCompactionConfig`); `run` wires it | REPL never enables it; `before_user_message` trigger is typed but never invoked; meta memory is bounded to 20 messages with no summary |

Key facts:
- `DMLEvent.frameKind` ∈ `query | meta | nested | object`; `depth` gives nesting.
- `repl_record_meta/3` (`src/prolog-src/deepclause_mi.pl`) keeps the last 20
  meta messages; `repl_meta_memory_text/2` feeds only the last 10 into the prompt.
- `@earendil-works/pi-tui@1.0.4` and `@earendil-works/pi-agent-core` are already
  present transitively; `@earendil-works/pi-tui` is **not** a declared dependency yet.

---

## 2. Cross-cutting decision: introduce a TUI layer

The status bar is a *fixed element*, which the current `readline` loop cannot
host. Rather than paint a scroll region with raw ANSI, adopt
`@earendil-works/pi-tui` (already installed through pi) and keep the line
renderer only for non-TTY / `--plain` / `--json`.

### 2.1 Layout

```
┌ transcript (scroll view; streams, tool panels, answers, errors) ┐
│  meta │ ✔ assert — ok                                            │
│  run  │ Example Domain                                           │
├ status bar (fixed, 2 lines) ────────────────────────────────────┤
│ ~/work/project · session repl_… · backend pi                    │
│ ↑12.3k ↓4.5k R8.1k CH71%   meta 9.4k/128k 7%   gpt-4o · high    │
├ editor (fixed, multiline) ──────────────────────────────────────┤
│ dml> _                                                          │
└─────────────────────────────────────────────────────────────────┘
```

`TuiMainScreen` gives differential rendering and keeps the editor + status bar
fixed while the transcript scrolls.

**Superseded:** this section originally assumed the *regular* screen would scroll
the transcript and keep the dock fixed. In pi-tui the terminal owns scrollback in
regular mode, and `ScrollView`'s follow-end/wheel support belongs to the viewport
(alternate-screen) TUI. The implementation now uses `TuiAltScreen` with a
`VStack([ScrollView(transcript), dock])` layout root — the shape pi's own chat
viewport uses (`chat-viewport.js`).

### 2.2 New modules (`src/repl/tui/`)

| File | Responsibility |
|---|---|
| `app.ts` | Build `TuiMainScreen`, root `VStack`/`Container`: transcript + `StatusBarComponent` + editor; wire input, resize, and shutdown |
| `renderer.ts` | `TuiRenderer` — same public surface as `ReplRenderer` (`render`, `solution`, `writeLine`, `writeErrorLine`, `dispose`) so `dispatchReplInput`/`runInteractiveGoal` are unchanged |
| `status.ts` | `ReplStatusTracker` (usage + context accounting) and `StatusBarComponent` |
| `input.ts` | Editor + command/paste/history integration (`CustomEditor` from pi or pi-tui `Editor`) |
| `components/` | Event → component mapping (tool panels, streaming assistant, compaction notice) |

### 2.3 Selection and fallbacks

- New config `repl.tui: "auto" | "on" | "off"` (default `auto`).
- `auto` selects the TUI iff `process.stdout.isTTY && process.stdin.isTTY && !--plain && !--json`.
- Non-TTY, `--plain`, `--json`, and dumb terminals keep the existing
  `runInteractive` readline loop, renamed to `runInteractiveLine`.
- `startRepl` chooses between `runInteractiveLine` and `runTui`.
- `ReplRenderer` remains the contract; `TuiRenderer` is a second implementation.
  This keeps piped/stdin scripting and existing tests untouched.

### 2.4 Reuse

| Need | Reuse | Notes |
|---|---|---|
| Terminal + differential render | `TuiMainScreen`, `TUI` (`@earendil-works/pi-tui`) | Direct |
| Layout | `VStack`, `Container`, `Spacer`, `Box` | Direct |
| Transcript | `ScrollView`, `Text`, `Markdown` | Direct |
| Input | `CustomEditor` (pi) or `Editor` (pi-tui) | Direct; pi's editor adds autocomplete/history |
| Width math | `visibleWidth`, `truncateToWidth` | Direct |
| `/command` autocomplete | `CombinedAutocompleteProvider` (pi-tui) | Direct |
| Status bar *content* | `FooterComponent` (pi) | **Not directly reusable**: it depends on `AgentSession` + `FooterDataProvider`. Mirror its formatting instead. |
| Token formatting | `formatTokens` | Not exported by pi; reimplement (≈15 lines) or upstream-export it |
| Usage totals | `system/runtime/token-usage.ts` (`recordTokenUsage`, `TokenUsageTotals`) | Already in SDK |

**Action:** add `@earendil-works/pi-tui` (and `@earendil-works/pi-agent-core` if
we import `ThinkingLevel`) to `package.json` at the same version as pi
(`^1.0.4`) so the API is pinned deterministically.

---

## 3. Feature 1 — status bar

### 3.1 Data model (`ReplStatusTracker`)

Consumes every `DMLEvent` and maintains:

```ts
interface ReplStatus {
  // Cumulative, whole session
  totals: TokenUsageTotals;                 // reuse token-usage.ts
  // Per frame kind (meta | child), cumulative
  byFrame: Record<'meta' | 'child' | 'judge', TokenUsageTotals>;
  // Active frame (updated from model_call / usage / memory_compaction)
  active: {
    kind: 'meta' | 'child' | 'idle';
    depth: number;
    model?: string;
    lastInputTokens?: number;               // input + cacheRead + cacheWrite
    contextWindow?: number;
  };
  // Context size per memory slot (estimated before the first call)
  context: Record<'meta' | 'harness', { tokens: number; window: number }>;
  model: { id: string; provider: string; reasoning: boolean; thinkingLevel?: string };
}
```

Sources:
- `usage` → `recordTokenUsage` into `totals` and `byFrame[kind]`; set
  `active.lastInputTokens = input + cacheRead + cacheWrite` (mirrors pi's
  `FooterComponent`).
- `model_call` (`started`) → set `active.kind/depth/model`.
- `frameKind`/`depth` on each event → flip `active.kind` (`meta` → `meta`,
  `nested`/`object`/`query` → `child`).
- `memory_compaction` → recompute estimated context and flash a notice.
- Context window comes from the resolved backend (`PiModelInfo.contextWindow`);
  fall back to `config.modelOptions.*.maxContextTokens`.

`context['meta']` / `context['harness']` are estimated from the serialized
memory (`repl_memory_term/3`, `repl_meta_memory_text/2`) using
`estimateTokensForMessages` (`src/compaction.ts`). Once a real call happens, the
last `input+cacheRead+cacheWrite` for that slot is authoritative (pi's model).

### 3.2 Rendering

Modeled on pi's `FooterComponent.render` but slot-aware:

- Line 1: `~/relative/cwd · session <id> · backend <pi|env> [· FROZEN]`
- Line 2: `↑<in> ↓<out> [R<read>] [W<write>] [CH<hit>%]  <slot> <pct>%/<window>  <model>[ · <thinkingLevel>]`
  where `<slot>` is `meta` while the code-writing task runs and `run` while a
  child/object frame runs; `<pct>` is colored green/yellow/red at
  `≤70 / >70 / >90` like pi.
- Cache hit rate uses the same formula as pi:
  `cacheRead / (input + cacheRead + cacheWrite)`.

The tracker exposes `getStatus(): ReplStatus` and a `subscribe()` so the
`StatusBarComponent` re-renders on change (like pi's session stats cache,
keyed by an update counter rather than a full session scan).

### 3.3 Wiring

- `createReplRuntime` resolves the backend model **once** and exposes
  `contextWindow`, `reasoning`, `thinkingLevel` on `ReplRuntime` (today it only
  keeps `model`/`backend`). `repl.model` stays the display string.
- `runTui` feeds every rendered event through both `TuiRenderer` and
  `ReplStatusTracker`.
- `--no-events` still hides activity in the transcript but keeps the status bar
  (it is metadata, not activity).
- `--json`/`--plain` skip the tracker entirely.

### 3.4 Tests

- Unit: `ReplStatusTracker` over a synthetic event stream asserts totals,
  per-frame split (meta vs child), cache-hit rate, and the flip from
  `meta` → `run` when `frameKind` changes.
- Unit: context percentage uses `lastInputTokens` when present, else the
  estimate.
- Snapshot: `StatusBarComponent.render(width)` at several widths/colors.
- Integration: TTY-less run stays on `runInteractiveLine` and emits no TUI.

---

## 4. Feature 2 — reasoning models in `login` / `set-model`

### 4.1 What exists

- pi models carry `reasoning: boolean`, `thinkingLevelMap`, and
  `samplingParamsByThinkingLevel`; stream options accept
  `thinkingLevel?: ModelThinkingLevel` (`off | minimal | low | medium | high | xhigh | max`).
- `choosePiModel` already lists `reasoning` and `contextWindow`.
- `createPiAdapter(registry, model, maxTokens)` drops all of it and never passes
  `thinkingLevel`.

### 4.2 Config

Extend `BackendConfigSchema` (`src/cli/config.ts`):

```jsonc
"backend": {
  "kind": "pi",
  "model": "anthropic/claude-opus-4",
  "thinkingLevel": "high"        // new; validated per model on load/resolve
}
```

Keep `modelOptions.<slot>.reasoningEffort` for the `env` backend, and map it to
the existing `reasoningType`/`reasoningBudgetMap` machinery.

### 4.3 Backend changes

- `createPiBackend`/`createPiAdapter` return a new `PiBackendInfo`:
  `{ backend, modelId, provider, contextWindow, maxTokens, reasoning,
     thinkingLevel, supportedThinkingLevels }`.
- `createPiAdapter` accepts `{ maxTokens, thinkingLevel }` and passes
  `thinkingLevel` in the `streamSimple`/`complete` options.
- Validate the requested level against the model's `thinkingLevelMap`
  (`null` = unsupported) using pi's exported `resolveCliModel` /
  `resolveModelScopeWithDiagnostics`; clamp to the model default when invalid and
  surface a warning (never silently change to an unsupported level).
- Thread `config.backend.thinkingLevel` through `createReplRuntime`,
  `resolveLlmBackend`, and `run.ts`.

### 4.4 CLI changes

- `deepclause login`: after model selection, if the model is reasoning-capable,
  prompt for a thinking level. Readline path uses `pickFromList`; TUI path
  reuses `ThinkingSelectorComponent` (pi). Persist `backend.thinkingLevel`.
  Add `--thinking <level>` and `--no-thinking` flags.
- `deepclause set-model`: dispatch on the active backend:
  - `pi` → resolve through the pi catalog (`choosePiModel`), persist
    `backend.model` **and** `backend.thinkingLevel`; accept `--thinking`.
    (Today `set-model` edits the env DB and has no effect on the default pi
    backend — this fixes that.)
  - `env` → keep the DB flow; `--thinking` writes
    `modelOptions.<slot>.reasoningEffort`.
- `deepclause set-backend pi`: include the thinking-level step; `status` prints
  model + thinking level + context window.
- `deepclause show-model`: show the active backend's reasoning state.
- `deepclause repl`/`deepclause run`: accept `--thinking` override; the REPL
  `model.` command reports the level, and a new `model <provider/id> [level].`
  form persists both.

### 4.5 Reuse

- pi `ThinkingSelectorComponent` and `ModelSelectorComponent` (public exports).
- pi `resolveCliModel` / `resolveModelScopeWithDiagnostics` for validation.
- `ModelThinkingLevel` (pi-ai) and `ThinkingLevel` (pi-agent-core) types.
- The existing `pickFromList` (`src/repl/picker.ts`) for the non-TUI path.

### 4.6 Tests

- `createPiAdapter` forwards `thinkingLevel` to a stub `streamSimple`; an
  unsupported level falls back with a warning.
- `login --model X --thinking high` persists both fields.
- `set-model` with `kind=pi` writes `backend.model`/`backend.thinkingLevel`;
  with `kind=env` writes `modelOptions.run.reasoningEffort`.
- `show-model`/`set-backend status` include the reasoning level.

---

## 5. Feature 3 — compaction (pi algorithm, TypeScript)

**Decision (2026-xx): no DML compactor.** The DML compactor assets and the
`executeCompactor`/`DC_COMPACTOR_REWRITE_V1` protocol are retired from the REPL
path. Compaction is implemented in TypeScript, porting pi's algorithm
(`dist/core/compaction/compaction.js`): the summarized-conversation prompt, the
cut-point search, the chars/4 token estimate, and the trigger threshold. Where
pi's pure helpers are exported and shape-compatible we import them directly;
those that assume pi `AgentMessage`/`SessionEntry` are ported to operate on the
SDK's `MemoryMessage[]`.

### 5.1 New module: `src/repl/compaction.ts`

```ts
export interface CompactionSettings { enabled: boolean; reserveTokens: number; keepRecentTokens: number }
export const DEFAULT_COMPACTION_SETTINGS = { enabled: true, reserveTokens: 16384, keepRecentTokens: 20000 };

export function estimateMemoryTokens(messages: MemoryMessage[]): number;
export function shouldCompact(contextTokens: number, contextWindow: number, settings: CompactionSettings): boolean;

// Walk backwards accumulating estimated tokens until keepRecentTokens is met;
// cut only at user/assistant boundaries (never strip the last system message).
export function findCompactionCut(messages: MemoryMessage[], keepRecentTokens: number): { cutIndex: number; tokensToSummarize: number };

// Ported from pi's SUMMARIZATION_SYSTEM_PROMPT + SUMMARIZATION_PROMPT /
// UPDATE_SUMMARIZATION_PROMPT (preserve Goal/Constraints/Progress/Decisions/
// Next Steps/Critical Context).
export function buildSummarizationPrompt(messages, previousSummary?, customInstructions?): string;

export async function generateMemorySummary(params: {
  messages: MemoryMessage[];           // messages to summarize
  previousSummary?: string;
  backend: LLMBackend;                  // reuses pi's active model/streaming/cancellation
  model: string;
  signal?: AbortSignal;
  customInstructions?: string;
}): Promise<{ summary: string; usage?: LLMUsage }>;

export async function compactMemory(params: {
  messages: MemoryMessage[];
  previousSummary?: string;
  contextWindow?: number;
  settings?: CompactionSettings;
  backend: LLMBackend;
  model: string;
  signal?: AbortSignal;
  customInstructions?: string;
}): Promise<{ applied: boolean; messages: MemoryMessage[]; summary?: string; tokensBefore: number; tokensAfter: number; usage?: LLMUsage; reason?: string }>;
```

`generateMemorySummary` performs one `LLMBackend.complete` call with a
dedicated summarization system prompt and the serialized conversation wrapped in
`<conversation>` tags, so the model summarizes instead of continuing. It reuses
the already-injected pi backend, so it inherits model selection, credentials,
streaming, usage accounting, and cancellation. Generation failure (empty or
length-capped output) leaves memory untouched.

### 5.2 Loop compaction

Add a `beforeModelCall` hook to `DMLSession`/the runner that runs
`compactMemory` on the in-flight agent-loop messages before each model call when
`shouldCompact(lastInputTokens, contextWindow, settings)` is true. The hook
replaces `runner.applyCompactionBindings` for the REPL path and emits the same
`memory_compaction` event shape (`scope: 'loop'`, `trigger: 'before_model_call'`,
`beforeTokens`, `afterTokens`, `action`). The existing `run` path keeps the DML
compactor until it is migrated; the two must not both run.

### 5.3 Session / meta-memory compaction

Before assembling the meta prompt in `DMLSession.prompt`, when the session
threshold is crossed (or the user runs the command):

1. Read the meta memory (`repl_memory/3`, or reuse `repl_memory_term/3` +
   parse).
2. `compactMemory` over it with `scope: 'session'`/`trigger: 'before_user_message'`.
3. Write the rewritten list back (`repl_set_memory/3`) and persist it in the
   session snapshot (`sessions/<id>/harness.json`) so `--session` resume keeps
   the compacted memory.
4. Yield a `memory_compaction` event; the status bar and TUI notice consume it.

The summary is injected as a leading `system` or `assistant` message so the next
meta prompt's `{{MEMORY}}` section carries the checkpoint.

### 5.4 Config, auto-trigger, and manual command

- Replace the `compaction.bindings` DML config with TS settings:
  `compaction: { enabled, reserveTokens, keepRecentTokens, auto }`. Keep the old
  schema reading for backward compatibility (ignored with a warning).
- **Auto:** `shouldCompact(estimatedContext, contextWindow, settings)` gates the
  session pass and the loop hook. Context comes from the status tracker.
- **Manual:** `compact.` / `compact <instructions>.` in the REPL classifier and
  `--compact` for one-shot; prints
  `compacted meta 12.4k → 3.1k tokens`.
- Retire `default-session-compactor.dml` / `default-loop-compactor.dml` from the
  REPL seeds (leave the files for `run` until it is migrated).

### 5.5 Tests

- Pure unit: `estimateMemoryTokens`, `shouldCompact`, `findCompactionCut`
  boundary cases (empty, shorter than budget, cut splitting a turn).
- `generateMemorySummary` builds the correct system+user prompt and calls a stub
  `LLMBackend` once.
- `compactMemory` rewrites memory, reports `before > after`, and is a no-op when
  below threshold or when the summary is empty.
- `DMLSession.prompt` compacts meta memory before the meta prompt and persists
  it; a resumed session restores the compacted memory.
- Loop compaction runs inside a meta call and a child call, emitting
  `memory_compaction` with correct `before/after` tokens.
- `compact.` reports the delta.
- The old DML compaction tests are removed/rewritten; `run`'s temporary DML path
  keeps its tests until migrated.

---

## 6. Phasing

| Phase | Deliverable | Depends on |
|---|---|---|
| **P0** | Add `@earendil-works/pi-tui` dep; extract `ReplRenderer` event surface into an interface; rename readline path to `runInteractiveLine` | — |
| **P1** | `ReplStatusTracker` + token/context accounting (headless, no TUI) with unit tests | P0 |
| **P2** | `TuiRenderer` + `TuiMainScreen` app (transcript + editor), status bar component, `repl.tui` config; kept behind `auto` | P1 |
| **P3** | Reasoning backend plumbing: `PiBackendInfo`, thinking level in adapter, config field, `login`/`set-model`/`set-backend` prompts + flags | P0 |
| **P4** | Loop compaction wired into the REPL (thread `compaction` through `HarnessReplOptions`/`DMLSession`) | P0 |
| **P5** | Session/meta compaction hook + `runCompactorBinding` refactor + `compact.` command + auto-trigger | P4, P1 |
| **P6** | Status-bar compaction notice, docs (`REPL.md`, `STATUS.md`), and the `deepclause-pi` follow-ups (`/dc-status`, `/compact`) | P2, P3, P5 |

Each phase lands with tests; `plain`/`json` output and the existing REPL test
suite must stay unchanged throughout.

---

## 7. Reuse inventory (pi → DeepClause)

| pi API | Use | Coupling |
|---|---|---|
| `TuiMainScreen`, `TUI`, `VStack`, `Container`, `ScrollView`, `Text`, `Markdown`, `Spacer`, `Box` | TUI scaffold + transcript | none |
| `CustomEditor` (coding-agent) / `Editor` (tui) | REPL input | none |
| `visibleWidth`, `truncateToWidth`, `CombinedAutocompleteProvider` | width math + completion | none |
| `ThinkingSelectorComponent`, `ModelSelectorComponent` | reasoning/model pickers | needs a `TUI` + `ModelRuntime` |
| `FooterComponent` / `formatTokens` | status formatting reference | `AgentSession`-coupled / not exported → mirror |
| `calculateContextTokens`, `estimateTokens`, `shouldCompact`, `DEFAULT_COMPACTION_SETTINGS`, `getLastAssistantUsage` | context math + auto-trigger | none |
| `ContextUsage` type | status typing | none |
| `ThinkingLevel`, `ModelThinkingLevel`, `Model`, `ThinkingLevelMap` | reasoning typing/validation | none |
| `CompactionSummaryMessageComponent` | TUI compaction notice | none |

SDK-native reuse: `ReplRenderer`, `system/runtime/token-usage.ts`,
`src/compaction.ts`, `src/cli/config.ts` (`resolveCompactionConfig`),
`src/repl/picker.ts`, the frame/event metadata already on every `DMLEvent`.

---

## 8. Risks and open decisions

1. **TUI scope creep.** A pi-parity TUI is large. Mitigate by keeping the
   `ReplRenderer` contract and shipping `auto` selection so plain/json/scripts
   are unaffected; the TUI can start as transcript + status + editor only.
2. **`FooterComponent` is not reusable as-is.** Decide whether to mirror its
   render logic (recommended) or upstream-export `formatTokens`/usage helpers.
3. **Reasoning level vs provider.** Some pi models expose `thinkingLevelMap`
   with `null` entries; the picker must show only supported levels and
   `createPiAdapter` must warn-and-clamp, never error a whole turn.
4. **Context accounting before the first call.** Estimates (`chars/4`) can be
   off; the status bar must show the estimate with a marker and switch to real
   usage after the first response (pi does the same — `percent: null` after
   compaction).
5. **Compaction during a turn.** Session compaction must run between turns;
   loop compaction already runs inside `onBeforeModelCall`. Never compact while
   a child frame is mid-flight.
6. **Resume fidelity.** Compacted memory must be persisted in
   `sessions/<id>/harness.json` and restored on `--session`, otherwise a resume
   silently reverts to a larger memory.
7. **Model config split.** `set-model` (env DB) and `backend.model` (pi) are two
   sources of truth. Decide whether to keep both (dispatch by `backend.kind`,
   as proposed) or unify into one model field. Recommendation: dispatch by kind
   now, unify later.
8. **Dependency pinning.** `@earendil-works/pi-tui` is currently transitive;
   declare it explicitly to avoid an accidental major bump.
