# Sessions, traces, and harness history

How the REPL persists what happened — every session, its turns and event
traces, and every change to the harness — and exposes the durable links to DML
as `harness_*` facts so a harness can query its own past.

Status: implemented. Canonical reference:
[`DML_REPL_PROPOSAL.md`](./DML_REPL_PROPOSAL.md).

## 1. Principles

* **The links live in the harness.** Sessions, turns, and versions are asserted
  into the living module as `harness_*` facts, so a DML skill can query what
  happened before with `findall`/`aggregate_all`. It is data = code.
* **Verbose detail stays beside it.** Requests, streamed model text, and tool
  stdout live in JSONL sidecars that the facts point at by path — never in the
  facts, which stay compact and queryable.
* **Best-effort.** History recording never fails a turn; a write error is warned
  once (under `DEBUG_REPL`) and ignored.
* **System-owned, DML-callable.** History facts sit in a third visibility class:
  excluded from `listing`/`manifest`/`export` and protected from user
  definition, but callable from DML. (This is the `param/2` treatment.)

## 2. Layout

Rooted at `repl.history.dir` (default `.deepclause/repl/`). The pi extension
overrides it to `.pi/deepclause/repl/` to honor "never create `.deepclause/`".

```text
.deepclause/repl/
├── history.dml                           # workspace facts (harness_session/turn)
├── harnesses/
│   └── main/                             # one dir per harness (--harness)
│       ├── harness.dml                   # current content (source of truth)
│       ├── baseline.dml                  # version 0 / rollback floor
│       ├── history.dml                   # harness_change/changed/head/rollback
│       ├── HEAD.json                     # current version pointer
│       ├── changes.jsonl                 # one line per harness change
│       └── versions/
│           └── 0001_2026-01-15T10-35-12-900Z.json   # full snapshot per version
└── sessions/
    └── repl_m9x1_1/
        ├── session.json                  # metadata, start/end, exit, resumes
        ├── harness.json                  # durable harness snapshot for --session
        ├── turns.jsonl                   # one summary per turn
        └── events.jsonl                  # the runtime event trace
```

Both `history.dml` files are consulted into the module at session start, so every
session begins with the full queryable history. A project can hold several
harnesses; each has its own content, baseline, and versions (`--harness`,
`repl.harness`, default `main`).

## 3. The facts

```prolog
% One per REPL session. TraceDir is workspace-relative.
harness_session(SessionId, StartedAt, Model, Backend, TraceDir).
harness_session_end(SessionId, EndedAt).
harness_session_resume(SessionId, At).        % the session was reopened

% One per turn.
harness_turn(SessionId, Turn, Timestamp, Mode, Outcome).
%   Mode    = goal | prompt
%   Outcome = committed | answered | failed | aborted | query

% One per harness change (a commit, a load, or a reset).
harness_change(Version, Timestamp, Kind, SessionId, Turn).
%   Kind = commit | load | reset

% The predicates a version touched.
harness_changed(Version, Name, Arity, Action).
%   Action = added | removed | replaced

% Current version pointer (singleton) and the per-turn trace path.
harness_head(Version, Timestamp, SessionId, Turn).
harness_trace(SessionId, Turn, TracePath).

% A rollback moved HEAD from FromVersion back to ToVersion.
harness_rollback(FromVersion, ToVersion, Timestamp, SessionId, Turn).
```

Sessions are split into `harness_session/5` + `harness_session_end/2` (rather
than one fact with an `EndedAt` slot) so the fact log stays strictly
append-only: ending a session appends, it never rewrites.

Query examples:

```prolog
recent_changes(N, Cs) :-
    findall(V-Ts, harness_change(V, Ts, commit, _, _), All),
    last_n(All, N, Cs).

tried_before(Name/Arity) :-
    harness_changed(_, Name, Arity, replaced).

session_report(Id, Started, Turns) :-
    harness_session(Id, Started, _, _, _),
    aggregate_all(count, harness_turn(Id, _, _, _, _), Turns).
```

## 4. Visibility

| Category | `listing`/manifest | `export` | user may define | DML may call |
|---|---|---|---|---|
| user | yes | yes | yes | yes |
| reserved (`repl_*`, `meta_*`) | no | no | no | **no** |
| system (`param`, `tool_source`, history) | no | no | no | **yes** |

Concretely, `history_predicate(Name) :- sub_atom(Name, 0, _, _, harness_)` is
added to `repl_user_predicate/3` (excluded) and `repl_protected_name/1`
(protected) but **not** to `repl_reserved_name/1` — so `guard_reserved_direct`
still lets DML call the facts. `listing(history)` and the manifest's
`- history:` summary line surface them to the author.

## 5. What is recorded when

The host (`HistoryStore`, driven by `DMLSession`) owns persistence; Prolog only
materializes facts via `repl_history_load/2` / `repl_history_record/2`.

| Moment | Facts | Sidecars |
|---|---|---|
| session open | `harness_session/5` (and load prior `history.dml`) | `session.json` |
| each event | — | `events.jsonl` (buffered, flushed at turn end) |
| turn end | `harness_turn/5`, `harness_trace/3` | `turns.jsonl` |
| commit / load / reset | `harness_change/5`, `harness_changed/4`, `harness_head/4` | `harnesses/<name>/changes.jsonl`, `versions/`, `HEAD.json` |
| session end | `harness_session_end/2` | `session.json` update |

Timing matters: facts are written **after** the turn resolves (post-commit,
post-restore, post-abort) because `repl_snapshot_save`/`restore` wraps the turn
and would roll back anything asserted inside it.

## 6. Versions and diffs

A change's `changed` list is computed from manifests:
`touched` predicates (from `repl_last_commit_summary/2`) classify as `replaced`
if they existed before the turn and `added` otherwise; `removed` is
`before \ after`. Version numbers are monotonic per workspace; `HEAD.json`
carries the current one. Each version stores the full module source (from
`export_harness/3` with no header), so any version is reconstructible and
diffable.

## 7. Config

`.deepclause/config.json`:

```json
"repl": { "history": {
  "enabled": true,
  "dir": ".deepclause/repl",
  "trace": "summary",
  "maxEventBytes": 8192,
  "maxSessions": 50,
  "maxVersions": 200,
  "includeInExport": false,
  "redact": ["sk-[A-Za-z0-9]+", "Bearer [A-Za-z0-9._-]+"]
} }
```

* `trace`: `off` | `summary` (event metadata) | `full` (include streamed text).
* Redaction and `maxEventBytes` are applied to event content/args before
  writing.
* Default `enabled` is `true` for the REPL; the library
  (`createHarnessRepl`) records nothing unless `history` is passed.
* Retention prunes the oldest sessions/versions at session open (never
  mid-turn); `HEAD.json` always survives.

## 8. Tests

`tests/repl-history.test.ts`:

* a prompt turn writes `harness_session/turn/change/changed/head` to
  `history.dml`, `turns.jsonl`/`events.jsonl`, and `HEAD.json`;
* a second session loads the prior facts and can query
  `harness_change/5` and `harness_session/5` from a `goal:`;
* history facts are not listed as harness predicates.

## 9. Rollback

`rollback.` (also `repl.rollback()` / `session.rollback()`) undoes the last
harness transaction: it reads `HEAD`'s `parentVersion`, restores that version's
serialized module (or empties the module when there is no parent), and records
the action as `harness_rollback/5` + a `harness_head` move. It is refused when
the harness is frozen and reports `no harness changes to roll back` when HEAD is
already 0.

Version numbers stay monotonic: `HEAD.json` tracks both `version` (where HEAD
is) and `maxVersion` (the highest ever issued), so a commit after a rollback
still gets a fresh number. Repeating `rollback.` walks further back; a commit in
between branches forward from the restored version. Rollback preserves runtime
conversation memory (it restores code, not the conversation).

## 10. Resuming a session (`--session`)

`deepclause repl --session <id>` (or `--session last`) reopens an earlier
session: the same `sessionId`, its restored harness, and its conversation
memory, with turn numbering continuing where it stopped. A
`harness_session_resume/2` fact records the reopen; `session.json` keeps its
original `startedAt` and gains a `resumedAt` entry.

Resume is anchored by a **durable harness snapshot** written beside the session
(`sessions/<id>/harness.json`):

```json
{
  "sessionId": "repl_…",
  "version": 3,
  "hash": "…",
  "clean": true,
  "savedAt": "…",
  "scripts": [{ "path": "scripts/x.py", "sha256": "…" }],
  "memory": { "harness": "[message{…}]", "meta": "[message{…}]" },
  "module": "% DML harness …\n…"
}
```

* **When it is written.** After every recorded change (commit/load/reset) and
  again on session end. The end-of-session write captures the *current* module,
  including any goal-mode drift, so the harness is never lost when the process
  ends. The temp `.zip` safety net for aborted turns is unchanged.
* **What it restores.** The module is loaded back into the living module (a
  restore, not a new change); the serialized `harness`/`meta` memory seeds
  `create_session/4`; `turnCounter` continues from the last `harness_turn`.
* **What it does not restore.** Runtime state: subagent processes/services and
  in-flight work from the previous run are gone. A short system note is appended
  to the restored memory (and the authoring memory) saying the session was
  reloaded and transient state was reset, so an agent does not assume a previous
  service is still alive. The note is replaced — not stacked — across repeated
  resumes.
* **Consistency.** `hash` covers the module *and* the linked script hashes.
  Scripts live in the workspace and are *not* overwritten on resume; if the
  recomputed hash differs from the snapshot, a warning is emitted (under
  `DEBUG_REPL`) that the harness or its scripts changed since the session was
  saved.
* **Why not the scratch `.zip`?** It lives in the temp dir and may be gone. The
  session snapshot is durable, workspace-relative, and pruned with the session.

## 11. Follow-ups

* `export --with-history` to bundle `history.dml` + `HEAD.json`.
* CLI: `deepclause repl history`, `--version`, `diff v1 v2`.
* Optional read-only `repl_version/1` for skills to stamp their output.
* Optionally reconstruct a version back into a workspace (`repl restore vN`).
* Expose `--session` from the pi extension (`/dc-run --session`).
