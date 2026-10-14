# The REPL with running services: what it looks like

Status: **UX design**. REPL-only. Describes the experience of starting, watching,
and talking to session-scoped services and swarms in `deepclause repl`, on top of
the runtime design in [`DML_SESSION_SERVICES.md`](./DML_SESSION_SERVICES.md) §2 and
the process model in [`DML_SPECIALIZED_PROCESSES_EXAMPLE.md`](./DML_SPECIALIZED_PROCESSES_EXAMPLE.md).

The REPL already has the right skeleton. Its fullscreen TUI is:

```
┌──────────────────────────────────────────────┬─────────────────┐
│  ScrollView(transcript)                       │  side view       │
│  (user blocks, tool panes, streamed markdown, │  (harness, when  │
│   output/answer, compaction, media)           │   width ≥ 96)    │
└──────────────────────────────────────────────┴─────────────────┘
 dock (fixed):
   working       ← braille loader, present only while a turn runs
   spacer
   editor        ← PromptEditor; also shows ask_user/input questions
   statusBar     ← one dim row: workspace · running · tokens · context · model
```

Services add three things to that skeleton, nothing more: a **status segment**, a
**panel above the editor**, and **lifecycle lines in the transcript**. The
foreground turn's live streaming region is never touched.

---

## 1. Principles

1. **Foreground integrity.** A running service must never interleave output into
   the foreground turn's streamed markdown, tool pane, or editor. Service output
   is buffered and surfaced at safe points, or through the panel.
2. **Quiet by default.** A service is a background process, not a chatty one. It
   emits *state* and *milestones*; full output is available on demand
   (`/tail`, `/focus`).
3. **Pull, don't push.** The user and the agent decide when to look. The UI
   surfaces a change; it does not force a turn.
4. **The transcript is the record; the panel is the live view.** History goes to
   the session; `process_info` is the now.
5. **One editor.** Service input requests queue and reuse the editor, like
   `ask_user` already does, but only when the user is not mid-turn.

---

## 2. The status segment

The existing one-row status bar gains one segment, placed after the run state so
it survives narrow terminals:

```
~/proj  running (type to steer)  ⚙ desk 2/3  12k↑ 4k↓  · 42%  harness main        claude-sonnet-4
```

- `⚙ desk 2/3` — named service and progress (done/in-flight).
- Multiple: `⚙ 3 svc`; one with attention: `⚙ desk !`.
- Nothing running: segment omitted entirely (zero cost to the baseline).
- It fits the existing "drop whole segments, never truncate" rule in
  `StatusBarComponent`.

The **working loader** in the dock becomes shared: if any service is busy and no
foreground turn is running, the loader can show `⚙ desk · scribe drafting §2`
instead of being absent. A running foreground turn still owns the loader label.

---

## 3. The services panel

A collapsible component in the dock, directly above the editor. Collapsed, it is
one line per service; expanded, it is the supervision tree.

**Collapsed** (default when any service runs):

```
⚙ desk   planner idle · checker idle · scribe busy §2 (12s) · pool 1/3 · queue 0
```

**Expanded** (`/ps --watch` toggles it):

```
⚙ services (1)
  desk            one_for_one · restarts 1/8 · 41k tok · 3m
    ├─ planner    ready  · idle
    ├─ checker    ready  · idle
    ├─ scribe     busy   · drafting "§2 Methods" (12s, 8.1k tok)
    ├─ ledger     ready  · events=37
    └─ pool       running · w1 busy §2 · w2 idle · w3 idle · queue=0
```

This is `process_info/2` rendered: state, current activity, mailbox/queue depth,
restart counts, usage. It is the same data `/ps` prints to the transcript, so the
panel and the command cannot disagree.

The panel is polled like the harness side view (`setInterval` ~600 ms) so a change
is visible when it happens, not at the next turn boundary. On terminals narrower
than the panel needs, it collapses to the status segment and `/ps` remains the
full view.

---

## 4. Transcript lines

Service lifecycle appears as one-line, dim, prefixed entries — never as a pane
that competes with the foreground turn:

```
⚙ started desk (planner, checker, scribe, ledger, pool×3)
⚙ desk §1 done — "Introduction" (4 notes, 1 rejected) · 9k tok
⚙ desk.scribe crashed (verification_failed) → restarted (attempt 2)
⚙ desk stopped — budget exhausted after 120k tok
⚙ desk finished → §1–3, 41k tok, 3m12s
```

A service's **final result** is a normal answer block, framed and attributed:

```
⚙ desk › Article
  Quantum error correction has moved from theory toward ... [full answer]
```

Rules:

- Lifecycle lines are written only between foreground blocks, never while a
  stream is open (the renderer already tracks `lastBlock`; the service sink
  checks that state and defers).
- A service's own `output/1`/`task/N` events go to its buffer, not the
  transcript, unless it is the focused service (§6).
- Frame attribution uses the existing frame-prefix machinery, so a nested
  service call indents correctly.

---

## 5. Starting one

Three ways, all producing the same panel entry.

**Natural language** (the authoring agent starts it):

```
> start a research desk on quantum error correction, then help me with this PR

● authoring agent
  ⚙ serve desk
    started desk (planner, checker, scribe, ledger, pool×3)
  ...
  Desk is running in the background — I'll flag it when it's done.
  Now, about the PR: ...
```

The `serve` call is a **tool pane** in the transcript, exactly like `execute`:
collapsed to one line, expandable to show the service spec. The agent does not
block on it; the turn continues.

**Slash command**:

```
> /serve research_desk.dml "quantum error correction"
⚙ started desk from research_desk.dml (5 processes)
```

**From a `/goal`** that calls `serve/3`: the goal returns while the service lives.

---

## 6. Watching and talking to them

| Command | What it does |
|---|---|
| `/ps [name] [--watch]` | print the tree; `--watch` toggles the live panel |
| `/tail <name> [n]` | print the last `n` buffered events/lines |
| `/focus <name>` | follow a service: its streamed output renders live in the transcript, prefixed |
| `/unfocus` | back to quiet |
| `/call <name> <text>` | synchronous request; reply renders as a block (Esc to abandon the wait) |
| `/cast <name> <text>` | fire-and-forget message |
| `/answer <name> <text>` | reply to a service's pending input request |
| `/stop <name>` / `/stop all` | graceful stop; `/kill <name>` for immediate |

`/call` and `/tail` are the "inspect" path: they bring a bounded answer back into
the transcript without restarting anything. `/focus` is the "watch" path for a
service you care about right now:

```
> /focus desk
⚙ desk now streaming to this transcript (prefixed ⚙desk)

⚙desk  The surface code's threshold is around 1% ...
⚙desk  ... recent experiments report below-threshold operation ...
```

While a foreground turn is streaming, a focused service's output is **buffered**
and flushed as one block when the foreground block closes, so the two never
interleave mid-line.

---

## 7. A session, end to end

```
$ deepclause repl
DeepClause REPL · harness main · workspace ~/proj

> start a research desk on quantum error correction, then help me fix CVE-1234

  ● authoring agent
    ⚙ serve desk
      started desk (planner, checker, scribe, ledger, pool×3)
    Defined `research_desk` and committed. Desk is running.
    CVE-1234 is a prototype-pollution bug in the config loader; here's the fix …

  ⚙ desk   planner busy · checker idle · scribe idle · pool 0/3

  ~/proj  running (type to steer)  ⚙ desk 0/3  14k↑ 3k↓ · 38%   claude-sonnet-4

> /ps
  ⚙ services (1)
    desk            one_for_one · restarts 0 · 6k tok
      ├─ planner    busy · planning sections
      ├─ checker    ready
      ├─ scribe     ready
      ├─ ledger     ready · events=4
      └─ pool       w1 busy · w2 idle · w3 idle · queue=0

> /tail desk 5
  [8.1s] planner  task("Plan an article answering 'quantum error correction'…")
  [9.4s] planner  → 3 sections: Introduction, Methods, Results
  [9.5s] pool     dispatch §1 questions → w1
  [21s]  w1       → 4 notes (2 sources)
  …

>  (a few minutes of unrelated work; the desk advances in the background)

  ⚙ desk §1 done — "Introduction" (4 notes, 1 rejected) · 9k tok
  ⚙ desk §2 done — "Methods" (3 notes) · 15k tok

> /focus desk
  ⚙ desk now streaming to this transcript (prefixed ⚙desk)
  ⚙desk  Drafting "Results": the logical error rate falls below threshold …

> /call desk state
  ⚙ desk › {section: 3, phase: drafting, usage: 41k, rejected: 1}

> /stop desk
  ⚙ desk stopped (3/3 sections, 41k tok)

> /halt
```

Note what did **not** happen: no turn was interrupted, no modal appeared, the
editor stayed usable the whole time, and the foreground answer streamed cleanly
while the desk ran.

---

## 8. When a service needs the human

A service has no turn, so it cannot use `ask_user` freely. Input requests queue
and are surfaced non-modally:

```
⚙ desk needs input — use /answer desk <text>
   "Section 3 has no reliable sources. Skip it or widen the search?"
```

If the user is idle, the **editor border** shows the question (the same mechanism
`ask_user` uses today). If a turn is running, it waits with the indicator above;
it is presented at the next idle point. It never steals a running stream.

Approvals from a service's tool calls follow the same queue. Until answered, the
service blocks on that request; its other processes keep running.

---

## 9. Failures, restarts, budgets

All three render as one line and a panel state change:

```
⚙ desk.scribe crashed (verification_failed) → restarted (attempt 2)
⚙ desk.pool worker w2 crashed → replaced; re-dispatched §2
⚙ desk stopped — budget exhausted after 120k tok
⚙ desk needs attention — supervisor gave up after 8 restarts
```

A restarted service has **lost its state** (the scribe forgot its style). The UI
says so when it matters:

```
⚙ desk.scribe restarted — state lost; re-priming from the seed
```

`/ps <name>` shows a short lineage: restarts, last crash reason, last restart
time.

---

## 10. Interrupts

The keys keep their current meaning, extended:

- **Esc** — interrupts the *foreground* turn. Services are untouched.
- **Ctrl-C** — if the foreground turn is running, same as Esc; when idle and
  services are running, it asks: `stop 1 service? [y/N]`; on confirm, stops
  everything and exits.
- **Ctrl-D / `/halt`** — stops all services, then exits, writing their
  declarations and final state to the session record.

This is the one place where the old "Ctrl-C exits" has to change, because it
would otherwise silently kill a running desk.

---

## 11. Resume

On `--session`, the banner lists recorded services:

```
Resumed session 7f3a · harness main
  ⚙ desk  recorded (stopped) — restart with /serve --resume desk
```

`/ps` shows them dimmed as `recorded`, not `running`. `/serve --resume desk`
re-launches from the recorded declaration (same skill and args); the ledger's
history is replayed from the session record so the new run continues the story
rather than starting blank. Live process state is never claimed — a process is
not serializable.

---

## 12. What we can do (capability summary)

- **Start** a desk or swarm, from natural language (agent `serve` tool) or a slash
  command, without blocking the session.
- **See** the whole system state live: status segment, collapsible supervision
  tree, per-process state/usage/restarts.
- **Watch** a chosen service's stream, prefixed and buffered so it never corrupts
  the foreground turn.
- **Ask** a service a question (`/call`) or tail its log (`/tail`).
- **Message** a service (`/cast`) and **answer** its input requests (`/answer`).
- **Stop** one service, all of them, or the whole session, selectively.
- **Keep working** in the foreground the entire time; the editor, steering, the
  solution pager, and `ask_user` are unaffected.
- **Resume** a session and restart recorded services from their declarations.

---

## 13. Implementation mapping

| UX element | Where |
|---|---|
| `⚙` status segment | `ReplStatusSnapshot` + `StatusBarComponent` (`src/repl/status.ts`, `src/repl/tui/status-bar.ts`) |
| shared working label | `WorkingLine` (`src/repl/tui/working-line.ts`) + service-busy state |
| services panel | new component in the dock `VStack` (`src/cli/repl.ts` ~1616), polled like the harness view |
| lifecycle/result lines | a **service sink** parallel to `TuiRenderer`'s document sink; gated on `lastBlock !== 'streaming'` |
| per-service buffer + focus | `ServiceHost` in `DMLSession`; buffer ring + loud/quiet subscription |
| `/ps` `/tail` `/call` `/cast` `/focus` `/stop` `/answer` | the command classifier in `src/cli/repl.ts` / readline dispatcher |
| `serve` tool | `HarnessToolController` + `createHarnessTools` (`src/repl/harness-tools.ts`) |
| session record | `sessions/<id>/services.jsonl` beside the harness history |
| scheduler | `DMLRunner.stepLoop` generalized to a run queue; services are frames |

The one genuinely new plumbing piece is the **service sink**: a second output
channel that the renderer writes service events into, with the same deferral
rules already used to keep tool panes and streamed markdown from colliding.

---

## 14. Open UX questions

1. **Panel placement.** Dock row above the editor, or a tab in the existing side
   view (width ≥ 96 only)? A dock row works on narrow terminals; the side view is
   richer.
2. **Default verbosity.** Is "milestones only" right, or should a freshly started
   service stream until the user `/unfocus`es it? (Lean quiet.)
3. **Ctrl-C policy.** Confirm before stopping services, or stop only the
   foreground and require `/stop all`? (Lean confirm.)
4. **Loader sharing.** Should service activity animate the same loader as a
   foreground turn, or get a subdued second indicator?
5. **`/call` blocking.** Block the editor (Esc to abandon) or run as a mini-turn?
6. **Resume.** Restart recorded services automatically on a trusted workspace, or
   always require `/serve --resume`? (Lean explicit.)
