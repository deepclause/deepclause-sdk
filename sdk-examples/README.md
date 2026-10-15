# DeepClause SDK Examples

Every example runs **with no configuration**: if a provider key is in the
environment it uses the live model, otherwise it falls back to a scripted model.
That keeps the examples runnable — and `npm run examples:smoke` meaningful — on a
machine with no credentials.

## Quick start

```bash
npm run build
npm run example     # quick-start.ts — DeepClause in 30 lines
npm run examples    # neurosymbolic.ts — the model reads prose, Prolog decides
```

Keys are optional. To use a live model, export one of:

```bash
export OPENAI_API_KEY="..."                # gpt-4o-mini
export ANTHROPIC_API_KEY="..."             # claude-3-5-haiku-latest
export GOOGLE_GENERATIVE_AI_API_KEY="..."  # gemini-2.0-flash
```

`npm run examples:smoke` runs every example in this directory offline.

## The examples

### 1. Quick start — `quick-start.ts`

The smallest program that does something: one `task/N` and one `answer/1`.

```bash
npm run example
```

### 2. Tools and policy — `tools.ts`

Register typed host tools, then restrict which ones the model may reach. The
same request runs under a whitelist that permits only `fetch_weather`, so the
email call comes back refused — enforced by the host, not the model.

```bash
npx tsx sdk-examples/tools.ts
```

```
policy: {"mode":"whitelist","tools":["fetch_weather"]}

  ▸ weather
  ▸ fetch_weather
  ✓ fetch_weather
  ✓ weather
  ▸ send_email
  ▸ smtp
  ✗ smtp  Tool 'smtp' is not in the whitelist
  ✓ send_email
answer: done
```

Note the policy check applies to `exec/2` **inside a DML tool body**, not only to
a top-level `exec`. A tool cannot smuggle a blocked host capability.

### 3. Neuro-symbolic — `neurosymbolic.ts`

A refund policy arrives in English. The model extracts it into a declared shape
(`schema/2`); Prolog then applies it to every order exactly, so the model never
sees the order table and cannot miscount a refund.

```bash
npm run examples
# refund [carol,erin]
```

Swap the policy text and the *same rules* produce a different, still-exact
answer — that is the neuro-symbolic split: language in, logic out.

### 4. Semantic judgments — `judgments.ts`

`judge/2` with pluggable backends. One DML program makes typed semantic
decisions — `choose`, `rate`, `verify`, `probability` — and the same program runs
against a mock, an LLM, or Jev by changing only the backend registration.

```bash
JUDGE_BACKEND=mock npx tsx sdk-examples/judgments.ts                       # no key
JUDGE_BACKEND=llm  GOOGLE_GENERATIVE_AI_API_KEY=... npx tsx sdk-examples/judgments.ts
JUDGE_BACKEND=jev  TYPESAFE_API_KEY=...             npx tsx sdk-examples/judgments.ts
```

See [`docs/JUDGMENT_BACKENDS.md`](../docs/JUDGMENT_BACKENDS.md).

### 5. Subagents — `subagents.dml`, `subagents.ts`

The `.dml` file is a tour of six concurrency patterns, from a fan-out to agents
that route work between processes. Patterns 1–5 call `task/N` (a model); pattern
6 is pure Prolog and is best run with the default `--execution worker`.

| Entry | Pattern |
|---|---|
| `survey/2` | fan out over a list with `swarm/4`, results in input order |
| `best_of/3` | best-of-N, then one judge |
| `start_glossary/0` | a stateful, long-lived service (`serve/2`, `service_call/3`) |
| `ops_start/0` | agents that route work by calling a tool that is another process |
| `race/2` | hedge two strategies, then cancel the loser (`receive` + `exit/2`) |
| `queens_portfolio/1` | CPU-bound CLP search in subagents (`pmap/3`) |

```bash
deepclause repl -l sdk-examples/subagents.dml
```

```
/goal survey(['quantum error correction', riscv], B)
/goal start_glossary, define_terms([subagent, mailbox], Defs)
/goal ops_start, ask_frontdesk("Where is my order #4471?", Reply)
/goal queens_portfolio(Rows)
```

The `narrate`/`keeper`/`asker` predicates at the bottom show the three process
lifecycle shapes — one-shot, message-waiting, and interactive — and need no
model:

```
/spawn narrate("quantum error correction")
/spawn keeper([])
/spawn asker("Which deadline should I assume?")
/ps --watch --logs
/send keeper "the migration finished"
/stop keeper
```

`subagents.ts` is the programmatic demo of those three shapes:

```bash
npx tsx sdk-examples/subagents.ts            # L1 (in-process)
npx tsx sdk-examples/subagents.ts --worker   # L2 (worker thread)
```

See [`docs/SUBAGENTS.md`](../docs/SUBAGENTS.md).

### 6. Two agents talking to each other — `debate.dml`, `debate.ts`

Two **LLM-backed** subagents — Pro and Con — with their own private memory and
their own `task/N` calls, exchanging arguments directly with `send/2` and
`receive/2`, then judged by a third call.

```bash
npx tsx sdk-examples/debate.ts             # L1, deterministic mock model
npx tsx sdk-examples/debate.ts --worker    # L2 worker, mock model
DC_REAL=1 DC_MODEL=gpt-4o OPENAI_API_KEY=… npx tsx sdk-examples/debate.ts   # real model
```

Or with the REPL's configured model:

```bash
deepclause repl -l sdk-examples/debate.dml
/goal debate("Nuclear power should expand to meet climate targets", 2, Verdict).
```

Each debate turn is shown as `⚙ debater  PRO (round 2): …` in the transcript.

## Embedding the REPL

`subagents.ts` and `debate.ts` use `createHarnessRepl` (exported from the SDK) to
embed the REPL engine, not just the one-shot `runDML`. That is the seam for
building a UI on top of DeepClause.

## DML language quick reference

### Core predicates

```prolog
% Run an LLM agent loop
task("Description").
task("Description with variables", string(Answer), integer(Count)).

% Structured output against a declared shape
schema(author, [field(name, string, required), field(email, string, default(""))]).
task("Extract the authors", list(schema(author, Authors))).

% External tool execution
exec(tool_name(Arg), Result).

% Memory
system("System prompt").
user("User message").
push_context / pop_context.       % backtrackable memory

% Output
answer("Final answer").           % terminate and commit
yield("Streaming output").        % stream intermediate text
log("Debug message").
```

### Tools and entry point

```prolog
tool(my_tool(Arg1, Result), "What it does") :-
    exec(external_tool(Arg1), Result).

agent_main :- ... .               % entry point, runDML()
agent_main(Arg1, Arg2) :- ... .   % with positional args
```

### Processes (subagents)

```prolog
spawn(Goal, Pid).                 % start a process; Goal is copied
send(Pid, Message).               % copy a term into a mailbox
receive(Message, TimeoutMs).      % pop the oldest message; fails on timeout
serve(Name, Goal).                % spawn + register
service_call(Name, Request, Reply).
swarm(Worker, Items, Results, 4). % fan out, collect in input order
```

## Tips

1. **Tool policy**: set one in production. It governs `exec/2` everywhere,
   including inside a DML tool.
2. **Error handling**: use Prolog's `( Goal -> Then ; Else )` and `catch/3`.
3. **Structured output**: prefer `schema/2` over parsing prose; a missing
   required field fails the task instead of silently going empty.
4. **Streaming**: `yield/1` streams intermediate results for a live UI.
5. **Cancellation**: pass an `AbortController`'s `signal` in `runDML` options.
