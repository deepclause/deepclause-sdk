# Plain SWI-Prolog runtime: design proposal

Status: **design proposal, with a working core** · Branch:
`feat/plain-swi-prolog` · Target: a DeepClause REPL that runs on a stock
SWI-Prolog installation, including inside AgentVM.

### Implementation status

Implemented in `swipl/`:

- REPL entry point and QLF build (`bin/dc`, `scripts/build-qlf.sh`).
- Session lifecycle, `/load`, `/goal`, `/run`, `/listing`, `/save`, `/provider`,
  `/model`, `/config`.
- Frame driver for `request_agent_loop`, `request_llm`, `request_exec`,
  `wait_input`, output/answer/solution events.
- Agent loop with `finish`/`set_result`, typed outputs, DML tool engines, and
  host `exec/2` (`bash`, `ask_user`).
- OpenAI Chat Completions, Anthropic Messages, and Google Gemini streaming
  adapters; OpenAI-compatible providers and Azure auth.
- SSE parser, JSON/schema helpers, config/credentials, usage events.
- Unit tests and a local OpenAI-compatible integration test.
- Alpine/AgentVM image template and notes.

Not implemented yet: OpenAI Responses, Bedrock/Vertex, forms/processes,
history/compaction, natural-language authoring, and the optional C/C++ host.

## 0. Recommendation in one paragraph

Build the plain runtime in **SWI-Prolog itself**, not as a Prolog engine hidden
behind a new host. The existing `deepclause_mi.pl` is already a cooperative
runtime: it yields `request_agent_loop/…`, `request_llm/2`,
`request_exec/2`, `request_form/…`, and the host answers with
`post_agent_result/…` and friends. The TypeScript layer is a host, not the
engine. Replace that host with four Prolog modules: a frame driver, an LLM
provider layer, an agent loop, and a REPL. Use SWI's built-in HTTP/TLS client,
JSON library, threads, and process library. Compile the runtime to QLF for fast
startup. Ship the result in the AgentVM Alpine image by installing the existing
`swi-prolog` package. Keep a C/C++ embedding host as an **optional accelerator**
only if profiling shows a real need; do not make it the first implementation.

The evidence in §2 shows that this is not speculative: the frame loop, fake LLM
handoff, HTTPS, SSE reading, and C embedding all work on the installed SWI.

## 1. Goals and non-goals

### Goals

- Run `deepclause` with a normal SWI installation; no Node, npm, or WASM.
- Reuse `deepclause_mi.pl` and the DML semantics unchanged.
- Support the major LLM providers:
  - OpenAI (Chat Completions or Responses)
  - Anthropic Messages
  - Google Gemini
  - OpenRouter and the large OpenAI-compatible ecosystem
  - Azure OpenAI
  - Local OpenAI-compatible servers (Ollama, LM Studio, vLLM)
- Stream text and reasoning; cancel a turn; account for usage.
- Support `task/N`, `prompt/N`, `llm/2`, tool calls, and typed outputs.
- Run well inside AgentVM: interactive console, network through the VM, bash in
  the same guest, no host shell escape.
- Keep startup fast and memory small.

### Non-goals for the first version

- Full OAuth subscription login (Claude Pro/Max, ChatGPT subscription).
- AWS Bedrock SigV4 and Google Vertex OAuth. These need a credential helper;
  design the provider seam so they can be added later.
- Full parity with the TUI (forms, process swarms, compaction, history) on day
  one. Add them in phases.
- A second session store. Use the existing `.deepclause/` layout or a documented
  equivalent.
- A C/C++ host as the default. It is an optimization, not the architecture.

## 2. Research and feasibility evidence

All measurements below are on the development machine: SWI-Prolog 9.0.4,
x86_64 Linux.

### 2.1 The runtime is already host-neutral

`deepclause_mi.pl` exports the whole host protocol:

- `create_session/4`, `load_code/3`, `query_start/5`
- `step_frame/4`
- `post_agent_result/4`, `post_exec_result/3`, `post_llm_result/3`,
  `post_form_result/2`, `post_next_result/2`
- `create_tool_engine/4`, `step_tool_engine/4`, `post_tool_agent_result/5`
- `repl_meta_prompt/2,3`, `repl_run_meta/4`, `repl_commit/3`

A pure Prolog driver already works. This probe runs `member(X,[1,2,3])`
without any Node host:

```prolog
create_session(probe, _{}, "[]", "[]"),
repl_initial_state(probe, object, State),
query_start(probe, "member(X,[1,2,3])", object, State, none),
default_frame(probe, Frame),
step_frame(Frame, Status, Content, Payload), ...
```

Observed:

```text
status=solution ... bindings:['X'-"1"]
status=solution ... bindings:['X'-"2"]
status=solution ... bindings:['X'-"3"]
status=finished
```

The same loop handles `llm/2`:

```text
status=request_llm payload{messages:[user("hi")], modelSpec:none}
status=solution  bindings:['R'-"\"hello from fake\""]
status=finished
```

So the LLM layer is a **host concern**, not something that needs to be pushed
into the meta-interpreter.

### 2.2 Stock SWI has the needed libraries

Verified with the installed SWI:

- `library(http/http_open)` — HTTP/HTTPS client, including TLS.
- `library(http/http_client)` — POST helpers.
- `library(http/json)` — `atom_json_dict/3`, `json_read_dict/3`,
  `json_write_dict/3`.
- `library(readutil)` — `read_line_to_string/2` for SSE.
- `library(process)` — `process_create/3`, `process_wait/2`,
  `process_kill/2`.
- `library(thread)` — threads, message queues, `thread_signal/2`.
- `library(ssl)`, `library(socket)`, `library(base64)`, `library(uuid)`.
- `library(readline)` / `library(editline)` for a line-editing REPL fallback.

An HTTPS request succeeded against `https://example.com/` with status `200`.
A local SSE POST test read:

```text
data: {"delta":"hel"}

data: {"delta":"lo"}

data: [DONE]
```

This is the exact wire shape needed for streaming provider adapters.

### 2.3 QLF makes startup fast

The meta-interpreter is ~9.6k lines. Without compilation, loading the four
Prolog sources takes roughly 0.4–0.5 s on the development machine. Compiling
them to QLF and loading the QLF takes roughly 0.21–0.24 s, with about 9.5 MB
resident.

```bash
swipl -q -g "qcompile('deepclause_mi.pl'),halt"
swipl -q -O -g "dc_repl:main" -t halt
```

Native `swipl` startup itself is ~0.06 s. That is already fast enough for a
REPL; the remaining time is dominated by the harness load, not by the host
language. A C/C++ host would still load the same SWI runtime and QLF, so its
startup advantage is small.

### 2.4 C embedding is available, but not required

The installed SWI ships `SWI-Prolog.h`, `SWI-cpp.h`, `SWI-cpp2.h`, and
`swipl-ld`. A minimal embedding host compiles and runs:

```c
#include <SWI-Prolog.h>
int main(int argc, char **argv) {
  if (!PL_initialise(argc, argv)) return 1;
  term_t a = PL_new_term_refs(2);
  PL_put_atom_chars(a, "version");
  if (!PL_call_predicate(NULL, PL_Q_NODEBUG,
        PL_predicate("current_prolog_flag", 2, NULL), a)) return 1;
  return 0;
}
```

```bash
swipl-ld -o dc-embed dc-embed.c
```

That confirms the option is real. It does not show a performance need for it.

### 2.5 AgentVM can run SWI natively

AgentVM is an Alpine Linux guest (riscv64, TinyEMU/container2wasm) with an
interactive console mode. The Alpine package index has:

- `swi-prolog` 10.0.2-r1 for `edge/testing/riscv64`
- installed size roughly 18.5 MiB
- dependency on `libswipl.so.10`

There is **no `swi-prolog-dev` package** in that repository. That matters for a
C/C++ host: you would need to vendor headers or build SWI from source, and
cross-compile for riscv64. A pure Prolog runtime only needs the `swi-prolog`
package and `ca-certificates`.

AgentVM's interactive mode (`interactive: true`, `writeToStdin`,
`onStdout`, `onStderr`) is enough to run a console REPL inside the guest.

## 3. Architecture

```text
┌────────────────────────────── AgentVM guest (Alpine) ──────────────────────────────┐
│                                                                                    │
│  bin/dc  (swipl -O -q -g dc_repl:main -t halt)                                     │
│                                                                                    │
│  dc_repl.pl       read input, commands, render events, /load, /run, /goal          │
│  dc_session.pl    create_session, load_code, history, harness paths                 │
│  dc_frame.pl      step_frame dispatcher: output/answer/error/request_*              │
│                                                                                    │
│  dc_agent.pl      task/prompt agent loop, finish/set_result, tool calls             │
│  dc_tools.pl      DML tool engines, exec/2, bash, input/forms                       │
│  dc_process.pl    threads/message queues for subagents (later)                      │
│                                                                                    │
│  dc_llm.pl        normalized request/response; provider dispatch                    │
│  dc_llm_openai.pl           Chat Completions + OpenAI-compatible                    │
│  dc_llm_openai_responses.pl OpenAI Responses (optional first-class)                 │
│  dc_llm_anthropic.pl        Anthropic Messages                                      │
│  dc_llm_google.pl           Gemini generateContent                                  │
│  dc_sse.pl                  SSE line/event parser                                   │
│  dc_json.pl                 JSON/dict helpers, schema normalization                 │
│  dc_config.pl               env/config, API keys, provider registry                 │
│  dc_models.pl               model catalog, context/output limits, pricing           │
│                                                                                    │
│  swipl runtime + QLF + harness + docs                                               │
└────────────────────────────────────────────────────────────────────────────────────┘
```

The host loop is deliberately small:

```prolog
run_frame(Frame) :-
    step_frame(Frame, Status, Content, Payload),
    dispatch(Frame, Status, Content, Payload).

dispatch(_, finished, _, _) :- !.
dispatch(F, output, Text, _) :- !, write(Text), nl, run_frame(F).
dispatch(F, answer, Text, _) :- !, format("~w~n", [Text]), run_frame(F).
dispatch(F, error, Text, _) :- !, format(standard_error, "ERROR: ~w~n", [Text]).
dispatch(F, request_llm, _, Payload) :-
    !, dc_llm:complete_messages(Payload, Reply),
    post_llm_result(Session, success, Reply), run_frame(F).
dispatch(F, request_agent_loop, _, Payload) :-
    !, dc_agent:run(Payload), run_frame(F).
dispatch(F, request_exec, _, Payload) :-
    !, dc_tools:exec(Payload), run_frame(F).
dispatch(F, request_form, _, Payload) :- ...
```

The real implementation must handle cancellation and cleanup with
`setup_call_cleanup/3`, but the shape is this simple.

## 4. LLM layer design

### 4.1 Normalized API

Provider-specific JSON never leaks into DML or the agent loop. Define one term
shape in `dc_llm.pl`:

```prolog
llm_request{
    provider: openai_compatible,
    model: "gpt-4o-mini",
    messages: [
        message{role: system, content: "You are concise."},
        message{role: user, content: "Summarise this."}
    ],
    tools: [
        tool{name: "lookup",
             description: "Look up a record.",
             parameters: _{type: object,
                           properties: _{id: _{type: string}},
                           required: [id]}}
    ],
    temperature: 0.2,
    max_tokens: 4096,
    reasoning: none,
    stream: true
}.
```

```prolog
llm_response{
    text: "...",
    reasoning: "...",
    tool_calls: [
        tool_call{id: "call_1", name: "lookup", arguments: _{id: "A-17"}}
    ],
    usage: usage{input: 123, output: 45, total: 168,
                 cache_read: 0, cache_write: 0, reasoning: 0},
    finish_reason: tool_use,
    raw: _  % optional, for diagnostics only
}.
```

Streaming events use the same names as the existing `DMLEvent` vocabulary:

```prolog
llm_event(text(Chunk)).
llm_event(reasoning(Chunk)).
llm_event(tool_call(Id, Name, ArgumentsJsonFragment)).
llm_event(usage(Usage)).
llm_event(done(Response)).
llm_event(error(Message)).
```

`dc_llm:stream(Request, Callback, Response)` calls `call(Callback, Event)` for
each event and unifies `Response` at the end. A non-streaming provider can emit
one `text/1` event followed by `done/1`.

### 4.2 Provider behaviour

Each provider module implements three predicates:

```prolog
provider_request(+Provider, +Request, -Http).
provider_event(+Provider, +SseEvent, +State0, -State).
provider_finish(+Provider, +State, -Response).
```

`Http` is:

```prolog
http_request{
    url: "https://api.openai.com/v1/chat/completions",
    method: post,
    headers: ["Authorization"=..., "Content-Type"="application/json"],
    body: _{...},          % serialized with library(http/json)
    timeout: 120
}.
```

The generic transport in `dc_llm.pl` owns:

- JSON serialization and HTTP opening.
- status-code handling and error-body extraction.
- SSE buffering and `[DONE]`.
- Calling the provider's event parser.
- Retry/backoff and usage normalization.
- Cancellation and stream cleanup.

This means a new OpenAI-compatible provider is a few facts, not a new module.

### 4.3 Provider matrix

| Provider | Adapter | Endpoint | Auth |
|---|---|---|---|
| OpenAI | `openai_chat` (first), `openai_responses` (later) | `https://api.openai.com/v1/...` | `Authorization: Bearer` |
| Anthropic | `anthropic_messages` | `https://api.anthropic.com/v1/messages` | `x-api-key`, `anthropic-version` |
| Google Gemini | `google_generative` | `https://generativelanguage.googleapis.com/v1beta` | `x-goog-api-key` or key query |
| OpenRouter | `openai_compatible` | `https://openrouter.ai/api/v1` | Bearer |
| Azure OpenAI | `openai_chat` / `openai_responses` | deployment URL + `api-version` | `api-key` |
| Groq | `openai_compatible` | `https://api.groq.com/openai/v1` | Bearer |
| Mistral | `openai_compatible` | `https://api.mistral.ai` | Bearer |
| DeepSeek | `openai_compatible` | `https://api.deepseek.com` | Bearer |
| xAI | `openai_compatible` | `https://api.x.ai/v1` | Bearer |
| Together / Fireworks / Cerebras / NVIDIA / … | `openai_compatible` | provider base URL | Bearer |
| Ollama / LM Studio / vLLM | `openai_compatible` | local base URL | none or dummy |
| AWS Bedrock | later | SigV4 | credential helper |
| Google Vertex | later | OAuth | credential helper |

The OpenAI-compatible adapter is the highest-leverage piece: it covers most of
the ecosystem with one request/response implementation. Native adapters are
needed only where the wire protocol differs materially.

### 4.4 Provider-specific differences to isolate

**OpenAI Chat Completions**

- Request: `messages`, `tools` as `{type:"function", function:{name,
  description, parameters}}`, `stream: true`,
  `stream_options: {include_usage: true}`.
- Stream: `choices[0].delta.content`, `delta.tool_calls[index]`,
  `finish_reason`, final `usage`.
- Tool results: role `tool`, `tool_call_id`.
- Newer reasoning models may use `max_completion_tokens` and may reject
  `temperature`; keep a model capability fact and a small transform.

**OpenAI Responses**

- Request: `input` rather than `messages`, `stream: true`, function tools in the
  Responses shape.
- Stream: `response.output_text.delta`,
  `response.function_call_arguments.delta`, `response.completed`.
- This is what pi uses for OpenAI. It is the right long-term adapter if
  reasoning summaries and the newest tool features matter.

**Anthropic Messages**

- Request: `system` separate from `messages`, `max_tokens` required, tools use
  `input_schema`.
- Stream: `content_block_start`, `content_block_delta`
  (`text_delta`, `input_json_delta`, `thinking_delta`), `message_delta` usage.
- Tool results: user message with `tool_result` blocks; `tool_use` blocks in
  assistant messages.
- `tool_use` IDs are provider-supplied.

**Google Gemini**

- Request: `contents` with `role: user|model`, `parts`, `systemInstruction`,
  `tools: [{functionDeclarations: [...]}]`, `generationConfig`.
- Stream: `candidates[0].content.parts[].text` or `functionCall`;
  `usageMetadata`.
- Tool results: `functionResponse` parts.
- Function schemas are an OpenAPI subset; strip unsupported JSON Schema
  keywords such as `additionalProperties` and `anyOf` where necessary.

**OpenRouter / OpenAI-compatible**

- OpenAI Chat Completions with a different base URL and headers.
- Optional `HTTP-Referer`, `X-Title`, provider routing preferences.
- Some models expose a `reasoning` field or vendor-specific deltas; treat
  unknown fields as optional.

### 4.5 SSE parser

`dc_sse.pl` should be a small, well-tested state machine:

```prolog
sse_read(Stream, State0, State) :-
    read_line_to_string(Stream, Line0),
    (   Line0 == end_of_file
    ->  State = State0.put(done, true)
    ;   normalize_crlf(Line0, Line),
        sse_line(Line, State0, State1),
        sse_read(Stream, State1, State)
    ).

sse_line("", State0, State) :-          % blank line dispatches the event
    dispatch_event(State0, State).
sse_line(Line, State0, State) :-
    (   Line = ":" -> State = State0    % comment/keepalive
    ;   Line = "event: " -> ...
    ;   Line = "data: "  -> append data
    ;   State = State0 ).
```

Details:

- Join multiple `data:` lines with `\n`.
- Ignore comments and unknown fields.
- Treat `data: [DONE]` as end-of-stream for OpenAI-style APIs.
- Parse each payload with `atom_json_dict/3`; on a parse error, report a
  provider error rather than silently dropping the chunk.
- Cap one event and the accumulated tool-call JSON to a sane size.

### 4.6 HTTP, auth, and errors

Use `http_open/3` from `library(http/http_open)`:

```prolog
http_open(URL, In, [
    method(post),
    post(bytes('application/json', BodyString)),
    request_header('Content-Type'='application/json'),
    request_header('Authorization'=AuthHeader),
    status_code(Status),
    timeout(TimeoutSeconds)
]).
```

- Always use HTTPS; never disable certificate verification.
- `status_code/1` is required so 4xx/5xx bodies can be read and parsed into a
  useful error.
- On non-2xx, parse `{"error": {"message": ...}}` where possible; otherwise use
  the first 1–2 KB of the body.
- Retry on 429, 5xx, connection reset, and timeout with exponential backoff and
  jitter; honor `Retry-After` when present. Do not retry a 4xx authentication or
  validation error.
- Redact `Authorization`, `x-api-key`, `api-key`, and cookies from every log.

### 4.7 Streaming and cancellation

Run each LLM call in a worker thread owned by a small `llm_session` handle. The
worker:

1. Opens the HTTP stream.
2. Reads SSE lines and calls the provider event parser.
3. Calls the Prolog callback for each text/reasoning/tool/usage event.
4. Closes the stream and posts `done`.

Cancellation:

- The controller sets `cancel` in a thread-safe flag and closes the HTTP stream.
- `http_open`'s `timeout/1` bounds a stalled read.
- `thread_signal(Worker, throw(cancelled))` interrupts a worker at a safe point.
- `setup_call_cleanup/3` closes the stream even on exception.
- The agent loop checks the cancel flag before each model call, between tool
  calls, and before posting the final result.
- A cancelled turn posts `post_agent_result(..., failure, ..., "cancelled")` or
  returns a clean cancellation event; it must not leave a frame suspended.

A blocking C transport would need `PL_handle_signals()` in its progress
callback; the pure-Prolog transport avoids that.

### 4.8 Reasoning

Reasoning is optional and model-dependent:

- Anthropic: `thinking_delta` blocks.
- OpenAI Responses: reasoning summary/text deltas; Chat Completions may only
  report reasoning tokens.
- Gemini: thought summaries where the model exposes them.
- If a provider returns no reasoning text, report nothing rather than inventing
  it.
- Normalize reasoning tokens into `usage.reasoning` when available.

### 4.9 Model catalog and configuration

Keep a small Prolog fact database (`dc_models.pl`) generated from pi's model
database or curated by hand:

```prolog
model(openai, "gpt-4o-mini", _{context: 128000, max_output: 16384,
    reasoning: false, input_cost: 0.15, output_cost: 0.60}).
model(anthropic, "claude-sonnet-4-5", _{context: 200000, max_output: 8192,
    reasoning: true, input_cost: 3.00, output_cost: 15.00}).
```

At runtime, the catalog provides:

- context window for compaction/guardrails
- max output tokens
- whether a reasoning level is supported
- optional price display

Configuration precedence:

1. CLI flags (`--provider`, `--model`, `--api-key-env`)
2. environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`,
   `GEMINI_API_KEY`, `OPENROUTER_API_KEY`, `AZURE_OPENAI_API_KEY`, …)
3. workspace `.deepclause/config.json`
4. user `~/.config/deepclause/config.json`
5. built-in defaults

Never store API keys in the harness/DML file. In AgentVM, prefer an injected
config file with mode 0600 or environment variables from the VM launcher.

## 5. Agent loop

`dc_agent.pl` is the Prolog equivalent of `runInjectedAgentLoop` in
`src/agent.ts`.

For a `request_agent_loop` payload:

1. Decode `taskDescription`, `outputVars`, `userTools`, `memory`,
   `toolScope`, `reasoningEffort`, `recipeContext`, `modelSpec`, and `mode`.
2. Build the system prompt and conversation history.
3. Add synthetic tools:
   - `finish(success)` for task mode.
   - `set_result(variable, value)` when there are typed outputs.
4. Add the DML tools from `userTools`, honoring the tool scope.
5. Call `dc_llm:stream/3` with the provider tools.
6. Accumulate text/reasoning and tool-call argument fragments.
7. On `finish`, validate required outputs and stop.
8. On `set_result`, coerce/validate against the `typed_var` and record it.
9. On a user tool call:
   - Validate arguments.
   - Execute the DML tool through `create_tool_engine/4` /
     `step_tool_engine/4`, handling nested `request_exec` and
     `request_agent_loop`.
   - Post `post_tool_agent_result/5`.
   - Append the tool result to the provider message history.
10. Append the assistant message and loop until the model stops calling tools
    or `finish` succeeds.
11. `post_agent_result(SessionId, Success, Variables, Messages, FullMemory)`.

Typed outputs are easier in Prolog than in TypeScript: the MI already sends
`typed_var{name, type, schema?}` dicts. Reuse `typed_arg/3` and the schema
helpers in `deepclause_mi.pl` rather than reimplementing them.

Task-mode protocol rules:

- `finish(true)` is refused while a required output is unset.
- A rejected `finish`/`set_result` is returned to the model as a tool result and
  can be retried a bounded number of times.
- A user tool failure is data for the model, not a protocol failure.
- Task mode requires an explicit finish; chat mode ends when the model stops
  calling tools.
- Enforce a model-call budget and a repeated-stall guard.

Memory:

- Preserve system messages separately from conversation history.
- Post the full agent memory so the MI can keep it across turns.
- A simple context guard can estimate tokens and either refuse an oversized
  request or summarize older messages with the same model. Full pi-style
  compaction is a later phase.

`request_llm/2` (used by `llm/2`) is much simpler: convert messages to a
provider request without tools, stream the text, and `post_llm_result/3`.

`request_judgment` can reuse the same completion path with a JSON-output prompt
and validation. `sample_token` can use provider logprobs where available and a
constrained-prompt fallback otherwise; defer it if it becomes provider-specific.

## 6. Host tools

### 6.1 `bash` / `exec/2`

Inside AgentVM, the Prolog REPL and bash run in the same guest. Implement
`exec(bash(...))` with `process_create/3`:

```prolog
process_create(path('/bin/sh'),
    ['-c', Command],
    [cwd(Workspace),
     stdout(pipe(Out)),
     stderr(pipe(Err)),
     process(Pid)]),
```

- Read stdout/stderr on separate threads to avoid pipe deadlock.
- Apply a wall-clock timeout and an idle timeout; kill the process group.
- Return `stdout`, `stderr`, `exitCode`, and elapsed time in the same shape the
  MI already expects.
- Run with a controlled environment (`PATH`, `HOME`, `TMPDIR`, `WORKSPACE`).
- Inside AgentVM, the VM is the sandbox; do not nest bwrap. Outside AgentVM,
  reuse the existing host-shell strategy or warn that the shell is unsandboxed.
- Never let DML paths escape the workspace.

### 6.2 Input and forms

- `input_required` / `ask_user`: print the prompt and read a line; call
  `provide_input/2`.
- `request_form`: render a minimal textual form, validate with the existing
  `form_validate_values/3`, and call `post_form_result/2`.
- A richer form UI is a later phase; the host protocol already exists.

### 6.3 Processes and subagents

The MI's process primitives (`spawn`, `send`, `receive`, `register`, `swarm`)
can be implemented with SWI threads, message queues, and a session-scoped
registry. This is a separate module and can be deferred. For v0, implement
`self`, `sleep`, `process_info`, and `whereis`; return a clear unsupported error
for the rest.

## 7. REPL

A plain but functional REPL:

```text
dml> /help
dml> /load skills/review.dml
dml> /goal review("text")
dml> /run skills/review.dml "text"
dml> /listing
dml> /model anthropic/claude-sonnet-4-5
dml> /provider openrouter
dml> /save
dml> /quit
```

- Use `library(readline)` when available; fall back to
  `read_line_to_string/2`.
- `read_term/3` for `/goal` so multi-line Prolog and quoted strings work.
- Stream text and reasoning to stdout; print `task_activity` and `tool_call`
  events as concise lines.
- On `answer`, print the final result.
- On `error`, print the error and continue; do not kill the session.
- Keep one session alive across turns; do not spawn `swipl` per input.
- On exit, `end_frame/1`, `destroy_session/1`, and flush history.
- Work mode / authoring mode can be added after the execution path is stable.
  The MI already exposes `repl_meta_prompt/2,3`, `repl_commit/3`,
  `repl_execute_frame/4`, and the listing/export predicates.

History and harness layout:

- Reuse `.deepclause/repl/harnesses/<name>/harness.dml` and
  `.deepclause/repl/sessions/<id>/`.
- For v0, `/load` and `/save` are enough; event/history JSONL can follow the
  existing shapes.
- Do not create a second hidden store; the Prolog runtime should read the same
  files as the SDK.

## 8. AgentVM deployment

### 8.1 Image changes

The current image is Alpine Linux with Python and a preinstalled `pi` Node
runtime. Add:

```dockerfile
# Alpine edge/testing, riscv64
RUN apk add --no-cache swi-prolog ca-certificates
```

The package is `swi-prolog` 10.0.2-r1 (about 18.5 MiB installed). Pin the
version and repository for reproducible builds. If the base image is on a
stable branch, either enable `edge/testing` for this package or vendor the APK.

Copy the runtime:

```text
/opt/deepclause/
├── bin/dc
├── prolog/*.pl
├── qlf/*.qlf
└── docs/*
```

Compile the QLF **inside the image build**; QLF is tied to the SWI version and
architecture. Do not copy QLF from the x86_64 host.

### 8.2 Interactive console

Start AgentVM with `interactive: true`, then run:

```text
/opt/deepclause/bin/dc
```

Pipe host input with `writeToStdin`, render `onStdout`/`onStderr` to the host
terminal. The Node AgentVM launcher is only a console bridge; the DeepClause
runtime itself is Prolog.

### 8.3 Network and credentials

- AgentVM networking is NAT through the host. Enable it only when a model
  provider is configured.
- Install `ca-certificates`; verify `https://api.openai.com/` or another
  provider endpoint from the guest.
- Use the AgentVM firewall to allow only configured provider domains. Default
  deny for everything else.
- Inject API keys as environment variables or a 0600 config file in the mounted
  workspace; do not bake them into the image or the DML harness.
- If the network is disabled, fail with a clear message rather than hanging.

### 8.4 Where bash runs

Because the REPL is inside AgentVM, `bash` is already inside the VM. Do not
route it back to the host. This is the simplest security story: the VM is the
sandbox, and the Prolog host has no host-shell capability.

### 8.5 Performance caveat

AgentVM is a riscv64 VM. Native SWI under TinyEMU will be slower than the
x86_64 measurements in §2.3. Measure:

- `swipl -q -g halt` startup
- QLF load
- a simple DML goal
- a streaming model call

If the REPL is too slow inside the guest, fall back to running the REPL on the
host and using AgentVM only for `bash`; that requires a host-tool RPC but keeps
the same Prolog runtime. The design should keep this boundary swappable.

## 9. Compiled host option (C/C++ and others)

### 9.1 When it is worth it

A compiled host is worth considering only if one of these is true:

- Measured startup or per-turn overhead of SWI + QLF is unacceptable inside
  AgentVM.
- You need a single binary with no `swipl` launcher.
- You need HTTP/2, a proxy stack, OAuth, or provider SDK behavior that SWI's
  HTTP client cannot provide.
- You need to embed the REPL in an existing C/C++ application.

It is **not** required for LLM support: SWI already has HTTP/TLS/JSON.

### 9.2 C++ embedding design

Recommended if a compiled host is chosen:

- C++17 with `SWI-cpp2.h` (RAII wrappers around `term_t`, `PlQuery`, etc.).
- `libcurl` for HTTP/2, streaming, cancellation, proxies, and TLS.
- `nlohmann/json` for request/response manipulation if the provider adapters
  move into C++.
- SWI C API:
  - `PL_initialise`
  - `PL_open_query` / `PL_next_solution` or `PL_call_predicate`
  - `PL_register_foreign` for `llm_*` predicates
  - `PL_thread_attach_engine` / `PL_thread_destroy_engine` for worker threads
  - `PL_handle_signals()` inside libcurl progress callbacks so Ctrl-C and
    `thread_signal/2` can cancel a blocking request.
- Build with `swipl-ld` on a normal Linux host:

```bash
swipl-ld -o dc-embed dc_embed.cpp -lcurl
```

- Or CMake with `find_path(SWI-Prolog.h)` and `find_library(swipl)`.

Host design options:

1. **Full C++ host** — owns the REPL loop, embeds SWI, implements providers.
2. **C++ foreign library** — `dc_llm.so` loaded by SWI; Prolog keeps the REPL and
   provider policy, C++ provides `llm_http_*` transport. This is the better
   middle ground: a small FFI surface, no second process.
3. **C++ sidecar** — a separate `dc-llm` process speaks line-delimited JSON over
   pipes/sockets. Easiest to build with modern HTTP libraries; adds a process
   and serialization boundary.

### 9.3 AgentVM constraints for a C/C++ host

- The guest is riscv64; cross-compile with an Alpine riscv64 sysroot.
- `swi-prolog` is packaged, but **headers are not** (`swi-prolog-dev` does not
  exist in the index). Vendor the matching headers from SWI 10.0.2 or build SWI
  from source in the image.
- Link against `libswipl.so.10`; set `SWI_HOME_DIR`/`PLBASE` so runtime data
  (`boot.prc`, library) is found.
- Build the host inside the image (slow, needs `build-base`) or cross-compile
  with a matching sysroot. This is a significant maintenance cost.
- A C/C++ host still loads the same SWI runtime and QLF; the performance gain
  may be small compared with the build complexity.

### 9.4 Other languages

| Option | Assessment |
|---|---|
| **C** | Works with SWI C API, but manual term/memory handling; C++ is safer. |
| **C++** | Best fit: mature SWI C++ wrappers, libcurl, JSON. |
| **Rust** | Possible via SWI C ABI/FFI; good HTTP ecosystem, but no mature SWI embedding crate and more FFI work. |
| **Go** | cgo + SWI C API; excellent HTTP/2 and concurrency, but cgo build and SWI headers complicate deployment. |
| **Sidecar (any language)** | Good for OAuth/HTTP2/proxy; adds an IPC boundary and a second process. |

Recommendation: **do not start here**. Keep the LLM provider interface in
Prolog so a compiled transport can be added later without changing DML or the
agent loop.

## 10. Performance plan

1. **QLF everywhere.** Compile the MI, memory, strings, analysis, and the new
   modules at build time. Load QLF, not `.pl`.
2. **Keep the session alive.** A REPL turn must not re-consult the runtime.
3. **Avoid unnecessary JSON round-trips.** Keep native Prolog dicts/terms in the
   agent loop; convert only at the provider boundary.
4. **Stream.** The user sees text while the model runs; perceived latency is
   dominated by first token, not host startup.
5. **Bound tool output.** Truncate large `bash`/tool results before sending them
   back to the model, as the TypeScript runtime does.
6. **Measure inside AgentVM.** Do not assume x86_64 numbers. If the RISC-V guest
   is too slow, move the REPL to the host and keep AgentVM for tools.

## 11. Security and safety

- **API keys**: environment or 0600 config; never in DML, logs, or the image.
- **TLS**: verification on; no “accept any certificate” mode.
- **Prompt injection**: document text and tool output are untrusted data, not
  instructions. Keep system prompts separate; do not let tool output change
  tool permissions.
- **Tool policy**: explicit allowlist per run. `bash`/`exec` are host
  capabilities; forms/input are user channels.
- **Workspace isolation**: resolve every path under the workspace; reject
  traversal.
- **AgentVM**: use its firewall to allowlist provider domains; default deny.
- **Redaction**: strip `Authorization`, `x-api-key`, `api-key`, and cookies from
  diagnostics.
- **OAuth**: do not implement refresh-token storage in Prolog. If subscription
  login is required, use a credential helper/sidecar with a small, audited
  surface.

## 12. Testing

Provider adapters are the highest-risk part; test them without live keys.

1. **Recorded HTTP fixtures.** Store request/response and SSE transcripts for
   OpenAI, Anthropic, Google, OpenRouter, Azure, and one local provider. Replay
   them through `dc_llm` with a fake transport.
2. **Local mock server.** Use `library(http/http_server)` to serve SSE on
   localhost and test `http_open`/SSE/cancellation end to end.
3. **Contract tests.** Assert exact request JSON: URLs, headers, tool schema
   shape, message role mapping, `max_tokens` field names, and stream options.
4. **Agent loop tests.** Fake provider returns text, reasoning, one/multiple tool
   calls, malformed tool args, `finish` without results, `set_result` with wrong
   types, and cancellation.
5. **Frame loop tests.** Reuse the probes in §2.1 to verify `solution`,
   `request_llm`, `request_exec`, and error paths.
6. **QLF test.** Load only QLF and run a DML smoke test in a clean process.
7. **AgentVM smoke test.** Boot the image, run `swipl -q -g halt`, then a local
   mock provider, then a real provider if credentials are available.
8. **Security tests.** Path traversal, oversized output, non-2xx errors, timeout,
   cancellation, and redaction.

## 13. Implementation phases

### Phase 0 — branch and probes (done)

- Branch `feat/plain-swi-prolog`.
- Confirm frame loop, fake LLM handoff, HTTPS, SSE, QLF, and C embedding.
- Fix the current undefined export `repl_harness_module_info/2` that prevents
  `use_module` from loading `deepclause_mi.pl` under plain SWI.

### Phase 1 — frame host and REPL skeleton

- `dc_repl.pl`, `dc_frame.pl`, `dc_session.pl`.
- `/help`, `/load`, `/goal`, `/run`, `/listing`, `/quit`.
- No model calls; verify DML execution and output.

### Phase 2 — LLM transport

- `dc_llm.pl`, `dc_sse.pl`, `dc_config.pl`.
- OpenAI-compatible Chat Completions provider + fake provider.
- `llm/2` and streaming/cancellation.
- Local mock server and fixture tests.

### Phase 3 — agent loop

- `dc_agent.pl` for `task/N` and `prompt/N`.
- `finish`/`set_result`, typed outputs, tool calls.
- DML tool engine execution and `post_tool_agent_result`.
- `prompt` authoring path if needed.

### Phase 4 — provider breadth

- Anthropic Messages.
- Google Gemini.
- OpenRouter and Azure configuration.
- OpenAI Responses adapter if reasoning/tool parity requires it.
- Reasoning deltas and usage normalization.

### Phase 5 — AgentVM

- Alpine image with `swi-prolog` + `ca-certificates`.
- QLF build in the image.
- Interactive console bridge.
- Network/firewall/credential configuration.
- Measure RISC-V performance.

### Phase 6 — parity work

- Forms, input, history, Work mode, shell workflows.
- Process/subagent primitives.
- Compaction.
- C++ transport only if measurements justify it.

## 14. Risks and open questions

1. **SWI version compatibility.** Host SWI is 9.0.4; Alpine edge has 10.0.2. Run
   the full MI test suite under SWI 10 before relying on it.
2. **AgentVM RISC-V performance.** The QLF numbers are x86_64. Measure inside
   the guest; if unacceptable, move the REPL to the host and keep AgentVM for
   bash.
3. **Provider drift.** Pin request shapes with fixtures; add a provider smoke
   test when APIs change.
4. **OAuth and cloud auth.** Bedrock/Vertex and subscription logins need a
   credential helper. Decide whether that is in scope before promising “all
   providers.”
5. **Streaming cancellation.** Test blocking reads, timeouts, and close-from-
   another-thread behavior under SWI 9 and 10.
6. **Agent loop parity.** Nested tools, typed outputs, and memory are subtle.
   Port from `src/agent.ts` with tests, not from memory.
7. **C/C++ build cost.** Alpine lacks SWI headers; cross-compiling a C++ host is
   real work. Keep it out of the critical path.
8. **Model catalog.** Generating a Prolog catalog from pi's model database is a
   build step; decide whether to ship all models or a curated subset.

## 15. Decision

Adopt the **pure SWI-Prolog host** for the plain runtime:

- It runs on the default SWI installation.
- It runs inside AgentVM using the packaged `swi-prolog` 10.0.2.
- It reuses the existing MI and DML semantics.
- It starts in roughly 0.3 s with QLF, before VM emulation overhead.
- It has no Node/npm/WASM dependency.
- It keeps the LLM layer behind a provider behaviour, so a C/C++ transport can
  be added later if measurements demand it.

The first implementation should be **OpenAI-compatible Chat Completions +
Anthropic + Google**, with a fake provider and recorded fixtures. Add OpenAI
Responses and cloud-auth providers after the core loop is stable.

A C++ embedding host is a valid optimization, not the foundation. Use it only
if AgentVM measurements show that SWI's startup, HTTP client, or JSON handling
is the bottleneck.
