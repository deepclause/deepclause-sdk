# DeepClause SDK Architecture

## Overview

DeepClause SDK is a **neurosymbolic AI system** that combines Prolog-based symbolic reasoning with LLM-powered task execution. It provides a unique approach where deterministic logic programming controls when and how AI language models are invoked, enabling precise, verifiable AI workflows.

```
┌─────────────────────────────────────────────────────────────────────┐
│                        User Application                              │
├─────────────────────────────────────────────────────────────────────┤
│                     DeepClause SDK (TypeScript)                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
│  │   sdk.ts    │  │  runner.ts  │  │  agent.ts   │  │  tools.ts   │ │
│  │  (Factory)  │  │ (Executor)  │  │ (LLM Loop)  │  │  (Policy)   │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘ │
├─────────────────────────────────────────────────────────────────────┤
│                    Prolog Layer (SWI-Prolog WASM)                   │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │              deepclause_mi.pl (Meta-Interpreter)             │    │
│  │  - State threading (memory, params)                          │    │
│  │  - task()/prompt() predicates → Agent Loop                   │    │
│  │  - exec() predicate → External Tools                         │    │
│  │  - Cooperative execution via engine yields                   │    │
│  └─────────────────────────────────────────────────────────────┘    │
├─────────────────────────────────────────────────────────────────────┤
│                         LLM Providers                                │
│         OpenAI │ Anthropic │ Google │ OpenRouter                    │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Core Components

### 1. SDK Layer (`sdk.ts`)

**Purpose**: Factory and public API for creating SDK instances.

**Responsibilities**:
- Auto-detect LLM provider from model name
- Initialize SWI-Prolog WASM runtime
- Create and configure DMLRunner
- Manage tool registry and policies
- Handle instance lifecycle (dispose)

**Key Interface**:
```typescript
interface DeepClauseSDK {
  runDML(code: string, options?: RunOptions): AsyncGenerator<DMLEvent>;
  registerTool(name: string, tool: ToolDefinition): void;
  setToolPolicy(policy: ToolPolicy): void;
  getMemory(): MemoryMessage[];
  dispose(): void;
}
```

### 2. Runner (`runner.ts`)

**Purpose**: Bridge between TypeScript and Prolog, executing DML code cooperatively.

**Responsibilities**:
- Parse and load DML code into Prolog session
- Run Prolog engine cooperatively (yield-based execution)
- Handle engine requests:
  - `request_agent_loop` → Invoke agent.ts
  - `request_exec` → Execute external tools
  - `request_input` → Request user input
- Convert between JS and Prolog data types
- Manage memory state across task() calls
- Emit DML events (output, stream, answer, etc.)

**Key Flow**:
```
DML Code → Prolog Engine → Yield Request → Runner Handler → Resume Engine
                ↑                                    │
                └────────────────────────────────────┘
```

### 3. Agent Loop (`agent.ts`)

**Purpose**: Execute LLM-powered tasks with tool calling capability.

**Responsibilities**:
- Build system prompts with task description and available tools
- Run iterative LLM calls until task completion
- Handle built-in LLM tools: `finish()`, `set_result()`
- Convert registered tools to AI SDK format
- Support streaming responses
- Maintain conversation history within a task
- Return messages for memory persistence

**Built-in LLM Tools** (available to the LLM during `task()` execution):

| Tool | Description |
|------|-------------|
| `finish(success)` | Signal task completion (true/false) - **required** |
| `set_result(variable, value)` | Store result value for output variables |

**Note:** These are tools the LLM calls, not Prolog predicates. They cannot be called directly from DML code.

**Agent Loop Cycle**:
```
1. Build messages (system + memory + task description)
2. Call LLM with tools
3. Process response:
   - Text output → Stream to user
   - Tool calls → Execute and add results
   - finish() → Exit loop
4. Repeat until finished or max iterations
```

### 4. Prolog Meta-Interpreter (`deepclause_mi.pl`)

**Purpose**: Execute DML code with state threading and cooperative yields.

**Key Predicates**:

| Predicate | Description |
|-----------|-------------|
| `task(Desc)` | Invoke LLM agent with description, inherits memory |
| `task(Desc, Var1, ...)` | Task with output variables to store |
| `prompt(Desc)` | LLM call with fresh/empty memory |
| `exec(Tool, Result)` | Call external tool |
| `system(Text)` | Add system context to memory |
| `user(Text)` | Add user message to memory |
| `answer(Text)` | Output answer to user |
| `yield(Text)` | Stream intermediate output |
| `param(Key, Value)` | Access runtime parameters |

**State Threading**:
```prolog
State = state{
  memory: [message{role: Role, content: Content}, ...],
  params: ParamsDict,
  context_stack: [...],
  depth: N
}
```

### 5. Tool System

The DeepClause tool system has two distinct layers that serve different purposes:

#### External Tools (via `exec/2`)

External tools are called from DML using `exec(tool_name(args...), Result)`. They execute in **fresh, isolated state** and are used for I/O operations, web searches, code execution, etc.

**Built-in External Tools**:

| Tool | Provider | Description |
|------|----------|-------------|
| `vm_exec(command)` | AgentVM | Execute shell commands in sandboxed Alpine Linux VM |
| `web_search(query)` | Brave | Search the web using Brave Search API |
| `news_search(query)` | Brave | Search for recent news articles |
| `ask_user(prompt)` | Internal | Request input from the user |

**Key Characteristics**:
- **Fresh State**: Each `exec()` call runs in isolation - no access to current Prolog state
- **No Backtracking**: Tool results are computed once and don't participate in backtracking
- **Side Effects**: Tools can perform I/O, network requests, file operations

**Example**:
```prolog
exec(web_search(query: "AI news"), Results),
exec(vm_exec(command: "python3 script.py"), Output)
```

#### Internal Tools (DML Wrappers via `tool/2`)

DML tool wrappers are defined in Prolog using `tool/2` predicates. They provide a **named interface** for the LLM to call during `task()` execution, typically wrapping external tools with additional logic.

**Key Characteristics**:
- **LLM Accessible**: Tools defined with `tool/2` appear in the LLM's available tools during `task()`
- **Fresh State per Call**: When the LLM calls a DML tool, it runs in a fresh Prolog engine
- **Can Call External Tools**: Tool bodies can use `exec/2` to call external tools

**Example**:
```prolog
% Define a tool the LLM can use
tool(search(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query, count: 10), Results).

% LLM can now call 'search' during task() execution
agent_main(Topic) :-
    task("Research {Topic} using the search tool.").
```

#### Agent-Internal Tools (LLM-only, not Prolog predicates)

These tools are **only available to the LLM** during `task()` execution. They are NOT Prolog predicates and cannot be called directly from DML code:

| Tool | Description |
|------|-------------|
| `finish(success)` | **Required** - Signal task completion (true/false) |
| `set_result(variable, value)` | Store result in an output variable |

These are handled by the agent loop and cannot be overridden by user-defined tools.

**Important:** When a `task()` has output variables (e.g., `task(Desc, Result)`), the LLM uses `set_result` internally to populate them. The DML code receives the result through Prolog unification, not by calling `set_result` directly.

#### State Isolation Summary

```
┌────────────────────────────────────────────────────────────────┐
│                    DML Program State                            │
│  - Memory (conversation history) - BACKTRACKABLE               │
│  - Params dict                                                  │
│  - Context stack                                                │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Normal Predicates         │    tool() and exec() calls       │
│   ──────────────────────    │    ────────────────────────      │
│   ✓ Full state access       │    ✗ Fresh isolated state        │
│   ✓ Memory persists         │    ✗ No memory access            │
│   ✓ Backtracking works      │    ✗ Results are deterministic   │
│   ✓ Can modify state        │    ✗ Cannot modify main state    │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

**Why Isolation?** Tool execution runs in a separate Prolog engine to ensure:
1. Tools cannot corrupt the main program state
2. Backtracking in the main program doesn't re-execute tools
3. LLM tool calls are predictable and repeatable

#### Nested Agent Loops in Tools

Tools can use `task()` and `prompt()` internally to combine Prolog logic with LLM reasoning:

```prolog
tool(explain_calculation(A, B, Explanation)) :-
    Sum is A + B,  % Prolog computation
    format(string(Desc), "Explain ~w + ~w = ~w to a child", [A, B, Sum]),
    task(Desc, Explanation).  % LLM explanation
```

When `task()` runs inside a tool:
1. The tool engine yields `request_agent_loop` with the task description
2. TypeScript runs a nested agent loop via `runAgentLoop()`
3. The result is posted back via `post_tool_agent_result()`
4. The tool engine resumes with the result bound to the output variable

**Tool Availability:** Nested agent loops have access to all DML tools **except** the calling tool (automatic call stack exclusion to prevent infinite recursion).

#### Manual Tool Scoping

You can further control tool availability using `with_tools/2` and `without_tools/2`:

```prolog
% Only allow specific tools in nested task
tool(restricted_task(Input, Output), "Runs with limited tools") :-
    with_tools([tool_a, tool_b], (
        format(string(Desc), "Process '~w'", [Input]),
        task(Desc, Output)
    )).

% Exclude specific tools from nested task
tool(safe_task(Input, Output), "Excludes dangerous tools") :-
    without_tools([dangerous_tool], (
        format(string(Desc), "Process '~w' safely", [Input]),
        task(Desc, Output)
    )).
```

Tool scoping can also be used directly in `agent_main`:

```prolog
agent_main :-
    with_tools([safe_tool], (
        task("Do something with only safe tools", Result)
    )),
    answer(Result).
```

---

## Data Flow

### DML Execution Flow

```
┌──────────────────────────────────────────────────────────────────┐
│ 1. User calls sdk.runDML(code, options)                          │
└──────────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────────────────────────────────────────────────────────┐
│ 2. Runner parses DML code into Prolog session                    │
│    - Creates unique session ID                                   │
│    - Asserts params as param/2 facts                             │
│    - Loads user clauses into session module                      │
└──────────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────────────────────────────────────────────────────────┐
│ 3. Runner creates Prolog engine for agent_main/1                 │
│    - Initial state: empty memory, params dict                    │
│    - Runs meta-interpreter (mi/3)                                │
└──────────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────────────────────────────────────────────────────────┐
│ 4. Engine yields on special predicates                           │
│    - task() → request_agent_loop                                 │
│    - exec() → request_exec                                       │
│    - ask_input() → request_input                                 │
└──────────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────────────────────────────────────────────────────────┐
│ 5. Runner handles request                                        │
│    - Agent loop: calls runAgentLoop(), posts result back         │
│    - Exec: calls tool, posts result back                         │
│    - Input: yields input_required event, waits for response      │
└──────────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────────────────────────────────────────────────────────┐
│ 6. Engine resumes with result, continues execution               │
│    - Memory state updated with agent conversation                │
│    - Loop continues until no more yields                         │
└──────────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────────────────────────────────────────────────────────┐
│ 7. Engine completes, Runner yields 'finished' event              │
│    - Optional trace log if tracing enabled                       │
└──────────────────────────────────────────────────────────────────┘
```

### Memory Threading

Memory flows between tasks to maintain conversation context:

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│     Task 1      │     │     Task 2      │     │     Task 3      │
│  (Phase 1)      │     │  (Phase 2)      │     │  (Phase 3)      │
├─────────────────┤     ├─────────────────┤     ├─────────────────┤
│ Memory: []      │────▶│ Memory: [       │────▶│ Memory: [       │
│                 │     │   user: Task1   │     │   user: Task1   │
│ Runs agent,     │     │   asst: Resp1   │     │   asst: Resp1   │
│ gets response   │     │   ...tool calls │     │   user: Task2   │
│                 │     │ ]               │     │   asst: Resp2   │
│ Returns msgs    │     │                 │     │   ...           │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

**Key Design**: System messages are NOT persisted (they're task-specific). Only user/assistant messages flow between tasks.

---

## File Structure

```
deepclause-sdk/
├── src/
│   ├── index.ts          # Public exports
│   ├── sdk.ts            # SDK factory and main interface
│   ├── runner.ts         # DML execution engine
│   ├── agent.ts          # LLM agent loop with tool orchestration
│   ├── tools.ts          # Tool registry (AgentVM, Search tools)
│   ├── types.ts          # TypeScript type definitions
│   ├── cli/              # CLI implementation
│   │   ├── index.ts      # CLI entry point
│   │   ├── commands.ts   # Command handlers
│   │   ├── run.ts        # DML execution command
│   │   ├── compile.ts    # Compilation/validation
│   │   ├── config.ts     # Configuration management
│   │   ├── tools.ts      # CLI-specific tool setup
│   │   ├── search.ts     # Search tool integration
│   │   ├── mcp.ts        # MCP protocol support
│   │   ├── prompt.ts     # User prompting
│   │   └── tui/          # Terminal UI components
│   │       └── index.ts
│   ├── prolog/           # Prolog-JS bridge
│   │   ├── loader.ts     # SWI-Prolog WASM loader
│   │   └── bridge.ts     # JS-Prolog data conversion
│   └── prolog-src/       # Prolog source files
│       ├── deepclause_mi.pl      # Meta-interpreter
│       ├── deepclause_strings.pl # String interpolation
│       └── deepclause_memory.pl  # Memory utilities
├── sdk-examples/         # TypeScript SDK examples
│   ├── basic-usage.ts    # Simple SDK usage
│   ├── deep-research.ts  # Multi-phase research agent
│   ├── neurosymbolic.ts  # Hybrid logic+LLM example
│   ├── quick-start.ts    # Minimal getting started
│   └── no-api-key.ts     # Usage without LLM
├── dml-examples/         # DML file examples
│   ├── deep_research.dml # Research agent DML
│   ├── vm-exec.dml       # AgentVM code execution
│   └── test.dml          # Simple test DML
├── tests/
│   ├── cli.test.ts       # CLI tests
│   ├── run.test.ts       # DML execution tests
│   ├── compile.test.ts   # Compilation tests
│   ├── config.test.ts    # Configuration tests
│   ├── language-features.test.ts  # DML language tests
│   └── llm-integration.test.ts    # LLM integration tests
├── docs/
│   └── DML_REFERENCE.md  # DML language reference
├── vendor/
│   └── swipl-wasm/       # SWI-Prolog WASM runtime
└── dist/                 # Compiled output
```

---

## Key Design Decisions

### 1. Cooperative Execution
Instead of blocking on LLM calls, the Prolog engine yields control back to TypeScript. This enables:
- Async/await compatibility
- Cancellation via AbortSignal
- Streaming responses
- Event-driven architecture

### 2. State Threading
Memory is threaded through the Prolog state (not global dynamic predicates). This enables:
- Backtracking without memory corruption
- Isolated sessions
- Predictable state management

### 3. Separation of Concerns
- **Prolog**: Logic, control flow, symbolic reasoning
- **TypeScript**: I/O, LLM calls, tool execution
- **LLM**: Natural language understanding, generation

### 4. Tool Abstraction
Tools are registered in TypeScript and exposed to both:
- Prolog via `exec(tool_name, Result)` 
- LLM agent via tool calling

---

## DML Language

DML (DeepClause Markup Language) is Prolog with extensions:

```prolog
% Entry point - task() returns results via Prolog unification
agent_main(Topic) :-
    system("You are a research assistant"),
    task("Research the topic: ~w", [Topic]),
    task("Write a summary of your findings", Summary),  % Summary bound by LLM
    output(Summary),   % output/1 is a Prolog predicate
    answer("Done!").   % answer/1 is a Prolog predicate

% String interpolation
task("Hello ~w!", [Name]).

% Parameter access  
param(api_key, Key).

% External tools via exec/2 (Prolog predicate)
exec(web_search(query: Topic), SearchResults),  % Brave web search
exec(vm_exec(command: "python3 script.py"), ExecResult),  % AgentVM code execution

% Tool definitions (LLM-accessible wrappers)
tool(search(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query), Results).
```

**Prolog predicates** (callable from DML): `task/N`, `system/1`, `user/1`, `output/1`, `answer/1`, `exec/2`, `param/2`

**LLM-internal tools** (only callable by LLM inside task): `finish()`, `set_result()`, plus any `tool/2` definitions

---

## Event Types

| Event | Description |
|-------|-------------|
| `output` | Final text output from agent |
| `stream` | Streaming text chunk (with `done` flag) |
| `log` | Debug/info log message |
| `answer` | Answer from `answer/1` predicate |
| `tool_call` | Tool invocation (includes `toolName`, `toolArgs`, `toolResult`) |
| `input_required` | Waiting for user input (includes `prompt`) |
| `error` | Execution error |
| `finished` | Execution complete (may include `trace` and `results`) |

---

## LLM Provider Support

The SDK uses Vercel AI SDK for LLM abstraction:

| Provider | Models | API Key Env Var |
|----------|--------|-----------------|
| OpenAI | gpt-4o, gpt-4, etc. | `OPENAI_API_KEY` |
| Anthropic | claude-3-*, etc. | `ANTHROPIC_API_KEY` |
| Google | gemini-* | `GOOGLE_GENERATIVE_AI_API_KEY` |
| OpenRouter | Any | `OPENROUTER_API_KEY` |

Provider is auto-detected from model name or can be explicitly specified.

---

## Example: Deep Research Agent

```typescript
const sdk = await createDeepClause({ model: 'gemini-2.0-flash' });

// External tools are pre-registered: web_search, news_search, vm_exec, ask_user
// LLM-internal tools (finish, set_result) are automatic inside task()

const dml = `
agent_main(Topic) :-
    system("You are a research analyst with web search capabilities"),
    task("Search for information about: ~w", [Topic]),
    task("Write a comprehensive report based on your research", Report),
    % Report is bound by the LLM via set_result internally
    output(Report),
    answer("Research complete!").
`;

for await (const event of sdk.runDML(dml, { args: ["AI trends"] })) {
  if (event.type === 'stream') process.stdout.write(event.content);
  if (event.type === 'answer') console.log(event.content);
}
```

---

## Future Considerations

1. **Parallel task execution** - Running independent tasks concurrently
2. **Persistent memory** - Save/restore conversation across sessions
3. **Tool sandboxing** - Enhanced security for tool execution
4. **Debugging tools** - Interactive debugger for DML execution
5. **Compiled DML** - Pre-compile DML for faster startup
