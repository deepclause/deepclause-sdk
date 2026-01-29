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
- Handle built-in tools: `finish()`, `ask_user()`, `store()`
- Convert registered tools to AI SDK format
- Support streaming responses
- Maintain conversation history within a task
- Return messages for memory persistence

**Agent Loop Cycle**:
```
1. Build messages (system + history + task)
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

### 5. Tool System (`tools.ts`)

**Purpose**: Tool registration and access control.

**Features**:
- Register external tools with Zod or JSON Schema
- Whitelist/blacklist policy with wildcard support
- Built-in tools (filesystem, etc.) with schema validation

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
│   ├── agent.ts          # LLM agent loop
│   ├── tools.ts          # Tool registry and policy
│   ├── types.ts          # TypeScript type definitions
│   ├── prolog/
│   │   ├── loader.ts     # SWI-Prolog WASM loader
│   │   ├── bridge.ts     # JS-Prolog data conversion
│   │   ├── deepclause_mi.pl      # Meta-interpreter
│   │   ├── deepclause_strings.pl # String interpolation
│   │   └── deepclause_memory.pl  # Memory utilities
│   └── prolog-src/       # Prolog source (copied to dist)
│       └── deepclause_mi.pl
├── examples/
│   ├── basic-usage.ts    # Simple example
│   ├── deep-research.ts  # Multi-phase research agent
│   ├── neurosymbolic.ts  # Hybrid logic+LLM example
│   └── quick-start.ts    # Minimal getting started
├── tests/
│   ├── api.test.ts       # API tests
│   ├── policy.test.ts    # Tool policy tests
│   ├── memory.test.ts    # Memory threading tests
│   └── ...
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
% Entry point
agent_main(Topic) :-
    system("You are a research assistant"),
    task("Research the topic: ~w", [Topic]),
    task("Write a summary of your findings", Summary),
    answer(Summary).

% String interpolation
task("Hello ~w!", [Name]).

% Parameter access  
param(api_key, Key).

% External tools
exec(brave_search(Query), Results).

% Tool definitions
tool(my_tool(Input, Output), "Description") :-
    ... implementation ...
```

---

## Event Types

| Event | Description |
|-------|-------------|
| `output` | Final text output from agent |
| `stream` | Streaming text chunk |
| `log` | Debug/info log message |
| `answer` | Answer from `answer/1` predicate |
| `input_required` | Waiting for user input |
| `error` | Execution error |
| `finished` | Execution complete (may include trace) |

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

sdk.registerTool('brave_search', {
  description: 'Search the web',
  parameters: z.object({ query: z.string() }),
  execute: async ({ query }) => searchBrave(query),
});

const dml = `
agent_main(Topic) :-
    system("You are a research analyst"),
    task("Search for information about: ~w", [Topic]),
    task("Write a comprehensive report based on your research", Report),
    exec(write_file("report.md", Report), _),
    answer("Report saved!").
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
