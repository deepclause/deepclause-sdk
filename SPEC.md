# DeepClause SDK - Simplified DML Language Specification

## Overview

This document specifies a radically simplified version of the DeepClause Meta Language (DML). The new design eliminates `@-predicates` in favor of a cleaner `task()` predicate that runs an agent loop, while keeping the power of Prolog-based symbolic reasoning and external tool integration.

## Design Goals

1. **Simplicity**: Remove `@-predicates` complexity, use `task()` for all LLM interactions
2. **Clarity**: Clear separation between symbolic Prolog code and LLM agent loops
3. **Backtrackable Memory**: LLM conversation memory integrated with Prolog's backtracking
4. **Self-Contained**: New SDK package independent of legacy code
5. **Modern JS API**: Clean async generator-based API with Vercel AI SDK
6. **Security**: White/blacklist rules for external tool calls

---

## 1. Core Language Changes

### 1.1 Removal of @-Predicates

The old `@("instruction")` syntax for LLM-powered predicates is **removed entirely**.

**Old syntax (removed):**
```prolog
extract_temperature(Data, Temp) :- 
    @("Extract the temperature from Data in celsius and output in Temp").
```

### 1.2 The `task/1` and `task/N` Predicates

All LLM interactions now go through the `task()` predicate which runs an agent loop.

#### `task(TaskDescription)` - Simple task execution
```prolog
agent_main :-
    task("Summarize the current state of AI research").
```

#### `task(TaskDescription, Var1, Var2, ...)` - Task with output variables
```prolog
agent_main :-
    task("Search for the top 3 programming languages and store in Results", Results),
    format(string(Msg), "Found: ~w", [Results]),
    answer(Msg).
```

The task description can reference variable names that will be bound:
```prolog
agent_main :-
    task("Analyze this text and extract the main topic, store it in Topic", Topic),
    task("Find related articles about {Topic}, store URLs in Articles", Articles).
```

### 1.3 Agent Loop Tools (LLM-Only, Not Prolog Predicates)

When `task()` runs, the LLM agent has access to these built-in tools. **These are NOT Prolog predicates** - they can only be called by the LLM inside a task, not from DML code directly:

| Tool | Description |
|------|-------------|
| `finish(true)` | Complete task successfully, `task()` succeeds |
| `finish(false)` | Complete task with failure, `task()` fails (triggers backtracking) |
| `ask_user(prompt)` | Pause and wait for user input, stays in agent loop |
| `set_result(variable, value)` | Store a value in one of the task's output variables |
| Any `tool(...)` predicate | User-defined tool wrappers from the DML source file |

**Note:** Results from `task()` are returned via Prolog unification, not by calling `set_result` in DML code.

### 1.4 External Tool Execution

External tools (web search, file operations, VM execution, etc.) use the `exec/2` predicate:

```prolog
agent_main :-
    exec(web_search("AI news"), Results),
    task("Summarize these search results: {Results}").
```

### 1.5 User-Defined Tools (`tool/1` and `tool/2`)

User-defined tool wrappers expose Prolog predicates to the task agent.

#### Basic Syntax (`tool/1`)

```prolog
% Simple form - last arg is output, all others are inputs
tool(search_web(Query, Results)) :-
    exec(web_search(Query), Results).
```

#### With Description (`tool/2`)

```prolog
% tool/2 adds a description for the LLM
tool(sub_agent(SubTask, Output),
     "Delegate a subtask to a specialized sub-agent") :-
    push_context,
    task(SubTask, Output),
    pop_context.

tool(calculate(Expr, Result),
     "Calculate a mathematical expression") :-
    exec(calculator(Expr), Result).
```

#### Schema Inference Rules

- **First N-1 arguments**: inputs (type: string)
- **Last argument**: output (type: string)
- **Parameter names**: auto-generated as `arg1`, `arg2`, etc.

The Prolog implementation source code is automatically included in the tool description sent to the LLM.

#### Example Usage

```prolog
% Define tools with descriptions
tool(search_web(Query, Results),
     "Search the web for information") :-
    exec(web_search(Query), Results).

tool(sub_agent(Task, Output),
     "Run a subtask with a fresh context") :-
    push_context,
    task(Task, Output),
    pop_context.

agent_main :-
    % Task agent can now call search_web() and sub_agent()
    task("Use search_web to find info about Prolog, store in Info", Info).
```

---

## 2. Memory Management

### 2.1 Conversation Memory Model

The meta-interpreter maintains an LLM conversation memory as a list of messages:
```prolog
[
    message{role: system, content: "You are a helpful assistant"},
    message{role: user, content: "Hello"},
    message{role: assistant, content: "Hi there!"}
]
```

### 2.2 Memory Predicates

| Predicate | Description |
|-----------|-------------|
| `system(Text)` | Add system message to memory |
| `user(Text)` | Add user message to memory |
| `push_context` | Save current memory state (for backtracking) |
| `pop_context` | Restore previous memory state |
| `answer(Text)` | Send final answer to user and add to memory |

### 2.3 Backtrackable Memory

Memory changes integrate with Prolog's backtracking:

```prolog
agent_main :-
    system("You are a research assistant"),
    push_context,
    (   task("Find recent AI papers")
    ;   pop_context,
        task("Search for AI news instead")  % Falls back with restored memory
    ).
```

---

## 3. Parameters

Parameters are passed directly to `agent_main`:

```prolog
agent_main(Topic, MaxResults) :-
    format(string(Query), "Research ~w", [Topic]),
    task(Query, Results),
    answer(Results).
```

Invocation from JS:
```javascript
await runDML(code, { Topic: "quantum computing", MaxResults: 5 });
```

For UI parameter declarations (file pickers, dropdowns, etc.):
```prolog
:- param("topic", "Enter research topic").
:- param("file:file", "Select input file").
:- param("format:select(json,csv,xml)", "Output format").

agent_main :-
    param("topic", _, Topic),
    param("file:file", _, File),
    param("format:select(json,csv,xml)", _, Format),
    % ... use Topic, File, Format
```

---

## 4. Built-in Predicates Reference

### 4.1 Core Control

| Predicate | Description |
|-----------|-------------|
| `task(Desc)` | Run agent loop for task description |
| `task(Desc, V1, ...)` | Run agent loop with output variables |
| `exec(Tool, Output)` | Execute external tool |
| `tool(Goal) :- Body` | Define a tool with implicit schema (last arg = output) |
| `tool(Goal, Desc) :- Body` | Define a tool with explicit description |

### 4.2 Memory

| Predicate | Description |
|-----------|-------------|
| `system(Text)` | Add system message |
| `user(Text)` | Add user message |
| `push_context` | Save memory state |
| `pop_context` | Restore memory state |

### 4.3 Output

| Predicate | Description |
|-----------|-------------|
| `answer(Text)` | Send final answer |
| `yield(Text)` | Stream intermediate output |
| `log(Text)` | Log message (visible to user) |

### 4.4 Prolog Standard Library

All SWI-Prolog built-ins remain available:
- `library(lists)`, `library(clpfd)`, `library(clpr)`
- `library(strings)`, `library(readutil)`
- `library(quasi_quotations)`
- File I/O: `open/3`, `read_string/3`, `close/1`

---

## 5. JavaScript API

### 5.1 Core API

```typescript
interface DeepClauseSDK {
  // Main execution function
  runDML(
    code: string, 
    options?: RunOptions
  ): AsyncGenerator<DMLEvent>;
  
  // Register external tools
  registerTool(name: string, tool: ToolDefinition): void;
  
  // Tool access control
  setToolPolicy(policy: ToolPolicy): void;
}

interface RunOptions {
  params?: Record<string, any>;        // Parameters for agent_main
  workspacePath?: string;              // Path to workspace directory
  model?: string;                      // LLM model to use
  onUserInput?: (prompt: string) => Promise<string>;  // User input handler
}

interface DMLEvent {
  type: 'output' | 'log' | 'answer' | 'input_required' | 'error' | 'finished';
  content?: string;
  prompt?: string;  // For input_required
}

interface ToolDefinition {
  description: string;
  parameters: z.ZodSchema;
  execute: (args: any) => Promise<any>;
}

interface ToolPolicy {
  mode: 'whitelist' | 'blacklist';
  tools: string[];  // Tool names to allow/deny
}
```

### 5.2 Usage Example

```typescript
import { createDeepClause } from 'deepclause-sdk';

const dml = await createDeepClause({
  apiKey: process.env.OPENAI_API_KEY,
  model: 'gpt-4o',
});

// Register custom tool
dml.registerTool('fetch_weather', {
  description: 'Get current weather for a location',
  parameters: z.object({ location: z.string() }),
  execute: async ({ location }) => {
    // ... fetch weather
    return { temp: 72, condition: 'sunny' };
  }
});

// Set tool policy
dml.setToolPolicy({
  mode: 'whitelist',
  tools: ['web_search', 'fetch_weather', 'read_file']
});

// Run DML code
const code = `
agent_main(Topic) :-
    task("Research {Topic} and summarize", Summary),
    answer(Summary).
`;

for await (const event of dml.runDML(code, { params: { Topic: 'AI safety' }})) {
  switch (event.type) {
    case 'output':
    case 'log':
      console.log(event.content);
      break;
    case 'answer':
      console.log('Final answer:', event.content);
      break;
    case 'input_required':
      // Handle user input
      break;
    case 'error':
      console.error(event.content);
      break;
  }
}
```

---

## 6. Architecture

### 6.1 Layer Structure

```
┌─────────────────────────────────────────────────────┐
│           JavaScript Runtime (Node.js)              │
│  ┌───────────────────────────────────────────────┐  │
│  │  DeepClause SDK                               │  │
│  │  - runDML() async generator                   │  │
│  │  - Tool registration & policy                 │  │
│  │  - Vercel AI SDK integration                  │  │
│  └───────────────────────────────────────────────┘  │
│                        ↕ FFI                        │
│  ┌───────────────────────────────────────────────┐  │
│  │  SWI-Prolog WASM                              │  │
│  │  - deepclause_mi.pl (meta-interpreter)        │  │
│  │  - Memory management                          │  │
│  │  - String interpolation                       │  │
│  │  - Cooperative execution                      │  │
│  └───────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────┘
```

### 6.2 Execution Flow

1. **JS: `runDML(code, options)`**
   - Initialize SWI-Prolog WASM if needed
   - Parse DML code into Prolog clauses
   - Create engine with `agent_main` goal

2. **Prolog: Meta-interpreter loop**
   - Execute goal, intercept special predicates
   - For `task()`: yield `request_agent_loop` to JS
   - For `exec()`: yield `request_exec` to JS
   - For `answer/yield`: yield `output` to JS

3. **JS: Handle requests**
   - `request_agent_loop`: Run LLM agent loop with Vercel AI SDK
   - `request_exec`: Execute tool, return result to Prolog
   - `output`: Yield event to caller

4. **Repeat until finished or error**

### 6.3 No More mi.qsave

The meta-interpreter is loaded as plain `.pl` files at runtime:
- `deepclause_mi.pl` - Core meta-interpreter
- `deepclause_strings.pl` - String interpolation
- `deepclause_memory.pl` - Memory management

---

## 7. Tool White/Blacklist Implementation

### 7.1 Policy Configuration

```javascript
// Whitelist mode: only listed tools can be used
sdk.setToolPolicy({
  mode: 'whitelist',
  tools: ['web_search', 'read_file', 'write_file']
});

// Blacklist mode: listed tools are blocked
sdk.setToolPolicy({
  mode: 'blacklist', 
  tools: ['vm_exec', 'shell_command']
});
```

### 7.2 Enforcement

When `exec(Tool, Output)` is called:
1. Extract tool name from `Tool` term
2. Check against policy
3. If blocked: fail with error message
4. If allowed: execute and return result

---

## 8. File Structure

```
deepclause-sdk/
├── package.json
├── tsconfig.json
├── src/
│   ├── index.ts              # Main exports
│   ├── sdk.ts                # DeepClauseSDK class
│   ├── runner.ts             # DML execution runner
│   ├── tools.ts              # Tool definitions and management
│   ├── prolog/
│   │   ├── loader.ts         # SWI-Prolog WASM loader
│   │   └── bridge.ts         # JS-Prolog bridge
│   └── prolog-src/
│       ├── deepclause_mi.pl      # Meta-interpreter
│       ├── deepclause_strings.pl # String interpolation
│       └── deepclause_memory.pl  # Memory management
├── tests/
│   ├── task.test.ts          # task() predicate tests
│   ├── exec.test.ts          # exec() tool tests
│   ├── memory.test.ts        # Memory/backtracking tests
│   ├── api.test.ts           # JS API tests
│   └── policy.test.ts        # Tool policy tests
└── examples/
    ├── hello-world.ts
    ├── research-agent.ts
    └── custom-tools.ts
```

---

## 9. Migration from Old DML

### 9.1 @-Predicate Conversion

**Old:**
```prolog
extract_topics(Text, Topics) :-
    @("Extract main topics from Text as a list").

agent_main :-
    tool(web_search("AI"), Results),
    extract_topics(Results, Topics),
    answer(Topics).
```

**New:**
```prolog
agent_main :-
    exec(web_search("AI"), Results),
    task("Extract main topics from this text: {Results}, store in Topics", Topics),
    answer(Topics).
```

### 9.2 Tool Calls

**Old:** `tool(web_search(Query), Results)`
**New:** `exec(web_search(Query), Results)`

### 9.3 Chat/LLM Calls

**Old:** `chat("Summarize this")`
**New:** `task("Summarize the current context")`

---

## 10. Implementation Phases

### Phase 1: Core Infrastructure
- [ ] Package structure setup
- [ ] SWI-Prolog WASM integration
- [ ] Basic JS-Prolog bridge

### Phase 2: Meta-interpreter
- [ ] `deepclause_mi.pl` with task/exec handling
- [ ] Memory management with backtracking
- [ ] String interpolation

### Phase 3: Agent Loop
- [ ] Vercel AI SDK integration
- [ ] Tool execution in agent loop
- [ ] finish/ask_user/store tools

### Phase 4: External Tools
- [ ] Tool registration API
- [ ] Tool policy enforcement
- [ ] Built-in tools (web_search, file I/O, etc.)

### Phase 5: Polish
- [ ] Error handling
- [ ] Comprehensive tests
- [ ] Documentation
- [ ] Examples

---

## Appendix A: Example DML Programs

### A.1 Simple Research Agent

```prolog
agent_main(Topic) :-
    system("You are a research assistant. Be thorough and cite sources."),
    task("Search for recent information about {Topic}", SearchResults),
    task("Analyze and summarize the findings", Summary),
    answer(Summary).
```

### A.2 Multi-step with Fallback

```prolog
agent_main(Query) :-
    (   try_academic_search(Query, Results)
    ;   try_web_search(Query, Results)
    ;   Results = "No results found"
    ),
    task("Summarize: {Results}"),
    answer(Results).

try_academic_search(Query, Results) :-
    exec(google_scholar(Query), Results),
    Results \= "".

try_web_search(Query, Results) :-
    exec(web_search(Query), Results),
    Results \= "".
```

### A.3 User Interaction

```prolog
tool(clarify(Question, Answer)) :-
    % This exposes clarify to the task agent
    wait_for_input(Question, Answer).

agent_main :-
    task("Ask the user what they want to research, use clarify tool", Topic),
    task("Research {Topic} thoroughly"),
    task("Write a comprehensive report", Report),
    answer(Report).
```

### A.4 Sub-agent Pattern

```prolog
tool(delegate(SubTask, Output)) :-
    push_context,
    task(SubTask, Output),
    pop_context.

agent_main(ComplexTask) :-
    task("Break down {ComplexTask} into subtasks", Subtasks),
    process_subtasks(Subtasks, Results),
    task("Combine all results: {Results}", FinalAnswer),
    answer(FinalAnswer).

process_subtasks([], []).
process_subtasks([Task|Tasks], [Result|Results]) :-
    task("Execute subtask using delegate: {Task}", Result),
    process_subtasks(Tasks, Results).
```
