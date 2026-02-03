# DeepClause CLI Tool Specification

**Version:** 1.0.0  
**Last Updated:** January 29, 2026

## Table of Contents

1. [Overview](#overview)
2. [Installation & Setup](#installation--setup)
3. [Command Reference](#command-reference)
4. [Configuration](#configuration)
5. [Compilation System](#compilation-system)
6. [MD-to-DML Conversion Prompt](#md-to-dml-conversion-prompt)
7. [DML Language Reference](#dml-language-reference)
8. [DML Examples Library](#dml-examples-library)
9. [Architecture](#architecture)
10. [TUI Presentation](#tui-presentation)

---

## Overview

The DeepClause CLI (`deepclause`) is a command-line tool for compiling natural language task descriptions (Markdown) into executable DML (DeepClause Meta Language) programs, and running them with LLM-powered execution.

### Key Features

- **Markdown to DML compilation**: Convert human-readable task descriptions into executable agent programs
- **LLM-powered execution**: Run DML programs with multiple LLM providers
- **MCP tool integration**: Connect to Model Context Protocol servers for external tools
- **Rich TUI output**: Beautiful terminal UI for interactive use
- **Headless mode**: Clean output for scripting and automation

---

## Installation & Setup

```bash
# Global installation
npm install -g deepclause-cli

# Or use npx
npx deepclause [command]
```

### First-time Setup

```bash
# Interactive configuration wizard
deepclause configure
```

---

## Command Reference

### Synopsis

```
deepclause [command] [options] [arguments]
```

### Commands Overview

| Command | Description |
|---------|-------------|
| `configure` | Configure LLM providers and MCP servers |
| `compile` | Compile Markdown to DML |
| `run` | Execute a DML program |
| `list-tools` | Show available tools |
| `list-commands` | Show compiled DML commands |
| `show-model` | Display current model configuration |
| `help` | Show help information |

---

### `deepclause configure`

Interactive configuration wizard for setting up LLM providers and MCP servers.

```bash
deepclause configure
```

**Prompts:**
1. Select model provider (openai, anthropic, google, openrouter)
2. Enter API key
3. Select default model
4. Configure MCP servers (optional)

#### `deepclause configure mcp-add`

Add an MCP server configuration.

```bash
deepclause configure mcp-add --name <name> --type <type> [options]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--name <name>` | Unique identifier for the MCP server |
| `--type <type>` | Connection type: `stdio`, `http`, `streamable-http`, `sse` |
| `--command <cmd>` | Command to start server (for `stdio` type) |
| `--url <url>` | Server URL (for `http`, `streamable-http`, `sse` types) |
| `--env <key=value>` | Environment variables (can be repeated) |

**Examples:**

```bash
# Add a custom MCP server (stdio)
deepclause configure mcp-add \
  --name my-tools \
  --type stdio \
  --command "npx -y my-mcp-server"

# Add an HTTP-based MCP server
deepclause configure mcp-add \
  --name custom-api \
  --type http \
  --url "http://localhost:3001"
```

> **Note:** File access is available via Prolog predicates (`read_file_to_string/3`, `write_to_file/2`) or through the `vm_exec` tool. No separate filesystem MCP server is needed.

#### `deepclause configure mcp-remove`

Remove an MCP server configuration.

```bash
deepclause configure mcp-remove --name <name>
```

#### `deepclause configure mcp-list`

List configured MCP servers.

```bash
deepclause configure mcp-list
```

---

### `deepclause compile`

Compile a Markdown task description into a DML program.

```bash
deepclause compile <input_file.md> [output_path]
```

**Arguments:**

| Argument | Description |
|----------|-------------|
| `input_file.md` | Source Markdown file describing the task |
| `output_path` | Output directory (relative to `.deepclause/`), default: same as input |

**Options:**

| Option | Description |
|--------|-------------|
| `--force` | Overwrite existing DML file |
| `--validate-only` | Only validate, don't save |
| `--prompt <file>` | Use custom conversion prompt |
| `--verbose` | Show detailed compilation output |

**Output Files:**

```
.deepclause/
  └── [output_path]/
      ├── input_file.dml       # Compiled DML program
      └── input_file.meta.json # Metadata file
```

**Meta File Structure:**

```json
{
  "version": "1.0.0",
  "source": "tasks/competitor_analysis.md",
  "sourceHash": "sha256:abc123...",
  "compiledAt": "2026-01-29T10:30:00Z",
  "model": "gpt-4o",
  "description": "Analyzes competitors in a given market segment",
  "parameters": [
    { "name": "company", "description": "Company name to analyze", "required": true },
    { "name": "depth", "description": "Analysis depth (shallow/deep)", "default": "shallow" }
  ],
  "tools": ["web_search", "vm_exec"],
  "history": [
    { 
      "version": 1, 
      "timestamp": "2026-01-29T10:30:00Z", 
      "sourceHash": "sha256:abc123...",
      "model": "gpt-4o"
    },
    { 
      "version": 2, 
      "timestamp": "2026-01-30T14:15:00Z", 
      "sourceHash": "sha256:def456...",
      "model": "claude-3-opus"
    }
  ]
}
```

**Tool Dependencies:**

The `tools` array is automatically populated during compilation by analyzing the generated DML:
- Only **external tools** (MCP/AgentVM) invoked via `exec/2` are included
- DML tool wrappers (defined with `tool/3`) are not included unless they call external tools
- At runtime, the CLI verifies all listed tools are available before execution
- Missing tools result in a clear error message listing what needs to be configured

```

**Examples:**

```bash
# Compile a task description
deepclause compile tasks/competitor_analysis.md

# Compile to a specific output directory
deepclause compile tasks/competitor_analysis.md tools/

# Validate without saving
deepclause compile tasks/competitor_analysis.md --validate-only
```

---

### `deepclause run`

Execute a compiled DML program.

```bash
deepclause run [options] <file> [args...]
```

**Arguments:**

| Argument | Description |
|----------|-------------|
| `file` | DML file path (with or without `.dml` extension) |
| `args...` | Arguments passed to `agent_main` |

**Options:**

| Option | Description |
|--------|-------------|
| `--workspace <path>` | Working directory for file operations (default: `./`) |
| `--dml-base <path>` | Base directory for DML files (default: `./.deepclause`) |
| `--verbose` | Show debug output including tool calls |
| `--headless` | Plain output only, no TUI formatting |
| `--trace <file>` | Save execution trace to file |
| `--param <key=value>` | Pass named parameter (can be repeated) |
| `--model <model>` | Override configured model |
| `--dry-run` | Show what would be executed without running |

**Examples:**

```bash
# Run a compiled DML with positional arguments
deepclause run tools/competitor_analysis "Acme Corp"

# Run with named parameters
deepclause run tools/research --param topic="AI Safety" --param depth=deep

# Run with trace output
deepclause run tools/analysis "data.csv" --trace ./traces/analysis.json

# Run in headless mode for scripting
deepclause run tools/report "Q4 2025" --headless > report.md
```

---

### `deepclause list-tools`

Display all available tools from registered MCP servers.

```bash
deepclause list-tools [options]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |
| `--server <name>` | Filter by MCP server name |

**Output Example:**

```
Available Tools:

🛠️ agentvm (built-in)
  ├─ vm_exec(command) - Execute shell command in sandboxed Alpine Linux VM

🔍 brave (built-in)
  ├─ web_search(query, count?, freshness?) - Search the web
  └─ news_search(query, count?, freshness?) - Search recent news

📁 File Access (via Prolog or vm_exec)
  ├─ read_file_to_string(Path, String, []) - Read file contents (Prolog)
  ├─ write_to_file(Path, Content) - Write to file (Prolog)
  └─ vm_exec("cat file.txt") - Read via shell command

---

### `deepclause list-commands`

Display all compiled DML commands with their descriptions.

```bash
deepclause list-commands [options]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |
| `--detailed` | Show parameters and tool dependencies |

**Output Example:**

```
Compiled Commands:

📋 tools/competitor_analysis
   Analyzes competitors in a given market segment
   Usage: deepclause run tools/competitor_analysis <company>

📋 tools/research
   Deep research agent with web search and report generation
   Usage: deepclause run tools/research --param topic=<topic>

📋 tools/code_review
   Automated code review with best practices analysis
   Usage: deepclause run tools/code_review <file_or_directory>
```

**Detailed Output Example (`--detailed`):**

```
Compiled Commands:

📋 tools/competitor_analysis
   Analyzes competitors in a given market segment
   Parameters:
     • company (required) - Company name to analyze
     • depth (optional, default: "shallow") - Analysis depth
   Tool Dependencies:
     • web_search (brave)
     • vm_exec (agentvm)
   Usage: deepclause run tools/competitor_analysis <company>

📋 tools/research
   Deep research agent with web search and report generation
   Parameters:
     • topic (required) - Research topic
     • max_sources (optional, default: 10) - Maximum sources
   Tool Dependencies:
     • web_search (brave)
     • fetch_url (brave-search)
   Usage: deepclause run tools/research --param topic=<topic>
```

---

### `deepclause show-model`

Display current model configuration.

```bash
deepclause show-model
```

**Output Example:**

```
Current Model Configuration:

  Provider:    OpenAI
  Model:       gpt-4o
  API Key:     sk-...abc (configured)
  Temperature: 0.7
  Max Tokens:  8192
```

---

## Configuration

### Configuration File Location

```
.deepclause/
  └── config.json
```

### Configuration Schema

```json
{
  "version": "1.0.0",
  "llm": {
    "provider": "openai",
    "model": "gpt-4o",
    "apiKey": "${OPENAI_API_KEY}",
    "temperature": 0.7,
    "maxTokens": 8192
  },
  "mcpServers": {
    "custom-tools": {
      "type": "stdio",
      "command": "npx",
      "args": ["-y", "my-mcp-server"],
      "env": {}
    }
  },
  "defaults": {
    "workspace": "./",
    "dmlBase": "./.deepclause",
    "verbose": false,
    "headless": false
  }
}
```

### Environment Variable Support

API keys and sensitive values can reference environment variables:

```json
{
  "apiKey": "${OPENAI_API_KEY}"
}
```

---

## Compilation System

### Overview

The compilation system converts natural language Markdown task descriptions into executable DML programs through an agentic loop:

1. **Parse** - Read and parse the Markdown input
2. **Convert** - Use LLM with conversion prompt to generate DML
3. **Validate** - Validate DML syntax using the SDK
4. **Iterate** - If validation fails, iterate with error feedback
5. **Save** - Write DML and metadata files

### Compilation Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    Markdown Input                           │
│  - Task description in natural language                     │
│  - Parameter specifications                                 │
│  - Tool requirements                                        │
│  - Expected behavior and edge cases                         │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  Conversion Prompt                          │
│  - DML language reference                                   │
│  - Pattern library with examples                            │
│  - Best practices and constraints                           │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  LLM Agent Loop                             │
│  1. Generate DML from markdown + prompt                     │
│  2. Validate with SDK                                       │
│  3. If errors, feed back and regenerate                     │
│  4. Maximum 3 iterations                                    │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                    Output Files                             │
│  .deepclause/path/file.dml      - Compiled program          │
│  .deepclause/path/file.meta.json - Metadata                 │
└─────────────────────────────────────────────────────────────┘
```

### Custom Prompt Override

The default conversion prompt can be overridden by placing a custom prompt file at:

```
.deepclause/md2dml.prompt
```

---

## MD-to-DML Conversion Prompt

The following is the default system prompt used to convert Markdown task descriptions into DML programs. It is designed to be comprehensive yet hackable.

The prompt is dynamically constructed at compile time to include:
1. The DML language reference (below)
2. The list of available tools from configured MCP servers and built-in AgentVM
3. Example DML programs

```markdown
# Markdown to DML Conversion Prompt

You are an expert DML (DeepClause Meta Language) programmer. Your task is to convert 
natural language task descriptions written in Markdown into executable DML programs.

## Tool System Overview

DeepClause has a layered tool system with different types of tools serving different purposes.

### Tool Categories

```
┌──────────────────────────────────────────────────────────────────────┐
│                        Tool Categories                                │
├──────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  1. EXTERNAL TOOLS (via exec/2)                                       │
│     Called directly from DML, execute in fresh isolated state         │
│     ├── AgentVM: vm_exec                                              │
│     ├── Search: web_search, news_search                               │
│     ├── User Input: ask_user                                          │
│     └── MCP Servers: custom configured tools                          │
│                                                                       │
│  2. DML TOOL WRAPPERS (via tool/2)                                    │
│     Defined in DML, available to LLM during task()                    │
│     └── Wrap external tools with custom logic/descriptions            │
│                                                                       │
│  3. LLM-INTERNAL TOOLS (NOT Prolog predicates!)                       │
│     Only callable by the LLM inside task(), not from DML code         │
│     ├── finish(success) - REQUIRED to complete task                   │
│     └── set_result(var, value) - store output variable values         │
│                                                                       │
└──────────────────────────────────────────────────────────────────────┘
```

### Built-in External Tools

These are always available via `exec/2`:

| Tool | Provider | Description |
|------|----------|-------------|
| `vm_exec(command: Cmd)` | AgentVM | Execute shell command in sandboxed Alpine Linux VM |
| `web_search(query: Q)` | Brave | Search the web, returns structured results |
| `news_search(query: Q)` | Brave | Search recent news articles |
| `ask_user(prompt: P)` | Internal | Request input from the user |

**Tip:** For Python code execution, use `vm_exec(command: "python3 -c 'your_code'")` or `vm_exec(command: "python3 script.py")`.

### LLM-Internal Tools (NOT Prolog Predicates)

These are **only callable by the LLM** during `task()` execution. They are NOT Prolog predicates and cannot be called directly from DML code:

| Tool | Description |
|------|-------------|
| `finish(success)` | **Required** - Signal task completion. Pass `true` for success, `false` for failure. |
| `set_result(variable, value)` | Store a value for an output variable. Used with `task(Desc, Var)`. |

**Important:** The DML code does NOT call these directly. When you write `task("Do something", Result)`, the LLM internally uses `set_result` to populate `Result`, and `finish` to complete. The result is returned to your DML code via Prolog unification.

**Wrong (these are not predicates):**
```prolog
agent_main :-
    task("Generate code", Code),
    set_result(output, Code),   % ERROR: set_result is not a predicate!
    finish(success).            % ERROR: finish is not a predicate!
```

**Correct:**
```prolog
agent_main :-
    task("Generate code", Code),  % Code is bound by LLM via set_result internally
    output(Code),                  % output/1 IS a predicate
    answer("Done!").               % answer/1 IS a predicate
```

### State and Isolation

**Critical Concept:** Tools and `exec()` calls run in **fresh, isolated state**:

| Context | Memory Access | State Changes | Backtracking |
|---------|---------------|---------------|--------------|
| Normal predicates | ✓ Full access | ✓ Persists | ✓ Full backtracking |
| `exec()` calls | ✗ No access | ✗ Isolated | ✗ Deterministic |
| DML tool bodies | ✗ Fresh state | ✗ Isolated | ✗ Deterministic |

This means:
- Memory added with `system()` or `user()` **backtrack** when execution backtracks
- Tool results from `exec()` are computed once and **don't backtrack**
- DML tool bodies (defined with `tool/2`) run in isolation - they cannot see or modify the main program's state

**Example of backtrackable memory:**
```prolog
% If approach_1 fails, memory from system() is backtracked
agent_main :-
    (   system("You are approach 1 assistant"),
        task("Try approach 1"),
        approach_1_succeeds
    ;   system("You are approach 2 assistant"),  % Fresh memory!
        task("Try approach 2")
    ).
```

## Available External Tools

The following tools are available for use with `exec/2`. Use only tools from this list.

### Built-in Tools (AgentVM)

| Tool | Description |
|------|-------------|
| `vm_exec(command)` | Execute any shell command in the sandboxed Alpine Linux VM |

### Built-in Search Tools

| Tool | Description |
|------|-------------|
| `web_search(query, count?, freshness?)` | Search the web using Brave Search API |
| `news_search(query, count?, freshness?)` | Search recent news articles |

### Built-in User Interaction

| Tool | Description |
|------|-------------|
| `ask_user(prompt)` | Ask the user a question and wait for response |

### Configured MCP Tools

{MCP_TOOLS_TABLE}

**Note:** When you use a tool in the generated DML, it will be registered as a dependency
in the compiled program's metadata. Only use tools that are actually needed.

## DML Language Overview

DML is a simplified Prolog dialect designed for AI agent programming. It combines 
declarative logic programming with LLM-powered task execution.

### Program Structure

Every DML program must have an `agent_main` entry point that accepts 0-2 arguments:

```prolog
% No arguments
agent_main :- ...

% One argument
agent_main(Topic) :- ...

% Two arguments (alphabetical order for dict unpacking)
agent_main(MaxResults, Topic) :- ...
```

### Core Predicates

#### Task Execution

| Predicate | Description |
|-----------|-------------|
| `task(Description)` | Execute an LLM task with current memory |
| `task(Description, Var)` | Execute task, bind result to Var |
| `task(Description, Var1, Var2)` | Execute task, bind two results |
| `prompt(Description)` | Execute LLM task with **fresh/empty memory** |
| `prompt(Description, Var)` | Prompt with fresh memory, bind result |

**Important:** Variable names in the description must match the Prolog variables:
```prolog
task("Analyze this and store the result in Summary.", Summary)
```

**task() vs prompt():** 
- `task()` inherits current memory (conversation context flows through)
- `prompt()` starts with empty memory (for isolated subtasks)

#### External Tool Execution

| Predicate | Description |
|-----------|-------------|
| `exec(Tool, Result)` | Execute external tool directly |

```prolog
exec(web_search(query: "AI news"), Results)
exec(vm_exec(command: "python3 script.py"), Output)
exec(ask_user(prompt: "What is your name?"), Response)
```

#### Memory Management (Backtrackable)

| Predicate | Description |
|-----------|-------------|
| `system(Text)` | Add system message (LLM instructions) |
| `user(Text)` | Add user message to context |
| `push_context` | Save memory state (for isolation) |
| `push_context(clear)` | Save and clear memory |
| `pop_context` | Restore previous memory state |

**Note:** Memory changes are backtrackable. If a branch fails and Prolog backtracks, memory is restored to its previous state.

#### Output

| Predicate | Description |
|-----------|-------------|
| `output(Text)` | Emit progress/intermediate output |
| `yield(Text)` | Alias for output/1 |
| `log(Text)` | Emit debug/log message |
| `answer(Text)` | Emit final answer (commits execution) |

#### DML Tool Wrappers

Define tools that the LLM can call during `task()`. These provide a named interface that wraps external tools:

```prolog
% Tool definition: tool(Head, Description) :- Body.
% - Head: tool name with parameters (last param is typically the result)
% - Description: string shown to the LLM
% - Body: implementation (usually calls exec/2)

tool(search(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query), Results).

tool(run_python(Code, Output), "Execute Python code") :-
    exec(vm_exec(command: "python3 -c '" ++ Code ++ "'"), Output).
```

**Important:** Tool bodies run in **fresh isolated state**. They cannot:
- Access memory from the main program
- Modify the main program's state
- Use backtracking from the main program

They are essentially stateless functions that the LLM can invoke.

#### Parameters

Access named parameters with `param/2`:

```prolog
param(topic, Topic)  % Bind Topic to the 'topic' parameter value
```

```prolog
param(name, "Description", Variable)
```

### String Interpolation

Use `{variable}` for interpolation in task descriptions:

```prolog
agent_main(Topic) :-
    task("Research the topic: {Topic}"),
    answer("Done").
```

Or use `format/3` for complex formatting:

```prolog
format(string(Query), "Find information about ~w in the year ~d", [Topic, Year])
```

### Control Flow

```prolog
% Conjunction (and)
goal1, goal2, goal3

% Disjunction (or)
(goal1 ; goal2)

% If-then-else
(Condition -> Then ; Else)

% Negation as failure
\+ goal

% Cut (commit to this branch)
!
```

### Backtracking

DML supports full Prolog backtracking across LLM calls:

```prolog
% Try multiple approaches
agent_main :-
    (   try_approach_1
    ;   try_approach_2  % Falls back if first fails
    ;   fallback_approach
    ),
    answer("Done").
```

### List Processing

```prolog
% Recursive list processing
process_items([]).
process_items([H|T]) :-
    process_one(H),
    process_items(T).

% Using findall
findall(X, some_condition(X), Results)

% Using maplist
maplist(process_one, Items)
```

---

## Common Patterns

### Pattern 1: Simple Task Agent

For straightforward tasks that need LLM reasoning:

```prolog
agent_main(Topic) :-
    system("You are a helpful research assistant."),
    task("Research {Topic} and provide a comprehensive summary."),
    answer("Research complete!").
```

### Pattern 2: Multi-Step Workflow

For tasks requiring sequential steps:

```prolog
agent_main(Topic) :-
    system("You are a thorough research assistant."),
    
    % Step 1: Gather information
    output("Step 1: Gathering information..."),
    task("Search for recent information about {Topic}. Store key findings in Findings.", Findings),
    
    % Step 2: Analyze
    output("Step 2: Analyzing findings..."),
    format(string(AnalysisTask), "Analyze these findings: ~w. Identify patterns and insights. Store in Analysis.", [Findings]),
    task(AnalysisTask, Analysis),
    
    % Step 3: Generate report
    output("Step 3: Generating report..."),
    format(string(ReportTask), "Create a structured report based on this analysis: ~w", [Analysis]),
    task(ReportTask),
    
    answer("Report generated successfully!").
```

### Pattern 3: Tool-Enabled Agent

For tasks requiring external tool access:

```prolog
% Define tools available to the LLM (description is second arg)
tool(search(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query, count: 10), Results).

tool(fetch_page(Url, Content), "Fetch and extract content from a URL") :-
    exec(fetch_url(url: Url), Content).

agent_main(Topic) :-
    system("You are a research assistant with web search capabilities.
            Use the search tool to find information.
            Use fetch_page to get detailed content from promising URLs."),
    
    task("Research {Topic} thoroughly using available tools. Synthesize your findings."),
    answer("Research complete!").
```

### Pattern 4: Iterative Refinement

For tasks requiring iteration until quality criteria met:

```prolog
agent_main(Task) :-
    system("You are a meticulous assistant that iterates until quality is achieved."),
    initial_attempt(Task, Draft),
    refine_until_good(Draft, Final),
    answer(Final).

initial_attempt(Task, Draft) :-
    format(string(T), "Create an initial draft for: ~w. Store in Draft.", [Task]),
    task(T, Draft).

refine_until_good(Current, Final) :-
    task("Review this work and rate quality 1-10. Store rating in Rating and feedback in Feedback.", Rating, Feedback),
    (   Rating >= 8
    ->  Final = Current
    ;   format(string(RefineTask), "Improve based on feedback: ~w. Current work: ~w. Store improved version in Improved.", [Feedback, Current]),
        task(RefineTask, Improved),
        refine_until_good(Improved, Final)
    ).
```

### Pattern 5: Parallel Sub-Tasks with Delegation

For complex tasks that can be decomposed:

```prolog
tool(delegate(SubTask, Result), "Delegate a subtask to a specialized sub-agent") :-
    push_context(clear),
    system("You are a focused specialist. Complete only the assigned task."),
    task("{SubTask} Store the result in Result.", Result),
    pop_context.

agent_main(ComplexTask) :-
    system("You are a project manager that decomposes and delegates tasks."),
    
    % Decompose the task
    task("Break down this task into 3-5 independent subtasks: {ComplexTask}. Store as a Prolog list in Subtasks.", Subtasks),
    
    % Process each subtask
    process_subtasks(Subtasks, Results),
    
    % Synthesize results
    format(string(SynthTask), "Synthesize these results into a coherent output: ~w", [Results]),
    task(SynthTask),
    
    answer("Complex task completed!").

process_subtasks([], []).
process_subtasks([Task|Tasks], [Result|Results]) :-
    tool(delegate(Task, Result)),
    process_subtasks(Tasks, Results).
```

### Pattern 6: Fallback Chain

For tasks where multiple approaches should be tried:

```prolog
agent_main(Query) :-
    system("You are a resourceful assistant."),
    (   try_precise_search(Query)
    ;   try_broad_search(Query)
    ;   try_inference(Query)
    ),
    answer("Query resolved!").

try_precise_search(Query) :-
    output("Trying precise search..."),
    exec(search_exact(query: Query), Results),
    Results \= [],
    task("Summarize these precise results: {Results}").

try_broad_search(Query) :-
    output("Trying broad search..."),
    exec(search_broad(query: Query), Results),
    Results \= [],
    task("Filter and summarize relevant results: {Results}").

try_inference(Query) :-
    output("No search results, using inference..."),
    task("Based on your knowledge, provide the best answer for: {Query}").
```

### Pattern 7: User Interaction Loop

For tasks requiring user clarification:

```prolog
tool(ask_user(Question, Response), "Ask the user a clarifying question") :-
    wait_for_input(Question, Response).

agent_main :-
    system("You are a helpful assistant. Ask clarifying questions when needed."),
    
    task("Greet the user and ask what they need help with today."),
    gather_requirements(Requirements),
    
    format(string(ExecuteTask), "Complete this task based on requirements: ~w", [Requirements]),
    task(ExecuteTask),
    
    answer("Task completed! Let me know if you need anything else.").

gather_requirements(Requirements) :-
    task("Do you have enough information to proceed? If yes, summarize requirements in Requirements. If no, ask a clarifying question using ask_user tool.", Requirements),
    (   Requirements \= needs_clarification
    ->  true
    ;   gather_requirements(Requirements)
    ).
```

### Pattern 8: Neurosymbolic Reasoning

Combining Prolog logic with LLM capabilities:

```prolog
% Knowledge base (symbolic rules)
category(laptop, electronics).
category(phone, electronics).
category(headphones, audio).

price_range(budget, 0, 500).
price_range(mid, 500, 1500).
price_range(premium, 1500, 10000).

in_budget(Product, MaxPrice) :-
    product_price(Product, Price),
    Price =< MaxPrice.

% Dynamic facts from LLM
:- dynamic product_price/2.

agent_main(Budget, Query) :-
    system("You are a product recommendation assistant."),
    
    % Use LLM to understand user intent
    task("Extract the product category from: {Query}. Store in Category.", Category),
    
    % Fetch product data
    exec(get_products(category: Category), Products),
    
    % Assert prices into knowledge base
    assert_prices(Products),
    
    % Use Prolog rules to filter
    findall(P, (member(P, Products), in_budget(P, Budget)), Filtered),
    
    % Use LLM to generate recommendation
    format(string(RecTask), "Recommend from these products: ~w for someone with budget $~w", [Filtered, Budget]),
    task(RecTask),
    
    answer("Here are my recommendations!").

assert_prices([]).
assert_prices([Product|Rest]) :-
    get_dict(name, Product, Name),
    get_dict(price, Product, Price),
    assertz(product_price(Name, Price)),
    assert_prices(Rest).
```

### Pattern 9: Report Generation with File Output

For tasks that produce file artifacts:

```prolog
agent_main(Topic) :-
    system("You are a report writer. Generate well-structured markdown reports."),
    
    % Research phase
    output("Researching topic..."),
    task("Research {Topic} thoroughly. Store findings in Findings.", Findings),
    
    % Writing phase
    output("Writing report..."),
    format(string(WriteTask), "Write a comprehensive markdown report about ~w based on these findings: ~w. Include sections for Introduction, Key Findings, Analysis, and Conclusion. Store the complete markdown in Report.", [Topic, Findings]),
    task(WriteTask, Report),
    
    % Save to file
    format(string(Filename), "~w_report.md", [Topic]),
    write_file(Filename, Report),
    
    format(string(Msg), "Report saved to ~w", [Filename]),
    answer(Msg).
```

### Pattern 10: Code Generation and Execution

For tasks involving code:

```prolog
% Helper tool to run Python code
tool(run_python(Code, Output), "Execute Python code in the sandboxed VM") :-
    % Save code to temp file and execute (handles multi-line code safely)
    exec(vm_exec(command: "cat > /tmp/script.py << 'ENDOFCODE'\n" ++ Code ++ "\nENDOFCODE"), _),
    exec(vm_exec(command: "python3 /tmp/script.py"), Result),
    get_dict(stdout, Result, Output).

agent_main(Task) :-
    system("You are a coding assistant. Write and test code to solve problems."),
    
    % Generate code
    task("Write Python code to solve: {Task}. Store only the code in Code.", Code),
    
    % Execute and capture output
    output("Executing code..."),
    tool(run_python(Code, Result)),
    
    % Analyze results
    format(string(AnalyzeTask), "The code produced this output: ~w. Explain the results.", [Result]),
    task(AnalyzeTask),
    
    answer("Code executed successfully!").
```

### Pattern 11: AgentVM Shell Commands

For tasks requiring shell commands, file operations, or network access:

```prolog
agent_main(Url) :-
    system("You are a web research assistant with shell access."),
    
    % Download web content
    output("Fetching content..."),
    format(string(Cmd), "wget -q -O- ~w | head -200", [Url]),
    exec(vm_exec(command: Cmd), WebResult),
    get_dict(stdout, WebResult, WebContent),
    
    % Process with Python
    task("Write Python code to extract all links from this HTML. Store in Code.", Code),
    exec(vm_exec(command: "python3 -c '" ++ Code ++ "'"), LinksResult),
    get_dict(stdout, LinksResult, Links),
    
    % Summarize findings
    format(string(SummaryTask), "Summarize what you found at this URL based on these links: ~w", [Links]),
    task(SummaryTask),
    
    answer("Web analysis complete!").
```

### Pattern 12: Data Pipeline with Package Installation

For data analysis tasks requiring external packages:

```prolog
agent_main(DataUrl, Question) :-
    system("You are a data scientist with full Python environment access."),
    
    % Setup environment
    output("Setting up environment..."),
    exec(vm_exec(command: "pip install pandas matplotlib seaborn"), _),
    
    % Download data
    output("Downloading data..."),
    format(string(DownloadCmd), "wget -q -O /tmp/data.csv ~w", [DataUrl]),
    exec(vm_exec(command: DownloadCmd), _),
    
    % Generate analysis code
    output("Analyzing data..."),
    format(string(AnalysisTask), "Write Python code to:
1. Load /tmp/data.csv with pandas
2. Answer this question: ~w
3. Print clear results
Store in Code.", [Question]),
    task(AnalysisTask, Code),
    
    % Save and execute analysis
    exec(vm_exec(command: "cat > /tmp/analysis.py << 'EOF'\n" ++ Code ++ "\nEOF"), _),
    exec(vm_exec(command: "python3 /tmp/analysis.py"), Result),
    get_dict(stdout, Result, Output),
    
    % Interpret results
    output("Interpreting results..."),
    format(string(InterpretTask), "Explain these analysis results in plain language: ~w", [Output]),
    task(InterpretTask),
    
    answer("Data analysis complete!").
```

---

## Conversion Guidelines

When converting Markdown to DML:

1. **Identify the core task** - What is the primary goal?
2. **Determine parameters** - What inputs does the agent need?
3. **Map to patterns** - Which DML pattern best fits?
4. **Define required tools** - What external capabilities are needed?
5. **Handle edge cases** - Add fallbacks and error handling
6. **Add progress output** - Keep users informed with `output/1`

### Input Markdown Structure

The input Markdown should contain:

```markdown
# Task Name

Brief description of what this task accomplishes.

## Parameters

- `param_name` (required/optional): Description
- `another_param` (optional, default: value): Description

## Requirements

- List of capabilities needed (web search, file access, etc.)

## Behavior

Detailed description of:
- Steps the agent should take
- Decision points and conditions
- Expected outputs

## Edge Cases

- What to do when X happens
- Fallback behaviors
```

### Output Requirements

Your DML output must:

1. Start with a comment header describing the program
2. Define tools with `tool/3` predicates (args, result, description)
3. Have a single `agent_main` entry point
4. Use appropriate system prompts
5. Include progress outputs for long-running tasks
6. End with `answer/1` to signal completion
7. Handle stated edge cases
8. **Only use tools from the Available Tools list** - these become dependencies

---

## Example Conversion

### Input Markdown

```markdown
# Competitor Analysis

Analyze a company's competitors and generate a strategic report.

## Parameters

- `company` (required): The company name to analyze
- `depth` (optional, default: "standard"): Analysis depth (quick/standard/deep)

## Requirements

- Web search capability
- File writing for report output

## Behavior

1. Search for the company and identify its industry
2. Find top 5 competitors in that industry
3. For each competitor, gather:
   - Company overview
   - Key products/services
   - Strengths and weaknesses
4. Generate a comparative analysis
5. Save report to file

## Edge Cases

- If company not found, ask user for clarification
- If fewer than 5 competitors found, proceed with available data
```

### Output DML

```prolog
% ============================================================
% Competitor Analysis Agent
% ============================================================
% Analyzes a company's competitors and generates a strategic report.
%
% Parameters:
%   - Company: Company name to analyze (required)
%   - Depth: Analysis depth - quick/standard/deep (default: standard)
%
% Output: Saves report to {company}_competitor_analysis.md

% Tool definitions (description as second argument)
tool(search_web(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query, count: 10), Results).

tool(ask_clarification(Question, Response), "Ask user for clarification") :-
    wait_for_input(Question, Response).

% Main entry point
agent_main(Company, Depth) :-
    system("You are a strategic business analyst specializing in competitive analysis.
            Be thorough, objective, and data-driven in your analysis.
            Structure your findings clearly."),
    
    % Validate company exists
    output("Identifying company and industry..."),
    identify_company(Company, Industry),
    
    % Find competitors
    output("Finding competitors..."),
    find_competitors(Company, Industry, Depth, Competitors),
    
    % Analyze each competitor
    output("Analyzing competitors..."),
    analyze_competitors(Competitors, Analyses),
    
    % Generate report
    output("Generating report..."),
    generate_report(Company, Industry, Analyses, Report),
    
    % Save report
    format(string(Filename), "~w_competitor_analysis.md", [Company]),
    write_file(Filename, Report),
    
    format(string(FinalMsg), "Analysis complete! Report saved to ~w", [Filename]),
    answer(FinalMsg).

% Default depth if not provided
agent_main(Company) :-
    agent_main(Company, standard).

% Identify company and industry with fallback
identify_company(Company, Industry) :-
    format(string(SearchQuery), "~w company industry sector", [Company]),
    tool(search_web(SearchQuery, Results)),
    (   Results \= []
    ->  format(string(Task), "Based on these search results: ~w, identify the primary industry for ~w. Store just the industry name in Industry.", [Results, Company]),
        task(Task, Industry)
    ;   format(string(ClarifyQ), "I couldn't find information about '~w'. Could you provide more details or check the spelling?", [Company]),
        tool(ask_clarification(ClarifyQ, Clarification)),
        identify_company(Clarification, Industry)
    ).

% Find competitors based on depth
find_competitors(Company, Industry, Depth, Competitors) :-
    competitor_count(Depth, Count),
    format(string(SearchQuery), "top competitors of ~w in ~w industry", [Company, Industry]),
    tool(search_web(SearchQuery, Results)),
    format(string(Task), "From these results: ~w, identify up to ~w main competitors of ~w. Store as a Prolog list of company names in Competitors.", [Results, Count, Company]),
    task(Task, Competitors),
    (   Competitors = []
    ->  log("Warning: No competitors found, using industry leaders"),
        format(string(FallbackTask), "List ~w leading companies in the ~w industry as a Prolog list in Competitors.", [Count, Industry]),
        task(FallbackTask, Competitors)
    ;   true
    ).

competitor_count(quick, 3).
competitor_count(standard, 5).
competitor_count(deep, 10).

% Analyze each competitor
analyze_competitors([], []).
analyze_competitors([Competitor|Rest], [Analysis|Analyses]) :-
    output("Analyzing " ++ Competitor ++ "..."),
    analyze_one(Competitor, Analysis),
    analyze_competitors(Rest, Analyses).

analyze_one(Competitor, Analysis) :-
    format(string(SearchQuery), "~w company overview products services", [Competitor]),
    tool(search_web(SearchQuery, Results)),
    format(string(Task), "Analyze ~w based on: ~w. Create a structured analysis with: 1) Company Overview, 2) Key Products/Services, 3) Strengths, 4) Weaknesses. Store in Analysis as a structured summary.", [Competitor, Results]),
    task(Task, Analysis).

% Generate final report
generate_report(Company, Industry, Analyses, Report) :-
    format(string(Task), "Create a comprehensive markdown report analyzing ~w's competitive landscape in the ~w industry. 
    
Competitor analyses: ~w

Structure the report with:
1. Executive Summary
2. Industry Overview  
3. Competitor Profiles (one section per competitor)
4. Comparative Analysis (table comparing key metrics)
5. Strategic Recommendations
6. Conclusion

Store the complete markdown report in Report.", [Company, Industry, Analyses]),
    task(Task, Report).
```

---

## Validation

After generating DML, validate:

1. **Syntax** - Valid Prolog syntax
2. **Entry point** - `agent_main` exists with correct arity
3. **Tool definitions** - All tools called via `tool(...)` have implementations
4. **Variables** - Variables in task descriptions match Prolog variables
5. **Termination** - All paths lead to `answer/1`

If validation fails, fix the issues and regenerate.
```

---

## DML Language Reference

See the comprehensive DML language reference in [docs/DML_REFERENCE.md](docs/DML_REFERENCE.md).

### Quick Reference

| Predicate | Description |
|-----------|-------------|
| `agent_main` | Program entry point |
| `task(Desc)` | Execute LLM task |
| `task(Desc, Var)` | Execute task with output |
| `exec(Tool, Result)` | Execute external tool |
| `system(Text)` | Add system message |
| `user(Text)` | Add user message |
| `output(Text)` | Emit progress output |
| `answer(Text)` | Emit final answer |
| `log(Text)` | Emit log message |
| `push_context` | Save memory state |
| `pop_context` | Restore memory state |
| `param(Key, Desc, Var)` | Access parameter |

---

## DML Examples Library

### Example 1: Simple Research Agent

**Use Case:** Quick research on any topic

```prolog
% simple_research.dml
% Quick research agent for any topic

agent_main(Topic) :-
    system("You are a knowledgeable research assistant."),
    task("Provide a comprehensive overview of {Topic}, covering key concepts, recent developments, and practical applications."),
    answer("Research complete!").
```

### Example 2: Code Review Agent

**Use Case:** Automated code review with best practices

```prolog
% code_review.dml
% Automated code review agent

tool(read_code(Path, Content), "Read source code from a file") :-
    read_file(Path, Content).

tool(write_review(Path, Content), "Write review to a file") :-
    write_file(Path, Content).

agent_main(FilePath) :-
    system("You are an expert code reviewer. Focus on:
            - Code quality and readability
            - Potential bugs and edge cases
            - Performance considerations
            - Security vulnerabilities
            - Best practices and design patterns
            Be constructive and provide specific suggestions."),
    
    output("Reading source code..."),
    tool(read_code(FilePath, Code)),
    
    output("Analyzing code..."),
    format(string(ReviewTask), "Review this code thoroughly:

```
~w
```

Provide a detailed review with:
1. Summary (overall assessment)
2. Issues Found (categorized by severity)
3. Suggestions for Improvement
4. Positive Aspects

Store as markdown in Review.", [Code]),
    task(ReviewTask, Review),
    
    format(string(ReviewPath), "~w.review.md", [FilePath]),
    tool(write_review(ReviewPath, Review)),
    
    format(string(Msg), "Review saved to ~w", [ReviewPath]),
    answer(Msg).
```

### Example 3: Meeting Summarizer

**Use Case:** Summarize meeting transcripts

```prolog
% meeting_summary.dml
% Meeting transcript summarizer

agent_main(TranscriptPath) :-
    system("You are an expert meeting summarizer. Extract key information accurately."),
    
    read_file(TranscriptPath, Transcript),
    
    output("Extracting key information..."),
    task("From this meeting transcript, extract:
          1. Attendees (store in Attendees)
          2. Main topics discussed (store in Topics)  
          3. Action items with owners (store in Actions)
          4. Key decisions made (store in Decisions)", 
          Attendees, Topics, Actions, Decisions),
    
    output("Generating summary..."),
    format(string(SummaryTask), "Create a professional meeting summary with:
          - Date/Attendees: ~w
          - Topics: ~w
          - Decisions: ~w
          - Action Items: ~w
          
          Format as clean markdown.", [Attendees, Topics, Decisions, Actions]),
    task(SummaryTask, Summary),
    
    format(string(SummaryPath), "~w_summary.md", [TranscriptPath]),
    write_file(SummaryPath, Summary),
    
    answer(Summary).
```

### Example 4: Data Analysis Agent

**Use Case:** Analyze CSV data and generate insights

```prolog
% data_analysis.dml
% CSV data analysis agent with full Python environment

agent_main(CsvPath, Question) :-
    system("You are a data analyst. Use Python with pandas for analysis.
            Always handle errors gracefully."),
    
    % Setup Python environment
    output("Setting up analysis environment..."),
    exec(vm_exec(command: "pip install pandas numpy"), _),
    
    % Load data - workspace is shared at /workspace
    output("Loading data..."),
    format(string(LoadCode), "
import pandas as pd
df = pd.read_csv('/workspace/~w')
print('Shape:', df.shape)
print('Columns:', df.columns.tolist())
print('First rows:', df.head())
print('Data types:', df.dtypes)
print('Basic stats:', df.describe())
    ", [CsvPath]),
    exec(vm_exec(command: "python3 -c '" ++ LoadCode ++ "'"), DataInfoResult),
    get_dict(stdout, DataInfoResult, DataInfo),
    
    output("Analyzing data..."),
    format(string(AnalysisTask), "Based on this data info:
~w

Write Python code to answer: ~w
Load from /workspace/~w. Print clear results.
Store the code in AnalysisCode.", [DataInfo, Question, CsvPath]),
    task(AnalysisTask, AnalysisCode),
    
    exec(vm_exec(command: "cat > /tmp/analysis.py << 'EOF'\n" ++ AnalysisCode ++ "\nEOF"), _),
    exec(vm_exec(command: "python3 /tmp/analysis.py"), AnalysisResultDict),
    get_dict(stdout, AnalysisResultDict, AnalysisResult),
    
    output("Generating insights..."),
    format(string(InsightTask), "Explain these analysis results in plain language:
~w

Provide clear insights and actionable recommendations.", [AnalysisResult]),
    task(InsightTask),
    
    answer("Analysis complete!").
```

### Example 5: Email Drafter

**Use Case:** Draft professional emails

```prolog
% email_drafter.dml
% Professional email drafting agent

agent_main(Purpose, Recipient, Tone) :-
    system("You are an expert business communicator.
            Write clear, professional emails."),
    
    % Map tone to style guidelines
    tone_guidelines(Tone, Guidelines),
    
    format(string(DraftTask), "Draft an email with:
          - Purpose: ~w
          - Recipient: ~w
          - Tone guidelines: ~w
          
          Include appropriate greeting and sign-off.
          Store the complete email in Email.", [Purpose, Recipient, Guidelines]),
    task(DraftTask, Email),
    
    output("Draft email:"),
    output(Email),
    
    task("Review this email for:
          1. Clarity and conciseness
          2. Appropriate tone
          3. Grammar and spelling
          Suggest any improvements. If the email is good, say 'APPROVED'.
          Store assessment in Assessment.", Assessment),
    
    (   sub_string(Assessment, _, _, _, "APPROVED")
    ->  answer(Email)
    ;   output("Revising based on feedback..."),
        format(string(ReviseTask), "Revise this email based on feedback: ~w
              Original: ~w
              Store revised email in Revised.", [Assessment, Email]),
        task(ReviseTask, Revised),
        answer(Revised)
    ).

tone_guidelines(formal, "Use formal language, avoid contractions, be respectful and professional").
tone_guidelines(friendly, "Be warm and personable while remaining professional").
tone_guidelines(urgent, "Convey urgency clearly, be direct, use action-oriented language").
tone_guidelines(apologetic, "Express sincere apology, take responsibility, offer solutions").
```

### Example 6: Documentation Generator

**Use Case:** Generate documentation from code

```prolog
% doc_generator.dml
% Code documentation generator

agent_main(SourceDir) :-
    system("You are a technical writer specializing in API documentation.
            Write clear, comprehensive documentation with examples."),
    
    output("Scanning source files..."),
    directory_files(SourceDir, Files),
    include(is_source_file, Files, SourceFiles),
    
    output("Analyzing code structure..."),
    analyze_files(SourceFiles, Analyses),
    
    output("Generating documentation..."),
    format(string(DocTask), "Based on these code analyses:
~w

Generate comprehensive API documentation with:
1. Overview and getting started
2. API Reference (all public functions/classes)
3. Usage examples
4. Common patterns

Format as markdown. Store in Documentation.", [Analyses]),
    task(DocTask, Documentation),
    
    write_file("API_DOCUMENTATION.md", Documentation),
    answer("Documentation generated: API_DOCUMENTATION.md").

is_source_file(File) :-
    (   sub_string(File, _, _, 0, ".ts")
    ;   sub_string(File, _, _, 0, ".js")
    ;   sub_string(File, _, _, 0, ".py")
    ).

analyze_files([], []).
analyze_files([File|Rest], [Analysis|Analyses]) :-
    read_file(File, Content),
    format(string(AnalysisTask), "Analyze this source file and extract:
          - Public functions/classes with signatures
          - Purpose of each
          - Parameters and return types
          File: ~w
          Content: ~w
          Store analysis in Analysis.", [File, Content]),
    task(AnalysisTask, Analysis),
    analyze_files(Rest, Analyses).
```

### Example 7: Test Generator

**Use Case:** Generate unit tests from code

```prolog
% test_generator.dml
% Unit test generator

agent_main(SourceFile, Framework) :-
    system("You are a QA engineer specializing in comprehensive test coverage.
            Write tests that cover edge cases and error conditions."),
    
    read_file(SourceFile, Code),
    
    output("Analyzing code for testable units..."),
    format(string(AnalyzeTask), "Analyze this code and identify all testable functions/methods:
~w

For each, list:
- Function name
- Parameters
- Expected behavior
- Edge cases to test
Store in TestPlan.", [Code]),
    task(AnalyzeTask, TestPlan),
    
    output("Generating tests..."),
    format(string(GenTask), "Generate comprehensive unit tests using ~w framework.
          Test plan: ~w
          Original code: ~w
          
          Include:
          - Happy path tests
          - Edge case tests  
          - Error handling tests
          - Mock setup where needed
          
          Store complete test file in Tests.", [Framework, TestPlan, Code]),
    task(GenTask, Tests),
    
    format(string(TestFile), "~w.test", [SourceFile]),
    write_file(TestFile, Tests),
    
    format(string(Msg), "Generated tests: ~w", [TestFile]),
    answer(Msg).
```

### Example 8: Translation Agent with Quality Check

**Use Case:** High-quality translation with verification

```prolog
% translator.dml
% Translation agent with quality verification

agent_main(Text, TargetLanguage) :-
    system("You are a professional translator with expertise in nuanced, 
            culturally-appropriate translations."),
    
    % Initial translation
    output("Translating..."),
    format(string(TranslateTask), "Translate this text to ~w:
          
~w

Preserve meaning, tone, and cultural context.
Store translation in Translation.", [TargetLanguage, Text]),
    task(TranslateTask, Translation),
    
    % Back-translation for quality check
    output("Verifying quality..."),
    task("Translate this back to English to verify accuracy:
          {Translation}
          Store in BackTranslation.", BackTranslation),
    
    % Compare and refine if needed
    format(string(CompareTask), "Compare original and back-translation:
          Original: ~w
          Back-translation: ~w
          
          Rate accuracy 1-10 and note any meaning loss.
          Store rating in Rating and issues in Issues.", [Text, BackTranslation]),
    task(CompareTask, Rating, Issues),
    
    (   Rating >= 8
    ->  answer(Translation)
    ;   output("Refining translation..."),
        format(string(RefineTask), "Refine this translation addressing these issues: ~w
              Original: ~w
              Current translation: ~w
              Store improved version in Refined.", [Issues, Text, Translation]),
        task(RefineTask, Refined),
        answer(Refined)
    ).
```

### Example 9: Interview Prep Agent

**Use Case:** Prepare for job interviews

```prolog
% interview_prep.dml
% Job interview preparation agent

tool(ask_question(Question, Response), "Ask user a practice question") :-
    wait_for_input(Question, Response).

agent_main(JobTitle, Company) :-
    system("You are an experienced career coach and interview expert."),
    
    % Research the role and company
    output("Researching role and company..."),
    format(string(ResearchTask), "Research ~w roles at ~w. Identify:
          1. Typical responsibilities
          2. Required skills
          3. Company culture and values
          4. Common interview format
          Store findings in Research.", [JobTitle, Company]),
    task(ResearchTask, Research),
    
    % Generate practice questions
    output("Generating practice questions..."),
    format(string(QuestionsTask), "Based on this research: ~w
          
          Generate 5 likely interview questions, mixing:
          - Behavioral questions
          - Technical questions  
          - Situational questions
          Store as a Prolog list in Questions.", [Research]),
    task(QuestionsTask, Questions),
    
    % Practice session
    output("Starting practice session..."),
    practice_questions(Questions, Feedback),
    
    % Summary and tips
    format(string(SummaryTask), "Summarize the practice session:
          Feedback given: ~w
          
          Provide:
          1. Strengths demonstrated
          2. Areas to improve
          3. Final tips for the interview", [Feedback]),
    task(SummaryTask),
    
    answer("Good luck with your interview!").

practice_questions([], []).
practice_questions([Q|Qs], [F|Fs]) :-
    format(string(FormattedQ), "~w\n\n(Answer as you would in a real interview)", [Q]),
    tool(ask_question(FormattedQ, Answer)),
    format(string(FeedbackTask), "Evaluate this interview answer:
          Question: ~w
          Answer: ~w
          
          Provide constructive feedback on:
          - Content and relevance
          - Structure (STAR method if applicable)
          - Areas for improvement
          Store feedback in Feedback.", [Q, Answer]),
    task(FeedbackTask, F),
    output(F),
    practice_questions(Qs, Fs).
```

### Example 10: Multi-Source Research Agent

**Use Case:** Deep research combining multiple sources

```prolog
% deep_research.dml
% Multi-source research agent

tool(search_web(Query, Results), "Search the web") :-
    exec(web_search(query: Query, count: 10), Results).

tool(search_academic(Query, Results), "Search academic papers") :-
    exec(academic_search(query: Query), Results).

tool(fetch_page(Url, Content), "Fetch page content") :-
    exec(fetch_url(url: Url), Content).

agent_main(Topic, Depth) :-
    system("You are a thorough research analyst. Cite sources, distinguish facts 
            from opinions, and synthesize information from multiple perspectives."),
    
    output("Phase 1: Web research..."),
    web_research(Topic, WebFindings),
    
    output("Phase 2: Academic research..."),
    academic_research(Topic, AcademicFindings),
    
    output("Phase 3: Deep dive on key sources..."),
    deep_dive(Topic, Depth, DeepFindings),
    
    output("Phase 4: Synthesizing findings..."),
    format(string(SynthTask), "Synthesize these research findings:
          
          Web Sources: ~w
          Academic Sources: ~w
          Deep Dive: ~w
          
          Create a comprehensive research report with:
          1. Executive Summary
          2. Background and Context
          3. Key Findings (with citations)
          4. Multiple Perspectives
          5. Gaps in Current Knowledge
          6. Conclusions and Recommendations
          7. References
          
          Store in Report.", [WebFindings, AcademicFindings, DeepFindings]),
    task(SynthTask, Report),
    
    format(string(Filename), "~w_research_report.md", [Topic]),
    write_file(Filename, Report),
    
    answer(Report).

web_research(Topic, Findings) :-
    format(string(Query), "~w latest developments", [Topic]),
    tool(search_web(Query, Results)),
    format(string(Task), "Summarize key findings from: ~w. Note any contradictions or debates. Store in Findings.", [Results]),
    task(Task, Findings).

academic_research(Topic, Findings) :-
    (   tool(search_academic(Topic, Results)),
        Results \= []
    ->  format(string(Task), "Summarize academic findings: ~w. Note methodology and limitations. Store in Findings.", [Results]),
        task(Task, Findings)
    ;   Findings = "No academic sources found"
    ).

deep_dive(_, shallow, "Skipped deep dive for shallow research").
deep_dive(Topic, standard, Findings) :-
    deep_dive_sources(Topic, 3, Findings).
deep_dive(Topic, deep, Findings) :-
    deep_dive_sources(Topic, 6, Findings).

deep_dive_sources(Topic, Count, Findings) :-
    format(string(Query), "~w comprehensive guide", [Topic]),
    tool(search_web(Query, Results)),
    format(string(SelectTask), "Select the ~w most authoritative URLs from: ~w. Store as list in Urls.", [Count, Results]),
    task(SelectTask, Urls),
    fetch_and_analyze(Urls, Findings).

fetch_and_analyze([], "").
fetch_and_analyze([Url|Urls], Findings) :-
    (   tool(fetch_page(Url, Content))
    ->  format(string(Task), "Extract key information from: ~w. Store in Info.", [Content]),
        task(Task, Info)
    ;   Info = ""
    ),
    fetch_and_analyze(Urls, RestFindings),
    format(string(Findings), "~w\n\n~w", [Info, RestFindings]).
```

---

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        DeepClause CLI                           │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    Command Parser                          │  │
│  │  - Argument parsing (commander.js)                        │  │
│  │  - Configuration loading                                  │  │
│  │  - Command routing                                        │  │
│  └───────────────────────────────────────────────────────────┘  │
│                              │                                   │
│  ┌───────────────────────────┼───────────────────────────────┐  │
│  │                    Core Services                           │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐   │  │
│  │  │  Compiler   │  │   Runner    │  │  MCP Manager    │   │  │
│  │  │             │  │             │  │                 │   │  │
│  │  │ - MD parse  │  │ - DML exec  │  │ - Server mgmt   │   │  │
│  │  │ - Prompt    │  │ - Event     │  │ - Tool registry │   │  │
│  │  │ - Validate  │  │   handling  │  │ - Protocol      │   │  │
│  │  └─────────────┘  └─────────────┘  └─────────────────┘   │  │
│  └───────────────────────────────────────────────────────────┘  │
│                              │                                   │
│  ┌───────────────────────────┼───────────────────────────────┐  │
│  │                  DeepClause SDK                            │  │
│  │  - Prolog WASM runtime                                    │  │
│  │  - Meta-interpreter                                       │  │
│  │  - Memory management                                      │  │
│  │  - Vercel AI SDK integration                              │  │
│  └───────────────────────────────────────────────────────────┘  │
│                              │                                   │
│  ┌───────────────────────────┼───────────────────────────────┐  │
│  │                    TUI Layer                               │  │
│  │  - Ink (React for CLI)                                    │  │
│  │  - Progress indicators                                    │  │
│  │  - Styled output                                          │  │
│  │  - Interactive prompts                                    │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Technology Stack

| Component | Technology |
|-----------|------------|
| Runtime | Node.js 20+ |
| Language | TypeScript 5.x |
| CLI Framework | Commander.js |
| TUI | Ink (React for CLI) |
| LLM Integration | Vercel AI SDK |
| MCP Client | @modelcontextprotocol/sdk |
| Prolog | SWI-Prolog WASM |
| Schema Validation | Zod |

### File Structure

```
deepclause-cli/
├── package.json
├── tsconfig.json
├── src/
│   ├── index.ts              # CLI entry point
│   ├── commands/
│   │   ├── configure.ts      # Configuration command
│   │   ├── compile.ts        # Compilation command
│   │   ├── run.ts            # Run command
│   │   ├── list-tools.ts     # List tools command
│   │   └── list-commands.ts  # List commands command
│   ├── compiler/
│   │   ├── markdown-parser.ts
│   │   ├── dml-generator.ts
│   │   ├── validator.ts
│   │   └── prompts/
│   │       └── md2dml.prompt.ts  # Default conversion prompt
│   ├── runner/
│   │   ├── executor.ts
│   │   └── event-handler.ts
│   ├── mcp/
│   │   ├── manager.ts
│   │   ├── client.ts
│   │   └── tool-registry.ts
│   ├── config/
│   │   ├── loader.ts
│   │   └── schema.ts
│   ├── tui/
│   │   ├── App.tsx
│   │   ├── components/
│   │   │   ├── Header.tsx
│   │   │   ├── Progress.tsx
│   │   │   ├── ToolCall.tsx
│   │   │   ├── Output.tsx
│   │   │   └── Error.tsx
│   │   └── themes/
│   │       └── default.ts
│   └── utils/
│       ├── logger.ts
│       └── hash.ts
├── prompts/
│   └── md2dml.prompt         # Default prompt (can be overridden)
└── tests/
    ├── compiler.test.ts
    ├── runner.test.ts
    └── mcp.test.ts
```

---

## TUI Presentation

### Design Philosophy

The TUI should be:
- **Informative**: Show what's happening at each step
- **Beautiful**: Use colors, borders, and icons appropriately
- **Responsive**: Handle terminal resize gracefully
- **Non-intrusive**: In headless mode, produce clean, pipeable output

### Visual Elements

```
┌─────────────────────────────────────────────────────────────────┐
│  🔷 DeepClause                                     gpt-4o      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Running: tools/competitor_analysis                             │
│  Args: "Acme Corp"                                             │
│                                                                 │
│  ┌─ Progress ────────────────────────────────────────────────┐ │
│  │ ✓ Identifying company and industry                        │ │
│  │ ✓ Finding competitors (5 found)                           │ │
│  │ ● Analyzing competitors... (3/5)                          │ │
│  │ ○ Generating report                                       │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─ Tool Call ───────────────────────────────────────────────┐ │
│  │ 🔧 web_search                                             │ │
│  │    query: "TechCorp company overview products"            │ │
│  │    ─────────────────────────────────────────────          │ │
│  │    ✓ 10 results returned                                  │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                 │
│  ┌─ Output ──────────────────────────────────────────────────┐ │
│  │ Analyzing TechCorp...                                     │ │
│  │ Found key products: CloudSuite, DataEngine, AIAssist      │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  Tokens: 12,450 in / 3,200 out    Time: 45s    Cost: $0.12   │
└─────────────────────────────────────────────────────────────────┘
```

### Headless Mode Output

```
[info] Running: tools/competitor_analysis
[info] Args: "Acme Corp"
[progress] Identifying company and industry
[progress] Finding competitors
[tool] web_search: 10 results
[output] Analyzing TechCorp...
[output] Found key products: CloudSuite, DataEngine, AIAssist
[progress] Generating report
[answer] Analysis complete! Report saved to Acme_Corp_competitor_analysis.md
```

### Inspiration

- [Gemini CLI](https://github.com/google-gemini/gemini-cli) - Clean design, good progress indication
- [Vercel CLI](https://vercel.com/cli) - Minimal, beautiful output
- [GitHub CLI](https://cli.github.com/) - Consistent styling, good UX

---

## Appendix A: Default md2dml.prompt Location

The default prompt is embedded in the CLI but can be overridden by placing a file at:

```
.deepclause/md2dml.prompt
```

The CLI checks for this file first before using the built-in prompt.

## Appendix B: AgentVM (Built-in)

AgentVM is bundled with the DeepClause CLI and provides secure sandboxed code execution. It is automatically available without any configuration.

### Overview

AgentVM is a lightweight WASM-based Linux virtual machine (Alpine Linux with Python 3.12) that runs in a worker thread. It allows you to execute shell commands and capture their output, making it an ideal sandbox for AI agents.

**Features:**
- **Full Linux VM**: Runs Alpine Linux with Python 3.12 in a WASM-based emulator
- **Networking**: Built-in DHCP, DNS, and TCP/UDP NAT for internet access (when enabled)
- **Shared Filesystem**: Workspace directory mounted at `/workspace` in both DML and AgentVM
- **Command Execution**: Execute shell commands and capture stdout/stderr
- **Worker Thread**: Runs in a separate thread to avoid blocking

### Built-in Tool

| Tool | Description |
|------|-------------|
| `vm_exec(command)` | Execute any shell command in the sandboxed Alpine Linux VM |

### Shared Workspace

The workspace directory is mounted in both the SWI-Prolog WASM environment and AgentVM at the same path. This means:

- Files written by DML are immediately accessible to Python/shell code in the VM
- Files created by VM code are immediately readable from DML
- No path translation needed - same paths work everywhere

### Tool Response Format

The `vm_exec` tool returns a dict with:
```prolog
{
  stdout: "command output",
  stderr: "error output if any",
  exitCode: 0
}
```

Access fields using `get_dict/3`:
```prolog
exec(vm_exec(command: "echo hello"), Result),
get_dict(stdout, Result, Output)
```

### Example Usage in DML

#### Simple Python Execution
```prolog
agent_main(Task) :-
    system("You are a coding assistant. Write Python to solve problems."),
    task("Write Python code to solve: {Task}. Store in Code.", Code),
    format(string(Cmd), "python3 -c '~w'", [Code]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    format(string(Msg), "Output: ~w", [Output]),
    answer(Msg).
```

#### Shell Commands with Networking
```prolog
agent_main(Url) :-
    system("You are a web scraping assistant."),
    format(string(Cmd), "wget -q -O- ~w | head -100", [Url]),
    exec(vm_exec(command: Cmd), Result),
    task("Analyze this HTML content: {Result}. Summarize what you find."),
    answer("Analysis complete!").
```

#### Data Processing Pipeline
```prolog
agent_main(CsvPath, Question) :-
    system("You are a data analyst."),
    
    % Generate and run analysis code (VM sees same file at same path)
    format(string(AnalysisTask), "Write Python code using pandas to analyze ~w and answer: ~w. Store in Code.", [CsvPath, Question]),
    task(AnalysisTask, Code),
    
    % Write code to file and execute
    format(string(RunCmd), "python3 -c '~w'", [Code]),
    exec(vm_exec(command: RunCmd), Result),
    get_dict(stdout, Result, Output),
    
    % Explain results
    format(string(ExplainTask), "Explain these results in plain language: ~w", [Output]),
    task(ExplainTask),
    answer("Analysis complete!").
```

#### Install Packages and Run Complex Code
```prolog
agent_main(Task) :-
    system("You are an expert Python developer."),
    
    % Install required packages
    exec(vm_exec(command: "pip install requests beautifulsoup4"), _),
    
    % Generate code
    task("Write Python code to: {Task}. Use requests and beautifulsoup4 if needed. Store in Code.", Code),
    
    % Save code to file and execute (better for multi-line code)
    exec(vm_exec(command: "cat > /tmp/script.py << 'EOF'\n" ++ Code ++ "\nEOF"), _),
    exec(vm_exec(command: "python3 /tmp/script.py"), Result),
    get_dict(stdout, Result, Output),
    
    format(string(Msg), "Result: ~w", [Output]),
    answer(Msg).
```

### VM Lifecycle

The AgentVM is automatically managed by the CLI:
- **Started**: When a DML program first calls an AgentVM tool
- **Reused**: The same VM instance is reused for all subsequent calls in the same run
- **Stopped**: When the DML program completes

### Workspace Example

File paths work the same in DML predicates and VM code:

```bash
deepclause run tools/analysis --workspace ./my-project
```

```prolog
% Same paths work in DML and VM
agent_main :-
    % Read with VM shell command
    exec(vm_exec(command: "cat /workspace/data.csv"), Data),
    get_dict(stdout, Data, CsvContents),
    
    % Python can access the same files directly
    exec(vm_exec(command: "python3 -c \"import pandas; df = pandas.read_csv('/workspace/data.csv'); print(df.head())\""), Result),
    get_dict(stdout, Result, Output),
    
    % Write from Python
    exec(vm_exec(command: "python3 -c \"open('/workspace/output.txt', 'w').write('hello')\""), _),
    
    % Read back with shell
    exec(vm_exec(command: "cat /workspace/output.txt"), ReadBack),
    get_dict(stdout, ReadBack, FileContents),
    
    answer("Done!").
```

Documentation: https://github.com/deepclause/agentvm

## Appendix C: Supported LLM Providers

| Provider | Models | API Key Variable |
|----------|--------|------------------|
| OpenAI | gpt-4o, gpt-4-turbo, gpt-3.5-turbo | `OPENAI_API_KEY` |
| Anthropic | claude-3.5-sonnet, claude-3-opus | `ANTHROPIC_API_KEY` |
| Google | gemini-2.0-flash, gemini-2.5-flash, gemini-pro | `GOOGLE_GENERATIVE_AI_API_KEY` |
| OpenRouter | Various | `OPENROUTER_API_KEY` |