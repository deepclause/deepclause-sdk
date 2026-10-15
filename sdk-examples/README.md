# DeepClause SDK Examples

This directory contains examples demonstrating how to use the DeepClause SDK.

## Prerequisites

1. Build the SDK:
   ```bash
   npm run build
   ```

2. Set up API keys (environment variables):
   ```bash
  export GOOGLE_GENERATIVE_AI_API_KEY="..."
  # optional, for deep-research.ts live web search
  export BRAVE_API_KEY="..."
   ```

For a no-key validation path, start with:

```bash
npx tsx sdk-examples/no-api-key.ts
```

## Quick Start

The simplest example to get started:

```bash
npm run example
# or
npx tsx sdk-examples/quick-start.ts
```

To exercise the currently supported no-key paths end-to-end:

```bash
npm run examples:smoke
```

## Available Examples

### 1. Quick Start (`quick-start.ts`)

Minimal example showing core functionality:
- Creating SDK instance
- Running DML code
- Handling events

### 2. Basic Usage (`basic-usage.ts`)

Comprehensive examples covering all SDK features:

| Example | Description |
|---------|-------------|
| 1. Simple Task | Basic `task()` execution |
| 2. Task with Variables | Binding output variables |
| 3. Custom Tools | Registering and using tools |
| 4. Tool Policy | Whitelist/blacklist security |
| 5. User Input | Handling interactive input |
| 6. Parameters | Passing parameters to DML |
| 7. Sub-agents | Delegating to sub-agents |
| 8. Streaming | Using `yield()` for output |
| 9. Cancellation | AbortController support |
| 10. Research Agent | Complete workflow example |

Run specific example:
```bash
npx tsx sdk-examples/basic-usage.ts 3
```

### 3. Neurosymbolic (`neurosymbolic.ts`)

Advanced examples showcasing Prolog + LLM integration:

| Example | Description |
|---------|-------------|
| 1. Product Recommendation | Constraint filtering + LLM generation |
| 2. Puzzle Solver | CLP(FD) solving + LLM explanation |
| 3. Backtracking Reasoning | Prolog backtracking for fault tolerance |

```bash
npx tsx sdk-examples/neurosymbolic.ts 1
```

### 4. No-Key Example (`no-api-key.ts`)

Runs without provider credentials and demonstrates:
- Tool registration
- Tool whitelist and blacklist policies
- A Prolog-only DML run with no model call

```bash
npx tsx sdk-examples/no-api-key.ts
```

### 5. Deep Research (`deep-research.ts`)

Longer interactive research workflow with optional Brave Search integration:

```bash
npx tsx sdk-examples/deep-research.ts "test topic"
```

Without `GOOGLE_GENERATIVE_AI_API_KEY`, this example now skips cleanly. Without `BRAVE_API_KEY`, it falls back to mock search results.

### 6. Subagents (`subagents.ts`, `subagents.dml`)

The `.dml` file is a tour of six concurrency patterns, from a fan-out to
agents that route work between processes. Patterns 1–5 call `task/N` (a model);
pattern 6 is pure Prolog and is best run with the default `--execution worker`
to spread it over cores.

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
/goal survey([quantum error correction, riscv], B)
/goal start_glossary, define_terms([subagent, mailbox], Defs)
/goal ops_start, ask_frontdesk("Where is my order #4471?", Reply)
/goal queens_portfolio(Rows)
```

The `narrate`/`keeper`/`asker` predicates at the bottom show the three process
lifecycle shapes — one-shot, message-waiting, and interactive — and need no
model:

```bash
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

### 7. Two agents talking to each other (`debate.ts`, `debate.dml`)

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
```

```
/goal debate("Nuclear power should expand to meet climate targets", 2, Verdict).
```

Each debate turn is shown as `⚙ debater  PRO (round 2): …` in the transcript.

## DML Language Quick Reference

### Core Predicates

```prolog
% Run an LLM agent loop
task("Description").
task("Description with Variable binding", Variable).

% External tool execution
exec(tool_name(args), Result).

% Memory management
system("System prompt").      % Add system message
user("User message").         % Add user message
push_context.                 % Save memory state
pop_context.                  % Restore memory state

% Output
answer("Final answer").       % Send final answer
yield("Streaming output").    % Stream intermediate output
log("Debug message").         % Log message
```

### Tool Definition

```prolog
% Define a tool available to the task agent
tool(my_tool(Arg1, Result)) :-
    exec(external_tool(Arg1), Result).
```

### Entry Point

```prolog
agent_main :-
    % Your code here
    answer("Done!").

% With parameters
agent_main(Param1, Param2) :-
    % Use Param1, Param2
    answer("Done!").
```

## Creating Your Own Example

```typescript
import { createDeepClause } from '../src/index.js';
import { z } from 'zod';

async function myExample() {
  const dc = await createDeepClause({
    model: 'gpt-4o',
    apiKey: process.env.OPENAI_API_KEY,
  });

  // Register tools
  dc.registerTool('my_tool', {
    description: 'What this tool does',
    parameters: z.object({
      input: z.string(),
    }),
    execute: async (args) => {
      // Tool implementation
      return { result: 'success' };
    },
  });

  // Define DML
  const code = `
    tool(my_tool_wrapper(Input, Output)) :-
        exec(my_tool(Input), Output).

    agent_main :-
        system("You are a helpful assistant."),
        task("Do something with my_tool"),
        answer("Done!").
  `;

  // Run and handle events
  try {
    for await (const event of dc.runDML(code)) {
      console.log(event.type, event.content);
    }
  } finally {
    await dc.dispose();
  }
}

myExample().catch(console.error);
```

## Tips

1. **Tool Policy**: Always set a tool policy in production to control what tools the LLM can access.

2. **Error Handling**: Use Prolog's `(Goal -> Then ; Else)` construct for graceful error handling.

3. **Backtracking**: Use `push_context`/`pop_context` to make memory backtrackable.

4. **Streaming**: Use `yield()` to stream intermediate results for better UX.

5. **Cancellation**: Pass an `AbortController.signal` to enable user cancellation.
