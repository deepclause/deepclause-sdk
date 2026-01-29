# DeepClause SDK Examples

This directory contains examples demonstrating how to use the DeepClause SDK.

## Prerequisites

1. Build the SDK:
   ```bash
   npm run build
   ```

2. Set up API keys (environment variables):
   ```bash
   export OPENAI_API_KEY="sk-..."
   # or
   export ANTHROPIC_API_KEY="sk-ant-..."
   ```

## Quick Start

The simplest example to get started:

```bash
npm run example
# or
npx ts-node --esm examples/quick-start.ts
```

## Available Examples

### 1. Quick Start (`quick-start.ts`)

Minimal example showing core functionality:
- Creating SDK instance
- Registering a custom tool
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
npx ts-node --esm examples/basic-usage.ts 3
```

### 3. Neurosymbolic (`neurosymbolic.ts`)

Advanced examples showcasing Prolog + LLM integration:

| Example | Description |
|---------|-------------|
| 1. Product Recommendation | Constraint filtering + LLM generation |
| 2. Puzzle Solver | CLP(FD) solving + LLM explanation |
| 3. Backtracking Reasoning | Prolog backtracking for fault tolerance |

```bash
npx ts-node --esm examples/neurosymbolic.ts 1
```

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
