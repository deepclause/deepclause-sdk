# DeepClause SDK

A neurosymbolic AI system combining **Prolog-based symbolic reasoning** with **LLM-powered task execution**.

Write declarative logic programs that seamlessly integrate with large language models.

## Features

- 🧠 **Neurosymbolic AI** - Combine symbolic reasoning with neural language models
- 🔄 **Backtrackable Execution** - Full Prolog backtracking works across LLM calls
- 🛠️ **Custom Tools** - Register TypeScript/JavaScript tools callable from DML
- 🎯 **Multiple LLM Providers** - OpenAI, Anthropic, Google, OpenRouter
- 📝 **Simplified DML** - Clean, readable syntax built on Prolog

## Installation

```bash
npm install deepclause-sdk
```

## Quick Start

```typescript
import { createDeepClause } from 'deepclause-sdk';

// Create SDK instance
const dc = await createDeepClause({
  model: 'gpt-4o',
  apiKey: process.env.OPENAI_API_KEY,
});

// Define DML code
const code = `
  agent_main :-
      system("You are a helpful assistant."),
      task("Explain what Prolog is in one sentence."),
      answer("Done!").
`;

// Run and handle events
for await (const event of dc.runDML(code)) {
  if (event.type === 'answer') {
    console.log('Answer:', event.content);
  }
}

await dc.dispose();
```

## Core Concepts

### DML (DeepClause Meta Language)

DML is a simplified Prolog dialect designed for AI agent programming. Every DML program needs an `agent_main` entry point:

```prolog
agent_main :-
    system("You are a research assistant."),
    task("Find information about quantum computing."),
    answer("Research complete!").
```

### Tasks

The `task/1` predicate invokes an LLM to perform work:

```prolog
agent_main :-
    task("Summarize the key points of machine learning."),
    answer("Done").
```

Use `task/2` to capture the LLM's response in a variable:

```prolog
agent_main :-
    task("List 3 programming languages. Store them in Languages.", Languages),
    format(string(Msg), "Found: ~w", [Languages]),
    answer(Msg).
```

### Custom Tools

Register tools in TypeScript:

```typescript
import { z } from 'zod';

dc.registerTool('calculator', {
  description: 'Perform mathematical calculations',
  parameters: z.object({
    expression: z.string().describe('Math expression to evaluate'),
  }),
  execute: async ({ expression }) => {
    return { result: eval(expression) };
  },
});
```

**Important:** Registered tools are NOT automatically available to the LLM during `task()`. This is by design - it gives you full control over the agent's capabilities.

To expose a tool to the LLM, define it in DML using the `tool/2` clause form:

```prolog
% Define a tool the LLM can call (last argument is output)
tool(calculate(Expression, Result), "Calculate a math expression") :-
    exec(calculator(expression: Expression), Result).

agent_main :-
    system("You have a calculate tool available."),
    task("What is 15 * 23?"),
    answer("Done!").
```

Or call registered tools directly with `exec/2` for deterministic execution:

```prolog
agent_main :-
    exec(calculator(expression: "15 * 23"), Result),
    format(string(Msg), "Result: ~w", [Result]),
    answer(Msg).
```

This architecture lets you:
- Control exactly which tools the LLM can use
- Add Prolog logic around tool calls
- Keep sensitive tools hidden from the LLM

### Memory & Context Management

Build conversation context with `system/1` and `user/1`:

```prolog
agent_main :-
    system("You are a cooking assistant."),
    user("I want to make pasta."),
    task("Suggest a simple pasta recipe."),
    answer("Recipe provided!").
```

Use `push_context` and `pop_context` for sub-task isolation:

```prolog
agent_main :-
    system("You are a research assistant."),
    task("Find info about topic A."),
    push_context(clear),  % Save memory to stack, clear for fresh task
    system("You are an analyst."),  % New context for isolated task
    task("Analyze topic B independently."),
    pop_context,  % Restore original memory (topic A context)
    task("Summarize topic A findings."),
    answer("Done!").
```

Note: `push_context(clear)` saves the current memory and clears it completely. Use `push_context` (without `clear`) to save memory while keeping it active.

Use `prompt/N` for one-off LLM calls that don't affect memory:

```prolog
agent_main :-
    prompt("Generate a creative title.", Title),
    format(string(Msg), "Title: ~w", [Title]),
    answer(Msg).
```

### File I/O

DML can read and write files in the workspace directory:

```prolog
agent_main :-
    write_file("output.txt", "Hello from DML!"),
    read_file("output.txt", Content),
    answer(Content).
```

Pass `workspacePath` when running DML:

```typescript
for await (const event of dc.runDML(code, { workspacePath: './my-workspace' })) {
  console.log(event);
}
```

Available file operations:
- `read_file(File, Content)` - Read file contents
- `write_file(File, Content)` - Write to file
- `append_file(File, Content)` - Append to file
- `exists_file(File)` - Check if file exists
- `exists_directory(Dir)` - Check if directory exists
- `directory_files(Dir, Files)` - List directory contents
- `make_directory(Dir)` - Create directory

### Backtracking

DML supports full Prolog backtracking - even across LLM calls:

```prolog
agent_main :-
    output("Trying approach 1"),
    task("Solve this impossible problem."),
    fail.  % Force backtracking

agent_main :-
    output("Trying approach 2"),
    task("Solve this easier problem."),
    answer("Solved with approach 2!").
```

## Supported Models

| Provider | Models |
|----------|--------|
| OpenAI | `gpt-4o`, `gpt-4o-mini`, `gpt-4-turbo`, `o1`, `o1-mini` |
| Anthropic | `claude-3-5-sonnet-20240620`, `claude-3-opus-20240229` |
| Google | `gemini-2.0-flash`, `gemini-1.5-pro` |
| OpenRouter | Any model via `openrouter/provider/model` |

## API Reference

### `createDeepClause(options)`

Create an SDK instance.

```typescript
const dc = await createDeepClause({
  model: 'gpt-4o',           // Required: LLM model name
  apiKey: '...',             // API key (uses env var if not provided)
  provider: 'openai',        // Optional: auto-detected from model
  temperature: 0.7,          // Optional: 0-1
  maxTokens: 4096,           // Optional: max response tokens
});
```

### `dc.runDML(code, options)`

Execute DML code. Returns an async generator of events.

```typescript
for await (const event of dc.runDML(code, {
  params: { name: 'Alice' },  // Parameters accessible in DML
  workspacePath: './workspace',
  onUserInput: async (prompt) => readline(prompt),
})) {
  console.log(event.type, event.content);
}
```

**Event Types:**
- `output` - Output from `output/1` or `yield/1`
- `log` - Debug logs from `log/1`
- `answer` - Final answer from `answer/1`
- `input_required` - User input requested via `wait_for_input/2`
- `error` - Runtime errors
- `finished` - Execution complete

### `dc.registerTool(name, definition)`

Register a custom tool.

```typescript
dc.registerTool('fetch_url', {
  description: 'Fetch content from a URL',
  parameters: z.object({
    url: z.string().url(),
  }),
  execute: async ({ url }) => {
    const response = await fetch(url);
    return { content: await response.text() };
  },
});
```

### `dc.dispose()`

Clean up resources. Always call when done.

## Examples

See the [examples](./examples) directory:

- `quick-start.ts` - Minimal example
- `basic-usage.ts` - Comprehensive examples
- `neurosymbolic.ts` - Advanced symbolic reasoning
- `deep-research.ts` - Multi-step research agent

Run examples:

```bash
# Set your API key
export OPENAI_API_KEY="sk-..."

# Run quick start
npm run example

# Run all examples
npm run examples
```

## License

MIT
