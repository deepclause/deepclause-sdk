# DeepClause: Guide for AI Agents

DeepClause is a neuro-symbolic framework that allows you to build reliable agents by compiling Markdown specifications into executable Prolog-based DML (DeepClause Meta Language) programs.

As an AI coding agent, you can use DeepClause to:
1. **Automate complex workflows** with guaranteed execution semantics (backtracking, recursion).
2. **Implement hard logic/math constraints** using Prolog's CLP libraries.
3. **Isolate context** between sub-tasks to prevent "context-window bias".

---

## 1. Using the CLI Tool

The `deepclause` CLI is your primary interface for creating and testing skills.

### Compilation
Convert a Markdown specification into a logic program:
```bash
deepclause compile tasks/my-task.md
```
This generates `.deepclause/tools/my-task.dml` and a metadata file.

### Execution
Run a compiled skill:
```bash
deepclause run .deepclause/tools/my-task "Positional Arg" --param key=value
```

### One-Shot Mode (Experimental)
Generate and run a DML program on the fly from a prompt:
```bash
deepclause run --prompt "Search for latest news on AI and summarize them" --verbose
```

---

## 2. DML Reference for Agents

DML combines LLM "vibes" with Prolog "hard logic".

### Core Predicates
- `task(Description, ...Vars)`: LLM call with current context. Supports type wrappers: `integer(X)`, `boolean(Y)`, `list(string(Z))`.
- `prompt(Description, ...Vars)`: LLM call with **fresh context** (empty memory).
- `exec(Tool(Args), Result)`: Direct call to external tools (AgentVM, Search, MCP).
- `system(Text)` / `user(Text)`: Manage backtrackable conversation memory.
- `answer(Text)`: Terminate and commit the final result.

### Solving Logic/Math (CLP)
Avoid Python for math constraints. Use Constraint Logic Programming:
```prolog
:- use_module(library(clpfd)). % Integers
:- use_module(library(clpq)).  % Rational numbers
:- use_module(library(clpr)).  % Real numbers
```

---

## 3. Using as a Library (SDK)

To integrate DeepClause into a TypeScript application:

```typescript
import { createDeepClause } from 'deepclause-sdk';

const dc = await createDeepClause({
  model: 'gpt-4o',
  apiKey: process.env.OPENAI_API_KEY,
});

// Run DML code
for await (const event of dc.runDML(dmlCode, { params: { topic: 'AI' } })) {
  if (event.type === 'answer') console.log('Result:', event.content);
}

await dc.dispose();
```

---

## 4. Best Practices for Agents

1. **Prefer Prolog for File I/O**: Use `open/3`, `write/2`, `read/2` instead of calling `vm_exec` with Python.
2. **Type-Safe Tasks**: Always use type wrappers in `task/N` (e.g., `integer(Count)`) to ensure the LLM returns valid data types.
3. **Backtracking for Retries**: Define multiple clauses for `agent_main` to implement fallback strategies.
4. **Context Hygiene**: Use `push_context` or `prompt/N` to keep the context window clean for specific sub-tasks.
5. **Spec-Driven**: Start by writing a high-quality Markdown file, compile it, and then refine the generated `.dml` if necessary.
