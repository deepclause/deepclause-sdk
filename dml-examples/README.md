# DML Examples

This directory contains example DML (Declarative Modeling Language) programs demonstrating various features of the DeepClause SDK.

## Running Examples

```bash
# Run any example
deepclause run dml-examples/<example>.dml

# Or with node
node dist/cli/index.js run dml-examples/<example>.dml
```

## Examples

### Basic Examples

- **`test.dml`** - Simple test program
- **`vm-exec.dml`** - Demonstrates executing external tools via `exec/2`

### Agent Patterns

- **`coding-agent.dml`** - A coding assistant using idiomatic Prolog failure-driven control flow
- **`knowledge-agent.dml`** - Shopping assistant with Prolog knowledge base (products, cart, discounts)

### Advanced Features

- **`nested-task-test.dml`** - Demonstrates using `task()` inside tool definitions to combine Prolog logic with LLM reasoning
- **`deep_research.dml`** - Deep research agent with multi-step reasoning

## Key Concepts Demonstrated

### Failure-Driven Control Flow

```prolog
% Try each approach until one succeeds
handle_task(Task) :-
    approach_1(Task).
handle_task(Task) :-
    approach_2(Task).
handle_task(_) :-
    fallback_response.
```

### Tools with Nested LLM Calls

```prolog
% Tools can use task() internally
tool(explain_calculation(A, B, Explanation)) :-
    Sum is A + B,  % Prolog computation
    format(string(Desc), "Explain ~w + ~w = ~w to a child", [A, B, Sum]),
    task(Desc, Explanation).  % LLM explanation
```

### Knowledge Base Integration

```prolog
% Prolog facts as knowledge base
product("laptop", 999).
category("laptop", "electronics").

% Tools query the knowledge base
tool(list_products(Products)) :-
    findall(product(Name, Price), product(Name, Price), Products).
```

## See Also

- [DML Reference](../docs/DML_REFERENCE.md) - Complete language reference
- [Architecture](../ARCHITECTURE.md) - How DML execution works
- [SDK Examples](../sdk-examples/) - TypeScript integration examples
