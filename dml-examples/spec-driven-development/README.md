# Spec-Driven Development with DeepClause

This example demonstrates how to use DeepClause for spec-driven development (SDD), where your specification becomes the executable program.

## The Workflow

```
┌─────────────────┐     ┌─────────────┐     ┌─────────────┐     ┌──────────┐
│  Requirements   │────▶│   Compile   │────▶│     Run     │────▶│   Test   │
│   (Markdown)    │     │   to DML    │     │    DML      │     │  Output  │
└─────────────────┘     └─────────────┘     └─────────────┘     └──────────┘
        ▲                                                              │
        │                                                              │
        └──────────────────── Iterate ─────────────────────────────────┘
```

## Why This Matters

Traditional AI coding tools feed specs to LLMs and hope for the best. The output is non-deterministic—run the same prompt twice, get different code.

With DeepClause:
1. **Specs compile to programs** - The spec becomes actual executable logic
2. **Execution is deterministic** - Same input → same execution path
3. **Specs are versioned** - Track changes in git, see exactly what changed
4. **Behavior is inspectable** - Read the DML to see what it will do

## Example: Code Review Spec

### Version 1: Basic Review

See [code-review-v1.md](./code-review-v1.md) - A basic code review spec.

```bash
# Compile v1
deepclause compile code-review-v1.md

# Run it
deepclause run code-review-v1.dml src/handler.ts
```

### Version 2: Security-Focused Review  

See [code-review-v2.md](./code-review-v2.md) - Same spec with added security requirements.

```bash
# Recompile after changing requirements
deepclause compile code-review-v2.md

# Run - now includes security checks
deepclause run code-review-v2.dml src/handler.ts
```

### Version 3: Multi-File Review

See [code-review-v3.md](./code-review-v3.md) - Extended to handle multiple files.

```bash
# Recompile
deepclause compile code-review-v3.md

# Run on multiple files
deepclause run code-review-v3.dml "src/auth.ts,src/db.ts,src/handler.ts"
```

## Reproducibility Demo

The key insight: **the same DML produces the same execution path every time**.

```bash
# Run the same review twice
deepclause run code-review-v2.dml src/handler.ts > review1.md
deepclause run code-review-v2.dml src/handler.ts > review2.md

# The execution path is identical (tool calls, order of operations)
# Only LLM output text may vary, but the structure is deterministic
```

Compare this to prompting an LLM directly, where you might get completely different approaches each time.

## Key Files

| File | Description |
|------|-------------|
| [code-review-v1.md](./code-review-v1.md) | Basic review spec |
| [code-review-v1.dml](./code-review-v1.dml) | Compiled DML (generated) |
| [code-review-v2.md](./code-review-v2.md) | Security-focused spec |
| [code-review-v2.dml](./code-review-v2.dml) | Compiled DML (generated) |
| [code-review-v3.md](./code-review-v3.md) | Multi-file spec |
| [code-review-v3.dml](./code-review-v3.dml) | Compiled DML (generated) |

## Iteration Checklist

When evolving your spec:

1. **Edit the markdown** - Change requirements, add behaviors
2. **Recompile** - `deepclause compile spec.md --force`
3. **Review the DML** - Inspect what changed in the logic
4. **Test** - Run with sample inputs
5. **Commit both** - Track spec + DML together in version control
