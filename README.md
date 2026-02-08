# DeepClause CLI and SDK

Compile markdown specs into executable logic programs. Guaranteed execution semantics for agentic workflows.

![docs/overview.png](docs/overview.png)


## What This Is

AI skills and tools are everywhere—but most are just prompts. When a prompt fails, you tweak it. When you need branching logic, you write wrapper code. When you want retry behavior, you build it yourself.

DeepClause takes a different approach: **compile task descriptions into DML programs**—a Prolog-based language that handles control flow, error recovery, and tool orchestration automatically.


```
Markdown description  →  compile  →  Logic program  →  run  →  Output
```

## Sandboxed by Default

Everything runs in WebAssembly—no native code execution, no container setup required:

- **[AgentVM](https://github.com/deepclause/agentvm)**: A lightweight WASM-based Linux environment for shell commands, file operations, and Python/Node execution
- **Prolog runtime**: The logic engine itself runs in WASM (SWI-Prolog compiled to WebAssembly)

This means that DML tools and agents can execute arbitrary shell commands with minimal chance of escaping to your host system.

## Beyond Markdown: Why Logic Programming?

Markdown skills are great for simple, linear workflows. But real-world tasks often need:

- **Branching logic** - Try approach A, fall back to B if it fails
- **Iteration** - Process a list of items one by one
- **State management** - Isolate context between sub-tasks
- **Error recovery** - Handle failures gracefully
- **Composition** - Build complex skills from simpler ones

When you give markdown instructions to a typical agentic loop, there's no guarantee these requirements will actually be followed—the LLM might ignore the fallback logic or skip items in a list. 

By compiling to Prolog, you get **guaranteed execution semantics**: backtracking ensures fallbacks happen, recursion processes every item, and unification binds variables correctly. You define *what* should happen—the runtime guarantees *how*.



## Spec-Driven Development That Compiles

[Spec-driven development](https://martinfowler.com/articles/exploring-gen-ai/sdd-3-tools.html) proposes writing specifications before code, with the spec becoming the source of truth. Current SDD tools (Kiro, spec-kit, Tessl) generate elaborate markdown artifacts that are then fed to coding agents—but the output is still non-deterministic, and you end up reviewing both specs *and* generated code.

DeepClause offers a different approach: **specs that compile to actual programs**.

```bash
# Your spec
cat > api-client.md << 'EOF'
# API Client Generator
Generate a TypeScript API client from an OpenAPI spec URL.

## Arguments
- SpecUrl: URL to an OpenAPI/Swagger JSON specification

## Behavior
- Fetch the OpenAPI spec from SpecUrl
- Extract endpoints and types
- Generate typed client code
- Write to output file
EOF

# Compile it once
deepclause compile api-client.md

# Run it deterministically, forever
deepclause run api-client.dml "https://api.example.com/openapi.json"
```

The compiled `.dml` is inspectable logic—you can see exactly what it does:

```prolog
tool(fetch_spec(Url, Spec), "Fetch OpenAPI specification") :-
    exec(web_fetch(url: Url), Spec).

agent_main(SpecUrl) :-
    system("You are an API client generator..."),
    fetch_spec(SpecUrl, Spec),
    task("Extract endpoints from: {Spec}", Endpoints),
    task("Generate TypeScript client for: {Endpoints}", Code),
    exec(vm_exec(command: "cat > client.ts"), Code),
    answer("Generated client.ts").
```

Unlike traditional SDD where specs guide but don't control, DeepClause specs **become** the executable. The spec *is* the code—just at a higher abstraction level.

## Quick Start

```bash
# Install
npm install -g deepclause-sdk

# Set API key (or ANTHROPIC_API_KEY, GOOGLE_API_KEY, etc.)
export OPENAI_API_KEY="sk-..."

# Initialize in your project
deepclause init

# Configure the model for compilation
deepclause set-model openai/gpt-4o

# Create a task description
cat > .deepclause/tools/explain.md << 'EOF'
# Code Explainer

Explain what a piece of code does in plain English.

## Arguments
- Code: The source code to explain

## Behavior
- Break down the code into logical sections
- Explain each section's purpose
- Note any potential issues
EOF

# Compile to executable DML
deepclause compile .deepclause/tools/explain.md

# Run it
deepclause run .deepclause/tools/explain.dml "function fib(n) { return n < 2 ? n : fib(n-1) + fib(n-2) }"
```

## Use Cases

### Reliable tools for coding agents

Give your AI coding assistant more deterministic, inspectable tools instead of hoping prompts work:

```bash
# Define a tool the agent can use
cat > .deepclause/tools/api-docs.md << 'EOF'
# API Documentation Lookup
Search for API documentation and summarize usage patterns.

## Arguments
- Query: The API or library name to look up

## Tools needed
- web_search

## Behavior
- Search for official documentation
- Summarize usage patterns and examples
EOF

# Compile it once
deepclause compile .deepclause/tools/api-docs.md

# Now your coding agent can run it reliably
deepclause run .deepclause/tools/api-docs.dml "Stripe PaymentIntent"
```

The compiled `.dml` files execute the same way every time—no prompt variance, no skipped steps. Build up a library of tools your agent can trust.

### Automation pipelines

Chain compiled programs together:

```bash
deepclause run review-code.dml src/handler.ts > review.md
deepclause run summarize.dml review.md
```

### Shareable, versionable task logic

Check `.dml` files into version control. The logic is inspectable—you can see exactly what the program does, not just what prompt it sends.

## Example Task Descriptions

### Web Research
```markdown
# Web Research
Search the web and synthesize findings into a report.

## Arguments
- Question: The research question to investigate

## Tools needed
- web_search

## Behavior
- Search for 3-5 authoritative sources on the Question
- Extract key findings from each
- Write a summary with inline citations
```

### Code Review
```markdown
# Code Review
Review code for bugs, security issues, and style.

## Arguments
- FilePath: Path to the file to review

## Tools needed
- vm_exec (to read files)

## Behavior
- Read the file at FilePath
- Check for common bugs and anti-patterns
- Identify security concerns
- Suggest improvements
- Be concise and actionable
```

### Data Analysis
```markdown
# CSV Analyzer
Analyze a CSV file and describe its contents.

## Arguments
- FilePath: Path to the CSV file to analyze

## Tools needed  
- vm_exec (to run Python)

## Behavior
- Load the CSV at FilePath with pandas
- Describe the schema (columns, types, row count)
- Identify interesting patterns
- Generate summary statistics
```

## Available Tools

Skills can use these built-in tools:

| Tool | Description |
|------|-------------|
| `web_search` | Search the web (requires `BRAVE_API_KEY`) |
| `news_search` | Search recent news |
| `vm_exec` | Run shell commands in a sandbox |
| `ask_user` | Prompt the user for input |

Configure tools in `.deepclause/config.json`.

MCP support is on the roadmap.

## CLI Reference

```bash
deepclause init                    # Set up .deepclause/ folder
deepclause compile <file.md>       # Compile Markdown to DML
deepclause compile-all <dir>       # Compile all .md files in directory
deepclause run <file.dml> [args]   # Execute a compiled skill
deepclause list-commands           # List available compiled skills
deepclause list-tools              # Show available tools
deepclause set-model <model>       # Change default model
```

### Run Options

```bash
deepclause run skill.dml "input" \
  --model google/gemini-2.5-flash \   # Override model (e.g. use cheaper model for execution, SOTA model for planning/compilation)
  --stream \                           # Stream output
  --verbose \                          # Show tool calls
  --workspace ./data                   # Set working directory
```

## Configuration

`.deepclause/config.json`:
```json
{
  "model": "gpt-4o",
  "provider": "openai",
  "agentvm": { "network": true }
}
```

### Model at Compile Time vs Run Time

The model specified in `config.json` (or via `--model`) is used during **compilation** to generate the DML program. At **run time**, you can use a different model:

```bash
# Compile with GPT-4o (better at understanding intent)
deepclause set-model openai/gpt-4o
deepclause compile research.md

# Run with a faster/cheaper model
deepclause run research.dml "quantum computing" --model google/gemini-2.5-flash
```

This lets you use a more capable model for the one-time compilation step, then execute with a faster or cheaper model for repeated runs.

### Supported Models

| Provider | Models |
|----------|--------|
| OpenAI | `gpt-4o`, `gpt-4o-mini`, `o1`, `o3-mini` |
| Anthropic | `claude-sonnet-4-20250514`, `claude-3-5-sonnet-20241022` |
| Google | `gemini-2.5-pro`, `gemini-2.5-flash` |
| OpenRouter | Any model via `openrouter/provider/model` |

## Understanding DML

The compiled `.dml` files use DML (DeepClause Meta Language), a dialect of Prolog designed for AI workflows.

```prolog
% Generated from research.md
tool(search(Query, Results), "Search the web") :-
    exec(web_search(query: Query), Results).

agent_main(Topic) :-
    system("You are a research assistant..."),
    task("Research {Topic} and summarize findings."),
    answer("Done").
```

You can edit DML directly for fine-grained control. See the [DML Reference](./docs/DML_REFERENCE.md) for the full language spec.


### Backtracking: Automatic Retry Logic

Prolog's backtracking means you can define multiple approaches. If one fails, execution automatically tries the next:

```prolog
% Try fast approach first, fall back to thorough approach
agent_main(Question) :-
    system("Answer concisely."),
    task("Answer: {Question}"),
    validate_answer,  % Fails if answer is inadequate
    answer("Done").

agent_main(Question) :-
    system("Be thorough and detailed."),
    task("Research and answer: {Question}"),
    answer("Done").
```

If `validate_answer` fails, Prolog backtracks and tries the second clause. No explicit if/else needed. Backtracking resets the execution state (including LLM context) to the original choice point!

### Recursion: Processing Lists

Handle variable-length inputs naturally:

```prolog
% Process each file in a list
process_files([]).
process_files([File|Rest]) :-
    task("Review {File} for issues."),
    process_files(Rest).

agent_main(Files) :-
    process_files(Files),
    answer("All files reviewed.").
```

### Memory Isolation: Independent Sub-tasks

Use `prompt/N` for LLM calls that shouldn't share context:

```prolog
agent_main(Topic) :-
    system("You are a researcher."),
    task("Research {Topic} deeply.", Findings),
    
    % Independent critique - fresh context, no bias from main research
    prompt("As a skeptic, critique: {Findings}", Critique),
    
    % Back to main context
    task("Address this critique: {Critique}"),
    answer("Done").
```

### Tool Scoping: Controlled Capabilities

Limit what tools are available to specific sub-tasks:

```prolog
tool(dangerous_action(X, Result), "Do something risky") :-
    exec(vm_exec(command: X), Result).

agent_main(Task) :-
    % Main task has all tools
    task("Plan how to: {Task}", Plan),
    
    % Execute with restricted tools - no dangerous_action allowed
    without_tools([dangerous_action], (
        task("Execute this plan safely: {Plan}")
    )),
    answer("Done").
```

### Composition: Building Blocks

Define reusable predicates and compose them:

```prolog
% Reusable building blocks
search_and_summarize(Query, Summary) :-
    exec(web_search(query: Query), Results),
    task("Summarize: {Results}", Summary).

verify_facts(Text, Verified) :-
    task("Fact-check this text: {Text}", Issues),
    (Issues = "none" -> Verified = Text ; fix_issues(Text, Issues, Verified)).

% Compose into a skill
agent_main(Topic) :-
    search_and_summarize(Topic, Draft),
    verify_facts(Draft, Final),
    answer(Final).
```

## Using as a Library

Embed DeepClause in your own applications:

```typescript
import { createDeepClause } from 'deepclause-sdk';

const dc = await createDeepClause({
  model: 'gpt-4o',
  apiKey: process.env.OPENAI_API_KEY,
});

for await (const event of dc.runDML(code)) {
  console.log(event.type, event.content);
}

await dc.dispose();
```

See [sdk-examples/](./sdk-examples/) for more.

## More Resources

- [DML Reference](./docs/DML_REFERENCE.md) - Full language documentation
- [Examples](./dml-examples/) - Sample DML programs
- [Architecture](./ARCHITECTURE.md) - How it works

## License

MIT
