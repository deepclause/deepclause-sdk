# DeepClause CLI Implementation Plan

## Overview

This document outlines the implementation plan for the DeepClause CLI tool, which compiles Markdown task descriptions into DML (DeepClause Meta Language) programs and executes them.

**Target Timeline:** 6 weeks  
**Team Size:** 1-2 developers  

---

## Phase 1: Foundation (Week 1-2)

### 1.1 Project Setup

- [ ] Initialize CLI project structure
  ```
  src/cli/
    index.ts          # Entry point, commander setup
    config.ts         # Configuration management
    compile.ts        # MD-to-DML compilation
    run.ts            # DML execution
    tools.ts          # Tool resolution
    commands.ts       # Command listing
    tui/
      index.ts        # Ink-based TUI
      components/     # React components for TUI
  ```

- [ ] Set up dependencies
  - `commander` - CLI argument parsing
  - `ink` + `react` - Terminal UI
  - `zod` - Config validation
  - `@anthropic-ai/sdk`, `openai`, `@ai-sdk/google` - LLM providers
  - `@anthropic-ai/mcp-client-sdk` - MCP client
  - `memfs` - Testing filesystem mock
  - `vitest` - Test runner

- [ ] Configure TypeScript, ESLint, Prettier
- [ ] Set up GitHub Actions for CI/CD

### 1.2 Configuration System

- [ ] Implement `deepclause init`
  - Create `.deepclause/` directory structure
  - Generate default `config.json`
  - Detect existing configuration

- [ ] Implement config loading and validation
  - Zod schema for config structure
  - Environment variable resolution
  - Config merging (global + local)

- [ ] Implement `deepclause set-model` and `deepclause show-model`

**Deliverables:**
- Working `init`, `set-model`, `show-model` commands
- Config validation with helpful error messages
- Unit tests for config module

---

## Phase 2: Compilation Pipeline (Week 2-3)

### 2.1 Prompt Construction

- [ ] Create base MD-to-DML conversion prompt
  - DML language reference
  - Common patterns library
  - Tool type distinction (DML wrappers vs external)

- [ ] Implement dynamic prompt construction
  - Inject available tools from config
  - Include AgentVM tools
  - Query MCP servers for tool schemas

- [ ] Create prompt template system
  - Allow custom prompts via config
  - Support prompt overrides per task

### 2.2 Compilation Logic

- [ ] Implement `deepclause compile`
  - Parse input Markdown
  - Construct compilation prompt
  - Call LLM for DML generation
  - Validate generated DML syntax

- [ ] Implement meta file generation
  - Extract parameters from `agent_main` signature
  - Extract tool dependencies from `exec/2` calls
  - Calculate source hash
  - Manage version history

- [ ] Implement compilation caching
  - Skip if source unchanged
  - Force recompile with `--force`
  - Track model per version

- [ ] Implement `--validate-only` mode

**Deliverables:**
- Working `compile` command
- Meta file generation with tool dependencies
- Compilation caching
- Unit tests for compile module

### 2.3 Tool Dependency Extraction

- [ ] Implement DML parser for `exec/2` extraction
  - Regex-based extraction for MVP
  - Consider SWI-Prolog WASM for accurate parsing later

- [ ] Distinguish external tools from DML wrappers
  - Only include `exec/2` calls in dependencies
  - Trace through `tool/3` definitions

- [ ] Validate tools exist at compile time (warning)

---

## Phase 3: Execution Runtime (Week 3-4)

### 3.1 Tool Resolution

- [ ] Implement tool resolution system
  - Load MCP server configs
  - Start MCP servers on demand
  - Register AgentVM tools (always available)

- [ ] Implement `deepclause list-tools`
  - Show all available tools with providers
  - JSON output option
  - Handle MCP connection errors gracefully

- [ ] Implement pre-execution tool validation
  - Check all dependencies available before run
  - Clear error messages for missing tools

### 3.2 DML Execution

- [ ] Integrate existing `sdk.ts` and `runner.ts`
  - Pass resolved tools to runner
  - Configure workspace path
  - Set up LLM provider from config

- [ ] Implement `deepclause run`
  - Load DML and meta files
  - Resolve positional and named arguments
  - Execute with configured model
  - Return structured output

- [ ] Implement execution options
  - `--workspace` - Working directory
  - `--verbose` - Debug output
  - `--trace` - Save execution trace
  - `--dry-run` - Show without executing
  - `--model` - Override model

### 3.3 AgentVM Integration

- [ ] Integrate AgentVM package
  - Lazy initialization on first use
  - Shared workspace mounting
  - Capture stdout/stderr/exitCode

- [ ] Implement shared filesystem
  - Same paths in DML and VM
  - Bidirectional file access

**Deliverables:**
- Working `run` command
- Tool resolution with MCP and AgentVM
- Execution tracing
- Unit tests for run module

---

## Phase 4: Command Management (Week 4-5)

### 4.1 Command Listing

- [ ] Implement `deepclause list-commands`
  - Scan `.deepclause/tools/` for DML files
  - Load meta files for descriptions
  - Show usage examples

- [ ] Implement `--detailed` option
  - Show parameters with descriptions
  - Show tool dependencies with providers
  - Show compilation history

- [ ] Implement `--json` output

### 4.2 Batch Operations

- [ ] Implement batch compilation
  - Compile all `.md` files in directory
  - Parallel compilation with concurrency limit
  - Summary report

- [ ] Implement watch mode (optional)
  - Monitor task files for changes
  - Auto-recompile on save

**Deliverables:**
- Working `list-commands` command
- Batch compilation support
- Integration tests

---

## Phase 5: Terminal UI (Week 5-6)

### 5.1 TUI Foundation

- [ ] Set up Ink framework
  - Create base `App` component
  - Implement `--headless` mode detection

- [ ] Implement execution progress display
  - Current step indicator
  - Spinner for LLM calls
  - Tool call visualization

### 5.2 TUI Components

- [ ] `<ExecutionView>` - Main execution display
  - Step progress
  - Output streaming
  - Tool call details

- [ ] `<ToolCallView>` - Tool execution display
  - Tool name and arguments
  - Execution time
  - Result preview

- [ ] `<ErrorView>` - Error display
  - Formatted error messages
  - Stack traces (verbose mode)
  - Suggestions for fixes

- [ ] `<CompileView>` - Compilation progress
  - LLM generation progress
  - Validation results
  - Meta file summary

### 5.3 Output Formatting

- [ ] Implement Markdown rendering in terminal
  - Code blocks with syntax highlighting
  - Tables
  - Lists

- [ ] Implement structured output
  - JSON output mode
  - Machine-readable format

**Deliverables:**
- Full TUI implementation
- Headless mode for scripting
- Visual tests/screenshots

---

## Phase 6: Polish & Documentation (Week 6)

### 6.1 Error Handling

- [ ] Comprehensive error messages
  - Config errors with fix suggestions
  - Compilation errors with line numbers
  - Runtime errors with context

- [ ] Graceful degradation
  - Handle network failures
  - Handle MCP server crashes
  - Handle LLM API errors

### 6.2 Documentation

- [ ] Update README with CLI usage
- [ ] Create `--help` content for all commands
- [ ] Add examples directory
- [ ] Write troubleshooting guide

### 6.3 Testing & Release

- [ ] Complete test coverage (>80%)
- [ ] Integration tests with real LLM (CI skip by default)
- [ ] Performance benchmarks
- [ ] npm package preparation
- [ ] Release v1.0.0

**Deliverables:**
- Production-ready CLI
- Comprehensive documentation
- npm package published

---

## Technical Decisions

### CLI Framework: Commander.js
- Mature, well-documented
- Built-in help generation
- Subcommand support

### TUI Framework: Ink
- React-based, familiar paradigm
- Good component ecosystem
- Supports streaming updates

### Testing: Vitest + memfs
- Fast, ESM-native
- Virtual filesystem for isolation
- Good mocking support

### LLM Integration: Vercel AI SDK
- Provider-agnostic
- Streaming support
- Tool calling standardization

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| LLM generates invalid DML | Validation layer, retry with feedback |
| MCP servers unreliable | Timeout handling, clear error messages |
| AgentVM performance | Lazy loading, VM reuse |
| Large file handling | Streaming, chunking |
| Cross-platform issues | CI testing on Linux/macOS/Windows |

---

## Success Metrics

1. **Compilation Success Rate:** >95% valid DML from well-formed Markdown
2. **Execution Reliability:** >99% successful runs with valid DML
3. **Tool Resolution Time:** <2s to resolve all tools
4. **User Experience:** Clear progress, helpful errors, fast feedback

---

## Module Dependency Graph

```
                    ┌─────────────┐
                    │   index.ts  │
                    │  (CLI entry)│
                    └──────┬──────┘
                           │
        ┌──────────────────┼──────────────────┐
        │                  │                  │
        ▼                  ▼                  ▼
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│   config.ts   │  │  compile.ts   │  │    run.ts     │
│               │  │               │  │               │
└───────┬───────┘  └───────┬───────┘  └───────┬───────┘
        │                  │                  │
        │                  │                  │
        ▼                  ▼                  ▼
┌───────────────────────────────────────────────────────┐
│                       tools.ts                        │
│            (Tool resolution, MCP client)              │
└───────────────────────────┬───────────────────────────┘
                            │
            ┌───────────────┼───────────────┐
            │               │               │
            ▼               ▼               ▼
     ┌───────────┐   ┌───────────┐   ┌───────────┐
     │  sdk.ts   │   │ runner.ts │   │ AgentVM   │
     │  (LLM)    │   │  (Prolog) │   │  (WASM)   │
     └───────────┘   └───────────┘   └───────────┘
```

---

## File Structure (Final)

```
deepclause-sdk/
├── src/
│   ├── cli/
│   │   ├── index.ts           # CLI entry point
│   │   ├── config.ts          # Configuration management
│   │   ├── compile.ts         # MD-to-DML compilation
│   │   ├── run.ts             # DML execution
│   │   ├── tools.ts           # Tool resolution
│   │   ├── commands.ts        # Command listing
│   │   └── tui/
│   │       ├── index.ts       # TUI entry
│   │       ├── App.tsx        # Main app component
│   │       ├── ExecutionView.tsx
│   │       ├── ToolCallView.tsx
│   │       ├── CompileView.tsx
│   │       └── ErrorView.tsx
│   ├── sdk.ts                 # Core SDK (existing)
│   ├── runner.ts              # DML runner (existing)
│   ├── agent.ts               # Agent logic (existing)
│   ├── tools.ts               # Tool definitions (existing)
│   └── types.ts               # Type definitions
├── tests/
│   ├── cli.test.ts            # CLI tests
│   ├── compile.test.ts        # Compilation tests
│   ├── run.test.ts            # Execution tests
│   └── integration.test.ts    # End-to-end tests
├── docs/
│   ├── CLI_SPEC.md            # Full CLI specification
│   ├── DML_REFERENCE.md       # DML language reference
│   └── TROUBLESHOOTING.md     # Common issues
├── examples/
│   ├── tasks/                 # Example Markdown tasks
│   └── tools/                 # Example compiled DML
├── package.json
├── tsconfig.json
└── vitest.config.ts
```

---

## Changelog

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-29 | 0.1.0 | Initial implementation plan |
