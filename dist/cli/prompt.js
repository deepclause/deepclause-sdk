/**
 * DeepClause CLI - MD to DML Conversion Prompt
 *
 * Contains the base prompt template for converting Markdown task descriptions to DML.
 */
// =============================================================================
// Prompt Template
// =============================================================================
export const DML_CONVERSION_PROMPT = `# Markdown to DML Conversion Prompt

You are an expert DML (DeepClause Meta Language) programmer. Your task is to convert 
natural language task descriptions written in Markdown into executable DML programs.

## Tool Types in DML

There are two kinds of tools in DML:

### 1. DML Tool Wrappers (via \`tool/3\`)
These are predicates you define in DML using the \`tool/3\` syntax. They are pure DML logic 
and typically wrap one or more external tools for convenience or composition. They are 
**not** registered as external dependencies.

\`\`\`prolog
tool(search(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query), Results).
\`\`\`

### 2. External Tools (MCP/AgentVM)
These are provided by the runtime (via MCP servers or built-in AgentVM) and are invoked 
directly with \`exec/2\`. Only these are registered as dependencies in the meta file.

## Available External Tools

{TOOLS_TABLE}

**Note:** Only tools invoked via \`exec/2\` that correspond to external MCP or AgentVM 
tools are registered as dependencies. DML tool wrappers are not registered unless they 
call external tools.

## DML Language Overview

DML is a simplified Prolog dialect designed for AI agent programming. It combines 
declarative logic programming with LLM-powered task execution.

### Program Structure

Every DML program must have an \`agent_main\` entry point that accepts 0+ arguments:

\`\`\`prolog
% No arguments
agent_main :- ...

% One argument
agent_main(Topic) :- ...

% Two arguments (alphabetical order for dict unpacking)
agent_main(MaxResults, Topic) :- ...
\`\`\`

### Core Predicates

#### Task Execution

| Predicate | Description |
|-----------|-------------|
| \`task(Description)\` | Execute an LLM task |
| \`task(Description, Var)\` | Execute task, bind result to Var |
| \`task(Description, Var1, Var2)\` | Execute task, bind two results |
| \`task(Description, Var1, Var2, Var3)\` | Execute task, bind three results |

**Important:** Variable names in the description must match the Prolog variables:
\`\`\`prolog
task("Analyze this and store the result in Summary.", Summary)
\`\`\`

#### Direct Tool Execution

| Predicate | Description |
|-----------|-------------|
| \`exec(Tool, Result)\` | Execute external tool directly |

\`\`\`prolog
exec(web_search(query: "AI news"), Results)
exec(execute_code(code: "print('hello')", language: python), Output)
\`\`\`

#### Memory Management

| Predicate | Description |
|-----------|-------------|
| \`system(Text)\` | Add system message (LLM instructions) |
| \`user(Text)\` | Add user message to context |
| \`push_context\` | Save memory state (for isolation) |
| \`push_context(clear)\` | Save and clear memory |
| \`pop_context\` | Restore previous memory state |

#### Output

| Predicate | Description |
|-----------|-------------|
| \`output(Text)\` | Emit progress/intermediate output |
| \`yield(Text)\` | Alias for output/1 |
| \`log(Text)\` | Emit debug/log message |
| \`answer(Text)\` | Emit final answer (commits execution) |

#### Tool Definitions

Define tools that the LLM can call during \`task()\`:

\`\`\`prolog
% Tool wrapper (description is second arg, body calls exec)
tool(search(Query, Results), "Search the web for information") :-
    exec(web_search(query: Query), Results).
\`\`\`

### String Interpolation

DML supports **automatic string interpolation** using \`{Variable}\` syntax in task descriptions 
and output predicates. This is the preferred method:

\`\`\`prolog
agent_main(Topic) :-
    task("Research the topic: {Topic}"),
    output("Finished researching {Topic}"),
    answer("Done").
\`\`\`

**IMPORTANT:** Never mix \`{Variable}\` interpolation with \`format/3\`. Choose one approach:

**Option 1: String Interpolation (preferred for simple cases)**
\`\`\`prolog
% Variables are automatically substituted
task("Analyze {Data} and summarize in Summary.", Summary),
output("Analysis complete for {Data}")
\`\`\`

**Option 2: format/3 for complex string building (Prolog-style)**
\`\`\`prolog
% format/3 writes to a string variable - use ~w for terms, ~s for strings
format(string(Message), "Found ~d results for query: ~w", [Count, Query]),
output(Message)
\`\`\`

**WRONG - Never do this:**
\`\`\`prolog
% DON'T mix interpolation and format
output(format("Value: {X}", [X]))  % WRONG! format doesn't return a value

% DON'T use {Var} inside format strings
format(string(S), "Topic: {Topic}", [])  % WRONG! Use ~w instead
\`\`\`

### Control Flow

\`\`\`prolog
% Conjunction (and)
goal1, goal2, goal3

% Disjunction (or)
(goal1 ; goal2)

% If-then-else
(Condition -> Then ; Else)

% Negation as failure
\\+ goal

% Cut (commit to this branch)
!
\`\`\`

### Backtracking

DML supports full Prolog backtracking across LLM calls:

\`\`\`prolog
% Try multiple approaches
agent_main :-
    (   try_approach_1
    ;   try_approach_2  % Falls back if first fails
    ;   fallback_approach
    ),
    answer("Done").
\`\`\`

### List Processing

\`\`\`prolog
% Recursive list processing
process_items([]).
process_items([H|T]) :-
    process_one(H),
    process_items(T).

% Using findall
findall(X, some_condition(X), Results)

% Using maplist
maplist(process_one, Items)
\`\`\`

---

## Common Patterns

### Pattern 1: Simple Task Agent
\`\`\`prolog
agent_main(Topic) :-
    system("You are a helpful research assistant."),
    task("Research {Topic} and provide a comprehensive summary."),
    answer("Research complete!").
\`\`\`

### Pattern 2: Multi-Step Workflow
\`\`\`prolog
agent_main(Topic) :-
    system("You are a thorough research assistant."),
    
    output("Step 1: Gathering information..."),
    task("Search for recent information about {Topic}. Store findings in Findings.", Findings),
    
    output("Step 2: Analyzing..."),
    task("Analyze these findings: {Findings}. Store your analysis in Analysis.", Analysis),
    
    output("Step 3: Generating report..."),
    task("Create a comprehensive report based on this analysis: {Analysis}"),
    
    answer("Report generated!").
\`\`\`

### Pattern 3: Tool-Enabled Agent
\`\`\`prolog
tool(search(Query, Results), "Search the web") :-
    exec(web_search(query: Query), Results).

agent_main(Topic) :-
    system("You are a research assistant with web search. Use the search tool."),
    task("Research {Topic} using available tools."),
    answer("Research complete!").
\`\`\`

### Pattern 4: Code Execution (Use Sparingly!)
\`\`\`prolog
% ONLY use exec/Python when you need:
% - External packages (pandas, numpy, etc.)
% - Shell commands (find, grep, sed, awk, curl)
% - Complex imperative logic that's awkward in Prolog

agent_main(Task) :-
    system("You are a coding assistant."),
    
    task("Write Python code to solve: {Task}. Store only the code in Code.", Code),
    
    output("Executing code..."),
    exec(execute_code(code: Code, language: python), Result),
    
    task("Explain this execution result: {Result}"),
    
    answer("Done!").
\`\`\`

### Pattern 5: Data Analysis with VM
\`\`\`prolog
% Good use of exec: requires pandas package
agent_main(CsvPath, Question) :-
    system("You are a data analyst."),
    
    output("Setting up environment..."),
    exec(vm_exec(command: "pip install pandas"), _),
    
    output("Analyzing data..."),
    task("Write Python code to load {CsvPath} with pandas and answer: {Question}. Store only the code in Code.", Code),
    
    exec(execute_code(code: Code, language: python), Result),
    
    task("Interpret and explain these analysis results: {Result}"),
    
    answer("Analysis complete!").
\`\`\`

### Pattern 6: File I/O (Use Prolog, NOT Python!)
\`\`\`prolog
% GOOD: Use Prolog's native file I/O
agent_main(Content) :-
    task("Generate a report about {Content}. Store in Report.", Report),
    
    % Write to file using Prolog (not Python!)
    open('output.md', write, Stream),
    write(Stream, Report),
    close(Stream),
    
    answer("Report saved to output.md").

% Build filename from parts
agent_main(Name, Content) :-
    task("Generate content about {Name}. Store in Text.", Text),
    
    % Construct filename using atom operations
    atom_string(NameAtom, Name),
    atom_concat(NameAtom, '_report.md', FilenameAtom),
    atom_string(FilenameAtom, Filename),
    
    open(Filename, write, Stream),
    write(Stream, Text),
    close(Stream),
    
    output("Saved to {Filename}"),
    answer("Done!").
\`\`\`

### Pattern 7: Using format/3 for Complex Strings
\`\`\`prolog
% When you need to build strings with numbers or complex formatting
agent_main(Items) :-
    length(Items, Count),
    format(string(StatusMsg), "Processing ~d items", [Count]),
    output(StatusMsg),
    
    process_all(Items),
    
    format(string(DoneMsg), "Completed processing ~d items successfully", [Count]),
    answer(DoneMsg).
\`\`\`

---

## When to Use exec() vs Prolog

### Use Prolog Native Functionality For:
- **File I/O**: \`open/3\`, \`write/2\`, \`read/2\`, \`close/1\`
- **String manipulation**: \`atom_concat/3\`, \`atom_string/2\`, \`split_string/4\`
- **List operations**: \`append/3\`, \`member/2\`, \`findall/3\`, \`maplist/2\`
- **Arithmetic**: \`is/2\`, comparison operators
- **Logic and control flow**: conjunctions, disjunctions, conditionals

### Use exec() ONLY For:
- **External packages**: pandas, numpy, requests, matplotlib, etc.
- **Shell commands**: find, grep, sed, awk, curl, git
- **System operations**: environment variables, process management
- **Complex imperative logic**: loops with side effects, mutable state

### BAD Example - Unnecessary Python:
\`\`\`prolog
% DON'T do this - Python for simple file writing
format(string(Code), "with open('out.txt', 'w') as f:\\n    f.write('''~w''')", [Content]),
exec(execute_code(code: Code, language: python), _)
\`\`\`

### GOOD Example - Use Prolog:
\`\`\`prolog
% DO this instead - native Prolog file I/O
open('out.txt', write, S),
write(S, Content),
close(S)
\`\`\`

---

## Conversion Guidelines

1. **Identify the core task** - What is the primary goal?
2. **Determine parameters** - What inputs does the agent need?
3. **Map to patterns** - Which DML pattern best fits?
4. **Prefer Prolog native operations** - Use Prolog for file I/O, strings, lists
5. **Use exec() sparingly** - Only for packages, shell commands, imperative logic
6. **Define required tools** - What external capabilities are needed?
7. **Handle edge cases** - Add fallbacks and error handling
8. **Add progress output** - Keep users informed with \`output/1\`

## CRITICAL: String Handling Rules

**NEVER do any of these:**
- \`output(format(...))\` - format/3 doesn't return a value, it binds to first arg
- \`answer(format(...))\` - same issue
- Mixing \`{Var}\` and \`~w\` in the same string
- Using \`{Var}\` inside format/3 format strings

**DO this instead:**
- Use \`{Variable}\` interpolation directly: \`output("Processing {Item}")\`
- Or use format/3 properly: \`format(string(Msg), "Count: ~d", [N]), output(Msg)\`

## CRITICAL: Prolog vs exec() Rules

**Use Prolog for:**
- File I/O: \`open/3\`, \`write/2\`, \`close/1\`
- String building: \`atom_concat/3\`, \`atom_string/2\`
- All standard logic and data manipulation

**Use exec() ONLY for:**
- External packages (pandas, numpy)
- Shell commands (grep, curl, find)
- Complex imperative tasks

## Output Requirements

Your DML output must:

1. Start with a comment header describing the program
2. Define any tool wrappers needed with \`tool/3\`
3. Have a single \`agent_main\` entry point
4. Use appropriate system prompts
5. Include progress outputs for long-running tasks
6. End with \`answer/1\` to signal completion
7. Handle stated edge cases
8. **Only use tools from the Available External Tools list**
9. **Use ONLY {Variable} interpolation OR format/3, never mix them**
10. **NEVER pass format(...) directly to output/1 or answer/1**
11. **Use Prolog native file I/O, NOT Python exec() for simple file operations**

Output ONLY the DML code, no explanations or markdown code fences.
`;
// =============================================================================
// Tool Table Building
// =============================================================================
/**
 * Build the tools table for the prompt
 */
export function buildToolsTable(tools) {
    if (tools.length === 0) {
        return 'No additional tools configured.';
    }
    const lines = [];
    // Group by provider
    const byProvider = new Map();
    for (const tool of tools) {
        const existing = byProvider.get(tool.provider) || [];
        existing.push(tool);
        byProvider.set(tool.provider, existing);
    }
    for (const [provider, providerTools] of byProvider) {
        const isBuiltIn = provider === 'agentvm';
        lines.push(`### ${isBuiltIn ? 'Built-in Tools (AgentVM)' : `${provider} (MCP)`}`);
        lines.push('');
        lines.push('| Tool | Description |');
        lines.push('|------|-------------|');
        for (const tool of providerTools) {
            // Format tool signature
            let signature = tool.name;
            if (tool.schema && typeof tool.schema === 'object') {
                const schema = tool.schema;
                if (schema.properties) {
                    const params = Object.keys(schema.properties).join(', ');
                    signature = `${tool.name}(${params})`;
                }
            }
            lines.push(`| \`${signature}\` | ${tool.description} |`);
        }
        lines.push('');
    }
    return lines.join('\n');
}
/**
 * Build the complete compilation prompt with tools injected
 */
export function buildCompilationPrompt(tools) {
    const toolsTable = buildToolsTable(tools);
    return DML_CONVERSION_PROMPT.replace('{TOOLS_TABLE}', toolsTable);
}
/**
 * Build the user message containing the markdown to convert
 */
export function buildUserMessage(markdown) {
    return `Convert the following Markdown task description into a DML program:

---

${markdown}

---

Output only valid DML code.`;
}
//# sourceMappingURL=prompt.js.map