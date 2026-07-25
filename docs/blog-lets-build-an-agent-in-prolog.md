# Let's Build an Agent in Prolog!

Most AI agents are prompt chains held together with Python glue. When something goes wrong — and it will — you're debugging invisible state across a dozen async callbacks. What if there was a language *designed* for this? One where control flow is a first-class concept, retry logic is free, and your agent's behavior is a provable program?

Enter **DML** — the DeepClause Meta Language. It's a Prolog dialect purpose-built for programming AI agents. You get LLM calls as native predicates, automatic backtracking across model invocations, typed outputs, and tool orchestration — all in a few lines of declarative code.

Let's build something real: a **vulnerability scanner agent** that analyzes a codebase, finds security issues, and reports them. We'll start dead simple and layer on capabilities, the way you'd actually develop this.

## The Simplest Possible Agent

Every DML program starts with `agent_main`. Think of it as `main()` but for agents. Here's the absolute minimum:

```prolog
agent_main :-
    system("You are a senior security researcher.
            You specialize in finding vulnerabilities
            in source code."),
    task("Analyze common web application patterns and
          list the top 5 vulnerability categories
          you would look for during a security audit."),
    answer("Analysis complete.").
```

Three predicates. That's it.

- **`system/1`** sets the LLM's persona. It goes into the conversation as a system message — persistent context that shapes every subsequent call.
- **`task/1`** sends a request to the model. The LLM thinks, responds, and execution continues.
- **`answer/1`** emits the final result and *commits* — no backtracking happens after this. The program is done.

Run it:

```bash
deepclause run vuln-scanner.dml
```

The model produces a nice list of OWASP categories. Useful as a starting point, but not exactly a scanner. It's just talking to itself. Let's make it interactive.

## Adding User Input

Real tools talk to users. DML tools are defined with the `tool/2` predicate — a name, a description (shown to the LLM), and a body that does the actual work:

```prolog
% Tool: Ask the user a question
tool(ask(Prompt, Response),
     "Ask the user a question and get their response") :-
    exec(ask_user(prompt: Prompt), Result),
    Response = Result.user_response.

agent_main :-
    system("You are a senior security researcher.
            You specialize in finding vulnerabilities
            in source code."),
    task("Ask the user what codebase or file they want
          to analyze for security vulnerabilities.
          Store their response in Target.", Target),
    task("Based on the target '{Target}', outline a
          security audit plan. List what you would check
          and why. Store the plan in Plan.", Plan),
    output(Plan),
    answer("Audit plan generated.").
```

A few things to unpack:

**`tool/2`** makes a predicate visible to the LLM as a callable function. When we tell the model to "ask the user" in a `task()`, it can call our `ask` tool — DML routes that to the `exec/2` call under the hood, which invokes the registered `ask_user` external tool. The response flows back into the Prolog variable. The key insight: tools defined with `tool/2` are *available to the LLM during any `task()` call*. The model decides when to use them.

**`task/2`** (with two arguments) is where DML gets interesting. The second argument is an *output variable*. We instruct the model to "ask the user" and "store their response in Target" — the LLM calls the `ask` tool, gets the user's answer, and the runtime binds it to `Target`. You reference the variable name *in the prompt itself*, and the runtime extracts it. This is how data flows between steps: not through callbacks or shared mutable state, but through Prolog unification.

**`output/1`** emits progress to the caller without ending execution. Unlike `answer/1`, it doesn't commit — the program keeps running.

The model now asks the user what to scan, gets a target, and produces a plan. Better. But a plan without execution is just a document.

## Running Shell Commands

Let's give our agent hands. The `vm_exec` built-in runs shell commands in a sandboxed environment:

```prolog
% Tool: Ask the user a question
tool(ask(Prompt, Response),
     "Ask the user a question and get their response") :-
    exec(ask_user(prompt: Prompt), Result),
    Response = Result.user_response.

% Tool: Run a shell command in the sandbox
tool(run_shell(Command, Output),
     "Run a shell command and return stdout") :-
    exec(vm_exec(command: Command), Result),
    get_dict(stdout, Result, Output).

% Tool: Read a file
tool(read_file(Path, Content),
     "Read the contents of a file") :-
    format(string(Cmd), "cat '~w'", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Content).

agent_main :-
    system("You are a senior security researcher performing
            a hands-on code audit. You can run shell commands
            and read files. Be thorough and methodical."),

    task("Ask the user for the directory to scan.
          Store their answer in Target.", Target),

    output("Scanning codebase..."),
    task("Explore the directory '{Target}':
          1. List all source files using run_shell
          2. Identify the languages and frameworks used
          3. Read key files that are likely to contain
             security-relevant code (auth, input handling,
             database queries, API endpoints)
          Store a summary of what you found in Recon.", Recon),

    output("Analyzing for vulnerabilities..."),
    task("Based on your reconnaissance: {Recon}

          Read the relevant source files and analyze them
          for security vulnerabilities. Look for:
          - SQL injection
          - XSS (cross-site scripting)
          - Authentication/authorization flaws
          - Hardcoded secrets or credentials
          - Insecure deserialization
          - Path traversal
          - Command injection

          For each finding, note the file, line, and a
          brief explanation.

          Store your findings in Findings.", Findings),

    output("Generating report..."),
    task("Write a concise security report based on these
          findings: {Findings}

          Format as Markdown with severity ratings
          (Critical/High/Medium/Low) for each issue.
          Include remediation advice.
          Store the report in Report.", Report),

    output(Report),
    answer("Security audit complete.").
```

Now the agent *actually does things*. It runs `ls`, `find`, `cat` — reads source files, greps for patterns, and applies security expertise to real code. The LLM chooses which tools to use and when, but the *control flow* — the sequence of recon → analysis → reporting — is deterministic. Defined in Prolog. No prompt engineering gymnastics to keep the model on track.

Notice how tool definitions are just Prolog clauses. `read_file` is syntactic sugar built on `run_shell` via `format/3`. You can compose tools arbitrarily, add validation logic, error handling — it's all just Prolog.

## Typed Outputs and Deterministic Branching

Here's where DML earns its keep. So far, we've been treating LLM outputs as opaque strings. But what if we need *structured* decisions?

DML supports **type-safe output variables**. Wrap a variable in `integer()`, `boolean()`, `list(string())`, or `object()` and the runtime enforces the type with schema validation — no more parsing JSON from markdown code fences.

Let's use this to build a proper triage system:

```prolog
% --- Tool Definitions (same as before) ---

tool(ask(Prompt, Response),
     "Ask the user a question and get their response") :-
    exec(ask_user(prompt: Prompt), Result),
    Response = Result.user_response.

tool(run_shell(Command, Output),
     "Run a shell command and return stdout") :-
    exec(vm_exec(command: Command), Result),
    get_dict(stdout, Result, Output).

tool(read_file(Path, Content),
     "Read the contents of a file") :-
    format(string(Cmd), "cat '~w'", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Content).

% --- Severity-based response ---

handle_findings(Count, _) :-
    Count =:= 0,
    answer("No vulnerabilities found. The codebase looks clean.").

handle_findings(Count, Findings) :-
    Count > 0, Count =< 3,
    task("Write a brief security advisory for these
          low-count findings: {Findings}
          Store the advisory in Advisory.", Advisory),
    answer(Advisory).

handle_findings(Count, Findings) :-
    Count > 3,
    output("⚠  High number of findings — running deep analysis..."),
    task("These findings need detailed remediation plans: {Findings}
          For each vulnerability:
          1. Explain the attack vector
          2. Show a proof-of-concept (safe, illustrative)
          3. Provide the exact code fix

          Store the deep analysis in DeepReport.", DeepReport),
    task("Write an executive summary of this security audit.
          Total issues: {Count}. Store in Summary.", Summary),
    output(Summary),
    answer(DeepReport).

% --- Main ---

agent_main :-
    system("You are a senior security researcher.
            Be precise about vulnerability counts."),

    task("Ask the user which directory to audit
          using the ask tool. Store their answer in Target.",
         Target),

    output("Scanning..."),
    task("Scan '{Target}' for source files and read the
          security-relevant ones. Analyze for vulnerabilities.
          Return the total number of distinct vulnerabilities
          found in Count.",
         integer(Count)),

    task("List each vulnerability with file, line number,
          and category. Store as Findings.",
         list(string(Findings))),

    format(string(Msg),
           "Found ~w potential vulnerabilities.", [Count]),
    output(Msg),

    handle_findings(Count, Findings).
```

Let's trace what happens:

1. **`integer(Count)`** — the LLM must return a number. Not "about five" or "several." An integer. The runtime validates this with a Zod schema and will retry if the model returns garbage.

2. **`list(string(Findings))`** — the findings come back as a proper list of strings. Not a markdown blob. A Prolog list you can `length/2`, `member/2`, or iterate over.

3. **`handle_findings/2`** uses Prolog's multiple clause definitions — the killer feature. Three clauses, three behaviors:
   - Zero findings → clean bill of health
   - 1–3 findings → brief advisory
   - 4+ findings → deep analysis with PoCs and fixes

This is *pattern matching on LLM output driving deterministic program behavior*. The model provides the data; Prolog provides the logic. No `if/elif/else` chains buried in prompt templates. No hoping the model follows your instructions about when to go deep vs. keep it brief.

And because it's Prolog — if any `task()` fails, DML backtracks. The memory state rolls back automatically. You get retry logic for free, just by writing multiple clauses.

## What Just Happened

In about 60 lines of DML, we built a vulnerability scanner that:

- Talks to the user
- Explores a real filesystem
- Reads and analyzes source code
- Returns typed, validated results
- Branches behavior based on structured LLM output
- Has automatic retry semantics via backtracking

No framework. No dependency graph. No orchestration layer. Just logic.

The interesting bit isn't any single feature — it's how they compose. Prolog's unification means data flows naturally between steps. Backtracking means error handling is structural, not bolted on. Tool definitions are just clauses, so you can test and compose them like any other predicate.

DML doesn't try to make LLMs deterministic. It wraps them in a deterministic control layer so you get the creativity where you need it (analysis, explanation, decision-making) and the guarantees where you need those (sequencing, branching, type safety).

---

Try it yourself:

```bash
npm install -g deepclause
deepclause run vuln-scanner.dml
```

The full DML reference is at [github.com/deepclause/deepclause-sdk](https://github.com/deepclause/deepclause-sdk). Star the repo, file issues, break things. We're building this in the open.
