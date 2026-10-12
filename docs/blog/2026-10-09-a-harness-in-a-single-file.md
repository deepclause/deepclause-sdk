# A Harness in a Single File: Agent Skills as REPL Modules

*(As usual, this is written by a human, code/AI-slop is on GitHub ;-)*

For a while now I've been somewhat bothered by the fact that DeepClause's DML language doesn't come with a REPL. While I was thinking a bit about how to add one, I quickly realized that it would be much more interesting to build a combined Agent/REPL kind of thing — especially in the context of RSI, that is "recursively self improving" harnesses (see references at the end).

So the idea became:

1. Build an agent that has only access to a REPL that can load and execute DML code.
2. Instruct the agent to use its access to the REPL to dynamically add, execute, and modify code until it can successfully solve a task.

There are a few points here that make DML (and its Prolog ancestry) very useful here:

- **State management:** Prolog is "functional" (actually it's more "relational", but that's another story…) and therefore stateless. The loaded predicates ("functions") define the entire state and can be easily inspected and modified on the fly. There are no global variables or references in the interpreter memory.
- **RLM-like structures:** Code written by the agent will be much more useful if we can adopt an RLM-like structure — that means the code can again call or orchestrate underlying LLM models or sub-agents. This is exactly what DML was designed for in the first place! We have predicates that trigger entire agentic loops, call decision models, or run a single prompt with fresh context. At the same time we have access to host tools (such as e.g. bash) and can reach them through DML code.
- **Transactional changes:** Another very interesting point is that the simplicity of the REPL state management allows one to easily implement transactions around changes to the code in the REPL. That gives the code-writing agent an easy way to test and potentially rollback changes without polluting the code and REPL state too much.

## An RLM-style harness defined in a single file

So how does it work? First, install DeepClause via npm, configure model/provider and start the REPL.

```bash
#> npm install -g deepclause-sdk
#> deepclause init
#> deepclause login
#> deepclause repl
```

You will then be greeted with a banner and a running REPL session:

<!-- screenshot: REPL banner / startup -->

Now you can directly enter a prompt or execute a DML/Prolog goal using e.g. `/goal member(X, [a,b,c,d]).`

Much more interesting is of course to start with a prompt. So let's enter "Build me a small coding agent!" and you will see output like this:

<!-- screenshot: authoring turn (begin_turn … end_turn) -->

Eventually, if you use a decent model, you can inspect what your harness looks like using `/listing` and you will see code like this:

```prolog
module_doc("Coding agent: read, search, and edit a repo and run its tests.").

% --- data = code: facts the agent can query and reuse ---
doc(project/1, "Known projects in this workspace.").
project(payments).
project(checkout).

% --- tools: predicates, plus a tool/2 annotation that exposes them to task/N ---
run_shell(Command, Output) :-
    exec(bash(command: Command), Result),
    ( get_dict(stdout, Result, Out) -> Output = Out ; Output = "" ).

fs_read(Path, Text) :- read_file(Path, Text).
fs_write(Path, Content) :- write_file(Path, Content).

tool(run_shell(Command, Output), "Run a shell command; returns stdout.") :- run_shell(Command, Output).
tool(fs_read(Path, Text), "Read a workspace file.") :- fs_read(Path, Text).
tool(fs_write(Path, Content), "Write a workspace file.") :- fs_write(Path, Content).

% --- skills: predicates. This one scopes the tools a model turn may call. ---
doc(patch/3, "Apply a minimal fix and run the tests. patch(+File, +Issue, -Summary).").
patch(File, Issue, Summary) :-
    system("Make the smallest change that fixes the issue. Run the project's tests."),
    with_tools([run_shell, fs_read, fs_write],
        task("Fix this issue in {File}: {Issue}. Then run the tests and report what changed.", Summary)).

% --- memory: the module can also read its own past ---
doc(recent_changes/2, "The last N committed changes.").
recent_changes(N, Changes) :-
    findall(V-Ts, harness_change(V, Ts, commit, _, _), All),
    last_n(All, N, Changes).
```

This is a snapshot of all the functionality that the harness provides at this point. But what does all this mean?

### Descriptions, annotations, instructions

The things you would normally spread across a system prompt, a tool schema, and a separate `SKILL.md` all live in the module:

- **`module_doc/1`** is the one-line description of the harness as a whole. `listing` prints it first as `skill: …`, and it is the natural thing to show a human (or another agent) when they ask what this module is for.
- **`doc/2`** attaches documentation to a single predicate: `doc(patch/3, "Apply a minimal fix and run the tests. patch(+File, +Issue, -Summary).")`. It shows up in `listing`, in the manifest and in `visualize`, so the docs and the code cannot drift apart the way a README usually does.
- **`tool/2`** is the tool schema. `tool(run_shell(Command, Output), "Run a shell command; returns stdout.") :- run_shell(Command, Output).` says "a nested agent may call `run_shell/2` with these arguments." It is an annotation, not an implementation — the clause just delegates to the real predicate. Un-annotated predicates are still callable by you via `/goal` or the authoring agent's `execute`, but they stay invisible to the model inside a `task/N`.
- **`system/1`** (and the `task`/`prompt` text) is where the instruction lives. In `patch/3`, the `system("Make the smallest change…")` line is the model's system prompt for that turn.

The point is that the file is self-describing. An agent that can read `module_doc`, `doc`, `tool` and the predicate sources already has everything it needs to use and modify the harness. There is no separate skill description to keep in sync and no tool JSON to hand-maintain.

### Ordinary code

Most of a good harness is not a model call. It is ordinary Prolog, and that is deliberate:

- **Deterministic logic:** pattern matching, recursion, `findall`, arithmetic, CLP(FD)/CLP(Q)/CLP(R) constraints, `format/2`.
- **Data = code:** small facts like `project(payments).` are queried and reused exactly like predicates.
- **Glue:** reading a shell result, choosing a branch, formatting an answer. `run_shell/2` above is pure code that happens to call a host tool; `patch/3` is one model call wrapped in a system prompt and a scoped tool list.

This is where you push the fuzzy part to the edges. The model only sees the point where genuine judgment is needed; everything around it is a program you can read, test with `execute`, and re-run. Skills can also carry **tests** (`test_<name>/N`, or a small `check/2` runner), so a change is verified rather than assumed. `recent_changes/2` above is a tiny example: plain Prolog that reads the module's own commit history.

### Sub-agents

`task/N` is not a plain completion: it is a bounded agentic turn. It inherits the harness's conversation memory, and it can call any `tool/2`-annotated predicate (subject to `with_tools/2`). Because a `task/N` may call a predicate that itself contains a `task/N`, skills compose into sub-agents — the harness can orchestrate a router, a planner, a verifier or a worker, all as ordinary predicates.

- **`task/N`** — model turn with the current memory; may call the exposed tools and, through them, other skills.
- **`prompt/N`** — the isolated variant: a single model call with *fresh* memory, no shared conversation. Good for classification, extraction, or a clean second opinion.
- **`with_tools/2`** — scopes the tool set for one turn. `patch/3` gets `run_shell`, `fs_read`, `fs_write`; a read-only `explain/2` skill would get only the readers. Least privilege, expressed in the same clause that uses it.
- **Semantic judgments** — `choose/3`, `rate/3`, `verify/3`, `probability/3` (and `judge/2`) are typed "model-as-classifier" primitives you can drop into the middle of a Prolog rule; they map to a judge backend and return typed values.

So `patch/3` is already a sub-agent: a system prompt, a scoped tool set and a task. If you later want a "review the patch" step, you add another predicate and call it — or point a `verify/3` at its output. The library grows by composition, and every piece stays inspectable.

## Exporting and importing a harness

The REPL's living module is not a special runtime-only format. `/export` serializes it to a plain, self-contained `.dml` file:

```text
dml> /export coding_agent.dml
exported 14 clauses, 4 tools, 0 scripts → coding_agent.dml
```

The export is deterministic: directives first, then tools, then embedded scripts, then user predicates sorted by `name/arity`, with variables renamed `A`, `B`, … so that `export → load → export` is byte-stable. That makes a harness a normal versioned artifact: commit it, diff it, mail it.

To import it:

```bash
deepclause repl -l coding_agent.dml          # load and continue interactively
deepclause repl -l coding_agent.dml -f       # load frozen (read-only)
deepclause run coding_agent.dml --goal 'patch("src/cli.ts", "…", S)'
```

Loading a file targets the harness named after it (`coding_agent.dml` → harness `coding_agent`), creating it if it does not exist. The loaded file becomes that harness's **baseline**, so it works with no prior history and `/rollback` floors at the imported version — it will never roll back past the file you loaded. A project can hold several harnesses side by side (`--harness <name>`), each with its own content and version history. Use a `.zip` target to bundle linked `script_file/2` scripts into one portable file.

The practical consequence: a harness is a portable unit of agent behavior. You can keep one per model, per task, or per team, check improvements into git, and hand someone a single file that *is* the agent.

## Advantages and use cases

Why bother with a single-file harness instead of a prompt plus a pile of skills?

- **Model-specific harnesses.** Different models need different amounts of scaffolding. A strong model can be given a fairly autonomous loop; a small or local model does much better with a short, explicit workflow and a single model call at the end. Since the harness is text, you can keep a tuned harness per model and compare them on the same tasks.
- **Task-specific harnesses.** Triage, coding, data cleaning, literature review — each is a module. They can share helper predicates and compose, and a project can hold several at once.
- **Continuous optimization.** A harness is a mutable artifact with a version history and a rollback. You can change one predicate, test it, measure it against a small task set, and keep the change only if it helps. This is exactly the bounded *edit → verify → keep* loop the recent self-improvement literature converges on (see references), with the difference that the object being edited is one readable file.
- **Cost, latency and determinism.** Push everything that doesn't need judgment into Prolog. You call the model only where it earns its keep, which is cheaper, faster and far more reproducible across runs and machines.
- **Auditability and review.** Every change is a diff with a version; every capability is a predicate; the tools a model may use are declared in `with_tools/2`. For anything that has to be reviewed — regulated workflows especially — a small, inspectable module beats a prompt.
- **Portability and sharing.** One file, no registry: import, fork, re-export.

A few honest caveats. A single-file harness does not make the model smarter, and a "relevant" skill does not automatically improve results — recent empirical work finds that utility depends heavily on content and configuration, sometimes not at all. Optimizing a harness needs a decent objective and evaluation set, and it is easy to overfit a small benchmark. And the module is not a sandbox: its own guardrails live inside the code the agent can edit, so the hard boundaries (workspace confinement, approval-gated host tools) have to stay outside it. Within those limits, though, making the harness a single inspectable file is a surprisingly useful shape — and a convenient place to point a self-improvement loop.

If you want to try it:

```bash
npm install -g deepclause-sdk
mkdir my-agent && cd my-agent
deepclause init
deepclause repl
```

Then ask for a harness, query it with `/goal`, and `/export` the result.

## References

- *Recursive Self-Improvement in AI: From Bounded Self-Refinement to Autonomous Research Loops* — arXiv:2607.07663 (survey).
- MESH-Harness: *Self-Improving Agent Harnesses via Bandit-Guided Compositional Evolution* — arXiv:2610.05300.
- *The Harness as the Only Mutable Surface: Compliance-Bounded Self-Evolution of LLM Agents, with a Measured Admission Gate* — arXiv:2610.10629.
- *SAGE: A Statistical Acceptance Gate for Self-Evolving Agents* — arXiv:2609.36043.
- *SEABench: Benchmarking Endogenous Misalignment in Self-Evolving Agents* — arXiv:2609.35596.
- *AgentEvolver: System-Wide Self-Evolution Through Task Execution* — arXiv:2610.11613.
- *Skill-V: Verifiable Self-Evolving Skill Library for Interactive Agents* — arXiv:2610.11781.
- *SkillForge: Co-Evolving Skills and Agents via Dynamic Skill Lifecycles* — arXiv:2610.09832.
- *SelfSearch: Reward-Free Search for Self-Improving Agents* — arXiv:2609.37968.
- *An Empirical Study of Agent Skills' Downstream Utility* — arXiv:2610.08875.
- *Which Self-Improvements Should We Trust? Reliable Self-Improvement When Agents Reuse Their Benchmarks* — arXiv:2609.33180.
- *Verification and Self-Improvement in Agentic AI: Foundations and Limits* — arXiv:2610.10611.
- *Layered Mutability: Continuity and Governance in Persistent Self-Modifying Agents* — arXiv:2604.14717.

---

*Previous post: [DeepClause "Turbo DML" Agent Harness](https://deepclause.substack.com/p/deepclause-turbo-dml-agent-harness).*
