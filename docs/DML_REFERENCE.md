# DML Language Reference

**DML** (DeepClause Meta Language) is a simplified Prolog dialect for programming AI agents. It combines declarative logic programming with LLM-powered task execution.

## Table of Contents

- [Program Structure](#program-structure)
- [Entry Point](#entry-point)
- [Core Predicates](#core-predicates)
  - [task/1, task/2, task/3, task/4](#task-predicates)
  - [exec/2](#exec2)
  - [system/1, user/1](#memory-predicates)
  - [output/1, yield/1, log/1](#output-predicates)
  - [answer/1](#answer1)
  - [param/3](#param3)
- [Control Flow](#control-flow)
- [String Interpolation](#string-interpolation)
- [Backtracking](#backtracking)
- [Full Prolog Support](#full-prolog-support)

---

## Program Structure

A DML program is a collection of Prolog clauses. Comments use `%` for single lines or `/* */` for blocks:

```prolog
% This is a single line comment

/* 
   This is a 
   multi-line comment 
*/

% Define helper predicates
greet(Name, Greeting) :-
    format(string(Greeting), "Hello, ~w!", [Name]).

% Main entry point
agent_main :-
    greet("World", Msg),
    answer(Msg).
```

---

## Entry Point

Every DML program must define `agent_main` as the entry point. It can have 0, 1, or 2 arguments:

### agent_main/0
```prolog
agent_main :-
    task("Do something"),
    answer("Done").
```

### agent_main/1
Receives the first parameter:
```prolog
agent_main(Name) :-
    format(string(Msg), "Hello, ~w!", [Name]),
    answer(Msg).
```

### agent_main/2
Receives first two parameters:
```prolog
agent_main(Name, Task) :-
    system("You are a helpful assistant."),
    task(Task),
    answer("Complete").
```

---

## Core Predicates

### Task Predicates

#### `task(+Description)`

Invoke an LLM to perform a task. The description is sent to the model.

```prolog
agent_main :-
    system("You are a coding assistant."),
    task("Write a function to calculate factorial in Python."),
    answer("Code generated!").
```

#### `task(+Description, -Var1)`

Invoke LLM and bind output to a variable. Mention the variable name in the description:

```prolog
agent_main :-
    task("Calculate 2 + 2 and store the result in Answer.", Answer),
    format(string(Msg), "The answer is: ~w", [Answer]),
    answer(Msg).
```

#### `task(+Description, -Var1, -Var2)`

Bind multiple variables:

```prolog
agent_main :-
    task("Generate a name and age for a character. Store name in Name and age in Age.", Name, Age),
    format(string(Msg), "Character: ~w, Age: ~w", [Name, Age]),
    answer(Msg).
```

#### `task(+Description, -Var1, -Var2, -Var3)`

Bind up to three variables.

**Note:** The LLM during `task()` can only call tools defined in DML (see [Tool Definitions](#tool-definitions)). Registered TypeScript tools are not automatically available - this gives you control over what the LLM can do.

---

### `exec(+ToolCall, -Output)`

Execute a registered TypeScript tool directly and capture its output.

```prolog
% Assuming 'calculator' tool is registered in TypeScript
agent_main :-
    exec(calculator(expression: "15 * 23"), Result),
    format(string(Msg), "Calculation result: ~w", [Result]),
    answer(Msg).
```

Tool arguments use `key: value` syntax:

```prolog
exec(fetch_url(url: "https://example.com"), Response)
```

Or positional if the tool supports it:

```prolog
exec(get_time, CurrentTime)
```

---

### Tool Definitions

Define tools in DML that the LLM can call during `task()`. Tools wrap `exec/2` calls to registered TypeScript tools:

```prolog
% Declare tool schema - makes it available to the LLM
:- tool(calculate(Expression), "Calculate a mathematical expression").

% Implement the tool - called when LLM uses it
tool(calculate(Expression, Result)) :-
    exec(calculator(expression: Expression), Result).

agent_main :-
    system("You have a calculate tool."),
    task("What is the square root of 144?"),
    answer("Done").
```

Tool declarations have the form:
```prolog
:- tool(name(Arg1, Arg2, ...), "Description").
```

Tool implementations match:
```prolog
tool(name(Arg1, Arg2, ..., Result)) :- ...
```

You can add Prolog logic in tool implementations:
```prolog
:- tool(safe_divide(A, B), "Safely divide two numbers").

tool(safe_divide(A, B, Result)) :-
    (   B =:= 0
    ->  Result = error("Division by zero")
    ;   exec(calculator(expression: A/B), Result)
    ).
```

#### Using `task()` Inside Tools

Tools can use `task()` and `prompt()` internally to combine Prolog logic with LLM reasoning:

```prolog
% A tool that computes then explains
:- tool(explain_calculation(A, B), "Calculate and explain the result").

tool(explain_calculation(A, B, Explanation)) :-
    Sum is A + B,  % Prolog computation
    format(string(Desc), "Explain ~w + ~w = ~w to a child", [A, B, Sum]),
    task(Desc, Explanation).  % LLM explanation

% A tool that uses LLM for formatting
:- tool(format_data(Data), "Format data nicely using LLM").

tool(format_data(Data, FormattedOutput)) :-
    format(string(Desc), "Format this data nicely: ~w", [Data]),
    task(Desc, FormattedOutput).
```

**Important:** When `task()` runs inside a tool, the nested agent does NOT have access to DML tools (to prevent infinite recursion). It only has access to `finish` and `set_result`.

---

### Memory Predicates

Memory predicates build the conversation context sent to the LLM.

#### `system(+Text)`

Add a system message (instructions for the LLM):

```prolog
agent_main :-
    system("You are a helpful cooking assistant."),
    system("Always provide measurements in metric units."),
    task("Suggest a recipe for pasta."),
    answer("Done").
```

#### `user(+Text)`

Add a user message to the context:

```prolog
agent_main :-
    system("You are a helpful assistant."),
    user("I want to learn about machine learning."),
    task("Explain the basics of machine learning."),
    answer("Explained!").
```

---

### Output Predicates

#### `output(+Text)`

Emit an output event to the caller. Does NOT affect the LLM conversation. Useful for progress updates:

```prolog
agent_main :-
    output("Starting analysis..."),
    task("Analyze the data"),
    output("Analysis complete!"),
    answer("Done").
```

#### `yield(+Text)`

Alias for `output/1`.

#### `log(+Text)`

Emit a log event. Similar to output but typed as 'log' for filtering:

```prolog
agent_main :-
    log("Debug: entering main"),
    task("Process request"),
    log("Debug: task completed"),
    answer("Done").
```

---

### `answer(+Text)`

Emit the final answer and **commit** - no backtracking occurs after `answer/1`:

```prolog
agent_main :-
    task("Generate a poem"),
    answer("Poem complete!").  % Execution stops here
```

**Important:** `answer/1` commits the execution. After answering, the program completes and does not try alternative clauses.

---

### `param(+Key, +Description, -Value)`

Access parameters passed from the caller:

```prolog
agent_main :-
    param(name, "User's name", Name),
    param(age, "User's age", Age),
    format(string(Msg), "Hello ~w, you are ~w years old!", [Name, Age]),
    answer(Msg).
```

Called from TypeScript:
```typescript
dc.runDML(code, { params: { name: 'Alice', age: 30 } });
```

---

## Control Flow

DML supports standard Prolog control flow:

### Conjunction (and)
```prolog
goal1, goal2, goal3
```

### Disjunction (or)
```prolog
(goal1 ; goal2)
```

### If-Then-Else
```prolog
(Condition -> Then ; Else)
```

### If-Then (soft cut)
```prolog
(Condition -> Then)
```

### Negation as Failure
```prolog
\+ goal
```

### Cut
```prolog
!
```

### Catch/Throw
```prolog
catch(Goal, Error, Recovery)
throw(Error)
```

---

## String Interpolation

Text arguments support `{{variable}}` interpolation with parameters:

```prolog
agent_main :-
    param(topic, "Research topic", Topic),
    system("You are a research assistant."),
    task("Research the topic: {{topic}}"),  % Topic is interpolated
    answer("Research on {{topic}} complete!").
```

---

## Backtracking

DML supports full Prolog backtracking, **including across LLM calls**. This enables powerful retry logic:

### Multiple Clauses

```prolog
% Try first approach
agent_main :-
    output("Trying approach 1..."),
    task("Solve using method A"),
    fail.  % Force failure to try next clause

% Fallback approach
agent_main :-
    output("Trying approach 2..."),
    task("Solve using method B"),
    answer("Solved!").
```

### State Restoration

When backtracking occurs, the memory state is automatically restored:

```prolog
agent_main :-
    system("Context A"),  % This is in memory
    output("Trying path A"),
    fail.  % Memory is rolled back!

agent_main :-
    system("Context B"),  % Fresh memory
    output("Trying path B"),
    answer("Done").
```

### Disjunction Backtracking

```prolog
agent_main :-
    (
        task("Try difficult approach"),
        fail  % Backtrack
    ;
        task("Try easier approach")
    ),
    answer("Completed").
```

---

## Full Prolog Support

DML is built on SWI-Prolog, so you have access to the full Prolog language:

### Lists
```prolog
process_items([]).
process_items([H|T]) :-
    format(string(Msg), "Processing: ~w", [H]),
    output(Msg),
    process_items(T).

agent_main :-
    process_items([apple, banana, cherry]),
    answer("All items processed").
```

### Arithmetic
```prolog
agent_main :-
    X is 10 + 5 * 2,
    format(string(Msg), "Result: ~w", [X]),
    answer(Msg).
```

### String Formatting
```prolog
agent_main :-
    format(string(S), "Hello ~w, you are ~d years old", [alice, 30]),
    answer(S).
```

### Findall
```prolog
color(red).
color(green).
color(blue).

agent_main :-
    findall(C, color(C), Colors),
    format(string(Msg), "Colors: ~w", [Colors]),
    answer(Msg).
```

### Assert/Retract (within session)
```prolog
agent_main :-
    assertz(fact(42)),
    fact(X),
    format(string(Msg), "Stored fact: ~w", [X]),
    answer(Msg).
```

### Dictionaries (SWI-Prolog)
```prolog
agent_main :-
    D = point{x: 10, y: 20},
    X = D.x,
    format(string(Msg), "X coordinate: ~w", [X]),
    answer(Msg).
```

---

## Complete Example

```prolog
% research_assistant.dml
% A multi-step research assistant

% Helper to log progress
step(N, Desc) :-
    format(string(Msg), "Step ~w: ~w", [N, Desc]),
    output(Msg).

% Main entry point
agent_main :-
    param(topic, "Research topic", Topic),
    
    % Set up the assistant
    system("You are a thorough research assistant."),
    system("Provide detailed, accurate information."),
    
    % Step 1: Overview
    step(1, "Getting overview"),
    format(string(Task1), "Provide a brief overview of ~w", [Topic]),
    task(Task1),
    
    % Step 2: Key concepts
    step(2, "Identifying key concepts"),
    task("List the 3 most important concepts related to this topic. Store them in Concepts.", Concepts),
    
    % Step 3: Deep dive
    step(3, "Deep dive analysis"),
    format(string(Task3), "Explain these concepts in detail: ~w", [Concepts]),
    task(Task3),
    
    % Complete
    format(string(FinalMsg), "Research on '~w' complete!", [Topic]),
    answer(FinalMsg).
```

Run with:
```typescript
for await (const event of dc.runDML(code, { 
  params: { topic: 'quantum computing' } 
})) {
  console.log(event.type, event.content);
}
```
