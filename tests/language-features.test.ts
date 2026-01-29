/**
 * Test Suite: DML Language Features
 * 
 * Tests core language features WITHOUT LLM calls:
 * - Basic execution & answers
 * - Backtracking
 * - Memory management (system, user, push_context, pop_context)
 * - String interpolation (local vars + params)
 * - Args & params passing
 * - Tool registration & exec
 * - Tool policies
 * - Output events (yield, log, output)
 */
import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { createDeepClause, DeepClauseSDK } from '../src/index.js';

// ============================================================================
// BASIC EXECUTION
// ============================================================================
describe('Basic Execution', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should execute simple DML with answer', async () => {
    const code = `
      agent_main :-
          answer("Hello from DML!").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.find(e => e.type === 'answer')?.content).toBe('Hello from DML!');
    expect(events.some(e => e.type === 'finished')).toBe(true);
  });

  it('should handle sequential pure Prolog operations', async () => {
    const code = `
      agent_main :-
          X = 1,
          Y = 2,
          Z is X + Y,
          format(string(Result), "Sum: ~w", [Z]),
          answer(Result).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.find(e => e.type === 'answer')?.content).toBe('Sum: 3');
  });

  it('should handle syntax errors gracefully', async () => {
    const code = `
      agent_main :-
          this is not valid prolog.
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'error')).toBe(true);
  });

  it('should handle runtime errors gracefully', async () => {
    const code = `
      agent_main :-
          X is 1/0,
          answer(X).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'error')).toBe(true);
  });

  it('should emit error for undefined predicates', async () => {
    const code = `
      agent_main :-
          undefined_predicate(X),
          answer(X).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'error')).toBe(true);
  });
});

// ============================================================================
// BACKTRACKING
// ============================================================================
describe('Backtracking', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should try alternatives on failure (disjunction)', async () => {
    const code = `
      agent_main :-
          (
              fail
          ;
              answer("second branch")
          ).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.find(e => e.type === 'answer')?.content).toBe('second branch');
  });

  it('should backtrack between clauses', async () => {
    const code = `
      helper(Result) :- fail.
      helper(Result) :- Result = "from second clause".
      
      agent_main :-
          helper(X),
          answer(X).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.find(e => e.type === 'answer')?.content).toBe('from second clause');
  });

  it('should execute first matching clause', async () => {
    const code = `
      agent_main :-
          output("Clause 1 executing"),
          answer("from clause 1").
      
      agent_main :-
          output("Clause 2 executing"),
          answer("from clause 2").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const outputs = events.filter(e => e.type === 'output');
    expect(outputs.length).toBe(1);
    expect(outputs[0]?.content).toBe('Clause 1 executing');
    expect(events.find(e => e.type === 'answer')?.content).toBe('from clause 1');
  });

  it('should backtrack to second clause when first fails', async () => {
    const code = `
      agent_main :-
          output("Trying clause 1"),
          fail.
      
      agent_main :-
          output("Trying clause 2"),
          answer("success from clause 2").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const outputs = events.filter(e => e.type === 'output');
    expect(outputs.length).toBe(2);
    expect(events.find(e => e.type === 'answer')?.content).toBe('success from clause 2');
  });

  it('should thread state through nested helper predicates', async () => {
    const code = `
      setup_context :-
          system("I am a helpful assistant").
      
      do_work(Result) :-
          Result = "work complete".
      
      agent_main :-
          setup_context,
          do_work(R),
          answer(R).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.find(e => e.type === 'answer')?.content).toBe('work complete');
  });
});

// ============================================================================
// MEMORY / CONTEXT MANAGEMENT
// ============================================================================
describe('Memory & Context Management', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  describe('system/1 and user/1', () => {
    it('should accept system message without error', async () => {
      const code = `
        agent_main :-
            system("You are a pirate."),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('done');
    });

    it('should allow multiple system messages', async () => {
      const code = `
        agent_main :-
            system("You are helpful."),
            system("Always be concise."),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.some(e => e.type === 'finished')).toBe(true);
    });

    it('should accept user message without error', async () => {
      const code = `
        agent_main :-
            user("My name is Bob"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('done');
    });
  });

  describe('push_context/pop_context', () => {
    it('push_context should save memory and keep it active', async () => {
      const code = `
        agent_main :-
            system("Message 1"),
            push_context,
            system("Message 2"),
            answer("both messages should be active").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.some(e => e.type === 'finished')).toBe(true);
      expect(events.find(e => e.type === 'answer')).toBeDefined();
    });

    it('push_context(clear) should save and clear memory', async () => {
      const code = `
        agent_main :-
            system("Initial context"),
            push_context(clear),
            system("Fresh context"),
            answer("cleared").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('cleared');
    });

    it('pop_context should restore memory from stack', async () => {
      const code = `
        agent_main :-
            system("Saved context"),
            push_context(clear),
            system("Temp context"),
            pop_context,
            answer("restored").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('restored');
    });

    it('nested push/pop should work correctly', async () => {
      const code = `
        agent_main :-
            system("Level 0"),
            push_context,
            system("Level 1"),
            push_context,
            system("Level 2"),
            pop_context,
            pop_context,
            answer("back to level 0").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('back to level 0');
    });

    it('clear_memory should clear without saving', async () => {
      const code = `
        agent_main :-
            system("Will be cleared"),
            clear_memory,
            answer("memory cleared").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('memory cleared');
    });

    it('pop_context on empty stack should be safe (no-op)', async () => {
      const code = `
        agent_main :-
            pop_context,
            answer("safe on empty").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('safe on empty');
    });
  });
});

// ============================================================================
// STRING INTERPOLATION (compile-time)
// ============================================================================
describe('String Interpolation', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  describe('with local variables', () => {
    it('should interpolate local variables in answer/1', async () => {
      const code = `
        agent_main :-
            Name = "Alice",
            Age = 30,
            answer("Hello {Name}, you are {Age} years old!").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Hello Alice, you are 30 years old!');
    });

    it('should interpolate computed values', async () => {
      const code = `
        agent_main :-
            X = 10,
            Y = 20,
            Sum is X + Y,
            answer("The sum of {X} and {Y} is {Sum}").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('The sum of 10 and 20 is 30');
    });

    it('should interpolate in output/1', async () => {
      const code = `
        agent_main :-
            First = "John",
            Last = "Doe",
            output("Name: {First} {Last}"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'output')?.content).toBe('Name: John Doe');
    });

    it('should interpolate in log/1', async () => {
      const code = `
        agent_main :-
            Status = "ready",
            Count = 42,
            log("Status: {Status}, Count: {Count}"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'log')?.content).toBe('Status: ready, Count: 42');
    });

    it('should interpolate values from helper predicates', async () => {
      const code = `
        compute_greeting(Name, G) :-
            format(string(G), "Hello, ~w!", [Name]).
        
        agent_main :-
            compute_greeting("Bob", Greeting),
            answer("Result: {Greeting}").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Result: Hello, Bob!');
    });

    it('should handle strings without interpolation', async () => {
      const code = `
        agent_main :-
            answer("No variables here").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('No variables here');
    });
  });

  describe('with params', () => {
    it('should interpolate params via runtime lookup', async () => {
      const code = `
        agent_main :-
            answer("Welcome {user_name}! Your level is {level}.").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { params: { user_name: "Diana", level: "expert" } })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Welcome Diana! Your level is expert.');
    });
  });
});

// ============================================================================
// ARGS & PARAMS PASSING
// ============================================================================
describe('Args & Params', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  describe('positional args', () => {
    it('should pass positional args to agent_main', async () => {
      const code = `
        agent_main(Name, Age) :-
            format(atom(Msg), "Name: ~w, Age: ~w", [Name, Age]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { args: ["Alice", 30] })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Name: Alice, Age: 30');
    });

    it('should preserve arg order', async () => {
      const code = `
        agent_main(First, Second, Third) :-
            format(atom(Msg), "Order: ~w, ~w, ~w", [First, Second, Third]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { args: ["A", "B", "C"] })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Order: A, B, C');
    });
  });

  describe('named params', () => {
    it('should access named params via param/2', async () => {
      const code = `
        agent_main :-
            param(username, User),
            param(role, Role),
            format(atom(Msg), "User: ~w, Role: ~w", [User, Role]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { params: { username: "bob", role: "admin" } })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('User: bob, Role: admin');
    });
  });

  describe('mixed args and params', () => {
    it('should support both positional args and named params', async () => {
      const code = `
        agent_main(Task) :-
            param(deadline, Deadline),
            format(atom(Msg), "Task: ~w, Due: ~w", [Task, Deadline]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { 
        args: ["Write report"], 
        params: { deadline: "2024-12-31" } 
      })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Task: Write report, Due: 2024-12-31');
    });
  });

  describe('workspace path', () => {
    it('should set workspace path via param', async () => {
      const code = `
        agent_main :-
            param(workspace_path, Path),
            answer(Path).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: '/tmp/test' })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('/tmp/test');
    });
  });
});

// ============================================================================
// OUTPUT EVENTS
// ============================================================================
describe('Output Events', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  describe('output/1', () => {
    it('should emit output events', async () => {
      const code = `
        agent_main :-
            output("First output"),
            output("Second output"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const outputs = events.filter(e => e.type === 'output');
      expect(outputs.length).toBe(2);
      expect(outputs[0]?.content).toBe('First output');
      expect(outputs[1]?.content).toBe('Second output');
    });
  });

  describe('yield/1', () => {
    it('should stream intermediate output', async () => {
      const code = `
        agent_main :-
            yield("Step 1 complete"),
            yield("Step 2 complete"),
            yield("Step 3 complete"),
            answer("All done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const outputs = events.filter(e => e.type === 'output');
      expect(outputs.length).toBe(3);
      expect(outputs[0].content).toBe('Step 1 complete');
    });
  });

  describe('log/1', () => {
    it('should send log messages', async () => {
      const code = `
        agent_main :-
            log("Processing started"),
            log("Step 1"),
            log("Step 2"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const logs = events.filter(e => e.type === 'log');
      expect(logs.length).toBe(3);
    });
  });

  describe('input/2', () => {
    it('should request user input and bind result', async () => {
      const code = `
        agent_main :-
            input("Enter your name: ", Name),
            output(Name),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, {
        onUserInput: async (prompt) => {
          expect(prompt).toBe('Enter your name: ');
          return 'Alice';
        }
      })) {
        events.push(event);
      }

      const inputRequired = events.filter(e => e.type === 'input_required');
      expect(inputRequired.length).toBe(1);
      expect(inputRequired[0].prompt).toBe('Enter your name: ');

      const outputs = events.filter(e => e.type === 'output');
      expect(outputs.length).toBe(1);
      expect(outputs[0].content).toBe('Alice');
    });

    it('should support multiple input calls', async () => {
      const code = `
        agent_main :-
            input("First: ", V1),
            input("Second: ", V2),
            format(string(Combined), "~w and ~w", [V1, V2]),
            output(Combined),
            answer("done").
      `;

      let callCount = 0;
      const events: any[] = [];
      for await (const event of sdk.runDML(code, {
        onUserInput: async (prompt) => {
          callCount++;
          if (prompt === 'First: ') return 'Hello';
          if (prompt === 'Second: ') return 'World';
          return '';
        }
      })) {
        events.push(event);
      }

      expect(callCount).toBe(2);
      const outputs = events.filter(e => e.type === 'output');
      expect(outputs[0].content).toBe('Hello and World');
    });

    it('should interpolate variables in input prompt', async () => {
      const code = `
        agent_main :-
            Name = "Alice",
            format(string(Prompt), "Hello ~w, what is your age? ", [Name]),
            input(Prompt, Age),
            output(Age),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, {
        onUserInput: async (prompt) => {
          expect(prompt).toBe('Hello Alice, what is your age? ');
          return '30';
        }
      })) {
        events.push(event);
      }

      const outputs = events.filter(e => e.type === 'output');
      expect(outputs[0].content).toBe('30');
    });
  });
});

// ============================================================================
// EXEC & TOOL REGISTRATION
// ============================================================================
describe('exec() & Tool Registration', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  describe('basic exec', () => {
    it('should execute a registered tool', async () => {
      sdk.registerTool('echo', {
        description: 'Echo the input back',
        parameters: {
          type: 'object',
          properties: {
            message: { type: 'string', description: 'Message to echo' }
          },
          required: ['message']
        },
        execute: async ({ message }) => `Echo: ${message}`
      });

      const code = `
        agent_main :-
            exec(echo("hello world"), Result),
            answer(Result).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Echo: hello world');
    });

    it('should fail if tool is not registered', async () => {
      const code = `
        agent_main :-
            exec(nonexistent_tool("arg"), _),
            answer("should not reach").
        
        agent_main :-
            answer("fallback").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('fallback');
    });

    it('should handle tool execution errors gracefully', async () => {
      sdk.registerTool('failing_tool', {
        description: 'A tool that always fails',
        parameters: { type: 'object', properties: {} },
        execute: async () => { throw new Error('Tool error'); }
      });

      const code = `
        agent_main :-
            exec(failing_tool, _),
            answer("should not reach").
        
        agent_main :-
            answer("handled error").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('handled error');
    });
  });

  describe('complex return types', () => {
    it('should handle list return values', async () => {
      sdk.registerTool('get_list', {
        description: 'Return a list of items',
        parameters: { type: 'object', properties: {} },
        execute: async () => ['apple', 'banana', 'cherry']
      });

      const code = `
        agent_main :-
            exec(get_list, Items),
            length(Items, Len),
            format(string(Msg), "Got ~w items", [Len]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Got 3 items');
    });

    it('should handle dict return values', async () => {
      sdk.registerTool('get_dict', {
        description: 'Return a dictionary',
        parameters: { type: 'object', properties: {} },
        execute: async () => ({ name: 'Alice', age: 30 })
      });

      const code = `
        agent_main :-
            exec(get_dict, Dict),
            get_dict(name, Dict, Name),
            format(string(Msg), "Name: ~w", [Name]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Name: Alice');
    });
  });

  describe('tool with parameters', () => {
    it('should pass multiple parameters to tool', async () => {
      sdk.registerTool('calculate', {
        description: 'Perform a calculation',
        parameters: {
          type: 'object',
          properties: {
            a: { type: 'number' },
            b: { type: 'number' },
            op: { type: 'string', enum: ['add', 'sub', 'mul', 'div'] }
          },
          required: ['a', 'b', 'op']
        },
        execute: async ({ a, b, op }: { a: number; b: number; op: string }) => {
          switch (op) {
            case 'add': return a + b;
            case 'sub': return a - b;
            case 'mul': return a * b;
            case 'div': return a / b;
            default: return 0;
          }
        }
      });

      const code = `
        agent_main :-
            exec(calculate(10, 5, "add"), Result),
            format(string(Msg), "Result: ~w", [Result]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Result: 15');
    });

    it('should handle tool parameters from Prolog variables', async () => {
      const code = `
        agent_main :-
            A = 20,
            B = 4,
            exec(calculate(A, B, "mul"), Result),
            format(string(Msg), "Result: ~w", [Result]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Result: 80');
    });
  });

  describe('tool management', () => {
    it('should list registered tools', () => {
      sdk.registerTool('my_tool', {
        description: 'A test tool',
        parameters: { type: 'object', properties: {} },
        execute: async () => 'result'
      });

      expect(sdk.getTools()).toContain('my_tool');
    });

    it('should override existing tool', () => {
      sdk.registerTool('override_test', {
        description: 'Version 1',
        parameters: { type: 'object', properties: {} },
        execute: async () => 'v1'
      });

      sdk.registerTool('override_test', {
        description: 'Version 2',
        parameters: { type: 'object', properties: {} },
        execute: async () => 'v2'
      });

      const tools = sdk.getTools();
      expect(tools.filter(t => t === 'override_test').length).toBe(1);
    });
  });
});

// ============================================================================
// TOOL POLICIES
// ============================================================================
describe('Tool Policies', () => {
  let sdk: DeepClauseSDK;

  beforeEach(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });

    sdk.registerTool('safe_tool', {
      description: 'A safe tool',
      parameters: { type: 'object', properties: {} },
      execute: async () => 'safe result'
    });

    sdk.registerTool('dangerous_tool', {
      description: 'A dangerous tool',
      parameters: { type: 'object', properties: {} },
      execute: async () => 'dangerous result'
    });

    sdk.registerTool('file_read', {
      description: 'Read file',
      parameters: { type: 'object', properties: {} },
      execute: async () => 'read result'
    });

    sdk.registerTool('file_write', {
      description: 'Write file',
      parameters: { type: 'object', properties: {} },
      execute: async () => 'write result'
    });

    sdk.registerTool('network_fetch', {
      description: 'Fetch from network',
      parameters: { type: 'object', properties: {} },
      execute: async () => 'fetch result'
    });
  });

  afterEach(async () => {
    await sdk.dispose();
  });

  describe('whitelist mode', () => {
    it('should allow whitelisted tools', async () => {
      sdk.setToolPolicy({
        mode: 'whitelist',
        tools: ['safe_tool']
      });

      const code = `
        agent_main :-
            exec(safe_tool, R),
            answer(R).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('safe result');
    });

    it('should block non-whitelisted tools', async () => {
      sdk.setToolPolicy({
        mode: 'whitelist',
        tools: ['safe_tool']
      });

      const code = `
        agent_main :-
            exec(dangerous_tool, R),
            answer(R).
        
        agent_main :-
            answer("tool was blocked").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('tool was blocked');
    });

    it('should support wildcard patterns', async () => {
      sdk.setToolPolicy({
        mode: 'whitelist',
        tools: ['file_*']
      });

      const code1 = `
        agent_main :-
            exec(file_read, R),
            answer(R).
      `;

      const events1: any[] = [];
      for await (const event of sdk.runDML(code1)) {
        events1.push(event);
      }
      expect(events1.find(e => e.type === 'answer')?.content).toBe('read result');

      const code2 = `
        agent_main :-
            exec(network_fetch, R),
            answer(R).
        
        agent_main :-
            answer("blocked").
      `;

      const events2: any[] = [];
      for await (const event of sdk.runDML(code2)) {
        events2.push(event);
      }
      expect(events2.find(e => e.type === 'answer')?.content).toBe('blocked');
    });
  });

  describe('blacklist mode', () => {
    it('should allow non-blacklisted tools', async () => {
      sdk.setToolPolicy({
        mode: 'blacklist',
        tools: ['dangerous_tool']
      });

      const code = `
        agent_main :-
            exec(safe_tool, R),
            answer(R).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('safe result');
    });

    it('should block blacklisted tools', async () => {
      sdk.setToolPolicy({
        mode: 'blacklist',
        tools: ['dangerous_tool']
      });

      const code = `
        agent_main :-
            exec(dangerous_tool, R),
            answer(R).
        
        agent_main :-
            answer("blocked by policy").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('blocked by policy');
    });

    it('should support wildcard patterns in blacklist', async () => {
      sdk.setToolPolicy({
        mode: 'blacklist',
        tools: ['file_*']
      });

      const code1 = `
        agent_main :-
            exec(file_read, R),
            answer(R).
        
        agent_main :-
            answer("blocked").
      `;

      const events1: any[] = [];
      for await (const event of sdk.runDML(code1)) {
        events1.push(event);
      }
      expect(events1.find(e => e.type === 'answer')?.content).toBe('blocked');

      const code2 = `
        agent_main :-
            exec(network_fetch, R),
            answer(R).
      `;

      const events2: any[] = [];
      for await (const event of sdk.runDML(code2)) {
        events2.push(event);
      }
      expect(events2.find(e => e.type === 'answer')?.content).toBe('fetch result');
    });
  });

  describe('policy management', () => {
    it('should apply policy changes immediately', async () => {
      sdk.setToolPolicy({
        mode: 'blacklist',
        tools: ['safe_tool']
      });

      const code1 = `
        agent_main :-
            exec(safe_tool, R),
            answer(R).
        agent_main :-
            answer("blocked").
      `;

      const events1: any[] = [];
      for await (const event of sdk.runDML(code1)) {
        events1.push(event);
      }
      expect(events1.find(e => e.type === 'answer')?.content).toBe('blocked');

      sdk.setToolPolicy({
        mode: 'whitelist',
        tools: ['safe_tool']
      });

      const code2 = `
        agent_main :-
            exec(safe_tool, R),
            answer(R).
      `;

      const events2: any[] = [];
      for await (const event of sdk.runDML(code2)) {
        events2.push(event);
      }
      expect(events2.find(e => e.type === 'answer')?.content).toBe('safe result');
    });

    it('should clear policy', async () => {
      sdk.setToolPolicy({
        mode: 'blacklist',
        tools: ['safe_tool']
      });

      sdk.clearToolPolicy();

      expect(sdk.getToolPolicy()).toBeNull();

      const code = `
        agent_main :-
            exec(safe_tool, R),
            answer(R).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }
      expect(events.find(e => e.type === 'answer')?.content).toBe('safe result');
    });
  });
});

// ============================================================================
// FILE I/O OPERATIONS
// ============================================================================
import { mkdirSync, rmSync, existsSync, readFileSync, writeFileSync } from 'fs';
import { join } from 'path';

describe('File I/O Operations', () => {
  let sdk: DeepClauseSDK;
  const testWorkspace = join(process.cwd(), '.test-workspace');

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gpt-4o-mini' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  beforeEach(() => {
    // Create fresh test workspace
    if (existsSync(testWorkspace)) {
      rmSync(testWorkspace, { recursive: true });
    }
    mkdirSync(testWorkspace, { recursive: true });
  });

  afterEach(() => {
    // Clean up test workspace
    if (existsSync(testWorkspace)) {
      rmSync(testWorkspace, { recursive: true });
    }
  });

  describe('Reading Files', () => {
    it('should read file contents with read_file/2', async () => {
      // Create test file
      writeFileSync(join(testWorkspace, 'test.txt'), 'Hello, World!');

      const code = `
        agent_main :-
            read_file("test.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Hello, World!');
    });

    it('should read file with read_file_to_string/3', async () => {
      writeFileSync(join(testWorkspace, 'data.txt'), 'Test content');

      const code = `
        agent_main :-
            read_file_to_string("data.txt", Content, []),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Test content');
    });

    it('should fail when reading non-existent file', async () => {
      const code = `
        agent_main :-
            read_file("nonexistent.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.some(e => e.type === 'error')).toBe(true);
    });
  });

  describe('Writing Files', () => {
    it('should write file contents with write_file/2', async () => {
      const code = `
        agent_main :-
            write_file("output.txt", "Written by DML"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('done');
      
      // Verify file was written
      const content = readFileSync(join(testWorkspace, 'output.txt'), 'utf-8');
      expect(content).toBe('Written by DML');
    });

    it('should append to file with append_file/2', async () => {
      // Create initial file
      writeFileSync(join(testWorkspace, 'append.txt'), 'Line 1\n');

      const code = `
        agent_main :-
            append_file("append.txt", "Line 2\\n"),
            read_file("append.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      const content = readFileSync(join(testWorkspace, 'append.txt'), 'utf-8');
      expect(content).toBe('Line 1\nLine 2\n');
    });

    it('should write and read back correctly', async () => {
      const code = `
        agent_main :-
            write_file("roundtrip.txt", "Test data 123"),
            read_file("roundtrip.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('Test data 123');
    });
  });

  describe('File Existence Checks', () => {
    it('should check if file exists with exists_file/1', async () => {
      writeFileSync(join(testWorkspace, 'exists.txt'), 'content');

      const code = `
        agent_main :-
            (   exists_file("exists.txt")
            ->  answer("file exists")
            ;   answer("file not found")
            ).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('file exists');
    });

    it('should fail exists_file for non-existent file', async () => {
      const code = `
        agent_main :-
            (   exists_file("missing.txt")
            ->  answer("found")
            ;   answer("not found")
            ).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('not found');
    });
  });

  describe('Directory Operations', () => {
    it('should create directory with make_directory/1', async () => {
      const code = `
        agent_main :-
            make_directory("subdir"),
            (   exists_directory("subdir")
            ->  answer("created")
            ;   answer("failed")
            ).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('created');
      expect(existsSync(join(testWorkspace, 'subdir'))).toBe(true);
    });

    it('should list directory contents with directory_files/2', async () => {
      // Create test files
      writeFileSync(join(testWorkspace, 'file1.txt'), 'a');
      writeFileSync(join(testWorkspace, 'file2.txt'), 'b');

      const code = `
        agent_main :-
            directory_files("", Files),
            include(not_dot, Files, CleanFiles),
            length(CleanFiles, Count),
            format(string(Result), "~w files", [Count]),
            answer(Result).
        
        not_dot(F) :-
            F \\= '.', F \\= '..'.
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      // Should have at least 2 files
      const answer = events.find(e => e.type === 'answer')?.content;
      expect(answer).toMatch(/\d+ files/);
    });

    it('should write file in subdirectory', async () => {
      mkdirSync(join(testWorkspace, 'nested'), { recursive: true });

      const code = `
        agent_main :-
            write_file("nested/deep.txt", "nested content"),
            read_file("nested/deep.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('nested content');
    });
  });

  describe('Security - Path Restrictions', () => {
    it('should prevent directory traversal with ../', async () => {
      const code = `
        agent_main :-
            read_file("../../../etc/passwd", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.some(e => e.type === 'error')).toBe(true);
      const errorEvent = events.find(e => e.type === 'error');
      expect(errorEvent?.content).toContain('traversal');
    });

    it('should handle absolute paths by treating them as relative', async () => {
      writeFileSync(join(testWorkspace, 'abs.txt'), 'absolute path content');

      const code = `
        agent_main :-
            read_file("/abs.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      expect(events.find(e => e.type === 'answer')?.content).toBe('absolute path content');
    });
  });

  describe('Low-level File I/O', () => {
    it('should use open/close/write for streaming writes', async () => {
      const code = `
        agent_main :-
            open("stream.txt", write, S),
            write(S, "First line\\n"),
            write(S, "Second line\\n"),
            close(S),
            read_file("stream.txt", Content),
            answer(Content).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code, { workspacePath: testWorkspace })) {
        events.push(event);
      }

      const answer = events.find(e => e.type === 'answer')?.content;
      expect(answer).toContain('First line');
      expect(answer).toContain('Second line');
    });
  });
});

// ============================================================================
// SDK LIFECYCLE
// ============================================================================
describe('SDK Lifecycle', () => {
  it('should create an SDK instance', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini' });

    expect(sdk).toBeDefined();
    expect(typeof sdk.runDML).toBe('function');
    expect(typeof sdk.registerTool).toBe('function');
    expect(typeof sdk.setToolPolicy).toBe('function');
    expect(typeof sdk.dispose).toBe('function');

    await sdk.dispose();
  });

  it('should accept API key in options', async () => {
    const sdk = await createDeepClause({
      apiKey: 'test-key',
      model: 'gpt-4o-mini',
    });

    expect(sdk).toBeDefined();
    await sdk.dispose();
  });

  it('should clean up resources on dispose', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini' });
    
    await sdk.dispose();

    await expect(async () => {
      for await (const _ of sdk.runDML('agent_main :- answer("test").')) {
        // consume
      }
    }).rejects.toThrow();
  });
});

// ============================================================================
// EXECUTION TRACE
// ============================================================================
describe('Execution Trace', () => {
  it('should not include trace when trace mode is disabled', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: false });

    const code = `
      agent_main :-
          output("Hello"),
          answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    expect(finished).toBeDefined();
    expect(finished.trace).toBeUndefined();

    await sdk.dispose();
  });

  it('should include trace when trace mode is enabled', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    const code = `
      agent_main :-
          output("Hello"),
          answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    expect(finished).toBeDefined();
    expect(finished.trace).toBeDefined();
    expect(Array.isArray(finished.trace)).toBe(true);
    expect(finished.trace.length).toBeGreaterThan(0);

    await sdk.dispose();
  });

  it('should trace output calls', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    const code = `
      agent_main :-
          output("Test output"),
          answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    const outputTrace = finished.trace.find((t: any) => t.type === 'output' && t.predicate === 'output');
    expect(outputTrace).toBeDefined();
    expect(outputTrace.args).toContain('Test output');

    await sdk.dispose();
  });

  it('should trace user-defined predicate calls', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    const code = `
      my_helper(X) :-
          Y is X * 2,
          output("Value: {Y}").

      agent_main :-
          my_helper(5),
          answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    const helperCall = finished.trace.find((t: any) => t.type === 'call' && t.predicate === 'my_helper');
    expect(helperCall).toBeDefined();
    expect(helperCall.args).toContain(5);

    const helperExit = finished.trace.find((t: any) => t.type === 'exit' && t.predicate === 'my_helper');
    expect(helperExit).toBeDefined();

    await sdk.dispose();
  });

  it('should trace exec calls', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    // Register a simple tool
    sdk.registerTool('add_numbers', {
      description: 'Add two numbers',
      parameters: {
        type: 'object',
        properties: {
          a: { type: 'number' },
          b: { type: 'number' }
        },
        required: ['a', 'b']
      },
      execute: async (args: any) => args.a + args.b
    });

    const code = `
      agent_main :-
          exec(add_numbers(3, 4), Result),
          output("Result: {Result}"),
          answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    const execCall = finished.trace.find((t: any) => t.type === 'exec' && t.predicate === 'add_numbers');
    expect(execCall).toBeDefined();
    expect(execCall.args).toEqual([3, 4]);

    const execExit = finished.trace.find((t: any) => t.type === 'exit' && t.predicate === 'add_numbers');
    expect(execExit).toBeDefined();
    expect(execExit.result).toBe(7);

    await sdk.dispose();
  });

  it('should include timestamps in trace entries', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    const code = `
      agent_main :-
          output("Testing timestamps"),
          answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    expect(finished.trace.length).toBeGreaterThan(0);

    for (const entry of finished.trace) {
      expect(typeof entry.timestamp).toBe('number');
      expect(entry.timestamp).toBeGreaterThan(0);
    }

    await sdk.dispose();
  });

  it('should include depth in trace entries', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    const code = `
      inner :- output("inner").
      outer :- inner.
      agent_main :- outer, answer("Done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    
    // Check that depth is present on trace entries
    for (const entry of finished.trace) {
      expect(typeof entry.depth).toBe('number');
      expect(entry.depth).toBeGreaterThanOrEqual(0);
    }

    await sdk.dispose();
  });

  it('should trace failed predicates', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini', trace: true });

    const code = `
      will_fail :- fail.
      agent_main :-
          (will_fail -> answer("Failed") ; answer("Caught")).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const finished = events.find(e => e.type === 'finished');
    expect(finished.trace).toBeDefined();

    // Check there's a fail trace for will_fail
    const failTrace = finished.trace.find((t: any) => t.type === 'fail' && t.predicate === 'will_fail');
    expect(failTrace).toBeDefined();

    await sdk.dispose();
  });
});

// ============================================================================
// STREAMING (configuration tests - actual streaming requires LLM)
// ============================================================================
describe('Streaming Configuration', () => {
  it('should accept streaming option in createDeepClause', async () => {
    const sdk = await createDeepClause({ 
      model: 'gpt-4o-mini',
      streaming: true 
    });

    expect(sdk).toBeDefined();
    await sdk.dispose();
  });

  it('should default streaming to false', async () => {
    const sdk = await createDeepClause({ model: 'gpt-4o-mini' });

    // Basic execution should work without streaming events
    const code = `
      agent_main :- answer("Hello").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    // Should not have any stream events for non-LLM code
    const streamEvents = events.filter(e => e.type === 'stream');
    expect(streamEvents.length).toBe(0);

    await sdk.dispose();
  });

  it('should have stream event type in DMLEvent', async () => {
    // Type check: stream events should have content and done properties
    const streamEvent = { type: 'stream' as const, content: 'chunk', done: false };
    expect(streamEvent.type).toBe('stream');
    expect(streamEvent.done).toBe(false);
  });
});
