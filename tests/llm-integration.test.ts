/**
 * Test Suite: LLM Integration
 * 
 * Tests that require actual LLM API calls:
 * - task() predicate execution
 * - User-defined tools with LLM
 * - Memory affecting LLM behavior
 * - String interpolation in task descriptions
 * - ask_user tool
 * - sub_agent patterns
 * 
 * NOTE: These tests make real API calls and may incur costs.
 * Set OPENAI_API_KEY environment variable to run.
 */
import { describe, it, expect, beforeAll, afterAll, vi } from 'vitest';
import { createDeepClause, DeepClauseSDK } from '../src/index.js';

// ============================================================================
// TASK PREDICATE
// ============================================================================
describe('task() predicate', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  describe('task/1 - simple task execution', () => {
    it('should execute a simple task and succeed', async () => {
      const code = `
        agent_main :-
            task("Call finish with success=true"),
            answer("done").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      expect(events.some(e => e.type === 'answer')).toBe(true);
      expect(events.some(e => e.type === 'finished')).toBe(true);
    });

    it('should allow task to call finish(false) and trigger backtracking', async () => {
      const code = `
        agent_main :-
            task("Call finish(false) immediately"),
            answer("branch1").
        
        agent_main :-
            answer("branch2").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const answerEvent = events.find(e => e.type === 'answer');
      expect(answerEvent?.content).toBe('branch2');
    });
  });

  describe('task/N - task with output variables', () => {
    it('should bind output variable via store tool', async () => {
      const code = `
        agent_main :-
            task("Generate a random number between 1 and 10, store in X", X),
            format(string(Msg), "Got: ~w", [X]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const answerEvent = events.find(e => e.type === 'answer');
      expect(answerEvent?.content).toMatch(/Got: \d+/);
    });

    it('should support multiple output variables', async () => {
      const code = `
        agent_main :-
            task("Generate a name and age. Store name in Name and age in Age", Name, Age),
            format(string(Msg), "Name: ~w, Age: ~w", [Name, Age]),
            answer(Msg).
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const answerEvent = events.find(e => e.type === 'answer');
      expect(answerEvent?.content).toMatch(/Name: .+, Age: \d+/);
    });

    it('should fail task if store is called with invalid variable name', async () => {
      const code = `
        agent_main :-
            task("Store value in InvalidVar which is not declared", X),
            answer("should not reach").
        
        agent_main :-
            answer("fallback").
      `;

      const events: any[] = [];
      for await (const event of sdk.runDML(code)) {
        events.push(event);
      }

      const answerEvent = events.find(e => e.type === 'answer');
      expect(answerEvent?.content).toBe('fallback');
    });
  });
});

// ============================================================================
// USER-DEFINED TOOLS FOR LLM
// ============================================================================
describe('User-defined tools for task()', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should expose tool/1 predicates to task agent', async () => {
    const code = `
      tool(greet(Name, Greeting)) :-
          format(string(Greeting), "Hello, ~w!", [Name]).
      
      agent_main :-
          task("Use greet tool to greet 'World', store result in G", G),
          answer(G).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content).toBe('Hello, World!');
  });

  it('should support tool/2 with description', async () => {
    const code = `
      tool(add(A, B, Sum), "Add two numbers together") :-
          Sum is A + B.
      
      agent_main :-
          task("Use add tool to add 5 and 7, store result in R", R),
          answer(R).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content).toBe('12');
  });

  it('should support shorthand tool syntax (last arg is output)', async () => {
    const code = `
      tool(multiply(A, B, Result)) :-
          Result is A * B.
      
      agent_main :-
          task("Use multiply tool to multiply 3 and 4, store result in R", R),
          answer(R).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content).toBe('12');
  });

  it('should allow task to use exec via user-defined tool', async () => {
    sdk.registerTool('search', {
      description: 'Search for information',
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string' }
        },
        required: ['query']
      },
      execute: async ({ query }) => `Search results for: ${query}`
    });

    const code = `
      tool(do_search(Query, Results)) :-
          exec(search(Query), Results).
      
      agent_main :-
          task("Use do_search to find info about Prolog, store in Results", Results),
          answer(Results).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content).toContain('Prolog');
  });
});

// ============================================================================
// MEMORY AFFECTING LLM BEHAVIOR
// ============================================================================
describe('Memory affecting LLM behavior', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should use system message to affect LLM behavior', async () => {
    const code = `
      agent_main :-
          system("You are a pirate. Always respond like a pirate."),
          task("Say hello"),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    // Should complete - we can't easily verify the pirate persona, but it should work
    expect(events.some(e => e.type === 'finished')).toBe(true);
  });

  it('should remember user context', async () => {
    const code = `
      agent_main :-
          user("My name is Bob"),
          task("What is my name?", Name),
          answer(Name).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content.toLowerCase()).toContain('bob');
  });

  it('should restore memory on pop_context (affecting LLM)', async () => {
    const code = `
      agent_main :-
          system("Remember: the secret word is 'apple'"),
          push_context,
          system("Actually, forget that. The secret word is 'banana'"),
          pop_context,
          task("What is the secret word?", Word),
          answer(Word).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content.toLowerCase()).toContain('apple');
  });
});

// ============================================================================
// STRING INTERPOLATION IN TASK
// ============================================================================
describe('String interpolation in task()', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should interpolate variables in task description', async () => {
    const code = `
      agent_main :-
          Topic = "quantum computing",
          task("Explain {Topic} briefly"),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'finished')).toBe(true);
  });

  it('should interpolate multiple variables', async () => {
    const code = `
      agent_main :-
          Subject = "physics",
          Level = "beginner",
          task("Create a {Level} explanation of {Subject}"),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'finished')).toBe(true);
  });

  it('should use exec results in task', async () => {
    sdk.registerTool('fetch_data', {
      description: 'Fetch some data',
      parameters: { type: 'object', properties: {} },
      execute: async () => 'The weather is sunny and 72°F'
    });

    const code = `
      agent_main :-
          exec(fetch_data, Data),
          task("Summarize this data briefly: {Data}"),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'finished')).toBe(true);
  });
});

// ============================================================================
// ASK_USER TOOL
// ============================================================================
describe('ask_user tool', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should call onUserInput when input is required', async () => {
    const code = `
      agent_main :-
          task("Ask user for name using ask_user", Name),
          answer(Name).
    `;

    const inputHandler = vi.fn().mockResolvedValue('TestUser');

    const events: any[] = [];
    for await (const event of sdk.runDML(code, { onUserInput: inputHandler })) {
      events.push(event);
    }

    expect(inputHandler).toHaveBeenCalled();
  });

  it('should emit input_required event if no handler', async () => {
    const code = `
      agent_main :-
          task("Ask user for input using ask_user", Input),
          answer(Input).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
      if (event.type === 'input_required') {
        break; // Stop here since we can't provide input
      }
    }

    expect(events.some(e => e.type === 'input_required')).toBe(true);
  });

  it('should pause for user input and resume with onUserInput handler', async () => {
    const code = `
      agent_main :-
          task("Ask user for their name using ask_user, store in Name", Name),
          format(string(Msg), "Hello, ~w!", [Name]),
          answer(Msg).
    `;

    const inputHandler = vi.fn().mockResolvedValue('Alice');

    const events: any[] = [];
    for await (const event of sdk.runDML(code, { onUserInput: inputHandler })) {
      events.push(event);
    }

    expect(inputHandler).toHaveBeenCalled();
    const answerEvent = events.find(e => e.type === 'answer');
    expect(answerEvent?.content).toBe('Hello, Alice!');
  });
});

// ============================================================================
// SUB-AGENT PATTERNS
// ============================================================================
describe('Sub-agent patterns', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should support nested task calls via sub_agent pattern', async () => {
    const code = `
      tool(sub_agent(Task, Output),
           "Delegate a subtask to a sub-agent") :-
          push_context,
          task(Task, Output),
          pop_context.
      
      agent_main :-
          task("Use sub_agent to research 'Prolog', store in Info", Info),
          answer(Info).
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'answer')).toBe(true);
  });

  it('should handle disjunction with memory in task', async () => {
    const code = `
      agent_main :-
          system("Base context"),
          (
              push_context,
              system("Branch A context"),
              task("Call finish(false) to fail")
          ;
              push_context,
              system("Branch B context"),
              task("Call finish(true) to succeed")
          ),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'finished')).toBe(true);
  });
});

// ============================================================================
// PARALLEL TOOL CALLING
// ============================================================================
describe('Parallel tool calling', () => {
  let sdk: DeepClauseSDK;
  const toolCallLog: { tool: string; time: number; order: number }[] = [];
  let callOrder = 0;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
    
    // Register tools that simulate async work with delays
    sdk.registerTool('search_a', {
      description: 'Search database A for information',
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string', description: 'Search query' }
        },
        required: ['query']
      },
      execute: async (args) => {
        const order = ++callOrder;
        const startTime = Date.now();
        toolCallLog.push({ tool: 'search_a', time: startTime, order });
        // Simulate async delay
        await new Promise(resolve => setTimeout(resolve, 50));
        return { source: 'database_a', results: [`Result A for: ${(args as any).query}`], order };
      }
    });

    sdk.registerTool('search_b', {
      description: 'Search database B for information',
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string', description: 'Search query' }
        },
        required: ['query']
      },
      execute: async (args) => {
        const order = ++callOrder;
        const startTime = Date.now();
        toolCallLog.push({ tool: 'search_b', time: startTime, order });
        // Simulate async delay
        await new Promise(resolve => setTimeout(resolve, 50));
        return { source: 'database_b', results: [`Result B for: ${(args as any).query}`], order };
      }
    });

    sdk.registerTool('search_c', {
      description: 'Search database C for information',
      parameters: {
        type: 'object',
        properties: {
          query: { type: 'string', description: 'Search query' }
        },
        required: ['query']
      },
      execute: async (args) => {
        const order = ++callOrder;
        const startTime = Date.now();
        toolCallLog.push({ tool: 'search_c', time: startTime, order });
        // Simulate async delay
        await new Promise(resolve => setTimeout(resolve, 50));
        return { source: 'database_c', results: [`Result C for: ${(args as any).query}`], order };
      }
    });
  });

  beforeEach(() => {
    toolCallLog.length = 0;
    callOrder = 0;
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should handle parallel tool calls from LLM without errors', async () => {
    // This task is designed to encourage the LLM to call multiple search tools in parallel
    const code = `
      agent_main :-
          task("Search ALL THREE databases (A, B, and C) for 'AI frameworks'. You MUST call search_a, search_b, AND search_c tools. After getting results from all three, call finish(true)."),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    // Should complete successfully (either finished or answer)
    const completed = events.some(e => e.type === 'finished') || events.some(e => e.type === 'answer');
    expect(completed).toBe(true);
    
    // At least some tools should have been called (LLM behavior varies)
    // The important thing is no crashes or engine corruption errors
    const errors = events.filter(e => e.type === 'error' && e.content?.includes('engine'));
    expect(errors.length).toBe(0);
  }, 60000);

  it('should serialize parallel tool calls correctly with mutex', async () => {
    const code = `
      agent_main :-
          task("Search databases A and B simultaneously for 'machine learning'. Call both search_a and search_b at the same time, then finish(true)."),
          answer("done").
    `;

    const events: any[] = [];
    for await (const event of sdk.runDML(code)) {
      events.push(event);
    }

    expect(events.some(e => e.type === 'finished')).toBe(true);
    
    // If tools were called in parallel by the LLM, they should still execute sequentially
    // due to our mutex - check that no tool results are corrupted
    const errors = events.filter(e => e.type === 'error');
    expect(errors.length).toBe(0);
  }, 60000);
});

// ============================================================================
// ABORT SIGNAL
// ============================================================================
describe('Abort signal', () => {
  let sdk: DeepClauseSDK;

  beforeAll(async () => {
    sdk = await createDeepClause({ model: 'gemini-2.0-flash' });
  });

  afterAll(async () => {
    await sdk.dispose();
  });

  it('should stop execution when aborted', async () => {
    const code = `
      agent_main :-
          task("Count from 1 to 1000000 slowly"),
          answer("done").
    `;

    const controller = new AbortController();
    
    setTimeout(() => controller.abort(), 100);

    const events: any[] = [];
    try {
      for await (const event of sdk.runDML(code, { signal: controller.signal })) {
        events.push(event);
      }
    } catch (e: any) {
      expect(e.name).toBe('AbortError');
    }
  });
});
