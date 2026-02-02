/**
 * DeepClause SDK - Basic Usage Examples
 * 
 * This file demonstrates how to use the DeepClause SDK to run
 * DML (DeepClause Meta Language) code with LLM-powered task execution.
 * 
 * Requirements:
 *   - Set OPENAI_API_KEY or ANTHROPIC_API_KEY environment variable
 *   - Example: export OPENAI_API_KEY="sk-..."
 */

import { createDeepClause } from '../src/index.js';
import { z } from 'zod';

// Helper to check for API keys
function checkApiKey(provider: string): boolean {
  const keys: Record<string, string | undefined> = {
    openai: process.env.OPENAI_API_KEY,
    anthropic: process.env.ANTHROPIC_API_KEY,
    google: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    openrouter: process.env.OPENROUTER_API_KEY,
  };
  
  if (!keys[provider]) {
    console.log(`⚠️  No ${provider.toUpperCase()} API key found.`);
    console.log(`   Set ${provider.toUpperCase()}_API_KEY environment variable to run this example.\n`);
    return false;
  }
  return true;
}

// Default run options with a simple onUserInput handler
// This prevents hangs when the LLM tries to ask the user a question
const defaultRunOptions = {
  onUserInput: async (prompt: string) => {
    console.log('[User Input Requested]', prompt);
    // For non-interactive examples, just acknowledge
    return 'Please proceed without user input.';
  }
};

// =============================================================================
// Example 1: Simple Task Execution
// =============================================================================

async function simpleTask() {
  console.log('\n=== Example 1: Simple Task Execution ===\n');

  if (!checkApiKey('google')) return;

  // Create SDK instance with your preferred model
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    temperature: 0.7,
  });

  // Simple DML code with a task
  const code = `
    agent_main :-
        system("You are a helpful assistant."),
        task("Explain what Prolog is in one sentence."),
        answer("Task completed!").
  `;

  try {
    // Run the DML code and process events
    for await (const event of dc.runDML(code, defaultRunOptions)) {
      switch (event.type) {
        case 'output':
          if (event.content) console.log('[Output]', event.content);
          break;
        case 'log':
          console.log('[Log]', event.content);
          break;
        case 'answer':
          console.log('[Answer]', event.content);
          break;
        case 'error':
          console.error('[Error]', event.content);
          break;
        case 'finished':
          console.log('[Finished]');
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 2: Task with Output Variables
// =============================================================================

async function taskWithVariables() {
  console.log('\n=== Example 2: Task with Output Variables ===\n');

  if (!checkApiKey('google')) return;

  // Create SDK instance with your preferred model
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    temperature: 0.7,
  });

  // DML code using task with output variable binding
  const code = `
    agent_main :-
        system("You are a research assistant."),
        task("List 3 benefits of functional programming. Store them in Benefits.", Benefits),
        format(string(Msg), "Benefits found: ~w", [Benefits]),
        answer(Msg).
  `;

  try {
    for await (const event of dc.runDML(code, defaultRunOptions)) {
      if (event.type === 'answer') {
        console.log('[Answer]', event.content);
      } else if (event.type === 'error') {
        console.error('[Error]', event.content);
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 3: Registering Custom Tools
// =============================================================================

async function customTools() {
  console.log('\n=== Example 3: Registering Custom Tools ===\n');

  if (!checkApiKey('google')) return;

  // Create SDK instance with your preferred model
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    temperature: 0.7,
  });

  // Register a custom "calculator" tool
  dc.registerTool('calculator', {
    description: 'Perform mathematical calculations',
    parameters: z.object({
      expression: z.string().describe('Mathematical expression to evaluate'),
    }),
    execute: async (args) => {
      const { expression } = args as { expression: string };
      try {
        // Simple evaluation (in production, use a proper math parser)
        const result = Function(`"use strict"; return (${expression})`)();
        return { success: true, result };
      } catch (error) {
        return { success: false, error: String(error) };
      }
    },
  });

  // Register a "fetch_weather" tool
  dc.registerTool('fetch_weather', {
    description: 'Get current weather for a city',
    parameters: z.object({
      city: z.string().describe('City name'),
    }),
    execute: async (args) => {
      const { city } = args as { city: string };
      // Mock weather data (replace with actual API call)
      return {
        city,
        temperature: Math.floor(Math.random() * 30) + 5,
        condition: ['sunny', 'cloudy', 'rainy'][Math.floor(Math.random() * 3)],
      };
    },
  });

  console.log('Registered tools:', dc.getTools());

  // DML code that uses exec() to call external tools
  // Simple syntax: last arg is output, first args are inputs
  const code = `
    % Define tool wrappers with descriptions
    % Schema is inferred: first N-1 args = inputs, last arg = output
    tool(calculate(Expr, Result),
         "Calculate a mathematical expression") :-
        exec(calculator(Expr), Result).

    tool(get_weather(City, Weather),
         "Get current weather for a city") :-
        exec(fetch_weather(City), Weather).

    agent_main :-
        system("You are a helpful assistant with access to calculation and weather tools."),
        task("Calculate 15 * 7 + 23 using the calculate tool, then get the weather for Paris."),
        answer("Tools executed successfully!").
  `;

  try {
    for await (const event of dc.runDML(code, defaultRunOptions)) {
      if (event.type === 'output' || event.type === 'answer') {
        console.log(`[${event.type}]`, event.content);
      } else if (event.type === 'error') {
        console.error('[Error]', event.content);
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 4: Tool Policy (Whitelist/Blacklist)
// =============================================================================

async function toolPolicy() {
  console.log('\n=== Example 4: Tool Policy ===\n');

  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    temperature: 0.7,
  });

  // Register some tools
  dc.registerTool('file_read', {
    description: 'Read a file',
    parameters: z.object({ path: z.string() }),
    execute: async () => ({ content: 'file contents...' }),
  });

  dc.registerTool('file_write', {
    description: 'Write to a file',
    parameters: z.object({ path: z.string(), content: z.string() }),
    execute: async () => ({ success: true }),
  });

  dc.registerTool('web_search', {
    description: 'Search the web',
    parameters: z.object({ query: z.string() }),
    execute: async () => ({ results: [] }),
  });

  // Set a whitelist policy - only allow read operations and web search
  dc.setToolPolicy({
    mode: 'whitelist',
    tools: ['file_read', 'web_*'],  // Supports wildcards!
  });

  console.log('Current policy:', dc.getToolPolicy());
  console.log('Registered tools:', dc.getTools());

  // Or use blacklist mode to block specific tools
  dc.setToolPolicy({
    mode: 'blacklist',
    tools: ['file_write', 'shell_*'],  // Block write and shell operations
  });

  console.log('Updated policy:', dc.getToolPolicy());

  // Clear policy to allow all tools
  dc.clearToolPolicy();
  console.log('Policy cleared:', dc.getToolPolicy());

  await dc.dispose();
}

// =============================================================================
// Example 5: User Input Handling
// =============================================================================

async function userInputHandling() {
  console.log('\n=== Example 5: User Input Handling ===\n');

  if (!checkApiKey('google')) return;

  // Create SDK instance with your preferred model
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    temperature: 0.7,
  });

  const code = `
    agent_main :-
        system("You are an interactive assistant."),
        task("Ask the user for their name and greet them personally."),
        answer("Interaction complete!").
  `;

  try {
    for await (const event of dc.runDML(code, {
      // Handle user input requests
      onUserInput: async (prompt) => {
        console.log('[Prompt]', prompt);
        // In a real app, this would get input from user
        return 'Alice';
      },
    })) {
      switch (event.type) {
        case 'input_required':
          console.log('[Input Required]', event.prompt);
          break;
        case 'output':
          console.log('[Output]', event.content);
          break;
        case 'answer':
          console.log('[Answer]', event.content);
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 6: Parameters and Backtracking
// =============================================================================

async function parametersAndBacktracking() {
  console.log('\n=== Example 6: Parameters and Backtracking ===\n');

  if (!checkApiKey('google')) return;

  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
    temperature: 0.7,
  });

  // DML with parameters (note: parameter order may vary)
  const code = `
    agent_main(Topic) :-
        format(string(SystemMsg), "You are a research assistant studying: ~w", [Topic]),
        system(SystemMsg),
        log("Starting research..."),
        task("Provide a brief summary of the topic in 2 sentences."),
        answer("Research completed!").
  `;

  try {
    for await (const event of dc.runDML(code, {
      params: {
        topic: 'quantum computing',
      },
      ...defaultRunOptions,
    })) {
      switch (event.type) {
        case 'log':
          console.log('[Log]', event.content);
          break;
        case 'output':
          if (event.content) console.log('[Output]', event.content);
          break;
        case 'answer':
          console.log('[Answer]', event.content);
          break;
        case 'error':
          console.error('[Error]', event.content);
          break;
        case 'finished':
          console.log('[Finished]');
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 7: Sub-agents and Context Management
// =============================================================================

async function subAgents() {
  console.log('\n=== Example 7: Sub-agents ===\n');

  if (!checkApiKey('google')) return;

  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
  });

  // DML with sub-agent pattern
  const code = `
    % Define a sub-agent tool
    tool(sub_agent(SubTask, Output),
         "Delegate a subtask to a specialized sub-agent") :-
        push_context,
        task(SubTask, Output),
        pop_context.

    % Main agent that delegates to sub-agents
    agent_main :-
        system("You are a project coordinator."),
        task("Break down 'build a website' into 3 subtasks. For each, use sub_agent to get a brief plan.", Plans),
        format(string(Result), "Project plans: ~w", [Plans]),
        answer(Result).
  `;

  try {
    for await (const event of dc.runDML(code, defaultRunOptions)) {
      if (event.type !== 'finished') {
        console.log(`[${event.type}]`, event.content ?? event.prompt ?? '');
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 8: Streaming Output with yield()
// =============================================================================

async function streamingOutput() {
  console.log('\n=== Example 8: Streaming Output ===\n');

  if (!checkApiKey('google')) return;

  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
  });

  const code = `
    agent_main :-
        system("You are a storyteller."),
        yield("Starting story generation..."),
        task("Write a very short 3-sentence story about a robot."),
        yield("Story generation complete!"),
        answer("Done!").
  `;

  try {
    for await (const event of dc.runDML(code, defaultRunOptions)) {
      if (event.type === 'output') {
        // Streaming output from yield()
        process.stdout.write(event.content ?? '');
      } else if (event.type === 'answer') {
        console.log('\n[Answer]', event.content);
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 9: Cancellation with AbortController
// =============================================================================

async function cancellation() {
  console.log('\n=== Example 9: Cancellation ===\n');

  if (!checkApiKey('google')) return;

  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
  });

  const code = `
    agent_main :-
        system("You are a thorough researcher."),
        task("Write a comprehensive 10-page essay about artificial intelligence."),
        answer("Essay complete!").
  `;

  const controller = new AbortController();

  // Cancel after 2 seconds
  setTimeout(() => {
    console.log('[Cancelling...]');
    controller.abort();
  }, 2000);

  try {
    for await (const event of dc.runDML(code, { signal: controller.signal, ...defaultRunOptions })) {
      if (event.type === 'error') {
        console.log('[Cancelled]', event.content);
      } else {
        console.log(`[${event.type}]`, event.content ?? '');
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example 10: Complete Research Agent
// =============================================================================

async function researchAgent() {
  console.log('\n=== Example 10: Complete Research Agent ===\n');

  if (!checkApiKey('google')) return;

  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey: process.env.GOOGLE_GENERATIVE_AI_API_KEY,
  });

  // Register research tools
  dc.registerTool('web_search', {
    description: 'Search the web for information',
    parameters: z.object({
      query: z.string().describe('Search query'),
      maxResults: z.number().optional().describe('Maximum results to return'),
    }),
    execute: async (args) => {
      const { query } = args as { query: string; maxResults?: number };
      // Mock search results
      return {
        results: [
          { title: `Result 1 for: ${query}`, url: 'https://example.com/1' },
          { title: `Result 2 for: ${query}`, url: 'https://example.com/2' },
        ],
      };
    },
  });

  dc.registerTool('save_report', {
    description: 'Save a research report to file',
    parameters: z.object({
      filename: z.string(),
      content: z.string(),
    }),
    execute: async (args) => {
      const { filename, content } = args as { filename: string; content: string };
      console.log(`[Mock] Saving report to ${filename} (${content.length} chars)`);
      return { success: true, path: `/reports/${filename}` };
    },
  });

  // Set security policy
  dc.setToolPolicy({
    mode: 'whitelist',
    tools: ['web_search', 'save_report'],
  });

  const code = `
    % Tool wrappers with descriptions
    tool(search(Query, Results),
         "Search the web for information") :-
        exec(web_search(Query), Results).

    tool(save(Filename, Content, Status),
         "Save content to a file") :-
        exec(save_report(Filename, Content), Status).

    % Main research workflow
    agent_main(Topic) :-
        format(string(SysMsg), 
            "You are a research assistant. Research: ~w", [Topic]),
        system(SysMsg),
        
        log("Starting research..."),
        task("Search for key information about the topic using the search tool.", SearchResults),
        
        log("Analyzing results..."),
        task("Analyze the search results and create a brief summary. Store in Summary.", Summary),
        
        log("Generating report..."),
        task("Save the summary as 'report.md' using the save tool."),
        
        format(string(FinalAnswer), "Research complete! Summary: ~w", [Summary]),
        answer(FinalAnswer).
  `;

  try {
    for await (const event of dc.runDML(code, {
      params: { Topic: 'sustainable energy' },
      ...defaultRunOptions,
    })) {
      switch (event.type) {
        case 'log':
          console.log('[LOG]', event.content);
          break;
        case 'output':
          console.log('[OUT]', event.content);
          break;
        case 'answer':
          console.log('\n[ANSWER]', event.content);
          break;
        case 'error':
          console.error('[ERROR]', event.content);
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Run Examples
// =============================================================================

async function main() {
  const examples = [
    { name: 'Simple Task', fn: simpleTask },
    { name: 'Task with Variables', fn: taskWithVariables },
    { name: 'Custom Tools', fn: customTools },
    { name: 'Tool Policy', fn: toolPolicy },
    { name: 'User Input Handling', fn: userInputHandling },
    { name: 'Parameters and Backtracking', fn: parametersAndBacktracking },
    { name: 'Sub-agents', fn: subAgents },
    { name: 'Streaming Output', fn: streamingOutput },
    { name: 'Cancellation', fn: cancellation },
    { name: 'Complete Research Agent', fn: researchAgent },
  ];

  // Run a specific example by number (from command line) or run all
  const exampleNum = process.argv[2] ? parseInt(process.argv[2], 10) : null;

  if (exampleNum !== null && exampleNum >= 1 && exampleNum <= examples.length) {
    const example = examples[exampleNum - 1];
    console.log(`\nRunning Example ${exampleNum}: ${example.name}`);
    await example.fn();
  } else {
    console.log('DeepClause SDK Examples');
    console.log('=======================');
    console.log('\nAvailable examples:');
    examples.forEach((ex, i) => {
      console.log(`  ${i + 1}. ${ex.name}`);
    });
    console.log('\nUsage: npx ts-node examples/basic-usage.ts [example_number]');
    console.log('\nExample: npx ts-node examples/basic-usage.ts 4');
    
    // Run example 4 (Tool Policy) as a demo since it doesn't need API keys
    console.log('\n--- Running Tool Policy demo (no API needed) ---');
    await toolPolicy();
  }
}

main().catch(console.error);
