/**
 * DeepClause SDK - Mock Provider Example
 * 
 * This example demonstrates the SDK with a mock LLM provider
 * for testing without API keys.
 */

import { createDeepClause } from '../src/index.js';
import { z } from 'zod';

async function mockProviderExample() {
  console.log('=== Mock Provider Example ===\n');
  console.log('This example demonstrates SDK functionality without needing API keys.\n');

  // For testing without an actual LLM, we'll use example 4 (Tool Policy)
  // which doesn't require LLM calls
  
  const dc = await createDeepClause({
    model: 'gpt-4o',  // Model name doesn't matter for policy testing
  });

  // Register various tools
  dc.registerTool('file_read', {
    description: 'Read a file',
    parameters: z.object({ path: z.string() }),
    execute: async ({ path }) => ({ content: `Contents of ${path}` }),
  });

  dc.registerTool('file_write', {
    description: 'Write to a file',
    parameters: z.object({ path: z.string(), content: z.string() }),
    execute: async ({ path }) => ({ success: true, path }),
  });

  dc.registerTool('file_delete', {
    description: 'Delete a file',
    parameters: z.object({ path: z.string() }),
    execute: async ({ path }) => ({ deleted: path }),
  });

  dc.registerTool('web_search', {
    description: 'Search the web',
    parameters: z.object({ query: z.string() }),
    execute: async ({ query }) => ({ results: [`Result for: ${query}`] }),
  });

  dc.registerTool('web_fetch', {
    description: 'Fetch a URL',
    parameters: z.object({ url: z.string() }),
    execute: async ({ url }) => ({ content: `HTML from ${url}` }),
  });

  dc.registerTool('shell_exec', {
    description: 'Execute a shell command',
    parameters: z.object({ command: z.string() }),
    execute: async ({ command }) => ({ output: `Executed: ${command}` }),
  });

  console.log('Registered tools:', dc.getTools());
  console.log('');

  // Demo 1: Whitelist mode
  console.log('--- Demo 1: Whitelist Mode ---');
  dc.setToolPolicy({
    mode: 'whitelist',
    tools: ['file_read', 'web_*'],  // Only allow read and web tools
  });
  console.log('Policy:', dc.getToolPolicy());
  console.log('');
  
  // Test which tools are allowed
  const testTools = ['file_read', 'file_write', 'file_delete', 'web_search', 'web_fetch', 'shell_exec'];
  console.log('Tool access check:');
  for (const tool of testTools) {
    const policy = dc.getToolPolicy();
    if (policy) {
      const allowed = policy.tools.some(pattern => {
        const regex = new RegExp(`^${pattern.replace(/\*/g, '.*')}$`);
        return regex.test(tool);
      });
      const status = policy.mode === 'whitelist' ? allowed : !allowed;
      console.log(`  ${tool}: ${status ? '✓ allowed' : '✗ blocked'}`);
    }
  }
  console.log('');

  // Demo 2: Blacklist mode
  console.log('--- Demo 2: Blacklist Mode ---');
  dc.setToolPolicy({
    mode: 'blacklist',
    tools: ['file_write', 'file_delete', 'shell_*'],  // Block write/delete/shell
  });
  console.log('Policy:', dc.getToolPolicy());
  console.log('');
  
  console.log('Tool access check:');
  for (const tool of testTools) {
    const policy = dc.getToolPolicy();
    if (policy) {
      const blocked = policy.tools.some(pattern => {
        const regex = new RegExp(`^${pattern.replace(/\*/g, '.*')}$`);
        return regex.test(tool);
      });
      const status = policy.mode === 'blacklist' ? !blocked : blocked;
      console.log(`  ${tool}: ${status ? '✓ allowed' : '✗ blocked'}`);
    }
  }
  console.log('');

  // Demo 3: Clear policy
  console.log('--- Demo 3: No Policy (Allow All) ---');
  dc.clearToolPolicy();
  console.log('Policy:', dc.getToolPolicy());
  console.log('All tools are now allowed!');
  console.log('');

  await dc.dispose();
  console.log('SDK disposed. Example complete!');
}

// Run simple DML parsing example
async function dmlParsingExample() {
  console.log('\n=== DML Parsing Example ===\n');

  const dc = await createDeepClause({
    model: 'gpt-4o',
  });

  // This DML code doesn't use task() so won't need LLM
  const code = `
    % Simple Prolog-only DML
    agent_main :-
        log("Hello from DeepClause!"),
        answer("DML execution works!").
  `;

  console.log('Running DML code (no LLM needed):');
  console.log('---');
  console.log(code.trim());
  console.log('---\n');

  try {
    for await (const event of dc.runDML(code)) {
      switch (event.type) {
        case 'log':
          console.log('[LOG]', event.content);
          break;
        case 'answer':
          console.log('[ANSWER]', event.content);
          break;
        case 'output':
          if (event.content && event.content !== 'undefined') {
            console.log('[OUTPUT]', event.content);
          }
          break;
        case 'error':
          console.error('[ERROR]', event.content);
          break;
        case 'finished':
          console.log('[FINISHED]');
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

async function main() {
  await mockProviderExample();
  await dmlParsingExample();
}

main().catch(console.error);
