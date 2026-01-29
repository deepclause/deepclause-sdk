/**
 * DeepClause SDK - Quick Start Example
 * 
 * This is a minimal example showing the core functionality.
 * Run with: npx ts-node examples/quick-start.ts
 */

import { createDeepClause } from '../src/index.js';
import { z } from 'zod';

async function main() {
  // Check for API key
  const apiKey = process.env.GOOGLE_GENERATIVE_AI_API_KEY;
  if (!apiKey) {
    console.error('❌ Error: GOOGLE_GENERATIVE_AI_API_KEY environment variable not set');
    process.exit(1);
  }
  
  // Create DeepClause instance
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey,
    temperature: 0.7,
    maxTokens: 8192,
  });

  // 3. Define DML code
  const dmlCode = `
    % Tool wrapper for the agent
    tool(current_time(Time)) :-
        exec(get_time, Time).

    % Main entry point
    agent_main :-
        system("You are a helpful assistant."),
        task("Get the current time and tell the user what time it is in a friendly way."),
        answer("Done!").
  `;

  // 4. Run and handle events
  console.log('Starting DeepClause execution...\n');

  try {
    for await (const event of dc.runDML(dmlCode)) {
      switch (event.type) {
        case 'output':
          console.log(event.content);
          break;
        case 'answer':
          console.log('\n✓', event.content);
          break;
        case 'error':
          console.error('✗ Error:', event.content);
          break;
        case 'finished':
          console.log('\nExecution complete.');
          break;
      }
    }
  } finally {
    // 5. Cleanup
    await dc.dispose();
  }
}

main().catch(console.error);
