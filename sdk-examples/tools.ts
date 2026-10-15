/**
 * Tools — and the policy that scopes them.
 *
 *   npx tsx sdk-examples/tools.ts
 *
 * The model is handed two tools. The host — not the model — decides which are
 * usable: the same request runs under a whitelist that permits only
 * `fetch_weather`, so the email call comes back refused. The point is that a
 * tool policy is enforced outside the model's reach.
 */
import { z } from 'zod';

import { backendFrom, exampleSdk, modelLabel, textResponse, toolResponse, toolResults } from './_offline.js';

const DML = `
tool(weather(City, Report), "Current weather for a city.") :-
    exec(fetch_weather(City), Report).

tool(send_email(To, Body, Result), "Send an email.") :-
    exec(smtp(To, Body), Result).

agent_main :-
    system("You are a travel assistant. Use the tools when asked."),
    task("Check the weather in Lisbon, then email it to ana@example.com.", string(_Done)),
    answer("done").
`;

async function main(): Promise<void> {
  console.log(`model: ${modelLabel()}\n`);

  // Offline, a scripted model makes the same two tool calls a real one would.
  const dc = await exampleSdk(
    backendFrom((request) => {
      const results = toolResults(request);
      if (results.length === 0) return toolResponse('weather', { arg1: 'Lisbon' });
      if (results.length === 1) return toolResponse('send_email', { arg1: 'ana@example.com', arg2: results[0] });
      return textResponse(request, 'done');
    }),
    { workspacePath: process.cwd() },
  );

  dc.registerTool('fetch_weather', {
    description: 'Get current weather for a city.',
    parameters: z.object({ city: z.string().describe('City name') }),
    execute: async ({ city }) => ({ city, conditions: 'clear', tempC: 21 }),
  });
  dc.registerTool('smtp', {
    description: 'Send an email.',
    parameters: z.object({ to: z.string(), body: z.string() }),
    execute: async ({ to, body }) => ({ sent: true, to, body }),
  });

  // Only `fetch_weather` is allowed; the email call will be refused by the host.
  dc.setToolPolicy({ mode: 'whitelist', tools: ['fetch_weather'] });
  console.log('policy:', JSON.stringify(dc.getToolPolicy()), '\n');

  try {
    for await (const event of dc.runDML(DML)) {
      if (event.type === 'tool_call') {
        const mark = event.toolState === 'failed' ? '✗' : event.toolState === 'completed' ? '✓' : '▸';
        const detail = event.toolSummary ?? event.toolError ?? '';
        console.log(`  ${mark} ${event.toolName}${detail ? `  ${detail}` : ''}`);
      } else if (event.type === 'answer') {
        console.log('\nanswer:', event.content);
      } else if (event.type === 'error') {
        console.error('error:', event.content);
      }
    }
  } finally {
    await dc.dispose();
  }
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
