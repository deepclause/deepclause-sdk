import { createDeepClause } from '../src/sdk.js';

async function test() {
  const sdk = await createDeepClause({ model: 'gpt-4o-mini', debug: true });
  
  const code = `
    agent_main :-
        answer("Hello from DML!").
  `;
  
  console.log('Running DML...');
  const events: any[] = [];
  try {
    for await (const event of sdk.runDML(code)) {
      console.log('Event:', event);
      events.push(event);
    }
  } catch (e) {
    console.error('Error:', e);
  }
  
  console.log('All events:', events);
  await sdk.dispose();
}

test().catch(console.error);
