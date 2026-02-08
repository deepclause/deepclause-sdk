import { describe, it, expect } from 'vitest';
import { createDeepClause } from '../src/sdk.js';

describe('Gas Limit', () => {
  it('should fail when gas limit is exceeded', async () => {
    const dc = await createDeepClause({
      model: 'gpt-4o',
      apiKey: 'test'
    });

    // Infinite recursion in DML
    const dml = `
      loop :- loop.
      agent_main :- loop.
    `;

    const events = [];
    for await (const event of dc.runDML(dml, { gasLimit: 10 })) {
      events.push(event);
    }

    const errorEvent = events.find(e => e.type === 'error');
    expect(errorEvent).toBeDefined();
    expect(errorEvent?.content).toContain('Gas exhausted');
    
    await dc.dispose();
  });

  it('should succeed when gas limit is sufficient', async () => {
    const dc = await createDeepClause({
      model: 'gpt-4o',
      apiKey: 'test'
    });

    const dml = `
      agent_main :- output("hello"), answer("done").
    `;

    const events = [];
    for await (const event of dc.runDML(dml, { gasLimit: 100 })) {
      events.push(event);
    }

    const errorEvent = events.find(e => e.type === 'error');
    expect(errorEvent).toBeUndefined();
    
    const finishedEvent = events.find(e => e.type === 'finished');
    expect(finishedEvent).toBeDefined();

    await dc.dispose();
  });
});
