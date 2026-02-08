import { describe, it, expect, vi, beforeEach } from 'vitest';
import { createDeepClause } from '../src/sdk.js';

// Mock AI SDK
vi.mock('ai', () => ({
  generateText: vi.fn().mockResolvedValue({
    text: 'agent_main :- answer("Hello World").'
  }),
  streamText: vi.fn()
}));

vi.mock('@ai-sdk/openai', () => ({
  openai: vi.fn()
}));

describe('SDK Compile', () => {
  it('should compile a prompt to DML', async () => {
    const dc = await createDeepClause({
      model: 'gpt-4o',
      apiKey: 'test-key'
    });

    const result = await dc.compile('Say hello world');

    expect(result.valid).toBe(true);
    expect(result.dml).toContain('agent_main');
    expect(result.dml).toContain('Hello World');
    
    await dc.dispose();
  });

  it('should handle compilation failures', async () => {
    const { generateText } = await import('ai');
    // Mock failure by returning invalid Prolog
    (generateText as any).mockResolvedValueOnce({
      text: 'invalid prolog code'
    });

    const dc = await createDeepClause({
      model: 'gpt-4o',
      apiKey: 'test-key'
    });

    const result = await dc.compile('Do something invalid', { maxAttempts: 1 });

    expect(result.valid).toBe(false);
    expect(result.errors).toBeDefined();
    expect(result.errors?.length).toBeGreaterThan(0);
    
    await dc.dispose();
  });
});
