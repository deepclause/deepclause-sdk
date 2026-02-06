/**
 * Tests for One-shot Mode (run -p)
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { mkdir, writeFile, rm } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';

// Mock compilePrompt
vi.mock('../src/cli/compile', async (importOriginal) => {
  const actual = await importOriginal() as any;
  return {
    ...actual,
    compilePrompt: vi.fn().mockResolvedValue({
      dml: 'agent_main :- answer("oneshot result").',
      tools: []
    })
  };
});

// Mock SDK
vi.mock('../src/sdk', () => ({
  createDeepClause: vi.fn().mockResolvedValue({
    runDML: async function* () {
      yield { type: 'answer', content: 'oneshot result' };
      yield { type: 'finished' };
    },
    registerTool: vi.fn(),
    dispose: vi.fn()
  })
}));

// No memfs mock for now, use real temp dirs like run.test.ts does
// or fix memfs usage. run.test.ts doesn't seem to use memfs mock.

const SAMPLE_CONFIG = {
  model: 'gpt-4o',
  provider: 'openai',
  workspace: './workspace'
};

describe('One-shot Mode', () => {
  let tempDir: string;
  let originalCwd: string;

  beforeEach(async () => {
    originalCwd = process.cwd();
    tempDir = join(tmpdir(), `deepclause-oneshot-test-${Date.now()}`);
    await mkdir(tempDir, { recursive: true });
    await mkdir(join(tempDir, '.deepclause'), { recursive: true });
    await mkdir(join(tempDir, 'workspace'), { recursive: true });
    
    await writeFile(
      join(tempDir, '.deepclause', 'config.json'),
      JSON.stringify(SAMPLE_CONFIG)
    );
    
    process.chdir(tempDir);
  });

  afterEach(async () => {
    process.chdir(originalCwd);
    try {
      await rm(tempDir, { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  it('should generate and run DML from prompt', async () => {
    const { run } = await import('../src/cli/run');
    const { compilePrompt } = await import('../src/cli/compile');
    
    const result = await run(undefined, [], {
      prompt: 'Do something oneshot'
    });
    
    expect(compilePrompt).toHaveBeenCalledWith('Do something oneshot', expect.any(Object));
    expect(result.answer).toBe('oneshot result');
  });

  it('should fail if neither file nor prompt is provided', async () => {
    const { run } = await import('../src/cli/run');
    
    await expect(run(undefined, [], {}))
      .rejects.toThrow('Either a DML file or a --prompt must be provided');
  });

  it('should use model and provider from options if provided', async () => {
    const { run } = await import('../src/cli/run');
    const { compilePrompt } = await import('../src/cli/compile');
    
    await run(undefined, [], {
      prompt: 'Test model override',
      model: 'claude-3-sonnet',
      provider: 'anthropic'
    });
    
    expect(compilePrompt).toHaveBeenCalledWith(
      'Test model override',
      expect.objectContaining({
        model: 'claude-3-sonnet',
        provider: 'anthropic'
      })
    );
  });
});
