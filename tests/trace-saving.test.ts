import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { mkdir, writeFile, rm, readFile } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import { createDeepClause } from '../src/sdk.js';

// Mock the SDK
vi.mock('../src/sdk.js', () => ({
  createDeepClause: vi.fn()
}));

// Mock AgentVM
vi.mock('deepclause-agentvm', () => ({
  AgentVM: vi.fn().mockImplementation(() => ({
    start: vi.fn(),
    stop: vi.fn(),
    exec: vi.fn()
  }))
}));

describe('Trace Saving', () => {
  let tempDir: string;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `deepclause-trace-test-${Date.now()}`);
    await mkdir(tempDir, { recursive: true });
    await mkdir(join(tempDir, '.deepclause'), { recursive: true });
    await mkdir(join(tempDir, 'workspace'), { recursive: true });
    
    // Create minimal config
    await writeFile(
      join(tempDir, '.deepclause', 'config.json'),
      JSON.stringify({
        model: 'gpt-4o',
        provider: 'openai',
        workspace: './workspace'
      })
    );
  });

  afterEach(async () => {
    try {
      await rm(tempDir, { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
    vi.clearAllMocks();
  });

  it('should save trace to file when --trace is specified', async () => {
    const dmlContent = `agent_main :- answer("done").`;
    const dmlPath = join(tempDir, 'test.dml');
    await writeFile(dmlPath, dmlContent);
    
    const tracePath = join(tempDir, 'trace.json');
    const mockTrace = [{ timestamp: 123, type: 'call', predicate: 'agent_main' }];

    // Mock SDK implementation
    (createDeepClause as any).mockResolvedValue({
      runDML: async function* () {
        yield { type: 'answer', content: 'done' };
        yield { type: 'finished', trace: mockTrace };
      },
      registerTool: vi.fn(),
      dispose: vi.fn()
    });

    const { run } = await import('../src/cli/run.js');
    
    // Change current working directory to tempDir so loadConfig finds the config
    const originalCwd = process.cwd();
    process.chdir(tempDir);

    try {
      await run(dmlPath, [], { trace: tracePath, headless: true });
      
      // Verify trace file exists and contains the mock trace
      const traceContent = await readFile(tracePath, 'utf-8');
      const parsedTrace = JSON.parse(traceContent);
      expect(parsedTrace).toEqual(mockTrace);
    } finally {
      process.chdir(originalCwd);
    }
  });

  it('should save trace to file even when an error occurs', async () => {
    const dmlContent = `agent_main :- loop.`;
    const dmlPath = join(tempDir, 'error_test.dml');
    await writeFile(dmlPath, dmlContent);
    
    const tracePath = join(tempDir, 'error_trace.json');
    const mockTrace = [{ timestamp: 456, type: 'call', predicate: 'loop' }];

    // Mock SDK implementation
    (createDeepClause as any).mockResolvedValue({
      runDML: async function* () {
        yield { type: 'error', content: 'infinite loop or something', trace: mockTrace };
      },
      registerTool: vi.fn(),
      dispose: vi.fn()
    });

    const { run } = await import('../src/cli/run.js');
    
    const originalCwd = process.cwd();
    process.chdir(tempDir);

    try {
      await run(dmlPath, [], { trace: tracePath, headless: true });
      
      // Verify trace file exists and contains the mock trace
      const traceContent = await readFile(tracePath, 'utf-8');
      const parsedTrace = JSON.parse(traceContent);
      expect(parsedTrace).toEqual(mockTrace);
    } finally {
      process.chdir(originalCwd);
    }
  });
});
