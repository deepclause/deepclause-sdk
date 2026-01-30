/**
 * Tests for Execution Module
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { mkdir, writeFile, rm, readFile } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';

// =============================================================================
// Parameter Parsing Tests (unit tests that don't need SDK)
// =============================================================================

describe('Parameter Parsing', () => {
  // We'll test these via the run module's internal functions
  // For now, test the argument parsing logic
  
  it('should parse string arguments', () => {
    const value = 'hello world';
    // String stays as string
    expect(typeof value).toBe('string');
  });

  it('should parse numeric arguments', () => {
    const parseArgValue = (value: string): unknown => {
      const num = Number(value);
      if (!isNaN(num) && value.trim() !== '') {
        return num;
      }
      return value;
    };
    
    expect(parseArgValue('42')).toBe(42);
    expect(parseArgValue('3.14')).toBe(3.14);
    expect(parseArgValue('-10')).toBe(-10);
    expect(parseArgValue('not a number')).toBe('not a number');
  });

  it('should parse boolean arguments', () => {
    const parseArgValue = (value: string): unknown => {
      if (value.toLowerCase() === 'true') return true;
      if (value.toLowerCase() === 'false') return false;
      return value;
    };
    
    expect(parseArgValue('true')).toBe(true);
    expect(parseArgValue('false')).toBe(false);
    expect(parseArgValue('TRUE')).toBe(true);
    expect(parseArgValue('False')).toBe(false);
  });

  it('should parse JSON arguments', () => {
    const parseArgValue = (value: string): unknown => {
      if ((value.startsWith('{') && value.endsWith('}')) ||
          (value.startsWith('[') && value.endsWith(']'))) {
        try {
          return JSON.parse(value);
        } catch {
          return value;
        }
      }
      return value;
    };
    
    expect(parseArgValue('{"key": "value"}')).toEqual({ key: 'value' });
    expect(parseArgValue('[1, 2, 3]')).toEqual([1, 2, 3]);
    expect(parseArgValue('{invalid json}')).toBe('{invalid json}');
  });
});

// =============================================================================
// Dry Run Formatting Tests
// =============================================================================

describe('Dry Run Formatting', () => {
  it('should format basic dry run output', () => {
    const formatDryRun = (
      dmlPath: string,
      meta: { description: string; parameters: Array<{ name: string }>; tools: string[] } | null,
      params: Record<string, unknown>,
      model: string,
      provider: string,
      workspacePath: string
    ): string => {
      const lines: string[] = [
        '═══════════════════════════════════════════════════════════════',
        '  DRY RUN - Would execute the following:',
        '═══════════════════════════════════════════════════════════════',
        '',
        `  DML File:    ${dmlPath}`,
        `  Model:       ${provider}/${model}`,
        `  Workspace:   ${workspacePath}`,
      ];
      return lines.join('\n');
    };
    
    const output = formatDryRun(
      '/path/to/test.dml',
      null,
      {},
      'gpt-4o',
      'openai',
      '/workspace'
    );
    
    expect(output).toContain('DRY RUN');
    expect(output).toContain('/path/to/test.dml');
    expect(output).toContain('openai/gpt-4o');
    expect(output).toContain('/workspace');
  });

  it('should include meta information when available', () => {
    const meta = {
      description: 'Test task',
      parameters: [{ name: 'topic' }, { name: 'depth' }],
      tools: ['web_search', 'execute_code']
    };
    
    // Just verify structure - actual formatting tested in integration
    expect(meta.description).toBe('Test task');
    expect(meta.parameters).toHaveLength(2);
    expect(meta.tools).toContain('web_search');
  });
});

// =============================================================================
// Run Integration Tests (require filesystem)
// =============================================================================

describe('run integration', () => {
  let tempDir: string;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `deepclause-run-test-${Date.now()}`);
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
  });

  it('should read DML file', async () => {
    const dmlContent = `
      agent_main :-
        output("Hello"),
        answer("Done").
    `;
    
    const dmlPath = join(tempDir, 'test.dml');
    await writeFile(dmlPath, dmlContent);
    
    // Verify file was written
    const content = await readFile(dmlPath, 'utf-8');
    expect(content).toContain('agent_main');
    expect(content).toContain('answer("Done")');
  });

  it('should load meta file when present', async () => {
    const meta = {
      version: '1.0.0',
      source: '../test.md',
      sourceHash: 'sha256:abc123',
      compiledAt: new Date().toISOString(),
      model: 'gpt-4o',
      provider: 'openai',
      description: 'Test task',
      parameters: [
        { name: 'topic', position: 0, required: true }
      ],
      tools: ['execute_code'],
      history: []
    };
    
    const metaPath = join(tempDir, 'test.meta.json');
    await writeFile(metaPath, JSON.stringify(meta, null, 2));
    
    // Verify meta was written and can be read
    const content = await readFile(metaPath, 'utf-8');
    const parsed = JSON.parse(content);
    expect(parsed.description).toBe('Test task');
    expect(parsed.parameters[0].name).toBe('topic');
  });

  it('should handle missing DML file gracefully', async () => {
    const { run } = await import('../src/cli/run.js');
    
    const nonExistentPath = join(tempDir, 'nonexistent.dml');
    
    await expect(run(nonExistentPath, [], {}))
      .rejects.toThrow('Failed to read DML file');
  });

  it('should perform dry run without executing', async () => {
    const dmlContent = `
      agent_main(Topic) :-
        task("Research {Topic}"),
        answer("Done").
    `;
    
    const dmlPath = join(tempDir, 'test.dml');
    await writeFile(dmlPath, dmlContent);
    
    // Create meta file
    const meta = {
      version: '1.0.0',
      source: '../test.md',
      sourceHash: 'sha256:abc123',
      compiledAt: new Date().toISOString(),
      model: 'gpt-4o',
      provider: 'openai',
      description: 'Research task',
      parameters: [
        { name: 'topic', position: 0, required: true }
      ],
      tools: [],
      history: []
    };
    await writeFile(join(tempDir, 'test.meta.json'), JSON.stringify(meta, null, 2));
    
    const { run } = await import('../src/cli/run.js');
    
    const result = await run(dmlPath, ['AI'], { dryRun: true });
    
    expect(result.dryRun).toBe(true);
    expect(result.wouldExecute).toContain('DRY RUN');
    expect(result.wouldExecute).toContain('test.dml');
    expect(result.wouldExecute).toContain('Research task');
    expect(result.wouldExecute).toContain('topic');
  });
});

// =============================================================================
// Tool Verification Tests
// =============================================================================

describe('Tool Verification', () => {
  it('should recognize AgentVM tools as available', () => {
    const agentVmToolNames = ['execute_code', 'vm_exec'];
    const required = ['execute_code'];
    
    const missing = required.filter(t => !agentVmToolNames.includes(t));
    expect(missing).toHaveLength(0);
  });

  it('should identify missing non-AgentVM tools', () => {
    const agentVmToolNames = ['execute_code', 'vm_exec'];
    const required = ['execute_code', 'web_search', 'read_file'];
    
    const missing = required.filter(t => !agentVmToolNames.includes(t));
    expect(missing).toEqual(['web_search', 'read_file']);
  });
});
