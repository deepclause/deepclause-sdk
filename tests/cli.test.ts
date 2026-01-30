/**
 * DeepClause CLI Test Suite
 * 
 * Tests for the command-line interface including:
 * - Command parsing and validation
 * - Compilation (MD to DML)
 * - Execution (DML runtime)
 * - Configuration management
 * - Tool dependency resolution
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { vol } from 'memfs';
import path from 'path';

// Mock filesystem for isolated tests
vi.mock('fs', async () => {
  const memfs = await import('memfs');
  return memfs.fs;
});

vi.mock('fs/promises', async () => {
  const memfs = await import('memfs');
  return memfs.fs.promises;
});

// =============================================================================
// Test Fixtures
// =============================================================================

const SAMPLE_TASK_MD = `# Research Agent

Research a topic and generate a comprehensive report.

## Parameters

- \`topic\` (required): The topic to research
- \`depth\` (optional, default: "standard"): Research depth (quick/standard/deep)

## Requirements

- Web search capability
- File writing for report output

## Behavior

1. Search for information about the topic
2. Analyze and synthesize findings
3. Generate a structured report
4. Save report to file

## Edge Cases

- If no results found, report that clearly
- Handle rate limiting gracefully
`;

const EXPECTED_DML_STRUCTURE = `% research_agent.dml
% Research a topic and generate a comprehensive report

agent_main(Depth, Topic) :-
    system("You are a thorough research assistant."),
    
    output("Researching topic..."),
    task("Research {Topic} thoroughly. Store findings in Findings.", Findings),
    
    output("Generating report..."),
    format(string(ReportTask), "Create a ~w report based on: ~w", [Depth, Findings]),
    task(ReportTask, Report),
    
    format(string(Filename), "~w_report.md", [Topic]),
    write_file(Filename, Report),
    
    answer("Research complete!").
`;

const SAMPLE_CONFIG = {
  model: 'gpt-4o',
  providers: {
    openai: { apiKey: 'test-key' }
  },
  mcp: {
    servers: {
      'brave-search': {
        command: 'npx',
        args: ['-y', '@anthropic/mcp-brave-search'],
        env: { BRAVE_API_KEY: 'test-brave-key' }
      }
    }
  }
};

const SAMPLE_META = {
  version: '1.0.0',
  source: 'tasks/research.md',
  sourceHash: 'sha256:abc123',
  compiledAt: '2026-01-29T10:30:00Z',
  model: 'gpt-4o',
  description: 'Research a topic and generate a comprehensive report',
  parameters: [
    { name: 'topic', description: 'The topic to research', required: true },
    { name: 'depth', description: 'Research depth', default: 'standard' }
  ],
  tools: ['web_search', 'vm_exec'],
  history: [
    { version: 1, timestamp: '2026-01-29T10:30:00Z', sourceHash: 'sha256:abc123', model: 'gpt-4o' }
  ]
};

// =============================================================================
// Configuration Tests
// =============================================================================

describe('Configuration', () => {
  beforeEach(() => {
    vol.reset();
  });

  describe('deepclause init', () => {
    it('should create default config file', async () => {
      const { initConfig } = await import('../src/cli/config');
      
      await initConfig('/workspace');
      
      expect(vol.existsSync('/workspace/.deepclause/config.json')).toBe(true);
      const config = JSON.parse(vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string);
      expect(config).toHaveProperty('model');
      expect(config).toHaveProperty('providers');
    });

    it('should not overwrite existing config without --force', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'existing' })
      });
      
      const { initConfig } = await import('../src/cli/config');
      
      await expect(initConfig('/workspace')).rejects.toThrow(/already exists/);
    });

    it('should overwrite existing config with --force', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'existing' })
      });
      
      const { initConfig } = await import('../src/cli/config');
      
      await initConfig('/workspace', { force: true });
      
      const config = JSON.parse(vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string);
      expect(config.model).not.toBe('existing');
    });
  });

  describe('deepclause set-model', () => {
    it('should update model in config', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG)
      });
      
      const { setModel } = await import('../src/cli/config');
      
      await setModel('/workspace', 'claude-3-opus');
      
      const config = JSON.parse(vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string);
      expect(config.model).toBe('claude-3-opus');
    });

    it('should validate model format', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG)
      });
      
      const { setModel } = await import('../src/cli/config');
      
      await expect(setModel('/workspace', '')).rejects.toThrow(/invalid model/i);
    });
  });

  describe('deepclause show-model', () => {
    it('should display current model', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG)
      });
      
      const { showModel } = await import('../src/cli/config');
      
      const result = await showModel('/workspace');
      expect(result).toBe('gpt-4o');
    });
  });

  describe('Config validation', () => {
    it('should validate required fields', async () => {
      const { validateConfig } = await import('../src/cli/config');
      
      expect(() => validateConfig({})).toThrow(/model.*required/i);
    });

    it('should validate MCP server config', async () => {
      const { validateConfig } = await import('../src/cli/config');
      
      const invalidConfig = {
        model: 'gpt-4o',
        mcp: {
          servers: {
            'test': {} // Missing command
          }
        }
      };
      
      expect(() => validateConfig(invalidConfig)).toThrow(/command.*required/i);
    });
  });
});

// =============================================================================
// Compilation Tests
// =============================================================================

describe('Compilation', () => {
  beforeEach(() => {
    vol.reset();
  });

  describe('deepclause compile', () => {
    it('should compile markdown to DML', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/tasks/research.md': SAMPLE_TASK_MD
      });
      
      const { compile } = await import('../src/cli/compile');
      
      // Mock the LLM call
      vi.mock('../src/sdk', () => ({
        generateDML: vi.fn().mockResolvedValue(EXPECTED_DML_STRUCTURE)
      }));
      
      await compile('/workspace/tasks/research.md', '/workspace/.deepclause/tools');
      
      expect(vol.existsSync('/workspace/.deepclause/tools/research.dml')).toBe(true);
      expect(vol.existsSync('/workspace/.deepclause/tools/research.meta.json')).toBe(true);
    });

    it('should create meta file with correct structure', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/tasks/research.md': SAMPLE_TASK_MD
      });
      
      const { compile } = await import('../src/cli/compile');
      
      await compile('/workspace/tasks/research.md', '/workspace/.deepclause/tools');
      
      const meta = JSON.parse(
        vol.readFileSync('/workspace/.deepclause/tools/research.meta.json', 'utf-8') as string
      );
      
      expect(meta).toHaveProperty('version');
      expect(meta).toHaveProperty('source');
      expect(meta).toHaveProperty('sourceHash');
      expect(meta).toHaveProperty('compiledAt');
      expect(meta).toHaveProperty('model');
      expect(meta).toHaveProperty('parameters');
      expect(meta).toHaveProperty('tools');
      expect(meta).toHaveProperty('history');
    });

    it('should extract tool dependencies from generated DML', async () => {
      const dmlWithTools = `
agent_main(Topic) :-
    exec(web_search(query: Topic), Results),
    exec(vm_exec(command: "echo hello"), Output),
    answer("Done").
`;
      
      const { extractToolDependencies } = await import('../src/cli/compile');
      
      const tools = extractToolDependencies(dmlWithTools);
      
      expect(tools).toContain('web_search');
      expect(tools).toContain('vm_exec');
      expect(tools).toHaveLength(2);
    });

    it('should not include DML tool wrappers as dependencies', async () => {
      const dmlWithWrapper = `
tool(search(Query, Results), "Search wrapper") :-
    exec(web_search(query: Query), Results).

agent_main(Topic) :-
    tool(search(Topic, Results)),
    answer("Done").
`;
      
      const { extractToolDependencies } = await import('../src/cli/compile');
      
      const tools = extractToolDependencies(dmlWithWrapper);
      
      // Should include web_search (external) but not 'search' (DML wrapper)
      expect(tools).toContain('web_search');
      expect(tools).not.toContain('search');
    });

    it('should append to history on recompile', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/tasks/research.md': SAMPLE_TASK_MD,
        '/workspace/.deepclause/tools/research.dml': 'old content',
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { compile } = await import('../src/cli/compile');
      
      await compile('/workspace/tasks/research.md', '/workspace/.deepclause/tools');
      
      const meta = JSON.parse(
        vol.readFileSync('/workspace/.deepclause/tools/research.meta.json', 'utf-8') as string
      );
      
      expect(meta.history).toHaveLength(2);
      expect(meta.history[1].version).toBe(2);
    });

    it('should validate-only without writing files', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/tasks/research.md': SAMPLE_TASK_MD
      });
      
      const { compile } = await import('../src/cli/compile');
      
      const result = await compile('/workspace/tasks/research.md', '/workspace/.deepclause/tools', {
        validateOnly: true
      });
      
      expect(result.valid).toBe(true);
      expect(vol.existsSync('/workspace/.deepclause/tools/research.dml')).toBe(false);
    });

    it('should skip compilation if source unchanged', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/tasks/research.md': SAMPLE_TASK_MD,
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify({
          ...SAMPLE_META,
          sourceHash: 'sha256:current-hash' // Matches current
        })
      });
      
      const { compile } = await import('../src/cli/compile');
      
      const result = await compile('/workspace/tasks/research.md', '/workspace/.deepclause/tools');
      
      expect(result.skipped).toBe(true);
    });

    it('should force recompile with --force', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/tasks/research.md': SAMPLE_TASK_MD,
        '/workspace/.deepclause/tools/research.dml': 'old content',
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { compile } = await import('../src/cli/compile');
      
      const result = await compile('/workspace/tasks/research.md', '/workspace/.deepclause/tools', {
        force: true
      });
      
      expect(result.skipped).toBe(false);
    });
  });

  describe('Prompt construction', () => {
    it('should include available tools in prompt', async () => {
      const { buildCompilationPrompt } = await import('../src/cli/compile');
      
      const tools = [
        { name: 'web_search', description: 'Search the web', provider: 'brave-search' },
        { name: 'vm_exec', description: 'Run shell commands', provider: 'agentvm' }
      ];
      
      const prompt = buildCompilationPrompt(SAMPLE_TASK_MD, tools);
      
      expect(prompt).toContain('web_search');
      expect(prompt).toContain('vm_exec');
      expect(prompt).toContain('brave-search');
      expect(prompt).toContain('agentvm');
    });

    it('should always include AgentVM tools', async () => {
      const { buildCompilationPrompt } = await import('../src/cli/compile');
      
      const prompt = buildCompilationPrompt(SAMPLE_TASK_MD, []);
      
      expect(prompt).toContain('vm_exec');
    });
  });
});

// =============================================================================
// Execution Tests
// =============================================================================

describe('Execution', () => {
  beforeEach(() => {
    vol.reset();
  });

  describe('deepclause run', () => {
    it('should execute DML program with positional args', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { run } = await import('../src/cli/run');
      
      const result = await run('/workspace/.deepclause/tools/research', ['AI agents']);
      
      expect(result).toHaveProperty('output');
      expect(result).toHaveProperty('answer');
    });

    it('should execute DML program with named params', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { run } = await import('../src/cli/run');
      
      const result = await run('/workspace/.deepclause/tools/research', [], {
        params: { topic: 'AI agents', depth: 'deep' }
      });
      
      expect(result).toHaveProperty('output');
    });

    it('should fail if required tools unavailable', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({
          model: 'gpt-4o',
          mcp: { servers: {} } // No MCP servers configured
        }),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify({
          ...SAMPLE_META,
          tools: ['web_search'] // Requires web_search but not configured
        })
      });
      
      const { run } = await import('../src/cli/run');
      
      await expect(run('/workspace/.deepclause/tools/research', ['topic']))
        .rejects.toThrow(/missing.*tools.*web_search/i);
    });

    it('should allow AgentVM tools without explicit config', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({
          model: 'gpt-4o',
          mcp: { servers: {} }
        }),
        '/workspace/.deepclause/tools/code_runner.dml': `
agent_main(Code) :-
    exec(vm_exec(command: Code), Result),
    answer(Result).
`,
        '/workspace/.deepclause/tools/code_runner.meta.json': JSON.stringify({
          ...SAMPLE_META,
          tools: ['vm_exec'] // AgentVM tool, always available
        })
      });
      
      const { run } = await import('../src/cli/run');
      
      // Should not throw - vm_exec is built-in
      await expect(run('/workspace/.deepclause/tools/code_runner', ['echo hello']))
        .resolves.toBeDefined();
    });

    it('should save execution trace with --trace', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { run } = await import('../src/cli/run');
      
      await run('/workspace/.deepclause/tools/research', ['AI'], {
        trace: '/workspace/trace.json'
      });
      
      expect(vol.existsSync('/workspace/trace.json')).toBe(true);
      const trace = JSON.parse(vol.readFileSync('/workspace/trace.json', 'utf-8') as string);
      expect(trace).toHaveProperty('steps');
      expect(trace).toHaveProperty('duration');
    });

    it('should dry-run without execution', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { run } = await import('../src/cli/run');
      
      const result = await run('/workspace/.deepclause/tools/research', ['AI'], {
        dryRun: true
      });
      
      expect(result.dryRun).toBe(true);
      expect(result.wouldExecute).toContain('agent_main');
    });
  });

  describe('Tool resolution', () => {
    it('should resolve MCP tools from config', async () => {
      const { resolveTools } = await import('../src/cli/tools');
      
      const tools = await resolveTools(SAMPLE_CONFIG, ['web_search']);
      
      expect(tools).toHaveProperty('web_search');
      expect(tools.web_search.provider).toBe('brave-search');
    });

    it('should resolve AgentVM tools without config', async () => {
      const { resolveTools } = await import('../src/cli/tools');
      
      const tools = await resolveTools({ model: 'gpt-4o' }, ['vm_exec']);
      
      expect(tools).toHaveProperty('vm_exec');
      expect(tools.vm_exec.provider).toBe('agentvm');
    });

    it('should report missing tools', async () => {
      const { resolveTools } = await import('../src/cli/tools');
      
      await expect(resolveTools({ model: 'gpt-4o' }, ['nonexistent_tool']))
        .rejects.toThrow(/nonexistent_tool/);
    });
  });
});

// =============================================================================
// Tool Listing Tests
// =============================================================================

describe('Tool Listing', () => {
  beforeEach(() => {
    vol.reset();
  });

  describe('deepclause list-tools', () => {
    it('should list all available tools', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG)
      });
      
      const { listTools } = await import('../src/cli/tools');
      
      const tools = await listTools('/workspace');
      
      // Should include AgentVM tools
      expect(tools.some(t => t.name === 'vm_exec')).toBe(true);
      
      // Should include MCP tools (if servers respond)
      // This would require mocking MCP client
    });

    it('should output JSON with --json flag', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG)
      });
      
      const { listTools } = await import('../src/cli/tools');
      
      const result = await listTools('/workspace', { json: true });
      
      expect(() => JSON.parse(result)).not.toThrow();
    });
  });

  describe('deepclause list-commands', () => {
    it('should list compiled DML commands', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META),
        '/workspace/.deepclause/tools/analysis.dml': 'agent_main :- answer("done").',
        '/workspace/.deepclause/tools/analysis.meta.json': JSON.stringify({
          ...SAMPLE_META,
          description: 'Analyze data'
        })
      });
      
      const { listCommands } = await import('../src/cli/commands');
      
      const commands = await listCommands('/workspace');
      
      expect(commands).toHaveLength(2);
      expect(commands.some(c => c.name === 'research')).toBe(true);
      expect(commands.some(c => c.name === 'analysis')).toBe(true);
    });

    it('should show detailed info with --detailed', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
        '/workspace/.deepclause/tools/research.dml': EXPECTED_DML_STRUCTURE,
        '/workspace/.deepclause/tools/research.meta.json': JSON.stringify(SAMPLE_META)
      });
      
      const { listCommands } = await import('../src/cli/commands');
      
      const commands = await listCommands('/workspace', { detailed: true });
      
      expect(commands[0]).toHaveProperty('parameters');
      expect(commands[0]).toHaveProperty('tools');
      expect(commands[0].tools).toContain('web_search');
    });
  });
});

// =============================================================================
// Shared Workspace Tests
// =============================================================================

describe('Shared Workspace', () => {
  beforeEach(() => {
    vol.reset();
  });

  it('should mount workspace identically in DML and AgentVM', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/data.csv': 'a,b,c\n1,2,3',
      '/workspace/.deepclause/tools/data_analysis.dml': `
agent_main :-
    read_file("data.csv", CsvContent),
    exec(vm_exec(command: "python3 -c \\"import pandas; df = pandas.read_csv('data.csv'); print(df.shape)\\""), Result),
    answer(Result).
`,
      '/workspace/.deepclause/tools/data_analysis.meta.json': JSON.stringify({
        ...SAMPLE_META,
        tools: ['vm_exec']
      })
    });
    
    const { run } = await import('../src/cli/run');
    
    const result = await run('/workspace/.deepclause/tools/data_analysis', [], {
      workspace: '/workspace'
    });
    
    // Both DML read_file and Python should see the same file
    expect(result.answer).toContain('1, 3'); // Shape (1, 3)
  });

  it('should allow cross-environment file sharing', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/.deepclause/tools/file_test.dml': `
agent_main :-
    % Write from Python
    exec(vm_exec(command: "python3 -c \\"open('output.txt', 'w').write('from python')\\""), _),
    % Read from DML
    read_file("output.txt", Content),
    answer(Content).
`,
      '/workspace/.deepclause/tools/file_test.meta.json': JSON.stringify({
        ...SAMPLE_META,
        tools: ['vm_exec']
      })
    });
    
    const { run } = await import('../src/cli/run');
    
    const result = await run('/workspace/.deepclause/tools/file_test', [], {
      workspace: '/workspace'
    });
    
    expect(result.answer).toBe('from python');
  });
});

// =============================================================================
// Error Handling Tests
// =============================================================================

describe('Error Handling', () => {
  beforeEach(() => {
    vol.reset();
  });

  it('should handle missing config file', async () => {
    vol.fromJSON({});
    
    const { loadConfig } = await import('../src/cli/config');
    
    await expect(loadConfig('/workspace')).rejects.toThrow(/config.*not found/i);
  });

  it('should handle invalid DML syntax', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/.deepclause/tools/invalid.dml': 'this is not valid prolog !!!',
      '/workspace/.deepclause/tools/invalid.meta.json': JSON.stringify(SAMPLE_META)
    });
    
    const { run } = await import('../src/cli/run');
    
    await expect(run('/workspace/.deepclause/tools/invalid', []))
      .rejects.toThrow(/syntax error/i);
  });

  it('should handle missing DML file', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG)
    });
    
    const { run } = await import('../src/cli/run');
    
    await expect(run('/workspace/.deepclause/tools/nonexistent', []))
      .rejects.toThrow(/not found/i);
  });

  it('should handle LLM API errors during compilation', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/tasks/test.md': SAMPLE_TASK_MD
    });
    
    vi.mock('../src/sdk', () => ({
      generateDML: vi.fn().mockRejectedValue(new Error('API rate limit exceeded'))
    }));
    
    const { compile } = await import('../src/cli/compile');
    
    await expect(compile('/workspace/tasks/test.md', '/workspace/.deepclause/tools'))
      .rejects.toThrow(/API rate limit/i);
  });

  it('should handle MCP server connection failures', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify({
        ...SAMPLE_CONFIG,
        mcp: {
          servers: {
            'broken-server': {
              command: 'nonexistent-command',
              args: []
            }
          }
        }
      })
    });
    
    const { listTools } = await import('../src/cli/tools');
    
    // Should not throw, but should report the error in output
    const tools = await listTools('/workspace');
    expect(tools.some(t => t.error)).toBe(true);
  });

  it('should handle AgentVM execution failures', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/.deepclause/tools/failing.dml': `
agent_main :-
    exec(vm_exec(command: "python3 -c \\"raise Exception('test error')\\""), Result),
    answer(Result).
`,
      '/workspace/.deepclause/tools/failing.meta.json': JSON.stringify({
        ...SAMPLE_META,
        tools: ['vm_exec']
      })
    });
    
    const { run } = await import('../src/cli/run');
    
    const result = await run('/workspace/.deepclause/tools/failing', []);
    
    // Should capture the error in stderr, not crash
    expect(result.error).toContain('test error');
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('Integration', () => {
  beforeEach(() => {
    vol.reset();
  });

  it('should complete full compile-and-run workflow', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/tasks/hello.md': `# Hello Agent

Say hello to the world.

## Parameters

- \`name\` (required): Name to greet

## Behavior

Greet the user by name.
`
    });
    
    const { compile } = await import('../src/cli/compile');
    const { run } = await import('../src/cli/run');
    
    // Compile
    await compile('/workspace/tasks/hello.md', '/workspace/.deepclause/tools');
    
    expect(vol.existsSync('/workspace/.deepclause/tools/hello.dml')).toBe(true);
    
    // Run
    const result = await run('/workspace/.deepclause/tools/hello', ['World']);
    
    expect(result.answer).toContain('World');
  });

  it('should handle batch compilation', async () => {
    vol.fromJSON({
      '/workspace/.deepclause/config.json': JSON.stringify(SAMPLE_CONFIG),
      '/workspace/tasks/task1.md': '# Task 1\nDo task 1.',
      '/workspace/tasks/task2.md': '# Task 2\nDo task 2.',
      '/workspace/tasks/task3.md': '# Task 3\nDo task 3.'
    });
    
    const { compileAll } = await import('../src/cli/compile');
    
    const results = await compileAll('/workspace/tasks', '/workspace/.deepclause/tools');
    
    expect(results.compiled).toBe(3);
    expect(results.failed).toBe(0);
    expect(vol.existsSync('/workspace/.deepclause/tools/task1.dml')).toBe(true);
    expect(vol.existsSync('/workspace/.deepclause/tools/task2.dml')).toBe(true);
    expect(vol.existsSync('/workspace/.deepclause/tools/task3.dml')).toBe(true);
  });
});

// =============================================================================
// DML Parser Tests
// =============================================================================

describe('DML Parser', () => {
  describe('extractToolDependencies', () => {
    it('should extract exec calls', () => {
      const { extractToolDependencies } = require('../src/cli/compile');
      
      const dml = `
agent_main :-
    exec(web_search(query: "test"), R1),
    exec(vm_exec(command: "echo hello"), R2).
`;
      
      const tools = extractToolDependencies(dml);
      expect(tools).toEqual(['web_search', 'vm_exec']);
    });

    it('should handle nested exec in tool definitions', () => {
      const { extractToolDependencies } = require('../src/cli/compile');
      
      const dml = `
tool(my_search(Q, R), "wrapper") :-
    exec(web_search(query: Q), R).

agent_main :-
    tool(my_search("test", R)).
`;
      
      const tools = extractToolDependencies(dml);
      expect(tools).toEqual(['web_search']);
    });

    it('should deduplicate tool names', () => {
      const { extractToolDependencies } = require('../src/cli/compile');
      
      const dml = `
agent_main :-
    exec(web_search(query: "a"), R1),
    exec(web_search(query: "b"), R2),
    exec(web_search(query: "c"), R3).
`;
      
      const tools = extractToolDependencies(dml);
      expect(tools).toEqual(['web_search']);
    });
  });

  describe('extractParameters', () => {
    it('should extract parameters from agent_main signature', () => {
      const { extractParameters } = require('../src/cli/compile');
      
      const dml = `agent_main(Topic, Depth) :- ...`;
      
      const params = extractParameters(dml);
      expect(params).toEqual([
        { name: 'depth', position: 0 },  // alphabetical
        { name: 'topic', position: 1 }
      ]);
    });

    it('should handle no parameters', () => {
      const { extractParameters } = require('../src/cli/compile');
      
      const dml = `agent_main :- answer("done").`;
      
      const params = extractParameters(dml);
      expect(params).toEqual([]);
    });
  });
});
