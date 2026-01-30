/**
 * Tests for Compilation Module
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { mkdir, writeFile, rm, readFile } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import {
  extractToolDependencies,
  extractParameters,
  extractDescription,
  validateDMLSyntax
} from '../src/cli/compile.js';
import { buildCompilationPrompt, buildUserMessage, buildToolsTable } from '../src/cli/prompt.js';
import type { Tool } from '../src/cli/tools.js';

// =============================================================================
// Tool Dependency Extraction Tests
// =============================================================================

describe('extractToolDependencies', () => {
  it('should extract simple exec calls', () => {
    const dml = `
      agent_main :-
        exec(search_web(Query), Result),
        exec(save_file(Path, Content), _).
    `;
    expect(extractToolDependencies(dml)).toEqual(['save_file', 'search_web']);
  });

  it('should handle exec with different spacing', () => {
    const dml = `
      exec( fetch_url( URL ), Data ),
      exec  (  parse_json  (  Input  )  ,  Output  )
    `;
    expect(extractToolDependencies(dml)).toEqual(['fetch_url', 'parse_json']);
  });

  it('should deduplicate tool names', () => {
    const dml = `
      exec(search_web(Q1), R1),
      exec(search_web(Q2), R2),
      exec(search_web(Q3), R3)
    `;
    expect(extractToolDependencies(dml)).toEqual(['search_web']);
  });

  it('should return empty array when no exec calls', () => {
    const dml = `
      agent_main :-
        think("What should I do?", Plan),
        print(Plan).
    `;
    expect(extractToolDependencies(dml)).toEqual([]);
  });

  it('should handle multiline exec calls', () => {
    const dml = `
      exec(
        fetch_data(
          url(URL),
          headers(Headers)
        ),
        Response
      )
    `;
    expect(extractToolDependencies(dml)).toEqual(['fetch_data']);
  });

  it('should not extract tool/3 definitions (internal wrappers)', () => {
    const dml = `
      tool(search, "Search the web", search_impl).
      search_impl(Query, Result) :-
        exec(brave_search(Query), Result).
    `;
    expect(extractToolDependencies(dml)).toEqual(['brave_search']);
  });

  it('should handle underscore and number in tool names', () => {
    const dml = `
      exec(fetch_v2(URL), R1),
      exec(api_call_3(Args), R2),
      exec(tool_name_123(X), R3)
    `;
    expect(extractToolDependencies(dml)).toEqual(['api_call_3', 'fetch_v2', 'tool_name_123']);
  });
});

// =============================================================================
// Parameter Extraction Tests
// =============================================================================

describe('extractParameters', () => {
  it('should extract single parameter', () => {
    const dml = `agent_main(Query) :- search(Query, Result).`;
    expect(extractParameters(dml)).toEqual([
      { name: 'query', position: 0, required: true }
    ]);
  });

  it('should extract multiple parameters sorted alphabetically', () => {
    const dml = `agent_main(URL, Depth, Format) :- crawl(URL, Depth, Format).`;
    const params = extractParameters(dml);
    expect(params).toHaveLength(3);
    expect(params[0].name).toBe('depth');
    expect(params[1].name).toBe('format');
    expect(params[2].name).toBe('u_r_l'); // URL becomes u_r_l due to uppercase handling
  });

  it('should handle no parameters', () => {
    const dml = `agent_main :- do_something.`;
    expect(extractParameters(dml)).toEqual([]);
  });

  it('should convert PascalCase to snake_case', () => {
    const dml = `agent_main(SearchQuery, MaxResults, OutputFormat) :- process.`;
    const params = extractParameters(dml);
    expect(params.map(p => p.name)).toEqual(['max_results', 'output_format', 'search_query']);
  });

  it('should handle complex whitespace', () => {
    const dml = `
      agent_main(
        URL ,
        Depth ,
        Format
      ) :- process.
    `;
    const params = extractParameters(dml);
    expect(params).toHaveLength(3);
  });
});

// =============================================================================
// Description Extraction Tests
// =============================================================================

describe('extractDescription', () => {
  it('should extract first heading', () => {
    const markdown = `# Search the Web\n\nThis task searches the web.`;
    expect(extractDescription(markdown)).toBe('Search the Web');
  });

  it('should handle multiple headings', () => {
    const markdown = `# Main Title\n\n## Subtitle\n\nContent`;
    expect(extractDescription(markdown)).toBe('Main Title');
  });

  it('should fall back to first paragraph if no heading', () => {
    const markdown = `This is a task that does something interesting.`;
    expect(extractDescription(markdown)).toBe('This is a task that does something interesting.');
  });

  it('should truncate long descriptions', () => {
    const longText = 'A'.repeat(150);
    const markdown = longText;
    expect(extractDescription(markdown).length).toBeLessThanOrEqual(100);
  });

  it('should return default for empty markdown', () => {
    expect(extractDescription('')).toBe('No description');
    expect(extractDescription('   \n   ')).toBe('No description');
  });
});

// =============================================================================
// DML Validation Tests
// =============================================================================

describe('validateDMLSyntax', () => {
  it('should accept valid DML', () => {
    const dml = `
      agent_main(Query) :-
        think("Analyzing query", Plan),
        exec(search(Query), Results),
        answer(Results).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it('should detect missing agent_main', () => {
    const dml = `
      helper(X, Y) :- process(X, Y).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(false);
    expect(result.errors).toContain('Missing agent_main predicate');
  });

  it('should detect unbalanced parentheses', () => {
    const dml = `
      agent_main(Query) :-
        exec(search(Query, Results).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(false);
    expect(result.errors).toContain('Unbalanced parentheses');
  });

  it('should detect unbalanced brackets', () => {
    const dml = `
      agent_main :-
        List = [a, b, c,
        process(List).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(false);
    expect(result.errors).toContain('Unbalanced brackets');
  });

  it('should detect unclosed strings', () => {
    const dml = `
      agent_main :-
        think("Unclosed string, Result).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(false);
    expect(result.errors).toContain('Unclosed string literal');
  });

  it('should handle escaped quotes in strings', () => {
    const dml = `
      agent_main :-
        think("He said \\"hello\\"", Result).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(true);
  });

  it('should handle nested parentheses', () => {
    const dml = `
      agent_main :-
        exec(api_call(nested(data(x, y, z))), Result),
        process(Result).
    `;
    const result = validateDMLSyntax(dml);
    expect(result.valid).toBe(true);
  });
});

// =============================================================================
// Prompt Building Tests
// =============================================================================

describe('buildToolsTable', () => {
  it('should format tools as markdown table', () => {
    const tools: Tool[] = [
      { name: 'search', description: 'Search the web', provider: 'mcp' },
      { name: 'execute_code', description: 'Run code', provider: 'agentvm' }
    ];
    const table = buildToolsTable(tools);
    expect(table).toContain('| Tool |');
    expect(table).toContain('`search`'); // Tool names are in backticks
    expect(table).toContain('`execute_code`');
  });

  it('should handle empty tools list', () => {
    const table = buildToolsTable([]);
    expect(table).toContain('No additional tools configured');
  });
});

describe('buildCompilationPrompt', () => {
  it('should include DML reference', () => {
    const prompt = buildCompilationPrompt([]);
    expect(prompt).toContain('agent_main');
    expect(prompt).toContain('exec/2');
    expect(prompt).toContain('task('); // task is the main predicate, not think
  });

  it('should include tools table when tools provided', () => {
    const tools: Tool[] = [
      { name: 'search', description: 'Search', provider: 'mcp' }
    ];
    const prompt = buildCompilationPrompt(tools);
    expect(prompt).toContain('`search`'); // Tool names wrapped in backticks
  });

  it('should include patterns and guidelines', () => {
    const prompt = buildCompilationPrompt([]);
    expect(prompt).toContain('Pattern');
    expect(prompt).toContain('Guidelines');
  });
});

describe('buildUserMessage', () => {
  it('should wrap markdown in delimiters', () => {
    const markdown = '# Test Task\n\nDo something';
    const message = buildUserMessage(markdown);
    expect(message).toContain('---'); // Uses horizontal rule delimiters
    expect(message).toContain('# Test Task');
    expect(message).toContain('DML'); // Should mention DML
  });
});

// =============================================================================
// Integration Tests (Mock LLM)
// =============================================================================

describe('compile integration', () => {
  let tempDir: string;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `deepclause-test-${Date.now()}`);
    await mkdir(tempDir, { recursive: true });
    await mkdir(join(tempDir, '.deepclause'), { recursive: true });
    
    // Create minimal config
    await writeFile(
      join(tempDir, '.deepclause', 'config.json'),
      JSON.stringify({
        model: 'gpt-4o',
        provider: 'openai'
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

  it('should skip compilation when source unchanged', async () => {
    // Create output directory and existing meta
    const outputDir = join(tempDir, 'output');
    await mkdir(outputDir, { recursive: true });
    
    const sourceContent = '# Test\nDo something';
    const sourceHash = 'sha256:' + 'test'.repeat(4); // Fake hash
    
    // Write source
    const sourcePath = join(tempDir, 'test.md');
    await writeFile(sourcePath, sourceContent);
    
    // Write existing meta with matching hash (we'll compute real hash)
    const crypto = await import('crypto');
    const realHash = 'sha256:' + crypto.createHash('sha256').update(sourceContent).digest('hex').substring(0, 16);
    
    await writeFile(
      join(outputDir, 'test.meta.json'),
      JSON.stringify({
        version: '1.0.0',
        source: '../test.md',
        sourceHash: realHash,
        compiledAt: new Date().toISOString(),
        model: 'gpt-4o',
        provider: 'openai',
        description: 'Test',
        parameters: [],
        tools: [],
        history: []
      })
    );
    
    // Import compile function
    const { compile } = await import('../src/cli/compile.js');
    
    // Should skip since hash matches
    const result = await compile(sourcePath, outputDir, { force: false });
    expect(result.skipped).toBe(true);
  });
});
