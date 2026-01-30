/**
 * DeepClause CLI - Execution Module
 * 
 * Executes compiled DML programs with full tool support.
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import { createDeepClause } from '../sdk.js';
import type { DMLEvent, ToolDefinition, JsonSchema } from '../types.js';
import { loadConfig, type Config, type Provider } from './config.js';
import { getAgentVMTools, type Tool } from './tools.js';
import { webSearch, newsSearch } from './search.js';
import type { MetaFile } from './compile.js';

// =============================================================================
// Types
// =============================================================================

export interface RunOptions {
  workspace?: string;
  verbose?: boolean;
  stream?: boolean;
  headless?: boolean;
  trace?: string;
  dryRun?: boolean;
  model?: string;
  provider?: Provider;
  params?: Record<string, string>;
}

export interface RunResult {
  output: string[];
  answer?: string;
  error?: string;
  dryRun?: boolean;
  wouldExecute?: string;
  trace?: object;
  events?: DMLEvent[];
}

// =============================================================================
// Main Run Function
// =============================================================================

/**
 * Execute a compiled DML program
 */
export async function run(
  file: string,
  args: string[],
  options: RunOptions = {}
): Promise<RunResult> {
  const absolutePath = path.resolve(file);
  
  // Config is always loaded from current working directory
  const configRoot = process.cwd();
  
  // Load DML file
  let dmlCode: string;
  try {
    dmlCode = await fs.readFile(absolutePath, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to read DML file: ${absolutePath}`);
  }

  // Try to load meta file
  const metaPath = absolutePath.replace(/\.dml$/, '.meta.json');
  let meta: MetaFile | null = null;
  try {
    const metaContent = await fs.readFile(metaPath, 'utf-8');
    meta = JSON.parse(metaContent) as MetaFile;
  } catch {
    // Meta file is optional
  }

  // Load config from cwd
  const config = await loadConfig(configRoot);
  const model = options.model || config.model;
  const provider = options.provider || config.provider;

  // Resolve workspace path
  const workspacePath = options.workspace 
    ? path.resolve(options.workspace)
    : path.resolve(config.workspace || './workspace');

  // Ensure workspace exists
  await fs.mkdir(workspacePath, { recursive: true });

  // Dry run mode - show what would be executed
  if (options.dryRun) {
    const params = buildParams(args, options.params, meta);
    return {
      output: [],
      dryRun: true,
      wouldExecute: formatDryRun(absolutePath, meta, params, model, provider, workspacePath)
    };
  }

  // Verify required tools are available
  if (meta?.tools && meta.tools.length > 0) {
    const toolCheck = await verifyToolsAvailable(config, meta.tools);
    if (!toolCheck.available) {
      throw new Error(`Missing required tools: ${toolCheck.missing.join(', ')}. ` +
        `Configure MCP servers or check tool names.`);
    }
  }

  // Build params from args and options
  const params = buildParams(args, options.params, meta);

  // Create SDK instance
  const sdk = await createDeepClause({
    model,
    provider,
    trace: !!options.trace,
    streaming: options.stream,
    debug: options.verbose
  });

  // Register tools from MCP servers and AgentVM
  await registerTools(sdk, config, options.verbose);

  // Execute DML
  const result: RunResult = {
    output: [],
    events: []
  };

  try {
    for await (const event of sdk.runDML(dmlCode, {
      params,
      args,
      workspacePath
    })) {
      result.events?.push(event);

      switch (event.type) {
        case 'output':
          if (event.content) {
            result.output.push(event.content);
            if (!options.headless) {
              console.log(event.content);
            }
          }
          break;

        case 'stream':
          // Real-time streaming of LLM responses
          if (options.stream && !options.headless && event.content) {
            process.stdout.write(event.content);
          }
          // Add newline when stream chunk is done
          if (options.stream && !options.headless && event.done) {
            process.stdout.write('\n');
          }
          break;

        case 'log':
          if (options.verbose && event.content && !options.headless) {
            console.log(`[log] ${event.content}`);
          }
          break;

        case 'answer':
          result.answer = event.content;
          break;

        case 'error':
          result.error = event.content;
          break;

        case 'finished':
          if (event.trace) {
            result.trace = event.trace;
          }
          break;

        case 'input_required':
          // For CLI, we'd need to handle stdin - for now just skip
          if (options.verbose) {
            console.log(`[input_required] ${event.prompt}`);
          }
          break;
      }
    }

    // Save trace if requested
    if (options.trace && result.trace) {
      const tracePath = path.resolve(options.trace);
      await fs.writeFile(tracePath, JSON.stringify(result.trace, null, 2) + '\n');
      if (options.verbose) {
        console.log(`Trace saved to: ${tracePath}`);
      }
    }

    return result;

  } finally {
    await sdk.dispose();
  }
}

// =============================================================================
// Parameter Building
// =============================================================================

/**
 * Build params dict from positional args, named params, and meta info
 */
function buildParams(
  args: string[],
  namedParams: Record<string, string> | undefined,
  meta: MetaFile | null
): Record<string, unknown> {
  const params: Record<string, unknown> = {};

  // Always store raw args for fallback (use 'args' not '_args' - underscore is special in Prolog)
  if (args.length > 0) {
    params['args'] = args.map(parseArgValue);
  }

  // If we have meta info with parameters, map positional args by name
  if (meta?.parameters && args.length > 0) {
    // Parameters are sorted by position in meta
    const sortedParams = [...meta.parameters].sort((a, b) => a.position - b.position);
    
    for (let i = 0; i < args.length && i < sortedParams.length; i++) {
      const param = sortedParams[i];
      params[param.name] = parseArgValue(args[i]);
    }
    
    // If more args than meta params, add them with generic names
    for (let i = sortedParams.length; i < args.length; i++) {
      params[`arg${i + 1}`] = parseArgValue(args[i]);
    }
  }

  // Add named params (override positional)
  if (namedParams) {
    for (const [key, value] of Object.entries(namedParams)) {
      params[key] = parseArgValue(value);
    }
  }

  return params;
}

/**
 * Parse argument value - try to convert to appropriate type
 */
function parseArgValue(value: string): unknown {
  // Try number
  const num = Number(value);
  if (!isNaN(num) && value.trim() !== '') {
    return num;
  }

  // Try boolean
  if (value.toLowerCase() === 'true') return true;
  if (value.toLowerCase() === 'false') return false;

  // Try JSON
  if ((value.startsWith('{') && value.endsWith('}')) ||
      (value.startsWith('[') && value.endsWith(']'))) {
    try {
      return JSON.parse(value);
    } catch {
      // Not valid JSON, return as string
    }
  }

  return value;
}

// =============================================================================
// Tool Registration
// =============================================================================

/**
 * Register tools from MCP servers and AgentVM
 */
async function registerTools(
  sdk: { registerTool: (name: string, tool: ToolDefinition) => void },
  _config: Config,
  verbose?: boolean
): Promise<void> {
  // Register AgentVM tools (built-in)
  const agentVmTools = getAgentVMTools();
  for (const tool of agentVmTools) {
    sdk.registerTool(tool.name, createToolDefinition(tool));
    if (verbose) {
      console.log(`[tool] Registered: ${tool.name} (agentvm)`);
    }
  }

  // TODO: In future, register MCP server tools here
  // This would involve:
  // 1. Starting MCP servers from config
  // 2. Querying their tool lists
  // 3. Creating proxy tool definitions
}

/**
 * Create a ToolDefinition from our Tool interface
 */
function createToolDefinition(tool: Tool): ToolDefinition {
  const defaultSchema: JsonSchema = { 
    type: 'object', 
    properties: {}, 
    required: [] 
  };
  return {
    description: tool.description,
    parameters: (tool.schema as JsonSchema) || defaultSchema,
    execute: async (args: Record<string, unknown>) => {
      // Handle built-in tools
      switch (tool.name) {
        case 'web_search':
          return await webSearch({
            query: String(args.query || args.arg1 || ''),
            count: typeof args.count === 'number' ? args.count : 10,
            freshness: typeof args.freshness === 'string' ? args.freshness : undefined,
          });
        case 'news_search':
          return await newsSearch({
            query: String(args.query || args.arg1 || ''),
            count: typeof args.count === 'number' ? args.count : 10,
            freshness: typeof args.freshness === 'string' ? args.freshness : undefined,
          });
        case 'execute_code':
        case 'vm_exec':
          // These are handled by external AgentVM service
          throw new Error(`Tool ${tool.name} requires AgentVM service connection (not yet implemented)`);
        default:
          throw new Error(`Tool ${tool.name} has no implementation`);
      }
    }
  };
}

/**
 * Verify that all required tools are available
 */
async function verifyToolsAvailable(
  config: Config,
  toolNames: string[]
): Promise<{ available: boolean; missing: string[] }> {
  const agentVmToolNames = getAgentVMTools().map(t => t.name);
  const missing: string[] = [];

  for (const name of toolNames) {
    if (!agentVmToolNames.includes(name)) {
      // Tool not in AgentVM, would need MCP server
      // For now, assume MCP tools are available if configured
      // TODO: Actually check MCP servers
      missing.push(name);
    }
  }

  // If there are MCP servers configured, assume missing tools might be there
  // In a full implementation, we'd actually query the servers
  if (config.mcp?.servers && Object.keys(config.mcp.servers).length > 0) {
    // Assume MCP servers might have the missing tools
    return { available: true, missing: [] };
  }

  return {
    available: missing.length === 0,
    missing
  };
}

// =============================================================================
// Formatting
// =============================================================================

/**
 * Format dry run output
 */
function formatDryRun(
  dmlPath: string,
  meta: MetaFile | null,
  params: Record<string, unknown>,
  model: string,
  provider: Provider,
  workspacePath: string
): string {
  const lines: string[] = [
    '═══════════════════════════════════════════════════════════════',
    '  DRY RUN - Would execute the following:',
    '═══════════════════════════════════════════════════════════════',
    '',
    `  DML File:    ${dmlPath}`,
    `  Model:       ${provider}/${model}`,
    `  Workspace:   ${workspacePath}`,
    ''
  ];

  if (meta) {
    lines.push(`  Description: ${meta.description}`);
    lines.push(`  Compiled:    ${meta.compiledAt}`);
    
    if (meta.parameters.length > 0) {
      lines.push('');
      lines.push('  Parameters:');
      for (const param of meta.parameters) {
        const value = params[param.name];
        const valueStr = value !== undefined ? ` = ${JSON.stringify(value)}` : ' (not provided)';
        lines.push(`    • ${param.name}${valueStr}`);
      }
    }

    if (meta.tools.length > 0) {
      lines.push('');
      lines.push('  Required Tools:');
      for (const tool of meta.tools) {
        lines.push(`    • ${tool}`);
      }
    }
  }

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════════════');

  return lines.join('\n');
}
