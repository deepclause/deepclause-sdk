/**
 * DeepClause CLI - Compilation Module
 * 
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as crypto from 'crypto';
import { loadConfig, type Config, type Provider } from './config.js';
import { getAgentVMTools, type Tool } from './tools.js';
import { 
  compileToDML, 
  extractParameters, 
  extractDescription 
} from '../compiler.js';

// =============================================================================
// Types
// =============================================================================

export interface CompileOptions {
  force?: boolean;
  validateOnly?: boolean;
  model?: string;
  provider?: Provider;
  temperature?: number;
  maxAttempts?: number;
  verbose?: boolean;
  stream?: boolean;
}

export interface CompileResult {
  output: string;
  tools: string[];
  skipped: boolean;
  valid: boolean;
  dml?: string;
  meta?: MetaFile;
  explanation?: string;
  attempts?: number;
}

export interface CompileAllResult {
  compiled: number;
  skipped: number;
  failed: number;
  errors: Array<{ file: string; error: string }>;
}

export interface MetaFile {
  version: string;
  source: string;
  sourceHash: string;
  compiledAt: string;
  model: string;
  provider: string;
  description: string;
  parameters: Array<{
    name: string;
    description?: string;
    required?: boolean;
    default?: string;
    position: number;
  }>;
  tools: string[];
  history: Array<{
    version: number;
    timestamp: string;
    sourceHash: string;
    model: string;
    provider: string;
  }>;
}

// =============================================================================
// Status Indicator
// =============================================================================

const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

class StatusIndicator {
  private frame = 0;
  private interval: ReturnType<typeof setInterval> | null = null;
  private message = '';
  private enabled: boolean;

  constructor(enabled = true) {
    this.enabled = enabled && process.stdout.isTTY === true;
  }

  start(message: string): void {
    if (!this.enabled) {
      console.log(`  ${message}`);
      return;
    }
    this.message = message;
    this.frame = 0;
    this.render();
    this.interval = setInterval(() => {
      this.frame = (this.frame + 1) % SPINNER_FRAMES.length;
      this.render();
    }, 80);
  }

  update(message: string): void {
    this.message = message;
    if (!this.enabled) {
      console.log(`  ${message}`);
    } else {
      this.render();
    }
  }

  private render(): void {
    process.stdout.write(`\r${SPINNER_FRAMES[this.frame]} ${this.message}`.padEnd(80));
  }

  stop(finalMessage?: string): void {
    if (this.interval) {
      clearInterval(this.interval);
      this.interval = null;
    }
    if (this.enabled) {
      process.stdout.write('\r' + ' '.repeat(80) + '\r');
    }
    if (finalMessage) {
      console.log(finalMessage);
    }
  }
}

// =============================================================================
// Main Compilation Functions
// =============================================================================

/**
 * Compile a Markdown task description to DML using an agentic loop
 */
export async function compile(
  sourcePath: string,
  outputDir: string,
  options: CompileOptions = {}
): Promise<CompileResult> {
  const maxAttempts = options.maxAttempts ?? 3;
  const verbose = options.verbose ?? false;
  const shouldStream = options.stream ?? true;
  
  // Resolve paths
  const absoluteSource = path.resolve(sourcePath);
  const absoluteOutputDir = path.resolve(outputDir);
  
  // Read source file
  let markdown: string;
  try {
    markdown = await fs.readFile(absoluteSource, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to read source file: ${absoluteSource}`);
  }

  // Calculate source hash
  const sourceHash = computeHash(markdown);

  // Determine output file names
  const baseName = path.basename(absoluteSource, path.extname(absoluteSource));
  const dmlPath = path.join(absoluteOutputDir, `${baseName}.dml`);
  const metaPath = path.join(absoluteOutputDir, `${baseName}.meta.json`);

  // Check for existing compilation (caching)
  if (!options.force && !options.validateOnly) {
    const existing = await loadExistingMeta(metaPath);
    if (existing && existing.sourceHash === sourceHash) {
      return {
        output: dmlPath,
        tools: existing.tools,
        skipped: true,
        valid: true,
        meta: existing
      };
    }
  }

  // Load config for model/provider (try cwd first, then source dir, then use defaults)
  let config: Config;
  try {
    config = await loadConfig(process.cwd());
  } catch {
    try {
      const sourceDir = path.dirname(absoluteSource).split('/.deepclause')[0];
      config = await loadConfig(sourceDir);
    } catch {
      // Use sensible defaults if no config found
      config = {
        model: 'gpt-4o',
        provider: 'openai' as Provider,
        providers: {},
        mcp: { servers: {} },
        agentvm: { network: true },
        dmlBase: '.deepclause/tools',
        workspace: './'
      };
    }
  }
  const model = options.model || config.model;
  const provider = options.provider || config.provider;

  // Get available tools for the prompt
  const tools = await getAvailableTools(config);

  // Status indicator
  const status = new StatusIndicator(verbose || shouldStream);
  status.start(`Compiling...`);

  try {
    // Call the shared compiler
    const result = await compileToDML(markdown, {
      model,
      provider: provider as any,
      temperature: options.temperature,
      maxAttempts,
      verbose,
      tools: tools as any // Map CLI tools to compiler tools
    });

    if (result.valid && result.dml) {
      status.stop();
      const dml = result.dml;
      const explanation = result.explanation || 'DML program compiled successfully.';
      
      // If validate-only, return without saving
      if (options.validateOnly) {
        console.log('\n✅ Compilation successful!\n');
        console.log(explanation);
        
        return {
          output: dmlPath,
          tools: result.tools,
          skipped: false,
          valid: true,
          dml,
          explanation,
          attempts: result.attempts
        };
      }

      // Save the DML
      await fs.mkdir(absoluteOutputDir, { recursive: true });

      // Load existing meta for history
      const existingMeta = await loadExistingMeta(metaPath);
      const history = existingMeta?.history || [];
      const newVersion = history.length + 1;

      // Create new meta file
      const extractedTools = result.tools;
      const extractedParams = extractParameters(dml);
      const description = extractDescription(markdown);

      const meta: MetaFile = {
        version: '1.0.0',
        source: path.relative(absoluteOutputDir, absoluteSource),
        sourceHash,
        compiledAt: new Date().toISOString(),
        model,
        provider,
        description,
        parameters: extractedParams,
        tools: extractedTools,
        history: [
          ...history,
          {
            version: newVersion,
            timestamp: new Date().toISOString(),
            sourceHash,
            model,
            provider
          }
        ]
      };

      // Write files
      await fs.writeFile(dmlPath, dml);
      await fs.writeFile(metaPath, JSON.stringify(meta, null, 2) + '\n');

      // Print success
      console.log('\n✅ Compilation successful!\n');
      console.log(explanation);

      return {
        output: dmlPath,
        tools: extractedTools,
        skipped: false,
        valid: true,
        dml,
        meta,
        explanation,
        attempts: result.attempts
      };
    } else {
      status.stop();
      console.log('\n❌ Compilation failed.\n');
      console.log('Validation errors:');
      for (const error of result.errors || []) {
        console.log(`  - ${error}`);
      }
      throw new Error(`Compilation failed: ${(result.errors || []).join(', ')}`);
    }
  } catch (error) {
    status.stop();
    throw error;
  }
}

/**
 * Compile all Markdown files in a directory
 */
export async function compileAll(
  sourceDir: string,
  outputDir: string,
  options: CompileOptions = {}
): Promise<CompileAllResult> {
  const absoluteSourceDir = path.resolve(sourceDir);
  const absoluteOutputDir = path.resolve(outputDir);

  // Find all markdown files
  let files: string[];
  try {
    const entries = await fs.readdir(absoluteSourceDir);
    files = entries.filter(f => f.endsWith('.md'));
  } catch (error) {
    throw new Error(`Failed to read source directory: ${absoluteSourceDir}`);
  }

  const result: CompileAllResult = {
    compiled: 0,
    skipped: 0,
    failed: 0,
    errors: []
  };

  // Compile each file
  for (const file of files) {
    const sourcePath = path.join(absoluteSourceDir, file);
    
    try {
      const compileResult = await compile(sourcePath, absoluteOutputDir, {
        ...options,
        stream: false,  // Don't stream when batch compiling
        verbose: false
      });
      
      if (compileResult.skipped) {
        result.skipped++;
      } else {
        result.compiled++;
      }
    } catch (error) {
      result.failed++;
      result.errors.push({
        file,
        error: (error as Error).message
      });
    }
  }

  return result;
}

/**
 * Compile a natural language prompt directly to DML without saving to disk
 */
export async function compilePrompt(
  prompt: string,
  options: CompileOptions = {}
): Promise<{ dml: string; tools: string[] }> {
  const config = await loadConfig(process.cwd());
  const model = options.model || config.model;
  const provider = options.provider || config.provider;

  // Get available tools for the prompt
  const tools = await getAvailableTools(config);

  const result = await compileToDML(prompt, {
    model,
    provider: provider as any,
    temperature: options.temperature,
    maxAttempts: options.maxAttempts,
    tools: tools as any
  });

  if (result.valid && result.dml) {
    return {
      dml: result.dml,
      tools: result.tools
    };
  }

  throw new Error(`Failed to generate valid DML: ${(result.errors || []).join(", ")}`);
}

// =============================================================================
// Tool Resolution
// =============================================================================

/**
 * Get all available tools for compilation prompt
 */
async function getAvailableTools(_config: Config): Promise<Tool[]> {
  const tools: Tool[] = [];
  tools.push(...getAgentVMTools());
  return tools;
}

// =============================================================================
// Helpers
// =============================================================================

/**
 * Compute SHA-256 hash of content
 */
function computeHash(content: string): string {
  return 'sha256:' + crypto.createHash('sha256').update(content).digest('hex').substring(0, 16);
}

/**
 * Load existing meta file if it exists
 */
async function loadExistingMeta(metaPath: string): Promise<MetaFile | null> {
  try {
    const content = await fs.readFile(metaPath, 'utf-8');
    return JSON.parse(content) as MetaFile;
  } catch {
    return null;
  }
}
