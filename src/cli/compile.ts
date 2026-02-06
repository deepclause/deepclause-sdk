/**
 * DeepClause CLI - Compilation Module
 * 
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as crypto from 'crypto';
import { streamText, generateText } from 'ai';
import { openai, createOpenAI } from '@ai-sdk/openai';
import { anthropic } from '@ai-sdk/anthropic';
import { google } from '@ai-sdk/google';
import { loadConfig, type Config, type Provider } from './config.js';
import { getAgentVMTools, type Tool } from './tools.js';
import { buildCompilationPrompt, buildUserMessage } from './prompt.js';
import { loadProlog, type SWIPLModule } from '../prolog/loader.js';

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

interface ValidationResult {
  valid: boolean;
  errors: string[];
  warnings?: string[];
}

interface CompilationAttempt {
  dml: string;
  validation: ValidationResult;
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
// Prolog Validation
// =============================================================================

let prologInstance: SWIPLModule | null = null;

/**
 * Get or initialize Prolog instance for validation
 */
async function getProlog(): Promise<SWIPLModule> {
  if (!prologInstance) {
    prologInstance = await loadProlog();
  }
  return prologInstance;
}

/**
 * Validate DML code using the actual Prolog parser
 */
async function validateWithProlog(dml: string): Promise<ValidationResult> {
  const errors: string[] = [];
  const warnings: string[] = [];

  try {
    const swipl = await getProlog();
    
    // Write code to temp file
    const tempPath = `/tmp/validate_${Date.now()}.pl`;
    swipl.FS.writeFile(tempPath, dml);

    // Try to parse the file using read_term which will catch syntax errors
    try {
      swipl.prolog.query(
        `catch(
          (
            open('${tempPath}', read, Stream, []),
            call_cleanup(
              read_all_terms(Stream),
              close(Stream)
            )
          ),
          Error,
          (
            Error = error(syntax_error(Message), _) -> 
              true 
            ; 
              (term_to_atom(Error, Message), true)
          )
        ), 
        read_all_terms(S) :- 
          repeat,
          read_term(S, T, [syntax_errors(error)]),
          (T == end_of_file -> ! ; fail)`
      );
      
      // Simple approach: try to consult the file
      try {
        swipl.prolog.call(`consult('${tempPath}')`);
      } catch (consultError) {
        const errMsg = consultError instanceof Error ? consultError.message : String(consultError);
        if (errMsg.includes('syntax') || errMsg.includes('error')) {
          // Extract meaningful error message
          const match = errMsg.match(/(?:syntax_error|error)\s*\(\s*['"]?([^'")\]]+)/i);
          if (match) {
            errors.push(`Syntax error: ${match[1]}`);
          } else {
            errors.push(`Parse error: ${errMsg.substring(0, 200)}`);
          }
        }
      }
    } catch (e) {
      const errMsg = e instanceof Error ? e.message : String(e);
      if (errMsg.includes('syntax') || errMsg.includes('error')) {
        errors.push(errMsg.substring(0, 200));
      }
    }

    // Check for required elements
    if (!dml.includes('agent_main')) {
      errors.push('Missing agent_main predicate - every DML program must define agent_main');
    }

    // Check for balanced delimiters
    const delimiterCheck = checkBalancedDelimiters(dml);
    errors.push(...delimiterCheck.errors);

    // Check for common mistakes
    const lintWarnings = lintDML(dml);
    warnings.push(...lintWarnings);

    // Cleanup temp file
    try {
      swipl.FS.unlink(tempPath);
    } catch {
      // Ignore cleanup errors
    }

  } catch (error) {
    // If Prolog itself fails, fall back to basic validation
    const basicResult = validateDMLSyntaxBasic(dml);
    return basicResult;
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings: warnings.length > 0 ? warnings : undefined
  };
}

/**
 * Check balanced delimiters
 */
function checkBalancedDelimiters(dml: string): { errors: string[] } {
  const errors: string[] = [];
  
  // Remove strings and comments for delimiter checking
  const stripped = dml
    .replace(/"(?:[^"\\]|\\.)*"/g, '""')  // Remove string contents
    .replace(/%.*$/gm, '')                 // Remove line comments
    .replace(/\/\*[\s\S]*?\*\//g, '');     // Remove block comments

  let parenCount = 0;
  let bracketCount = 0;
  let braceCount = 0;

  for (const char of stripped) {
    if (char === '(') parenCount++;
    if (char === ')') parenCount--;
    if (char === '[') bracketCount++;
    if (char === ']') bracketCount--;
    if (char === '{') braceCount++;
    if (char === '}') braceCount--;

    if (parenCount < 0) {
      errors.push('Unbalanced parentheses: extra closing )');
      break;
    }
    if (bracketCount < 0) {
      errors.push('Unbalanced brackets: extra closing ]');
      break;
    }
    if (braceCount < 0) {
      errors.push('Unbalanced braces: extra closing }');
      break;
    }
  }

  if (parenCount > 0) errors.push(`Unbalanced parentheses: ${parenCount} unclosed (`);
  if (bracketCount > 0) errors.push(`Unbalanced brackets: ${bracketCount} unclosed [`);
  if (braceCount > 0) errors.push(`Unbalanced braces: ${braceCount} unclosed {`);

  return { errors };
}

/**
 * Lint DML for common issues
 */
function lintDML(dml: string): string[] {
  const warnings: string[] = [];

  // Check for format() passed directly to answer/output
  if (/answer\s*\(\s*format\s*\(/.test(dml)) {
    warnings.push('Avoid passing format() directly to answer/1 - use format/2 with a stream or format_to_atom/3');
  }

  // Check for mixing {Var} and format/3
  const hasInterpolation = /\{[A-Z][a-zA-Z0-9_]*\}/.test(dml);
  const hasFormat = /format\s*\(/.test(dml);
  if (hasInterpolation && hasFormat) {
    warnings.push('Consider using either {Variable} interpolation OR format/3, not both');
  }

  return warnings;
}

/**
 * Basic syntax validation (fallback when Prolog fails)
 */
function validateDMLSyntaxBasic(dml: string): ValidationResult {
  const errors: string[] = [];

  if (!dml.includes('agent_main')) {
    errors.push('Missing agent_main predicate');
  }

  const delimiterCheck = checkBalancedDelimiters(dml);
  errors.push(...delimiterCheck.errors);

  // Check for unclosed strings
  const stringPattern = /"(?:[^"\\]|\\.)*"/g;
  const cleanedDml = dml.replace(stringPattern, '""');
  if ((cleanedDml.match(/"/g) || []).length % 2 !== 0) {
    errors.push('Unclosed string literal');
  }

  return {
    valid: errors.length === 0,
    errors: [...new Set(errors)]
  };
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

  // Build the compilation prompt
  const systemPrompt = buildCompilationPrompt(tools);
  const userMessage = buildUserMessage(markdown);

  // Status indicator
  const status = new StatusIndicator(verbose || shouldStream);
  
  // Agentic compilation loop
  const attempts: CompilationAttempt[] = [];
  let lastDml = '';
  let lastValidation: ValidationResult = { valid: false, errors: [] };

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    status.start(`Compiling (attempt ${attempt}/${maxAttempts})...`);

    try {
      // Generate DML (with or without streaming)
      let dml: string;
      
      if (shouldStream && attempt === 1) {
        // Only stream on first attempt for visibility
        dml = await generateDMLWithStreaming(
          systemPrompt,
          userMessage,
          attempts,
          model,
          provider,
          options.temperature,
          (chunk) => {
            if (verbose) {
              process.stdout.write(chunk);
            }
          }
        );
        if (verbose) {
          console.log(); // Newline after streaming
        }
      } else {
        dml = await generateDML(
          systemPrompt,
          userMessage,
          attempts,
          model,
          provider,
          options.temperature
        );
      }

      lastDml = dml;
      status.update(`Validating DML...`);

      // Validate with Prolog
      const validation = await validateWithProlog(dml);
      lastValidation = validation;
      
      attempts.push({ dml, validation });

      if (validation.valid) {
        status.stop();
        
        // Generate explanation
        const explanation = await generateExplanation(dml, model, provider);
        
        // If validate-only, return without saving
        if (options.validateOnly) {
          console.log('\n✅ Compilation successful!\n');
          console.log(explanation);
          
          return {
            output: dmlPath,
            tools: extractToolDependencies(dml),
            skipped: false,
            valid: true,
            dml,
            explanation,
            attempts: attempt
          };
        }

        // Save the DML
        await fs.mkdir(absoluteOutputDir, { recursive: true });

        // Load existing meta for history
        const existingMeta = await loadExistingMeta(metaPath);
        const history = existingMeta?.history || [];
        const newVersion = history.length + 1;

        // Create new meta file
        const extractedTools = extractToolDependencies(dml);
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
          attempts: attempt
        };
      } else {
        status.update(`Attempt ${attempt} failed: ${validation.errors[0] || 'validation error'}`);
        
        if (verbose) {
          console.log(`\n⚠️  Attempt ${attempt} validation errors:`);
          for (const error of validation.errors) {
            console.log(`   - ${error}`);
          }
        }
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      status.update(`Attempt ${attempt} error: ${message}`);
      
      attempts.push({
        dml: lastDml,
        validation: { valid: false, errors: [message] }
      });

      if (verbose) {
        console.log(`\n⚠️  Attempt ${attempt} error: ${message}`);
      }
    }
  }

  status.stop();

  // All attempts failed
  console.log('\n❌ Compilation failed after ' + maxAttempts + ' attempts.\n');
  
  console.log('Validation errors:');
  for (const error of lastValidation.errors) {
    console.log(`  - ${error}`);
  }
  
  if (verbose && lastDml) {
    console.log('\nLast generated code:\n');
    console.log('```prolog');
    console.log(lastDml);
    console.log('```');
  }

  throw new Error(`Compilation failed after ${maxAttempts} attempts: ${lastValidation.errors.join(', ')}`);
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

// =============================================================================
// LLM Integration
// =============================================================================

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

  // Build the compilation prompt
  const systemPrompt = buildCompilationPrompt(tools);
  const userMessage = buildUserMessage(prompt);

  const maxAttempts = options.maxAttempts ?? 3;
  const attempts: CompilationAttempt[] = [];
  let lastDml = "";
  let lastValidation: ValidationResult = { valid: false, errors: [] };

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      const dml = await generateDML(
        systemPrompt,
        userMessage,
        attempts,
        model,
        provider,
        options.temperature
      );

      lastDml = dml;
      const validation = await validateWithProlog(dml);
      lastValidation = validation;
      attempts.push({ dml, validation });

      if (validation.valid) {
        return {
          dml,
          tools: extractToolDependencies(dml)
        };
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      attempts.push({
        dml: lastDml,
        validation: { valid: false, errors: [message] }
      });
    }
  }

  throw new Error(`Failed to generate valid DML after ${maxAttempts} attempts: ${lastValidation.errors.join(", ")}`);
}

/**
 * Generate DML using LLM with streaming
 */
export async function generateDMLWithStreaming(
  systemPrompt: string,
  userMessage: string,
  previousAttempts: CompilationAttempt[],
  model: string,
  provider: Provider,
  temperature?: number,
  onChunk?: (chunk: string) => void
): Promise<string> {
  const llm = getLanguageModel(provider, model);
  
  // Build messages including previous attempts for self-correction
  const messages = buildMessages(systemPrompt, userMessage, previousAttempts);

  const result = await streamText({
    model: llm,
    messages,
    temperature: temperature ?? 0.3,
    maxOutputTokens: 8192
  });

  let dml = '';
  for await (const chunk of result.textStream) {
    dml += chunk;
    if (onChunk) {
      onChunk(chunk);
    }
  }

  return cleanDMLResponse(dml);
}

/**
 * Generate DML using LLM (non-streaming)
 */
async function generateDML(
  systemPrompt: string,
  userMessage: string,
  previousAttempts: CompilationAttempt[],
  model: string,
  provider: Provider,
  temperature?: number
): Promise<string> {
  const llm = getLanguageModel(provider, model);
  
  // Build messages including previous attempts for self-correction
  const messages = buildMessages(systemPrompt, userMessage, previousAttempts);

  const result = await generateText({
    model: llm,
    messages,
    temperature: temperature ?? 0.3,
    maxOutputTokens: 8192
  });

  return cleanDMLResponse(result.text);
}

/**
 * Build messages for the compilation, including error feedback from previous attempts
 */
function buildMessages(
  systemPrompt: string,
  userMessage: string,
  previousAttempts: CompilationAttempt[]
): Array<{ role: 'system' | 'user' | 'assistant'; content: string }> {
  const messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }> = [
    { role: 'system', content: systemPrompt },
    { role: 'user', content: userMessage }
  ];

  // Add previous attempts and their errors for self-correction
  for (const attempt of previousAttempts) {
    messages.push({
      role: 'assistant',
      content: attempt.dml
    });

    const errorFeedback = [
      'The code above has the following validation errors:',
      ...attempt.validation.errors.map(e => `- ${e}`),
      '',
      'Please fix these errors and generate corrected DML code.'
    ].join('\n');

    messages.push({
      role: 'user',
      content: errorFeedback
    });
  }

  return messages;
}

/**
 * Generate a brief explanation of the compiled DML
 */
async function generateExplanation(
  dml: string,
  model: string,
  provider: Provider
): Promise<string> {
  const llm = getLanguageModel(provider, model);

  try {
    const result = await generateText({
      model: llm,
      system: 'You are a technical writer. Explain code briefly and clearly.',
      prompt: `Briefly explain what this DML program does (2-3 sentences max):

\`\`\`prolog
${dml}
\`\`\`

Focus on: what it does, what tools it uses, and how it handles input. Be concise.`,
      temperature: 0.3,
      maxOutputTokens: 256
    });

    return result.text.trim();
  } catch {
    // If explanation generation fails, return a generic message
    return 'DML program compiled successfully.';
  }
}

/**
 * Clean up LLM response - remove markdown code fences
 */
function cleanDMLResponse(text: string): string {
  let dml = text.trim();
  
  // Remove markdown code fences
  if (dml.startsWith('```prolog') || dml.startsWith('```')) {
    dml = dml.replace(/^```(?:prolog)?\n?/, '').replace(/\n?```$/, '');
  }

  return dml.trim();
}

/**
 * Get the appropriate language model instance
 */
function getLanguageModel(provider: Provider, model: string) {
  switch (provider) {
    case 'openai':
      return openai(model);
    case 'anthropic':
      return anthropic(model);
    case 'google':
      return google(model);
    case 'openrouter': {
      const openrouter = createOpenAI({
        baseURL: 'https://openrouter.ai/api/v1',
        apiKey: process.env.OPENROUTER_API_KEY
      });
      return openrouter(model);
    }
    default:
      throw new Error(`Unsupported provider: ${provider}`);
  }
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
// DML Analysis
// =============================================================================

/**
 * Extract tool dependencies from DML code
 */
export function extractToolDependencies(dml: string): string[] {
  const execPattern = /exec\s*\(\s*([a-z_][a-z0-9_]*)\s*\(/gi;
  const tools = new Set<string>();
  
  let match;
  while ((match = execPattern.exec(dml)) !== null) {
    tools.add(match[1]);
  }
  
  return Array.from(tools).sort();
}

/**
 * Extract parameters from agent_main signature
 */
export function extractParameters(dml: string): Array<{ name: string; position: number; description?: string; required?: boolean }> {
  const mainPattern = /agent_main\s*\(([^)]*)\)\s*:-/;
  const match = mainPattern.exec(dml);
  
  if (!match || !match[1].trim()) {
    return [];
  }
  
  const args = match[1]
    .split(',')
    .map(arg => arg.trim())
    .filter(arg => arg.length > 0);
  
  return args
    .map((arg) => ({
      name: arg.replace(/([A-Z])/g, (_m, c, i) => (i > 0 ? '_' : '') + c.toLowerCase()).replace(/^_/, ''),
      position: 0,
      required: true
    }))
    .sort((a, b) => a.name.localeCompare(b.name))
    .map((param, index) => ({ ...param, position: index }));
}

/**
 * Extract description from markdown
 */
export function extractDescription(markdown: string): string {
  const lines = markdown.split('\n');
  
  for (const line of lines) {
    if (line.startsWith('# ')) {
      return line.substring(2).trim();
    }
  }
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed && !trimmed.startsWith('#')) {
      return trimmed.substring(0, 100);
    }
  }
  
  return 'No description';
}

// =============================================================================
// Legacy Validation (for backwards compatibility)
// =============================================================================

/**
 * Basic DML syntax validation (exported for backwards compatibility)
 */
export function validateDMLSyntax(dml: string): { valid: boolean; errors: string[] } {
  const result = validateDMLSyntaxBasic(dml);
  return { valid: result.valid, errors: result.errors };
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
