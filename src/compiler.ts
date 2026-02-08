/**
 * DeepClause SDK - Compilation Module
 * 
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */

import { generateText } from 'ai';
import { openai, createOpenAI } from '@ai-sdk/openai';
import { anthropic } from '@ai-sdk/anthropic';
import { google } from '@ai-sdk/google';
import { buildCompilationPrompt, buildUserMessage } from './compiler_prompt.js';
import { loadProlog, type SWIPLModule } from './prolog/loader.js';
import { CompileOptions, CompileResult } from './types.js';

// =============================================================================
// Internal Types
// =============================================================================

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
export async function validateWithProlog(dml: string): Promise<ValidationResult> {
  const errors: string[] = [];
  const warnings: string[] = [];

  try {
    const swipl = await getProlog();
    
    // Write code to temp file
    const tempPath = `/tmp/validate_${Date.now()}_${Math.random().toString(36).substring(7)}.pl`;
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
    return validateDMLSyntaxBasic(dml);
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
// Main Compilation Function
// =============================================================================

/**
 * Compile a natural language prompt or markdown directly to DML
 */
export async function compileToDML(
  source: string,
  options: CompileOptions
): Promise<CompileResult> {
  const model = options.model || 'gpt-4o';
  const provider = options.provider || 'openai';
  const maxAttempts = options.maxAttempts ?? 3;
  const temperature = options.temperature ?? 0.3;
  const tools = options.tools || [];

  // Build the compilation prompt
  const systemPrompt = buildCompilationPrompt(tools);
  const userMessage = buildUserMessage(source);

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
        temperature
      );

      lastDml = dml;
      const validation = await validateWithProlog(dml);
      lastValidation = validation;
      attempts.push({ dml, validation });

      if (validation.valid) {
        // Generate explanation
        const explanation = await generateExplanation(dml, model, provider);

        return {
          dml,
          tools: extractToolDependencies(dml),
          explanation,
          attempts: attempt,
          valid: true
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

  return {
    dml: lastDml,
    tools: extractToolDependencies(lastDml),
    valid: false,
    errors: lastValidation.errors,
    attempts: maxAttempts
  };
}

// =============================================================================
// LLM Integration
// =============================================================================

/**
 * Generate DML using LLM (non-streaming)
 */
async function generateDML(
  systemPrompt: string,
  userMessage: string,
  previousAttempts: CompilationAttempt[],
  model: string,
  provider: string,
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
  provider: string
): Promise<string> {
  const llm = getLanguageModel(provider, model);

  try {
    const result = await generateText({
      model: llm,
      system: 'You are a technical writer. Explain code briefly and clearly.',
      prompt: `Briefly explain what this DML program does (2-3 sentences max):\n\n\`\`\`prolog\n${dml}\n\`\`\`\n\nFocus on: what it does, what tools it uses, and how it handles input. Be concise.`,
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
function getLanguageModel(provider: string, model: string) {
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
