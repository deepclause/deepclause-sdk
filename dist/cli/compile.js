/**
 * DeepClause CLI - Compilation Module
 *
 * Compiles Markdown task descriptions to DML programs using LLM.
 */
import * as fs from 'fs/promises';
import * as path from 'path';
import * as crypto from 'crypto';
import { generateText } from 'ai';
import { openai, createOpenAI } from '@ai-sdk/openai';
import { anthropic } from '@ai-sdk/anthropic';
import { google } from '@ai-sdk/google';
import { loadConfig } from './config.js';
import { getAgentVMTools } from './tools.js';
import { buildCompilationPrompt, buildUserMessage } from './prompt.js';
// =============================================================================
// Main Compilation Functions
// =============================================================================
/**
 * Compile a Markdown task description to DML
 */
export async function compile(sourcePath, outputDir, options = {}) {
    // Resolve paths
    const absoluteSource = path.resolve(sourcePath);
    const absoluteOutputDir = path.resolve(outputDir);
    // Read source file
    let markdown;
    try {
        markdown = await fs.readFile(absoluteSource, 'utf-8');
    }
    catch (error) {
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
    // Load config for model/provider
    const config = await loadConfig(path.dirname(absoluteSource).split('/.deepclause')[0] || process.cwd());
    const model = options.model || config.model;
    const provider = options.provider || config.provider;
    // Get available tools for the prompt
    const tools = await getAvailableTools(config);
    // Build the compilation prompt
    const systemPrompt = buildCompilationPrompt(tools);
    const userMessage = buildUserMessage(markdown);
    // Call LLM to generate DML
    const dml = await generateDML(systemPrompt, userMessage, model, provider);
    // Extract metadata from generated DML
    const extractedTools = extractToolDependencies(dml);
    const extractedParams = extractParameters(dml);
    const description = extractDescription(markdown);
    // Validate DML syntax (basic validation)
    const validationResult = validateDMLSyntax(dml);
    if (!validationResult.valid) {
        throw new Error(`Generated DML has syntax errors: ${validationResult.errors.join(', ')}`);
    }
    // If validate-only, return without saving
    if (options.validateOnly) {
        return {
            output: dmlPath,
            tools: extractedTools,
            skipped: false,
            valid: true,
            dml
        };
    }
    // Ensure output directory exists
    await fs.mkdir(absoluteOutputDir, { recursive: true });
    // Load existing meta for history
    const existingMeta = await loadExistingMeta(metaPath);
    const history = existingMeta?.history || [];
    const newVersion = history.length + 1;
    // Create new meta file
    const meta = {
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
    return {
        output: dmlPath,
        tools: extractedTools,
        skipped: false,
        valid: true,
        dml,
        meta
    };
}
/**
 * Compile all Markdown files in a directory
 */
export async function compileAll(sourceDir, outputDir, options = {}) {
    const absoluteSourceDir = path.resolve(sourceDir);
    const absoluteOutputDir = path.resolve(outputDir);
    // Find all markdown files
    let files;
    try {
        const entries = await fs.readdir(absoluteSourceDir);
        files = entries.filter(f => f.endsWith('.md'));
    }
    catch (error) {
        throw new Error(`Failed to read source directory: ${absoluteSourceDir}`);
    }
    const result = {
        compiled: 0,
        skipped: 0,
        failed: 0,
        errors: []
    };
    // Compile each file
    for (const file of files) {
        const sourcePath = path.join(absoluteSourceDir, file);
        try {
            const compileResult = await compile(sourcePath, absoluteOutputDir, options);
            if (compileResult.skipped) {
                result.skipped++;
            }
            else {
                result.compiled++;
            }
        }
        catch (error) {
            result.failed++;
            result.errors.push({
                file,
                error: error.message
            });
        }
    }
    return result;
}
// =============================================================================
// LLM Integration
// =============================================================================
/**
 * Generate DML using LLM
 */
async function generateDML(systemPrompt, userMessage, model, provider) {
    const llm = getLanguageModel(provider, model);
    const result = await generateText({
        model: llm,
        system: systemPrompt,
        prompt: userMessage,
        temperature: 0.3, // Lower temperature for more consistent code generation
        maxTokens: 8192
    });
    // Clean up the response - remove any markdown code fences if present
    let dml = result.text.trim();
    // Remove markdown code fences
    if (dml.startsWith('```prolog') || dml.startsWith('```')) {
        dml = dml.replace(/^```(?:prolog)?\n?/, '').replace(/\n?```$/, '');
    }
    return dml.trim();
}
/**
 * Get the appropriate language model instance
 */
function getLanguageModel(provider, model) {
    switch (provider) {
        case 'openai':
            return openai(model);
        case 'anthropic':
            return anthropic(model);
        case 'google':
            return google(model);
        case 'openrouter': {
            // OpenRouter uses OpenAI-compatible API
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
async function getAvailableTools(_config) {
    const tools = [];
    // Always include AgentVM tools
    tools.push(...getAgentVMTools());
    // TODO: In Phase 3, add MCP server tool discovery
    // For now, we just use AgentVM tools
    return tools;
}
// =============================================================================
// DML Analysis
// =============================================================================
/**
 * Extract tool dependencies from DML code
 * Only extracts external tools called via exec/2
 */
export function extractToolDependencies(dml) {
    // Match exec(tool_name(...), ...) patterns
    const execPattern = /exec\s*\(\s*([a-z_][a-z0-9_]*)\s*\(/gi;
    const tools = new Set();
    let match;
    while ((match = execPattern.exec(dml)) !== null) {
        tools.add(match[1]);
    }
    return Array.from(tools).sort();
}
/**
 * Extract parameters from agent_main signature
 */
export function extractParameters(dml) {
    // Match agent_main(Arg1, Arg2, ...) :-
    const mainPattern = /agent_main\s*\(([^)]*)\)\s*:-/;
    const match = mainPattern.exec(dml);
    if (!match || !match[1].trim()) {
        return [];
    }
    const args = match[1]
        .split(',')
        .map(arg => arg.trim())
        .filter(arg => arg.length > 0);
    // Convert PascalCase to snake_case and sort alphabetically
    // (Prolog args are passed in alphabetical order from dict)
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
 * Extract description from markdown (first heading or paragraph)
 */
export function extractDescription(markdown) {
    const lines = markdown.split('\n');
    // Look for first heading
    for (const line of lines) {
        if (line.startsWith('# ')) {
            return line.substring(2).trim();
        }
    }
    // Fall back to first non-empty line
    for (const line of lines) {
        const trimmed = line.trim();
        if (trimmed && !trimmed.startsWith('#')) {
            return trimmed.substring(0, 100);
        }
    }
    return 'No description';
}
// =============================================================================
// Validation
// =============================================================================
/**
 * Basic DML syntax validation
 */
export function validateDMLSyntax(dml) {
    const errors = [];
    // Check for agent_main
    if (!dml.includes('agent_main')) {
        errors.push('Missing agent_main predicate');
    }
    // Check for balanced parentheses
    let parenCount = 0;
    for (const char of dml) {
        if (char === '(')
            parenCount++;
        if (char === ')')
            parenCount--;
        if (parenCount < 0) {
            errors.push('Unbalanced parentheses');
            break;
        }
    }
    if (parenCount !== 0 && !errors.includes('Unbalanced parentheses')) {
        errors.push('Unbalanced parentheses');
    }
    // Check for balanced brackets
    let bracketCount = 0;
    for (const char of dml) {
        if (char === '[')
            bracketCount++;
        if (char === ']')
            bracketCount--;
        if (bracketCount < 0) {
            errors.push('Unbalanced brackets');
            break;
        }
    }
    if (bracketCount !== 0 && !errors.includes('Unbalanced brackets')) {
        errors.push('Unbalanced brackets');
    }
    // Check for unclosed strings
    const stringPattern = /"(?:[^"\\]|\\.)*"/g;
    const cleanedDml = dml.replace(stringPattern, '""');
    if ((cleanedDml.match(/"/g) || []).length % 2 !== 0) {
        errors.push('Unclosed string literal');
    }
    return {
        valid: errors.length === 0,
        errors: [...new Set(errors)] // Dedupe
    };
}
// =============================================================================
// Helpers
// =============================================================================
/**
 * Compute SHA-256 hash of content
 */
function computeHash(content) {
    return 'sha256:' + crypto.createHash('sha256').update(content).digest('hex').substring(0, 16);
}
/**
 * Load existing meta file if it exists
 */
async function loadExistingMeta(metaPath) {
    try {
        const content = await fs.readFile(metaPath, 'utf-8');
        return JSON.parse(content);
    }
    catch {
        return null;
    }
}
//# sourceMappingURL=compile.js.map