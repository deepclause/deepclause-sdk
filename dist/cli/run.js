/**
 * DeepClause CLI - Execution Module
 *
 * Executes compiled DML programs with full tool support.
 */
import * as fs from 'fs/promises';
import * as path from 'path';
import { applyResolvedModelConfig, buildModelOverride, getToolsDir, loadConfig, resolveModelSlot, } from './config.js';
import { promptUser } from './interactive.js';
import { compilePrompt } from './compile.js';
import { executeDml } from '../system/runtime/dml-executor.js';
import { buildDmlParams } from '../system/runtime/dml-params.js';
import { verifyRuntimeToolsAvailable } from '../system/runtime/runtime-tools.js';
// =============================================================================
// Main Run Function
// =============================================================================
/**
 * Execute a compiled DML program or generate and run DML from a prompt
 */
export async function run(file, args, options = {}) {
    const configRoot = path.resolve(options.configRoot ?? process.cwd());
    const config = await loadConfig(configRoot);
    let dmlCode;
    let meta = null;
    let absolutePath;
    if (options.prompt) {
        if (options.verbose) {
            console.log(`[CLI] One-shot mode: generating DML from prompt...`);
        }
        const modelOverride = buildModelOverride(options.model, options.provider);
        const compileResult = await compilePrompt(options.prompt, {
            model: modelOverride,
            temperature: options.temperature,
            verbose: options.verbose,
            sandbox: options.sandbox,
            audit: options.audit,
        });
        dmlCode = compileResult.dml;
        const compileSelection = resolveModelSlot(config, 'compile', {
            modelId: modelOverride,
            temperature: options.temperature,
        });
        if (options.verbose) {
            console.log('\n--- Final Validated DML ---');
            console.log(dmlCode);
            console.log('---------------------------\n');
        }
        // Create a synthetic meta object for one-shot mode
        meta = {
            version: '1.0.0',
            source: 'oneshot',
            sourceHash: '',
            compiledAt: new Date().toISOString(),
            model: compileSelection.model,
            provider: compileSelection.provider,
            description: options.prompt,
            parameters: [],
            tools: compileResult.tools,
            history: []
        };
    }
    else {
        if (!file) {
            throw new Error('Either a DML file or a --prompt must be provided');
        }
        absolutePath = await resolveDmlPath(file, configRoot);
        // Load DML file
        try {
            dmlCode = await fs.readFile(absolutePath, 'utf-8');
        }
        catch {
            throw new Error(`Failed to read DML file: ${absolutePath}`);
        }
        // Try to load meta file
        const metaPath = absolutePath.replace(/\.dml$/, '.meta.json');
        try {
            const metaContent = await fs.readFile(metaPath, 'utf-8');
            meta = JSON.parse(metaContent);
        }
        catch {
            // Meta file is optional
        }
    }
    const runSelection = resolveModelSlot(config, 'run', {
        modelId: buildModelOverride(options.model, options.provider),
        temperature: options.temperature,
    });
    applyResolvedModelConfig(runSelection);
    // Resolve workspace path
    const workspacePath = options.workspace
        ? path.resolve(configRoot, options.workspace)
        : path.resolve(configRoot, config.workspace || './workspace');
    // Ensure workspace exists
    await fs.mkdir(workspacePath, { recursive: true });
    // Dry run mode - show what would be executed
    if (options.dryRun) {
        const params = buildDmlParams(args, options.params, meta);
        return {
            output: [],
            dryRun: true,
            wouldExecute: formatDryRun(absolutePath || 'oneshot', meta, params, runSelection.id, workspacePath)
        };
    }
    // Verify required tools are available
    if (meta?.tools && meta.tools.length > 0) {
        const toolCheck = verifyRuntimeToolsAvailable(config, meta.tools);
        if (!toolCheck.available) {
            throw new Error(`Missing required tools: ${toolCheck.missing.join(', ')}. ` +
                `Configure MCP servers or check tool names.`);
        }
    }
    // Build params from args and options
    const params = buildDmlParams(args, options.params, meta);
    const currentSkillSlug = absolutePath ? resolveCatalogSkillSlug(configRoot, absolutePath) : undefined;
    const result = await executeDml({
        dmlCode,
        config,
        workspaceRoot: configRoot,
        workspacePath,
        selection: runSelection,
        args,
        params,
        gasLimit: options.gasLimit,
        stream: options.stream,
        trace: !!options.trace,
        verbose: options.verbose,
        headless: options.headless,
        sandbox: options.sandbox,
        signal: options.signal,
        toolAbortSignalRef: options.toolAbortSignalRef,
        onEvent: options.onEvent,
        skillCatalog: {
            workspaceRoot: configRoot,
            currentSkillSlug,
            onChildEvent: options.onChildEvent,
        },
        onUserInput: options.onUserInput ?? (options.headless
            ? async () => ''
            : promptUser),
    });
    if (options.trace && result.trace) {
        const tracePath = path.resolve(options.trace);
        await fs.writeFile(tracePath, JSON.stringify(result.trace, null, 2) + '\n');
        if (options.verbose) {
            console.log(`Trace saved to: ${tracePath}`);
        }
    }
    const runResult = {
        output: result.output,
        answer: result.answer,
        error: result.error,
        trace: result.trace,
        events: result.events,
        usageByModel: result.usageByModel,
    };
    return runResult;
}
async function resolveDmlPath(file, configRoot) {
    const candidate = path.resolve(configRoot, file);
    if (await fileExists(candidate)) {
        return candidate;
    }
    if (!candidate.endsWith('.dml') && await fileExists(`${candidate}.dml`)) {
        return `${candidate}.dml`;
    }
    return candidate;
}
function resolveCatalogSkillSlug(configRoot, absolutePath) {
    const toolsDir = path.resolve(getToolsDir(configRoot));
    const fileDir = path.dirname(path.resolve(absolutePath));
    if (fileDir !== toolsDir) {
        return undefined;
    }
    return path.basename(absolutePath, '.dml');
}
async function fileExists(filePath) {
    try {
        await fs.access(filePath);
        return true;
    }
    catch {
        return false;
    }
}
// =============================================================================
// Formatting
// =============================================================================
/**
 * Format dry run output
 */
function formatDryRun(dmlPath, meta, params, modelId, workspacePath) {
    const lines = [
        '═══════════════════════════════════════════════════════════════',
        '  DRY RUN - Would execute the following:',
        '═══════════════════════════════════════════════════════════════',
        '',
        `  DML File:    ${dmlPath}`,
        `  Model:       ${modelId}`,
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
//# sourceMappingURL=run.js.map