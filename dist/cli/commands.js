/**
 * DeepClause CLI - Command Listing Module
 *
 * Lists compiled DML commands and their metadata.
 */
import * as fs from 'fs/promises';
import * as path from 'path';
import { configExists, ensureSystemOverrideSeeds, getSystemDir, getToolsDir } from './config.js';
const SYSTEM_COMMAND_DEFINITIONS = [
    {
        name: 'plan',
        description: 'Creates a simple standalone DML plan file from a request and saves it under plans/ in your workspace.',
        parameters: [
            { name: 'request', description: 'What the generated plan should do', required: true, position: 0 },
        ],
    },
    {
        name: 'deep-planner',
        description: 'Generates a multi-strategy DML plan with multiple agent_main clauses. Prolog tries strategies in order via backtracking.',
        parameters: [
            { name: 'request', description: 'What the generated plan should do', required: true, position: 0 },
        ],
    },
    {
        name: 'security-planner',
        description: 'Generates a multi-strategy security analysis plan for finding bugs and vulnerabilities.',
        parameters: [
            { name: 'request', description: 'Target to analyze (directory, file pattern, or specific concern)', required: true, position: 0 },
        ],
    },
];
const SYSTEM_COMMAND_NAMES = new Set(SYSTEM_COMMAND_DEFINITIONS.map((definition) => definition.name));
// =============================================================================
// Command Listing
// =============================================================================
/**
 * List all compiled DML commands
 */
export async function listCommands(workspaceRoot, options = {}) {
    const toolsDir = getToolsDir(workspaceRoot);
    const commands = [];
    if (await configExists(workspaceRoot)) {
        await ensureSystemOverrideSeeds(workspaceRoot);
    }
    let files = [];
    try {
        files = await fs.readdir(toolsDir);
    }
    catch (error) {
        if (error.code !== 'ENOENT') {
            throw error;
        }
    }
    // Find all .dml files
    const dmlFiles = files.filter((fileName) => {
        if (!fileName.endsWith('.dml')) {
            return false;
        }
        return !SYSTEM_COMMAND_NAMES.has(fileName.replace('.dml', ''));
    });
    for (const dmlFile of dmlFiles) {
        const name = dmlFile.replace('.dml', '');
        const metaPath = path.join(toolsDir, `${name}.meta.json`);
        const dmlPath = path.join(toolsDir, dmlFile);
        const commandPath = path.relative(workspaceRoot, dmlPath).replace(/\.dml$/, '');
        let meta = null;
        try {
            const content = await fs.readFile(metaPath, 'utf-8');
            meta = JSON.parse(content);
        }
        catch {
            // No meta file, use defaults
        }
        const orderedParameters = orderParameters(meta?.parameters);
        const command = {
            name,
            displayName: normalizeDisplayName(meta?.name),
            path: commandPath,
            description: meta?.description || 'No description available',
            usage: buildCliUsage(commandPath, orderedParameters),
        };
        if (options.detailed && meta) {
            command.parameters = orderedParameters;
            command.triggerPhrases = normalizeTriggerPhrases(meta);
            command.capabilities = humanizeCapabilities(meta.capabilities);
            command.tools = meta.tools;
            command.compiledAt = meta.compiledAt;
            command.model = meta.model;
        }
        commands.push(command);
    }
    commands.push(...await listSystemCommands(workspaceRoot, options));
    // Sort by name
    commands.sort((a, b) => a.name.localeCompare(b.name));
    return commands;
}
/**
 * Get information about a specific command
 */
export async function getCommand(workspaceRoot, name) {
    const systemCommand = await getSystemCommand(workspaceRoot, name);
    if (systemCommand) {
        return systemCommand;
    }
    const toolsDir = getToolsDir(workspaceRoot);
    const dmlPath = path.join(toolsDir, `${name}.dml`);
    const metaPath = path.join(toolsDir, `${name}.meta.json`);
    // Check if DML file exists
    try {
        await fs.access(dmlPath);
    }
    catch {
        return null;
    }
    let meta = null;
    try {
        const content = await fs.readFile(metaPath, 'utf-8');
        meta = JSON.parse(content);
    }
    catch {
        // No meta file
    }
    const commandPath = path.relative(workspaceRoot, dmlPath).replace(/\.dml$/, '');
    const orderedParameters = orderParameters(meta?.parameters);
    return {
        name,
        displayName: normalizeDisplayName(meta?.name),
        path: commandPath,
        description: meta?.description || 'No description available',
        usage: buildCliUsage(commandPath, orderedParameters),
        parameters: orderedParameters,
        triggerPhrases: meta ? normalizeTriggerPhrases(meta) : undefined,
        capabilities: humanizeCapabilities(meta?.capabilities),
        tools: meta?.tools,
        compiledAt: meta?.compiledAt,
        model: meta?.model
    };
}
/**
 * Check if a command exists
 */
export async function commandExists(workspaceRoot, name) {
    if (await getSystemCommand(workspaceRoot, name)) {
        return true;
    }
    const toolsDir = getToolsDir(workspaceRoot);
    const dmlPath = path.join(toolsDir, `${name}.dml`);
    try {
        await fs.access(dmlPath);
        return true;
    }
    catch {
        return false;
    }
}
async function listSystemCommands(workspaceRoot, options) {
    if (!await configExists(workspaceRoot)) {
        return [];
    }
    const commands = await Promise.all(SYSTEM_COMMAND_DEFINITIONS.map((definition) => buildSystemCommand(workspaceRoot, definition, options)));
    return commands.filter((command) => command !== null);
}
async function getSystemCommand(workspaceRoot, name) {
    const definition = SYSTEM_COMMAND_DEFINITIONS.find((entry) => entry.name === name);
    if (!definition || !await configExists(workspaceRoot)) {
        return null;
    }
    return buildSystemCommand(workspaceRoot, definition, { detailed: true });
}
async function buildSystemCommand(workspaceRoot, definition, options) {
    const dmlPath = path.join(getSystemDir(workspaceRoot), `${definition.name}.dml`);
    try {
        await fs.access(dmlPath);
    }
    catch {
        return null;
    }
    const commandPath = path.relative(workspaceRoot, dmlPath).replace(/\.dml$/, '');
    const orderedParameters = orderParameters(definition.parameters);
    return {
        name: definition.name,
        path: commandPath,
        description: definition.description,
        usage: buildCliUsage(commandPath, orderedParameters),
        parameters: options.detailed ? orderedParameters : undefined,
    };
}
function orderParameters(parameters) {
    if (!parameters || parameters.length === 0) {
        return undefined;
    }
    return [...parameters]
        .map((parameter, index) => ({ parameter, index }))
        .sort((left, right) => (left.parameter.position ?? left.index) - (right.parameter.position ?? right.index))
        .map(({ parameter }) => parameter);
}
function buildCliUsage(commandPath, parameters) {
    const placeholders = (parameters ?? []).map(formatUsageParameter);
    return ['deepclause', 'run', commandPath, ...placeholders].join(' ').trim();
}
function formatUsageParameter(parameter) {
    if (parameter.required === false || parameter.default !== undefined) {
        return parameter.default !== undefined
            ? `[${parameter.name}=${parameter.default}]`
            : `[${parameter.name}]`;
    }
    return `<${parameter.name}>`;
}
function normalizeDisplayName(value) {
    const trimmed = value?.trim();
    return trimmed ? trimmed : undefined;
}
function normalizeTriggerPhrases(meta) {
    const source = Array.isArray(meta.triggerPhrases)
        ? meta.triggerPhrases
        : (Array.isArray(meta.trigger_phrases) ? meta.trigger_phrases : undefined);
    if (!source || source.length === 0) {
        return undefined;
    }
    const triggerPhrases = Array.from(new Set(source
        .filter((phrase) => typeof phrase === 'string')
        .map((phrase) => phrase.trim())
        .filter((phrase) => phrase.length > 0)));
    return triggerPhrases.length > 0 ? triggerPhrases : undefined;
}
function humanizeCapabilities(capabilities) {
    if (!capabilities || capabilities.length === 0) {
        return undefined;
    }
    return capabilities.map((capability) => humanizeCapability(capability));
}
function humanizeCapability(capability) {
    switch (capability) {
        case 'file_io':
            return 'Reads or writes workspace files';
        case 'network':
            return 'Uses network access';
        case 'shell':
            return 'Runs shell commands';
        default: {
            const normalized = capability.replace(/_/g, ' ').trim();
            return normalized.charAt(0).toUpperCase() + normalized.slice(1);
        }
    }
}
//# sourceMappingURL=commands.js.map