/**
 * DeepClause CLI Configuration Module
 *
 * Handles configuration loading, validation, and management.
 */
import { z } from 'zod';
import { readFileSync, readdirSync } from 'fs';
import * as fs from 'fs/promises';
import * as path from 'path';
import { ensureDefaultSkillSeeds, writeDefaultSkillSeeds } from './default-skills.js';
import { getSystemCompactorAssetPath, getPackagedRecipeAssetsDir, getSystemPromptAssetPath, getSystemSkillAssetPath, getWorkspaceDocAssetPath, getWorkspaceRecipeAssetsDir, } from '../system/assets/index.js';
import { DEFAULT_MODEL_IDS, DEFAULT_TEMPERATURES, buildModelOverride, formatModelId, normalizeModelId, resolveModelSlotConfig, } from '../system/config/model-slots.js';
export { buildModelOverride } from '../system/config/model-slots.js';
// =============================================================================
// Configuration Schema
// =============================================================================
const MCPServerSchema = z.object({
    command: z.string().min(1, 'MCP server command is required'),
    args: z.array(z.string()).optional().default([]),
    env: z.record(z.string()).optional().default({})
});
const MCPConfigSchema = z.object({
    servers: z.record(MCPServerSchema).optional().default({})
});
const AgentVMConfigSchema = z.object({
    /** Enable networking in the VM (default: false for security) */
    network: z.boolean().optional().default(false)
}).optional().default({ network: false });
const ShellConfigSchema = z.object({
    wrapper: z.enum(['auto', 'clean-room', 'bwrap', 'sandbox-exec']).optional().default('auto'),
    strictIsolation: z.boolean().optional().default(false),
}).optional().default({ wrapper: 'auto', strictIsolation: false });
const ProviderConfigSchema = z.object({
    apiKey: z.string().optional(),
    baseUrl: z.string().optional()
});
const ProvidersSchema = z.object({
    openai: ProviderConfigSchema.optional(),
    anthropic: ProviderConfigSchema.optional(),
    google: ProviderConfigSchema.optional(),
    openrouter: ProviderConfigSchema.optional()
}).optional().default({});
const ToolPolicySchema = z.object({
    mode: z.enum(['whitelist', 'blacklist']),
    tools: z.array(z.string()).default([]),
});
const CompactorDefinitionSchema = z.object({
    source: z.string().min(1, 'DML compactor source is required'),
    sourceType: z.enum(['inline', 'file', 'auto']).optional(),
    timeoutMs: z.number().int().nonnegative().optional(),
    gasLimit: z.number().int().positive().optional(),
    model: z.string().min(1).optional(),
    provider: z.enum(['openai', 'anthropic', 'google', 'openrouter']).optional(),
    inheritTools: z.boolean().optional(),
    toolPolicy: ToolPolicySchema.nullish(),
});
const CompactorBindingSchema = z.object({
    name: z.string().min(1).optional(),
    scope: z.enum(['session', 'loop', 'run']),
    trigger: z.enum(['before_user_message', 'before_model_call', 'before_task', 'after_task']),
    compactor: CompactorDefinitionSchema,
});
const CompactionConfigSchema = z.object({
    enabled: z.boolean().optional(),
    bindings: z.array(CompactorBindingSchema).optional(),
}).optional();
const ModelsSchema = z.object({
    gateway: z.string().min(1).default(DEFAULT_MODEL_IDS.gateway),
    run: z.string().min(1).default(DEFAULT_MODEL_IDS.run),
    compile: z.string().min(1).default(DEFAULT_MODEL_IDS.compile),
});
const TemperaturesSchema = z.object({
    gateway: z.number().min(0).max(2).default(DEFAULT_TEMPERATURES.gateway),
    run: z.number().min(0).max(2).default(DEFAULT_TEMPERATURES.run),
    compile: z.number().min(0).max(2).default(DEFAULT_TEMPERATURES.compile),
});
const ModelSlotOverridesSchema = z.object({
    maxContextTokens: z.number().int().positive().optional(),
    maxOutputTokens: z.number().int().positive().optional(),
    reasoningEffort: z.string().optional(),
});
const ModelOptionsSchema = z.object({
    gateway: ModelSlotOverridesSchema.optional(),
    run: ModelSlotOverridesSchema.optional(),
    compile: ModelSlotOverridesSchema.optional(),
}).optional().default({});
export const ConfigSchema = z.object({
    models: ModelsSchema.default(DEFAULT_MODEL_IDS),
    temperatures: TemperaturesSchema.default(DEFAULT_TEMPERATURES),
    modelOptions: ModelOptionsSchema,
    providers: ProvidersSchema,
    mcp: MCPConfigSchema.optional().default({ servers: {} }),
    agentvm: AgentVMConfigSchema,
    shell: ShellConfigSchema,
    compaction: CompactionConfigSchema,
    dmlBase: z.string().optional().default('.deepclause/tools'),
    workspace: z.string().optional().default('./'),
    model: z.string().optional(),
    provider: z.enum(['openai', 'anthropic', 'google', 'openrouter']).optional(),
});
// =============================================================================
// Default Configuration
// =============================================================================
const DEFAULT_CONFIG = {
    models: { ...DEFAULT_MODEL_IDS },
    temperatures: { ...DEFAULT_TEMPERATURES },
    modelOptions: {},
    providers: {},
    mcp: { servers: {} },
    agentvm: { network: false },
    shell: { wrapper: 'auto', strictIsolation: false },
    compaction: {
        enabled: true,
        bindings: [
            {
                name: 'default-session',
                scope: 'session',
                trigger: 'before_user_message',
                compactor: {
                    source: '.deepclause/system/default-session-compactor.dml',
                    sourceType: 'file',
                    timeoutMs: 15_000,
                    inheritTools: false,
                },
            },
            {
                name: 'default-loop',
                scope: 'loop',
                trigger: 'before_model_call',
                compactor: {
                    source: '.deepclause/system/default-loop-compactor.dml',
                    sourceType: 'file',
                    timeoutMs: 15_000,
                    inheritTools: false,
                },
            },
        ],
    },
    dmlBase: '.deepclause/tools',
    workspace: './'
};
// =============================================================================
// Configuration Paths
// =============================================================================
export function getConfigDir(workspaceRoot) {
    return path.join(workspaceRoot, '.deepclause');
}
export function getConfigPath(workspaceRoot) {
    return path.join(getConfigDir(workspaceRoot), 'config.json');
}
export function getToolsDir(workspaceRoot) {
    return path.join(getConfigDir(workspaceRoot), 'tools');
}
export function getSystemDir(workspaceRoot) {
    return path.join(getConfigDir(workspaceRoot), 'system');
}
export function getDocsDir(workspaceRoot) {
    return path.join(getConfigDir(workspaceRoot), 'docs');
}
// =============================================================================
// Configuration Operations
// =============================================================================
/**
 * Initialize DeepClause configuration in a workspace
 */
export async function initConfig(workspaceRoot, options = {}) {
    const configDir = getConfigDir(workspaceRoot);
    const configPath = getConfigPath(workspaceRoot);
    const toolsDir = getToolsDir(workspaceRoot);
    const systemDir = getSystemDir(workspaceRoot);
    const docsDir = getDocsDir(workspaceRoot);
    // Check for existing config
    try {
        await fs.access(configPath);
        if (!options.force) {
            throw new Error('Configuration already exists. Use --force to overwrite.');
        }
    }
    catch (error) {
        if (error.code !== 'ENOENT') {
            throw error;
        }
    }
    // Create directories
    await fs.mkdir(configDir, { recursive: true });
    await fs.mkdir(toolsDir, { recursive: true });
    await fs.mkdir(systemDir, { recursive: true });
    await fs.mkdir(docsDir, { recursive: true });
    // Create config with optional model override
    const initialModelId = options.model ? normalizeModelId(options.model) : DEFAULT_MODEL_IDS.run;
    const config = {
        ...DEFAULT_CONFIG,
        models: {
            gateway: initialModelId,
            run: initialModelId,
            compile: initialModelId,
        },
    };
    // Write config
    await fs.writeFile(configPath, serializeConfig(config));
    // Create .gitignore for the .deepclause directory
    const gitignorePath = path.join(configDir, '.gitignore');
    const gitignoreContent = `# DeepClause generated files
*.meta.json

# Keep tools directory but ignore compiled files by default
# Uncomment to track compiled DML:
# !tools/*.dml
`;
    await fs.writeFile(gitignorePath, gitignoreContent);
    await writeDefaultSkillSeeds(toolsDir, initialModelId);
    await ensureSystemOverrideSeeds(workspaceRoot, { overwrite: true });
    await writeRecipeSeeds(workspaceRoot, { overwrite: true });
    await ensureWorkspaceDocSeeds(workspaceRoot, { overwrite: true });
}
export async function ensureSystemOverrideSeeds(workspaceRoot, options = {}) {
    const systemDir = getSystemDir(workspaceRoot);
    await fs.mkdir(systemDir, { recursive: true });
    await copySystemOverrideSeeds(systemDir, options.overwrite ?? false);
}
async function copySystemOverrideSeeds(systemDir, overwrite) {
    const packagedAssets = [
        {
            fileName: 'conductor.dml',
            content: readFileSync(getSystemSkillAssetPath('conductor'), 'utf8'),
        },
        {
            fileName: 'skill-creator.dml',
            content: readFileSync(getSystemSkillAssetPath('skill-creator'), 'utf8'),
        },
        {
            fileName: 'plan.dml',
            content: readFileSync(getSystemSkillAssetPath('plan'), 'utf8'),
        },
        {
            fileName: 'deep-planner.dml',
            content: readFileSync(getSystemSkillAssetPath('deep-planner'), 'utf8'),
        },
        {
            fileName: 'security-planner.dml',
            content: readFileSync(getSystemSkillAssetPath('security-planner'), 'utf8'),
        },
        {
            fileName: 'CONDUCTOR_PROMPT.md',
            content: readFileSync(getSystemPromptAssetPath('conductor'), 'utf8'),
        },
        {
            fileName: 'DML_COMPILER_PROMPT.md',
            content: readFileSync(getSystemPromptAssetPath('skill-creator'), 'utf8'),
        },
        {
            fileName: 'TASK_PROMPT.md',
            content: readFileSync(getSystemPromptAssetPath('task'), 'utf8'),
        },
        {
            fileName: 'default-session-compactor.dml',
            content: readFileSync(getSystemCompactorAssetPath('default-session-compactor'), 'utf8'),
        },
        {
            fileName: 'default-loop-compactor.dml',
            content: readFileSync(getSystemCompactorAssetPath('default-loop-compactor'), 'utf8'),
        },
    ];
    await Promise.all(packagedAssets.map(async ({ fileName, content }) => {
        const filePath = path.join(systemDir, fileName);
        if (!overwrite) {
            try {
                await fs.access(filePath);
                return;
            }
            catch {
                // Seed the packaged system file when it is missing.
            }
        }
        await fs.writeFile(filePath, content, 'utf8');
    }));
}
async function writeRecipeSeeds(workspaceRoot, options = {}) {
    const sourceRoot = getPackagedRecipeAssetsDir();
    const targetRoot = getWorkspaceRecipeAssetsDir(workspaceRoot);
    await fs.mkdir(targetRoot, { recursive: true });
    await copyRecipeSeedDirectory(sourceRoot, targetRoot, options.overwrite ?? false);
}
async function copyRecipeSeedDirectory(sourceDir, targetDir, overwrite) {
    const entries = readdirSync(sourceDir, { withFileTypes: true });
    for (const entry of entries) {
        const sourcePath = path.join(sourceDir, entry.name);
        const targetPath = path.join(targetDir, entry.name);
        if (entry.isDirectory()) {
            await fs.mkdir(targetPath, { recursive: true });
            await copyRecipeSeedDirectory(sourcePath, targetPath, overwrite);
            continue;
        }
        if (!overwrite) {
            try {
                await fs.access(targetPath);
                continue;
            }
            catch {
                // Seed the packaged recipe file when it is missing.
            }
        }
        await fs.writeFile(targetPath, readFileSync(sourcePath, 'utf8'), 'utf8');
    }
}
export async function ensureWorkspaceDocSeeds(workspaceRoot, options = {}) {
    const docsDir = getDocsDir(workspaceRoot);
    await fs.mkdir(docsDir, { recursive: true });
    await copyWorkspaceDocSeeds(docsDir, options.overwrite ?? false);
}
async function copyWorkspaceDocSeeds(docsDir, overwrite) {
    const packagedDocs = [
        {
            fileName: 'TUI.md',
            content: readFileSync(getWorkspaceDocAssetPath('tui'), 'utf8'),
        },
        {
            fileName: 'DML_REFERENCE.md',
            content: readFileSync(getWorkspaceDocAssetPath('dml-reference'), 'utf8'),
        },
    ];
    await Promise.all(packagedDocs.map(async ({ fileName, content }) => {
        const filePath = path.join(docsDir, fileName);
        if (!overwrite) {
            try {
                await fs.access(filePath);
                return;
            }
            catch {
                // Seed the packaged doc when it is missing.
            }
        }
        await fs.writeFile(filePath, content, 'utf8');
    }));
}
/**
 * Load and validate configuration
 */
export async function loadConfig(workspaceRoot) {
    const configPath = getConfigPath(workspaceRoot);
    let rawConfig;
    try {
        const content = await fs.readFile(configPath, 'utf-8');
        rawConfig = JSON.parse(content);
    }
    catch (error) {
        if (error.code === 'ENOENT') {
            throw new Error(`Configuration not found at ${configPath}. Run 'deepclause init' first.`);
        }
        throw new Error(`Failed to read config: ${error.message}`);
    }
    // Resolve environment variables in config
    const resolvedConfig = resolveEnvVars(rawConfig);
    const migratedConfig = migrateLegacyConfig(resolvedConfig);
    // Validate
    const config = validateConfig(migratedConfig);
    await ensureSystemOverrideSeeds(workspaceRoot);
    await ensureDefaultSkillSeeds(getToolsDir(workspaceRoot), config.models.run);
    await ensureWorkspaceDocSeeds(workspaceRoot);
    return config;
}
/**
 * Validate configuration object
 */
export function validateConfig(config) {
    const result = ConfigSchema.safeParse(config);
    if (!result.success) {
        const errors = result.error.issues
            .map(issue => `  - ${issue.path.join('.')}: ${issue.message}`)
            .join('\n');
        throw new Error(`Invalid configuration:\n${errors}`);
    }
    return {
        ...result.data,
        models: {
            gateway: normalizeModelId(result.data.models.gateway),
            run: normalizeModelId(result.data.models.run),
            compile: normalizeModelId(result.data.models.compile),
        },
    };
}
/**
 * Parse a model string in either canonical provider:model form, legacy provider/model form,
 * or just model name form.
 */
export function parseModelString(modelString) {
    return normalizeModelId(modelString);
}
/**
 * Format a model id as canonical provider:model
 */
export function formatModelString(modelId) {
    return formatModelId(modelId);
}
/**
 * Set the default model in configuration.
 * When slot is omitted, all slots are updated for backward compatibility.
 */
export async function setModel(workspaceRoot, modelString, slot) {
    const modelId = parseModelString(modelString);
    const config = await loadConfig(workspaceRoot);
    const updatedSlots = slot ? [slot] : ['gateway', 'run', 'compile'];
    for (const nextSlot of updatedSlots) {
        config.models[nextSlot] = modelId;
    }
    const configPath = getConfigPath(workspaceRoot);
    await fs.writeFile(configPath, serializeConfig(config));
    const resolved = resolveModelSlotConfig(config, updatedSlots[0]);
    const infoLines = [];
    infoLines.push(`Model set: ${modelId} (${updatedSlots.join(', ')} slot${updatedSlots.length > 1 ? 's' : ''})`);
    if (resolved.contextWindow) {
        infoLines.push(`  Context window: ${resolved.contextWindow.toLocaleString()} tokens`);
    }
    if (resolved.maxOutputTokens) {
        infoLines.push(`  Max output:     ${resolved.maxOutputTokens.toLocaleString()} tokens`);
    }
    infoLines.push(`  Reasoning:      ${resolved.reasoning ? 'yes' : 'no'}${resolved.reasoningType && resolved.reasoningType !== 'none' ? ` (${resolved.reasoningType})` : ''}`);
    infoLines.push(`  Complexity:     ${resolved.complexity ?? 'unknown'}`);
    return { modelId, updatedSlots, info: infoLines.join('\n') };
}
/**
 * Get the current model configuration from configuration
 */
export async function showModel(workspaceRoot) {
    const config = await loadConfig(workspaceRoot);
    const lines = ['gateway', 'run', 'compile'].map((slotName) => {
        const modelId = formatModelString(config.models[slotName]);
        const temperature = config.temperatures[slotName];
        const resolved = resolveModelSlotConfig(config, slotName);
        const ctx = resolved.contextWindow ? `${resolved.contextWindow.toLocaleString()}` : 'unknown';
        const out = resolved.maxOutputTokens ? `${resolved.maxOutputTokens.toLocaleString()}` : 'unknown';
        const reasoning = resolved.reasoning ? 'yes' : 'no';
        const complexity = resolved.complexity ?? 'unknown';
        return `${slotName}: ${modelId} (temp=${temperature}, ctx=${ctx}, out=${out}, reasoning=${reasoning}, complexity=${complexity})`;
    });
    return {
        models: { ...config.models },
        temperatures: { ...config.temperatures },
        formatted: lines.join('\n'),
    };
}
export function resolveModelSlot(config, slot, overrides = {}) {
    return resolveModelSlotConfig(config, slot, overrides);
}
export function applyResolvedModelConfig(selection) {
    if (!selection.apiKey) {
        return;
    }
    switch (selection.provider) {
        case 'openai':
            process.env.OPENAI_API_KEY = selection.apiKey;
            break;
        case 'anthropic':
            process.env.ANTHROPIC_API_KEY = selection.apiKey;
            break;
        case 'google':
            process.env.GOOGLE_GENERATIVE_AI_API_KEY = selection.apiKey;
            break;
        case 'openrouter':
            process.env.OPENROUTER_API_KEY = selection.apiKey;
            break;
    }
}
export function getDefaultConfig() {
    return {
        ...DEFAULT_CONFIG,
        models: { ...DEFAULT_CONFIG.models },
        temperatures: { ...DEFAULT_CONFIG.temperatures },
        providers: { ...DEFAULT_CONFIG.providers },
        mcp: { servers: {} },
        agentvm: { network: DEFAULT_CONFIG.agentvm?.network ?? false },
        shell: { ...DEFAULT_CONFIG.shell },
        compaction: DEFAULT_CONFIG.compaction
            ? {
                ...DEFAULT_CONFIG.compaction,
                bindings: DEFAULT_CONFIG.compaction.bindings
                    ? DEFAULT_CONFIG.compaction.bindings.map((binding) => ({
                        ...binding,
                        compactor: { ...binding.compactor },
                    }))
                    : undefined,
            }
            : undefined,
    };
}
/**
 * Update configuration with partial changes
 */
export async function updateConfig(workspaceRoot, updates) {
    const config = await loadConfig(workspaceRoot);
    const updated = {
        ...config,
        ...updates,
        models: { ...config.models, ...updates.models },
        temperatures: { ...config.temperatures, ...updates.temperatures },
        providers: { ...config.providers, ...updates.providers },
        mcp: updates.mcp ? { ...config.mcp, ...updates.mcp } : config.mcp,
        agentvm: updates.agentvm ? { ...config.agentvm, ...updates.agentvm } : config.agentvm,
        shell: updates.shell ? { ...config.shell, ...updates.shell } : config.shell,
        compaction: updates.compaction
            ? {
                ...config.compaction,
                ...updates.compaction,
                bindings: updates.compaction.bindings ?? config.compaction?.bindings,
            }
            : config.compaction,
    };
    // Re-validate
    const validated = validateConfig(updated);
    const configPath = getConfigPath(workspaceRoot);
    await fs.writeFile(configPath, serializeConfig(validated));
    return validated;
}
// =============================================================================
// Helper Functions
// =============================================================================
/**
 * Resolve environment variable references in configuration
 * Supports ${VAR_NAME} and $VAR_NAME syntax
 */
function resolveEnvVars(obj) {
    if (typeof obj === 'string') {
        // Replace ${VAR} or $VAR patterns
        return obj.replace(/\$\{([^}]+)\}|\$([A-Z_][A-Z0-9_]*)/gi, (match, braced, plain) => {
            const varName = braced || plain;
            const value = process.env[varName];
            if (value === undefined) {
                // Return original if not found - allows for optional env vars
                return match;
            }
            return value;
        });
    }
    if (Array.isArray(obj)) {
        return obj.map(resolveEnvVars);
    }
    if (obj !== null && typeof obj === 'object') {
        const result = {};
        for (const [key, value] of Object.entries(obj)) {
            result[key] = resolveEnvVars(value);
        }
        return result;
    }
    return obj;
}
function migrateLegacyConfig(config) {
    if (config === null || typeof config !== 'object' || Array.isArray(config)) {
        return config;
    }
    const record = { ...config };
    const legacyModel = typeof record.model === 'string' ? record.model : undefined;
    const legacyProvider = typeof record.provider === 'string' ? record.provider : undefined;
    const legacyModelId = legacyModel ? buildModelOverride(legacyModel, legacyProvider) : undefined;
    const rawModels = record.models && typeof record.models === 'object' && !Array.isArray(record.models)
        ? record.models
        : {};
    const rawTemperatures = record.temperatures && typeof record.temperatures === 'object' && !Array.isArray(record.temperatures)
        ? record.temperatures
        : {};
    record.models = {
        gateway: typeof rawModels.gateway === 'string'
            ? normalizeModelId(rawModels.gateway)
            : legacyModelId ?? DEFAULT_MODEL_IDS.gateway,
        run: typeof rawModels.run === 'string'
            ? normalizeModelId(rawModels.run)
            : legacyModelId ?? DEFAULT_MODEL_IDS.run,
        compile: typeof rawModels.compile === 'string'
            ? normalizeModelId(rawModels.compile)
            : legacyModelId ?? DEFAULT_MODEL_IDS.compile,
    };
    record.temperatures = {
        gateway: coerceTemperature(rawTemperatures.gateway, DEFAULT_TEMPERATURES.gateway),
        run: coerceTemperature(rawTemperatures.run, DEFAULT_TEMPERATURES.run),
        compile: coerceTemperature(rawTemperatures.compile, DEFAULT_TEMPERATURES.compile),
    };
    const rawShell = record.shell && typeof record.shell === 'object' && !Array.isArray(record.shell)
        ? record.shell
        : {};
    record.shell = {
        wrapper: typeof rawShell.wrapper === 'string' && ['auto', 'clean-room', 'bwrap', 'sandbox-exec'].includes(rawShell.wrapper)
            ? rawShell.wrapper
            : 'auto',
        strictIsolation: typeof rawShell.strictIsolation === 'boolean'
            ? rawShell.strictIsolation
            : false,
    };
    return record;
}
function coerceTemperature(value, fallback) {
    if (typeof value === 'number' && value >= 0 && value <= 2) {
        return value;
    }
    if (typeof value === 'string') {
        const parsed = Number.parseFloat(value);
        if (Number.isFinite(parsed) && parsed >= 0 && parsed <= 2) {
            return parsed;
        }
    }
    return fallback;
}
function serializeConfig(config) {
    const { model, provider, ...rest } = config;
    return JSON.stringify(rest, null, 2) + '\n';
}
export function resolveCompactionConfig(config, workspaceRoot) {
    const compaction = config.compaction;
    if (!compaction) {
        return undefined;
    }
    const bindings = compaction.bindings?.map((binding) => ({
        ...binding,
        compactor: {
            ...binding.compactor,
            source: binding.compactor.sourceType === 'file' && !path.isAbsolute(binding.compactor.source)
                ? path.resolve(workspaceRoot, binding.compactor.source)
                : binding.compactor.source,
        },
    }));
    return {
        ...compaction,
        bindings,
    };
}
/**
 * Check if configuration exists
 */
export async function configExists(workspaceRoot) {
    try {
        await fs.access(getConfigPath(workspaceRoot));
        return true;
    }
    catch {
        return false;
    }
}
export async function deepClauseDirExists(workspaceRoot) {
    try {
        await fs.access(getConfigDir(workspaceRoot));
        return true;
    }
    catch {
        return false;
    }
}
/**
 * Get list of configured MCP servers
 */
export function getMCPServers(config) {
    return config.mcp?.servers || {};
}
//# sourceMappingURL=config.js.map