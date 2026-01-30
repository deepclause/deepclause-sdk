/**
 * DeepClause CLI Configuration Module
 *
 * Handles configuration loading, validation, and management.
 */
import { z } from 'zod';
import * as fs from 'fs/promises';
import * as path from 'path';
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
export const ConfigSchema = z.object({
    model: z.string().min(1, 'Model is required'),
    provider: z.enum(['openai', 'anthropic', 'google', 'openrouter']).default('openai'),
    providers: ProvidersSchema,
    mcp: MCPConfigSchema.optional().default({ servers: {} }),
    agentvm: AgentVMConfigSchema,
    dmlBase: z.string().optional().default('.deepclause/tools'),
    workspace: z.string().optional().default('./')
});
// =============================================================================
// Default Configuration
// =============================================================================
const DEFAULT_CONFIG = {
    model: 'gpt-4o',
    provider: 'openai',
    providers: {},
    mcp: { servers: {} },
    agentvm: { network: false },
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
    // Create config with optional model override
    const config = {
        ...DEFAULT_CONFIG,
        model: options.model || DEFAULT_CONFIG.model
    };
    // Write config
    await fs.writeFile(configPath, JSON.stringify(config, null, 2) + '\n');
    // Create .gitignore for the .deepclause directory
    const gitignorePath = path.join(configDir, '.gitignore');
    const gitignoreContent = `# DeepClause generated files
*.meta.json

# Keep tools directory but ignore compiled files by default
# Uncomment to track compiled DML:
# !tools/*.dml
`;
    await fs.writeFile(gitignorePath, gitignoreContent);
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
    // Validate
    return validateConfig(resolvedConfig);
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
    return result.data;
}
/**
 * Parse a model string in format "provider/model" or just "model"
 * If no provider specified, attempts to infer from model name
 */
export function parseModelString(modelString) {
    if (!modelString || modelString.trim() === '') {
        throw new Error('Invalid model: model name cannot be empty');
    }
    // Check for provider/model format
    if (modelString.includes('/')) {
        const [providerPart, ...modelParts] = modelString.split('/');
        const provider = providerPart.toLowerCase();
        const model = modelParts.join('/'); // Handle models with / in name
        if (!['openai', 'anthropic', 'google', 'openrouter'].includes(provider)) {
            throw new Error(`Unknown provider: ${provider}. Valid providers: openai, anthropic, google, openrouter`);
        }
        return { provider, model };
    }
    // Infer provider from model name
    const model = modelString;
    let provider;
    if (model.startsWith('gpt-') || model.startsWith('o1') || model.startsWith('o3')) {
        provider = 'openai';
    }
    else if (model.startsWith('claude-')) {
        provider = 'anthropic';
    }
    else if (model.startsWith('gemini-')) {
        provider = 'google';
    }
    else {
        // Default to openrouter for unknown models
        provider = 'openrouter';
    }
    return { provider, model };
}
/**
 * Format provider and model as a single string
 */
export function formatModelString(provider, model) {
    return `${provider}/${model}`;
}
/**
 * Set the default model in configuration
 * Accepts format: "provider/model" or just "model" (provider inferred)
 */
export async function setModel(workspaceRoot, modelString) {
    const { provider, model } = parseModelString(modelString);
    const config = await loadConfig(workspaceRoot);
    config.model = model;
    config.provider = provider;
    const configPath = getConfigPath(workspaceRoot);
    await fs.writeFile(configPath, JSON.stringify(config, null, 2) + '\n');
    return { provider, model };
}
/**
 * Get the current model from configuration
 */
export async function showModel(workspaceRoot) {
    const config = await loadConfig(workspaceRoot);
    return {
        provider: config.provider,
        model: config.model,
        formatted: formatModelString(config.provider, config.model)
    };
}
/**
 * Update configuration with partial changes
 */
export async function updateConfig(workspaceRoot, updates) {
    const config = await loadConfig(workspaceRoot);
    const updated = { ...config, ...updates };
    // Re-validate
    const validated = validateConfig(updated);
    const configPath = getConfigPath(workspaceRoot);
    await fs.writeFile(configPath, JSON.stringify(validated, null, 2) + '\n');
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
/**
 * Get list of configured MCP servers
 */
export function getMCPServers(config) {
    return config.mcp?.servers || {};
}
//# sourceMappingURL=config.js.map