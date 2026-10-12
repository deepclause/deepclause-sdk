/**
 * DeepClause CLI Configuration Module
 * 
 * Handles configuration loading, validation, and management.
 */

import { z } from 'zod';
import { readFileSync } from 'fs';
import * as fs from 'fs/promises';
import * as path from 'path';
import {
  getWorkspaceDocAssetPath,
} from '../system/assets/index.js';
import type { CompactionSettings } from '../repl/compaction.js';
import {
  DEFAULT_MODEL_IDS,
  DEFAULT_TEMPERATURES,
  buildModelOverride,
  formatModelId,
  normalizeModelId,
  resolveModelSlotConfig,
  type ModelSlot,
  type Provider,
  type ResolvedModelConfig,
} from '../system/config/model-slots.js';

export type { ModelSlot, Provider, ResolvedModelConfig } from '../system/config/model-slots.js';
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
  /** Wall-clock timeout for a single shell command, in ms (0 = disabled). */
  timeoutMs: z.number().int().nonnegative().optional(),
  /** Kill a command that produces no output for this many ms (0 = disabled). */
  idleTimeoutMs: z.number().int().nonnegative().optional(),
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

/**
 * DML REPL settings. All optional; see docs/DML_REPL_PROPOSAL.md §13.4.
 */
/** TypeScript compaction settings for the REPL (ported from pi). */
const ReplCompactionSchema = z.object({
  enabled: z.boolean().optional().default(true),
  /** Compact automatically when the context crosses the threshold. */
  auto: z.boolean().optional().default(true),
  reserveTokens: z.number().int().positive().optional().default(16384),
  keepRecentTokens: z.number().int().positive().optional().default(20000),
}).optional().default({});

const ReplConfigSchema = z.object({
  exportOnExit: z.boolean().optional().default(false),
  exportPath: z.string().optional().default('harness.dml'),
  frozen: z.boolean().optional().default(false),
  locked: z.boolean().optional().default(false),
  editMode: z.enum(['transactional', 'direct']).optional().default('transactional'),
  maxDepth: z.number().int().positive().optional().default(3),
  // 0 means unlimited.
  maxModelCalls: z.number().int().nonnegative().optional().default(100),
  writeDepth: z.number().int().positive().optional().default(20),
  /** Harness to edit by default (a project can hold several; default `main`). */
  harness: z.string().optional().default('main'),
  promptPrefix: z.string().optional().default('dml> '),
  /** Interactive UI: auto (TUI on a TTY), on, or off (line mode). */
  tui: z.enum(['auto', 'on', 'off']).optional().default('auto'),
  render: z.enum(['rich', 'plain', 'json']).optional(),
  showReasoning: z.enum(['off', 'summary', 'full']).optional().default('summary'),
  showToolArgs: z.boolean().optional().default(true),
  maxToolResultChars: z.number().int().positive().optional().default(2000),
  color: z.boolean().optional().default(true),
  tools: z.array(z.string()).nullish(),
  /** pi-style context compaction (TypeScript; replaces the DML compactor). */
  compaction: ReplCompactionSchema,
  /** Media display + image generation (docs/DML_MEDIA.md). */
  media: z.object({
    /** Workspace-relative directory for generated images. */
    dir: z.string().optional().default('.deepclause/media'),
    maxBytes: z.number().int().positive().optional().default(10485760),
    /** Image model as `provider:id`; default: first available. */
    imageModel: z.string().optional(),
    /** Default generation size in px. */
    size: z.number().int().positive().optional(),
    /** Hand images to the OS viewer when the terminal cannot render them. */
    open: z.boolean().optional().default(true),
    /** Include base64 image data in `--json` events (default true). */
    inlineJson: z.boolean().optional().default(true),
  }).optional().default({}),
  /** Persistent session/trace/harness history (docs/DML_REPL_SESSION_HISTORY.md). */
  history: z.object({
    enabled: z.boolean().optional().default(true),
    dir: z.string().optional().default('.deepclause/repl'),
    trace: z.enum(['off', 'summary', 'full']).optional().default('summary'),
    maxEventBytes: z.number().int().positive().optional().default(8192),
    maxSessions: z.number().int().positive().optional().default(50),
    maxVersions: z.number().int().positive().optional().default(200),
    includeInExport: z.boolean().optional().default(false),
    redact: z.array(z.string()).optional(),
  }).optional().default({}),
}).optional().default({});

/**
 * Model backend selection. `pi` (the default) uses pi's model registry and
 * credential store, which ship as a dependency of this SDK; `env` uses the
 * SDK provider/API-key settings.
 */
const BackendConfigSchema = z.object({
  kind: z.enum(['env', 'pi']).optional().default('pi'),
  agentDir: z.string().optional(),
  modelsPath: z.string().nullish(),
  authPath: z.string().nullish(),
  model: z.string().optional(),
  /** Thinking level for reasoning models: off|minimal|low|medium|high|xhigh|max. */
  thinkingLevel: z.string().optional(),
}).optional().default({ kind: 'pi' });

const JevJudgeConfigSchema = z.object({
  enabled: z.boolean().optional().default(false),
  model: z.string().optional().default('jev-latest'),
  apiKeyEnv: z.string().optional().default('TYPESAFE_API_KEY'),
}).optional().default({});

/**
 * Semantic judgment backend selection. `llm` reuses the active model; `jev`
 * (TypeSafe System One) is opt-in and only registered when enabled and its API
 * key is present. See docs/DML_REPL_PROPOSAL.md and DML_REFERENCE.md.
 */
const JudgmentConfigSchema = z.object({
  default: z.string().optional().default('llm'),
  jev: JevJudgeConfigSchema,
}).optional().default({});

export const ConfigSchema = z.object({
  models: ModelsSchema.default(DEFAULT_MODEL_IDS),
  backend: BackendConfigSchema,
  temperatures: TemperaturesSchema.default(DEFAULT_TEMPERATURES),
  modelOptions: ModelOptionsSchema,
  providers: ProvidersSchema,
  mcp: MCPConfigSchema.optional().default({ servers: {} }),
  agentvm: AgentVMConfigSchema,
  shell: ShellConfigSchema,
  repl: ReplConfigSchema,
  judgment: JudgmentConfigSchema,
  dmlBase: z.string().optional().default('.deepclause/tools'),
  workspace: z.string().optional().default('./'),
  model: z.string().optional(),
  provider: z.enum(['openai', 'anthropic', 'google', 'openrouter']).optional(),
});

export type Config = z.infer<typeof ConfigSchema>;
export type MCPServer = z.infer<typeof MCPServerSchema>;

// =============================================================================
// Default Configuration
// =============================================================================

const DEFAULT_CONFIG: Config = {
  models: { ...DEFAULT_MODEL_IDS },
  temperatures: { ...DEFAULT_TEMPERATURES },
  modelOptions: {},
  backend: { kind: 'pi' },
  providers: {},
  mcp: { servers: {} },
  agentvm: { network: false },
  shell: { wrapper: 'auto', strictIsolation: false },
  repl: {
    exportOnExit: false,
    exportPath: 'harness.dml',
    frozen: false,
    locked: false,
    editMode: 'transactional',
    maxDepth: 3,
    maxModelCalls: 100,
    writeDepth: 20,
    promptPrefix: 'dml> ',
    harness: 'main',
    tui: 'auto' as const,
    showReasoning: 'summary',
    showToolArgs: true,
    maxToolResultChars: 2000,
    color: true,
    media: { dir: '.deepclause/media', maxBytes: 10485760, open: true, inlineJson: true },
    compaction: {
      enabled: true,
      auto: true,
      reserveTokens: 16384,
      keepRecentTokens: 20000,
    },
    history: {
      enabled: true,
      dir: '.deepclause/repl',
      trace: 'summary' as const,
      maxEventBytes: 8192,
      maxSessions: 50,
      maxVersions: 200,
      includeInExport: false,
    },
  },
  judgment: {
    default: 'llm',
    jev: { enabled: false, model: 'jev-latest', apiKeyEnv: 'TYPESAFE_API_KEY' },
  },
  dmlBase: '.deepclause/tools',
  workspace: './'
};

// =============================================================================
// Configuration Paths
// =============================================================================

export function getConfigDir(workspaceRoot: string): string {
  return path.join(workspaceRoot, '.deepclause');
}

export function getConfigPath(workspaceRoot: string): string {
  return path.join(getConfigDir(workspaceRoot), 'config.json');
}

export function getToolsDir(workspaceRoot: string): string {
  return path.join(getConfigDir(workspaceRoot), 'tools');
}

export function getSystemDir(workspaceRoot: string): string {
  return path.join(getConfigDir(workspaceRoot), 'system');
}

export function getDocsDir(workspaceRoot: string): string {
  return path.join(getConfigDir(workspaceRoot), 'docs');
}

// =============================================================================
// Configuration Operations
// =============================================================================

/**
 * Initialize DeepClause configuration in a workspace
 */
export async function initConfig(
  workspaceRoot: string,
  options: { force?: boolean; model?: string } = {}
): Promise<void> {
  const configDir = getConfigDir(workspaceRoot);
  const configPath = getConfigPath(workspaceRoot);
  const toolsDir = getToolsDir(workspaceRoot);
  const systemDir = getSystemDir(workspaceRoot);
  const docsDir = getDocsDir(workspaceRoot);

  // Check for existing config
  try {
    await fs.access(configPath);
    if (!options.force) {
      throw new Error(
        'Configuration already exists. Use --force to overwrite.'
      );
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') {
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
  const config: Config = {
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

  await ensureWorkspaceDocSeeds(workspaceRoot, { overwrite: true });
}

export async function ensureWorkspaceDocSeeds(
  workspaceRoot: string,
  options: { overwrite?: boolean } = {},
): Promise<void> {
  const docsDir = getDocsDir(workspaceRoot);
  await fs.mkdir(docsDir, { recursive: true });
  await copyWorkspaceDocSeeds(docsDir, options.overwrite ?? false);
}

async function copyWorkspaceDocSeeds(docsDir: string, overwrite: boolean): Promise<void> {
  const packagedDocs = [
    {
      fileName: 'DML_REFERENCE.md',
      content: readFileSync(getWorkspaceDocAssetPath('dml-reference'), 'utf8'),
    },
    {
      fileName: 'DML_AUTHORING.md',
      content: readFileSync(getWorkspaceDocAssetPath('dml-authoring'), 'utf8'),
    },
  ];

  await Promise.all(packagedDocs.map(async ({ fileName, content }) => {
    const filePath = path.join(docsDir, fileName);
    if (!overwrite) {
      try {
        await fs.access(filePath);
        return;
      } catch {
        // Seed the packaged doc when it is missing.
      }
    }
    await fs.writeFile(filePath, content, 'utf8');
  }));
}

/**
 * Load and validate configuration
 */
export async function loadConfig(workspaceRoot: string): Promise<Config> {
  const configPath = getConfigPath(workspaceRoot);

  let rawConfig: unknown;
  try {
    const content = await fs.readFile(configPath, 'utf-8');
    rawConfig = JSON.parse(content);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      throw new Error(
        `Configuration not found at ${configPath}. Run 'deepclause init' first.`
      );
    }
    throw new Error(`Failed to read config: ${(error as Error).message}`);
  }

  // Resolve environment variables in config
  const resolvedConfig = resolveEnvVars(rawConfig);
  const migratedConfig = migrateLegacyConfig(resolvedConfig);

  // Validate
  const config = validateConfig(migratedConfig);
  await ensureWorkspaceDocSeeds(workspaceRoot);
  return config;
}

/**
 * Validate configuration object
 */
export function validateConfig(config: unknown): Config {
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
export function parseModelString(modelString: string): string {
  return normalizeModelId(modelString);
}

/**
 * Format a model id as canonical provider:model
 */
export function formatModelString(modelId: string): string {
  return formatModelId(modelId);
}

/**
 * Set the default model in configuration.
 * When slot is omitted, all slots are updated for backward compatibility.
 */
export async function setModel(
  workspaceRoot: string,
  modelString: string,
  slot?: ModelSlot,
): Promise<{ modelId: string; updatedSlots: ModelSlot[]; info: string }> {
  const modelId = parseModelString(modelString);

  const config = await loadConfig(workspaceRoot);
  const updatedSlots: ModelSlot[] = slot ? [slot] : ['gateway', 'run', 'compile'];
  for (const nextSlot of updatedSlots) {
    config.models[nextSlot] = modelId;
  }

  const configPath = getConfigPath(workspaceRoot);
  await fs.writeFile(configPath, serializeConfig(config));

  const resolved = resolveModelSlotConfig(config, updatedSlots[0]);
  const infoLines: string[] = [];
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
export async function showModel(workspaceRoot: string): Promise<{
  models: Record<ModelSlot, string>;
  temperatures: Record<ModelSlot, number>;
  formatted: string;
}> {
  const config = await loadConfig(workspaceRoot);
  const lines = (['gateway', 'run', 'compile'] as ModelSlot[]).map((slotName) => {
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

export function resolveModelSlot(
  config: Config,
  slot: ModelSlot,
  overrides: { modelId?: string; temperature?: number } = {},
): ResolvedModelConfig {
  return resolveModelSlotConfig(config, slot, overrides);
}

export function applyResolvedModelConfig(selection: ResolvedModelConfig): void {
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

export function getDefaultConfig(): Config {
  return {
    ...DEFAULT_CONFIG,
    models: { ...DEFAULT_CONFIG.models },
    temperatures: { ...DEFAULT_CONFIG.temperatures },
    providers: { ...DEFAULT_CONFIG.providers },
    backend: { ...DEFAULT_CONFIG.backend },
    mcp: { servers: {} },
    agentvm: { network: DEFAULT_CONFIG.agentvm?.network ?? false },
    shell: { ...DEFAULT_CONFIG.shell },
    repl: {
      ...DEFAULT_CONFIG.repl,
      compaction: { ...DEFAULT_CONFIG.repl.compaction },
    },
  };
}

/**
 * Update configuration with partial changes
 */
export async function updateConfig(
  workspaceRoot: string,
  updates: Partial<Config>
): Promise<Config> {
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
function resolveEnvVars(obj: unknown): unknown {
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
    const result: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(obj)) {
      result[key] = resolveEnvVars(value);
    }
    return result;
  }

  return obj;
}

function migrateLegacyConfig(config: unknown): unknown {
  if (config === null || typeof config !== 'object' || Array.isArray(config)) {
    return config;
  }

  const record = { ...(config as Record<string, unknown>) };
  const legacyModel = typeof record.model === 'string' ? record.model : undefined;
  const legacyProvider = typeof record.provider === 'string' ? record.provider as Provider : undefined;
  const legacyModelId = legacyModel ? buildModelOverride(legacyModel, legacyProvider) : undefined;

  const rawModels = record.models && typeof record.models === 'object' && !Array.isArray(record.models)
    ? record.models as Record<string, unknown>
    : {};
  const rawTemperatures = record.temperatures && typeof record.temperatures === 'object' && !Array.isArray(record.temperatures)
    ? record.temperatures as Record<string, unknown>
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
    ? record.shell as Record<string, unknown>
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

function coerceTemperature(value: unknown, fallback: number): number {
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

function serializeConfig(config: Config): string {
  const { model, provider, ...rest } = config;
  return JSON.stringify(rest, null, 2) + '\n';
}

export interface ReplCompactionConfig extends CompactionSettings {
  /** Whether compaction is applied automatically when the threshold is crossed. */
  auto: boolean;
}

/** Resolve the REPL's TypeScript compaction settings. */
export function resolveReplCompactionConfig(config: Config): ReplCompactionConfig {
  const compaction = config.repl.compaction;
  return {
    enabled: compaction.enabled,
    auto: compaction.auto,
    reserveTokens: compaction.reserveTokens,
    keepRecentTokens: compaction.keepRecentTokens,
  };
}

/**
 * Check if configuration exists
 */
export type ReplConfig = Config['repl'];

/**
 * Load configuration for the REPL without requiring `.deepclause/` to exist.
 *
 * The REPL is a goal-mode-capable tool that must run from a clean checkout
 * (see the plan's smoke test), so a missing config falls back to defaults
 * rather than creating the workspace directory.
 */
export async function loadReplConfig(workspaceRoot: string): Promise<Config> {
  if (await configExists(workspaceRoot)) {
    return loadConfig(workspaceRoot);
  }
  return getDefaultConfig();
}

export async function configExists(workspaceRoot: string): Promise<boolean> {
  try {
    await fs.access(getConfigPath(workspaceRoot));
    return true;
  } catch {
    return false;
  }
}

export async function deepClauseDirExists(workspaceRoot: string): Promise<boolean> {
  try {
    await fs.access(getConfigDir(workspaceRoot));
    return true;
  } catch {
    return false;
  }
}

/**
 * Get list of configured MCP servers
 */
export function getMCPServers(config: Config): Record<string, MCPServer> {
  return config.mcp?.servers || {};
}
