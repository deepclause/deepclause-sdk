/**
 * DeepClause CLI Configuration Module
 *
 * Handles configuration loading, validation, and management.
 */
import { z } from 'zod';
import type { CompactionOptions } from '../types.js';
import { type ModelSlot, type ResolvedModelConfig } from '../system/config/model-slots.js';
export type { ModelSlot, Provider, ResolvedModelConfig } from '../system/config/model-slots.js';
export { buildModelOverride } from '../system/config/model-slots.js';
declare const MCPServerSchema: any;
export declare const ConfigSchema: any;
export type Config = z.infer<typeof ConfigSchema>;
export type MCPServer = z.infer<typeof MCPServerSchema>;
export declare function getConfigDir(workspaceRoot: string): string;
export declare function getConfigPath(workspaceRoot: string): string;
export declare function getToolsDir(workspaceRoot: string): string;
export declare function getSystemDir(workspaceRoot: string): string;
export declare function getDocsDir(workspaceRoot: string): string;
/**
 * Initialize DeepClause configuration in a workspace
 */
export declare function initConfig(workspaceRoot: string, options?: {
    force?: boolean;
    model?: string;
}): Promise<void>;
export declare function ensureSystemOverrideSeeds(workspaceRoot: string, options?: {
    overwrite?: boolean;
}): Promise<void>;
export declare function ensureWorkspaceDocSeeds(workspaceRoot: string, options?: {
    overwrite?: boolean;
}): Promise<void>;
/**
 * Load and validate configuration
 */
export declare function loadConfig(workspaceRoot: string): Promise<Config>;
/**
 * Validate configuration object
 */
export declare function validateConfig(config: unknown): Config;
/**
 * Parse a model string in either canonical provider:model form, legacy provider/model form,
 * or just model name form.
 */
export declare function parseModelString(modelString: string): string;
/**
 * Format a model id as canonical provider:model
 */
export declare function formatModelString(modelId: string): string;
/**
 * Set the default model in configuration.
 * When slot is omitted, all slots are updated for backward compatibility.
 */
export declare function setModel(workspaceRoot: string, modelString: string, slot?: ModelSlot): Promise<{
    modelId: string;
    updatedSlots: ModelSlot[];
    info: string;
}>;
/**
 * Get the current model configuration from configuration
 */
export declare function showModel(workspaceRoot: string): Promise<{
    models: Record<ModelSlot, string>;
    temperatures: Record<ModelSlot, number>;
    formatted: string;
}>;
export declare function resolveModelSlot(config: Config, slot: ModelSlot, overrides?: {
    modelId?: string;
    temperature?: number;
}): ResolvedModelConfig;
export declare function applyResolvedModelConfig(selection: ResolvedModelConfig): void;
export declare function getDefaultConfig(): Config;
/**
 * Update configuration with partial changes
 */
export declare function updateConfig(workspaceRoot: string, updates: Partial<Config>): Promise<Config>;
export declare function resolveCompactionConfig(config: Config, workspaceRoot: string): CompactionOptions | undefined;
/**
 * Check if configuration exists
 */
export declare function configExists(workspaceRoot: string): Promise<boolean>;
export declare function deepClauseDirExists(workspaceRoot: string): Promise<boolean>;
/**
 * Get list of configured MCP servers
 */
export declare function getMCPServers(config: Config): Record<string, MCPServer>;
//# sourceMappingURL=config.d.ts.map