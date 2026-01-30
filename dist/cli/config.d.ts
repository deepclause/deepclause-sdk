/**
 * DeepClause CLI Configuration Module
 *
 * Handles configuration loading, validation, and management.
 */
import { z } from 'zod';
declare const MCPServerSchema: z.ZodObject<{
    command: z.ZodString;
    args: z.ZodDefault<z.ZodOptional<z.ZodArray<z.ZodString, "many">>>;
    env: z.ZodDefault<z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodString>>>;
}, "strip", z.ZodTypeAny, {
    args: string[];
    command: string;
    env: Record<string, string>;
}, {
    command: string;
    args?: string[] | undefined;
    env?: Record<string, string> | undefined;
}>;
export declare const ConfigSchema: z.ZodObject<{
    model: z.ZodString;
    provider: z.ZodDefault<z.ZodEnum<["openai", "anthropic", "google", "openrouter"]>>;
    providers: z.ZodDefault<z.ZodOptional<z.ZodObject<{
        openai: z.ZodOptional<z.ZodObject<{
            apiKey: z.ZodOptional<z.ZodString>;
            baseUrl: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }>>;
        anthropic: z.ZodOptional<z.ZodObject<{
            apiKey: z.ZodOptional<z.ZodString>;
            baseUrl: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }>>;
        google: z.ZodOptional<z.ZodObject<{
            apiKey: z.ZodOptional<z.ZodString>;
            baseUrl: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }>>;
        openrouter: z.ZodOptional<z.ZodObject<{
            apiKey: z.ZodOptional<z.ZodString>;
            baseUrl: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }, {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        }>>;
    }, "strip", z.ZodTypeAny, {
        openai?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        anthropic?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        google?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        openrouter?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
    }, {
        openai?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        anthropic?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        google?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        openrouter?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
    }>>>;
    mcp: z.ZodDefault<z.ZodOptional<z.ZodObject<{
        servers: z.ZodDefault<z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodObject<{
            command: z.ZodString;
            args: z.ZodDefault<z.ZodOptional<z.ZodArray<z.ZodString, "many">>>;
            env: z.ZodDefault<z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodString>>>;
        }, "strip", z.ZodTypeAny, {
            args: string[];
            command: string;
            env: Record<string, string>;
        }, {
            command: string;
            args?: string[] | undefined;
            env?: Record<string, string> | undefined;
        }>>>>;
    }, "strip", z.ZodTypeAny, {
        servers: Record<string, {
            args: string[];
            command: string;
            env: Record<string, string>;
        }>;
    }, {
        servers?: Record<string, {
            command: string;
            args?: string[] | undefined;
            env?: Record<string, string> | undefined;
        }> | undefined;
    }>>>;
    dmlBase: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    workspace: z.ZodDefault<z.ZodOptional<z.ZodString>>;
}, "strip", z.ZodTypeAny, {
    model: string;
    provider: "openai" | "anthropic" | "google" | "openrouter";
    providers: {
        openai?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        anthropic?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        google?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        openrouter?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
    };
    mcp: {
        servers: Record<string, {
            args: string[];
            command: string;
            env: Record<string, string>;
        }>;
    };
    dmlBase: string;
    workspace: string;
}, {
    model: string;
    provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
    providers?: {
        openai?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        anthropic?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        google?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
        openrouter?: {
            apiKey?: string | undefined;
            baseUrl?: string | undefined;
        } | undefined;
    } | undefined;
    mcp?: {
        servers?: Record<string, {
            command: string;
            args?: string[] | undefined;
            env?: Record<string, string> | undefined;
        }> | undefined;
    } | undefined;
    dmlBase?: string | undefined;
    workspace?: string | undefined;
}>;
export type Provider = 'openai' | 'anthropic' | 'google' | 'openrouter';
export type Config = z.infer<typeof ConfigSchema>;
export type MCPServer = z.infer<typeof MCPServerSchema>;
export declare function getConfigDir(workspaceRoot: string): string;
export declare function getConfigPath(workspaceRoot: string): string;
export declare function getToolsDir(workspaceRoot: string): string;
/**
 * Initialize DeepClause configuration in a workspace
 */
export declare function initConfig(workspaceRoot: string, options?: {
    force?: boolean;
    model?: string;
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
 * Parse a model string in format "provider/model" or just "model"
 * If no provider specified, attempts to infer from model name
 */
export declare function parseModelString(modelString: string): {
    provider: Provider;
    model: string;
};
/**
 * Format provider and model as a single string
 */
export declare function formatModelString(provider: Provider, model: string): string;
/**
 * Set the default model in configuration
 * Accepts format: "provider/model" or just "model" (provider inferred)
 */
export declare function setModel(workspaceRoot: string, modelString: string): Promise<{
    provider: Provider;
    model: string;
}>;
/**
 * Get the current model from configuration
 */
export declare function showModel(workspaceRoot: string): Promise<{
    provider: Provider;
    model: string;
    formatted: string;
}>;
/**
 * Update configuration with partial changes
 */
export declare function updateConfig(workspaceRoot: string, updates: Partial<Config>): Promise<Config>;
/**
 * Check if configuration exists
 */
export declare function configExists(workspaceRoot: string): Promise<boolean>;
/**
 * Get list of configured MCP servers
 */
export declare function getMCPServers(config: Config): Record<string, MCPServer>;
export {};
//# sourceMappingURL=config.d.ts.map