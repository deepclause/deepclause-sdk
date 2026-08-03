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
    models: z.ZodDefault<z.ZodObject<{
        gateway: z.ZodDefault<z.ZodString>;
        run: z.ZodDefault<z.ZodString>;
        compile: z.ZodDefault<z.ZodString>;
    }, "strip", z.ZodTypeAny, {
        run: string;
        gateway: string;
        compile: string;
    }, {
        run?: string | undefined;
        gateway?: string | undefined;
        compile?: string | undefined;
    }>>;
    temperatures: z.ZodDefault<z.ZodObject<{
        gateway: z.ZodDefault<z.ZodNumber>;
        run: z.ZodDefault<z.ZodNumber>;
        compile: z.ZodDefault<z.ZodNumber>;
    }, "strip", z.ZodTypeAny, {
        run: number;
        gateway: number;
        compile: number;
    }, {
        run?: number | undefined;
        gateway?: number | undefined;
        compile?: number | undefined;
    }>>;
    modelOptions: z.ZodDefault<z.ZodOptional<z.ZodObject<{
        gateway: z.ZodOptional<z.ZodObject<{
            maxContextTokens: z.ZodOptional<z.ZodNumber>;
            maxOutputTokens: z.ZodOptional<z.ZodNumber>;
            reasoningEffort: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        }, {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        }>>;
        run: z.ZodOptional<z.ZodObject<{
            maxContextTokens: z.ZodOptional<z.ZodNumber>;
            maxOutputTokens: z.ZodOptional<z.ZodNumber>;
            reasoningEffort: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        }, {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        }>>;
        compile: z.ZodOptional<z.ZodObject<{
            maxContextTokens: z.ZodOptional<z.ZodNumber>;
            maxOutputTokens: z.ZodOptional<z.ZodNumber>;
            reasoningEffort: z.ZodOptional<z.ZodString>;
        }, "strip", z.ZodTypeAny, {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        }, {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        }>>;
    }, "strip", z.ZodTypeAny, {
        run?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        gateway?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        compile?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
    }, {
        run?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        gateway?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        compile?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
    }>>>;
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
    agentvm: z.ZodDefault<z.ZodOptional<z.ZodObject<{
        /** Enable networking in the VM (default: false for security) */
        network: z.ZodDefault<z.ZodOptional<z.ZodBoolean>>;
    }, "strip", z.ZodTypeAny, {
        network: boolean;
    }, {
        network?: boolean | undefined;
    }>>>;
    shell: z.ZodDefault<z.ZodOptional<z.ZodObject<{
        wrapper: z.ZodDefault<z.ZodOptional<z.ZodEnum<["auto", "clean-room", "bwrap", "sandbox-exec"]>>>;
        strictIsolation: z.ZodDefault<z.ZodOptional<z.ZodBoolean>>;
    }, "strip", z.ZodTypeAny, {
        strictIsolation: boolean;
        wrapper: "auto" | "clean-room" | "bwrap" | "sandbox-exec";
    }, {
        strictIsolation?: boolean | undefined;
        wrapper?: "auto" | "clean-room" | "bwrap" | "sandbox-exec" | undefined;
    }>>>;
    compaction: z.ZodOptional<z.ZodObject<{
        enabled: z.ZodOptional<z.ZodBoolean>;
        bindings: z.ZodOptional<z.ZodArray<z.ZodObject<{
            name: z.ZodOptional<z.ZodString>;
            scope: z.ZodEnum<["session", "loop", "run"]>;
            trigger: z.ZodEnum<["before_user_message", "before_model_call", "before_task", "after_task"]>;
            compactor: z.ZodObject<{
                source: z.ZodString;
                sourceType: z.ZodOptional<z.ZodEnum<["inline", "file", "auto"]>>;
                timeoutMs: z.ZodOptional<z.ZodNumber>;
                gasLimit: z.ZodOptional<z.ZodNumber>;
                model: z.ZodOptional<z.ZodString>;
                provider: z.ZodOptional<z.ZodEnum<["openai", "anthropic", "google", "openrouter"]>>;
                inheritTools: z.ZodOptional<z.ZodBoolean>;
                toolPolicy: z.ZodOptional<z.ZodNullable<z.ZodObject<{
                    mode: z.ZodEnum<["whitelist", "blacklist"]>;
                    tools: z.ZodDefault<z.ZodArray<z.ZodString, "many">>;
                }, "strip", z.ZodTypeAny, {
                    tools: string[];
                    mode: "whitelist" | "blacklist";
                }, {
                    mode: "whitelist" | "blacklist";
                    tools?: string[] | undefined;
                }>>>;
            }, "strip", z.ZodTypeAny, {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    tools: string[];
                    mode: "whitelist" | "blacklist";
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            }, {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    mode: "whitelist" | "blacklist";
                    tools?: string[] | undefined;
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            }>;
        }, "strip", z.ZodTypeAny, {
            scope: "session" | "loop" | "run";
            trigger: "before_user_message" | "before_model_call" | "before_task" | "after_task";
            compactor: {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    tools: string[];
                    mode: "whitelist" | "blacklist";
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            };
            name?: string | undefined;
        }, {
            scope: "session" | "loop" | "run";
            trigger: "before_user_message" | "before_model_call" | "before_task" | "after_task";
            compactor: {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    mode: "whitelist" | "blacklist";
                    tools?: string[] | undefined;
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            };
            name?: string | undefined;
        }>, "many">>;
    }, "strip", z.ZodTypeAny, {
        enabled?: boolean | undefined;
        bindings?: {
            scope: "session" | "loop" | "run";
            trigger: "before_user_message" | "before_model_call" | "before_task" | "after_task";
            compactor: {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    tools: string[];
                    mode: "whitelist" | "blacklist";
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            };
            name?: string | undefined;
        }[] | undefined;
    }, {
        enabled?: boolean | undefined;
        bindings?: {
            scope: "session" | "loop" | "run";
            trigger: "before_user_message" | "before_model_call" | "before_task" | "after_task";
            compactor: {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    mode: "whitelist" | "blacklist";
                    tools?: string[] | undefined;
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            };
            name?: string | undefined;
        }[] | undefined;
    }>>;
    dmlBase: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    workspace: z.ZodDefault<z.ZodOptional<z.ZodString>>;
    model: z.ZodOptional<z.ZodString>;
    provider: z.ZodOptional<z.ZodEnum<["openai", "anthropic", "google", "openrouter"]>>;
}, "strip", z.ZodTypeAny, {
    modelOptions: {
        run?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        gateway?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        compile?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
    };
    agentvm: {
        network: boolean;
    };
    workspace: string;
    models: {
        run: string;
        gateway: string;
        compile: string;
    };
    temperatures: {
        run: number;
        gateway: number;
        compile: number;
    };
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
    shell: {
        strictIsolation: boolean;
        wrapper: "auto" | "clean-room" | "bwrap" | "sandbox-exec";
    };
    dmlBase: string;
    model?: string | undefined;
    provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
    compaction?: {
        enabled?: boolean | undefined;
        bindings?: {
            scope: "session" | "loop" | "run";
            trigger: "before_user_message" | "before_model_call" | "before_task" | "after_task";
            compactor: {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    tools: string[];
                    mode: "whitelist" | "blacklist";
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            };
            name?: string | undefined;
        }[] | undefined;
    } | undefined;
}, {
    model?: string | undefined;
    modelOptions?: {
        run?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        gateway?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
        compile?: {
            reasoningEffort?: string | undefined;
            maxContextTokens?: number | undefined;
            maxOutputTokens?: number | undefined;
        } | undefined;
    } | undefined;
    provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
    agentvm?: {
        network?: boolean | undefined;
    } | undefined;
    compaction?: {
        enabled?: boolean | undefined;
        bindings?: {
            scope: "session" | "loop" | "run";
            trigger: "before_user_message" | "before_model_call" | "before_task" | "after_task";
            compactor: {
                source: string;
                model?: string | undefined;
                provider?: "openai" | "anthropic" | "google" | "openrouter" | undefined;
                gasLimit?: number | undefined;
                toolPolicy?: {
                    mode: "whitelist" | "blacklist";
                    tools?: string[] | undefined;
                } | null | undefined;
                sourceType?: "inline" | "file" | "auto" | undefined;
                timeoutMs?: number | undefined;
                inheritTools?: boolean | undefined;
            };
            name?: string | undefined;
        }[] | undefined;
    } | undefined;
    workspace?: string | undefined;
    models?: {
        run?: string | undefined;
        gateway?: string | undefined;
        compile?: string | undefined;
    } | undefined;
    temperatures?: {
        run?: number | undefined;
        gateway?: number | undefined;
        compile?: number | undefined;
    } | undefined;
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
    shell?: {
        strictIsolation?: boolean | undefined;
        wrapper?: "auto" | "clean-room" | "bwrap" | "sandbox-exec" | undefined;
    } | undefined;
    dmlBase?: string | undefined;
}>;
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