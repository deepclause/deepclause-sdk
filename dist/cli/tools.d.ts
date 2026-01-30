/**
 * DeepClause CLI - Tool Resolution Module
 *
 * Handles MCP server connections and tool discovery.
 */
import { Config } from './config.js';
export interface Tool {
    name: string;
    description: string;
    provider: string;
    schema?: object;
    error?: string;
}
export interface ListToolsOptions {
    json?: boolean;
}
/**
 * List all available tools from AgentVM, Search, and configured MCP servers
 */
export declare function listTools(workspaceRoot: string, options?: ListToolsOptions): Promise<Tool[] | string>;
/**
 * Resolve specific tools by name, verifying they are available
 */
export declare function resolveTools(config: Config, toolNames: string[]): Promise<Record<string, Tool>>;
/**
 * Check if all required tools are available
 */
export declare function verifyTools(config: Config, toolNames: string[]): Promise<{
    available: boolean;
    missing: string[];
}>;
/**
 * Get all built-in tools (AgentVM + Search)
 */
export declare function getAgentVMTools(): Tool[];
//# sourceMappingURL=tools.d.ts.map