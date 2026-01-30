/**
 * DeepClause CLI - MCP (Model Context Protocol) Client Module
 *
 * Handles connections to MCP servers for tool discovery and execution.
 *
 * TODO: Full implementation requires @modelcontextprotocol/sdk package
 * For now, this provides the interface and placeholder implementation.
 */
import type { MCPServer } from './config.js';
import type { Tool } from './tools.js';
import type { ChildProcess } from 'child_process';
export interface MCPConnection {
    server: MCPServer;
    name: string;
    process?: ChildProcess;
    tools: Tool[];
    connected: boolean;
    error?: string;
}
export interface MCPToolCallResult {
    success: boolean;
    result?: unknown;
    error?: string;
}
/**
 * Connect to an MCP server and discover its tools
 */
export declare function connectMCPServer(name: string, config: MCPServer): Promise<MCPConnection>;
/**
 * Disconnect from an MCP server
 */
export declare function disconnectMCPServer(name: string): Promise<void>;
/**
 * Disconnect from all MCP servers
 */
export declare function disconnectAllMCPServers(): Promise<void>;
/**
 * Get tools from an MCP server
 */
export declare function getMCPServerTools(name: string, config: MCPServer): Promise<Tool[]>;
/**
 * Call a tool on an MCP server
 */
export declare function callMCPTool(serverName: string, toolName: string, _args: Record<string, unknown>): Promise<MCPToolCallResult>;
/**
 * Check if an MCP server is connected
 */
export declare function isMCPServerConnected(name: string): boolean;
/**
 * Get connection status for all servers
 */
export declare function getMCPServerStatus(): Map<string, {
    connected: boolean;
    error?: string;
}>;
//# sourceMappingURL=mcp.d.ts.map