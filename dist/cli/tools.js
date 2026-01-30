/**
 * DeepClause CLI - Tool Resolution Module
 *
 * Handles MCP server connections and tool discovery.
 */
import { getMCPServers } from './config.js';
// =============================================================================
// Built-in AgentVM Tools
// =============================================================================
const AGENTVM_TOOLS = [
    {
        name: 'vm_exec',
        description: 'Execute a shell command in a sandboxed Alpine Linux VM with Python. Returns stdout, stderr, and exit code.',
        provider: 'agentvm',
        schema: {
            type: 'object',
            properties: {
                command: { type: 'string', description: 'Shell command to execute (e.g., "python3 script.py", "echo hello", "ls -la")' }
            },
            required: ['command']
        }
    }
];
// =============================================================================
// Built-in Search Tools
// =============================================================================
const SEARCH_TOOLS = [
    {
        name: 'web_search',
        description: 'Search the web using Brave Search API. Returns structured results with titles, URLs, and descriptions.',
        provider: 'brave',
        schema: {
            type: 'object',
            properties: {
                query: { type: 'string', description: 'Search query' },
                count: { type: 'number', description: 'Number of results (default: 10, max: 20)' },
                freshness: { type: 'string', description: 'Filter by freshness: pd (past day), pw (past week), pm (past month), py (past year)' }
            },
            required: ['query']
        }
    },
    {
        name: 'news_search',
        description: 'Search for recent news articles using Brave Search API.',
        provider: 'brave',
        schema: {
            type: 'object',
            properties: {
                query: { type: 'string', description: 'Search query' },
                count: { type: 'number', description: 'Number of results (default: 10, max: 20)' },
                freshness: { type: 'string', description: 'Filter by freshness: pd (past day), pw (past week), pm (past month)' }
            },
            required: ['query']
        }
    }
];
// =============================================================================
// Tool Resolution
// =============================================================================
/**
 * List all available tools from AgentVM, Search, and configured MCP servers
 */
export async function listTools(workspaceRoot, options = {}) {
    const { loadConfig } = await import('./config.js');
    const config = await loadConfig(workspaceRoot);
    const tools = [...AGENTVM_TOOLS, ...SEARCH_TOOLS];
    // Get tools from MCP servers
    const mcpServers = getMCPServers(config);
    for (const [serverName, serverConfig] of Object.entries(mcpServers)) {
        try {
            const serverTools = await getToolsFromMCPServer(serverName, serverConfig);
            tools.push(...serverTools);
        }
        catch (error) {
            // Add error entry for failed server
            tools.push({
                name: `[${serverName}]`,
                description: `Failed to connect: ${error.message}`,
                provider: serverName,
                error: error.message
            });
        }
    }
    if (options.json) {
        return JSON.stringify(tools, null, 2);
    }
    return formatToolsList(tools);
}
/**
 * Resolve specific tools by name, verifying they are available
 */
export async function resolveTools(config, toolNames) {
    const resolved = {};
    const missing = [];
    // All built-in tools (AgentVM + Search)
    const builtInTools = [...AGENTVM_TOOLS, ...SEARCH_TOOLS];
    // Check built-in tools first
    for (const name of toolNames) {
        const builtInTool = builtInTools.find(t => t.name === name);
        if (builtInTool) {
            resolved[name] = builtInTool;
        }
    }
    // Check MCP servers for remaining tools
    const remainingTools = toolNames.filter(name => !resolved[name]);
    if (remainingTools.length > 0) {
        const mcpServers = getMCPServers(config);
        for (const [serverName, serverConfig] of Object.entries(mcpServers)) {
            try {
                const serverTools = await getToolsFromMCPServer(serverName, serverConfig);
                for (const tool of serverTools) {
                    if (remainingTools.includes(tool.name) && !resolved[tool.name]) {
                        resolved[tool.name] = tool;
                    }
                }
            }
            catch {
                // Server failed, continue checking others
            }
        }
    }
    // Check for missing tools
    for (const name of toolNames) {
        if (!resolved[name]) {
            missing.push(name);
        }
    }
    if (missing.length > 0) {
        throw new Error(`Missing tools: ${missing.join(', ')}. Configure MCP servers or check tool names.`);
    }
    return resolved;
}
/**
 * Check if all required tools are available
 */
export async function verifyTools(config, toolNames) {
    try {
        await resolveTools(config, toolNames);
        return { available: true, missing: [] };
    }
    catch (error) {
        const match = error.message.match(/Missing tools: (.+)\./);
        const missing = match ? match[1].split(', ') : toolNames;
        return { available: false, missing };
    }
}
// =============================================================================
// MCP Server Integration
// =============================================================================
/**
 * Get tools from an MCP server
 */
async function getToolsFromMCPServer(serverName, serverConfig) {
    // Use the MCP module for server connection
    const { getMCPServerTools } = await import('./mcp.js');
    return getMCPServerTools(serverName, serverConfig);
}
// =============================================================================
// Formatting
// =============================================================================
function formatToolsList(tools) {
    const byProvider = new Map();
    for (const tool of tools) {
        const existing = byProvider.get(tool.provider) || [];
        existing.push(tool);
        byProvider.set(tool.provider, existing);
    }
    const lines = [];
    for (const [provider, providerTools] of byProvider) {
        let icon;
        let type;
        switch (provider) {
            case 'agentvm':
                icon = '🖥️';
                type = 'built-in';
                break;
            case 'brave':
                icon = '🔍';
                type = 'built-in';
                break;
            default:
                icon = '📦';
                type = 'MCP';
        }
        lines.push(`${icon} ${provider} (${type})`);
        for (const tool of providerTools) {
            if (tool.error) {
                lines.push(`  ⚠️  ${tool.name} - ${tool.description}`);
            }
            else {
                lines.push(`  ├─ ${tool.name} - ${tool.description}`);
            }
        }
        lines.push('');
    }
    return lines.join('\n');
}
/**
 * Get all built-in tools (AgentVM + Search)
 */
export function getAgentVMTools() {
    return [...AGENTVM_TOOLS, ...SEARCH_TOOLS];
}
//# sourceMappingURL=tools.js.map