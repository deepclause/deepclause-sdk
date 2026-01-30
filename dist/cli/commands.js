/**
 * DeepClause CLI - Command Listing Module
 *
 * Lists compiled DML commands and their metadata.
 */
import * as fs from 'fs/promises';
import * as path from 'path';
import { getToolsDir } from './config.js';
// =============================================================================
// Command Listing
// =============================================================================
/**
 * List all compiled DML commands
 */
export async function listCommands(workspaceRoot, options = {}) {
    const toolsDir = getToolsDir(workspaceRoot);
    let files;
    try {
        files = await fs.readdir(toolsDir);
    }
    catch (error) {
        if (error.code === 'ENOENT') {
            return [];
        }
        throw error;
    }
    // Find all .dml files
    const dmlFiles = files.filter(f => f.endsWith('.dml'));
    const commands = [];
    for (const dmlFile of dmlFiles) {
        const name = dmlFile.replace('.dml', '');
        const metaPath = path.join(toolsDir, `${name}.meta.json`);
        const dmlPath = path.join(toolsDir, dmlFile);
        let meta = null;
        try {
            const content = await fs.readFile(metaPath, 'utf-8');
            meta = JSON.parse(content);
        }
        catch {
            // No meta file, use defaults
        }
        const command = {
            name,
            path: path.relative(workspaceRoot, dmlPath).replace(/\.dml$/, ''),
            description: meta?.description || 'No description available'
        };
        if (options.detailed && meta) {
            command.parameters = meta.parameters;
            command.tools = meta.tools;
            command.compiledAt = meta.compiledAt;
            command.model = meta.model;
        }
        commands.push(command);
    }
    // Sort by name
    commands.sort((a, b) => a.name.localeCompare(b.name));
    return commands;
}
/**
 * Get information about a specific command
 */
export async function getCommand(workspaceRoot, name) {
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
    return {
        name,
        path: path.relative(workspaceRoot, dmlPath).replace(/\.dml$/, ''),
        description: meta?.description || 'No description available',
        parameters: meta?.parameters,
        tools: meta?.tools,
        compiledAt: meta?.compiledAt,
        model: meta?.model
    };
}
/**
 * Check if a command exists
 */
export async function commandExists(workspaceRoot, name) {
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
//# sourceMappingURL=commands.js.map