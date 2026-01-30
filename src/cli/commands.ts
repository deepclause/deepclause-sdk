/**
 * DeepClause CLI - Command Listing Module
 * 
 * Lists compiled DML commands and their metadata.
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import { getToolsDir } from './config.js';

// =============================================================================
// Types
// =============================================================================

export interface Parameter {
  name: string;
  description?: string;
  required?: boolean;
  default?: string;
}

export interface CommandInfo {
  name: string;
  path: string;
  description: string;
  parameters?: Parameter[];
  tools?: string[];
  compiledAt?: string;
  model?: string;
}

export interface ListCommandsOptions {
  json?: boolean;
  detailed?: boolean;
}

interface MetaFile {
  version: string;
  source: string;
  sourceHash: string;
  compiledAt: string;
  model: string;
  description: string;
  parameters: Parameter[];
  tools: string[];
  history: Array<{
    version: number;
    timestamp: string;
    sourceHash: string;
    model: string;
  }>;
}

// =============================================================================
// Command Listing
// =============================================================================

/**
 * List all compiled DML commands
 */
export async function listCommands(
  workspaceRoot: string,
  options: ListCommandsOptions = {}
): Promise<CommandInfo[]> {
  const toolsDir = getToolsDir(workspaceRoot);
  
  let files: string[];
  try {
    files = await fs.readdir(toolsDir);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      return [];
    }
    throw error;
  }
  
  // Find all .dml files
  const dmlFiles = files.filter(f => f.endsWith('.dml'));
  
  const commands: CommandInfo[] = [];
  
  for (const dmlFile of dmlFiles) {
    const name = dmlFile.replace('.dml', '');
    const metaPath = path.join(toolsDir, `${name}.meta.json`);
    const dmlPath = path.join(toolsDir, dmlFile);
    
    let meta: MetaFile | null = null;
    try {
      const content = await fs.readFile(metaPath, 'utf-8');
      meta = JSON.parse(content) as MetaFile;
    } catch {
      // No meta file, use defaults
    }
    
    const command: CommandInfo = {
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
export async function getCommand(
  workspaceRoot: string,
  name: string
): Promise<CommandInfo | null> {
  const toolsDir = getToolsDir(workspaceRoot);
  const dmlPath = path.join(toolsDir, `${name}.dml`);
  const metaPath = path.join(toolsDir, `${name}.meta.json`);
  
  // Check if DML file exists
  try {
    await fs.access(dmlPath);
  } catch {
    return null;
  }
  
  let meta: MetaFile | null = null;
  try {
    const content = await fs.readFile(metaPath, 'utf-8');
    meta = JSON.parse(content) as MetaFile;
  } catch {
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
export async function commandExists(
  workspaceRoot: string,
  name: string
): Promise<boolean> {
  const toolsDir = getToolsDir(workspaceRoot);
  const dmlPath = path.join(toolsDir, `${name}.dml`);
  
  try {
    await fs.access(dmlPath);
    return true;
  } catch {
    return false;
  }
}
