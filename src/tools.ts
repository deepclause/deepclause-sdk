/**
 * Tool management utilities
 */

import type { ToolPolicy, CompileTool } from './types.js';

/**
 * Built-in tools for compilation descriptions
 */
export const BUILTIN_COMPILE_TOOLS: CompileTool[] = [
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
  },
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

/**
 * Get all built-in tools for compilation
 */
export function getBuiltInCompileTools(): CompileTool[] {
  return [...BUILTIN_COMPILE_TOOLS];
}

/**
 * Check if a tool is allowed by the current policy
 */
export function checkToolPolicy(
  toolName: string,
  policy: ToolPolicy | null
): { allowed: boolean; reason?: string } {
  // No policy means all tools are allowed
  if (!policy) {
    return { allowed: true };
  }

  const matches = policy.tools.some(pattern => matchToolPattern(pattern, toolName));

  if (policy.mode === 'whitelist') {
    if (matches) {
      return { allowed: true };
    }
    return { 
      allowed: false, 
      reason: `Tool '${toolName}' is not in the whitelist` 
    };
  }

  // Blacklist mode
  if (matches) {
    return { 
      allowed: false, 
      reason: `Tool '${toolName}' is blocked by blacklist` 
    };
  }
  return { allowed: true };
}

/**
 * Match a tool name against a pattern (supports wildcards)
 */
function matchToolPattern(pattern: string, toolName: string): boolean {
  // Convert glob pattern to regex
  const regexPattern = pattern
    .replace(/[.+^${}()|[\]\\]/g, '\\$&') // Escape special chars except *
    .replace(/\*/g, '.*'); // Convert * to .*

  const regex = new RegExp(`^${regexPattern}$`);
  return regex.test(toolName);
}

/**
 * Extract tool name from a Prolog term
 * e.g., "web_search(query)" -> "web_search"
 */
export function extractToolName(term: string): string {
  const match = term.match(/^([a-z_][a-z0-9_]*)/i);
  return match ? match[1] : term;
}

/**
 * Parse tool arguments from a Prolog term
 * e.g., "web_search(\"query\", 10)" -> ["query", 10]
 */
export function parseToolArgs(term: string): unknown[] {
  // Find the content between parentheses
  const match = term.match(/\((.+)\)$/s);
  if (!match) {
    return [];
  }

  const argsStr = match[1];
  const args: unknown[] = [];
  
  // Simple parsing (doesn't handle all edge cases)
  let current = '';
  let depth = 0;
  let inString = false;
  let stringChar = '';

  for (let i = 0; i < argsStr.length; i++) {
    const char = argsStr[i];
    const prevChar = i > 0 ? argsStr[i - 1] : '';

    if (!inString) {
      if (char === '"' || char === "'") {
        inString = true;
        stringChar = char;
        current += char;
      } else if (char === '(' || char === '[' || char === '{') {
        depth++;
        current += char;
      } else if (char === ')' || char === ']' || char === '}') {
        depth--;
        current += char;
      } else if (char === ',' && depth === 0) {
        args.push(parseValue(current.trim()));
        current = '';
      } else {
        current += char;
      }
    } else {
      current += char;
      if (char === stringChar && prevChar !== '\\') {
        inString = false;
      }
    }
  }

  if (current.trim()) {
    args.push(parseValue(current.trim()));
  }

  return args;
}

/**
 * Parse a single value from Prolog term string
 */
function parseValue(str: string): unknown {
  // String literal
  if ((str.startsWith('"') && str.endsWith('"')) ||
      (str.startsWith("'") && str.endsWith("'"))) {
    return str.slice(1, -1)
      .replace(/\\"/g, '"')
      .replace(/\\'/g, "'")
      .replace(/\\\\/g, '\\');
  }

  // Number
  const num = Number(str);
  if (!isNaN(num)) {
    return num;
  }

  // Boolean
  if (str === 'true') return true;
  if (str === 'false') return false;

  // List
  if (str.startsWith('[') && str.endsWith(']')) {
    // Recursively parse list items
    const inner = str.slice(1, -1);
    if (!inner.trim()) return [];
    // For simplicity, just return the string for now
    return str;
  }

  // Atom or variable
  return str;
}
