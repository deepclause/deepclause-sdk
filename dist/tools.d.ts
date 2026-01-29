/**
 * Tool management utilities
 */
import type { ToolPolicy } from './types.js';
/**
 * Check if a tool is allowed by the current policy
 */
export declare function checkToolPolicy(toolName: string, policy: ToolPolicy | null): {
    allowed: boolean;
    reason?: string;
};
/**
 * Extract tool name from a Prolog term
 * e.g., "web_search(query)" -> "web_search"
 */
export declare function extractToolName(term: string): string;
/**
 * Parse tool arguments from a Prolog term
 * e.g., "web_search(\"query\", 10)" -> ["query", 10]
 */
export declare function parseToolArgs(term: string): unknown[];
//# sourceMappingURL=tools.d.ts.map