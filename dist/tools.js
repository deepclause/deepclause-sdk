/**
 * Tool management utilities
 */
/**
 * Check if a tool is allowed by the current policy
 */
export function checkToolPolicy(toolName, policy) {
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
function matchToolPattern(pattern, toolName) {
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
export function extractToolName(term) {
    const match = term.match(/^([a-z_][a-z0-9_]*)/i);
    return match ? match[1] : term;
}
/**
 * Parse tool arguments from a Prolog term
 * e.g., "web_search(\"query\", 10)" -> ["query", 10]
 */
export function parseToolArgs(term) {
    // Find the content between parentheses
    const match = term.match(/\((.+)\)$/s);
    if (!match) {
        return [];
    }
    const argsStr = match[1];
    const args = [];
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
            }
            else if (char === '(' || char === '[' || char === '{') {
                depth++;
                current += char;
            }
            else if (char === ')' || char === ']' || char === '}') {
                depth--;
                current += char;
            }
            else if (char === ',' && depth === 0) {
                args.push(parseValue(current.trim()));
                current = '';
            }
            else {
                current += char;
            }
        }
        else {
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
function parseValue(str) {
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
    if (str === 'true')
        return true;
    if (str === 'false')
        return false;
    // List
    if (str.startsWith('[') && str.endsWith(']')) {
        // Recursively parse list items
        const inner = str.slice(1, -1);
        if (!inner.trim())
            return [];
        // For simplicity, just return the string for now
        return str;
    }
    // Atom or variable
    return str;
}
//# sourceMappingURL=tools.js.map