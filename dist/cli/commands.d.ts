/**
 * DeepClause CLI - Command Listing Module
 *
 * Lists compiled DML commands and their metadata.
 */
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
/**
 * List all compiled DML commands
 */
export declare function listCommands(workspaceRoot: string, options?: ListCommandsOptions): Promise<CommandInfo[]>;
/**
 * Get information about a specific command
 */
export declare function getCommand(workspaceRoot: string, name: string): Promise<CommandInfo | null>;
/**
 * Check if a command exists
 */
export declare function commandExists(workspaceRoot: string, name: string): Promise<boolean>;
//# sourceMappingURL=commands.d.ts.map