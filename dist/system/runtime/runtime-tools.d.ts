import type { Config } from '../../cli/config.js';
import type { DMLEvent, DeepClauseSDK } from '../../types.js';
import type { ShellManager } from './shell-manager.js';
export declare function getBuiltInRuntimeToolNames(): string[];
export declare function verifyRuntimeToolsAvailable(config: Config, toolNames: string[]): {
    available: boolean;
    missing: string[];
};
export declare function registerLocalRuntimeTools(sdk: DeepClauseSDK, options: {
    workspaceRoot?: string;
    workspacePath: string;
    shell: ShellManager;
    signal?: AbortSignal;
    toolAbortSignalRef?: {
        signal?: AbortSignal;
    };
    onEvent?: (event: DMLEvent) => void;
    skillCatalog?: {
        listSkills: () => Promise<unknown>;
        runSkill: (args: Record<string, unknown>) => Promise<unknown>;
    };
}): void;
export declare function urlFetch(workspacePath: string, args: Record<string, unknown>, signal?: AbortSignal): Promise<Record<string, unknown>>;
export declare function truncateUrlFetchTextBody(body: string, maxChars?: number): {
    body: string;
    truncated: boolean;
    originalLength: number;
    returnedLength: number;
};
export declare function resolveWorkspacePath(workspacePath: string, filePath: string): string;
//# sourceMappingURL=runtime-tools.d.ts.map