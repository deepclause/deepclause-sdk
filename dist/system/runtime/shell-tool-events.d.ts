import type { DMLEvent } from '../../types.js';
import type { ShellExecObserver } from './agentvm-manager.js';
export declare function buildToolStartEvent(toolName: string, toolArgs: Record<string, unknown>): DMLEvent;
export declare function buildToolCompletionEvent(toolName: string, toolArgs: Record<string, unknown>, toolResult: unknown): DMLEvent;
export declare function buildToolFailureEvent(toolName: string, toolArgs: Record<string, unknown>, error: unknown): DMLEvent;
export declare function createShellToolEventBridge(options: {
    toolName: string;
    toolArgs: Record<string, unknown>;
    emit?: (event: DMLEvent) => void;
}): ShellExecObserver | undefined;
//# sourceMappingURL=shell-tool-events.d.ts.map