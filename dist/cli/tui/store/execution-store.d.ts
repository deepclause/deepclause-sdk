/**
 * Execution state management for the Ink TUI.
 * Tracks running tasks, tool calls, and activity logs.
 */
import type { ConductorLogEvent } from '../../../system/runtime/conductor.js';
import type { TokenUsageByModel } from '../../../system/runtime/token-usage.js';
export interface TaskEntry {
    id: string;
    description: string;
    state: 'started' | 'completed' | 'failed';
    depth: number;
    startedAt: number;
    completedAt?: number;
}
export interface ActiveToolStatus {
    scopeKey: string;
    scopeLabel: string;
    toolName: string;
    toolState: 'starting' | 'running' | 'completed' | 'failed';
}
export interface ExecutionState {
    /** Activity log lines */
    activityLines: string[];
    /** Task tree entries */
    tasks: TaskEntry[];
    /** Currently active tools */
    activeTools: ActiveToolStatus[];
    /** Accumulated token usage */
    tokenUsage: TokenUsageByModel;
    /** Whether execution is in progress */
    running: boolean;
}
export type ExecutionAction = {
    type: 'PUSH_ACTIVITY';
    line: string;
} | {
    type: 'CLEAR_ACTIVITY';
} | {
    type: 'SET_TASKS';
    tasks: TaskEntry[];
} | {
    type: 'ADD_TASK';
    task: TaskEntry;
} | {
    type: 'UPDATE_TASK';
    id: string;
    state: 'completed' | 'failed';
} | {
    type: 'SET_ACTIVE_TOOLS';
    tools: ActiveToolStatus[];
} | {
    type: 'SET_TOKEN_USAGE';
    usage: TokenUsageByModel;
} | {
    type: 'SET_RUNNING';
    running: boolean;
} | {
    type: 'HANDLE_LOG_EVENT';
    event: ConductorLogEvent;
};
export declare function createInitialExecutionState(): ExecutionState;
export declare function executionReducer(state: ExecutionState, action: ExecutionAction): ExecutionState;
//# sourceMappingURL=execution-store.d.ts.map