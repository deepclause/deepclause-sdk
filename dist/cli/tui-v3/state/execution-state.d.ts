/**
 * Execution state management for TUI v3.
 * Tracks running tools, tasks, and activity log.
 */
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
    activityLines: string[];
    tasks: TaskEntry[];
    activeTools: ActiveToolStatus[];
    tokenUsage: Record<string, {
        input: number;
        output: number;
    }>;
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
    type: 'ADD_ACTIVE_TOOL';
    tool: ActiveToolStatus;
} | {
    type: 'REMOVE_ACTIVE_TOOL';
    scopeKey: string;
} | {
    type: 'SET_TOKEN_USAGE';
    usage: Record<string, {
        input: number;
        output: number;
    }>;
} | {
    type: 'SET_RUNNING';
    running: boolean;
};
export declare function createInitialExecutionState(): ExecutionState;
export declare function executionReducer(state: ExecutionState, action: ExecutionAction): ExecutionState;
//# sourceMappingURL=execution-state.d.ts.map