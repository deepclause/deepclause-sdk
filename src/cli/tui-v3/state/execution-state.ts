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
  tokenUsage: Record<string, { input: number; output: number }>;
  running: boolean;
}

export type ExecutionAction =
  | { type: 'PUSH_ACTIVITY'; line: string }
  | { type: 'CLEAR_ACTIVITY' }
  | { type: 'SET_TASKS'; tasks: TaskEntry[] }
  | { type: 'ADD_TASK'; task: TaskEntry }
  | { type: 'UPDATE_TASK'; id: string; state: 'completed' | 'failed' }
  | { type: 'SET_ACTIVE_TOOLS'; tools: ActiveToolStatus[] }
  | { type: 'ADD_ACTIVE_TOOL'; tool: ActiveToolStatus }
  | { type: 'REMOVE_ACTIVE_TOOL'; scopeKey: string }
  | { type: 'SET_TOKEN_USAGE'; usage: Record<string, { input: number; output: number }> }
  | { type: 'SET_RUNNING'; running: boolean };

const MAX_ACTIVITY_LINES = 400;

export function createInitialExecutionState(): ExecutionState {
  return {
    activityLines: [],
    tasks: [],
    activeTools: [],
    tokenUsage: {},
    running: false,
  };
}

export function executionReducer(state: ExecutionState, action: ExecutionAction): ExecutionState {
  switch (action.type) {
    case 'PUSH_ACTIVITY': {
      const lines = [...state.activityLines, action.line];
      if (lines.length > MAX_ACTIVITY_LINES) {
        lines.splice(0, lines.length - MAX_ACTIVITY_LINES);
      }
      return { ...state, activityLines: lines };
    }
    case 'CLEAR_ACTIVITY':
      return { ...state, activityLines: [], tasks: [], activeTools: [] };
    case 'SET_TASKS':
      return { ...state, tasks: action.tasks };
    case 'ADD_TASK':
      return { ...state, tasks: [...state.tasks, action.task] };
    case 'UPDATE_TASK': {
      const tasks = state.tasks.map((t) =>
        t.id === action.id ? { ...t, state: action.state, completedAt: Date.now() } : t,
      );
      return { ...state, tasks };
    }
    case 'SET_ACTIVE_TOOLS':
      return { ...state, activeTools: action.tools };
    case 'ADD_ACTIVE_TOOL': {
      const existing = state.activeTools.filter((t) => t.scopeKey !== action.tool.scopeKey);
      return { ...state, activeTools: [...existing, action.tool] };
    }
    case 'REMOVE_ACTIVE_TOOL':
      return { ...state, activeTools: state.activeTools.filter((t) => t.scopeKey !== action.scopeKey) };
    case 'SET_TOKEN_USAGE':
      return { ...state, tokenUsage: action.usage };
    case 'SET_RUNNING':
      return { ...state, running: action.running };
    default:
      return state;
  }
}
