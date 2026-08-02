/**
 * Execution state management for TUI v3.
 * Tracks running tools, tasks, and activity log.
 */
const MAX_ACTIVITY_LINES = 400;
export function createInitialExecutionState() {
    return {
        activityLines: [],
        tasks: [],
        activeTools: [],
        tokenUsage: {},
        running: false,
    };
}
export function executionReducer(state, action) {
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
            const tasks = state.tasks.map((t) => t.id === action.id ? { ...t, state: action.state, completedAt: Date.now() } : t);
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
//# sourceMappingURL=execution-state.js.map