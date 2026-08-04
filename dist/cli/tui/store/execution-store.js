/**
 * Execution state management for the Ink TUI.
 * Tracks running tasks, tool calls, and activity logs.
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
        case 'SET_TOKEN_USAGE':
            return { ...state, tokenUsage: action.usage };
        case 'SET_RUNNING':
            return { ...state, running: action.running };
        case 'HANDLE_LOG_EVENT':
            return handleLogEvent(state, action.event);
        default:
            return state;
    }
}
function handleLogEvent(state, logEvent) {
    const { event } = logEvent;
    if (event.type === 'task_activity') {
        return handleTaskActivity(state, logEvent);
    }
    if (event.type === 'tool_call' && event.toolName) {
        return handleToolEvent(state, logEvent);
    }
    if (event.type === 'stream' || event.type === 'usage' || event.type === 'finished') {
        return state;
    }
    // General activity line
    const content = event.content ?? '';
    if (content) {
        const prefix = logEvent.scope === 'child' ? `[${logEvent.childSlug ?? '?'}] ` : '';
        const line = `${prefix}${event.type}: ${content}`;
        const lines = [...state.activityLines, line];
        if (lines.length > MAX_ACTIVITY_LINES) {
            lines.splice(0, lines.length - MAX_ACTIVITY_LINES);
        }
        return { ...state, activityLines: lines };
    }
    return state;
}
function handleTaskActivity(state, logEvent) {
    const { event } = logEvent;
    if (!event.taskId)
        return state;
    if (event.taskState === 'started') {
        const depth = state.tasks.filter((t) => t.state === 'started').length;
        const task = {
            id: event.taskId,
            description: summarizeDescription(event.taskDescription ?? ''),
            state: 'started',
            depth,
            startedAt: Date.now(),
        };
        return { ...state, tasks: [...state.tasks, task] };
    }
    if (event.taskState === 'completed' || event.taskState === 'failed') {
        const tasks = state.tasks.map((t) => t.id === event.taskId ? { ...t, state: event.taskState, completedAt: Date.now() } : t);
        return { ...state, tasks };
    }
    return state;
}
function handleToolEvent(state, logEvent) {
    const { event } = logEvent;
    if (!event.toolName || !event.toolState)
        return state;
    const scopeKey = logEvent.scope === 'child'
        ? `child:${logEvent.childSlug ?? '?'}:${event.toolName}`
        : `main:${event.toolName}`;
    const scopeLabel = logEvent.scope === 'child' ? (logEvent.childSlug ?? '?') : 'main';
    if (event.toolState === 'starting' || event.toolState === 'running') {
        const existing = state.activeTools.filter((t) => t.scopeKey !== scopeKey);
        const tool = {
            scopeKey,
            scopeLabel,
            toolName: event.toolName,
            toolState: event.toolState,
        };
        return { ...state, activeTools: [...existing, tool] };
    }
    // completed or failed - remove from active
    const activeTools = state.activeTools.filter((t) => t.scopeKey !== scopeKey);
    const line = `${scopeLabel}:${event.toolName} ${event.toolState}`;
    const lines = [...state.activityLines, line];
    if (lines.length > MAX_ACTIVITY_LINES) {
        lines.splice(0, lines.length - MAX_ACTIVITY_LINES);
    }
    return { ...state, activeTools, activityLines: lines };
}
function summarizeDescription(raw) {
    const firstLine = raw.split(/\r?\n/)[0] ?? '';
    const trimmed = firstLine.replace(/\s+/g, ' ').trim();
    return trimmed.length > 80 ? trimmed.slice(0, 77) + '…' : trimmed;
}
//# sourceMappingURL=execution-store.js.map