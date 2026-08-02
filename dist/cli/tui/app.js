import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Root <App> component for the Ink-based TUI.
 * Replaces the monolithic FullscreenTui class with a React component tree.
 */
import { useReducer, useCallback, useEffect } from 'react';
import { Box, useApp, useStdout } from 'ink';
import { MenuBar } from './components/layout/MenuBar.js';
import { StatusBar } from './components/layout/StatusBar.js';
import { CommandInput } from './components/layout/CommandInput.js';
import { PaneLayout } from './components/layout/PaneLayout.js';
import { SessionPane } from './components/panes/SessionPane.js';
import { MessagesPane } from './components/panes/MessagesPane.js';
import { ExecutionPane } from './components/panes/ExecutionPane.js';
import { TasksPane } from './components/panes/TasksPane.js';
import { ContextPane } from './components/panes/ContextPane.js';
import { appReducer, createInitialAppState } from './store/app-state.js';
import { sessionReducer, createInitialSessionState } from './store/session-store.js';
import { executionReducer, createInitialExecutionState } from './store/execution-store.js';
import { useSession } from './hooks/useSession.js';
import { useExecution } from './hooks/useExecution.js';
import { useKeyBindings } from './hooks/useKeyBindings.js';
export const App = ({ workspaceRoot }) => {
    const { exit } = useApp();
    const { stdout } = useStdout();
    const [appState, appDispatch] = useReducer(appReducer, undefined, createInitialAppState);
    const [sessionState, sessionDispatch] = useReducer(sessionReducer, undefined, createInitialSessionState);
    const [executionState, executionDispatch] = useReducer(executionReducer, undefined, createInitialExecutionState);
    // Track terminal resize
    useEffect(() => {
        const handleResize = () => {
            appDispatch({
                type: 'RESIZE',
                columns: stdout.columns ?? 80,
                rows: stdout.rows ?? 24,
            });
        };
        stdout.on('resize', handleResize);
        return () => { stdout.off('resize', handleResize); };
    }, [stdout]);
    // Session management
    const { sendMessage, selectSession } = useSession({
        workspaceRoot,
        dispatch: sessionDispatch,
    });
    // Execution monitoring
    useExecution({ dispatch: executionDispatch });
    // Handle command submission
    const handleSubmit = useCallback(async (text) => {
        appDispatch({ type: 'SET_BUSY', busy: true });
        appDispatch({ type: 'SET_MODE', mode: 'normal' });
        try {
            await sendMessage(text);
        }
        finally {
            appDispatch({ type: 'SET_BUSY', busy: false });
        }
    }, [sendMessage]);
    // Handle cancel/quit
    const handleCancel = useCallback(() => {
        if (appState.busy) {
            // Cancel running execution
            appDispatch({ type: 'SET_BUSY', busy: false });
        }
        else {
            exit();
        }
    }, [appState.busy, exit]);
    // Keyboard bindings
    useKeyBindings({
        dispatch: appDispatch,
        onSubmit: (text) => { void handleSubmit(text); },
        onCancel: handleCancel,
        inputActive: appState.mode === 'command',
    });
    const contentHeight = Math.max(1, appState.rows - 4); // menu + status + input + borders
    const sessionTitle = sessionState.activeDetail?.title ?? 'No session';
    return (_jsxs(Box, { flexDirection: "column", width: appState.columns, height: appState.rows, children: [_jsx(MenuBar, { sessionTitle: sessionTitle, busy: appState.busy }), _jsx(PaneLayout, { sessionPaneCollapsed: appState.sessionPaneCollapsed, focusedPane: appState.focusedPane, height: contentHeight, children: {
                    sessions: (_jsx(SessionPane, { sessions: sessionState.sessions, activeSessionId: sessionState.activeSessionId, collapsed: appState.sessionPaneCollapsed, focused: appState.focusedPane === 'sessions', onSelect: (id) => { void selectSession(id); } })),
                    messages: (_jsx(MessagesPane, { messages: sessionState.messages, streamingContent: sessionState.streamingContent, focused: appState.focusedPane === 'messages', autoScroll: appState.autoScroll })),
                    process: (_jsx(ExecutionPane, { activityLines: executionState.activityLines, activeTools: executionState.activeTools, running: executionState.running, focused: appState.focusedPane === 'process', height: Math.floor(contentHeight / 2) })),
                    tasks: (_jsx(TasksPane, { tasks: executionState.tasks, focused: appState.focusedPane === 'tasks', height: Math.floor(contentHeight / 4) })),
                    context: (_jsx(ContextPane, { tokenUsage: executionState.tokenUsage, focused: appState.focusedPane === 'context', height: Math.floor(contentHeight / 4) })),
                } }), _jsx(CommandInput, { active: appState.mode === 'command', onSubmit: (text) => { void handleSubmit(text); }, onEscape: () => appDispatch({ type: 'SET_MODE', mode: 'normal' }) }), _jsx(StatusBar, { focusedPane: appState.focusedPane, autoScroll: appState.autoScroll, busy: appState.busy, mode: appState.mode })] }));
};
//# sourceMappingURL=app.js.map