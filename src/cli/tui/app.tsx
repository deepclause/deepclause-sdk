/**
 * Root <App> component for the Ink-based TUI.
 * Replaces the monolithic FullscreenTui class with a React component tree.
 */

import React, { useReducer, useCallback, useEffect } from 'react';
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

interface AppProps {
  workspaceRoot: string;
  sandbox?: boolean;
}

export const App: React.FC<AppProps> = ({ workspaceRoot }) => {
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
  const handleSubmit = useCallback(async (text: string) => {
    appDispatch({ type: 'SET_BUSY', busy: true });
    appDispatch({ type: 'SET_MODE', mode: 'normal' });
    try {
      await sendMessage(text);
    } finally {
      appDispatch({ type: 'SET_BUSY', busy: false });
    }
  }, [sendMessage]);

  // Handle cancel/quit
  const handleCancel = useCallback(() => {
    if (appState.busy) {
      // Cancel running execution
      appDispatch({ type: 'SET_BUSY', busy: false });
    } else {
      exit();
    }
  }, [appState.busy, exit]);

  // Keyboard bindings
  useKeyBindings({
    dispatch: appDispatch,
    onSubmit: (text: string) => { void handleSubmit(text); },
    onCancel: handleCancel,
    inputActive: appState.mode === 'command',
  });

  const contentHeight = Math.max(1, appState.rows - 4); // menu + status + input + borders
  const sessionTitle = sessionState.activeDetail?.title ?? 'No session';

  return (
    <Box flexDirection="column" width={appState.columns} height={appState.rows}>
      {/* Menu bar */}
      <MenuBar sessionTitle={sessionTitle} busy={appState.busy} />

      {/* Main content area */}
      <PaneLayout
        sessionPaneCollapsed={appState.sessionPaneCollapsed}
        focusedPane={appState.focusedPane}
        height={contentHeight}
      >
        {{
          sessions: (
            <SessionPane
              sessions={sessionState.sessions}
              activeSessionId={sessionState.activeSessionId}
              collapsed={appState.sessionPaneCollapsed}
              focused={appState.focusedPane === 'sessions'}
              onSelect={(id) => { void selectSession(id); }}
            />
          ),
          messages: (
            <MessagesPane
              messages={sessionState.messages}
              streamingContent={sessionState.streamingContent}
              focused={appState.focusedPane === 'messages'}
              autoScroll={appState.autoScroll}
            />
          ),
          process: (
            <ExecutionPane
              activityLines={executionState.activityLines}
              activeTools={executionState.activeTools}
              running={executionState.running}
              focused={appState.focusedPane === 'process'}
              height={Math.floor(contentHeight / 2)}
            />
          ),
          tasks: (
            <TasksPane
              tasks={executionState.tasks}
              focused={appState.focusedPane === 'tasks'}
              height={Math.floor(contentHeight / 4)}
            />
          ),
          context: (
            <ContextPane
              tokenUsage={executionState.tokenUsage}
              focused={appState.focusedPane === 'context'}
              height={Math.floor(contentHeight / 4)}
            />
          ),
        }}
      </PaneLayout>

      {/* Command input */}
      <CommandInput
        active={appState.mode === 'command'}
        onSubmit={(text) => { void handleSubmit(text); }}
        onEscape={() => appDispatch({ type: 'SET_MODE', mode: 'normal' })}
      />

      {/* Status bar */}
      <StatusBar
        focusedPane={appState.focusedPane}
        autoScroll={appState.autoScroll}
        busy={appState.busy}
        mode={appState.mode}
      />
    </Box>
  );
};
