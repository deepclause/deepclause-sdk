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
import { useKeyBindings } from './hooks/useKeyBindings.js';

interface AppProps {
  workspaceRoot: string;
  sandbox?: boolean;
}

export const App: React.FC<AppProps> = ({ workspaceRoot, sandbox }) => {
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
    // Set initial size
    handleResize();
    stdout.on('resize', handleResize);
    return () => { stdout.off('resize', handleResize); };
  }, [stdout]);

  // Session management - wire execution dispatch for activity tracking
  const { sendMessage, selectSession, createSession } = useSession({
    workspaceRoot,
    dispatch: sessionDispatch,
    executionDispatch,
    sandbox,
  });

  // Handle command submission (text or slash commands)
  const handleSubmit = useCallback(async (text: string) => {
    const trimmed = text.trim();
    if (!trimmed) return;

    // Handle slash commands
    if (trimmed.startsWith('/')) {
      const parts = trimmed.slice(1).split(/\s+/);
      const command = parts[0];
      const rawArgs = trimmed.slice(1 + command.length).trim();

      switch (command) {
        case 'exit':
        case 'quit':
          exit();
          return;
        case 'new':
          appDispatch({ type: 'SET_BUSY', busy: true });
          try {
            await createSession(rawArgs || undefined);
          } finally {
            appDispatch({ type: 'SET_BUSY', busy: false });
          }
          return;
        case 'sessions':
          appDispatch({ type: 'TOGGLE_SESSION_PANE' });
          return;
        case 'help':
          sessionDispatch({
            type: 'APPEND_MESSAGE',
            message: {
              role: 'system',
              content: [
                'Commands:',
                '  /new [title]    Create a new session',
                '  /sessions       Toggle session list',
                '  /help           Show this help',
                '  /exit           Quit the TUI',
                '',
                'Shortcuts:',
                '  Ctrl+C          Cancel / Quit',
                '  Ctrl+B          Toggle session pane',
                '  Alt+1..5        Focus pane',
                '  Tab             Cycle panes',
                '  ?               Help',
              ].join('\n'),
            },
          });
          return;
        case 'cancel':
          // TODO: wire abort signal
          return;
        default:
          // Pass unknown slash commands as messages to conductor (skill invocations)
          break;
      }
    }

    appDispatch({ type: 'SET_BUSY', busy: true });
    try {
      await sendMessage(trimmed);
    } finally {
      appDispatch({ type: 'SET_BUSY', busy: false });
    }
  }, [sendMessage, createSession, exit, sessionDispatch, appDispatch]);

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
    onCancel: handleCancel,
    inputActive: appState.mode === 'command',
  });

  const contentHeight = Math.max(1, appState.rows - 3); // menu + input + status
  const sessionTitle = sessionState.activeDetail?.title ?? (sessionState.loading ? 'Loading…' : 'No session');

  return (
    <Box flexDirection="column" width={appState.columns} height={appState.rows}>
      {/* Menu bar */}
      <MenuBar sessionTitle={sessionTitle} busy={appState.busy} />

      {/* Main content area */}
      <PaneLayout
        sessionPaneCollapsed={appState.sessionPaneCollapsed}
        focusedPane={appState.focusedPane}
        height={contentHeight}
        columns={appState.columns}
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

      {/* Command input - always active */}
      <CommandInput
        active={appState.mode === 'command'}
        busy={appState.busy}
        onSubmit={(text) => { void handleSubmit(text); }}
        onActivate={() => appDispatch({ type: 'SET_MODE', mode: 'command' })}
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
