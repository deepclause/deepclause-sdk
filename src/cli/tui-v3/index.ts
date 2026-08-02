/**
 * TUI v3 Entry Point — startTuiV3()
 *
 * Wires together the event loop, renderer, state management, and components
 * into a fully functioning terminal UI with zero-flicker differential rendering.
 */

import { EventLoop } from './event-loop.js';
import { VStack } from './layout/vstack.js';
import { ScrollView } from './layout/scroll-view.js';
import { Header } from './components/header.js';
import { Input } from './components/input.js';
import { StatusBar } from './components/status-bar.js';
import { Messages } from './components/messages.js';
import { Activity } from './components/activity.js';
import { HelpDialog } from './dialogs/help.js';
import { composeOverlay } from './layout/overlay.js';
import {
  createInitialAppState,
  appReducer,
  type AppAction,
} from './state/app-state.js';
import {
  createInitialSessionState,
  sessionReducer,
  type SessionAction,
} from './state/session-state.js';
import {
  createInitialExecutionState,
  executionReducer,
  type ExecutionAction,
} from './state/execution-state.js';
import type { Component, KeyEvent } from './types.js';
import {
  createConductorSession,
  getConductorSessionDetail,
  listConductorSessions,
  runConductorTurn,
  type ConductorLogEvent,
  type ConductorSessionMessage,
} from '../../system/runtime/conductor.js';

export interface TuiV3Options {
  sandbox?: boolean;
}

/**
 * Start the TUI v3 (differential renderer).
 * Returns when the user exits.
 */
export async function startTuiV3(
  workspaceRoot = process.cwd(),
  options: TuiV3Options = {},
): Promise<void> {
  // --- State ---
  let appState = createInitialAppState();
  let sessionState = createInitialSessionState();
  let executionState = createInitialExecutionState();

  // --- Dispatch functions ---
  const dispatchApp = (action: AppAction): void => {
    appState = appReducer(appState, action);
    syncStateToComponents();
  };

  const dispatchSession = (action: SessionAction): void => {
    sessionState = sessionReducer(sessionState, action);
    syncStateToComponents();
  };

  const dispatchExecution = (action: ExecutionAction): void => {
    executionState = executionReducer(executionState, action);
    syncStateToComponents();
  };

  // --- Components ---
  // Use a temporary no-op render request until the event loop is started
  let requestRender: () => void = () => {};

  const header = new Header(requestRender);
  const messagesComp = new Messages(requestRender);
  const activityComp = new Activity(requestRender);
  const input = new Input(requestRender);
  const statusBar = new StatusBar(requestRender);
  const helpDialog = new HelpDialog(requestRender);

  const messageScroll = new ScrollView(messagesComp, requestRender);
  const activityScroll = new ScrollView(activityComp, requestRender);

  // --- Layout ---
  const mainLayout = new VStack(requestRender);

  // --- Root component that orchestrates everything ---
  const rootComponent: Component = {
    dirty: true,
    minHeight: 0,
    flexGrow: 1,

    invalidate() {
      this.dirty = true;
      requestRender();
    },

    render(width: number): string[] {
      const height = appState.rows;

      // Layout: header(1) + content(flex) + input(1) + status(1)
      const contentHeight = Math.max(1, height - 3);

      // Distribute content between messages and activity
      const activityHeight = executionState.running ? Math.min(6, Math.floor(contentHeight / 4)) : 0;
      const messagesHeight = contentHeight - activityHeight;

      const rows: string[] = [];

      // Header
      rows.push(...header.render(width));

      // Messages (scrollable)
      rows.push(...messageScroll.renderWithHeight(width, messagesHeight));

      // Activity (if running)
      if (activityHeight > 0) {
        rows.push(...activityScroll.renderWithHeight(width, activityHeight));
      }

      // Input
      rows.push(...input.render(width));

      // Status bar
      rows.push(...statusBar.render(width));

      // Overlay composition (help dialog)
      if (helpDialog.isVisible) {
        const overlayRows = helpDialog.render(width);
        const overlayWidth = Math.min(60, width - 4);
        return composeOverlay(rows, overlayRows, overlayWidth, width, height);
      }

      this.dirty = false;
      return rows;
    },

    handleInput(key: KeyEvent): boolean {
      // Dialogs consume input first
      if (helpDialog.isVisible) {
        return helpDialog.handleInput(key) ?? false;
      }

      // Help toggle
      if (key.sequence === '?' && !input.active) {
        helpDialog.show();
        return true;
      }

      // Tab: cycle panes
      if (key.name === 'tab' && !key.ctrl && !key.meta) {
        dispatchApp({ type: 'CYCLE_PANE' });
        return true;
      }

      // Scroll (Shift+Up/Down or PgUp/PgDn) when messages pane focused
      if (appState.focusedPane === 'messages') {
        if (messageScroll.handleInput(key)) return true;
      }
      if (appState.focusedPane === 'process') {
        if (activityScroll.handleInput(key)) return true;
      }

      // Input component always gets remaining keys
      if (input.handleInput(key)) return true;

      return false;
    },
  };

  // --- Sync state to components ---
  function syncStateToComponents(): void {
    header.setTitle(sessionState.activeTitle);
    header.setBusy(appState.busy);

    messagesComp.setMessages(sessionState.messages.map((m) => ({
      role: m.role,
      content: m.content,
      pending: m.pending,
      error: m.error,
    })));
    messagesComp.setStreaming(sessionState.streamingContent);

    activityComp.setRunning(executionState.running);
    activityComp.setActiveTools(executionState.activeTools.map((t) => ({
      name: t.toolName,
      scope: t.scopeLabel,
      state: t.toolState,
    })));

    statusBar.setMode(appState.mode);
    statusBar.setFocusedPane(appState.focusedPane);
    statusBar.setBusy(appState.busy);
    statusBar.setFollowMode(messageScroll.isFollowing);

    requestRender();
  }

  // --- Command handling ---
  async function handleSubmit(text: string): Promise<void> {
    const trimmed = text.trim();
    if (!trimmed) return;

    // Slash commands
    if (trimmed.startsWith('/')) {
      const parts = trimmed.slice(1).split(/\s+/);
      const command = parts[0];
      const rawArgs = trimmed.slice(1 + command.length).trim();

      switch (command) {
        case 'exit':
        case 'quit':
          eventLoop.stop();
          return;
        case 'new':
          dispatchApp({ type: 'SET_BUSY', busy: true });
          try {
            await createNewSession(rawArgs || undefined);
          } finally {
            dispatchApp({ type: 'SET_BUSY', busy: false });
          }
          return;
        case 'sessions':
          dispatchApp({ type: 'TOGGLE_SESSION_PANE' });
          return;
        case 'help':
          helpDialog.show();
          return;
        case 'cancel':
          if (abortController) {
            abortController.abort();
          }
          return;
        default:
          break;
      }
    }

    // Send message
    dispatchApp({ type: 'SET_BUSY', busy: true });
    try {
      await sendMessage(trimmed);
    } finally {
      dispatchApp({ type: 'SET_BUSY', busy: false });
    }
  }

  // --- Session management ---
  let abortController: AbortController | null = null;

  async function createNewSession(title?: string): Promise<void> {
    dispatchSession({ type: 'SET_LOADING', loading: true });
    try {
      const session = await createConductorSession(workspaceRoot, title ?? 'New Session');
      const sessions = await listConductorSessions(workspaceRoot);
      dispatchSession({ type: 'SET_SESSIONS', sessions: sessions.map((s) => ({ id: s.id, title: s.title })) });
      dispatchSession({ type: 'SET_ACTIVE_SESSION', id: session.id, title: session.title });
      dispatchSession({ type: 'SET_MESSAGES', messages: [] });
    } finally {
      dispatchSession({ type: 'SET_LOADING', loading: false });
    }
  }

  async function selectSession(id: string): Promise<void> {
    dispatchSession({ type: 'SET_LOADING', loading: true });
    try {
      const detail = await getConductorSessionDetail(workspaceRoot, id);
      dispatchSession({ type: 'SET_ACTIVE_SESSION', id, title: detail.title });
      const messages = (detail.messages ?? []).map((m: ConductorSessionMessage) => ({
        role: m.role as 'user' | 'assistant' | 'system',
        content: m.content,
      }));
      dispatchSession({ type: 'SET_MESSAGES', messages });
    } finally {
      dispatchSession({ type: 'SET_LOADING', loading: false });
    }
  }

  async function sendMessage(text: string): Promise<void> {
    // Auto-create session if none active
    if (!sessionState.activeSessionId) {
      await createNewSession();
    }

    const sessionId = sessionState.activeSessionId!;

    dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'user', content: text } });
    dispatchSession({ type: 'SET_STREAMING', content: '' });
    dispatchExecution({ type: 'SET_RUNNING', running: true });

    let streamBuffer = '';
    abortController = new AbortController();

    try {
      const result = await runConductorTurn(text, {
        workspaceRoot,
        sessionId,
        stream: true,
        headless: true,
        sandbox: options.sandbox,
        signal: abortController.signal,
        onEvent: (logEvent: ConductorLogEvent) => {
          const { event } = logEvent;

          // Handle streaming text
          if (event.type === 'stream' && event.content) {
            streamBuffer += event.content;
            dispatchSession({ type: 'SET_STREAMING', content: streamBuffer });
          }

          // Track tool activity
          if (event.type === 'tool_call' && event.toolName && event.toolState) {
            const scopeKey = logEvent.scope === 'child'
              ? `child:${logEvent.childSlug ?? '?'}:${event.toolName}`
              : `main:${event.toolName}`;
            const scopeLabel = logEvent.scope === 'child' ? (logEvent.childSlug ?? '?') : 'main';

            if (event.toolState === 'starting' || event.toolState === 'running') {
              dispatchExecution({
                type: 'ADD_ACTIVE_TOOL',
                tool: { scopeKey, scopeLabel, toolName: event.toolName, toolState: event.toolState },
              });
            } else {
              dispatchExecution({ type: 'REMOVE_ACTIVE_TOOL', scopeKey });
              dispatchExecution({ type: 'PUSH_ACTIVITY', line: `${scopeLabel}:${event.toolName} ${event.toolState}` });
            }
          }

          // Track task activity
          if (event.type === 'task_activity' && event.taskId) {
            if (event.taskState === 'started') {
              dispatchExecution({
                type: 'ADD_TASK',
                task: {
                  id: event.taskId,
                  description: event.taskDescription ?? '',
                  state: 'started',
                  depth: executionState.tasks.filter((t) => t.state === 'started').length,
                  startedAt: Date.now(),
                },
              });
            } else if (event.taskState === 'completed' || event.taskState === 'failed') {
              dispatchExecution({
                type: 'UPDATE_TASK',
                id: event.taskId,
                state: event.taskState as 'completed' | 'failed',
              });
            }
          }
        },
      });

      // Finalize
      const finalContent = result.answer || streamBuffer || '';
      if (finalContent) {
        dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'assistant', content: finalContent } });
      }
      if (result.error) {
        dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'system', content: result.error, error: true } });
      }

      // Refresh sessions list
      const sessions = await listConductorSessions(workspaceRoot);
      dispatchSession({ type: 'SET_SESSIONS', sessions: sessions.map((s) => ({ id: s.id, title: s.title })) });
    } catch (error) {
      const message = (error as Error).message;
      dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'system', content: message, error: true } });
    } finally {
      dispatchSession({ type: 'SET_STREAMING', content: null });
      dispatchExecution({ type: 'SET_RUNNING', running: false });
      abortController = null;
    }
  }

  // --- Wire callbacks ---
  input.setOnSubmit((text) => { void handleSubmit(text); });
  helpDialog.setOnClose(() => { dispatchApp({ type: 'SET_OVERLAY', overlay: 'none' }); });

  // --- Event Loop ---
  const eventLoop = new EventLoop({
    root: rootComponent,
    onExit: () => {
      if (appState.busy && abortController) {
        abortController.abort();
        dispatchApp({ type: 'SET_BUSY', busy: false });
      } else {
        eventLoop.stop();
      }
    },
  });

  // Now wire the real requestRender
  requestRender = eventLoop.requestRender;

  // Update all component requestRender references
  header['requestRenderFn'] = requestRender;
  messagesComp['requestRenderFn'] = requestRender;
  activityComp['requestRenderFn'] = requestRender;
  input['requestRenderFn'] = requestRender;
  statusBar['requestRenderFn'] = requestRender;
  helpDialog['requestRenderFn'] = requestRender;
  messageScroll['requestRenderFn'] = requestRender;
  activityScroll['requestRenderFn'] = requestRender;
  mainLayout['requestRenderFn'] = requestRender;

  // --- Initialize ---
  // Load sessions and auto-select or create
  try {
    const sessions = await listConductorSessions(workspaceRoot);
    dispatchSession({ type: 'SET_SESSIONS', sessions: sessions.map((s) => ({ id: s.id, title: s.title })) });
    if (sessions.length > 0) {
      await selectSession(sessions[0].id);
    } else {
      await createNewSession();
    }
  } catch {
    await createNewSession();
  }

  // Start the event loop (blocks until exit)
  await eventLoop.start();

  // Cleanup
  header.dispose();
}
