/**
 * TUI v3 Entry Point — startTuiV3()
 *
 * Wires together the event loop, renderer, state management, and components
 * into a fully functioning terminal UI with zero-flicker differential rendering.
 *
 * Layout: Borland 90s IDE style with multi-pane layout.
 * ┌─────────────────────────────────────────────────────────┐
 * │ ≡ DeepClause                              session title │  <- Logo bar
 * ├───┬──────────────────────────┬──────────────────────────┤
 * │ S │                          │  Activity (Execution)    │
 * │ e │      Messages            ├──────────────────────────┤
 * │ s │      (main content)      │  Steps (Tasks)           │
 * │ s │                          ├──────────────────────────┤
 * │   │                          │  Context (Tokens)        │
 * ├───┴──────────────────────────┴──────────────────────────┤
 * │┌─────────────────────────────────────────────────────┐  │
 * ││› multiline input                                    │  │  <- Editor
 * │└─────────────────────────────────────────────────────┘  │
 * ├─────────────────────────────────────────────────────────┤
 * │ F1 Help F2 Sess F3 Msgs F4 Exec F5 Task F6 Ctx Tab▸  │  <- Status bar
 * └─────────────────────────────────────────────────────────┘
 */

import { EventLoop } from './event-loop.js';
import { ScrollView } from './layout/scroll-view.js';
import { Header } from './components/header.js';
import { Input } from './components/input.js';
import { StatusBar } from './components/status-bar.js';
import { Messages } from './components/messages.js';
import { Activity } from './components/activity.js';
import { Sessions } from './components/sessions.js';
import { Tasks } from './components/tasks.js';
import { Context } from './components/context.js';
import { HelpDialog } from './dialogs/help.js';
import { composeOverlay } from './layout/overlay.js';
import {
  createInitialAppState,
  appReducer,
  type AppAction,
  type PaneKind,
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
import { style, ANSI, padRight, truncate } from './util/ansi.js';
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
  const sessionsComp = new Sessions(requestRender);
  const tasksComp = new Tasks(requestRender);
  const contextComp = new Context(requestRender);
  const input = new Input(requestRender);
  const statusBar = new StatusBar(requestRender);
  const helpDialog = new HelpDialog(requestRender);

  const messageScroll = new ScrollView(messagesComp, requestRender);
  const activityScroll = new ScrollView(activityComp, requestRender);
  const tasksScroll = new ScrollView(tasksComp, requestRender);
  const contextScroll = new ScrollView(contextComp, requestRender);

  // --- Pane border wrappers ---
  // Each pane is wrapped with a Borland-style border that shows focus state
  function renderPaneWithBorder(
    title: string,
    content: string[],
    width: number,
    height: number,
    focused: boolean,
  ): string[] {
    const borderColor = focused ? ANSI.brightCyan : ANSI.cyan;
    const titleColor = focused ? ANSI.brightWhite : ANSI.white;
    const innerWidth = Math.max(0, width - 2);
    const innerHeight = Math.max(0, height - 2);

    const rows: string[] = [];

    // Top border with title
    const titleStr = ` ${title} `;
    const titleLen = titleStr.length;
    const topLeft = '┌';
    const topRight = '┐';
    const topFill = '─'.repeat(Math.max(0, innerWidth - titleLen));
    rows.push(
      style(topLeft, borderColor)
      + style(titleStr, titleColor, ANSI.bold)
      + style(topFill + topRight, borderColor),
    );

    // Content rows
    for (let i = 0; i < innerHeight; i++) {
      const line = content[i] ?? '';
      const paddedLine = padRight(truncate(line, innerWidth), innerWidth);
      rows.push(style('│', borderColor) + paddedLine + style('│', borderColor));
    }

    // Bottom border
    rows.push(style('└' + '─'.repeat(innerWidth) + '┘', borderColor));

    return rows;
  }

  // --- Right column component that stacks Activity, Tasks, Context ---
  const rightColumn: Component = {
    dirty: true,
    minHeight: 1,
    flexGrow: 1,
    invalidate() { this.dirty = true; requestRender(); },
    render(width: number): string[] {
      const rows: string[] = [];
      const totalHeight = Math.max(3, appState.rows - 1 - input.height - 1);

      // Divide right column among visible panes
      const visiblePanes: Array<{ key: PaneKind; title: string; scroll: ScrollView }> = [];
      if (appState.paneVisibility.process) {
        visiblePanes.push({ key: 'process', title: 'Activity', scroll: activityScroll });
      }
      if (appState.paneVisibility.tasks) {
        visiblePanes.push({ key: 'tasks', title: 'Steps', scroll: tasksScroll });
      }
      if (appState.paneVisibility.context) {
        visiblePanes.push({ key: 'context', title: 'Context', scroll: contextScroll });
      }

      if (visiblePanes.length === 0) {
        return Array(totalHeight).fill('');
      }

      const basePaneHeight = Math.floor(totalHeight / visiblePanes.length);
      let remainingHeight = totalHeight % visiblePanes.length;

      for (const pane of visiblePanes) {
        const paneHeight = basePaneHeight + (remainingHeight-- > 0 ? 1 : 0);
        const content = pane.scroll.renderWithHeight(width - 2, paneHeight - 2);
        const bordered = renderPaneWithBorder(
          pane.title,
          content,
          width,
          paneHeight,
          appState.focusedPane === pane.key,
        );
        rows.push(...bordered);
      }

      return rows;
    },
  };

  // --- Root component that orchestrates the full layout ---
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
      const rows: string[] = [];

      // === Header (1 row) ===
      rows.push(...header.render(width));

      // === Main content area ===
      // Height = total - header(1) - dynamic input - status(1)
      const inputHeight = input.height;
      const contentHeight = Math.max(3, height - 1 - inputHeight - 1);

      // Layout columns: [sessions?] [messages] [right-sidebar?]
      const sessionVisible = appState.paneVisibility.sessions;
      const rightVisible = appState.paneVisibility.process || appState.paneVisibility.tasks || appState.paneVisibility.context;

      const sessionWidth = sessionVisible ? Math.min(sessionsComp.isCollapsed ? 5 : 26, Math.max(0, width - 20)) : 0;
      const availableAfterSessions = Math.max(0, width - sessionWidth);
      const rightWidth = rightVisible
        ? Math.min(50, Math.max(0, availableAfterSessions - 20), Math.max(24, Math.floor(width * 0.3)))
        : 0;
      const messagesWidth = Math.max(0, width - sessionWidth - rightWidth);

      // Render each column
      const sessionRows: string[] = [];
      const messageRows: string[] = [];
      const rightRows: string[] = [];

      // Sessions pane
      if (sessionVisible) {
        const sessContent = sessionsComp.render(sessionWidth - 2);
        const bordered = renderPaneWithBorder(
          'Sess',
          sessContent,
          sessionWidth,
          contentHeight,
          appState.focusedPane === 'sessions',
        );
        sessionRows.push(...bordered);
      }

      // Messages pane (center)
      const msgContent = messageScroll.renderWithHeight(messagesWidth - 2, contentHeight - 2);
      const msgBordered = renderPaneWithBorder(
        'Messages',
        msgContent,
        messagesWidth,
        contentHeight,
        appState.focusedPane === 'messages',
      );
      messageRows.push(...msgBordered);

      // Right column
      if (rightVisible) {
        rightRows.push(...rightColumn.render(rightWidth));
      }

      // Join columns side-by-side
      for (let i = 0; i < contentHeight; i++) {
        let line = '';
        if (sessionVisible) {
          line += padRight(sessionRows[i] ?? '', sessionWidth);
        }
        line += padRight(messageRows[i] ?? '', messagesWidth);
        if (rightVisible) {
          line += padRight(rightRows[i] ?? '', rightWidth);
        }
        rows.push(line);
      }

      // === Input area ===
      rows.push(...input.render(width));

      // === Status bar ===
      rows.push(...statusBar.render(width));

      // === Overlay (help dialog) ===
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

      // === Function key shortcuts (Borland-style pane navigation) ===
      // F1 = Help
      if (key.name === 'f1') {
        helpDialog.show();
        return true;
      }
      // F2 = Toggle Sessions pane
      if (key.name === 'f2') {
        dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane: 'sessions' });
        return true;
      }
      // F3 = Focus Messages
      if (key.name === 'f3') {
        dispatchApp({ type: 'SET_FOCUSED_PANE', pane: 'messages' });
        return true;
      }
      // F4 = Focus/Toggle Activity (Execution)
      if (key.name === 'f4') {
        if (!appState.paneVisibility.process) {
          dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane: 'process' });
        }
        dispatchApp({ type: 'SET_FOCUSED_PANE', pane: 'process' });
        return true;
      }
      // F5 = Focus/Toggle Tasks
      if (key.name === 'f5') {
        if (!appState.paneVisibility.tasks) {
          dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane: 'tasks' });
        }
        dispatchApp({ type: 'SET_FOCUSED_PANE', pane: 'tasks' });
        return true;
      }
      // F6 = Focus/Toggle Context
      if (key.name === 'f6') {
        if (!appState.paneVisibility.context) {
          dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane: 'context' });
        }
        dispatchApp({ type: 'SET_FOCUSED_PANE', pane: 'context' });
        return true;
      }

      // ? = Help (when not typing)
      if (key.sequence === '?' && !input.active) {
        helpDialog.show();
        return true;
      }

      // Shift+Tab: cycle backwards
      if (key.name === 'tab' && key.shift) {
        // Reverse cycle
        const visiblePanes = (['sessions', 'messages', 'process', 'tasks', 'context'] as PaneKind[])
          .filter((p) => appState.paneVisibility[p]);
        if (visiblePanes.length > 0) {
          const idx = visiblePanes.indexOf(appState.focusedPane);
          const prev = visiblePanes[(idx - 1 + visiblePanes.length) % visiblePanes.length];
          dispatchApp({ type: 'SET_FOCUSED_PANE', pane: prev });
        }
        return true;
      }

      // Tab: cycle through visible panes
      if (key.name === 'tab' && !key.ctrl && !key.meta) {
        dispatchApp({ type: 'CYCLE_PANE' });
        return true;
      }

      // Alt+1-5: direct pane focus
      if (key.meta && key.sequence >= '1' && key.sequence <= '5') {
        const panes: PaneKind[] = ['sessions', 'messages', 'process', 'tasks', 'context'];
        const idx = parseInt(key.sequence) - 1;
        if (idx < panes.length && appState.paneVisibility[panes[idx]]) {
          dispatchApp({ type: 'SET_FOCUSED_PANE', pane: panes[idx] });
        }
        return true;
      }

      // Scroll (Shift+Up/Down or PgUp/PgDn) based on focused pane
      if (appState.focusedPane === 'messages') {
        if (messageScroll.handleInput(key)) return true;
      }
      if (appState.focusedPane === 'process') {
        if (activityScroll.handleInput(key)) return true;
      }
      if (appState.focusedPane === 'tasks') {
        if (tasksScroll.handleInput(key)) return true;
      }
      if (appState.focusedPane === 'context') {
        if (contextScroll.handleInput(key)) return true;
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

    // Sync sessions pane
    sessionsComp.setSessions(sessionState.sessions.map((s) => ({ id: s.id, title: s.title })));
    sessionsComp.setActiveSession(sessionState.activeSessionId);

    // Sync tasks pane
    tasksComp.setTasks(executionState.tasks.map((t) => ({
      id: t.id,
      description: t.description,
      state: t.state,
      depth: t.depth,
    })));

    // Sync context pane
    contextComp.setTokenUsage(executionState.tokenUsage);

    // Sync status bar
    statusBar.setMode(appState.mode);
    statusBar.setFocusedPane(appState.focusedPane);
    statusBar.setBusy(appState.busy);
    statusBar.setFollowMode(messageScroll.isFollowing);
    statusBar.setPaneVisibility(appState.paneVisibility);

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
          dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane: 'sessions' });
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

          // Track token usage
          if (event.type === 'usage' && event.usage) {
            const modelKey = logEvent.modelId ?? 'unknown';
            const current = { ...executionState.tokenUsage };
            const existing = current[modelKey] ?? { input: 0, output: 0 };
            current[modelKey] = {
              input: existing.input + (event.usage.inputTokens ?? 0),
              output: existing.output + (event.usage.outputTokens ?? 0),
            };
            dispatchExecution({ type: 'SET_TOKEN_USAGE', usage: current });
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
  sessionsComp['requestRenderFn'] = requestRender;
  tasksComp['requestRenderFn'] = requestRender;
  contextComp['requestRenderFn'] = requestRender;
  input['requestRenderFn'] = requestRender;
  statusBar['requestRenderFn'] = requestRender;
  helpDialog['requestRenderFn'] = requestRender;
  messageScroll['requestRenderFn'] = requestRender;
  activityScroll['requestRenderFn'] = requestRender;
  tasksScroll['requestRenderFn'] = requestRender;
  contextScroll['requestRenderFn'] = requestRender;

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
