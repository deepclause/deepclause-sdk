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
 * │ S │                          │                          │
 * │ e │      Messages            │  Context (Tokens)        │
 * │ s │      (main content)      │                          │
 * │ s │                          │                          │
 * │   │                          │                          │
 * ├───┴──────────────────────────┴──────────────────────────┤
 * │┌─────────────────────────────────────────────────────┐  │
 * ││› multiline input                                    │  │  <- Editor
 * │└─────────────────────────────────────────────────────┘  │
 * ├─────────────────────────────────────────────────────────┤
 * │ F1 Help F2 Sess F3 Ctx Tab Next ^C Quit               │  <- Status bar
 * └─────────────────────────────────────────────────────────┘
 */

import { EventLoop } from './event-loop.js';
import { ScrollView } from './layout/scroll-view.js';
import { Header } from './components/header.js';
import { Input } from './components/input.js';
import { StatusBar } from './components/status-bar.js';
import { Messages } from './components/messages.js';
import { Sessions } from './components/sessions.js';
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
  type SessionSummary,
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
  const sessionsComp = new Sessions(requestRender);
  const contextComp = new Context(requestRender);
  const input = new Input(requestRender);
  const statusBar = new StatusBar(requestRender);
  const helpDialog = new HelpDialog(requestRender);

  const messageScroll = new ScrollView(messagesComp, requestRender);
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
    const borderColor = focused ? ANSI.brightWhite : ANSI.cyan;
    const titleColor = focused ? ANSI.brightYellow : ANSI.white;
    const innerWidth = Math.max(0, width - 2);
    const innerHeight = Math.max(0, height - 2);

    const rows: string[] = [];

    // Top border with title
    const titleStr = ` ${title} `;
    const titleLen = titleStr.length;
    const topLeft = '╔';
    const topRight = '╗';
    const topFill = '═'.repeat(Math.max(0, innerWidth - titleLen));
    rows.push(
      style(topLeft, borderColor)
      + style(titleStr, titleColor, ANSI.bold)
      + style(topFill + topRight, borderColor),
    );

    // Content rows
    for (let i = 0; i < innerHeight; i++) {
      const line = content[i] ?? '';
      const paddedLine = padRight(truncate(line, innerWidth), innerWidth);
      rows.push(style('║', borderColor) + paddedLine + style('║', borderColor));
    }

    // Bottom border
    rows.push(style('╚' + '═'.repeat(innerWidth) + '╝', borderColor));

    return rows;
  }

  // --- Hideable right-side context column ---
  const rightColumn: Component = {
    dirty: true,
    minHeight: 1,
    flexGrow: 1,
    invalidate() { this.dirty = true; requestRender(); },
    render(width: number): string[] {
      const totalHeight = Math.max(3, appState.rows - 1 - input.height - 1);
      if (!appState.paneVisibility.context) {
        return Array(totalHeight).fill('');
      }
      const content = contextScroll.renderWithHeight(width - 2, totalHeight - 2);
      return renderPaneWithBorder(
        'Context',
        content,
        width,
        totalHeight,
        appState.focusedPane === 'context',
      );
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
      const rightVisible = appState.paneVisibility.context;

      const sessionWidth = sessionVisible ? Math.min(sessionsComp.isCollapsed ? 5 : 32, Math.max(0, width - 20)) : 0;
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
        const sessContent = sessionsComp.renderWithHeight(sessionWidth - 2, contentHeight - 2);
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
      const handlePaneShortcut = (pane: PaneKind): void => {
        if (!appState.paneVisibility[pane]) {
          dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane });
          dispatchApp({ type: 'SET_FOCUSED_PANE', pane });
        } else if (appState.focusedPane === pane) {
          dispatchApp({ type: 'TOGGLE_PANE_VISIBILITY', pane });
        } else {
          dispatchApp({ type: 'SET_FOCUSED_PANE', pane });
        }
      };

      // F1 = Help
      if (key.name === 'f1') {
        helpDialog.show();
        return true;
      }
      // F2 = Toggle Sessions pane
      if (key.name === 'f2') {
        handlePaneShortcut('sessions');
        return true;
      }
      // F3 = Focus/Toggle Context
      if (key.name === 'f3') {
        handlePaneShortcut('context');
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
        const visiblePanes = (['sessions', 'messages', 'context'] as PaneKind[])
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

      // Alt+1-3: direct pane focus
      if (key.meta && key.sequence >= '1' && key.sequence <= '3') {
        const panes: PaneKind[] = ['sessions', 'messages', 'context'];
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
      if (appState.focusedPane === 'context') {
        if (contextScroll.handleInput(key)) return true;
      }
      if (appState.focusedPane === 'sessions') {
        if (sessionsComp.handleInput(key)) return true;
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

    // Sync sessions pane
    sessionsComp.setSessions(sessionState.sessions.map((s) => ({
      id: s.id,
      title: s.title,
      updatedAt: s.updatedAt,
    })));
    sessionsComp.setActiveSession(sessionState.activeSessionId);

    // Sync context pane
    contextComp.setTokenUsage(executionState.tokenUsage);
    contextComp.setContextTokens(executionState.contextTokens);

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
  let memoryContextTokens = 0;

  async function createNewSession(title?: string): Promise<void> {
    dispatchSession({ type: 'SET_LOADING', loading: true });
    try {
      const session = await createConductorSession(workspaceRoot, title ?? 'New Session');
      const sessions = await listConductorSessions(workspaceRoot);
      dispatchSession({ type: 'SET_SESSIONS', sessions: sessions.map(toSessionSummary) });
      dispatchSession({ type: 'SET_ACTIVE_SESSION', id: session.id, title: session.title });
      dispatchSession({ type: 'SET_MESSAGES', messages: [] });
      dispatchExecution({ type: 'CLEAR_ACTIVITY' });
      dispatchExecution({ type: 'SET_TOKEN_USAGE', usage: {} });
      dispatchExecution({ type: 'SET_CONTEXT_TOKENS', tokens: 0 });
      memoryContextTokens = 0;
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
      memoryContextTokens = estimateTextTokens(detail.taskMemory ?? '')
        + estimateTextTokens(detail.assistantMemory ?? '');
      dispatchExecution({ type: 'CLEAR_ACTIVITY' });
      dispatchExecution({
        type: 'SET_TOKEN_USAGE',
        usage: Object.fromEntries(Object.entries(detail.usageByModel ?? {}).map(([model, usage]) => [
          model,
          { input: usage.inputTokens, output: usage.outputTokens },
        ])),
      });
      dispatchExecution({ type: 'SET_CONTEXT_TOKENS', tokens: estimateContextTokens(detail) });
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
    dispatchExecution({ type: 'CLEAR_ACTIVITY' });
    dispatchExecution({ type: 'SET_RUNNING', running: true });
    dispatchExecution({
      type: 'SET_CONTEXT_TOKENS',
      tokens: estimateMessageTokens(sessionState.messages) + memoryContextTokens,
    });

    let streamBuffer = '';
    let answerReceived = false;
    const displayedTools = new Set<string>();
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

          // Show tool invocations in the thinking box, never their output.
          if (event.type === 'tool_call' && event.toolName) {
            const scopeKey = logEvent.scope === 'child'
              ? `child:${logEvent.childSlug ?? '?'}:${event.toolName}`
              : `main:${event.toolName}`;
            const scopeLabel = logEvent.scope === 'child' ? (logEvent.childSlug ?? '?') : 'main';
            if (
              !displayedTools.has(scopeKey)
              && (event.toolState === undefined || event.toolState === 'starting' || event.toolState === 'running')
            ) {
              displayedTools.add(scopeKey);
              streamBuffer += `${streamBuffer && !streamBuffer.endsWith('\n') ? '\n' : ''}▶ ${scopeLabel}:${event.toolName}\n`;
              dispatchSession({ type: 'SET_STREAMING', content: streamBuffer });
            }
          }

          if (event.type === 'answer') {
            answerReceived = true;
            dispatchSession({ type: 'SET_STREAMING', content: null });
            if (event.content) {
              dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'assistant', content: event.content } });
              dispatchExecution({
                type: 'SET_CONTEXT_TOKENS',
                tokens: estimateMessageTokens(sessionState.messages) + memoryContextTokens,
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
      if (finalContent && !answerReceived) {
        dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'assistant', content: finalContent } });
      }
      if (result.error) {
        dispatchSession({ type: 'APPEND_MESSAGE', message: { role: 'system', content: result.error, error: true } });
      }

      // Refresh sessions list
      const sessions = await listConductorSessions(workspaceRoot);
      dispatchSession({ type: 'SET_SESSIONS', sessions: sessions.map(toSessionSummary) });
      const activeSession = sessions.find((session) => session.id === sessionId);
      if (activeSession) {
        dispatchSession({ type: 'SET_ACTIVE_SESSION', id: activeSession.id, title: activeSession.title });
      }
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
  sessionsComp.setOnSelect((id) => {
    if (!appState.busy) void selectSession(id);
  });
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
    onResize: (columns, rows) => {
      dispatchApp({ type: 'RESIZE', columns, rows });
    },
  });

  // Now wire the real requestRender
  requestRender = eventLoop.requestRender;

  // Update all component requestRender references
  header['requestRenderFn'] = requestRender;
  messagesComp['requestRenderFn'] = requestRender;
  sessionsComp['requestRenderFn'] = requestRender;
  contextComp['requestRenderFn'] = requestRender;
  input['requestRenderFn'] = requestRender;
  statusBar['requestRenderFn'] = requestRender;
  helpDialog['requestRenderFn'] = requestRender;
  messageScroll['requestRenderFn'] = requestRender;
  contextScroll['requestRenderFn'] = requestRender;

  // --- Initialize ---
  // Load sessions and auto-select or create
  try {
    const sessions = await listConductorSessions(workspaceRoot);
    dispatchSession({ type: 'SET_SESSIONS', sessions: sessions.map(toSessionSummary) });
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

function toSessionSummary(session: { id: string; title: string; updatedAt: string }): SessionSummary {
  return { id: session.id, title: session.title, updatedAt: session.updatedAt };
}

function estimateContextTokens(detail: {
  messages?: ConductorSessionMessage[];
  taskMemory?: string;
  assistantMemory?: string;
}): number {
  return estimateMessageTokens(detail.messages ?? [])
    + estimateTextTokens(detail.taskMemory ?? '')
    + estimateTextTokens(detail.assistantMemory ?? '');
}

function estimateMessageTokens(messages: Array<{ role: string; content: string }>): number {
  return estimateTextTokens(messages.map((message) => `${message.role}: ${message.content}`).join('\n'));
}

function estimateTextTokens(text: string): number {
  const normalized = text.trim();
  return normalized ? Math.max(1, Math.ceil(normalized.length / 4)) : 0;
}
