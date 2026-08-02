/**
 * Hook for session CRUD operations.
 * Wires session management and conductor turn execution to the TUI stores.
 */
import { useCallback, useEffect, useRef } from 'react';
import { createConductorSession, getConductorSessionDetail, listConductorSessions, runConductorTurn, } from '../../../system/runtime/conductor.js';
export function useSession({ workspaceRoot, dispatch, executionDispatch, sandbox }) {
    const activeSessionIdRef = useRef(null);
    const streamBufferRef = useRef('');
    const abortRef = useRef(null);
    const loadSessions = useCallback(async () => {
        dispatch({ type: 'SET_LOADING', loading: true });
        try {
            const sessions = await listConductorSessions(workspaceRoot);
            dispatch({ type: 'SET_SESSIONS', sessions });
            return sessions;
        }
        finally {
            dispatch({ type: 'SET_LOADING', loading: false });
        }
    }, [workspaceRoot, dispatch]);
    const selectSession = useCallback(async (id) => {
        dispatch({ type: 'SET_LOADING', loading: true });
        try {
            const detail = await getConductorSessionDetail(workspaceRoot, id);
            activeSessionIdRef.current = id;
            dispatch({ type: 'SET_ACTIVE_SESSION', id, detail });
            // Convert detail messages to display messages
            const messages = (detail.messages ?? []).map((m) => ({
                role: m.role,
                content: m.content,
            }));
            dispatch({ type: 'SET_MESSAGES', messages });
        }
        finally {
            dispatch({ type: 'SET_LOADING', loading: false });
        }
    }, [workspaceRoot, dispatch]);
    const createSession = useCallback(async (title) => {
        dispatch({ type: 'SET_LOADING', loading: true });
        try {
            const session = await createConductorSession(workspaceRoot, title ?? 'New Session');
            const sessions = await listConductorSessions(workspaceRoot);
            dispatch({ type: 'SET_SESSIONS', sessions });
            await selectSession(session.id);
            return session;
        }
        finally {
            dispatch({ type: 'SET_LOADING', loading: false });
        }
    }, [workspaceRoot, dispatch, selectSession]);
    const sendMessage = useCallback(async (text) => {
        if (!text.trim())
            return;
        // Auto-create session if none active
        if (!activeSessionIdRef.current) {
            const session = await createConductorSession(workspaceRoot, 'New Session');
            activeSessionIdRef.current = session.id;
            const sessions = await listConductorSessions(workspaceRoot);
            dispatch({ type: 'SET_SESSIONS', sessions });
            dispatch({ type: 'SET_ACTIVE_SESSION', id: session.id, detail: { ...session, messages: [] } });
        }
        const sessionId = activeSessionIdRef.current;
        dispatch({ type: 'APPEND_MESSAGE', message: { role: 'user', content: text } });
        dispatch({ type: 'SET_STREAMING', content: '' });
        streamBufferRef.current = '';
        executionDispatch({ type: 'SET_RUNNING', running: true });
        executionDispatch({ type: 'PUSH_ACTIVITY', line: `task ${text}` });
        abortRef.current = new AbortController();
        try {
            const result = await runConductorTurn(text, {
                workspaceRoot,
                sessionId,
                stream: true,
                headless: true,
                sandbox,
                signal: abortRef.current.signal,
                onEvent: (logEvent) => {
                    const { event } = logEvent;
                    // Forward all events to execution store for activity/tasks/tools tracking
                    executionDispatch({ type: 'HANDLE_LOG_EVENT', event: logEvent });
                    // Handle streaming text accumulation
                    if (event.type === 'stream' && event.content) {
                        streamBufferRef.current += event.content;
                        dispatch({ type: 'SET_STREAMING', content: streamBufferRef.current });
                    }
                },
            });
            // Finalize: use the accumulated stream content or the result answer
            const finalContent = result.answer || streamBufferRef.current || '';
            if (finalContent) {
                dispatch({ type: 'APPEND_MESSAGE', message: { role: 'assistant', content: finalContent } });
            }
            if (result.error) {
                dispatch({ type: 'APPEND_MESSAGE', message: { role: 'system', content: result.error, error: true } });
                executionDispatch({ type: 'PUSH_ACTIVITY', line: `error ${result.error}` });
            }
            // Refresh sessions list after turn completes
            const sessions = await listConductorSessions(workspaceRoot);
            dispatch({ type: 'SET_SESSIONS', sessions });
        }
        catch (error) {
            const message = error.message;
            dispatch({ type: 'APPEND_MESSAGE', message: { role: 'system', content: message, error: true } });
            executionDispatch({ type: 'PUSH_ACTIVITY', line: `error ${message}` });
        }
        finally {
            dispatch({ type: 'SET_STREAMING', content: null });
            executionDispatch({ type: 'SET_RUNNING', running: false });
            abortRef.current = null;
            streamBufferRef.current = '';
        }
    }, [workspaceRoot, dispatch, executionDispatch, sandbox]);
    // Load sessions on mount and auto-select/create
    useEffect(() => {
        void (async () => {
            const sessions = await loadSessions();
            if (sessions && sessions.length > 0) {
                await selectSession(sessions[0].id);
            }
            else {
                await createSession();
            }
        })();
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, []);
    return { loadSessions, createSession, selectSession, sendMessage };
}
//# sourceMappingURL=useSession.js.map