/**
 * Hook for session CRUD operations.
 */
import { useCallback, useEffect } from 'react';
import { createConductorSession, getConductorSessionDetail, listConductorSessions, runConductorTurn, } from '../../../system/runtime/conductor.js';
export function useSession({ workspaceRoot, dispatch }) {
    const loadSessions = useCallback(async () => {
        dispatch({ type: 'SET_LOADING', loading: true });
        try {
            const sessions = await listConductorSessions(workspaceRoot);
            dispatch({ type: 'SET_SESSIONS', sessions });
        }
        finally {
            dispatch({ type: 'SET_LOADING', loading: false });
        }
    }, [workspaceRoot, dispatch]);
    const selectSession = useCallback(async (id) => {
        dispatch({ type: 'SET_LOADING', loading: true });
        try {
            const detail = await getConductorSessionDetail(workspaceRoot, id);
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
            await loadSessions();
            await selectSession(session.id);
        }
        finally {
            dispatch({ type: 'SET_LOADING', loading: false });
        }
    }, [workspaceRoot, dispatch, loadSessions, selectSession]);
    const sendMessage = useCallback(async (text) => {
        if (!text.trim())
            return;
        dispatch({ type: 'APPEND_MESSAGE', message: { role: 'user', content: text } });
        dispatch({ type: 'SET_STREAMING', content: '' });
        try {
            const logHandler = (logEvent) => {
                const { event } = logEvent;
                if (event.type === 'stream' && event.content) {
                    // Streaming updates handled via execution store
                    dispatch({ type: 'SET_STREAMING', content: event.content });
                }
                if (event.type === 'answer' && event.content) {
                    dispatch({ type: 'APPEND_MESSAGE', message: { role: 'assistant', content: event.content } });
                }
            };
            await runConductorTurn(text, { workspaceRoot, onEvent: logHandler });
        }
        finally {
            dispatch({ type: 'SET_STREAMING', content: null });
        }
    }, [workspaceRoot, dispatch]);
    // Load sessions on mount
    useEffect(() => {
        void loadSessions();
    }, [loadSessions]);
    return { loadSessions, createSession, selectSession, sendMessage };
}
//# sourceMappingURL=useSession.js.map