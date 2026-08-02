/**
 * Hook for subscribing to conductor execution events.
 */
import { useCallback } from 'react';
export function useExecution({ dispatch }) {
    const handleLogEvent = useCallback((event) => {
        dispatch({ type: 'HANDLE_LOG_EVENT', event });
    }, [dispatch]);
    const clearActivity = useCallback(() => {
        dispatch({ type: 'CLEAR_ACTIVITY' });
    }, [dispatch]);
    const setRunning = useCallback((running) => {
        dispatch({ type: 'SET_RUNNING', running });
    }, [dispatch]);
    return { handleLogEvent, clearActivity, setRunning };
}
//# sourceMappingURL=useExecution.js.map