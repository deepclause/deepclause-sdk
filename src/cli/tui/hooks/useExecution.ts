/**
 * Hook for subscribing to conductor execution events.
 */

import { useCallback } from 'react';
import type { ExecutionAction } from '../store/execution-store.js';
import type { ConductorLogEvent } from '../../../system/runtime/conductor.js';

export interface UseExecutionOptions {
  dispatch: (action: ExecutionAction) => void;
}

export interface ExecutionActions {
  handleLogEvent: (event: ConductorLogEvent) => void;
  clearActivity: () => void;
  setRunning: (running: boolean) => void;
}

export function useExecution({ dispatch }: UseExecutionOptions): ExecutionActions {
  const handleLogEvent = useCallback((event: ConductorLogEvent) => {
    dispatch({ type: 'HANDLE_LOG_EVENT', event });
  }, [dispatch]);

  const clearActivity = useCallback(() => {
    dispatch({ type: 'CLEAR_ACTIVITY' });
  }, [dispatch]);

  const setRunning = useCallback((running: boolean) => {
    dispatch({ type: 'SET_RUNNING', running });
  }, [dispatch]);

  return { handleLogEvent, clearActivity, setRunning };
}
