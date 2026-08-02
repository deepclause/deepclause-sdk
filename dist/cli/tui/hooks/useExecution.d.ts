/**
 * Hook for subscribing to conductor execution events.
 */
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
export declare function useExecution({ dispatch }: UseExecutionOptions): ExecutionActions;
//# sourceMappingURL=useExecution.d.ts.map