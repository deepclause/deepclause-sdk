/**
 * Hook for session CRUD operations.
 * Wires session management and conductor turn execution to the TUI stores.
 */
import type { SessionAction } from '../store/session-store.js';
import type { ExecutionAction } from '../store/execution-store.js';
import { type ConductorSessionSummary } from '../../../system/runtime/conductor.js';
export interface UseSessionOptions {
    workspaceRoot: string;
    dispatch: (action: SessionAction) => void;
    executionDispatch: (action: ExecutionAction) => void;
    sandbox?: boolean;
}
export interface SessionActions {
    loadSessions: () => Promise<ConductorSessionSummary[] | void>;
    createSession: (title?: string) => Promise<ConductorSessionSummary | void>;
    selectSession: (id: string) => Promise<void>;
    sendMessage: (text: string) => Promise<void>;
}
export declare function useSession({ workspaceRoot, dispatch, executionDispatch, sandbox }: UseSessionOptions): SessionActions;
//# sourceMappingURL=useSession.d.ts.map