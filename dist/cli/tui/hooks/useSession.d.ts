/**
 * Hook for session CRUD operations.
 */
import type { SessionAction } from '../store/session-store.js';
export interface UseSessionOptions {
    workspaceRoot: string;
    dispatch: (action: SessionAction) => void;
}
export interface SessionActions {
    loadSessions: () => Promise<void>;
    createSession: (title?: string) => Promise<void>;
    selectSession: (id: string) => Promise<void>;
    sendMessage: (text: string) => Promise<void>;
}
export declare function useSession({ workspaceRoot, dispatch }: UseSessionOptions): SessionActions;
//# sourceMappingURL=useSession.d.ts.map