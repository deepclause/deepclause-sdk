/**
 * SWI-Prolog WASM Loader
 */
interface Query {
    next(): unknown;
    once(): unknown;
}
interface Prolog {
    call(goal: string, opts?: {
        module?: string;
        async?: boolean;
    }): unknown;
    query(goal: string, input?: Record<string, unknown>): Query;
    forEach(goal: string, input?: unknown, callback?: (prolog: Prolog, answer: unknown) => void): Promise<unknown>;
}
interface FSFilesystems {
    NODEFS: unknown;
    MEMFS: unknown;
}
export interface SWIPLModule {
    prolog: Prolog;
    FS: typeof FS & {
        filesystems: FSFilesystems;
    };
}
/**
 * Load and initialize SWI-Prolog WASM
 */
export declare function loadProlog(): Promise<SWIPLModule>;
/**
 * Mount a workspace directory to /workspace in the WASM filesystem
 * Uses NODEFS for direct host filesystem access
 */
export declare function mountWorkspace(swipl: SWIPLModule, workspacePath: string): void;
/**
 * Get the currently mounted workspace path
 */
export declare function getWorkspacePath(): string | null;
export {};
//# sourceMappingURL=loader.d.ts.map