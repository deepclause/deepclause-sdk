import { type ShellExecObserver, type ShellExecResult } from './agentvm-manager.js';
export type HostShellWrapperKind = 'clean-room' | 'bwrap' | 'sandbox-exec';
export type HostShellWrapperPreference = 'auto' | HostShellWrapperKind;
export type HostShellBwrapExecWrapperMode = 'direct' | 'setpriv-no-new-privs';
export interface HostShellConfig {
    wrapper?: HostShellWrapperPreference;
    strictIsolation?: boolean;
}
export interface ResolvedHostShellStrategy {
    wrapperKind: HostShellWrapperKind;
    strictIsolation: boolean;
    backendLabel: string;
    description: string;
    bwrapExecWrapperMode?: HostShellBwrapExecWrapperMode;
}
interface HostShellWrapperProbes {
    isBwrapUsable(strictIsolation: boolean): boolean;
    resolveBwrapExecWrapperMode?: (strictIsolation: boolean) => HostShellBwrapExecWrapperMode | null;
    isSandboxExecUsable(): boolean;
}
export interface ShellManager {
    readonly kind: 'host' | 'sandbox';
    exec(command: string, signal?: AbortSignal, observer?: ShellExecObserver): Promise<ShellExecResult>;
    dispose(): Promise<void>;
}
export interface CreateShellManagerOptions {
    workspacePath: string;
    sandbox?: boolean;
    network?: boolean;
    hostConfig?: HostShellConfig;
}
export declare class HostShellManager implements ShellManager {
    private readonly workspacePath;
    readonly kind: "host";
    private readonly strategy;
    constructor(workspacePath: string, hostConfig?: HostShellConfig);
    exec(command: string, signal?: AbortSignal, observer?: ShellExecObserver): Promise<ShellExecResult>;
    dispose(): Promise<void>;
}
export declare function createShellManager(options: CreateShellManagerOptions): ShellManager;
export declare function describeShellExecutionBackend(sandbox?: boolean, hostConfig?: HostShellConfig): {
    backendLabel: string;
    description: string;
};
export declare function resolveHostShellStrategy(options?: {
    hostConfig?: HostShellConfig;
    platform?: NodeJS.Platform;
    probes?: HostShellWrapperProbes;
    envOverride?: string;
}): ResolvedHostShellStrategy;
export declare function resolveHostShellWrapperKind(options?: {
    platform?: NodeJS.Platform;
    probes?: HostShellWrapperProbes;
    wrapperPreference?: HostShellWrapperPreference;
    envOverride?: string;
    strictIsolation?: boolean;
}): HostShellWrapperKind;
export {};
//# sourceMappingURL=shell-manager.d.ts.map