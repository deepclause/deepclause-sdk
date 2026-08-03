export interface ShellExecResult {
    success: boolean;
    stdout: string;
    stderr: string;
    exitCode: number;
    summary: string;
    pid?: number;
    backend: 'host' | 'sandbox';
    backendLabel?: string;
}
export interface ShellExecStartEvent {
    command: string;
    pid?: number;
    backend: 'host' | 'sandbox';
    backendLabel?: string;
}
export interface ShellExecChunkEvent {
    command: string;
    chunk: string;
    pid?: number;
    backend: 'host' | 'sandbox';
    backendLabel?: string;
}
export interface ShellExecExitEvent {
    command: string;
    pid?: number;
    backend: 'host' | 'sandbox';
    backendLabel?: string;
    success: boolean;
    exitCode: number;
    summary: string;
}
export interface ShellExecObserver {
    onStart?: (event: ShellExecStartEvent) => void;
    onStdout?: (event: ShellExecChunkEvent) => void;
    onStderr?: (event: ShellExecChunkEvent) => void;
    onExit?: (event: ShellExecExitEvent) => void;
}
export declare class AgentVMManager {
    private readonly workspacePath;
    private readonly network;
    private vm;
    readonly kind: "sandbox";
    constructor(workspacePath: string, network: boolean);
    exec(command: string, signal?: AbortSignal, observer?: ShellExecObserver): Promise<ShellExecResult>;
    dispose(): Promise<void>;
    private getVM;
}
//# sourceMappingURL=agentvm-manager.d.ts.map