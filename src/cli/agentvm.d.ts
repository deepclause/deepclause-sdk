declare module 'deepclause-agentvm' {
  export interface AgentVMResult {
    stdout: string;
    stderr: string;
    exitCode: number;
  }

  export interface AgentVMOptions {
    network?: boolean;
    mounts?: Record<string, string>;
    wasmPath?: string;
    mac?: string;
  }

  export class AgentVM {
    constructor(options?: AgentVMOptions);
    start(): Promise<void>;
    stop(): Promise<void>;
    exec(command: string): Promise<AgentVMResult>;
  }
}
