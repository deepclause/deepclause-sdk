/**
 * Types for `deepclause-agentvm`, which ships JavaScript only.
 *
 * The surface below is the one the SDK uses plus the options the runtime accepts
 * (see the package README's `new AgentVM(options)` section). Update it when a
 * dependency bump changes the constructor or a method signature.
 */
declare module 'deepclause-agentvm' {
  export interface AgentVMResult {
    stdout: string;
    stderr: string;
    exitCode: number;
  }

  export interface AgentVMOptions {
    /** Host path to the `.wasm` image; defaults to the one shipped with the package. */
    wasmPath?: string;
    /** VM path to host path, e.g. `{ '/workspace': '/host/dir' }`. */
    mounts?: Record<string, string>;
    /** Enable networking (default: true). */
    network?: boolean;
    /** VM-wide network rate limit in bytes/sec (default: 2 MiB/s; 0 disables). */
    networkRateLimit?: number;
    mac?: string;
    debug?: boolean;
    /** Raw/interactive mode: skip the exec-mode shell setup. */
    interactive?: boolean;
    /** Persist the guest root via an ext4 overlay; requires a `/workspace` mount. */
    persistentRoot?: boolean;
    /** Host directory for the overlay image `upper.img`. */
    persistentRootDir?: string;
  }

  export class AgentVM {
    constructor(options?: AgentVMOptions);
    start(): Promise<void>;
    stop(): Promise<void>;
    exec(command: string): Promise<AgentVMResult>;
  }

  /** Copy a raw RGBA frame into a target buffer (framebuffer/console support). */
  export function blitFrame(target: Uint8Array, targetStride: number, frame: unknown): void;
}
