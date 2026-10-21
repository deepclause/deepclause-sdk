import { spawn, spawnSync, type ChildProcess } from 'child_process';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readlinkSync, realpathSync, rmSync } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import {
  AgentVMManager,
  type ShellExecObserver,
  type ShellExecOptions,
  type ShellExecResult,
} from './agentvm-manager.js';
import { getHostShellIdleTimeoutMs, resolveShellTimeoutMs } from './shell-limits.js';

const ABORT_FORCE_KILL_MS = 500;
const EXIT_STDIO_GRACE_MS = 25;
const HOST_SHELL_WRAPPER_OVERRIDE_ENV = 'DC_HOST_SHELL_WRAPPER';
const HOST_SHELL_DEBUG_ENV = 'DC_HOST_SHELL_DEBUG';
const DEFAULT_HOST_EXECUTABLE_LOOKUP_DIRS = process.platform === 'darwin'
  ? ['/usr/bin', '/bin', '/usr/sbin', '/sbin', '/usr/local/bin', '/opt/homebrew/bin']
  : ['/usr/bin', '/bin', '/usr/sbin', '/sbin', '/usr/local/bin'];
const DEFAULT_BWRAP_READ_ONLY_PATHS = [
  '/bin',
  '/usr',
  '/sbin',
  '/lib',
  '/lib64',
  '/etc',
  '/usr/local',
  '/opt',
  '/nix',
  '/run/current-system',
] as const;
const DEFAULT_BWRAP_REQUIRED_HOST_FILES = ['/etc/resolv.conf'] as const;

export type HostShellWrapperKind = 'clean-room' | 'bwrap' | 'sandbox-exec';
export type HostShellWrapperPreference = 'auto' | HostShellWrapperKind;
export type HostShellBwrapExecWrapperMode = 'direct' | 'setpriv-no-new-privs';

export interface HostShellConfig {
  wrapper?: HostShellWrapperPreference;
  strictIsolation?: boolean;
  /** Wall-clock timeout for a command, in ms; overrides the env default. */
  timeoutMs?: number;
  /** Idle (no output) timeout for a command, in ms; overrides the env default. */
  idleTimeoutMs?: number;
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

interface HostShellLaunchPlan {
  executable: string;
  args: string[];
  env: NodeJS.ProcessEnv;
  strategy: ResolvedHostShellStrategy;
  scratchDir: string;
}

export interface ShellManager {
  readonly kind: 'host' | 'sandbox';
  exec(command: string, signal?: AbortSignal, observer?: ShellExecObserver, options?: ShellExecOptions): Promise<ShellExecResult>;
  dispose(): Promise<void>;
}

export interface CreateShellManagerOptions {
  workspacePath: string;
  sandbox?: boolean;
  network?: boolean;
  hostConfig?: HostShellConfig;
}

export class HostShellManager implements ShellManager {
  readonly kind = 'host' as const;
  private readonly strategy: ResolvedHostShellStrategy;

  constructor(
    private readonly workspacePath: string,
    private readonly hostConfig: HostShellConfig = {},
  ) {
    this.strategy = resolveHostShellStrategy({ hostConfig });
  }

  async exec(
    command: string,
    signal?: AbortSignal,
    observer?: ShellExecObserver,
    options?: ShellExecOptions,
  ): Promise<ShellExecResult> {
    if (signal?.aborted) {
      throw abortError(signal.reason);
    }

    return new Promise((resolve, reject) => {
      const startedAt = Date.now();
      const idleTimeoutMs = typeof this.hostConfig.idleTimeoutMs === 'number' && this.hostConfig.idleTimeoutMs >= 0
        ? this.hostConfig.idleTimeoutMs
        : getHostShellIdleTimeoutMs();
      const wallTimeoutMs = typeof options?.timeoutMs === 'number' && options.timeoutMs >= 0
        ? options.timeoutMs
        : (typeof this.hostConfig.timeoutMs === 'number' && this.hostConfig.timeoutMs >= 0
          ? this.hostConfig.timeoutMs
          : resolveShellTimeoutMs());
      const launchPlan = buildHostShellLaunchPlan(this.strategy, command, this.workspacePath);
      const child = spawn(launchPlan.executable, launchPlan.args, {
        cwd: this.workspacePath,
        env: launchPlan.env,
        detached: process.platform !== 'win32',
        // stdin is /dev/null, so a command that reads it sees EOF instead of a
        // pipe nobody writes to. Without this, `cat` (or anything that reads
        // stdin) blocks until the idle timeout kills it — a runtime with no
        // stdin is not a reason to hang for 30 seconds.
        stdio: ['ignore', 'pipe', 'pipe'],
      });

      observer?.onStart?.({
        command,
        pid: child.pid,
        backend: this.kind,
        backendLabel: this.strategy.backendLabel,
      });

      let stdout = '';
      let stderr = '';
      let exitCode: number | null = null;
      let exitSignal: NodeJS.Signals | null = null;
      let timedOut = false;
      let timeoutKind: 'idle' | 'wall' | null = null;
      let idleTimer: ReturnType<typeof setTimeout> | null = null;
      let wallTimer: ReturnType<typeof setTimeout> | null = null;

      /** Shared kill path for the idle and wall-clock timers. */
      const fireTimeout = (kind: 'idle' | 'wall'): void => {
        if (settled || signal?.aborted || timedOut) {
          return;
        }
        timedOut = true;
        timeoutKind = kind;
        exitCode = 124;
        const message = kind === 'idle'
          ? `Command timed out after ${idleTimeoutMs}ms without output`
          : `Command timed out after ${wallTimeoutMs}ms`;
        stderr = stderr ? `${stderr}${stderr.endsWith('\n') ? '' : '\n'}${message}` : message;

        terminateChildProcessTree(child, 'SIGTERM');
        if (!forceKillTimer) {
          forceKillTimer = setTimeout(() => {
            if (child.exitCode === null && child.signalCode === null) {
              terminateChildProcessTree(child, 'SIGKILL');
            }
          }, ABORT_FORCE_KILL_MS);
          forceKillTimer.unref?.();
        }
      };

      const resetIdleTimer = () => {
        if (idleTimeoutMs <= 0 || settled || timedOut) {
          return;
        }
        if (idleTimer) {
          clearTimeout(idleTimer);
        }
        idleTimer = setTimeout(() => fireTimeout('idle'), idleTimeoutMs);
        idleTimer.unref?.();
      };

      child.stdout.setEncoding('utf8');
      child.stderr.setEncoding('utf8');
      child.stdout.on('data', (chunk) => {
        stdout += chunk;
        resetIdleTimer();
        observer?.onStdout?.({
          command,
          chunk,
          pid: child.pid,
          backend: this.kind,
          backendLabel: this.strategy.backendLabel,
        });
      });
      child.stderr.on('data', (chunk) => {
        stderr += chunk;
        resetIdleTimer();
        observer?.onStderr?.({
          command,
          chunk,
          pid: child.pid,
          backend: this.kind,
          backendLabel: this.strategy.backendLabel,
        });
      });

      let forceKillTimer: ReturnType<typeof setTimeout> | null = null;
      let exitGraceTimer: ReturnType<typeof setTimeout> | null = null;
      let settled = false;
      const maybeDestroyStreams = () => {
        child.stdout.destroy?.();
        child.stderr.destroy?.();
      };
      const buildResult = (): ShellExecResult => {
        const resolvedExitCode = timedOut
          ? 124
          : (typeof exitCode === 'number' ? exitCode : 1);
        const signalSuffix = exitSignal ? ` (signal ${exitSignal})` : '';
        return {
          success: resolvedExitCode === 0,
          stdout,
          stderr,
          exitCode: resolvedExitCode,
          pid: child.pid,
          backend: this.kind,
          backendLabel: launchPlan.strategy.backendLabel,
          summary: resolvedExitCode === 0
            ? `Command completed successfully via ${launchPlan.strategy.backendLabel}`
            : (timedOut
              ? `${
                timeoutKind === 'idle'
                  ? `Command timed out after ${idleTimeoutMs}ms without output`
                  : `Command timed out after ${wallTimeoutMs}ms`
              } via ${launchPlan.strategy.backendLabel}`
              : (stderr
                ? `Command failed via ${launchPlan.strategy.backendLabel}: ${stderr}`
                : `Command failed via ${launchPlan.strategy.backendLabel} with exit code ${resolvedExitCode}${signalSuffix}`)),
          timeMs: Date.now() - startedAt,
        };
      };
      const finalizeExit = (detachStreams = false) => {
        if (detachStreams) {
          maybeDestroyStreams();
        }
        const result = buildResult();
        observer?.onExit?.({
          command,
          pid: child.pid,
          backend: this.kind,
          backendLabel: result.backendLabel,
          success: result.success,
          exitCode: result.exitCode,
          summary: result.summary,
        });
        finalizeResolve(result);
      };
      const finalizeReject = (error: unknown) => {
        if (settled) {
          return;
        }
        settled = true;
        cleanupScratchDir(launchPlan.scratchDir);
        cleanup();
        reject(error);
      };
      const finalizeResolve = (result: ShellExecResult) => {
        if (settled) {
          return;
        }
        settled = true;
        cleanupScratchDir(launchPlan.scratchDir);
        cleanup();
        resolve(result);
      };

      const onAbort = () => {
        terminateChildProcessTree(child, 'SIGTERM');
        if (!forceKillTimer) {
          forceKillTimer = setTimeout(() => {
            if (child.exitCode === null && child.signalCode === null) {
              terminateChildProcessTree(child, 'SIGKILL');
            }
          }, ABORT_FORCE_KILL_MS);
          forceKillTimer.unref?.();
        }

        finalizeReject(abortError(signal?.reason));
      };
      const cleanup = () => {
        signal?.removeEventListener('abort', onAbort);
        if (idleTimer) {
          clearTimeout(idleTimer);
          idleTimer = null;
        }
        if (wallTimer) {
          clearTimeout(wallTimer);
          wallTimer = null;
        }
        if (forceKillTimer) {
          clearTimeout(forceKillTimer);
          forceKillTimer = null;
        }
        if (exitGraceTimer) {
          clearTimeout(exitGraceTimer);
          exitGraceTimer = null;
        }
      };
      signal?.addEventListener('abort', onAbort, { once: true });
      resetIdleTimer();
      if (wallTimeoutMs > 0) {
        wallTimer = setTimeout(() => fireTimeout('wall'), wallTimeoutMs);
        wallTimer.unref?.();
      }

      child.once('error', (error) => {
        if (signal?.aborted) {
          finalizeReject(abortError(signal.reason));
          return;
        }
        finalizeReject(error);
      });
      child.once('exit', (code, closeSignal) => {
        exitCode = timedOut ? 124 : (typeof code === 'number' ? code : 1);
        exitSignal = closeSignal;

        if (signal?.aborted || settled) {
          return;
        }

        exitGraceTimer = setTimeout(() => {
          if (!settled) {
            finalizeExit(true);
          }
        }, EXIT_STDIO_GRACE_MS);
        exitGraceTimer.unref?.();
      });
      child.once('close', (code, closeSignal) => {
        exitCode = timedOut ? 124 : (typeof code === 'number' ? code : (exitCode ?? 1));
        exitSignal = closeSignal ?? exitSignal;
        if (signal?.aborted) {
          finalizeReject(abortError(signal.reason));
          return;
        }
        finalizeExit(false);
      });
    });
  }

  async dispose(): Promise<void> {
    // No persistent process to clean up for host-shell execution.
  }
}

export function createShellManager(options: CreateShellManagerOptions): ShellManager {
  if (options.sandbox) {
    return new AgentVMManager(options.workspacePath, options.network ?? false, options.hostConfig?.timeoutMs);
  }

  return new HostShellManager(options.workspacePath, options.hostConfig);
}

/**
 * The execution backends that isolate `exec(bash(...))` from the host. `clean-room`
 * is the absence of a wrapper; it runs the command as the user, so it does not
 * qualify as a sandbox.
 */
export type ExecutionBackendKind = 'agentvm' | 'wasm' | HostShellWrapperKind;

export interface ExecutionEnvironment {
  kind: ExecutionBackendKind;
  backendLabel: string;
  description: string;
  /** True for a backend that provides filesystem isolation. */
  sandboxed: boolean;
}

/**
 * Resolve how shell commands will actually run, as data.
 *
 * One source of truth for three consumers that must agree: the harness
 * `require_sandbox` directive (checked at load against the *effective* backend,
 * so an installed-but-unused bwrap does not satisfy it), the environment block
 * put in front of the model, and the CLI status line. It was previously computed
 * but never read, so nothing told the agent whether it was sandboxed.
 */
export function describeShellExecutionBackend(
  sandbox = false,
  hostConfig: HostShellConfig = {},
): ExecutionEnvironment {
  if (sandbox) {
    return {
      kind: 'agentvm',
      backendLabel: 'sandbox[agentvm]',
      description: 'AgentVM sandbox',
      sandboxed: true,
    };
  }

  const strategy = resolveHostShellStrategy({ hostConfig });
  return {
    kind: strategy.wrapperKind,
    backendLabel: strategy.backendLabel,
    description: strategy.description,
    sandboxed: strategy.wrapperKind !== 'clean-room',
  };
}

export function resolveHostShellStrategy(options: {
  hostConfig?: HostShellConfig;
  platform?: NodeJS.Platform;
  probes?: HostShellWrapperProbes;
  envOverride?: string;
} = {}): ResolvedHostShellStrategy {
  const {
    hostConfig = {},
    platform = process.platform,
    probes = defaultHostShellWrapperProbes,
    envOverride = process.env[HOST_SHELL_WRAPPER_OVERRIDE_ENV],
  } = options;
  const strictIsolationRequested = hostConfig.strictIsolation ?? false;
  const normalizedOverride = normalizeHostShellWrapperPreference(envOverride);
  const normalizedPreference = normalizeHostShellWrapperPreference(hostConfig.wrapper);
  const shouldResolveBwrapExecWrapperMode = platform === 'linux'
    && typeof probes.resolveBwrapExecWrapperMode === 'function'
    && normalizedOverride !== 'clean-room'
    && normalizedOverride !== 'sandbox-exec'
    && !(normalizedOverride == null && normalizedPreference === 'clean-room')
    && !(normalizedOverride == null && normalizedPreference === 'sandbox-exec');
  const resolvedBwrapExecWrapperMode = shouldResolveBwrapExecWrapperMode
    ? probes.resolveBwrapExecWrapperMode?.(strictIsolationRequested)
    : undefined;
  const strategyProbes = resolvedBwrapExecWrapperMode !== undefined
    ? {
      ...probes,
      isBwrapUsable: () => resolvedBwrapExecWrapperMode !== null,
    }
    : probes;

  const wrapperKind = resolveHostShellWrapperKind({
    platform,
    probes: strategyProbes,
    wrapperPreference: hostConfig.wrapper,
    envOverride,
    strictIsolation: strictIsolationRequested,
  });
  const strictIsolation = wrapperKind === 'bwrap' && !!hostConfig.strictIsolation;
  const backendLabel = `host[${wrapperKind}${strictIsolation ? ' strict' : ''}]`;
  const bwrapExecWrapperMode = wrapperKind === 'bwrap'
    ? (resolvedBwrapExecWrapperMode ?? 'direct')
    : undefined;

  if (wrapperKind === 'bwrap') {
    return {
      wrapperKind,
      strictIsolation,
      backendLabel,
      description: strictIsolation
        ? 'bubblewrap sandbox with network disabled'
        : 'bubblewrap sandbox',
      bwrapExecWrapperMode,
    };
  }

  if (wrapperKind === 'sandbox-exec') {
    return {
      wrapperKind,
      strictIsolation: false,
      backendLabel,
      description: 'sandbox-exec host wrapper',
    };
  }

  return {
    wrapperKind,
    strictIsolation: false,
    backendLabel,
    description: hostConfig.strictIsolation
      ? 'clean-room host shell (strict isolation requested but unavailable)'
      : 'clean-room host shell',
  };
}

export function resolveHostShellWrapperKind(options: {
  platform?: NodeJS.Platform;
  probes?: HostShellWrapperProbes;
  wrapperPreference?: HostShellWrapperPreference;
  envOverride?: string;
  strictIsolation?: boolean;
} = {}): HostShellWrapperKind {
  const {
    platform = process.platform,
    probes = defaultHostShellWrapperProbes,
    wrapperPreference = 'auto',
    envOverride = process.env[HOST_SHELL_WRAPPER_OVERRIDE_ENV],
    strictIsolation = false,
  } = options;

  const normalizedOverride = normalizeHostShellWrapperPreference(envOverride);
  if (normalizedOverride) {
    logHostShellDebug('wrapper override selected', {
      platform,
      envOverride,
      wrapperPreference,
      strictIsolation,
    });
    return normalizedOverride;
  }

  const normalizedPreference = normalizeHostShellWrapperPreference(wrapperPreference);
  if (normalizedPreference) {
    logHostShellDebug('wrapper preference selected', {
      platform,
      wrapperPreference,
      strictIsolation,
    });
    return normalizedPreference;
  }

  if (platform === 'linux') {
    const bwrapUsable = probes.isBwrapUsable(strictIsolation);
    logHostShellDebug('bwrap probe completed', {
      platform,
      strictIsolation,
      usable: bwrapUsable,
    });
    if (bwrapUsable) {
      return 'bwrap';
    }
  }

  if (platform === 'darwin') {
    const sandboxExecUsable = probes.isSandboxExecUsable();
    logHostShellDebug('sandbox-exec probe completed', {
      platform,
      usable: sandboxExecUsable,
    });
    if (sandboxExecUsable) {
      return 'sandbox-exec';
    }
  }

  logHostShellDebug('wrapper fallback selected', {
    platform,
    strictIsolation,
    wrapperKind: 'clean-room',
  });
  return 'clean-room';
}

function normalizeHostShellWrapperPreference(value: string | undefined): HostShellWrapperKind | null {
  const normalized = value?.trim().toLowerCase();
  if (!normalized || normalized === 'auto') {
    return null;
  }
  if (normalized === 'clean-room' || normalized === 'bwrap' || normalized === 'sandbox-exec') {
    return normalized;
  }
  return null;
}

const defaultHostShellWrapperProbes: HostShellWrapperProbes = {
  isBwrapUsable: (strictIsolation) => resolveBwrapExecWrapperMode(strictIsolation) !== null,
  resolveBwrapExecWrapperMode,
  isSandboxExecUsable: () => commandSucceeds('sandbox-exec', ['-p', '(version 1) (allow default)', '/usr/bin/true']),
};

function commandSucceeds(command: string, args: string[]): boolean {
  return probeCommand(command, args).success;
}

function resolveBwrapExecWrapperMode(strictIsolation: boolean): HostShellBwrapExecWrapperMode | null {
  const probeArgs = buildBwrapProbeArgs(strictIsolation);
  const directProbe = probeCommand('bwrap', probeArgs);
  if (directProbe.success) {
    return 'direct';
  }

  if (!hasUidMapPermissionDenied(directProbe) || !canUseSetprivWrapper()) {
    return null;
  }

  const wrappedProbe = probeCommand('setpriv', ['--no-new-privs', resolveExecutable('bwrap'), ...probeArgs]);
  logHostShellDebug('bwrap setpriv fallback probe completed', {
    strictIsolation,
    usable: wrappedProbe.success,
  });
  return wrappedProbe.success ? 'setpriv-no-new-privs' : null;
}

function buildBwrapProbeArgs(strictIsolation: boolean): string[] {
  return [
    '--unshare-user-try',
    ...(strictIsolation ? ['--unshare-net'] : []),
    '--ro-bind', '/', '/',
    '/bin/true',
  ];
}

function canUseSetprivWrapper(): boolean {
  const executable = resolveExecutable('setpriv');
  return executable.includes('/') && pathExists(executable);
}

function hasUidMapPermissionDenied(result: {
  sanitized: ReturnType<typeof spawnSync>;
  inherited: ReturnType<typeof spawnSync>;
}): boolean {
  return [result.sanitized, result.inherited].some((probeResult) => {
    const stderr = typeof probeResult.stderr === 'string' ? probeResult.stderr : '';
    return stderr.includes('setting up uid map: Permission denied');
  });
}

function probeCommand(command: string, args: string[]): {
  success: boolean;
  sanitized: ReturnType<typeof spawnSync>;
  inherited: ReturnType<typeof spawnSync>;
} {
  const executable = resolveExecutable(command);
  const sanitized = spawnSync(executable, args, {
    cwd: '/',
    env: buildHostProbeEnv(),
    stdio: isHostShellDebugEnabled() ? 'pipe' : 'ignore',
    encoding: 'utf8',
  });
  logHostShellDebug('probe attempt', {
    command,
    executable,
    args,
    envKind: 'sanitized',
    result: summarizeProbeResult(sanitized),
  });
  logHostShellProbeFailureDiagnostics(command, sanitized, 'sanitized');
  if (sanitized.status === 0 && !sanitized.error) {
    return {
      success: true,
      sanitized,
      inherited: sanitized,
    };
  }

  const inherited = spawnSync(executable, args, {
    cwd: '/',
    env: process.env,
    stdio: isHostShellDebugEnabled() ? 'pipe' : 'ignore',
    encoding: 'utf8',
  });
  logHostShellDebug('probe attempt', {
    command,
    executable,
    args,
    envKind: 'inherited',
    result: summarizeProbeResult(inherited),
  });
  logHostShellProbeFailureDiagnostics(command, inherited, 'inherited');

  return {
    success: inherited.status === 0 && !inherited.error,
    sanitized,
    inherited,
  };
}

/**
 * The name every surface uses for the workspace: the WASM filesystem mounts it
 * there, AgentVM mounts it there, and bwrap is told to bind it there. Where a
 * mount is impossible the name is mapped onto the real directory instead.
 */
export const SANDBOX_WORKSPACE_PATH = '/workspace';

/**
 * The workspace path a command sees for this backend: `/workspace` where it is
 * mounted, and the real directory where a mount is impossible (so scripts can
 * still find it by `$WORKSPACE` and `$PWD`).
 */
function sandboxWorkspacePathFor(strategy: ResolvedHostShellStrategy, workspacePath: string): string {
  return strategy.wrapperKind === 'bwrap' ? SANDBOX_WORKSPACE_PATH : workspacePath;
}

/**
 * Rewrite workspace paths for backends that cannot mount `/workspace`.
 *
 * Only a word that *begins* with `/workspace` is a workspace path — preceded by
 * the start of the command, whitespace, or a shell metacharacter, and followed
 * by `/` or the end of the word. That keeps `/workspacex` and `/workspace-suffix`
 * untouched, and `-I/workspace/include` and `--dir=/workspace` working. A blind
 * search-and-replace would corrupt exactly the commands this exists to serve.
 *
 * This is a fallback, not the design: bwrap and AgentVM bind the real thing, so
 * launched *programs* see `/workspace` there rather than only the agent's paths.
 */
export function mapWorkspacePaths(command: string, workspacePath: string): string {
  // What may precede the name: the start of the command, whitespace, or a shell
  // metacharacter — plus a short option such as `-I/workspace/include`, which is
  // how compiler and interpreter flags are written.
  const prefix = '[\\s"\'`=(:;|&<>,@]|-[A-Za-z]+';
  // What may follow it. `-` is deliberately absent: `/workspace-suffix` is a
  // different path, not the workspace.
  const suffix = '[\\s"\'`()=:;|&<>,]';
  const pattern = new RegExp(`(^|${prefix})\\/workspace(?=\\/|$|${suffix})`, 'g');
  return command.replace(pattern, (_match, lead: string) => `${lead}${workspacePath}`);
}

function buildHostShellLaunchPlan(
  strategy: ResolvedHostShellStrategy,
  command: string,
  workspacePath: string,
): HostShellLaunchPlan {
  const scratchDir = createScratchDir();
  const sandboxWorkspacePath = sandboxWorkspacePathFor(strategy, workspacePath);
  const env = buildRestrictedEnv(scratchDir, sandboxWorkspacePath);
  // Where a mount is impossible (no bwrap, no AgentVM) the name still has to
  // work, so `/workspace` is mapped onto the real directory in the command
  // string. Where bwrap is in play the bind has already done it, and rewriting
  // as well would be wrong.
  const effectiveCommand = sandboxWorkspacePath === SANDBOX_WORKSPACE_PATH
    ? command
    : mapWorkspacePaths(command, workspacePath);

  switch (strategy.wrapperKind) {
    case 'bwrap':
      if (strategy.bwrapExecWrapperMode === 'setpriv-no-new-privs') {
        return {
          executable: resolveExecutable('setpriv'),
          args: ['--no-new-privs', resolveExecutable('bwrap'), ...buildBwrapArgs(effectiveCommand, workspacePath, scratchDir, env, strategy)],
          env,
          strategy,
          scratchDir,
        };
      }
      return {
        executable: resolveExecutable('bwrap'),
        args: buildBwrapArgs(effectiveCommand, workspacePath, scratchDir, env, strategy),
        env,
        strategy,
        scratchDir,
      };
    case 'sandbox-exec':
      return {
        executable: resolveExecutable('sandbox-exec'),
        args: ['-p', buildSandboxExecProfile(workspacePath, scratchDir), '/bin/bash', '--noprofile', '--norc', '-c', effectiveCommand],
        env,
        strategy,
        scratchDir,
      };
    case 'clean-room':
    default:
      return {
        executable: resolveExecutable('bash'),
        args: ['--noprofile', '--norc', '-c', effectiveCommand],
        env,
        strategy,
        scratchDir,
      };
  }
}

function createScratchDir(): string {
  const scratchDir = mkdtempSync(join(tmpdir(), 'deepclause-shell-'));
  mkdirSync(join(scratchDir, 'tmp'), { recursive: true });
  mkdirSync(join(scratchDir, '.config'), { recursive: true });
  mkdirSync(join(scratchDir, '.cache'), { recursive: true });
  mkdirSync(join(scratchDir, '.local', 'share'), { recursive: true });
  return scratchDir;
}

function cleanupScratchDir(scratchDir: string): void {
  try {
    rmSync(scratchDir, { recursive: true, force: true });
  } catch {
    // Best-effort cleanup only.
  }
}

function buildRestrictedEnv(
  scratchDir: string,
  sandboxWorkspacePath: string,
): NodeJS.ProcessEnv {
  const pathValue = getDefaultHostPathValue();

  return {
    PATH: pathValue,
    HOME: scratchDir,
    TMPDIR: join(scratchDir, 'tmp'),
    XDG_CONFIG_HOME: join(scratchDir, '.config'),
    XDG_CACHE_HOME: join(scratchDir, '.cache'),
    XDG_DATA_HOME: join(scratchDir, '.local', 'share'),
    LANG: 'C.UTF-8',
    LC_ALL: 'C.UTF-8',
    PWD: sandboxWorkspacePath,
    // Scripts can reach the workspace by name. `$TMPDIR` is the scratch
    // directory, which is disposable in every backend — unlike a hardcoded
    // /tmp, which is the real one when there is no sandbox.
    WORKSPACE: sandboxWorkspacePath,
  };
}

function buildHostProbeEnv(): NodeJS.ProcessEnv {
  return {
    PATH: getDefaultHostPathValue(),
    HOME: process.env.HOME ?? tmpdir(),
    LANG: 'C.UTF-8',
    LC_ALL: 'C.UTF-8',
    ...(process.env.XDG_RUNTIME_DIR ? { XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR } : {}),
  };
}

function buildBwrapArgs(
  command: string,
  workspacePath: string,
  scratchDir: string,
  env: NodeJS.ProcessEnv,
  strategy: ResolvedHostShellStrategy,
): string[] {
  const createdMountDirs = new Set<string>();
  const args = [
    '--die-with-parent',
    '--new-session',
    '--unshare-user-try',
    '--clearenv',
    '--proc', '/proc',
    '--dev', '/dev',
  ];

  if (strategy.strictIsolation) {
    args.push('--unshare-net');
  }

  for (const systemPath of DEFAULT_BWRAP_READ_ONLY_PATHS) {
    if (pathExists(systemPath)) {
      args.push('--ro-bind', systemPath, systemPath);
    }
  }

  for (const requiredHostPath of getAdditionalBwrapReadOnlyPaths()) {
    for (const directoryPath of buildMountParentDirs(requiredHostPath)) {
      if (createdMountDirs.has(directoryPath)) {
        continue;
      }
      args.push('--dir', directoryPath);
      createdMountDirs.add(directoryPath);
    }
    args.push('--ro-bind', requiredHostPath, requiredHostPath);
  }

  for (const directoryPath of buildMountParentDirs(scratchDir)) {
    if (createdMountDirs.has(directoryPath)) {
      continue;
    }
    args.push('--dir', directoryPath);
    createdMountDirs.add(directoryPath);
  }

  // The workspace is presented as /workspace, not at its host path. Every other
  // surface already calls it that — the WASM filesystem mounts it at /workspace
  // (src/prolog/loader.ts), AgentVM mounts it at /workspace — so this is what
  // makes one name true everywhere, and it stops the host path leaking into the
  // sandbox. `--chdir` matches, so a relative path means the same thing too.
  args.push('--dir', '/workspace');
  args.push('--bind', workspacePath, SANDBOX_WORKSPACE_PATH);
  args.push('--bind', scratchDir, scratchDir);
  //args.push('--tmpfs', '/tmp');
  args.push('--chdir', SANDBOX_WORKSPACE_PATH);

  for (const [key, value] of Object.entries(env)) {
    if (typeof value === 'string') {
      args.push('--setenv', key, value);
    }
  }

  args.push('/bin/bash', '--noprofile', '--norc', '-c', command);
  return args;
}

function buildMountParentDirs(targetPath: string): string[] {
  const segments = targetPath.split('/').filter(Boolean);
  const directories: string[] = [];
  let current = '';
  for (let index = 0; index < segments.length - 1; index += 1) {
    current += `/${segments[index]}`;
    directories.push(current);
  }
  return directories;
}

function buildSandboxExecProfile(workspacePath: string, scratchDir: string): string {
  const allowedWritablePaths = [workspacePath, scratchDir].map(escapeSandboxPath);
  return [
    '(version 1)',
    '(deny default)',
    '(import "system.sb")',
    '(allow process*)',
    '(allow network-outbound)',
    '(allow file-read* (subpath "/bin") (subpath "/usr") (subpath "/System") (subpath "/Library") (subpath "/dev"))',
    `(allow file-read* file-write* ${allowedWritablePaths.map((value) => `(subpath "${value}")`).join(' ')})`,
  ].join(' ');
}

function escapeSandboxPath(value: string): string {
  return value.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
}

function pathExists(value: string): boolean {
  return existsSync(value);
}

function getAdditionalBwrapReadOnlyPaths(): string[] {
  const requiredPaths = new Set<string>();

  for (const requiredHostFile of DEFAULT_BWRAP_REQUIRED_HOST_FILES) {
    const resolvedPath = resolveHostRealPathOutsideBwrapRoots(requiredHostFile);
    if (!resolvedPath) {
      continue;
    }
    requiredPaths.add(resolvedPath);
  }

  return [...requiredPaths];
}

function resolveHostRealPathOutsideBwrapRoots(filePath: string): string | null {
  if (!pathExists(filePath)) {
    return null;
  }

  try {
    const resolvedPath = realpathSync(filePath);
    if (resolvedPath === filePath || isPathCoveredByBwrapReadOnlyRoots(resolvedPath)) {
      return null;
    }
    return resolvedPath;
  } catch {
    return null;
  }
}

function isPathCoveredByBwrapReadOnlyRoots(targetPath: string): boolean {
  return DEFAULT_BWRAP_READ_ONLY_PATHS.some((rootPath) => targetPath === rootPath || targetPath.startsWith(`${rootPath}/`));
}

function resolveExecutable(command: string): string {
  if (command.includes('/')) {
    return command;
  }

  for (const directoryPath of getHostExecutableLookupDirs()) {
    const candidate = join(directoryPath, command);
    if (pathExists(candidate)) {
      return candidate;
    }
  }

  return command;
}

function getHostExecutableLookupDirs(): string[] {
  const envPath = process.env.PATH ?? '';
  const envDirectories = envPath
    .split(':')
    .map((value) => value.trim())
    .filter(Boolean);

  return [...new Set([...envDirectories, ...DEFAULT_HOST_EXECUTABLE_LOOKUP_DIRS])];
}

function getDefaultHostPathValue(): string {
  return process.platform === 'darwin'
    ? '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/homebrew/bin'
    : '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin';
}

function isHostShellDebugEnabled(): boolean {
  const value = process.env[HOST_SHELL_DEBUG_ENV]?.trim().toLowerCase();
  return value === '1' || value === 'true' || value === 'yes';
}

function logHostShellDebug(message: string, details: Record<string, unknown>): void {
  if (!isHostShellDebugEnabled()) {
    return;
  }

  process.stderr.write(`[deepclause:shell] ${message} ${JSON.stringify(details)}\n`);
}

function summarizeProbeResult(result: ReturnType<typeof spawnSync>): Record<string, unknown> {
  return {
    status: result.status,
    signal: result.signal,
    error: result.error
      ? {
        name: result.error.name,
        message: result.error.message,
        ...(typeof (result.error as NodeJS.ErrnoException).code === 'string'
          ? { code: (result.error as NodeJS.ErrnoException).code }
          : {}),
      }
      : null,
    stdout: typeof result.stdout === 'string' ? result.stdout.trim() : '',
    stderr: typeof result.stderr === 'string' ? result.stderr.trim() : '',
  };
}

function logHostShellProbeFailureDiagnostics(
  command: string,
  result: ReturnType<typeof spawnSync>,
  envKind: 'sanitized' | 'inherited',
): void {
  if (!isHostShellDebugEnabled() || command !== 'bwrap' || (result.status === 0 && !result.error)) {
    return;
  }

  logHostShellDebug('probe process diagnostics', {
    command,
    envKind,
    process: getCurrentProcessDiagnostics(),
  });
}

function getCurrentProcessDiagnostics(): Record<string, unknown> {
  const status = readProcStatusFields([
    'NoNewPrivs',
    'Seccomp',
    'Seccomp_filters',
    'Uid',
    'Gid',
    'Groups',
    'CapInh',
    'CapPrm',
    'CapEff',
    'CapBnd',
    'CapAmb',
  ]);

  return {
    pid: process.pid,
    ppid: process.ppid,
    cwd: process.cwd(),
    status,
    namespaces: {
      user: readNamespaceLink('user'),
      pid: readNamespaceLink('pid'),
      mnt: readNamespaceLink('mnt'),
    },
  };
}

function readProcStatusFields(fieldNames: string[]): Record<string, string | null> {
  try {
    const requestedFields = new Set(fieldNames);
    const lines = readFileSync('/proc/self/status', 'utf8').split('\n');
    const values: Record<string, string | null> = Object.fromEntries(fieldNames.map((name) => [name, null]));

    for (const line of lines) {
      const separatorIndex = line.indexOf(':');
      if (separatorIndex === -1) {
        continue;
      }
      const name = line.slice(0, separatorIndex);
      if (!requestedFields.has(name)) {
        continue;
      }
      values[name] = line.slice(separatorIndex + 1).trim();
    }

    return values;
  } catch {
    return Object.fromEntries(fieldNames.map((name) => [name, null]));
  }
}

function readNamespaceLink(name: 'user' | 'pid' | 'mnt'): string | null {
  try {
    return readlinkSync(`/proc/self/ns/${name}`);
  } catch {
    return null;
  }
}

function abortError(reason: unknown): Error {
  return reason instanceof Error
    ? reason
    : Object.assign(new Error('This operation was aborted'), { name: 'AbortError' });
}

function terminateChildProcessTree(child: ChildProcess, signal: NodeJS.Signals): void {
  if (typeof child.pid !== 'number' || child.pid <= 0) {
    return;
  }

  if (process.platform !== 'win32') {
    try {
      process.kill(-child.pid, signal);
      return;
    } catch {
      // Fall back to the direct child when process groups are unavailable.
    }
  }

  try {
    child.kill(signal);
  } catch {
    // Ignore termination races during cancellation.
  }
}