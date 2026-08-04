import { spawn, spawnSync } from 'child_process';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readlinkSync, realpathSync, rmSync } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import { AgentVMManager, } from './agentvm-manager.js';
const ABORT_FORCE_KILL_MS = 500;
const EXIT_STDIO_GRACE_MS = 25;
const DEFAULT_HOST_SHELL_IDLE_TIMEOUT_MS = 180_000;
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
];
const DEFAULT_BWRAP_REQUIRED_HOST_FILES = ['/etc/resolv.conf'];
export class HostShellManager {
    workspacePath;
    kind = 'host';
    strategy;
    constructor(workspacePath, hostConfig = {}) {
        this.workspacePath = workspacePath;
        this.strategy = resolveHostShellStrategy({ hostConfig });
    }
    async exec(command, signal, observer) {
        if (signal?.aborted) {
            throw abortError(signal.reason);
        }
        return new Promise((resolve, reject) => {
            const idleTimeoutMs = getHostShellIdleTimeoutMs();
            const launchPlan = buildHostShellLaunchPlan(this.strategy, command, this.workspacePath);
            const child = spawn(launchPlan.executable, launchPlan.args, {
                cwd: this.workspacePath,
                env: launchPlan.env,
                detached: process.platform !== 'win32',
            });
            observer?.onStart?.({
                command,
                pid: child.pid,
                backend: this.kind,
                backendLabel: this.strategy.backendLabel,
            });
            let stdout = '';
            let stderr = '';
            let exitCode = null;
            let exitSignal = null;
            let timedOut = false;
            let idleTimer = null;
            const resetIdleTimer = () => {
                if (idleTimeoutMs <= 0 || settled || timedOut) {
                    return;
                }
                if (idleTimer) {
                    clearTimeout(idleTimer);
                }
                idleTimer = setTimeout(() => {
                    if (settled || signal?.aborted) {
                        return;
                    }
                    timedOut = true;
                    exitCode = 124;
                    stderr = stderr
                        ? `${stderr}${stderr.endsWith('\n') ? '' : '\n'}Command timed out after ${idleTimeoutMs}ms without output`
                        : `Command timed out after ${idleTimeoutMs}ms without output`;
                    terminateChildProcessTree(child, 'SIGTERM');
                    if (!forceKillTimer) {
                        forceKillTimer = setTimeout(() => {
                            if (child.exitCode === null && child.signalCode === null) {
                                terminateChildProcessTree(child, 'SIGKILL');
                            }
                        }, ABORT_FORCE_KILL_MS);
                        forceKillTimer.unref?.();
                    }
                }, idleTimeoutMs);
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
            let forceKillTimer = null;
            let exitGraceTimer = null;
            let settled = false;
            const maybeDestroyStreams = () => {
                child.stdout.destroy?.();
                child.stderr.destroy?.();
            };
            const buildResult = () => {
                const resolvedExitCode = typeof exitCode === 'number'
                    ? exitCode
                    : (timedOut ? 124 : 1);
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
                            ? `Command timed out after ${idleTimeoutMs}ms without output via ${launchPlan.strategy.backendLabel}`
                            : (stderr
                                ? `Command failed via ${launchPlan.strategy.backendLabel}: ${stderr}`
                                : `Command failed via ${launchPlan.strategy.backendLabel} with exit code ${resolvedExitCode}${signalSuffix}`)),
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
            const finalizeReject = (error) => {
                if (settled) {
                    return;
                }
                settled = true;
                cleanupScratchDir(launchPlan.scratchDir);
                cleanup();
                reject(error);
            };
            const finalizeResolve = (result) => {
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
            child.once('error', (error) => {
                if (signal?.aborted) {
                    finalizeReject(abortError(signal.reason));
                    return;
                }
                finalizeReject(error);
            });
            child.once('exit', (code, closeSignal) => {
                exitCode = typeof code === 'number' ? code : 1;
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
                exitCode = typeof code === 'number' ? code : (exitCode ?? 1);
                exitSignal = closeSignal ?? exitSignal;
                if (signal?.aborted) {
                    finalizeReject(abortError(signal.reason));
                    return;
                }
                finalizeExit(false);
            });
        });
    }
    async dispose() {
        // No persistent process to clean up for host-shell execution.
    }
}
export function createShellManager(options) {
    if (options.sandbox) {
        return new AgentVMManager(options.workspacePath, options.network ?? false);
    }
    return new HostShellManager(options.workspacePath, options.hostConfig);
}
export function describeShellExecutionBackend(sandbox = false, hostConfig = {}) {
    if (sandbox) {
        return {
            backendLabel: 'sandbox[agentvm]',
            description: 'AgentVM sandbox',
        };
    }
    const strategy = resolveHostShellStrategy({ hostConfig });
    return {
        backendLabel: strategy.backendLabel,
        description: strategy.description,
    };
}
export function resolveHostShellStrategy(options = {}) {
    const { hostConfig = {}, platform = process.platform, probes = defaultHostShellWrapperProbes, envOverride = process.env[HOST_SHELL_WRAPPER_OVERRIDE_ENV], } = options;
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
export function resolveHostShellWrapperKind(options = {}) {
    const { platform = process.platform, probes = defaultHostShellWrapperProbes, wrapperPreference = 'auto', envOverride = process.env[HOST_SHELL_WRAPPER_OVERRIDE_ENV], strictIsolation = false, } = options;
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
function normalizeHostShellWrapperPreference(value) {
    const normalized = value?.trim().toLowerCase();
    if (!normalized || normalized === 'auto') {
        return null;
    }
    if (normalized === 'clean-room' || normalized === 'bwrap' || normalized === 'sandbox-exec') {
        return normalized;
    }
    return null;
}
const defaultHostShellWrapperProbes = {
    isBwrapUsable: (strictIsolation) => resolveBwrapExecWrapperMode(strictIsolation) !== null,
    resolveBwrapExecWrapperMode,
    isSandboxExecUsable: () => commandSucceeds('sandbox-exec', ['-p', '(version 1) (allow default)', '/usr/bin/true']),
};
function commandSucceeds(command, args) {
    return probeCommand(command, args).success;
}
function resolveBwrapExecWrapperMode(strictIsolation) {
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
function buildBwrapProbeArgs(strictIsolation) {
    return [
        '--unshare-user-try',
        ...(strictIsolation ? ['--unshare-net'] : []),
        '--ro-bind', '/', '/',
        '/bin/true',
    ];
}
function canUseSetprivWrapper() {
    const executable = resolveExecutable('setpriv');
    return executable.includes('/') && pathExists(executable);
}
function hasUidMapPermissionDenied(result) {
    return [result.sanitized, result.inherited].some((probeResult) => {
        const stderr = typeof probeResult.stderr === 'string' ? probeResult.stderr : '';
        return stderr.includes('setting up uid map: Permission denied');
    });
}
function probeCommand(command, args) {
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
function buildHostShellLaunchPlan(strategy, command, workspacePath) {
    const scratchDir = createScratchDir();
    const env = buildRestrictedEnv(scratchDir, workspacePath);
    switch (strategy.wrapperKind) {
        case 'bwrap':
            if (strategy.bwrapExecWrapperMode === 'setpriv-no-new-privs') {
                return {
                    executable: resolveExecutable('setpriv'),
                    args: ['--no-new-privs', resolveExecutable('bwrap'), ...buildBwrapArgs(command, workspacePath, scratchDir, env, strategy)],
                    env,
                    strategy,
                    scratchDir,
                };
            }
            return {
                executable: resolveExecutable('bwrap'),
                args: buildBwrapArgs(command, workspacePath, scratchDir, env, strategy),
                env,
                strategy,
                scratchDir,
            };
        case 'sandbox-exec':
            return {
                executable: resolveExecutable('sandbox-exec'),
                args: ['-p', buildSandboxExecProfile(workspacePath, scratchDir), '/bin/bash', '--noprofile', '--norc', '-c', command],
                env,
                strategy,
                scratchDir,
            };
        case 'clean-room':
        default:
            return {
                executable: resolveExecutable('bash'),
                args: ['--noprofile', '--norc', '-c', command],
                env,
                strategy,
                scratchDir,
            };
    }
}
function createScratchDir() {
    const scratchDir = mkdtempSync(join(tmpdir(), 'deepclause-shell-'));
    mkdirSync(join(scratchDir, 'tmp'), { recursive: true });
    mkdirSync(join(scratchDir, '.config'), { recursive: true });
    mkdirSync(join(scratchDir, '.cache'), { recursive: true });
    mkdirSync(join(scratchDir, '.local', 'share'), { recursive: true });
    return scratchDir;
}
function cleanupScratchDir(scratchDir) {
    try {
        rmSync(scratchDir, { recursive: true, force: true });
    }
    catch {
        // Best-effort cleanup only.
    }
}
function buildRestrictedEnv(scratchDir, workspacePath) {
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
        PWD: workspacePath,
    };
}
function buildHostProbeEnv() {
    return {
        PATH: getDefaultHostPathValue(),
        HOME: process.env.HOME ?? tmpdir(),
        LANG: 'C.UTF-8',
        LC_ALL: 'C.UTF-8',
        ...(process.env.XDG_RUNTIME_DIR ? { XDG_RUNTIME_DIR: process.env.XDG_RUNTIME_DIR } : {}),
    };
}
function buildBwrapArgs(command, workspacePath, scratchDir, env, strategy) {
    const createdMountDirs = new Set();
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
    for (const directoryPath of buildMountParentDirs(workspacePath)) {
        if (createdMountDirs.has(directoryPath)) {
            continue;
        }
        args.push('--dir', directoryPath);
        createdMountDirs.add(directoryPath);
    }
    for (const directoryPath of buildMountParentDirs(scratchDir)) {
        if (createdMountDirs.has(directoryPath)) {
            continue;
        }
        args.push('--dir', directoryPath);
        createdMountDirs.add(directoryPath);
    }
    args.push('--bind', workspacePath, workspacePath);
    args.push('--bind', scratchDir, scratchDir);
    //args.push('--tmpfs', '/tmp');
    args.push('--chdir', workspacePath);
    for (const [key, value] of Object.entries(env)) {
        if (typeof value === 'string') {
            args.push('--setenv', key, value);
        }
    }
    args.push('/bin/bash', '--noprofile', '--norc', '-c', command);
    return args;
}
function buildMountParentDirs(targetPath) {
    const segments = targetPath.split('/').filter(Boolean);
    const directories = [];
    let current = '';
    for (let index = 0; index < segments.length - 1; index += 1) {
        current += `/${segments[index]}`;
        directories.push(current);
    }
    return directories;
}
function buildSandboxExecProfile(workspacePath, scratchDir) {
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
function escapeSandboxPath(value) {
    return value.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
}
function pathExists(value) {
    return existsSync(value);
}
function getAdditionalBwrapReadOnlyPaths() {
    const requiredPaths = new Set();
    for (const requiredHostFile of DEFAULT_BWRAP_REQUIRED_HOST_FILES) {
        const resolvedPath = resolveHostRealPathOutsideBwrapRoots(requiredHostFile);
        if (!resolvedPath) {
            continue;
        }
        requiredPaths.add(resolvedPath);
    }
    return [...requiredPaths];
}
function resolveHostRealPathOutsideBwrapRoots(filePath) {
    if (!pathExists(filePath)) {
        return null;
    }
    try {
        const resolvedPath = realpathSync(filePath);
        if (resolvedPath === filePath || isPathCoveredByBwrapReadOnlyRoots(resolvedPath)) {
            return null;
        }
        return resolvedPath;
    }
    catch {
        return null;
    }
}
function isPathCoveredByBwrapReadOnlyRoots(targetPath) {
    return DEFAULT_BWRAP_READ_ONLY_PATHS.some((rootPath) => targetPath === rootPath || targetPath.startsWith(`${rootPath}/`));
}
function resolveExecutable(command) {
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
function getHostExecutableLookupDirs() {
    const envPath = process.env.PATH ?? '';
    const envDirectories = envPath
        .split(':')
        .map((value) => value.trim())
        .filter(Boolean);
    return [...new Set([...envDirectories, ...DEFAULT_HOST_EXECUTABLE_LOOKUP_DIRS])];
}
function getDefaultHostPathValue() {
    return process.platform === 'darwin'
        ? '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/homebrew/bin'
        : '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin';
}
function getHostShellIdleTimeoutMs() {
    const raw = process.env.DC_HOST_SHELL_IDLE_TIMEOUT_MS;
    if (raw != null) {
        const parsed = Number.parseInt(raw, 10);
        if (Number.isFinite(parsed) && parsed >= 0) {
            return parsed;
        }
    }
    return DEFAULT_HOST_SHELL_IDLE_TIMEOUT_MS;
}
function isHostShellDebugEnabled() {
    const value = process.env[HOST_SHELL_DEBUG_ENV]?.trim().toLowerCase();
    return value === '1' || value === 'true' || value === 'yes';
}
function logHostShellDebug(message, details) {
    if (!isHostShellDebugEnabled()) {
        return;
    }
    process.stderr.write(`[deepclause:shell] ${message} ${JSON.stringify(details)}\n`);
}
function summarizeProbeResult(result) {
    return {
        status: result.status,
        signal: result.signal,
        error: result.error
            ? {
                name: result.error.name,
                message: result.error.message,
                ...(typeof result.error.code === 'string'
                    ? { code: result.error.code }
                    : {}),
            }
            : null,
        stdout: typeof result.stdout === 'string' ? result.stdout.trim() : '',
        stderr: typeof result.stderr === 'string' ? result.stderr.trim() : '',
    };
}
function logHostShellProbeFailureDiagnostics(command, result, envKind) {
    if (!isHostShellDebugEnabled() || command !== 'bwrap' || (result.status === 0 && !result.error)) {
        return;
    }
    logHostShellDebug('probe process diagnostics', {
        command,
        envKind,
        process: getCurrentProcessDiagnostics(),
    });
}
function getCurrentProcessDiagnostics() {
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
function readProcStatusFields(fieldNames) {
    try {
        const requestedFields = new Set(fieldNames);
        const lines = readFileSync('/proc/self/status', 'utf8').split('\n');
        const values = Object.fromEntries(fieldNames.map((name) => [name, null]));
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
    }
    catch {
        return Object.fromEntries(fieldNames.map((name) => [name, null]));
    }
}
function readNamespaceLink(name) {
    try {
        return readlinkSync(`/proc/self/ns/${name}`);
    }
    catch {
        return null;
    }
}
function abortError(reason) {
    return reason instanceof Error
        ? reason
        : Object.assign(new Error('This operation was aborted'), { name: 'AbortError' });
}
function terminateChildProcessTree(child, signal) {
    if (typeof child.pid !== 'number' || child.pid <= 0) {
        return;
    }
    if (process.platform !== 'win32') {
        try {
            process.kill(-child.pid, signal);
            return;
        }
        catch {
            // Fall back to the direct child when process groups are unavailable.
        }
    }
    try {
        child.kill(signal);
    }
    catch {
        // Ignore termination races during cancellation.
    }
}
//# sourceMappingURL=shell-manager.js.map