import { EventEmitter } from 'events';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const childProcessMocks = vi.hoisted(() => ({
  spawn: vi.fn(),
  spawnSync: vi.fn(),
}));

vi.mock('child_process', () => ({
  spawn: childProcessMocks.spawn,
  spawnSync: childProcessMocks.spawnSync,
}));

import { HostShellManager, resolveHostShellStrategy, resolveHostShellWrapperKind } from '../src/system/runtime/shell-manager.js';

describe('HostShellManager cancellation', () => {
  beforeEach(() => {
    childProcessMocks.spawnSync.mockReset();
    childProcessMocks.spawnSync.mockReturnValue({ status: 1, error: undefined });
  });

  afterEach(() => {
    delete process.env.DC_HOST_SHELL_IDLE_TIMEOUT_MS;
    delete process.env.DC_HOST_SHELL_WRAPPER;
    delete process.env.DEEPCLAUSE_TEST_SECRET;
    vi.useRealTimers();
    vi.restoreAllMocks();
    childProcessMocks.spawn.mockReset();
  });

  it('defaults to clean-room host execution and scrubs ambient env vars', async () => {
    const child = createMockChildProcess(3210);
    childProcessMocks.spawn.mockReturnValue(child);
    process.env.DEEPCLAUSE_TEST_SECRET = 'should-not-leak';

    const manager = new HostShellManager('/tmp/workspace');
    const execPromise = manager.exec('printf hello');

    child.emit('close', 0, null);

    await expect(execPromise).resolves.toMatchObject({
      success: true,
      exitCode: 0,
      pid: 3210,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      summary: 'Command completed successfully via host[clean-room]',
    });

    const [, args, options] = childProcessMocks.spawn.mock.calls[0];
    expect(args).toEqual(['--noprofile', '--norc', '-c', 'printf hello']);
    expect(options).toEqual(expect.objectContaining({
      cwd: '/tmp/workspace',
      detached: true,
      env: expect.objectContaining({
        PWD: '/tmp/workspace',
      }),
    }));
    expect(options.env).not.toHaveProperty('DEEPCLAUSE_TEST_SECRET');
  });

  it('starts host commands in a separate process group and kills the whole group on abort', async () => {
    const child = createMockChildProcess(4321);
    childProcessMocks.spawn.mockReturnValue(child);
    const killSpy = vi.spyOn(process, 'kill').mockImplementation(() => true as never);
    const manager = new HostShellManager('/tmp/workspace');
    const controller = new AbortController();

    const execPromise = manager.exec('sleep 10', controller.signal);
    controller.abort();

    await expect(execPromise).rejects.toMatchObject({ name: 'AbortError' });
    expect(childProcessMocks.spawn).toHaveBeenCalledWith(expect.stringMatching(/(^|\/)bash$/), ['--noprofile', '--norc', '-c', 'sleep 10'], expect.objectContaining({
      cwd: '/tmp/workspace',
      detached: true,
    }));
    expect(killSpy).toHaveBeenCalledWith(-4321, 'SIGTERM');
  });

  it('reports shell start, output, and exit events to observers', async () => {
    const child = createMockChildProcess(9876);
    childProcessMocks.spawn.mockReturnValue(child);
    const manager = new HostShellManager('/tmp/workspace');
    const onStart = vi.fn();
    const onStdout = vi.fn();
    const onStderr = vi.fn();
    const onExit = vi.fn();

    const execPromise = manager.exec('printf hello', undefined, {
      onStart,
      onStdout,
      onStderr,
      onExit,
    });

    child.stdout.emit('data', 'hello');
    child.stderr.emit('data', 'warn');
    child.emit('exit', 0, null);
    child.emit('close', 0, null);

    await expect(execPromise).resolves.toEqual({
      success: true,
      stdout: 'hello',
      stderr: 'warn',
      exitCode: 0,
      pid: 9876,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      summary: 'Command completed successfully via host[clean-room]',
      timeMs: expect.any(Number),
    });

    expect(onStart).toHaveBeenCalledWith({
      command: 'printf hello',
      pid: 9876,
      backend: 'host',
      backendLabel: 'host[clean-room]',
    });
    expect(onStdout).toHaveBeenCalledWith({
      command: 'printf hello',
      chunk: 'hello',
      pid: 9876,
      backend: 'host',
      backendLabel: 'host[clean-room]',
    });
    expect(onStderr).toHaveBeenCalledWith({
      command: 'printf hello',
      chunk: 'warn',
      pid: 9876,
      backend: 'host',
      backendLabel: 'host[clean-room]',
    });
    expect(onExit).toHaveBeenCalledWith({
      command: 'printf hello',
      pid: 9876,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      success: true,
      exitCode: 0,
      summary: 'Command completed successfully via host[clean-room]',
    });
  });

  it('resolves once the shell exits even when a background child keeps stdio open', async () => {
    vi.useFakeTimers();

    const child = createMockChildProcess(2468);
    childProcessMocks.spawn.mockReturnValue(child);
    const onExit = vi.fn();
    const manager = new HostShellManager('/tmp/workspace');

    const execPromise = manager.exec('python3 -m http.server 8080 &', undefined, { onExit });

    child.stdout.emit('data', 'Server started\n');
    child.emit('exit', 0, null);

    await vi.advanceTimersByTimeAsync(30);

    await expect(execPromise).resolves.toEqual({
      success: true,
      stdout: 'Server started\n',
      stderr: '',
      exitCode: 0,
      pid: 2468,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      summary: 'Command completed successfully via host[clean-room]',
      timeMs: expect.any(Number),
    });

    expect(child.stdout.destroy).toHaveBeenCalledTimes(1);
    expect(child.stderr.destroy).toHaveBeenCalledTimes(1);
    expect(onExit).toHaveBeenCalledWith({
      command: 'python3 -m http.server 8080 &',
      pid: 2468,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      success: true,
      exitCode: 0,
      summary: 'Command completed successfully via host[clean-room]',
    });

    vi.useRealTimers();
  });

  it('still captures trailing stdout when close follows exit quickly', async () => {
    vi.useFakeTimers();

    const child = createMockChildProcess(1357);
    childProcessMocks.spawn.mockReturnValue(child);
    const manager = new HostShellManager('/tmp/workspace');

    const execPromise = manager.exec('printf hello', undefined);

    child.stdout.emit('data', 'hello');
    child.emit('exit', 0, null);
    child.stdout.emit('data', ' world');
    child.emit('close', 0, null);

    await expect(execPromise).resolves.toMatchObject({
      success: true,
      stdout: 'hello world',
      exitCode: 0,
      pid: 1357,
    });

    expect(child.stdout.destroy).not.toHaveBeenCalled();
    expect(child.stderr.destroy).not.toHaveBeenCalled();

    vi.useRealTimers();
  });

  it('fails host commands that stay silent past the idle timeout', async () => {
    vi.useFakeTimers();

    const child = createMockChildProcess(8642);
    childProcessMocks.spawn.mockReturnValue(child);
    process.env.DC_HOST_SHELL_IDLE_TIMEOUT_MS = '1000';

    const killSpy = vi.spyOn(process, 'kill').mockImplementation(() => true as never);
    const onExit = vi.fn();
    const manager = new HostShellManager('/tmp/workspace');

    const execPromise = manager.exec('uv pip install thing', undefined, { onExit });

    await vi.advanceTimersByTimeAsync(1001);

    expect(killSpy).toHaveBeenCalledWith(-8642, 'SIGTERM');

    child.emit('close', 124, 'SIGTERM');

    await expect(execPromise).resolves.toEqual({
      success: false,
      stdout: '',
      stderr: 'Command timed out after 1000ms without output',
      exitCode: 124,
      pid: 8642,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      summary: 'Command timed out after 1000ms without output via host[clean-room]',
      timeMs: expect.any(Number),
    });

    expect(onExit).toHaveBeenCalledWith({
      command: 'uv pip install thing',
      pid: 8642,
      backend: 'host',
      backendLabel: 'host[clean-room]',
      success: false,
      exitCode: 124,
      summary: 'Command timed out after 1000ms without output via host[clean-room]',
    });
  });

  it('resets the idle timeout when output continues arriving', async () => {
    vi.useFakeTimers();

    const child = createMockChildProcess(9753);
    childProcessMocks.spawn.mockReturnValue(child);
    process.env.DC_HOST_SHELL_IDLE_TIMEOUT_MS = '1000';

    const killSpy = vi.spyOn(process, 'kill').mockImplementation(() => true as never);
    const manager = new HostShellManager('/tmp/workspace');

    const execPromise = manager.exec('slow command', undefined);

    await vi.advanceTimersByTimeAsync(900);
    child.stdout.emit('data', 'still working\n');
    await vi.advanceTimersByTimeAsync(900);
    child.stderr.emit('data', 'more progress\n');
    await vi.advanceTimersByTimeAsync(900);

    expect(killSpy).not.toHaveBeenCalled();

    child.emit('close', 0, null);

    await expect(execPromise).resolves.toMatchObject({
      success: true,
      stdout: 'still working\n',
      stderr: 'more progress\n',
      exitCode: 0,
      pid: 9753,
      backend: 'host',
      summary: 'Command completed successfully via host[clean-room]',
    });
  });

  it('uses setpriv-wrapped bwrap when direct probing hits uid-map permission denial', async () => {
    const child = createMockChildProcess(5511);
    childProcessMocks.spawn.mockReturnValue(child);
    childProcessMocks.spawnSync
      .mockReturnValueOnce({
        status: 1,
        error: undefined,
        stdout: '',
        stderr: 'bwrap: setting up uid map: Permission denied',
      })
      .mockReturnValueOnce({
        status: 1,
        error: undefined,
        stdout: '',
        stderr: 'bwrap: setting up uid map: Permission denied',
      })
      .mockReturnValueOnce({
        status: 0,
        error: undefined,
        stdout: '',
        stderr: '',
      });

    const manager = new HostShellManager('/tmp/workspace');
    const execPromise = manager.exec('printf hello');

    child.emit('close', 0, null);

    await expect(execPromise).resolves.toMatchObject({
      success: true,
      exitCode: 0,
      pid: 5511,
      backend: 'host',
      backendLabel: 'host[bwrap]',
      summary: 'Command completed successfully via host[bwrap]',
    });

    expect(childProcessMocks.spawnSync).toHaveBeenCalledTimes(3);
    expect(childProcessMocks.spawn).toHaveBeenCalledWith(
      expect.stringMatching(/(^|\/)setpriv$/),
      expect.arrayContaining([
        '--no-new-privs',
        expect.stringMatching(/(^|\/)bwrap$/),
        '--die-with-parent',
        '--new-session',
        '--unshare-user-try',
      ]),
      expect.objectContaining({
        cwd: '/tmp/workspace',
        detached: true,
      }),
    );
  });

  it('presents the workspace as /workspace under bwrap', async () => {
    // bwrap is the one host backend that *can* mount the workspace, so it does —
    // and the bind is what makes `/workspace` true inside a command, rather than
    // only in the agent's path strings. Pinned here because bwrap cannot run on
    // every machine (unprivileged user namespaces are often disabled), so the
    // real-shell test cannot always cover it.
    const child = createMockChildProcess(7788);
    childProcessMocks.spawn.mockReturnValue(child);

    const manager = new HostShellManager('/tmp/workspace', { wrapper: 'bwrap' });
    const execPromise = manager.exec('cat /workspace/out.txt');
    child.emit('close', 0, null);
    await execPromise;

    const [executable, args, options] = childProcessMocks.spawn.mock.calls[0];
    expect(String(executable)).toMatch(/(^|\/)bwrap$/);
    // Bound at /workspace, not at its host path...
    expect(args).toEqual(expect.arrayContaining(['--bind', '/tmp/workspace', '/workspace']));
    expect(args).toEqual(expect.arrayContaining(['--chdir', '/workspace']));
    // ...so the command is passed through untouched, not rewritten.
    expect(args).toContain('cat /workspace/out.txt');
    // And the shell's environment agrees with its own view of the tree.
    expect(options.env).toEqual(expect.objectContaining({
      PWD: '/workspace',
      WORKSPACE: '/workspace',
    }));
  });

  it('maps /workspace onto the real directory when no mount is possible', async () => {
    const child = createMockChildProcess(7789);
    childProcessMocks.spawn.mockReturnValue(child);

    const manager = new HostShellManager('/tmp/workspace', { wrapper: 'clean-room' });
    const execPromise = manager.exec('cat /workspace/out.txt');
    child.emit('close', 0, null);
    await execPromise;

    const [, args, options] = childProcessMocks.spawn.mock.calls[0];
    expect(args).toContain('cat /tmp/workspace/out.txt');
    expect(options.env).toEqual(expect.objectContaining({
      PWD: '/tmp/workspace',
      WORKSPACE: '/tmp/workspace',
    }));
  });

  it('prefers bwrap on linux when the probe succeeds', () => {
    expect(resolveHostShellWrapperKind({
      platform: 'linux',
      probes: {
        isBwrapUsable: () => true,
        isSandboxExecUsable: () => false,
      },
    })).toBe('bwrap');
  });

  it('prefers sandbox-exec on macOS when available and bwrap is not in play', () => {
    expect(resolveHostShellWrapperKind({
      platform: 'darwin',
      probes: {
        isBwrapUsable: () => false,
        isSandboxExecUsable: () => true,
      },
    })).toBe('sandbox-exec');
  });

  it('honors an explicit wrapper override', () => {
    expect(resolveHostShellWrapperKind({
      platform: 'linux',
      probes: {
        isBwrapUsable: () => false,
        isSandboxExecUsable: () => false,
      },
      envOverride: 'clean-room',
    })).toBe('clean-room');
  });

  it('uses strict bwrap mode when strict isolation is requested and supported', () => {
    expect(resolveHostShellStrategy({
      hostConfig: { strictIsolation: true },
      platform: 'linux',
      probes: {
        isBwrapUsable: (strictIsolation) => strictIsolation,
        isSandboxExecUsable: () => false,
      },
    })).toMatchObject({
      wrapperKind: 'bwrap',
      strictIsolation: true,
      backendLabel: 'host[bwrap strict]',
    });
  });

  it('falls back to clean-room when strict isolation is requested but unavailable', () => {
    expect(resolveHostShellStrategy({
      hostConfig: { strictIsolation: true },
      platform: 'linux',
      probes: {
        isBwrapUsable: () => false,
        isSandboxExecUsable: () => false,
      },
    })).toMatchObject({
      wrapperKind: 'clean-room',
      strictIsolation: false,
      backendLabel: 'host[clean-room]',
    });
  });
});

function createMockChildProcess(pid: number) {
  const child = new EventEmitter() as EventEmitter & {
    pid: number;
    killed: boolean;
    exitCode: number | null;
    signalCode: NodeJS.Signals | null;
    stdout: EventEmitter & { setEncoding: (encoding: string) => void; destroy: ReturnType<typeof vi.fn> };
    stderr: EventEmitter & { setEncoding: (encoding: string) => void; destroy: ReturnType<typeof vi.fn> };
    kill: (signal?: NodeJS.Signals) => boolean;
  };

  child.pid = pid;
  child.killed = false;
  child.exitCode = null;
  child.signalCode = null;
  child.stdout = new EventEmitter() as EventEmitter & { setEncoding: (encoding: string) => void; destroy: ReturnType<typeof vi.fn> };
  child.stderr = new EventEmitter() as EventEmitter & { setEncoding: (encoding: string) => void; destroy: ReturnType<typeof vi.fn> };
  child.stdout.setEncoding = () => {};
  child.stderr.setEncoding = () => {};
  child.stdout.destroy = vi.fn();
  child.stderr.destroy = vi.fn();
  child.kill = () => {
    child.killed = true;
    return true;
  };

  return child;
}