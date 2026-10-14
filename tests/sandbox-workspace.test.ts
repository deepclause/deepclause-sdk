/**
 * One name for the workspace, on every surface.
 *
 * The WASM filesystem mounts the host project at `/workspace` and AgentVM does
 * the same, but a host shell has no reason to: bwrap is told to bind it there,
 * and where a mount is impossible (sandbox-exec, clean-room, Windows) the name
 * is mapped onto the real directory. Without this the model would have to reason
 * about two path spaces, which is what made it name the wrong path.
 *
 * The mapping is deliberately conservative — a blind replace would corrupt the
 * commands it exists to serve — so the tricky cases are pinned here.
 */
import { describe, expect, it } from 'vitest';

import {
  describeShellExecutionBackend,
  mapWorkspacePaths,
} from '../src/system/runtime/shell-manager.js';

const WS = '/home/me/proj';

describe('mapWorkspacePaths', () => {
  it('maps a word that begins /workspace', () => {
    expect(mapWorkspacePaths('cat /workspace/out.txt', WS)).toBe(`cat ${WS}/out.txt`);
    expect(mapWorkspacePaths('/workspace', WS)).toBe(WS);
    expect(mapWorkspacePaths('cd /workspace && ls', WS)).toBe(`cd ${WS} && ls`);
  });

  it('maps the forms a command actually uses', () => {
    expect(mapWorkspacePaths('python3 /workspace/s.py', WS)).toBe(`python3 ${WS}/s.py`);
    expect(mapWorkspacePaths('--dir=/workspace/out', WS)).toBe(`--dir=${WS}/out`);
    expect(mapWorkspacePaths('-I/workspace/include', WS)).toBe(`-I${WS}/include`);
    expect(mapWorkspacePaths('cat "/workspace/a b.txt"', WS)).toBe(`cat "${WS}/a b.txt"`);
    expect(mapWorkspacePaths("echo '/workspace/x'", WS)).toBe(`echo '${WS}/x'`);
    expect(mapWorkspacePaths('a=/workspace/x;ls /workspace/y', WS)).toBe(`a=${WS}/x;ls ${WS}/y`);
  });

  it('leaves lookalikes alone', () => {
    // Not a word boundary, so not a workspace path.
    expect(mapWorkspacePaths('echo /workspacex', WS)).toBe('echo /workspacex');
    expect(mapWorkspacePaths('echo /workspace-suffix', WS)).toBe('echo /workspace-suffix');
    // Not prefixed at all.
    expect(mapWorkspacePaths('echo workspace/out', WS)).toBe('echo workspace/out');
    expect(mapWorkspacePaths('echo /etc/passwd', WS)).toBe('echo /etc/passwd');
  });

  it('does not touch text that merely mentions the name', () => {
    // Deliberate: `$WORKSPACE` is the variable form, and the literal word
    // `/workspace` is what the mapping is for. A path inside a quoted string is
    // mapped; a bare word in prose inside a command is not distinguished, which
    // is why the runtime prefers a real mount wherever one is possible.
    expect(mapWorkspacePaths('echo "$WORKSPACE/out"', WS)).toBe('echo "$WORKSPACE/out"');
  });
});

describe('describeShellExecutionBackend', () => {
  it('reports agentvm as sandboxed', () => {
    expect(describeShellExecutionBackend(true)).toMatchObject({
      kind: 'agentvm',
      sandboxed: true,
    });
  });

  it('reports clean-room as unsandboxed', () => {
    expect(describeShellExecutionBackend(false, { wrapper: 'clean-room' })).toMatchObject({
      kind: 'clean-room',
      sandboxed: false,
    });
  });

  it('reports bwrap and sandbox-exec as sandboxed', () => {
    expect(describeShellExecutionBackend(false, { wrapper: 'bwrap' })).toMatchObject({
      kind: 'bwrap',
      sandboxed: true,
    });
    expect(describeShellExecutionBackend(false, { wrapper: 'sandbox-exec' })).toMatchObject({
      kind: 'sandbox-exec',
      sandboxed: true,
    });
  });
});

describe('the fallback path, for real', () => {
  // No mocking: this runs the actual shell. It covers the case the mapping
  // exists for — a backend that cannot mount /workspace (bwrap is unavailable on
  // many machines, and sandbox-exec never can) — and it is the case that used to
  // contradict the documentation.
  it('lets a real clean-room shell read and write /workspace', async () => {
    const fs = await import('fs/promises');
    const os = await import('os');
    const path = await import('path');
    const { HostShellManager } = await import('../src/system/runtime/shell-manager.js');

    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-ws-map-'));
    await fs.writeFile(path.join(workspace, 'f.txt'), 'CONTENT');
    const shell = new HostShellManager(workspace, { wrapper: 'clean-room' });

    try {
      const read = await shell.exec('cat /workspace/f.txt');
      expect(read.stdout.trim()).toBe('CONTENT');

      // A write, addressed by the same name, lands in the real workspace.
      await shell.exec('echo WROTE > /workspace/out.txt');
      expect(await fs.readFile(path.join(workspace, 'out.txt'), 'utf8')).toBe('WROTE\n');

      // Relative paths agree, and $WORKSPACE points at the real directory.
      const relative = await shell.exec('cat f.txt; echo "ws=$WORKSPACE"');
      expect(relative.stdout).toContain('CONTENT');
      expect(relative.stdout).toContain(`ws=${workspace}`);

      // Scratch is $TMPDIR and is not the workspace.
      const scratch = await shell.exec('echo scratch > "$TMPDIR/s"; ls "$TMPDIR/s"');
      expect(scratch.success).toBe(true);
      expect(await fs.readdir(workspace)).not.toContain('s');
    } finally {
      await shell.dispose();
      await fs.rm(workspace, { recursive: true, force: true });
    }
  }, 30000);
});
