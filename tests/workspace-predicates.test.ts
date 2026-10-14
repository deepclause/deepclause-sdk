/**
 * One definition, every surface. The workspace file predicates are real clauses
 * in the session module, and a host query is dispatched in that module, so DML
 * code and a host query resolve the same predicate.
 *
 * Regression guard for `read_file/2`, which had no definition at all in this
 * build: it existed only as an `mi_call/3` body, so it worked from DML and
 * failed from a host query.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { plainValue } from '../src/repl/frames.js';

let workspace: string;
let repl: HarnessRepl;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-wsp-'));
  await fs.writeFile(path.join(workspace, 'f.txt'), 'hello\n');
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

function host(goal: string): Record<string, unknown> {
  const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  return q.call(repl.session, goal) ?? {};
}

/** Read one binding, unwrapping the Prolog string wrapper. */
function val(goal: string, name: string): unknown {
  return plainValue(host(goal)[name]);
}

describe('workspace predicates from a host query', () => {
  it('reads a file', () => {
    expect(val('read_file("f.txt", C)', 'C')).toBe('hello\n');
    expect(val('read_file_to_string("f.txt", C, [])', 'C')).toBe('hello\n');
  });

  it('writes, appends, and reads back', () => {
    expect(host('write_file("g.txt", "xyz")').success).toBe(true);
    expect(val('read_file("g.txt", C)', 'C')).toBe('xyz');
    expect(host('append_file("g.txt", "-more")').success).toBe(true);
    expect(val('read_file("g.txt", C)', 'C')).toBe('xyz-more');
  });

  it('reports existence, size and time', () => {
    expect(host('exists_file("f.txt")').success).toBe(true);
    expect(host('exists_file("missing.txt")').success).toBe(false);
    expect(val('size_file("f.txt", S)', 'S')).toBe(6);
    expect(typeof val('time_file("f.txt", T)', 'T')).toBe('number');
  });

  it('creates, lists and deletes directories and files', () => {
    expect(host('make_directory_path("a/b")').success).toBe(true);
    expect(host('exists_directory("a/b")').success).toBe(true);
    expect(host('make_directory("c")').success).toBe(true);
    expect((val('directory_files(".", L)', 'L') as string[])).toContain('f.txt');
    expect(host('write_file("tmp.txt", "x")').success).toBe(true);
    expect(host('delete_file("tmp.txt")').success).toBe(true);
    expect(host('exists_file("tmp.txt")').success).toBe(false);
    expect(host('delete_directory("c")').success).toBe(true);
  });

  it('refuses directory traversal', () => {
    // `..` does not climb out of the workspace: /workspace is a mount point, and
    // above it is the VM's own (empty) filesystem, not the host. So this fails
    // rather than reaching anything — the mount is the boundary, not a string
    // check. See the containment test in path-semantics.test.ts for the canary.
    expect(val('read_file("../outside.txt", C)', 'C')).toBeUndefined();
  });

  it('resolves open/3 against the workspace, like every other path', () => {
    // open/3 is a raw SWI builtin and cannot be shadowed, so it used to fail for
    // a relative path and needed an absolute one. With the process working
    // directory set to /workspace it resolves relatively too, so streams are no
    // longer a special case with their own path rule.
    expect(host('(open("f.txt", read, St), close(St))').success).toBe(true);
    expect(val('open("f.txt", read, St), read_string(St, _, S), close(St)', 'S')).toBe('hello\n');
  });
});

describe('workspace predicates are real session predicates', () => {
  it('exposes them in the session module', () => {
    const sessionId = (repl.session as unknown as { sessionId: string }).sessionId;
    expect(host(`current_predicate('${sessionId}':read_file/2)`).success).toBe(true);
    expect(host(`current_predicate('${sessionId}':write_file/2)`).success).toBe(true);
  });

  it('does not list them as harness predicates', () => {
    const listed = (repl.session as unknown as { predicateList: () => string[] }).predicateList();
    expect(listed).not.toContain('read_file/2');
    expect(listed).not.toContain('write_file/2');
    expect(listed).not.toContain('size_file/2');
  });
});
