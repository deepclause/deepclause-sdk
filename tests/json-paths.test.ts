/**
 * Path access into model-produced structures.
 *
 * `json_get/3` replaces a chain of `dict_field/3` calls for a nested lookup, and a
 * missing path **fails** rather than throwing or yielding "". `json_gets/3`
 * projects with `*`, and `json_text/2` writes JSON — which the runtime could
 * always do (`atom_json_dict/3` is bidirectional) but the docs never said.
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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-json-paths-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

const DOC = 'x{results:[x{title:"A",authors:[x{name:"Ann"},x{name:"Bob"}]},'
  + 'x{title:"B",authors:[x{name:"Cid"}]}],meta:x{reviewed:false,n:2}}';

function host(goal: string): Record<string, unknown> {
  const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  return q.call(repl.session, goal) ?? {};
}

function val(goal: string, name: string): unknown {
  const raw = host(goal)[name];
  const unwrapped = plainValue(raw);
  return unwrapped === undefined ? raw : unwrapped;
}

describe('json_get/3,4', () => {
  it('reads a nested value by a dotted path', () => {
    expect(val(`json_get(${DOC}, "results.0.title", V)`, 'V')).toBe('A');
    expect(val(`json_get(${DOC}, "results.1.authors.0.name", V)`, 'V')).toBe('Cid');
  });

  it('reads the same value from a list path', () => {
    expect(val(`json_get(${DOC}, [results, 0, authors, 1, name], V)`, 'V')).toBe('Bob');
  });

  it('matches keys tolerantly, like dict_field', () => {
    // Prolog atoms arrive as strings over the bridge: `false`, not JS false.
    expect(val(`json_get(${DOC}, "Meta.Reviewed", V)`, 'V')).toBe('false');
  });

  it('fails on a missing path rather than throwing or yielding empty', () => {
    expect(host(`json_get(${DOC}, "results.9.title", V)`).V).toBeUndefined();
    expect(host(`json_get(${DOC}, "nope.title", V)`).V).toBeUndefined();
    expect(host(`json_get(${DOC}, "results.0.missing", V)`).V).toBeUndefined();
  });

  it('supplies a default with json_get/4', () => {
    expect(val(`json_get(${DOC}, "results.0.year", "unknown", V)`, 'V')).toBe('unknown');
    expect(val(`json_get(${DOC}, "results.0.title", "unknown", V)`, 'V')).toBe('A');
  });
});

describe('json_gets/3', () => {
  it('projects a field across a list', () => {
    const titles = val(`json_gets(${DOC}, "results.*.title", Vs)`, 'Vs') as unknown[];
    expect(titles.map((t) => plainValue(t))).toEqual(['A', 'B']);
  });

  it('flattens nested collections', () => {
    const names = val(`json_gets(${DOC}, "results.*.authors.*.name", Vs)`, 'Vs') as unknown[];
    expect(names.map((n) => plainValue(n))).toEqual(['Ann', 'Bob', 'Cid']);
  });

  it('collects a single value as a one-element list', () => {
    const titles = val(`json_gets(${DOC}, "results.0.title", Vs)`, 'Vs') as unknown[];
    expect(titles.map((t) => plainValue(t))).toEqual(['A']);
  });
});

describe('json_put/4 and json_text/2', () => {
  it('sets a nested key', () => {
    expect(val(`json_put(${DOC}, "meta.reviewed", yes, D), json_get(D, "meta.reviewed", V)`, 'V')).toBe('yes');
  });

  it('creates an intermediate key that was absent', () => {
    const r = val(`json_put(${DOC}, "meta.audit.who", "me", D), json_get(D, "meta.audit.who", V)`, 'V');
    expect(r).toBe('me');
  });

  it('round-trips through JSON text', () => {
    const text = val(`json_text(${DOC}, T)`, 'T');
    expect(typeof text).toBe('string');
    expect(text).toContain('"title"');
    // and back
    expect(val(`json_text(${DOC}, T), atom_json_dict(T, D, []), json_get(D, "results.1.title", V)`, 'V')).toBe('B');
  });

  it('rejects an index path for json_put', () => {
    expect(host(`json_put(${DOC}, "results.0.title", "C", D)`).D).toBeUndefined();
  });
});
