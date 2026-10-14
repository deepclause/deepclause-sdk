/**
 * The list idioms documented in `DML_REFERENCE.md` ("Lists and common operations")
 * and `PROLOG_SNIPPETS.md`, executed so the claim that they work stays honest.
 *
 * Several assumptions were wrong when this was first written, which is the point:
 * `nth0/4`, `numlist/4`, `chunk/3` and `rename_keys/3` are **not** available in this
 * build, `setof/3`/`bagof/3` fail on an empty solution set, and `sort/4` does not
 * work on dicts. Each of those is asserted here as a documented limitation, so the
 * guidance cannot quietly become wrong.
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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-lists-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

/** Unwrap the bridge's object form all the way down. */
function deep(value: unknown): unknown {
  const unwrapped = plainValue(value);
  if (Array.isArray(unwrapped)) return unwrapped.map(deep);
  if (unwrapped && typeof unwrapped === 'object') {
    const record = unwrapped as Record<string, unknown>;
    if ('$t' in record) return unwrapped;
    const out: Record<string, unknown> = {};
    for (const [key, item] of Object.entries(record)) out[key] = deep(item);
    return out;
  }
  return unwrapped;
}

/** Run a goal and read one binding. */
function run(goal: string, name = 'L'): unknown {
  const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  const result = q.call(repl.session, goal) ?? {};
  return deep(result[name]);
}

/** Run a goal and read one binding as Prolog's rendered text. */
function text(goal: string, name = 'L'): string {
  const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  const result = q.call(repl.session, `${goal.replace(/\s*$/, '')}, format(string(S), "~w", [${name}])`) ?? {};
  return String(plainValue(result.S) ?? '');
}

/** Succeed or fail, for the semidet and unavailable cases. */
function holds(goal: string): boolean {
  const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
  return (q.call(repl.session, goal) ?? {}).success === true;
}

describe('transform', () => {
  it('maps and combines', () => {
    expect(run('maplist([X,Y]>>(Y is X*X), [1,2,3], L)')).toEqual([1, 4, 9]);
    expect(run('maplist([A,B,C]>>(C is A+B), [1,2], [10,20], L)')).toEqual([11, 22]);
  });

  it('maps while skipping failures', () => {
    expect(run('convlist([X,Y]>>(integer(X), Y is X*2), [1,a,3], L)')).toEqual([2, 6]);
  });

  it('filters and folds', () => {
    expect(run('include([X]>>(0 is X mod 2), [1,2,3,4], L)')).toEqual([2, 4]);
    expect(run('exclude([X]>>(0 is X mod 2), [1,2,3,4], L)')).toEqual([1, 3]);
    expect(run('foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, S)', 'S')).toBe(6);
  });

  it('splits a list with append', () => {
    const goal = 'append(Chunk, Rest, [1,2,3]), length(Chunk, 2)';
    const q = (repl.session as unknown as { prologQuery: (g: string) => Record<string, unknown> }).prologQuery;
    const result = q.call(repl.session, goal) ?? {};
    expect(deep(result.Chunk)).toEqual([1, 2]);
    expect(deep(result.Rest)).toEqual([3]);
  });
});

describe('select and aggregate', () => {
  it('indexes, selects and checks membership', () => {
    expect(run('nth0(1, [a,b,c], E)', 'E')).toBe('b');
    expect(run('nth1(1, [a,b,c], E)', 'E')).toBe('a');
    expect(run('select(b, [a,b,c], L)')).toEqual(['a', 'c']);
    expect(holds('memberchk(b, [a,b,c])')).toBe(true);
    expect(run('last([a,b,c], E)', 'E')).toBe('c');
    expect(run('reverse([1,2,3], L)')).toEqual([3, 2, 1]);
  });

  it('aggregates', () => {
    expect(run('sum_list([1,2,3], S)', 'S')).toBe(6);
    expect(run('max_member(M, [1,3,2])', 'M')).toBe(3);
    expect(run('aggregate_all(max(N), member(N,[1,9,3]), M)', 'M')).toBe(9);
    expect(run('aggregate_all(count, between(1,5,_), N)', 'N')).toBe(5);
  });
});

describe('sort, group, dedupe', () => {
  it('sorts, with and without dedupe', () => {
    expect(run('sort([3,1,2,1], L)')).toEqual([1, 2, 3]);
    expect(run('msort([3,1,2,1], L)')).toEqual([1, 1, 2, 3]);
    expect(run('list_to_set([a,b,a,c], L)')).toEqual(['a', 'b', 'c']);
  });

  it('sorts pairs by key, stably', () => {
    // `Key-Value` is a term, so compare its rendered form rather than walking the
    // bridge's object shape.
    expect(text('keysort([b-2,a-1], L)')).toBe('[a-1,b-2]');
    expect(text('msort([b-1,a-2,a-1], L)')).toBe('[a-1,a-2,b-1]');
  });

  it('groups by key', () => {
    expect(text('group_pairs_by_key([a-1,a-2,b-3], G)', 'G')).toBe('[a-[1,2],b-[3]]');
  });

  it('zips and unzips', () => {
    expect(text('pairs_keys_values(P, [a,b], [1,2])', 'P')).toBe('[a-1,b-2]');
    expect(run('pairs_keys([a-1,b-2], K)', 'K')).toEqual(['a', 'b']);
    expect(run('pairs_values([a-1,b-2], V)', 'V')).toEqual([1, 2]);
  });
});

describe('strings and dicts inside a list', () => {
  it('drops the empty strings split_string leaves behind', () => {
    expect(run('split_string("a,,b", ",", "", P), exclude(==(""), P, L)')).toEqual(['a', 'b']);
    expect(run('split_string("a,,b", ",", "", P), include(dif(""), P, L)')).toEqual(['a', 'b']);
  });

  it('collects a field from a list of dicts', () => {
    expect(run('json_gets(x{rows:[x{name:"a"},x{name:"b"}]}, "rows.*.name", L)')).toEqual(['a', 'b']);
    expect(run('findall(N, (member(D, [x{name:"a"},x{name:"b"}]), dict_field(D, name, N)), L)'))
      .toEqual(['a', 'b']);
  });
});

describe('chunks/3, the documented batching helper', () => {
  const DEFS = `
chunks([], _, []).
chunks(List, N, [Chunk|Rest]) :-
    N > 0, length(Chunk, N), append(Chunk, Tail, List), chunks(Tail, N, Rest).
chunks(List, N, [List]) :- N > 0, length(List, Len), Len < N.
`;

  it('batches a list, keeping a short final chunk', async () => {
    await repl.session.loadCode(DEFS);
    expect(run('chunks([1,2,3,4], 2, L)')).toEqual([[1, 2], [3, 4]]);
    expect(run('chunks([1,2,3,4,5], 2, L)')).toEqual([[1, 2], [3, 4], [5]]);
    expect(run('chunks([1,2], 5, L)')).toEqual([[1, 2]]);
    expect(run('chunks([], 3, L)')).toEqual([]);
  });
});

describe('documented limitations', () => {
  it('findall succeeds on an empty set, setof and bagof fail', () => {
    expect(run('findall(X, member(X, []), L)')).toEqual([]);
    expect(holds('setof(X, member(X, []), _)')).toBe(false);
    expect(holds('bagof(X, member(X, []), _)')).toBe(false);
  });

  it('max_list fails on an empty list', () => {
    expect(holds('max_list([], _)')).toBe(false);
  });

  it('sort/4 does not work on dicts', () => {
    // It addresses arguments of a compound; a dict has none. predsort/3 is the way.
    expect(holds('catch(sort(1, @<, [x{n:3},x{n:1}], _), _, fail)')).toBe(false);
    expect(holds('predsort([O,A,B]>>compare(O,A,B), [3,1,2], _)')).toBe(true);
  });

  it('is missing the predicates the docs say are missing', () => {
    // Each either errors or fails; what matters is that none of them works, so the
    // documented workarounds (nth0/4 replacement, chunks/3) are not optional.
    for (const goal of [
      'nth0(1, [a,b,c], x, _)',
      'numlist(1, 9, 3, _)',
      'chunk([1,2], 1, _)',
      'rename_keys([], _, _)',
    ]) {
      expect(holds(goal), `${goal} should not succeed`).toBe(false);
    }
  });
});
