/**
 * `dict_field/3,4`: look up a model-produced dict field without the two traps.
 *
 * SWI dict keys are atoms and `get_dict/3` is strict — `get_dict("k", D)` raises
 * `type_error(dict-key, k)` rather than failing — and the model varies the spelling
 * of field names. `dict_field` matches on a normalised key (atom or string, any
 * case, `_`/`-`/space ignored).
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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-dictfield-'));
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

/** Read one binding, unwrapped. */
function val(goal: string, name: string): unknown {
  const raw = host(goal)[name];
  const unwrapped = plainValue(raw);
  return unwrapped === undefined ? raw : unwrapped;
}

describe('dict_field', () => {
  it('reads a field with an atom key', () => {
    expect(val('dict_field(x{name:"Alice"}, name, V)', 'V')).toBe('Alice');
  });

  it('reads a field when the lookup key is a string but the dict key is an atom', () => {
    // The trap: get_dict("name", x{name:"Alice"}, V) raises type_error(dict-key).
    expect(() => host('get_dict("name", x{name:"Alice"}, V)')).not.toThrow();
    expect(val('dict_field(x{name:"Alice"}, "name", V)', 'V')).toBe('Alice');
  });

  it('matches case and separators', () => {
    expect(val('dict_field(x{full_name:"Alice Chen"}, "FullName", V)', 'V')).toBe('Alice Chen');
    expect(val('dict_field(x{affiliation:"MIT"}, "affiliation", V)', 'V')).toBe('MIT');
  });

  it('uses the default when the field is absent', () => {
    expect(val('dict_field(x{name:"A"}, country, "", V)', 'V')).toBe('');
    expect(val('dict_field(x{name:"A"}, country, "unstated", V)', 'V')).toBe('unstated');
  });

  it('works over a list of model-shaped dicts', () => {
    const goal = 'findall(N, (member(D, [x{name:"A"},x{name:"B"}]), dict_field(D, "name", N)), L)';
    const list = val(goal, 'L') as unknown[];
    expect(list.map((entry) => plainValue(entry))).toEqual(['A', 'B']);
  });
});
