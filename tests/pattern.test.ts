/**
 * `pattern/3`: one syntax for building a string and for parsing it.
 *
 * Literal text matches exactly and `$Name` marks a slot, so the same template is
 * `format/3` and its inverse. `$` is the marker so a template can carry JSON (or
 * any brace-bearing text) verbatim and never collide with the `{Var}`
 * interpolation used in `task/N` descriptions.
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
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-pattern-'));
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

function val(goal: string, name: string): unknown {
  const raw = host(goal)[name];
  const unwrapped = plainValue(raw);
  return unwrapped === undefined ? raw : unwrapped;
}

/**
 * Parse and render the bindings as text. The bindings are a Prolog term, so
 * `format/2` renders them here rather than trying to walk the bridge's object form.
 */
function parseText(template: string, text: string): string {
  const goal = `pattern(${JSON.stringify(template)}, ${JSON.stringify(text)}, B), `
    + 'format(string(S), "~w", [B])';
  return String(val(goal, 'S'));
}

describe('pattern/3 — build', () => {
  it('renders the slots', () => {
    expect(val('pattern("Name: $name, age: $age", T, [name="Ada", age=36])', 'T')).toBe('Name: Ada, age: 36');
  });

  it('treats a single brace as literal text (so JSON is verbatim)', () => {
    const t = val('pattern("{\\"a\\": $V}", T, [v=1])', 'T');
    expect(t).toBe('{"a": 1}');
  });

  it('renders a literal dollar with $$', () => {
    expect(val('pattern("cost: $$5 for $item", T, [item="tea"])', 'T')).toBe('cost: $5 for tea');
  });
});

describe('pattern/3 — parse', () => {
  it('binds every slot', () => {
    const rendered = parseText('Name: $name, age: $age', 'Name: Ada, age: 36');
    expect(rendered).toContain('name=Ada');
    expect(rendered).toContain('age=36');
  });

  it('handles a slot at the very end', () => {
    expect(parseText('DOI: $doi', 'DOI: 10.1/x')).toContain('doi=10.1/x');
  });

  it('handles a slot at the very start', () => {
    expect(parseText('$head -- rest', 'alpha -- rest')).toContain('head=alpha');
  });

  it('captures up to the next literal, not greedily', () => {
    // "a - b - c" against `$Left - $Right` splits at the first " - ".
    const rendered = parseText('$left - $right', 'a - b - c');
    expect(rendered).toContain('left=a');
    expect(rendered).toContain('right=b - c');
  });

  it('converts a typed slot to a number', () => {
    // An integer renders without quotes.
    expect(parseText('n=$n:int', 'n=42')).toContain('n=42');
  });

  it('reports a typed slot that does not match, rather than binding text', () => {
    const r = host('catch(pattern("n=$n:int", "n=abc", B), error(E, _), R = E)');
    expect(JSON.stringify(r.R)).toContain('not an integer');
  });

  it('fails when the literals do not match', () => {
    expect(host('pattern("Name: $n", "Age: Ada", B)').B).toBeUndefined();
  });

  it('matches a slot name case- and separator-insensitively', () => {
    // Template slot `full_name`, binding key `FullName`.
    expect(val('pattern("x $full_name", T, [FullName="Ada"])', 'T')).toBe('x Ada');
  });
});

describe('pattern/3 — round trip and ambiguity', () => {
  it('parses what it built', () => {
    const goal = 'pattern("$a/$b", T, [a="x", b="y"]), '
      + 'pattern("$a/$b", T, B2), format(string(S), "~w", [B2])';
    const rendered = String(val(goal, 'S'));
    expect(rendered).toContain('a=x');
    expect(rendered).toContain('b=y');
  });

  it('rejects adjacent slots as ambiguous', () => {
    const r = host('catch(pattern("$a$b", T, [a=1, b=2]), error(E, _), R = E)');
    expect(JSON.stringify(r.R)).toContain('ambiguous');
  });

  it('reports a missing binding', () => {
    const r = host('catch(pattern("x $need", T, [other=1]), error(E, _), R = E)');
    expect(JSON.stringify(r.R)).toContain('no value bound for slot');
  });
});
