# Small deterministic Prolog/DML recipes

Use these for glue, not as a reason to build a large parser or custom algorithm.
Examples are ordinary goals; bind their inputs first. Consult `basics` for
unification, modes and side-effect/backtracking rules.

## Strings

- `split_string("a,b,,c", ",", "", Xs)` gives strings including `""`.
- `atomics_to_string(["a","b"], "-", S)` joins into a string.
- `string_upper("Ada", U)` / `string_lower/2` change case.
- `format(string(S), "~w/~d", [name, 2])` builds a string.
- `number_string(42, S)` / `atom_string(Atom, String)` convert deliberately.
- `normalize_space(string(T), "  a   b ")` collapses whitespace.
- `pattern("Name: $name", Text, [name="Ada"])` builds a template;
  bind Text instead to parse. `$n:int` converts a typed slot; `$$` is a dollar.

## Lists

- `member(X, Xs)` enumerates; `memberchk(X, Xs)` commits one match.
- `findall(X, between(1,3,X), Xs)` gives `[1,2,3]`; empty gives `[]`.
- `maplist([X,Y]>>(Y is X*2), [1,2,3], Ys)` sequentially maps.
- `include([X]>>(X > 0), [-1,0,2], Ys)` keeps positives.
- `exclude(==(""), ["", "a"], Ys)` drops empty strings.
- `foldl([X,A,B]>>(B is A+X), [1,2,3], 0, N)` gives 6.
- `sort([2,1,2], Ys)` sorts AND deduplicates; `msort/2` keeps duplicates.
- `keysort([b-2,a-1], Ys)` stable-sorts pairs by key.
- `sum_list([1,2,3], N)` gives 6; guard empty lists before min/max.
- `nth0(1, [a,b,c], X)` gives b; `nth1/3` is one-based.
- `append/3`, `reverse/2`, `length/2`, `list_to_set/2` are standard glue.

## Dicts and JSON

- `D = _{name:"Ada"}, get_dict(name, D, N)` reads a controlled atom key.
- `dict_field(D, "Name", N)` tolerates case/separator/key representation changes.
- `dict_field(D, country, "unknown", C)` supplies a default.
- `json_get(D, "rows.0.name", N)` reads a path; missing FAILS.
- `json_get(D, "rows.0.year", "unknown", Y)` supplies a default.
- `json_gets(D, "rows.*.name", Ns)` projects over lists.
- `json_put(D, "meta.reviewed", true, D2)` sets a dict-key path.
- `json_text(D, Text)` writes compact JSON.
- `atom_json_dict(Text, D, [])` parses bound Text, or writes when Text is unbound.
- `put_dict(score, D, 9, D2)` updates an immutable dict value.

## Parsing and regex

Use Python for substantial/format-sensitive parsing. For a small exact template,
use `pattern/3`. DCGs are supported by load-time expansion; `phrase/2,3` takes a
LIST, so use `string_codes/2` or the grammar's expected character representation.
Read `language` before mixing DCG literals, character lists and dcg/basics.
SWI regex interfaces: `re_match/2` tests, `re_matchsub/4` returns capture dicts.
`re_replace/4` behavior/options should be checked before assuming global replacement.
Do not invent list/regex helpers; exact signatures matter.

## Boundaries

- `get_dict("name", D, N)` is wrong: keys must be atoms.
- `X is Y+1` requires numeric Y; unification `X=Y+1` does not compute.
- `setof/3` / `bagof/3` fail for no solutions; findall gives an empty list.
- `sort/2` removes duplicates; it is not stable-preserve-all sorting.
- A nondeterministic mapper can multiply solutions and replay effects.
- `{Var}` interpolation is a loader feature in supported DML text positions;
  use `format/3` for an arbitrary computed string.
- Do not guess that nth0/4 or another library helper is present; check it with
  a pure bounded execute goal. Never try to infer a DML handler's availability
  only from current_predicate (form/exec handlers are not normal clauses).
- Host command syntax is NOT Prolog syntax. Read the current tool schema; no
  shell/stdin assumption belongs in a generic snippet.
