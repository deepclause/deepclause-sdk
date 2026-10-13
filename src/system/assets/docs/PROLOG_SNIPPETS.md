# Common Prolog/DML snippets (tested)

Tested against the bundled SWI-Prolog. These are **glue**, not the shape of a
skill: use a snippet to bind or check a value between `task/N` steps, not to
write a Prolog algorithm. Prefer a `task/N`/`prompt/N` step when the
transformation is open-ended (`DML_AUTHORING.md` §0).
`Y` is the output; `>>` introduces a yall lambda.

## Strings

| Task | Snippet | Result |
|---|---|---|
| Split on a separator | `split_string("a,b,,c", ",", "", L)` | `[a,b,'',c]` |
| Join | `atomic_list_concat([a,b,c], "-", S)` | `a-b-c` |
| Join (strings) | `atomics_to_string([a,b,c], "-", S)` | `a-b-c` |
| Lower / upper | `string_lower("AbC", L)` / `string_upper("AbC", U)` | `abc` / `ABC` |
| Atom case | `downcase_atom('ABC', L)` / `upcase_atom('abc', U)` | `abc` / `ABC` |
| Collapse whitespace | `normalize_space(string(T), "  a   b  ")` | `a b` |
| Build a string | `format(string(S), "~w/~w", [A, B])` | `a/b` |
| Number <-> string | `number_string(42, S)` / `atom_number("42", N)` | `"42"` / `42` |
| Substring test | `sub_string(S, _, _, _, "needle")` | true |
| Substring test (atom) | `sub_atom(A, _, _, _, needle)` | true |
| Concat | `string_concat("a", "b", S)` | `ab` |
| Char list | `string_chars("abc", L)` | `[a,b,c]` |
| JSON string -> dict | `atom_json_dict('{"a":[1,2]}', D, []), get_dict(a, D, A)` | `[1,2]` |

In `task/`, `output/`, `answer/` description strings, `{Var}` interpolates the
bound value; in `format/3` use `~w`.

## Regex (SWI PCRE — verified behavior)

| Task | Snippet | Result |
|---|---|---|
| Match test | `re_match("^[a-z]+[0-9]+$", S)` | true/false |
| Match with options | `re_match("(?i)hello", S, [])` | true (case-insensitive) |
| Capture groups | `re_matchsub("(?<user>[a-z]+)-(?<id>[0-9]+)", S, M, []), get_dict(id, M, Id)` | dict `_{0:..., user:..., id:...}` |
| Replace (first match) | `re_replace("[0-9]+", "#", "a1b22c333", R)` | `a#b22c333` |
| Collect every match | `re_foldl([M,Acc,[M|Acc]]>>true, "[0-9]+", "a1b22c333", [], L, []), reverse(L, Ms)` | `[_{0:"1"},_{0:"22"},_{0:"333"}]` |

Notes: `re_replace/4` here replaces only the **first** match — collect all matches
with `re_foldl/6` (or loop) to replace every one. `re_match/3` takes an options
list, not a capture variable; use `re_matchsub/4` to capture groups. Prefix a
pattern with `(?i)` for case-insensitive matching.

## Wildcards (`wildcard_match/2`)

| Pattern | Example | Result |
|---|---|---|
| `*` any run | `wildcard_match("*.dml", "skill.dml")` | true |
| `?` one char | `wildcard_match("a?c", "abc")` | true |
| `[a-z]` class | `wildcard_match("[a-z]*", "abc")` | true |
| `{a,b}` alternation | `wildcard_match("{a,b}*", "bzz")` | true |

## Lists

| Task | Snippet | Result |
|---|---|---|
| Membership | `member(X, [1,2,3])` | enumerates |
| Concatenate | `append([1,2], [3], L)` | `[1,2,3]` |
| Length | `length([a,b,c], N)` | `3` |
| Enumerate range | `numlist(1, 5, L)` | `[1,2,3,4,5]` |
| Collect | `findall(X, between(1,3,X), L)` | `[1,2,3]` |
| Map | `maplist([X,Y]>>(Y is X*X), [1,2,3], L)` | `[1,4,9]` |
| Filter (keep) | `include([X]>>(0 is X mod 2), [1,2,3,4], L)` | `[2,4]` |
| Filter (drop) | `exclude([X]>>(0 is X mod 2), [1,2,3,4], L)` | `[1,3]` |
| Fold | `foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, S)` | `6` |
| Sum / min / max | `sum_list([1,2,3], S)` / `min_list/2` / `max_list/2` / `max_member/2` | `6` |
| Aggregate | `aggregate_all(sum(X), member(X,[1,2,3]), S)` | `6` |
| Count | `aggregate_all(count, between(1,5,_), N)` | `5` |
| Sort + dedupe | `sort([3,1,2,1], L)` | `[1,2,3]` |
| Stable sort | `msort([3,1,2], L)` | `[1,2,3]` |
| Sort by value (pairs) | `sort(2, @>=, [a-1,b-3,c-2], L)` | `[b-3,c-2,a-1]` |
| Key sort (stable) | `keysort([b-2,a-1,c-2], L)` | `[a-1,b-2,c-2]` |
| Dedupe | `list_to_set([a,b,a,c], L)` | `[a,b,c]` |
| Set difference | `subtract([1,2,3,4], [2], L)` | `[1,3,4]` |
| Intersection / union | `intersection([1,2,3],[2,3,4],L)` / `union([1,2],[2,3],L)` | `[2,3]` / `[1,2,3]` |
| Partition | `partition([X]>>(0 is X mod 2), [1,2,3,4], E, O)` | `E=[2,4], O=[1,3]` |
| Index (0-based) | `nth0(1, [a,b,c], E)` | `b` |
| Last | `last([a,b,c], E)` | `c` |
| Group by key | `group_pairs_by_key([a-1,a-2,b-3], G)` | `[a-[1,2], b-[3]]` |

`sort/4`'s first argument is the **argument position** to sort on (here `2` =
the value in each `Key-Value`), not a sort mode.

## Dicts and data

| Task | Snippet |
|---|---|
| Read a field | `D = point{x:1,y:2}, get_dict(x, D, X)` |
| Model field, any key form | `dict_field(D, "affiliation", A)` — atom or string key, any case, `_`/`-`/space ignored; fails if absent |
| Model field with default | `dict_field(D, country, "unstated", C)` |
| Keys | `dict_keys(a{x:1,y:2}, K)` → `[x,y]` |
| Add/replace a field | `put_dict(score, a{name:"n"}, 9, D2)` |

## Numbers and time

| Task | Snippet |
|---|---|
| Range (generator) | `between(1, 6, N)` |
| Random int in range | `random_between(1, 6, N)` |
| Epoch seconds | `get_time(T)` |
| Broken-down time | `get_time(T), stamp_date_time(T, DT, local)` |
| Format a time | `format_time(string(S), "%Y-%m-%d", 1700000000)` → `2023-11-15` |

## Things that bite

- `re_replace/4` replaces only the **first** match (verified); use `re_foldl/6`
  to collect all matches.
- `re_match/3` takes an options list; use `re_matchsub/4` for capture groups.
- `sort/4`'s first argument is the argument position, not a mode.
- Dict keys must be **atoms**: `get_dict("name", D)` raises
  `type_error(dict-key, name)` rather than failing, and `x{"a":1}` is a syntax
  error. Use `dict_field/3,4` for model-produced dicts, whose key spelling varies.
- A top-level `:- use_module(library(clpfd)).` is required before `#=`/`ins`.
- Untrusted data (URLs, file contents, user text) goes through files/stdin,
  never interpolated into a shell command.
- `{Var}` interpolates **only** in `task/N`/`prompt/N` descriptions; use
  `format/3` (`~w`/`~q`/`~s`) everywhere else. A string literal escapes `\n`,
  `\"`, `\\`; a quoted atom doubles `'` (`'it''s'`).
- Building a command: single-quote known paths/flags, pass data via files/stdin,
  never splice untrusted text in. Write foreign code to a file (quoted heredoc
  `<<'EOF'`) rather than embedding it in a DML string.
