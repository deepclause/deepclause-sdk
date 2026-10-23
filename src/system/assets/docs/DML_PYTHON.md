# Python host capability

Use this topic only if the CURRENT host catalog registers `python`. The browser
registers Python/Pyodide and has no shell. Other embeddings may supply a different
capability; never guess based on a generic example.

## Contract

DML calls `exec(python(code: Code), Result)` or
`exec(python(script: "scripts/worker.py", argv: ["value"]), Result)`.
Supported options: `code`, `script`, `argv`, `timeoutMs`, `packages`.
`script` takes precedence over `code`. `argv` is a LIST OF STRINGS, passed
verbatim; spaces, quotes and empty strings are not shell-split.
There is no `stdin` option. Pass text as an argv element or through a workspace
file; do not rewrite source code to inject values.

```prolog-goal
exec(python(code: "import json,sys\nprint(json.dumps({'text':sys.argv[1]}))", argv: ["Ada Lovelace"]), R),
get_dict(exitCode, R, 0), get_dict(stdout, R, Json), atom_json_dict(Json, Data, []).
```

The host returns a dict with `exitCode`, `stdout`, `stderr`. A tool that ran with
nonzero exit is NOT a successful computation just because exec returned a dict.
Inspect status/exit code; `catch/3` only catches actual exceptions. Unknown
parameters and host refusal are different from nonzero script exits or plain
DML failure. Do not silently convert errors to empty results.

## Paths and shared files

Working directory is `/workspace`. Use relative workspace paths or `/workspace/...`.
Python and DML share that tree. Their private `/tmp` filesystems are NOT the
same: stage shared bytes under `/workspace/.staging/` or a unique workspace path
and clean up. Do not hardcode host `/home/...` paths or use a fixed shared staging
filename for concurrent runs. Standard Python `open` does not create parents.
The authoring `write_file` TOOL creates parents; DML `write_file/2` does not.

Scripts run in a browser sandbox, not a complete OS. No shell, Node/npm, CLI
binaries or native CPython wheels. Network requests use browser egress/CORS;
`import js; response = await js.fetch(url)` is available. Do not promise arbitrary
web crawling or desktop browser automation. Read the CURRENT schemas via
`reference(topic: "tools", section: "python")` if unsure.

## Linked scripts

Write nontrivial code once during authoring, in a workspace `.py` file; link it
for export, then call it. Script link names MUST differ from predicate names.

```prolog
script_file(count_py, "scripts/count.py").
count_words(Path, N) :-
    run_script(count_py, [Path], Stats),
    get_dict(words, Stats, N).
```

`run_script(Name, Args, Value)` resolves the link, converts atoms/numbers to argv
strings, checks `exitCode`, and parses stdout JSON (otherwise returns stdout).
The script prints one JSON result with a newline; diagnostics belong on stderr.
A nonzero exit throws `script_failed(...)`. Link files with `script_file/2` so
export can bundle them; a path mentioned only in code is not a package manifest.
`delete("script_file:count_py")` removes just that link, not the file or predicate.

Do not use `sys.exit()` in Pyodide helper scripts: the current bridge can report
SystemExit(0) as a failed run. Return normally and print JSON instead. Do not
assume separate invocations clear all Python modules; test repeated runs when
import state matters. That is a runtime behavior to inspect, not a reason to
invent process-isolation flags.

## Packages

`packages: ["package==version"]` installs compatible packages into the current
harness environment. Use pinned pure-Python/Pyodide-compatible packages.
`python_packages(action: "install", packages: [...])` and action `"list"` manage
the same environment. This is not a native OS package manager; arbitrary wheels
will not work. Prefer stdlib when it solves the job.
