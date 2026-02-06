# DML File Access Patterns

Common patterns for file operations in DML using `vm_exec`. All commands run in a sandboxed Alpine Linux VM with the workspace mounted at `/workspace`.

## Important: Extracting Results

`vm_exec` returns a dict with `stdout`, `stderr`, and `exitCode`. Always extract the stdout:

```prolog
exec(vm_exec(command: "ls /workspace"), Result),
get_dict(stdout, Result, Output)
```

---

## 1. List Directory Contents

### Basic listing
```prolog
% List files in a directory
list_files(Dir, Files) :-
    format(string(Cmd), "ls ~w", [Dir]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    split_string(Output, "
", "\s	
", Files).
```

### Detailed listing with metadata
```prolog
% List with sizes and dates
list_files_detailed(Dir, Output) :-
    format(string(Cmd), "ls -la ~w", [Dir]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output).
```

---

## 2. Find Files by Pattern

### Find by name (recursive)
```prolog
% Find all .ts files under a directory
find_by_name(Dir, Pattern, Files) :-
    format(string(Cmd), "find ~w -name '~w' -type f", [Dir, Pattern]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    split_string(Output, "
", "\s	
", Files).

% Usage: find_by_name("/workspace/src", "*.ts", Files)
```

### Find with depth limit
```prolog
% Find only in immediate directory (not recursive)
find_shallow(Dir, Pattern, Files) :-
    format(string(Cmd), "find ~w -maxdepth 1 -name '~w' -type f", [Dir, Pattern]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    split_string(Output, "
", "\s	
", Files).
```

### Find recently modified files
```prolog
% Find files modified in last N days
find_recent(Dir, Pattern, Days, Files) :-
    format(string(Cmd), "find ~w -name '~w' -mtime -~d", [Dir, Pattern, Days]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    split_string(Output, "
", "\s	
", Files).
```

### Count matching files
```prolog
% Count files matching pattern
count_files(Dir, Pattern, Count) :-
    format(string(Cmd), "find ~w -name '~w' -type f | wc -l", [Dir, Pattern]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    atom_string(Output, OutputStr),
    number_string(Count, OutputStr).
```

---

## 3. Glob Patterns

### Using ls with glob
```prolog
% Glob pattern matching
glob_files(Pattern, Files) :-
    format(string(Cmd), "ls ~w 2>/dev/null || true", [Pattern]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    split_string(Output, "
", "\s	
", Files).

% Usage: glob_files("/workspace/src/*.ts", Files)
```

---

## 4. Read File Contents

### Read entire file
```prolog
% Read complete file
read_file(Path, Content) :-
    format(string(Cmd), "cat ~w", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Content).
```

### Read first N lines
```prolog
% Read first N lines of a file
read_head(Path, N, Content) :-
    format(string(Cmd), "head -~d ~w", [N, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Content).
```

### Read last N lines
```prolog
% Read last N lines of a file
read_tail(Path, N, Content) :-
    format(string(Cmd), "tail -~d ~w", [N, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Content).
```

### Read line range
```prolog
% Read lines Start to End (1-indexed)
read_lines(Path, Start, End, Content) :-
    format(string(Cmd), "sed -n '~d,~dp' ~w", [Start, End, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Content).
```

### Read specific line
```prolog
% Read a single line by number
read_line(Path, LineNum, Line) :-
    format(string(Cmd), "sed -n '~dp' ~w", [LineNum, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Line).
```

---

## 5. Search in Files (Grep)

### Search for pattern in file
```prolog
% Grep for pattern in a file
grep(Path, Pattern, Matches) :-
    format(string(Cmd), "grep '~w' ~w || true", [Pattern, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Matches).
```

### Search with line numbers
```prolog
% Grep with line numbers
grep_n(Path, Pattern, Matches) :-
    format(string(Cmd), "grep -n '~w' ~w || true", [Pattern, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Matches).
```

### Recursive search in directory
```prolog
% Search recursively in all files
grep_recursive(Dir, Pattern, Matches) :-
    format(string(Cmd), "grep -rn '~w' ~w || true", [Pattern, Dir]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Matches).
```

### Find files containing pattern
```prolog
% List files that contain a pattern
grep_files_only(Dir, Pattern, Files) :-
    format(string(Cmd), "grep -rl '~w' ~w || true", [Pattern, Dir]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    split_string(Output, "
", "\s	
", Files).
```

### Case-insensitive search
```prolog
% Case-insensitive grep
grep_i(Path, Pattern, Matches) :-
    format(string(Cmd), "grep -i '~w' ~w || true", [Pattern, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Matches).
```

---

## 6. File Existence Checks

### Check if file exists
```prolog
% Check if a file exists
file_exists(Path) :-
    format(string(Cmd), "test -f ~w && echo yes || echo no", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    Output = "yes".
```

### Check if directory exists
```prolog
% Check if a directory exists
dir_exists(Path) :-
    format(string(Cmd), "test -d ~w && echo yes || echo no", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    Output = "yes".
```

---

## 7. File Metadata

### Get file size
```prolog
% Get file size in bytes
file_size(Path, Size) :-
    format(string(Cmd), "stat -c %s ~w", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    atom_string(Output, SizeStr),
    number_string(Size, SizeStr).
```

### Get line count
```prolog
% Count lines in a file
line_count(Path, Count) :-
    format(string(Cmd), "wc -l < ~w", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    atom_string(Output, CountStr),
    number_string(Count, CountStr).
```

### Get basename and dirname
```prolog
% Get filename from path
basename(Path, Name) :-
    format(string(Cmd), "basename ~w", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Name).

% Get directory from path
dirname(Path, Dir) :-
    format(string(Cmd), "dirname ~w", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Dir).
```

---

## 8. Process Multiple Files

### Apply command to each found file
```prolog
% Get line counts for all matching files
find_with_linecount(Dir, Pattern, Results) :-
    format(string(Cmd), "find ~w -name '~w' -type f -exec wc -l {} \;", [Dir, Pattern]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Results).
```

### Using xargs for batch processing
```prolog
% Process list of files with xargs
xargs_wc(Files, Result) :-
    atomic_list_concat(Files, ' ', FileList),
    format(string(Cmd), "echo '~w' | xargs wc -l", [FileList]),
    exec(vm_exec(command: Cmd), R),
    get_dict(stdout, R, Result).
```

---

## 9. Combining Patterns

### Find TypeScript files and read first lines
```prolog
% Get preview of each TS file in a directory
preview_ts_files(Dir, Previews) :-
    find_by_name(Dir, "*.ts", Files),
    maplist(file_preview, Files, Previews).

file_preview(Path, preview(Path, FirstLines)) :-
    read_head(Path, 5, FirstLines).
```

### Search and count occurrences
```prolog
% Count occurrences of pattern in a file
count_occurrences(Path, Pattern, Count) :-
    format(string(Cmd), "grep -c '~w' ~w || echo 0", [Pattern, Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(stdout, Result, Output),
    atom_string(Output, CountStr),
    number_string(Count, CountStr).
```

---

## 10. Error Handling

### Safe file read with fallback
```prolog
% Read file or return default on error
safe_read_file(Path, Content, Default) :-
    format(string(Cmd), "cat ~w 2>/dev/null", [Path]),
    exec(vm_exec(command: Cmd), Result),
    get_dict(exitCode, Result, ExitCode),
    (ExitCode = 0 
    -> get_dict(stdout, Result, Content)
    ;  Content = Default).
```

### Check command success
```prolog
% Execute and check if successful
exec_check(Cmd, Output) :-
    exec(vm_exec(command: Cmd), Result),
    get_dict(exitCode, Result, ExitCode),
    ExitCode = 0,
    get_dict(stdout, Result, Output).

exec_check(Cmd, error(Stderr)) :-
    exec(vm_exec(command: Cmd), Result),
    get_dict(exitCode, Result, ExitCode),
    ExitCode \= 0,
    get_dict(stderr, Result, Stderr).
```

---

## Quick Reference

| Operation | Command Pattern |
|-----------|-----------------|
| List files | `ls {dir}` |
| Find by name | `find {dir} -name '{pattern}' -type f` |
| Find shallow | `find {dir} -maxdepth 1 -name '{pattern}'` |
| Read file | `cat {path}` |
| Read head | `head -{n} {path}` |
| Read tail | `tail -{n} {path}` |
| Read range | `sed -n '{start},{end}p' {path}` |
| Grep | `grep '{pattern}' {path}` |
| Grep recursive | `grep -rn '{pattern}' {dir}` |
| File exists | `test -f {path}` |
| Dir exists | `test -d {path}` |
| File size | `stat -c %s {path}` |
| Line count | `wc -l < {path}` |
| Basename | `basename {path}` |
| Dirname | `dirname {path}` |
| Count files | `find {dir} -name '{pattern}' -type f \| wc -l` |

## Notes

1. **Path prefix**: All paths should use `/workspace/` prefix (mounted workspace)
2. **Error handling**: Use `|| true` to prevent grep from failing when no matches
3. **String parsing**: Use `split_string/4` to convert output to lists
4. **Alpine Linux**: The VM runs BusyBox - some GNU options may not be available
5. **Quoting**: Be careful with special characters in patterns
