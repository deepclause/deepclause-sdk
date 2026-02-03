# Code Review (Multi-File with Summary)

Version 3: Review multiple files and produce a unified report.

## Arguments

- Directory: Path to directory to review
- FilePattern: Glob pattern for files (e.g., "*.ts")

## Tools needed

- vm_exec (to list and read files)

## Behavior

1. List all files matching the pattern in the directory
2. For each file:
   - Perform security analysis
   - Check for bugs
   - Note style issues
3. Cross-reference issues across files (e.g., inconsistent patterns)
4. Generate a unified report with:
   - Per-file summaries
   - Cross-cutting concerns
   - Priority-ordered action items

## Output Format

```
# Code Review Report

## Executive Summary
- Files reviewed: N
- Critical issues: N
- Files needing attention: list

## Per-File Analysis

### filename.ts
- Security: [status]
- Bugs: N found
- Key issues: ...

### filename2.ts
...

## Cross-Cutting Concerns
- Inconsistencies found across files
- Patterns that should be standardized

## Action Items (Priority Order)
1. [CRITICAL] ...
2. [HIGH] ...
3. [MEDIUM] ...
```

## Example

Input: `review ./src "*.ts"`

Expected behavior:
- Lists all .ts files in ./src
- Reviews each file
- Identifies cross-file issues (e.g., different error handling patterns)
- Returns prioritized action items
