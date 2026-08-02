---
name: DeepClause Coding Workflow
description: Guidance for implementing and validating local repository changes in a DeepClause workspace.
tags: [coding, repository, tests, docs, deepclause]
when_to_use:
  - tasks that require making specific changes to repository files with clear validation steps
  - debugging and fixing a local behavior with focused edits and tests
  - updating documentation to reflect code changes in the same commit
  - work that is not intended to become a reusable DML skill but is still important to track and execute in a controlled way
  - making focused code changes with clear validation steps
  - implementing a feature or bug fix in the current repository
  - updating tests and documentation alongside code changes
  - debugging a local behavior before editing files
when_not_to_use:
  - purely informational web research
  - work that should become a reusable executable skill instead of a one-off repo change
globs: [src/**, tests/**, README.md, docs/**]
priority: high
---

# Goal

Make small, grounded repository changes with focused validation and clear outcomes.

# Workflow

1. Start from a concrete anchor such as a failing test, a named file, a command, or the nearest implementation surface.
2. Read only enough nearby code to form one falsifiable local hypothesis about what should change.
3. Prefer the smallest edit that tests that hypothesis.
4. Run the narrowest validation that can falsify the change before widening scope.
5. If the validation fails, repair the same slice first instead of opening new work.
6. Update documentation when behavior, workflow, or user-visible commands changed.

# When To Use

Use this recipe when the task is primarily about changing repository files, adding tests, or updating documentation in a controlled way.

# When Not To Use

Do not use this recipe for pure research or when the right outcome is to create a reusable DML worker skill.

# Examples

- Implement a narrow bug fix in `src/` and validate it with one focused Vitest file.
- Update a CLI workflow and document the changed behavior in `README.md`.
- Add a new config seed and verify it with the corresponding init tests.

# Escalation

If the task repeats often or clearly represents a reusable automation, stop treating it as a one-off workflow and create a proper skill instead.


# Editing Protocol

As a coding agent, you will navigate and modify files strictly using non-interactive Unix command-line utilities. You must execute edits precisely and safely, treating the filesystem as fragile. Never attempt to use interactive text editors like vim or nano.

1. Core Editing Directives
Context is Mandatory: Always read the target file or specific lines (using cat, head, tail, or sed -n) before applying changes. You must know exactly what you are modifying.

Precision over Broadness: Never use loose regex that might accidentally match unintended parts of a file. Target specific line numbers or highly unique strings.

Immediate Verification: Immediately verify your changes after an edit using git diff (if in a repository) or by re-reading the modified section to ensure the code didn't break.

Quote Appropriately: Always escape special characters appropriately in your bash commands to prevent syntax errors or unintended variable expansions.

2. Locating Context (grep & sed)
Before editing, find the exact line numbers and context.

Find line numbers: grep -n "target string" filename

View surrounding context: grep -C 3 "target string" filename (Shows 3 lines before and after).

View a specific line range: sed -n '10,25p' filename (Prints lines 10 through 25).

3. Modifying Code (sed)
Use sed for targeted in-place text replacement, deletion, or insertion. Always use the -i flag for in-place editing (Note: if the environment is macOS/BSD, you must use sed -i '').

Targeted substitution (by line number): sed -i '42s/old_variable/new_variable/' filename

Global substitution (with extreme caution): sed -i 's/old_string/new_string/g' filename

Delete specific lines: sed -i '42,45d' filename (Deletes lines 42 through 45).

Insert text after a specific line: sed -i '42a\    new_code_line();' filename

4. Writing and Appending (bash & Heredocs)
For generating new files, replacing entire files, or appending large multi-line blocks of code, avoid complex sed commands and use Bash redirects.

Append a single line: echo "export DEBUG=true" >> filename

Overwrite a file completely: echo "new content" > filename

Write multi-line blocks (Heredocs): Use quoted heredocs ('EOF') to prevent the shell from accidentally evaluating variables like $1 or $VAR inside the code block you are writing.

# Project Structure

The workspace is the current PWD. All commands must be relative to this path. You have access to all files in the workspace, but you should only read and modify files relevant to the current task. Use git commands to check the status of the repository and ensure you are not making unintended changes.

# Temporary files
Do not use /tmp. Instead, create temporary files in a new .deepclause.tmp folder in the current workspace with unique names to avoid conflicts.