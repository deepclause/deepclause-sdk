# DeepClause TUI Guide

## Overview

Running `deepclause` with no subcommand starts the fullscreen CLI TUI.

The TUI is the operator interface around a single built-in agent: the conductor.

- The left pane shows sessions.
- The center pane shows the conversation transcript.
- The upper-right pane shows live execution activity.
- The lower-right pane shows context, token usage, system asset sources, and shell execution details.

The local `.deepclause/docs/` folder contains editable workspace docs such as `TUI.md` and `DML_REFERENCE.md`. The system `plan` skill reads `DML_REFERENCE.md` and `.deepclause/system/DML_COMPILER_PROMPT.md` before generating new plan files.

## Command Bar

The command bar accepts three main input styles plus direct skill execution:

- Plain text sends a normal request to the conductor.
- `/...` runs a built-in slash command.
- `/<skill> [args]` runs a compiled local skill directly.
- `/plan <request>` runs the system `plan` skill from `.deepclause/system/plan.dml`.
- `/<plan> [args]` runs `plans/<plan>.dml` when no compiled local skill matches that name.
- `/run <file> [args]` runs a workspace DML file directly.
- `!<command>` runs a shell command directly in the configured workspace shell.

Examples:

```text
summarize the latest test failures
/sessions
/set-model openai:gpt-4.1 --slot run
/plan create a simple repo cleanup workflow
/run plans/repo_cleanup.dml
/compile create a skill that reviews changelogs
/deep-research transformers scaling laws
!git status
!npm test
```

Use `\!text` if you need to send a normal conductor prompt that starts with a literal `!`.

## Slash Commands

Common built-in slash commands:

- `/new [title]` creates a new conductor session.
- `/sessions` refreshes or switches sessions.
- `/set-model <model> [--slot <slot>]` updates the configured model for all slots or one slot.
- `/plan <request>` runs the system plan skill and writes a simple standalone plan under `plans/`.
- `/run <file> [args]` runs a workspace DML file directly.
- `/compile <spec>` runs the skill creator.
- `/skill-creator <spec>` is an alias for `/compile`.
- `/cancel` aborts the current running task, skill, or shell command.
- `/help` prints command help into the execution pane.
- `/quit` or `/exit` closes the TUI.

Any other `/name` is treated as a compiled skill invocation if that skill exists in the local catalog.
If no compiled skill matches, the TUI looks for `plans/<name>.dml` and runs that file directly.

## Direct Shell Mode

`!<command>` is a TUI-local execution mode. It does not go through the conductor.

- The shell command runs immediately.
- Output streams live into the execution pane.
- The process pane shows running state, PID when available, and host vs sandbox backend.
- `/cancel` and `Ctrl+C` stop the running shell command.
- The skill catalog refreshes after the shell command finishes, so newly created skills can appear without restarting the TUI.

If the TUI is running with sandbox mode enabled, shell commands use the AgentVM sandbox instead of the host shell.

## Menus and Palette

The TUI includes a Borland-style menu bar and an action palette.

- `F10` or `Ctrl+G` opens the menu bar.
- `Ctrl+P` opens the action palette.
- The Run menu includes prompt execution, shell command entry, repeat last command, cancellation, and skill catalog refresh.
- The View menu can move focus between panes.
- The Files and Skills menus open searchable pickers.

## Navigation and Keys

Important keys:

- `Left` and `Right` switch the focused pane when the command bar is empty.
- `Up` and `Down` select sessions or scroll the focused pane.
- `PgUp` and `PgDn` page-scroll the focused pane.
- `End` jumps back to the newest visible content.
- `Tab` autocompletes slash commands and skill names.
- `Ctrl+W` cycles focus across panes.

## Execution Pane

The execution pane is the main observability surface.

It can show:

- conductor streaming output
- child-skill activity
- tool lifecycle events
- live shell stdout and stderr
- active tool status, including PID when available

This is where to look when a task seems stuck, is waiting for clarification, or is running shell or tool-heavy work.

## Context Pane

The context pane shows session and runtime state, including:

- approximate context and token usage
- resolved source paths for conductor and skill-creator DML/prompt assets
- the active shell backend and shell working directory

Changes to `.deepclause/system/` overrides are picked up on the next conductor turn or skill-creator run. A dedicated reload action is not required.

## Sessions and Storage

Each session lives under `.deepclause/sessions/<session-id>/`.

Typical files include:

- `metadata.json`
- `messages.jsonl`
- `assistant-memory.md`
- `task-memory.md`
- `usage.json`

The TUI loads these sessions into the session list and shows their messages in the transcript pane.

## Relationship to Skills

The conductor is not the same thing as a compiled skill.

- The conductor handles freeform user requests and orchestration.
- Compiled skills are reusable DML workers in `.deepclause/tools/`.
- The `plan` system skill lives at `.deepclause/system/plan.dml` and follows the same override model as the other system DML assets.
- `/set-model` updates the configured gateway, run, and compile model selection for future turns, or only one slot when `--slot` is provided.
- `/compile` and `/skill-creator` use the skill creator to turn a spec into a validated local skill.

If a user asks how to automate something repeatedly, the right TUI workflow is usually to use `/compile <spec>` rather than repeating the same one-off prompt manually.

## Troubleshooting

- If a command appears to be stuck, check the execution pane for active tool status or a clarification request.
- If shell commands do not behave as expected, check the context pane for the current shell backend and working directory.
- If a newly created skill does not appear, refresh the skill catalog or run a new turn after the command completes.
- If a system prompt or system DML change does not seem active, verify the source paths shown in the context pane.