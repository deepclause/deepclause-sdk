/**
 * Slash-command completion for the REPL editor.
 *
 * Pi's `CombinedAutocompleteProvider` does the work: it matches commands by prefix
 * and, where a base path is given, completes file paths too.
 *
 * The list is *not* what the CLI parses — `classifyReplInput` has its own switch and
 * reads nothing from here — so the two drift, and a command that parses but is not
 * listed is invisible to the user. `/reasoning` and `/show-reasoning` were exactly
 * that. `tests/repl-tui-pickers.test.ts` now checks both directions: every advertised
 * command must parse, and every command the parser accepts must be advertised (or be
 * a deliberate alias).
 */

import {
  CombinedAutocompleteProvider,
  type AutocompleteProvider,
  type SlashCommand,
} from '@earendil-works/pi-tui';

/** The REPL's slash commands, with the argument each takes. */
export const REPL_COMMANDS: SlashCommand[] = [
  { name: 'goal', description: 'Run a DML goal', argumentHint: '<dml>' },
  { name: 'help', description: 'Show help' },
  { name: 'listing', description: 'List predicates', argumentHint: '[all|Name/Arity|prefix]' },
  { name: 'viz', description: 'Draw a skill as a diagram', argumentHint: '<entry> [presentation|spec]' },
  { name: 'export', description: 'Save the harness to a file', argumentHint: '[path]' },
  { name: 'model', description: 'Switch or show the active model', argumentHint: '[provider:id]' },
  { name: 'reasoning', description: 'Set the model reasoning level', argumentHint: '[off|minimal|low|medium|high|xhigh|max]' },
  { name: 'show-reasoning', description: 'How much reasoning to display', argumentHint: '[off|summary|full]' },
  { name: 'harness', description: 'Toggle the live harness side view', argumentHint: '[20-120|wider|narrower]' },
  { name: 'reset', description: 'Clear the harness' },
  { name: 'rollback', description: 'Restore the previous harness version' },
  { name: 'compact', description: 'Compact the conversation', argumentHint: '[instructions]' },
  { name: 'freeze', description: 'Refuse further edits' },
  { name: 'unfreeze', description: 'Allow edits again' },
  { name: 'params', description: 'Show session parameters' },
  { name: 'spawn', description: 'Start a session-scoped subagent', argumentHint: '<goal>' },
  { name: 'ps', description: 'List subagents (process table)', argumentHint: '[--watch] [--logs]' },
  { name: 'send', description: 'Send a DML term to a subagent', argumentHint: '<name> <term>' },
  { name: 'call', description: 'Request/reply with a service subagent', argumentHint: '<name> <term>' },
  { name: 'stop', description: 'Stop a subagent', argumentHint: '<name>' },
  { name: 'trace', description: 'Turn the execution trace on' },
  { name: 'notrace', description: 'Turn the execution trace off' },
  { name: 'show', description: 'Display a workspace image', argumentHint: '<path>' },
  { name: 'gen', description: 'Generate an image', argumentHint: '<prompt>' },
  { name: 'halt', description: 'Exit' },
];

/** Build the editor's autocomplete provider, with file completion under `cwd`. */
export function createReplAutocomplete(cwd: string): AutocompleteProvider {
  return new CombinedAutocompleteProvider(REPL_COMMANDS, cwd);
}
