/**
 * Harness tools for the REPL authoring agent.
 *
 * Unlike the old meta tools (which ran inside the Prolog meta frame), these are
 * plain host tools the TypeScript `HarnessAgent` calls directly. They operate on
 * the live harness through a `HarnessToolController` implemented by `DMLSession`.
 */

import type { DMLEvent, ToolDefinition } from '../types.js';
import type { DiagramMode } from '../visualize/index.js';

export interface HarnessExecuteResult {
  bindings: Array<{ name: string; value: string }>;
  solutions: Array<Array<{ name: string; value: string }>>;
  output: string;
  error?: string;
  /** Wall-clock duration of the goal, in milliseconds. */
  elapsedMs: number;
  /**
   * What was left out of the bindings, when not all of them were returned.
   * Without it a shortened value is indistinguishable from a short one, and an
   * omitted binding looks like a goal that bound nothing.
   */
  bindingsNote?: string;
}

/**
 * How much of each binding to return.
 *
 * A goal often binds one large term — a document, a nested dict — and the full
 * value is noise in the transcript and in the model's context. `brief` is for
 * "I want to know it bound something that looks like this"; `none` is for
 * "I only care that it succeeded and what it printed".
 */
export type BindingMode = 'full' | 'brief' | 'none';

/** Characters kept per value in `brief` mode. */
export const BRIEF_VALUE_CHARS = 80;

/** Read the `bindings` option; anything unrecognised keeps the full values. */
export function readBindingMode(value: unknown): BindingMode {
  return value === 'brief' || value === 'none' ? value : 'full';
}

export interface HarnessCommitResult {
  committed: boolean;
  /** Predicates added/replaced/removed by the commit. */
  changed?: string[];
  /** Advisory warnings (e.g. a nondeterministic helper). */
  warnings?: string[];
  error?: string;
}

/** The harness operations the authoring agent needs. Implemented by `DMLSession`. */
export interface HarnessToolController {
  isFrozen(): boolean;
  /** Read the harness: `all`, `Name/Arity`, `tools`, or a name prefix. */
  listing(filter: string): Promise<string>;
  /** Concise harness manifest for the system prompt. */
  manifest(): Promise<string>;
  /** Snapshot the harness and open a transaction. */
  beginTurn(): Promise<{ ok: boolean; error?: string }>;
  /** Validate and commit the open transaction; records a history version. */
  endTurn(): Promise<HarnessCommitResult>;
  /** Restore the last snapshot and discard edits. */
  rollbackTurn(): Promise<void>;
  /** Replace a predicate with the given complete clause block. */
  define(head: string, clauses: string): Promise<{ ok: boolean; error?: string }>;
  /** Assert one clause (new predicates only). */
  assertClause(clause: string): Promise<{ ok: boolean; error?: string }>;
  /** Assert a block of clauses in order. */
  assertMany(clauses: string): Promise<{ ok: boolean; error?: string }>;
  /** Retract matching clauses. */
  retractClause(clause: string): Promise<{ ok: boolean; error?: string }>;
  /** Delete a predicate by `Name/Arity`. */
  deletePredicate(nameArity: string): Promise<{ ok: boolean; error?: string }>;
  /** Run a DML goal against the scratch harness. */
  execute(
    goal: string,
    limit?: number,
    signal?: AbortSignal,
    onEvent?: (event: DMLEvent) => void,
    bindingMode?: BindingMode,
  ): Promise<HarnessExecuteResult>;
  /** ASCII diagram of a skill and its sub-agent/tool calls. */
  visualize(entry?: string, mode?: DiagramMode): Promise<{ message: string; event?: DMLEvent }>;
  /** Display a workspace image in the terminal. */
  showImage(path: string): Promise<{ message: string; event?: DMLEvent }>;
  /** Generate an image from a prompt and display it. */
  generateImage(prompt: string): Promise<{ message: string; event?: DMLEvent }>;
  /** Write a workspace file (creates parent directories). */
  writeFile(path: string, content: string): Promise<{ ok: boolean; error?: string }>;
  /** DML reference / authoring guide / examples / snippets. */
  reference(topic: string): Promise<string>;
  /** The models available in this session (for `with_model/2`). */
  listModels(): Promise<string>;
}

function text(message: string): Record<string, unknown> {
  return { text: message };
}

const stringProp = (description: string): { type: string; description: string } => ({ type: 'string', description });

/** Build the authoring agent's harness tools. */
export function createHarnessTools(controller: HarnessToolController): Map<string, ToolDefinition> {
  const tools = new Map<string, ToolDefinition>();
  const frozenError = (name: string): { ok: false; error: string } | undefined =>
    controller.isFrozen() ? { ok: false, error: `harness is frozen; ${name} is disabled` } : undefined;

  tools.set('listing', {
    description: 'Read the harness: all predicates (filter "all"), one predicate ("Name/Arity"), "tools", "scripts", or a name prefix. Call this before editing.',
    parameters: {
      type: 'object',
      properties: { filter: stringProp('all (default), a Name/Arity, tools, scripts, or a name prefix.') },
      required: [],
    },
    execute: async (args) => text(await controller.listing(String(args.filter ?? 'all').trim() || 'all')),
  });

  tools.set('begin_turn', {
    description: 'Open an edit transaction (snapshot the harness). You MUST call this before the first edit; `define`/`assert`/`assert_many`/`retract`/`delete` are refused while no transaction is open. `end_turn` commits it, `rollback` discards it.',
    parameters: { type: 'object', properties: {}, required: [] },
    execute: async () => {
      return controller.beginTurn();
    },
  });

  tools.set('end_turn', {
    description: 'Validate and commit the open transaction (opened by begin_turn) and record a harness version. Returns the predicates changed and any advisory warnings (e.g. a nondeterministic helper).',
    parameters: {
      type: 'object',
      properties: { summary: stringProp('One-line summary of the change (optional).') },
      required: [],
    },
    execute: async (_args, context) => {
      const result = await controller.endTurn();
      if (result.warnings?.length) {
        context?.onEvent?.({
          type: 'output',
          content: result.warnings.map((warning) => `⚠ ${warning}`).join('\n'),
        });
      }
      return result;
    },
  });

  tools.set('rollback', {
    description: 'Discard every edit made in the current transaction and restore the last committed harness.',
    parameters: { type: 'object', properties: {}, required: [] },
    execute: async () => {
      await controller.rollbackTurn();
      return { ok: true };
    },
  });

  tools.set('define', {
    description: 'Replace the predicate(s) defined by a clause block: remove every predicate the block defines (plus the head pattern) and assert the block in order. Requires an open transaction (call begin_turn first). Use this for any change to existing predicates — a block may define several.',
    parameters: {
      type: 'object',
      properties: {
        head: stringProp('A head pattern naming the predicate, e.g. "describe_image(Path, Text)".'),
        clauses: stringProp('The complete clause block, in order, each clause ending with a period.'),
      },
      required: ['head', 'clauses'],
    },
    execute: async (args) => frozenError('define') ?? controller.define(String(args.head ?? ''), String(args.clauses ?? '')),
  });

  tools.set('assert', {
    description: 'Assert one clause of a NEW predicate. Requires an open transaction (call begin_turn first). Refused if the predicate already has clauses: use define to replace it, or delete first.',
    parameters: {
      type: 'object',
      properties: { clause: stringProp('The DML clause/fact text to assert (no trailing period required).') },
      required: ['clause'],
    },
    execute: async (args) => frozenError('assert') ?? controller.assertClause(String(args.clause ?? '')),
  });

  tools.set('assert_many', {
    description: 'Assert a block of clauses in the order written. Requires an open transaction (call begin_turn first). Use this to define a whole predicate so clause order (base case before recursive rule) is preserved.',
    parameters: {
      type: 'object',
      properties: { clauses: stringProp('The DML clauses/facts, in order, each ending with a period.') },
      required: ['clauses'],
    },
    execute: async (args) => frozenError('assert_many') ?? controller.assertMany(String(args.clauses ?? '')),
  });

  tools.set('retract', {
    description: 'Remove the clauses matching a pattern, e.g. retract("f(1)") or retract("f(X) :- g(X)"). Reports how many were removed. Requires an open transaction (call begin_turn first). Reserved names are protected.',
    parameters: {
      type: 'object',
      properties: { clause: stringProp('The clause pattern to retract (no trailing period required).') },
      required: ['clause'],
    },
    execute: async (args) => frozenError('retract') ?? controller.retractClause(String(args.clause ?? '')),
  });

  tools.set('delete', {
    description: 'Delete a predicate entirely (abolishes all its clauses), e.g. delete("describe_image/2"). Requires an open transaction (call begin_turn first).',
    parameters: {
      type: 'object',
      properties: { predicate: stringProp('The predicate indicator to delete, e.g. "describe_image/2".') },
      required: ['predicate'],
    },
    execute: async (args) => frozenError('delete') ?? controller.deletePredicate(String(args.predicate ?? '')),
  });

  tools.set('execute', {
    description: 'Run a DML goal against the scratch harness and return its bindings and output. Use this to test changes before committing.',
    parameters: {
      type: 'object',
      properties: {
        goal: stringProp('The DML goal to run, e.g. "fact(5, F)".'),
        limit: { type: 'integer', description: 'Maximum solutions to collect (default 5).' },
        bindings: stringProp(
          "How much of each binding to return. 'full' (default) returns the values as they are; "
          + "'brief' keeps the names and shortens each value, for a goal that binds one large term; "
          + "'none' returns no bindings at all, for when you only care that the goal succeeded or what it printed. "
          + 'Both reduced modes say what they left out.',
        ),
      },
      required: ['goal'],
    },
    execute: async (args, context) => controller.execute(
      String(args.goal ?? ''),
      typeof args.limit === 'number' ? args.limit : undefined,
      context?.signal,
      context?.onEvent,
      readBindingMode(args.bindings),
    ),
  });

  tools.set('visualize', {
    description: 'Render a diagram of a skill (predicate) showing its model calls (task/prompt), tool calls (exec), and other skills it invokes. In a terminal that supports images the diagram is displayed to the user directly; otherwise the ASCII tree is returned so you can show it. The entry defaults to agent_main/N.',
    parameters: {
      type: 'object',
      properties: {
        entry: stringProp('Entry predicate, e.g. "patch/3" (default: agent_main/N).'),
        mode: stringProp('presentation (default, reader-friendly) or spec (engineering detail).'),
      },
      required: [],
    },
    execute: async (args, context) => {
      const { message, event } = await controller.visualize(
        typeof args.entry === 'string' ? args.entry : undefined,
        args.mode === 'spec' ? 'spec' : args.mode === 'presentation' ? 'presentation' : undefined,
      );
      if (event) context?.onEvent?.(event);
      return text(message);
    },
  });

  tools.set('write_file', {
    description: 'Write a workspace file with the given content (parent directories are created). Use this for SVG, JSON, scripts, or any file whose content is easier to pass whole than to build inside a DML string. The content argument is an ordinary string — no DML/shell escaping.',
    parameters: {
      type: 'object',
      properties: {
        path: stringProp('Workspace-relative path, e.g. "images/foo.svg".'),
        content: stringProp('The complete file content. Newlines and quotes are passed through verbatim.'),
      },
      required: ['path', 'content'],
    },
    execute: async (args) => controller.writeFile(String(args.path ?? ''), String(args.content ?? '')),
  });

  tools.set('show_image', {
    description: 'Display a workspace image (PNG/JPEG/GIF/WebP/SVG) to the user in the terminal. Use this when the user asks to see an image, or after writing one with save_image/write_file.',
    parameters: {
      type: 'object',
      properties: { path: stringProp('Workspace-relative path of the image to display.') },
      required: ['path'],
    },
    execute: async (args, context) => {
      const { message, event } = await controller.showImage(String(args.path ?? ''));
      if (event) context?.onEvent?.(event);
      return text(message);
    },
  });

  tools.set('generate_image', {
    description: 'Generate an image from a text prompt (using the configured image model) and display it to the user. Returns the workspace path of the written image.',
    parameters: {
      type: 'object',
      properties: { prompt: stringProp('What to depict (be specific: subject, style, composition).') },
      required: ['prompt'],
    },
    execute: async (args, context) => {
      const { message, event } = await controller.generateImage(String(args.prompt ?? ''));
      if (event) context?.onEvent?.(event);
      return text(message);
    },
  });

  tools.set('models', {
    description: 'List the LLM models available in this session (those with configured credentials) and the current default. Call this before writing `with_model(Spec, Goal)` so you use a real provider:model id.',
    parameters: { type: 'object', properties: {}, required: [] },
    execute: async () => text(await controller.listModels()),
  });

  tools.set('reference', {
    description: 'Read DeepClause language documentation. Topics: "language" (DML reference), "authoring" (authoring guide), "patterns" (harness patterns by use case), "examples" (harness examples), "snippets" (Prolog snippets), "tools" (host tool catalog).',
    parameters: {
      type: 'object',
      properties: { topic: stringProp('language | authoring | examples | snippets | tools.') },
      required: ['topic'],
    },
    execute: async (args) => text(await controller.reference(String(args.topic ?? 'language'))),
  });

  return tools;
}
