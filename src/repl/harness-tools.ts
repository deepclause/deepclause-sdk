/**
 * Harness tools for the REPL authoring agent.
 *
 * Unlike the old meta tools (which ran inside the Prolog meta frame), these are
 * plain host tools the TypeScript `HarnessAgent` calls directly. They operate on
 * the live harness through a `HarnessToolController` implemented by `DMLSession`.
 */

import type { DMLEvent, ShellSpec, ShellWorkflow, ToolDefinition } from '../types.js';
import type { DiagramMode } from '../visualize/index.js';
import type { ReferenceOptions } from './reference.js';
import { executeWorkEntry, validateWorkShell, workToolParameters } from '../work/index.js';

import type { Pid, ProcessInfo, SpawnOptions } from './processes.js';
import { pidTermText } from './processes.js';

export interface HarnessExecuteResult {
  bindings: Array<{ name: string; value: string }>;
  solutions: Array<Array<{ name: string; value: string }>>;
  output: string;
  error?: string;
  /** Wall-clock duration of the goal, in milliseconds. */
  elapsedMs: number;
  /** Explicit terminal outcome; an ended iterator is not necessarily success. */
  outcome?: 'succeeded' | 'failed' | 'cancelled' | 'suspended';
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
  /** Read the harness: `all`, `Name/Arity`, `tools`, `harnesses`, or `harness:Name`. */
  listing(filter: string): Promise<string>;
  /** Add a read-only foreign harness import inside the current edit transaction. */
  useHarness(name: string, predicates: string[], version?: string | number): Promise<{ ok: boolean; error?: string }>;
  /** The harness Work shell, when one is declared. */
  shell(): ShellSpec | null;
  /** Concise harness manifest for the system prompt. */
  manifest(): Promise<string>;
  /** Snapshot the harness and open a transaction. */
  beginTurn(): Promise<{ ok: boolean; error?: string; message?: string }>;
  /** Validate and commit the open transaction; records a history version. */
  endTurn(): Promise<HarnessCommitResult>;
  /** Restore the last snapshot and discard edits. */
  rollbackTurn(): Promise<void>;
  /** Replace a predicate with the given complete clause block. */
  define(head: string, clauses: string): Promise<{ ok: boolean; error?: string }>;
  /** Delete a predicate by `Name/Arity`. */
  deletePredicate(nameArity: string): Promise<{ ok: boolean; error?: string }>;
  /** Run a DML goal against the scratch harness. */
  execute(
    goal: string,
    limit?: number,
    signal?: AbortSignal,
    onEvent?: (event: DMLEvent) => void,
    bindingMode?: BindingMode,
    /** Optional wall-clock bound for host-integrated execute workers (ms). */
    timeoutMs?: number,
  ): Promise<HarnessExecuteResult>;
  /** Run a workflow goal in the top-level session so forms/input are available. */
  executeForeground(
    goal: string,
    limit?: number,
    signal?: AbortSignal,
    onEvent?: (event: DMLEvent) => void,
    bindingMode?: BindingMode,
    timeoutMs?: number,
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
  reference(topic: string, options?: ReferenceOptions): Promise<string>;
  /** The models available in this session (for `with_model/2`). */
  listModels(): Promise<string>;
  /** Start a session-scoped subagent; returns immediately. */
  spawnProcess(goal: string, options?: SpawnOptions): Promise<Pid>;
  /** The whole process state. */
  processes(): ProcessInfo[];
  /** Info for one process, for inspection. */
  processInfo(pid: Pid): ProcessInfo | undefined;
  /** The harness source a process is running (its spawn-time snapshot). */
  processCode(target: Pid | string): string | undefined;
  /** The harness version this session is currently editing. */
  currentHarnessVersion(): string;
  /** Resolve a registered process name. */
  whereisProcess(name: string): Pid | undefined;
  /** Copy a message term into a process's mailbox. */
  sendProcess(to: Pid | string, messageTerm: string): void;
  /** Request/reply against a service process. */
  callProcess(to: Pid | string, messageTerm: string, timeoutMs?: number): Promise<string>;
  /** Stop a process by pid or registered name. */
  stopProcess(target: Pid | string, reason?: unknown): Promise<void>;
}

function text(message: string): Record<string, unknown> {
  return { text: message };
}

/** A compact, model-readable table of the process state. */
export function formatProcessTable(processes: ProcessInfo[], currentVersion?: string): string {
  if (processes.length === 0) return 'no subagents';
  return processes
    .map((p) => {
      const usage = p.usage.totalTokens > 0 ? ` · ${p.usage.totalTokens} tok` : '';
      const activity = p.currentActivity ? ` · ${p.currentActivity}` : '';
      const error = p.error ? ` (${p.error})` : '';
      // The harness snapshot the process runs. `(old)` marks a process started
      // before the latest edit — the process is fine, its code is just older.
      const stale = currentVersion && p.harnessVersion && p.harnessVersion !== currentVersion;
      const code = p.harnessVersion ? ` · code ${p.harnessVersion.slice(0, 7)}${stale ? ' (old)' : ''}` : '';
      // `serve(Name, Goal)` labels a process by its goal functor but is reached by
      // its registered name, so show both: reading only the label is how a live
      // `test_service` looks like an anonymous `test_service_loop`.
      const aliases = (p.registeredAs ?? []).filter((name) => name !== p.name);
      const names = aliases.length > 0 ? ` (as ${aliases.join(', ')})` : '';
      return `${pidTermText(p.pid)}  ${p.name}${names}  ${p.state}${error}  mailbox ${p.mailboxDepth}${usage}${code}${activity}`;
    })
    .join('\n');
}

/** The slice of a session `formatProcessCode` needs. */
export interface ProcessCodeSource {
  whereisProcess(name: string): Pid | undefined;
  processes(): ProcessInfo[];
  processCode(target: Pid | string): string | undefined;
  processInfo(pid: Pid): ProcessInfo | undefined;
  currentHarnessVersion(): string;
}

/**
 * Render the harness snapshot one process is running, for `/ps --code`.
 *
 * With no target it reports the session's current harness version, which turns
 * the question "is this worker stale?" into a version comparison.
 */
export function formatProcessCode(source: ProcessCodeSource, target: string): string {
  if (!target) return `current harness code ${source.currentHarnessVersion()}`;
  const pid = source.whereisProcess(target)
    ?? source.processes().find((p) => pidTermText(p.pid) === target || p.pid.id === target)?.pid;
  const code = pid ? source.processCode(pid) : undefined;
  if (!pid || !code) return `no harness snapshot for ${target}`;
  const info = source.processInfo(pid);
  return `${pidTermText(pid)}  ${info?.name ?? target}  code ${info?.harnessVersion ?? '?'}\n${code}`;
}

const stringProp = (description: string): { type: string; description: string } => ({ type: 'string', description });

function usageLine(entry: ShellWorkflow): string {
  const args = [
    ...entry.inputs.map((arg) => `${arg.name}: ${arg.type}`),
    ...entry.outputs.map((arg) => `-> ${arg.name}: ${arg.type}`),
  ];
  return `${entry.goal}(${args.join(', ')})`;
}

/**
 * Tools for a Work agent. The model sees declared harness capabilities and
 * workflows, never raw DML or the authoring/execute tools.
 */
export function createWorkAgentTools(controller: HarnessToolController): Map<string, ToolDefinition> {
  const shell = controller.shell();
  const tools = new Map<string, ToolDefinition>();
  if (!shell) return tools;
  const invalid = validateWorkShell(shell);
  if (invalid.length) throw new Error(`Invalid Work shell: ${invalid.join('; ')}`);
  const base = createHarnessTools(controller);
  const showImage = base.get('show_image');
  if (showImage && shell.hostTools.includes('show_image')) tools.set('show_image', showImage);

  for (const tool of shell.tools) {
    if (tools.has(tool.name)) continue;
    tools.set(tool.name, {
      description: tool.description || `Run ${usageLine(tool)}`,
      parameters: workToolParameters(tool),
      execute: (args, context) => executeWorkEntry(tool, args,
        goal => controller.executeForeground(goal, 1, context?.signal, event => { if (event.type !== 'solution') context?.onEvent?.(event); }, 'full'),
        context?.signal),
    });
  }

  if (shell.workflows.length > 0) {
    const names = shell.workflows.map((workflow) => workflow.name);
    const descriptions = shell.workflows.map((workflow) => `- ${workflow.name}: ${usageLine(workflow)}`).join('\n');
    tools.set('run_workflow', {
      description: `Run a recurring harness workflow with its input values and return its outputs.\n${descriptions}`,
      parameters: {
        type: 'object',
        properties: {
          name: { type: 'string', enum: names, description: 'Workflow name.' },
          inputs: {
            type: 'object',
            description: 'Input values keyed by the workflow input names.',
          },
        },
        required: ['name'],
      },
      execute: async (args, context) => {
        const name = String(args.name ?? '');
        const workflow = shell.workflows.find((entry) => entry.name === name);
        if (!workflow) return { ok: false, status: 'failed', outputs: {}, output: '', elapsedMs: 0, error: `unknown workflow: ${name}` };
        const values = (args.inputs && typeof args.inputs === 'object' ? args.inputs : {}) as Record<string, unknown>;
        return executeWorkEntry(workflow, values,
          goal => controller.executeForeground(goal, 1, context?.signal, event => { if (event.type !== 'solution') context?.onEvent?.(event); }, 'full'),
          context?.signal);
      },
    });
  }
  return tools;
}

/** Build the authoring agent's harness tools. */
export function createHarnessTools(controller: HarnessToolController): Map<string, ToolDefinition> {
  const tools = new Map<string, ToolDefinition>();
  const frozenError = (name: string): { ok: false; error: string } | undefined =>
    controller.isFrozen() ? { ok: false, error: `harness is frozen; ${name} is disabled` } : undefined;

  tools.set('listing', {
    description: 'Read the current harness: "all", "Name/Arity", "tools", "scripts", or a name prefix. Use "harnesses" to list available harness modules, or "harness:Name" to inspect another harness read-only for reuse. Tool annotations are shown under their predicate.',
    parameters: {
      type: 'object',
      properties: { filter: stringProp('all (default), Name/Arity, tools, scripts, a name prefix, harnesses, or harness:Name.') },
      required: [],
    },
    execute: async (args) => text(await controller.listing(String(args.filter ?? 'all').trim() || 'all')),
  });

  tools.set('use_harness', {
    description: 'Make exported predicates from another harness available read-only. Inspect it first with listing("harness:Name"), then call this after begin_turn. The import is recorded as a :- use_harness(...) directive and committed by end_turn.',
    parameters: {
      type: 'object',
      properties: {
        name: stringProp('Harness slot name, lowercase letters/digits/underscores.'),
        predicates: stringProp('Comma-separated exported indicators, e.g. summarise/2,prioritise/2.'),
        version: { type: 'string', description: 'Optional published harness version.' },
      },
      required: ['name', 'predicates'],
    },
    execute: async (args) => controller.useHarness(
      String(args.name ?? ''),
      String(args.predicates ?? '').split(',').map((predicate) => predicate.trim()).filter(Boolean),
      typeof args.version === 'string' && args.version.trim() ? args.version.trim() : undefined,
    ),
  });

  tools.set('begin_turn', {
    description: 'Open the edit transaction (snapshot the harness). Call this once before the first edit; it is a no-op if the transaction is already open. `define`/`delete` are refused while no transaction is open. `end_turn` commits it, `rollback` discards it.',
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

  tools.set('delete', {
    description: 'Delete a predicate entirely (abolishes all its clauses), e.g. delete("describe_image/2"); a tool annotation with delete("tool:describe_image/2"); or one keyed annotation with delete("doc:describe_image/2"), delete("schema:Name"), or delete("script_file:Name"). Requires an open transaction (call begin_turn first).',
    parameters: {
      type: 'object',
      properties: { predicate: stringProp('The predicate indicator to delete, e.g. "describe_image/2".') },
      required: ['predicate'],
    },
    execute: async (args) => frozenError('delete') ?? controller.deletePredicate(String(args.predicate ?? '')),
  });

  tools.set('execute', {
    description: 'Run a DML goal against the scratch harness and return its bindings and output. Use this to test changes before committing. The host kills the scratch worker after the timeout (default 60s).',
    parameters: {
      type: 'object',
      properties: {
        goal: stringProp('The DML goal to run, e.g. "fact(5, F)".'),
        limit: { type: 'integer', description: 'Maximum solutions to collect (default 5).' },
        timeoutMs: { type: 'integer', description: 'Optional wall-clock bound in milliseconds (default 60000; max 600000). A timed-out goal is killed without wedging the session.' },
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
      typeof args.timeoutMs === 'number' && args.timeoutMs > 0
        ? Math.min(Math.round(args.timeoutMs), 600_000)
        : undefined,
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
    description: 'Read one focused DML contract before using an unfamiliar feature or after a contract error. With no section, returns an index; section ids select copyable bounded pages. Follow returned offset when More: yes. Topics: index, basics, language, tasks, shell, python, processes, media, forms, authoring, debugging, patterns, examples, snippets, tools (CURRENT host catalog and schemas).',
    parameters: {
      type: 'object',
      properties: {
        topic: stringProp('index | basics | language | tasks | shell | python | processes | media | forms | authoring | debugging | patterns | examples | snippets | tools'),
        section: stringProp('Exact section id from the topic index; do not invent ids.'),
        query: stringProp('Optional words to search within this topic instead of reading every section.'),
        offset: { type: 'integer', description: 'Block offset returned by a previous page. Omit for the first page.' },
      },
      required: ['topic'],
    },
    execute: async (args) => text(await controller.reference(String(args.topic ?? 'index'), {
      section: typeof args.section === 'string' ? args.section : undefined,
      query: typeof args.query === 'string' ? args.query : undefined,
      offset: typeof args.offset === 'number' ? args.offset : undefined,
    })),
  });

  // ---- Session-scoped subagents (spec §9.1) ------------------------------

  tools.set('ps', {
    description: 'List the session\'s subagents (id, name, state, mailbox depth, usage, and the harness code version each runs). Pass `code` to print the full harness snapshot a subagent is running — the way to check whether a worker is on stale code.',
    parameters: {
      type: 'object',
      properties: { code: stringProp('A process pid or registered name; omit to list. Pass "" for the current harness version.') },
      required: [],
    },
    execute: async (args) => text(
      args.code !== undefined
        ? formatProcessCode(controller, String(args.code))
        : formatProcessTable(controller.processes(), controller.currentHarnessVersion()),
    ),
  });

  tools.set('send', {
    description: 'Send a DML term to a subagent (by id or registered name). Fire-and-forget: use this for "text from the user" or a message to another process.',
    parameters: {
      type: 'object',
      properties: {
        to: stringProp('Process id (proc(...)) or registered name.'),
        message: stringProp('The DML term to send, for example the atom stop or a compound like request(a, b).'),
      },
      required: ['to', 'message'],
    },
    execute: async (args) => {
      try {
        controller.sendProcess(String(args.to ?? ''), String(args.message ?? 'true'));
        return { ok: true };
      } catch (error) {
        return { error: error instanceof Error ? error.message : String(error) };
      }
    },
  });

  tools.set('stop', {
    description: 'Stop a subagent (by id or registered name).',
    parameters: {
      type: 'object',
      properties: { target: stringProp('Process id or registered name.') },
      required: ['target'],
    },
    execute: async (args) => {
      await controller.stopProcess(String(args.target ?? ''));
      return { ok: true };
    },
  });

  tools.set('swarm', {
    description: 'Run a worker predicate over a list, each item in its own subagent, and return the results in input order. Prefer this to many spawn calls. Bound the list: each item is a process and usually a model call.',
    parameters: {
      type: 'object',
      properties: {
        worker: stringProp('Predicate name of arity +1: `worker(Item, Result)`, e.g. summarise_file.'),
        items: { type: 'array', description: 'DML terms, e.g. ["\"a.txt\"", "\"b.txt\""] or [1, 2, 3].' },
        concurrency: { type: 'integer', description: 'Max items running at once (default 4).' },
      },
      required: ['worker', 'items'],
    },
    execute: async (args, context) => {
      const worker = String(args.worker ?? '').trim();
      const items = Array.isArray(args.items) ? args.items.map((item) => String(item)) : [];
      if (!worker || items.length === 0) return { error: 'swarm requires a worker and a non-empty items list' };
      const concurrency = typeof args.concurrency === 'number' && args.concurrency > 0 ? `, ${args.concurrency}` : '';
      const goal = `swarm(${worker}, [${items.join(', ')}], DcSwarmResults${concurrency})`;
      const result = await controller.execute(goal, 1, context?.signal, context?.onEvent, 'full');
      if (result.error) return { error: result.error };
      const binding = result.bindings.find((entry) => entry.name === 'DcSwarmResults');
      return { results: binding?.value ?? result.output.trim() };
    },
  });

  tools.set('wait', {
    description: 'Wait for a subagent to finish and return its outcome: `ok(Result)` / `failed(Error)` / `killed(Reason)`. Blocks until it terminates; use only when you need the result. Result is the subagent terminal text.',
    parameters: {
      type: 'object',
      properties: { pid: stringProp('Process id from spawn, e.g. proc(main,s,p1), or a registered name.') },
      required: ['pid'],
    },
    execute: async (args, context) => {
      const pid = String(args.pid ?? '').trim();
      if (!pid) return { error: 'wait requires a pid' };
      const result = await controller.execute(`wait(${pid}, DcWaitOutcome)`, 1, context?.signal, context?.onEvent, 'full');
      if (result.error) return { error: result.error };
      const binding = result.bindings.find((entry) => entry.name === 'DcWaitOutcome');
      return { outcome: binding?.value ?? 'failed(unknown)' };
    },
  });

  return tools;
}
