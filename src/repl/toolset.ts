/**
 * REPL capability host tools.
 *
 * The only capability tool is **`bash`**, registered through the SDK's shell
 * manager and sandbox config. Everything else the model needs (file I/O,
 * search, fetching) is DML language, a linked script run through `bash`, or the
 * human-in-the-loop channel (`ask_user` / `input/2`). `--tools`/`--no-tools`
 * control whether `bash` is registered.
 */

import {
  createShellManager,
  describeShellExecutionBackend,
  type ExecutionEnvironment,
  type HostShellConfig,
} from '../system/runtime/shell-manager.js';
import { createShellToolEventBridge } from '../system/runtime/shell-tool-events.js';
import type { DMLEvent, ToolDefinition } from '../types.js';

export const DEFAULT_REPL_TOOLS = ['bash'] as const;

export interface ReplToolsetOptions {
  workspacePath: string;
  sandbox?: boolean;
  network?: boolean;
  hostConfig?: HostShellConfig;
  /** Capability tool names to register. `null`/empty means no capability tools. */
  toolNames?: string[] | null;
  signal?: AbortSignal;
  onEvent?: (event: DMLEvent) => void;
}

export interface ReplToolset {
  tools: Map<string, ToolDefinition>;
  /** Catalog text injected into the meta prompt. */
  catalog: string;
  /**
   * How shell commands will actually run here. The caller passes this to the
   * session so the harness `require_sandbox` check, the model's environment
   * block and this toolset all agree on one answer.
   */
  environment: ExecutionEnvironment;
  dispose(): Promise<void>;
}

/**
 * Describe the `bash` tool, including where it runs.
 *
 * The agent has to reconcile two surfaces — its own tools and DML's `exec` — and
 * they must not disagree about what a path means. So the description states the
 * one name, and states whether anything is actually isolated, rather than
 * letting the agent assume a sandbox is there.
 */
export function describeBashTool(environment: ExecutionEnvironment): string {
  const isolation = environment.kind === 'clean-room'
    ? 'There is NO sandbox: commands run as your user account, and anything you '
      + 'write outside the workspace is real and persists.'
    : `Isolated by ${environment.description}.`;
  return [
    'Run a shell command in the session workspace.',
    'Your working directory is the workspace, and /workspace names that same '
      + 'directory (DML uses the same name), so out/report.txt and '
      + '/workspace/out/report.txt are one file.',
    'Only files under the workspace are shared with the host. For scratch use '
      + '"$TMPDIR" (a per-session directory), not a fixed path like /tmp.',
    isolation,
  ].join(' ');
}

/** Human-readable catalog of the registered capability tools. */
export function buildToolCatalog(tools: Map<string, ToolDefinition>): string {
  if (tools.size === 0) {
    return 'No capability host tools are registered. Use DML/Prolog, or write a script file and run it with exec(bash(...)).';
  }
  const lines: string[] = [];
  for (const [name, tool] of tools) {
    const properties = tool.parameters?.properties ?? {};
    const params = Object.keys(properties).join(', ') || 'none';
    lines.push(`- ${name}: ${tool.description} (parameters: ${params})`);
  }
  lines.push('');
  lines.push('DML file I/O always uses the MI predicates (open/3, close/1, read_string/3, write/2, write_file/2, read_file/2, append_file/2); it is not a host tool.');
  return lines.join('\n');
}

/**
 * Join an argv list into a shell command, quoting every element.
 *
 * This is what makes `exec(bash(argv: [...]))` worth having: the author passes
 * values as a list and never writes a quote, so data containing spaces, quotes,
 * `$`, backticks, `;` or newlines cannot escape into the command. It still runs
 * through the same launch plan, so the sandbox and wrapper are unchanged.
 *
 * POSIX single-quote quoting: wrap in `'`, and end/restart the quote around any
 * embedded `'` (`'\''`).
 */
export function quoteArgv(argv: readonly string[]): string {
  return argv.map((item) => `'${item.replace(/'/g, `'\\''`)}'`).join(' ');
}

export async function createReplToolset(options: ReplToolsetOptions): Promise<ReplToolset> {
  const requested = options.toolNames === null
    ? []
    : options.toolNames ?? [...DEFAULT_REPL_TOOLS];
  const environment = describeShellExecutionBackend(options.sandbox, options.hostConfig ?? {});
  const tools = new Map<string, ToolDefinition>();
  let shell = requested.includes('bash')
    ? createShellManager({
      workspacePath: options.workspacePath,
      sandbox: options.sandbox,
      network: options.network,
      hostConfig: options.hostConfig,
    })
    : null;

  if (shell) {
    const shellManager = shell;
    tools.set('bash', {
      description: describeBashTool(environment),
      parameters: {
        type: 'object',
        properties: {
          command: { type: 'string', description: 'Shell command to execute.' },
          argv: {
            type: 'array',
            description: 'Command and arguments as a list, quoted by the runtime. '
              + 'Prefer this over `command` when any argument is data.',
          },
          timeoutMs: {
            type: 'integer',
            description: 'Wall-clock timeout in milliseconds (optional; overrides DC_HOST_SHELL_TIMEOUT_MS).',
          },
        },
      },
      execute: async (args, context) => {
        const argv = Array.isArray(args.argv)
          ? args.argv.map((item) => String(item))
          : undefined;
        const command = argv && argv.length > 0
          ? quoteArgv(argv)
          : String(args.command ?? args.arg1 ?? '');
        const timeoutMs = typeof args.timeoutMs === 'number' && Number.isFinite(args.timeoutMs) && args.timeoutMs > 0
          ? args.timeoutMs
          : undefined;
        // Prefer the per-turn signal the runner/agent loop passes so Ctrl-C
        // terminates the running command (fall back to a startup signal).
        return shellManager.exec(
          command,
          context?.signal ?? options.signal,
          createShellToolEventBridge({
            toolName: 'bash',
            toolArgs: timeoutMs === undefined ? { command } : { command, timeoutMs },
            emit: options.onEvent,
          }),
          { timeoutMs },
        );
      },
    });
  }

  return {
    tools,
    catalog: buildToolCatalog(tools),
    environment,
    dispose: async () => {
      if (shell) {
        await shell.dispose();
        shell = null;
      }
    },
  };
}
