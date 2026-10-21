/**
 * The environment and clock block put in front of the model.
 *
 * Two surfaces can run code — the agent's own tools and DML's `exec(bash(...))` —
 * and they must not disagree about what a path means or whether anything is
 * isolated. So the facts are stated once, from the same resolution the runtime
 * uses, instead of being left for the model to infer. This is also the only place
 * the model learns the date: without it a model reasons from its training cutoff.
 *
 * Both blocks are meant to be appended **last** in a system prompt. The prompt is
 * rebuilt for every model call, so a block that changes per turn invalidates any
 * prefix caching from that point; keeping the stable documentation ahead of it
 * preserves the cacheable head.
 */

import { SANDBOX_WORKSPACE_PATH, type ExecutionEnvironment } from './runtime/shell-manager.js';

export interface EnvironmentBlockOptions {
  workspacePath?: string;
  environment?: ExecutionEnvironment;
  /**
   * Name of the single capability host tool (default `bash`). A browser
   * embedding registers `python` (Pyodide) instead, and the block must say so or
   * the model calls a tool that does not exist.
   */
  capabilityTool?: string;
  /** Injectable clock, so tests do not depend on the wall calendar. */
  now?: Date;
}

/** `2026-02-14 14:31 (Saturday, Europe/Berlin, CET)` in the host's timezone. */
export function describeNow(now: Date = new Date()): string {
  const parts = new Intl.DateTimeFormat('en-GB', {
    year: 'numeric',
    month: '2-digit',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    weekday: 'long',
    timeZoneName: 'short',
  }).formatToParts(now);

  const get = (type: Intl.DateTimeFormatPartTypes): string =>
    parts.find((part) => part.type === type)?.value ?? '';
  const timezone = Intl.DateTimeFormat().resolvedOptions().timeZone;

  const date = `${get('year')}-${get('month')}-${get('day')}`;
  const time = `${get('hour')}:${get('minute')}`;
  const zoneNames = [timezone, get('timeZoneName')].filter(Boolean).join(', ');
  return `${date} ${time} (${get('weekday')}, ${zoneNames})`;
}

/**
 * The `EXECUTION ENVIRONMENT` block.
 *
 * Everything here is conditional on the effective backend, and says so. The one
 * claim that does *not* depend on a sandbox is that only the workspace reaches
 * the host: that is the WASM filesystem's mount, which is always there.
 */
export function describeEnvironment(options: EnvironmentBlockOptions = {}): string {
  const { workspacePath, environment, capabilityTool } = options;
  const sandboxed = environment?.sandboxed ?? false;
  const workspace = workspacePath ?? process.cwd();
  const label = environment?.backendLabel ?? 'unknown';
  const tool = capabilityTool ?? 'bash';
  const lines = [
    'EXECUTION ENVIRONMENT',
    `- Capability backend: ${label}${environment?.description ? ` (${environment.description})` : ''}.`,
  ];

  if (!sandboxed) {
    lines.push(
      `- There is NO sandbox: \`exec(${tool}(...))\` and the capability tool run as your user `
        + 'account. Writes outside the workspace are real files on this machine and '
        + 'persist, and a `rm` is a real `rm`.',
    );
  } else {
    lines.push(
      `- \`exec(${tool}(...))\` is isolated by ${environment?.description}. Writes outside `
        + 'the workspace stay inside that sandbox and are gone when it ends.',
    );
  }

  lines.push(
    `- Workspace, as your own tools see it: ${workspace}`,
    `- Workspace, inside DML and \`exec\`: ${SANDBOX_WORKSPACE_PATH} — and it is the `
      + `working directory, so \`out.txt\` and \`${SANDBOX_WORKSPACE_PATH}/out.txt\` are the `
      + 'same file. Prefer this name; it is the same everywhere.',
    '- Only files under the workspace reach the host. In DML, any other absolute path '
      + '(e.g. `/etc/x`, `/tmp/x`) is the ephemeral per-session VM filesystem, not this '
      + `machine. ${environment?.kind === 'wasm'
        ? 'Anything you want to keep goes under `/workspace`; `/tmp` is scratch and is gone when the worker ends.'
        : 'For scratch use `$TMPDIR` (per-session in every backend), not a fixed path.'}`,
    '- Do not hardcode the workspace\'s host path: it depends on how the session is run '
      + '(AgentVM, bubblewrap, sandbox-exec, a browser, or nothing at all).',
  );

  return lines.join('\n') + '\n';
}

/** `NOW` — always recomputed, so a long session does not read yesterday's date. */
export function describeCurrentTime(now: Date = new Date(), capabilityTool = 'bash'): string {
  const lookup = capabilityTool === 'bash'
    ? 'run `date` (or `exec(bash(argv: ["date", "-Is"]))`)'
    : 'use the host tool, e.g. `exec(python(code: "import datetime; print(datetime.datetime.now().isoformat())"), R)`';
  return [
    'NOW',
    `- ${describeNow(now)}`,
    `- Do not assume today's date from training data. For the exact time ${lookup}.`,
  ].join('\n') + '\n';
}

/** Both blocks, in the order they should be appended. */
export function describeRuntimeContext(options: EnvironmentBlockOptions = {}): string {
  return `\n${describeEnvironment(options)}\n${describeCurrentTime(options.now, options.capabilityTool)}\n`;
}
