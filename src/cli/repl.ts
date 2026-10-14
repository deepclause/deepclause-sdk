/**
 * `deepclause repl` — the DML REPL.
 *
 * M1 implements goal mode: `goal: <dml>.` runs a DML goal, anything else is
 * classified as an English prompt (prompt mode lands in M3). A small set of
 * out-of-band commands (`halt.`, `help.`, `listing.`, `reset.`, …) is matched
 * as whole-line keywords before the classifier.
 */

import type { Command } from 'commander';
import { readFileSync } from 'fs';
import * as path from 'path';
import * as readline from 'readline';
import { fileURLToPath } from 'url';

import { createHarnessRepl, type HarnessRepl } from '../repl/index.js';
import { HistoryStore } from '../repl/history.js';
import { resolveReplJudgeSelection } from '../repl/judge.js';
import { ReplRenderer, type RenderMode, type ReplEventRenderer } from '../repl/render.js';
import { TranscriptDocument } from '../repl/tui/document.js';
import { ChoicePicker } from '../repl/tui/picker.js';
import { TuiRenderer } from '../repl/tui/renderer.js';
import { WorkingLine } from '../repl/tui/working-line.js';
import { createReplAutocomplete } from '../repl/tui/autocomplete.js';
import { initTuiTheme, scrollbarStyles } from '../repl/tui/theme.js';
import { SizedTerminal } from '../repl/tui/terminal.js';
import { ReplStatusTracker, type ReplStatusSnapshot } from '../repl/status.js';
import { StatusBarComponent } from '../repl/tui/status-bar.js';
import {
  Container,
  ScrollView,
  Spacer,
  TuiAltScreen,
  VStack,
  matchesKey,
} from '@earendil-works/pi-tui';
import { editorTheme, formatInputAnswer } from '../repl/tui/theme.js';
import { PromptEditor } from '../repl/tui/editor.js';
import { loadProjectContext } from '../repl/project-context.js';
import { createPiBackend, THINKING_LEVELS } from '../repl/backend.js';
import { createReplToolset, type ReplToolset } from '../repl/toolset.js';
import { takeStartupPrologMessages } from '../prolog/loader.js';
import { createPiImageBackend } from '../repl/imagegen.js';
import { EnvModelResolver, PiModelResolver, type ModelResolver } from '../repl/model-resolver.js';
import type { ImageBackend } from '../types.js';
import { parseModelId, resolveModelSlotConfig, type Provider } from '../system/config/model-slots.js';
import { loadReplConfig, resolveReplCompactionConfig, updateConfig } from './config.js';
import {
  renderDiagramImage,
  resolveMedia,
  type DiagramMode,
  type GraphicsMode,
} from '../visualize/index.js';

export interface ReplCliOptions {
  goal?: string[];
  load?: string[];
  /** Resume a previous session by id (or `last`). */
  session?: string;
  first?: boolean;
  plain?: boolean;
  json?: boolean;
  workspace?: string;
  model?: string;
  provider?: string;
  /** Thinking level for reasoning models (off|minimal|low|medium|high|xhigh|max). */
  thinking?: string;
  temperature?: number;
  gasLimit?: number;
  depth?: number;
  maxModelCalls?: number;
  param?: Record<string, string>;
  verbose?: boolean;
  color?: boolean;
  freeze?: boolean;
  locked?: boolean;
  exportPath?: string;
  backend?: 'env' | 'pi';
  /** Render streamed model text/reasoning (default true). */
  stream?: boolean;
  /** How much model reasoning to show: off | summary | full. */
  showReasoning?: 'off' | 'summary' | 'full';
  /** Harness to edit (a project can hold several). Default `main`. */
  harness?: string;
  /** Render activity events (task activity, tool calls, usage, logs; default true). */
  events?: boolean;
  /** Save the harness to a temp file on exit (default true). */
  saveOnExit?: boolean;
  /** Default judgment backend name (llm|jev). */
  judge?: string;
  tools?: string;
  noTools?: boolean;
  sandbox?: boolean;
  /** Refuse to start unless the effective shell backend is a sandbox. */
  requireSandbox?: boolean;
  noAsk?: boolean;
  answer?: string;
  interactiveInput?: boolean;
  /** Image model for generation (`provider:id`); default: first available. */
  imageModel?: string;
  /** Hand images to the OS viewer when the terminal cannot render them. */
  open?: boolean;
  /** Interactive UI: auto (TUI on a TTY), on, or off. */
  tui?: 'auto' | 'on' | 'off';
}

export type ReplInput =
  | { kind: 'empty' }
  | { kind: 'comment' }
  | { kind: 'halt' }
  | { kind: 'help' }
  | { kind: 'listing'; filter?: string }
  | { kind: 'reset' }
  | { kind: 'rollback' }
  | { kind: 'compact'; instructions?: string }
  | { kind: 'model'; model?: string }
  | { kind: 'reasoning'; level?: string }
  | { kind: 'show-reasoning' }
  | { kind: 'export'; path?: string }
  | { kind: 'freeze' }
  | { kind: 'unfreeze' }
  | { kind: 'trace' }
  | { kind: 'notrace' }
  | { kind: 'params' }
  | { kind: 'viz'; entry?: string; mode: DiagramMode; graphics: GraphicsMode }
  | { kind: 'show'; path: string }
  | { kind: 'gen'; prompt: string }
  | { kind: 'goal'; goal: string }
  | { kind: 'prompt'; prompt: string }
  | { kind: 'unknown-command'; name: string };

const OUT_OF_BAND = new Set(['halt', 'help', 'reset', 'rollback', 'freeze', 'unfreeze', 'trace', 'notrace', 'params']);

const GRAPHICS_MODES: GraphicsMode[] = ['auto', 'kitty', 'iterm2', 'ascii', 'off'];

/** Parse the `/viz` argument list: `[entry] [--spec] [--graphics=<mode>]`. */
export function parseVizArgs(arg: string): { entry?: string; mode: DiagramMode; graphics: GraphicsMode } {
  const tokens = arg.split(/\s+/).filter(Boolean);
  let entry: string | undefined;
  let mode: DiagramMode = 'presentation';
  let graphics: GraphicsMode = 'auto';
  for (let i = 0; i < tokens.length; i += 1) {
    const token = tokens[i];
    if (token === '--spec') {
      mode = 'spec';
    } else if (token === '--presentation') {
      mode = 'presentation';
    } else if (token === '--ascii') {
      graphics = 'ascii';
    } else if (token === '--graphics') {
      const value = tokens[i + 1] as GraphicsMode | undefined;
      if (value && GRAPHICS_MODES.includes(value)) {
        graphics = value;
        i += 1;
      }
    } else if (token.startsWith('--graphics=')) {
      const value = token.slice('--graphics='.length) as GraphicsMode;
      if (GRAPHICS_MODES.includes(value)) {
        graphics = value;
      }
    } else if (!token.startsWith('-') && !entry) {
      entry = token;
    }
  }
  return { entry, mode, graphics };
}

/** Render a recorded turn as the line the user would retype (goals get `/goal`). */
export function formatTurnInput(turn: { mode: 'goal' | 'prompt'; request: string }): string {
  return turn.mode === 'goal' ? `/goal ${turn.request}` : turn.request;
}

/**
 * Classify one REPL input line.
 *
 * Rules (proposal §9.1): out-of-band commands are matched first as whole-line
 * keywords; `goal:` selects goal mode; everything else is an English prompt.
 * A `goal:` payload is never reinterpreted as a prompt.
 */
export function classifyReplInput(line: string): ReplInput {
  const trimmed = line.trim();
  if (!trimmed) {
    return { kind: 'empty' };
  }
  if (trimmed.startsWith('%')) {
    return { kind: 'comment' };
  }

  // Slash commands (the TUI-first surface): `/goal <dml>`, `/listing`, `/halt`, …
  if (trimmed.startsWith('/')) {
    const withoutSlash = trimmed.slice(1);
    const spaceIndex = withoutSlash.search(/\s/);
    const name = (spaceIndex === -1 ? withoutSlash : withoutSlash.slice(0, spaceIndex)).toLowerCase();
    const arg = (spaceIndex === -1 ? '' : withoutSlash.slice(spaceIndex)).trim();
    switch (name) {
      case 'goal':
        return arg ? { kind: 'goal', goal: arg } : { kind: 'unknown-command', name: 'goal (missing goal)' };
      case 'listing':
        return { kind: 'listing', filter: arg || undefined };
      case 'halt':
      case 'exit':
      case 'quit':
        return { kind: 'halt' };
      case 'help':
        return { kind: 'help' };
      case 'reset':
        return { kind: 'reset' };
      case 'rollback':
        return { kind: 'rollback' };
      case 'compact':
        return { kind: 'compact', instructions: arg || undefined };
      case 'export':
        return { kind: 'export', path: arg || undefined };
      case 'model':
        return { kind: 'model', model: arg || undefined };
      case 'reasoning':
      case 'reasoning-level':
        return { kind: 'reasoning', level: arg || undefined };
      case 'show-reasoning':
      case 'reasoning-summary':
        return { kind: 'show-reasoning' };
      case 'freeze':
        return { kind: 'freeze' };
      case 'unfreeze':
        return { kind: 'unfreeze' };
      case 'params':
        return { kind: 'params' };
      case 'trace':
        return { kind: 'trace' };
      case 'notrace':
        return { kind: 'notrace' };
      case 'viz':
      case 'diagram':
        return { kind: 'viz', ...parseVizArgs(arg) };
      case 'show':
      case 'image':
        return arg ? { kind: 'show', path: arg } : { kind: 'unknown-command', name: 'show (missing path)' };
      case 'gen':
      case 'imagine':
        return arg ? { kind: 'gen', prompt: arg } : { kind: 'unknown-command', name: 'gen (missing prompt)' };
      default:
        return { kind: 'unknown-command', name };
    }
  }

  // Out-of-band commands: whole line, optional trailing full stop.
  const commandLine = trimmed.replace(/\.\s*$/, '').trim();
  if (OUT_OF_BAND.has(commandLine)) {
    return { kind: commandLine } as ReplInput;
  }
  if (commandLine === 'listing') {
    return { kind: 'listing' };
  }
  if (commandLine.startsWith('listing(') && commandLine.endsWith(')')) {
    const filter = commandLine.slice('listing('.length, -1).trim();
    return { kind: 'listing', filter: filter || undefined };
  }
  if (commandLine === 'model') {
    return { kind: 'model' };
  }
  if (commandLine.startsWith('model ')) {
    return { kind: 'model', model: commandLine.slice('model '.length).trim() || undefined };
  }
  if (commandLine === 'compact') {
    return { kind: 'compact' };
  }
  if (commandLine.startsWith('compact ')) {
    return { kind: 'compact', instructions: commandLine.slice('compact '.length).trim() || undefined };
  }
  if (commandLine === 'export') {
    return { kind: 'export' };
  }
  if (commandLine.startsWith('export ')) {
    return { kind: 'export', path: commandLine.slice('export '.length).trim() || undefined };
  }

  if (trimmed.startsWith('goal:')) {
    return { kind: 'goal', goal: trimmed.slice('goal:'.length).trim() };
  }

  return { kind: 'prompt', prompt: trimmed };
}

/** Run one goal and render its solutions. Returns a process exit code. */
export async function runGoalWithRenderer(
  repl: HarnessRepl,
  goal: string,
  options: { first?: boolean; renderer: ReplEventRenderer; signal?: AbortSignal },
): Promise<number> {
  let hadError = false;
  let printedAny = false;
  for await (const event of repl.query(goal, { signal: options.signal })) {
    if (event.type === 'solution') {
      options.renderer.solution(event.solution?.bindings ?? []);
      printedAny = true;
      if (options.first) break;
    } else if (event.type === 'error') {
      hadError = true;
      options.renderer.render(event);
    } else if (event.type === 'finished') {
      // Nothing to print for finished.
    } else {
      options.renderer.render(event);
    }
  }
  if (!printedAny && !hadError) {
    options.renderer.writeLine('false.');
  }
  return hadError ? 1 : 0;
}

function selectRenderMode(options: ReplCliOptions): RenderMode {
  if (options.json) return 'json';
  if (options.plain) return 'plain';
  return process.stdout.isTTY ? 'rich' : 'plain';
}

/**
 * Build the unified input channel from CLI flags (proposal §13.6):
 *  - `--answer` supplies a canned reply; `--no-ask` fails immediately;
 *  - a TTY (or `--interactive-input`) prompts;
 *  - everything else returns undefined so autonomous runs never block.
 */
export function buildCliOnInput(
  options: ReplCliOptions,
): ((prompt: string, choices?: string[]) => Promise<string | undefined>) | undefined {
  if (options.answer !== undefined) {
    return async () => options.answer;
  }
  if (options.noAsk) {
    return async () => undefined;
  }
  const interactive = Boolean(process.stdin.isTTY) || Boolean(options.interactiveInput);
  if (!interactive) {
    return async () => undefined;
  }
  return async (prompt, choices) => {
    const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
    try {
      const suffix = choices && choices.length > 0 ? ` (${choices.join('/')})` : '';
      return await new Promise<string>((resolve) => rl.question(`${prompt}${suffix}\n> `, resolve));
    } finally {
      rl.close();
    }
  };
}

function resolveModel(modelId: string): { provider: Provider; model: string } {
  try {
    const parsed = parseModelId(modelId);
    return { provider: parsed.provider, model: parsed.model };
  } catch {
    return { provider: 'openai', model: modelId };
  }
}

export interface ReplRuntime {
  repl: HarnessRepl;
  /** String renderer for plain/json output, or the TUI's component renderer. */
  renderer: ReplEventRenderer;
  workspace: string;
  promptPrefix: string;
  toolset: ReplToolset;
  /** Package version, shown in the startup banner. */
  version: string;
  /** Resolved model and backend, shown in the startup banner. */
  model: string;
  backend: 'env' | 'pi';
  /** Active model capabilities (pi backend), used by the status bar. */
  contextWindow?: number;
  reasoning: boolean;
  /** How much model reasoning the renderer shows (the picker changes this live). */
  reasoningMode: 'off' | 'summary' | 'full';
  /** Models this session can switch to, for the model picker. */
  modelResolver?: ModelResolver;
  /** The backend, so a picker can change the reasoning level live. */
  llmBackend?: import('../types.js').LLMBackend;
  /** Set the reasoning level shown in the status bar. */
  setThinkingLevel?: (level: string | undefined) => void;
  thinkingLevel?: string;
  /** Session id of this run. */
  sessionId: string;
  /** Harness this session edits (a project can hold several). */
  harnessName: string;
  /** Session id when the run resumed an earlier session. */
  resumedSession?: string;
  /** True when persistent history is enabled (sessions can be resumed). */
  historyEnabled: boolean;
  /** Previous session inputs, oldest first (for the editor's up/down history). */
  inputHistory: string[];
  /** Current usage/context/model status for the status bar. */
  statusSnapshot(): ReplStatusSnapshot;
  /** Resolved interactive UI mode (`auto` unless overridden by config/flags). */
  tuiMode: 'auto' | 'on' | 'off';
  /** Replace the input channel (the interactive loop uses its own readline). */
  setOnInput(handler: (prompt: string, choices?: string[]) => Promise<string | undefined>): void;
  /** Set when the env backend has no credentials for the selected provider. */
  missingCredential?: string;
  /** Image generator for `task(…, media(X))` and `/gen`. */
  imageBackend?: ImageBackend;
}

const PROVIDER_ENV_KEYS: Record<string, string> = {
  openai: 'OPENAI_API_KEY',
  anthropic: 'ANTHROPIC_API_KEY',
  google: 'GOOGLE_GENERATIVE_AI_API_KEY',
  openrouter: 'OPENROUTER_API_KEY',
};

/** Build the harness + renderer from CLI options. */
export async function createReplRuntime(options: ReplCliOptions): Promise<ReplRuntime> {
  const workspace = path.resolve(options.workspace ?? './');
  const config = await loadReplConfig(workspace);
  const replCompaction = resolveReplCompactionConfig(config);
  // One harness per name within a project: `--harness`, else the `-l` file's
  // name, else the configured default (`main`).
  const harnessName = normalizeHarnessName(options.harness)
    ?? (options.load?.length ? harnessNameFromFile(options.load[0]) : undefined)
    ?? normalizeHarnessName(config.repl.harness)
    ?? 'main';
  const modelId = options.model ?? config.models.run;
  const resolved = resolveModel(modelId);
  const provider = (options.provider as Provider | undefined) ?? resolved.provider;

  const renderer = new ReplRenderer({
    mode: selectRenderMode(options),
    color: options.color !== false && config.repl.color !== false,
    verbose: options.verbose ?? false,
    stream: options.stream !== false,
    events: options.events !== false,
    reasoning: options.showReasoning ?? config.repl.showReasoning,
    onEvent: (event) => statusHolder.tracker?.observe(event),
    workspace,
    openImages: options.open !== false && config.repl.media.open !== false,
  });
  const statusHolder: { tracker?: ReplStatusTracker } = {};

  // `null` means no capability tools; `undefined` means use the default
  // (bash). An explicit `tools: []` in config also disables capability tools.
  const toolNames = options.noTools
    ? null
    : options.tools
      ? options.tools.split(',').map((name) => name.trim()).filter(Boolean)
      : (config.repl.tools ?? undefined);
  const toolset = await createReplToolset({
    workspacePath: workspace,
    sandbox: options.sandbox,
    network: config.agentvm?.network ?? false,
    hostConfig: config.shell,
    toolNames,
    // The runner already emits the tool_call start/completion events, so the
    // shell bridge here only forwards stream/log lines to avoid duplicates.
    onEvent: (event) => {
      if (event.type !== 'tool_call') {
        renderer.render(event);
      }
    },
  });

  // The CLI counterpart of `:- require_sandbox.`: enforce it without editing the
  // harness, for CI and for a run you do not want to be quietly unsandboxed.
  if (options.requireSandbox && !toolset.environment.sandboxed) {
    throw new Error(
      `--require-sandbox: the effective shell backend is ${toolset.environment.backendLabel}, `
      + 'which is not sandboxed. Install bubblewrap (Linux), grant sandbox-exec (macOS), '
      + 'or run with --sandbox (AgentVM).',
    );
  }

  const backendKind = options.backend ?? config.backend?.kind ?? 'pi';
  let llmBackend;
  let harnessModel = resolved.model;
  let harnessProvider: Provider = provider;
  let contextWindow: number | undefined;
  let reasoning = false;
  let thinkingLevel: string | undefined;
  let modelResolver: ModelResolver | undefined;
  if (backendKind === 'pi') {
    try {
      const pi = await createPiBackend({
        agentDir: config.backend?.agentDir,
        modelsPath: config.backend?.modelsPath ?? undefined,
        authPath: config.backend?.authPath ?? undefined,
        model: options.model ?? config.backend?.model,
        thinkingLevel: options.thinking ?? config.backend?.thinkingLevel,
      });
      llmBackend = pi.backend;
      harnessModel = pi.modelId.split('/').slice(1).join('/') || pi.modelId;
      harnessProvider = (pi.provider as Provider) ?? harnessProvider;
      contextWindow = pi.contextWindow;
      reasoning = pi.reasoning;
      thinkingLevel = pi.thinkingLevel;
      modelResolver = new PiModelResolver(pi.registry);
      if (pi.warning) process.stderr.write(`[deepclause] ${pi.warning}\n`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      throw new Error(
        `pi backend unavailable: ${message}\n`
        + "Run 'deepclause login' to sign in, or 'deepclause set-backend env' to use provider API keys.",
      );
    }
  } else {
    // env backend: take the context window from the bundled model database so
    // compaction thresholds still work without pi.
    contextWindow = resolveModelSlotConfig(config, 'run', {
      modelId: options.model ?? config.models.run,
      temperature: options.temperature ?? config.temperatures.run,
    }).contextWindow;
    modelResolver = new EnvModelResolver(String(harnessProvider));
  }

  // Judgment backends. `llm` reuses the active model/backend; `jev` is opt-in
  // and only registered when enabled and its API key is present. Building the
  // registry here (rather than in createHarnessRepl) lets the CLI report a
  // skipped `jev` backend and honor --judge.
  const { selection: judgeSelection, warnings: judgeWarnings } = resolveReplJudgeSelection({
    llmBackend,
    model: harnessModel,
    provider: harnessProvider,
    temperature: options.temperature ?? config.temperatures.run,
    defaultJudge: options.judge ?? config.judgment.default,
    jev: {
      enabled: config.judgment.jev.enabled,
      model: config.judgment.jev.model,
      apiKeyEnv: config.judgment.jev.apiKeyEnv,
    },
  });
  for (const warning of judgeWarnings) {
    process.stderr.write(`[deepclause] ${warning}\n`);
  }

  // History is needed before the session opens: `--session` resolves the
  // target id here and the resumed session reads its harness back on open.
  const historyOptions = config.repl.history.enabled
    ? {
        dir: config.repl.history.dir,
        trace: config.repl.history.trace,
        maxEventBytes: config.repl.history.maxEventBytes,
        maxSessions: config.repl.history.maxSessions,
        maxVersions: config.repl.history.maxVersions,
        includeInExport: config.repl.history.includeInExport,
        redact: config.repl.history.redact,
      }
    : undefined;

  let sessionId = options.session;
  let resume = false;
  if (sessionId) {
    if (!historyOptions) {
      throw new Error('--session requires repl.history to be enabled');
    }
    const store = new HistoryStore(workspace, '__session_lookup__', historyOptions);
    if (sessionId === 'last') {
      const latest = await store.latestSessionId();
      if (!latest) {
        throw new Error('no previous sessions found to resume');
      }
      sessionId = latest;
    } else if (!(await store.sessionExists(sessionId))) {
      throw new Error(`unknown session: ${sessionId}`);
    }
    resume = true;
  }

  // Seed the editor's up/down history from the target session's recorded turns.
  let inputHistory: string[] = [];
  if (historyOptions) {
    const store = new HistoryStore(workspace, '__session_lookup__', historyOptions);
    const target = sessionId ?? await store.latestSessionId();
    if (target) {
      inputHistory = (await store.readTurns(target))
        .map((turn) => formatTurnInput(turn))
        .filter((request) => request.length > 0);
    }
  }

  // Image generation for `task(…, media(X))`; best effort, pi backend only.
  let imageBackend: ImageBackend | undefined;
  if (backendKind === 'pi') {
    try {
      imageBackend = await createPiImageBackend({
        workspacePath: workspace,
        agentDir: config.backend?.agentDir ?? undefined,
        modelsPath: config.backend?.modelsPath ?? undefined,
        authPath: config.backend?.authPath ?? undefined,
        model: options.imageModel ?? config.repl.media.imageModel,
        dir: config.repl.media.dir,
        size: config.repl.media.size,
      });
    } catch {
      imageBackend = undefined;
    }
  }

  const repl = await createHarnessRepl({
    sessionId,
    resume,
    workspacePath: workspace,
    model: harnessModel,
    provider: harnessProvider,
    temperature: options.temperature ?? config.temperatures.run,
    params: options.param ?? {},
    gasLimit: options.gasLimit,
    maxDepth: options.depth ?? config.repl.maxDepth,
    maxModelCalls: options.maxModelCalls ?? config.repl.maxModelCalls,
    tools: toolset.tools,
    hostToolsCatalog: toolset.catalog,
    executionBackend: toolset.environment.kind,
    llmBackend,
    judgeSelection,
    imageBackend,
    modelResolver,
    history: historyOptions,
    harness: harnessName,
    autoLoadHarness: !(options.load?.length),
    compaction: { settings: replCompaction, auto: replCompaction.auto },
    contextWindow,
    onInput: buildCliOnInput(options),
  });

  // Terminal images (agent `visualize` and `/viz`) are painted through the
  // renderer; the TUI installs an image sink later, which this follows.
  repl.session.setImageHandler((lines) => renderer.image(lines));

  let missingCredential: string | undefined;

  const statusTracker = new ReplStatusTracker({
    model: {
      id: harnessModel,
      provider: String(harnessProvider),
      reasoning,
      thinkingLevel,
    },
    contextWindow,
  });
  statusHolder.tracker = statusTracker;
  if (backendKind === 'env') {
    const envKey = PROVIDER_ENV_KEYS[harnessProvider];
    const configured = config.providers?.[harnessProvider]?.apiKey;
    if (envKey && !configured && !process.env[envKey]) {
      missingCredential = `no credentials for provider '${harnessProvider}'. `
        + `Set ${envKey}, configure providers.${harnessProvider}.apiKey, `
        + "or run 'deepclause set-backend pi'.";
    }
  }

  return {
    repl,
    renderer,
    workspace,
    promptPrefix: config.repl.promptPrefix ?? 'dml> ',
    toolset,
    version: PACKAGE_VERSION,
    imageBackend,
    model: backendKind === 'pi' ? harnessModel : `${harnessProvider}:${harnessModel}`,
    backend: backendKind,
    contextWindow,
    reasoning,
    reasoningMode: options.showReasoning ?? config.repl.showReasoning,
    modelResolver,
    llmBackend,
    setThinkingLevel: (level) => statusTracker.setThinkingLevel(level),
    thinkingLevel,
    sessionId: repl.session.getSessionId(),
    harnessName,
    resumedSession: resume ? sessionId : undefined,
    historyEnabled: Boolean(historyOptions),
    inputHistory,
    statusSnapshot: () => statusTracker.snapshot(),
    tuiMode: options.tui ?? config.repl.tui ?? 'auto',
    setOnInput: (handler) => repl.setOnInput(handler),
    missingCredential,
  };
}

/** Package version, read once from package.json for the banner/`--version`. */
export const PACKAGE_VERSION: string = (() => {
  try {
    const here = path.dirname(fileURLToPath(import.meta.url));
    const pkg = JSON.parse(readFileSync(path.join(here, '..', '..', 'package.json'), 'utf8')) as { version?: string };
    return pkg.version ?? 'unknown';
  } catch {
    return 'unknown';
  }
})();

/** Startup banner for the interactive REPL (proposal §2.1). */
async function printBanner(runtime: ReplRuntime, loaded: string[]): Promise<void> {
  const rich = runtime.renderer.mode === 'rich';
  const paint = (text: string, code: string): string => (rich ? `\u001b[${code}m${text}\u001b[0m` : text);
  const dim = (text: string): string => paint(text, '2');
  const accent = (text: string): string => paint(text, '36');
  const bold = (text: string): string => paint(text, '1');

  const harnessFresh = runtime.repl.session.isEmpty();
  const predicateCount = runtime.repl.session.predicateCount();
  const harnessInfo = harnessFresh
    ? 'new — no predicates yet'
    : `${predicateCount} predicate${predicateCount === 1 ? '' : 's'}`;
  const loadedState = loaded.length > 0
    ? `${loaded.length} file${loaded.length === 1 ? '' : 's'} loaded`
    : undefined;
  const frozen = runtime.repl.isFrozen() ? `  ${paint(' FROZEN ', '41;97')}` : '';

  const summary = [
    loadedState ? dim(loadedState) : undefined,
    paint(runtime.model, '35'),
    dim(`backend ${runtime.backend}`),
    `${dim('harness')} ${paint(runtime.harnessName, '35')} ${dim(`(${harnessInfo})`)}`,
  ].filter((part): part is string => Boolean(part)).join(`  ${dim('·')}  `);

  const lines: string[] = [
    '',
    `  ${paint('DeepClause', '1;36')} ${paint('DML REPL', '1')}  ${dim(`v${runtime.version}`)}${frozen}`,
    `  ${summary}`,
    `  ${dim(runtime.workspace)}`,
  ];

  lines.push(
    runtime.resumedSession
      ? `  ${dim('session')} ${runtime.sessionId} ${accent('(resumed)')}`
      : `  ${dim('session')} ${runtime.sessionId}`,
  );

  const project = await loadProjectContext(runtime.workspace);
  if (project) {
    lines.push(`  ${dim('project context')} AGENTS.md ${dim(`(${project.bytes} bytes)`)}`);
  }

  // A short getting-started guide, shown on a fresh (blank) harness only.
  if (harnessFresh) {
    lines.push(
      '',
      `  ${bold('Two modes.')} Plain English authors the harness; ${accent('/goal')} runs DML directly.`,
      '',
      `    ${accent('<plain English>')}   default. Ask for anything: add a new skill, add a tool,`,
      `                      change existing logic, run or test code in the living`,
      `                      module, or just ask a question about the harness. An authoring`,
      `                      agent writes the DML, tests it with ${dim('execute')}, and commits it.`,
      `    ${accent('/goal <dml>')}        run DML/Prolog directly against the current harness.`,
      '',
      `  ${bold('Example')}`,
      `    add a research(Question, Answer) skill that answers with task/2`,
      `    run research on "why is the sky blue?"`,
      `    A = "…"`,
      '',
      `    ...or run DML directly (${accent('/goal')}):`,
      `    ${accent('/goal')} research("why is the sky blue?", A).`,
      `    A = "…"`,
      '',
      `  When you're ready, export the harness and re-use it whenever you need:`,
      `    /export research.dml              save a portable harness`,
      `    ${dim('$')} deepclause repl -l research.dml    load it back any time`,
      `    ${dim('$')} deepclause repl --session last   resume the last session`,
    );
  }

  lines.push(
    '',
    `  ${accent('/goal')} ${dim('<dml>')}            run a DML goal, e.g. ${dim('/goal member(X, [1,2,3])')}`,
    `  ${accent('<plain English>')}        author the harness`,
    '',
    `  ${accent('/help')}  ${accent('/listing')}  ${accent('/viz')}  ${accent('/export')}  ${accent('/freeze')}  ${accent('/model')}  ${accent('/halt')}`,
  );
  if (runtime.historyEnabled) {
    lines.push(`  ${dim('resume later:')} deepclause repl --session ${runtime.sessionId}`);
  }
  lines.push('');

  runtime.renderer.writeLine(lines.join('\n'));
}

/** Dispatch one classified input. Returns false when the REPL should exit. */
export async function dispatchReplInput(
  runtime: ReplRuntime,
  input: ReplInput,
  options: { first?: boolean; print?: (text: string) => void; exportPath?: string; signal?: AbortSignal } = {},
): Promise<boolean> {
  const { repl, renderer } = runtime;
  const print = options.print ?? ((text: string) => renderer.writeLine(text));
  switch (input.kind) {
    case 'empty':
    case 'comment':
      return true;
    case 'unknown-command':
      print(`unknown command: /${input.name} — try /help.`);
      return true;
    case 'halt':
      return false;
    case 'help':
      print(helpText());
      return true;
    case 'listing': {
      const text = await repl.list(input.filter);
      print(runtime.renderer.listing(text.trimEnd() || '% (empty harness)'));
      return true;
    }
    case 'reset':
      if (repl.isFrozen()) {
        print('ERROR: harness is frozen; reset is disabled. Use \'unfreeze.\' first.');
        return true;
      }
      await repl.reset();
      print('harness reset');
      return true;
    case 'rollback': {
      const result = await repl.rollback();
      if (result.ok) {
        print(`rolled back harness: version ${result.from} → ${result.to}`);
      } else {
        print(`ERROR: ${result.error ?? 'rollback failed'}`);
      }
      return true;
    }
    case 'compact': {
      const result = await repl.compact(input.instructions);
      if (result.applied) {
        print(`compacted meta memory: ${result.beforeTokens} → ${result.afterTokens} tokens`);
      } else if (result.error) {
        print(`compact skipped: ${result.error}`);
      } else {
        print(`nothing to compact (${result.beforeTokens} tokens)`);
      }
      return true;
    }
    case 'model': {
      if (input.model) {
        await updateConfig(runtime.workspace, { backend: { kind: 'pi', model: input.model } });
        print(`default model set to ${input.model} (takes effect on the next start)`);
      } else {
        print(`model: ${runtime.model}`);
        print(`backend: ${runtime.backend}`);
        print("set with 'model <provider/id>.' or 'deepclause login'");
      }
      return true;
    }
    case 'export': {
      const target = input.path ?? options.exportPath ?? 'harness.dml';
      try {
        const result = await repl.export(target);
        print(`exported ${result.clauses} clauses, ${result.tools} tools, ${result.scripts} scripts → ${result.path}`);
      } catch (error) {
        print(`ERROR: ${(error as Error).message}`);
      }
      return true;
    }
    case 'freeze':
      await repl.freeze();
      print(`harness frozen: assert/retract/load disabled. Use 'unfreeze.' to re-enable.`);
      return true;
    case 'unfreeze':
      try {
        await repl.unfreeze();
        print('harness unfrozen');
      } catch (error) {
        print(`ERROR: ${(error as Error).message}`);
      }
      return true;
    case 'trace':
      print('trace enabled');
      return true;
    case 'notrace':
      print('trace disabled');
      return true;
    case 'params':
      print('params: (see session configuration)');
      return true;
    case 'viz': {      if (runtime.missingCredential) {
        print(`ERROR: ${runtime.missingCredential}`);
        return true;
      }
      let result;
      try {
        result = await repl.diagram(input.entry, input.mode, { signal: options.signal });
      } catch (error) {
        print(`ERROR: ${(error as Error).message}`);
        return true;
      }
      print(`📊 ${input.mode} diagram of ${result.entry}`);
      for (const warning of result.warnings) {
        print(`  ⚠ ${warning}`);
      }
      const lines = await renderDiagramImage(result, {
        graphics: input.graphics,
        maxWidthCells: Math.max(20, renderer.getWidth() - 2),
        maxHeightCells: Math.max(8, (process.stdout.rows ?? 40) - 8),
      });
      if (lines) {
        renderer.image(lines);
        return true;
      }
      print(result.ascii);
      if (input.graphics !== 'ascii' && input.graphics !== 'off') {
        print('(no terminal image support here — showing text; /viz --ascii to force text)');
      }
      return true;
    }
    case 'show': {
      const resolved = await resolveMedia(runtime.workspace, { path: input.path });
      if (!resolved) {
        print(`ERROR: cannot read image ${input.path}`);
        return true;
      }
      renderer.render({
        type: 'media',
        media: { path: input.path, mimeType: resolved.mimeType, data: resolved.pngBase64 },
      });
      return true;
    }
    case 'gen': {
      if (!runtime.imageBackend) {
        print('ERROR: image generation is unavailable (no image backend; sign in to a provider with image models, or pass --image-model)');
        return true;
      }
      try {
        const generated = await runtime.imageBackend.generate({ prompt: input.prompt, signal: options.signal });
        const resolved = await resolveMedia(runtime.workspace, { path: generated.path, mimeType: generated.mimeType });
        renderer.render({
          type: 'media',
          media: { path: generated.path, mimeType: generated.mimeType, data: resolved?.pngBase64 },
        });
      } catch (error) {
        print(`ERROR: ${(error as Error).message}`);
      }
      return true;
    }
    case 'goal':
      await runGoalWithRenderer(repl, input.goal, { first: options.first, renderer, signal: options.signal });
      return true;
    case 'prompt': {
      if (runtime.missingCredential) {
        print(`ERROR: ${runtime.missingCredential}`);
        return true;
      }
      for await (const event of repl.prompt(input.prompt, { signal: options.signal })) {
        renderer.render(event);
      }
      return true;
    }
    default:
      return true;
  }
}

function helpText(): string {
  return [
    'DeepClause REPL — a living DML harness you author by talking to it.',
    '',
    '  Modes',
    '    /goal <dml>          run DML/Prolog directly against the harness.',
    '    <plain English>      author: ask for a skill, change existing logic, or ask a',
    '                         question. An agent writes, tests, and commits the DML.',
    '',
    '  Commands',
    '    /listing [filter]    inspect the harness (all | Name/Arity | tools | scripts)',
    '    /viz [entry] [--spec] diagram of a skill, drawn in the terminal',
    '                         (--graphics=auto|kitty|iterm2|ascii|off)',
    '    /show <path>         display a workspace image inline',
    '    /gen <prompt>        generate an image and display it',
    '    /help                this help',
    '    /reset               empty the harness',
    '    /rollback            undo the last change',
    '    /compact [notes]     compact the authoring memory',
    '    /export [file]       save the harness (use .zip to bundle linked files)',
    '    /model [provider/id] show / set the model',
    '    /freeze  /unfreeze   read-only lock',
    '    /params              show session parameters',
    '    /trace   /notrace    toggle verbose logging',
    '    /halt                exit',
    '',
    '  Examples',
    '    /goal member(X, [a,b,c]).',
    '    /listing all',
    '    add a tally(List, Counts) skill, plus a demo',
    '    what skills does this harness have?',
    '',
    '  Skills compose: task/N (LLM calls), exec/2 (host tools), judgments, CLP(FD)/',
    '  CLP(Q), and script_file/2 (bundled on export).',
  ].join('\n');
}

async function executeLoad(runtime: ReplRuntime, files: string[]): Promise<number> {
  for (const file of files) {
    const target = path.isAbsolute(file) ? file : path.resolve(runtime.workspace, file);
    const result = await runtime.repl.load(target);
    if (result.error) {
      runtime.renderer.writeErrorLine(`ERROR: ${result.error}`);
      return 1;
    }
    runtime.renderer.writeLine(`loaded ${file}`);
    for (const warning of result.warnings ?? []) {
      runtime.renderer.writeErrorLine(`warning: ${warning}`);
    }
  }
  return 0;
}

/** Non-interactive mode: read classified inputs line by line until EOF. */
/**
 * Ctrl-C exits, in every mode: this is the plain/readline path, where there is no
 * raw key to distinguish Escape from an editing sequence, so the TUI's
 * "Esc interrupts, Ctrl-C exits" cannot be mirrored exactly. A running turn is
 * aborted first, so nothing keeps writing after the UI is gone.
 */
function installSigint(
  holder: { turn: AbortController | null },
  rl?: readline.Interface,
  onExit?: () => Promise<void>,
): () => void {
  const handler = () => {
    holder.turn?.abort(new Error('cancelled by user'));
    if (onExit) {
      void onExit().finally(() => process.exit(130));
    } else {
      process.exit(130);
    }
  };
  process.on('SIGINT', handler);
  rl?.on('SIGINT', handler);
  return () => {
    process.off('SIGINT', handler);
    rl?.off('SIGINT', handler);
  };
}

async function runNonInteractive(
  runtime: ReplRuntime,
  options: ReplCliOptions,
  onExit?: () => Promise<void>,
): Promise<number> {
  let exitCode = 0;
  const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
  const holder: { turn: AbortController | null } = { turn: null };
  const removeSigint = installSigint(holder, rl, onExit);
  try {
    for await (const line of rl) {
      const input = classifyReplInput(line);
      if (input.kind === 'halt') break;
      const controller = new AbortController();
      holder.turn = controller;
      try {
        const keepGoing = await dispatchReplInput(runtime, input, {
          first: options.first,
          exportPath: options.exportPath,
          signal: controller.signal,
        });
        if (!keepGoing) break;
      } finally {
        holder.turn = null;
      }
    }
  } finally {
    removeSigint();
    rl.close();
  }
  return exitCode;
}

/** Interactive mode with the `dml>` prompt and a `;`/`.` pager for goals. */
async function runInteractive(
  runtime: ReplRuntime,
  options: ReplCliOptions,
  onExit?: () => Promise<void>,
): Promise<number> {
  const { promptPrefix } = runtime;
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: true,
  });
  // A single readline interface serves the REPL prompt, the input handler, and
  // the pager. `rl.question` can be called whenever no other question is
  // pending (the loop is blocked inside a turn), so ask_user / input/2 do not
  // need a second interface (which double-echoed and closed stdin).
  let rlClosed = false;
  rl.on('close', () => {
    rlClosed = true;
  });
  const ask = (query: string): Promise<string | null> => new Promise((resolve) => {
    // Calling question() on a closed interface throws; treat it as EOF.
    if (rlClosed) {
      resolve(null);
      return;
    }
    let settled = false;
    const finish = (value: string | null) => {
      if (settled) return;
      settled = true;
      rl.off('close', onClose);
      resolve(value);
    };
    const onClose = () => finish(null);
    rl.once('close', onClose);
    rl.question(query, (answer) => finish(answer));
  });

  runtime.setOnInput(async (prompt, choices) => {
    const options = choices && choices.length > 0 ? ` (${choices.join('/')})` : '';
    runtime.renderer.writeLine('');
    runtime.renderer.writeLine(`? ${prompt}${options}`);
    const answer = await ask('> ');
    return answer === null ? undefined : answer;
  });

  const holder: { turn: AbortController | null } = { turn: null };
  const removeSigint = installSigint(holder, rl, onExit);

  try {
    while (true) {
      const line = await ask(promptPrefix);
      if (line === null) break;
      const input = classifyReplInput(line);
      if (input.kind === 'halt') break;

      const controller = new AbortController();
      holder.turn = controller;
      try {
        if (input.kind === 'goal') {
          await runInteractiveGoal(runtime.repl, runtime.renderer, input.goal, options, ask, controller.signal);
        } else {
          const keepGoing = await dispatchReplInput(runtime, input, {
            first: options.first,
            exportPath: options.exportPath,
            signal: controller.signal,
          });
          if (!keepGoing) break;
        }
      } finally {
        holder.turn = null;
      }
    }
  } finally {
    removeSigint();
    rl.close();
  }
  return 0;
}

export async function runInteractiveGoal(
  repl: HarnessRepl,
  renderer: ReplEventRenderer,
  goal: string,
  options: ReplCliOptions,
  ask: (query: string) => Promise<string | null>,
  signal?: AbortSignal,
): Promise<void> {
  const iterator = repl.query(goal, { signal })[Symbol.asyncIterator]();
  let answered = false;
  let pageAll = false;
  try {
    while (true) {
      const { value, done } = await iterator.next();
      if (done) break;
      if (value.type === 'solution') {
        renderer.solution(value.solution?.bindings ?? [], '');
        answered = true;
        if (options.first) {
          renderer.writeLine('.');
          await iterator.return?.(undefined);
          return;
        }
        if (!pageAll) {
          // Pager: Enter or `;` pages the next solution, `a` prints all,
          // `.`/`q` stops. (Enter is the common "next" gesture in the TUI.)
          const key = await ask(' ;');
          const trimmed = (key ?? '').trim().toLowerCase();
          if (trimmed === 'a' || trimmed === 'w' || trimmed === 'p') {
            pageAll = true;
          } else if (trimmed === '.' || trimmed === 'q' || trimmed === 'd' || trimmed === 'stop') {
            renderer.writeLine('.');
            await iterator.return?.(undefined);
            return;
          }
          // Empty/Enter/`;`/anything else: fall through to the next solution.
        }
      } else if (value.type === 'error') {
        renderer.render(value);
      } else if (value.type !== 'finished') {
        // Render output/stream/tool_call/task_activity/usage/reasoning, exactly
        // like the `-g` path (runGoalWithRenderer); otherwise an interactive
        // goal is silent until its first solution.
        renderer.render(value);
      }
    }
  } finally {
    await iterator.return?.(undefined);
  }
  renderer.writeLine(answered ? '.' : 'false.');
}

/**
 * Interactive TUI for the REPL: a scrolling transcript, a fixed editor, and a
 * fixed status bar below the input. It reuses `ReplRenderer` (rich mode) for all
 * transcript formatting by redirecting its output through a `LineSink`.
 *
 * `ask_user` / `input/2` route through the same editor: a question is written
 * to the transcript and the next submitted line is returned to the runtime, so
 * input prompts never fight the main prompt for stdin.
 */
export async function runTui(
  runtime: ReplRuntime,
  options: ReplCliOptions,
  onExit?: () => Promise<void>,
  loaded: string[] = [],
): Promise<number> {
  initTuiTheme();
  // Sized, because pi-tui falls back to a hard 24 rows when the stream reports no
  // size, which leaves the bottom of a taller terminal untouched.
  const terminal = new SizedTerminal();
  // Fullscreen: an application-owned viewport (ScrollView) with the editor and
  // status bar pinned in a dock. In this mode pi-tui hands scrolling, mouse wheel,
  // text selection and search to the viewport, and reassigns layout on resize —
  // which is what makes the transcript scroll inside the app and the input box
  // behave when the terminal changes size.
  const tui = new TuiAltScreen(terminal, undefined, undefined, {
    mouse: true,
    wheelScrollLines: 'auto',
    copyOnSelect: true,
  });
  const document = new TranscriptDocument();
  const transcript = document;
  // The TUI renders components rather than lines: tool calls become panes and
  // streamed text becomes markdown. There is no external sink to wire.
  const working = new WorkingLine(tui);
  const renderer = new TuiRenderer({
    document,
    onWorking: (label) => {
      working.set(label);
      tui.requestRender();
    },
  });
  runtime.renderer = renderer;
  const runState = { running: false, steerable: false };
  /** Lines typed during a goal query, consumed by the solution pager. */
  const bufferedInput: string[] = [];
  const statusBar = new StatusBarComponent({
    getSnapshot: () => runtime.statusSnapshot(),
    workspace: runtime.workspace,
    sessionId: runtime.sessionId,
    harness: runtime.harnessName,
    backend: runtime.backend,
    isFrozen: () => runtime.repl.isFrozen(),
    isRunning: () => runState.running,
  });
  transcript.onChange = () => tui.requestRender();

  renderer.setImageSink((lines) => document.appendImageLines(lines));
  renderer.setWidth(terminal.columns);
  // pi-tui re-renders on resize but does not invalidate the component tree, and a
  // component that caches by width (a tool pane, markdown) would draw stale lines.
  const onResize = (): void => {
    terminal.refreshSize();
    renderer.setWidth(terminal.columns);
    tui.invalidate();
    tui.requestRender();
  };
  process.stdout.on('resize', onResize);

  const editorContainer = new Container();
  let editor: PromptEditor | undefined;
  let activePrompt = runtime.promptPrefix;
  let pendingIsInput = false;
  let pendingResolve: ((value: string | null) => void) | null = null;
  const holder: { turn: AbortController | null } = { turn: null };
  // The editor shows ask_user/input/2 questions in its border, so suppress the
  // duplicate `input_required` transcript line.
  runtime.renderer.setSuppressInputPrompt(true);

  // A single editor is reused for every prompt so up/down history persists for
  // the whole session (recreating it reset the history).
  const handleSubmit = (text: string): void => {
    const resolve = pendingResolve;
    const wasInput = pendingIsInput;
    pendingResolve = null;
    pendingIsInput = false;

    if (resolve) {
      // A pending ask (the main prompt, or ask_user / input/2): resolve it.
      if (text) {
        if (wasInput) {
          // A `ask_user` / `input/2` answer: keep a short record.
          transcript.append(formatInputAnswer(text));
        } else if (activePrompt === runtime.promptPrefix) {
          // A user turn: separate it from the previous output and from the
          // response that follows; render it as a pi-style blue block. The previous
          // turn's text block is closed first, so the new response starts below this.
          runtime.renderer.endTextBlock?.();
          if (transcript.getLines().length > 0) transcript.append('');
          transcript.append(text, 'user');
          transcript.append('');
        }
      }
    } else if (text.trim()) {
      // No pending ask means a turn is running.
      if (runState.steerable) {
        runtime.renderer.endTextBlock?.();
        transcript.append(text, 'steer');
        runtime.repl.steer(text);
      } else {
        // A goal query is running: queue the line for the solution pager.
        bufferedInput.push(text);
        transcript.append(text, 'steer');
      }
    }

    if (editor) {
      editor.setPrompt(null);
      editor.setText('');
      if (text) editor.addToHistory(text);
      tui.setFocus(editor);
    }
    tui.requestRender();
    resolve?.(text);
  };

  const createEditor = (): void => {
    editor = new PromptEditor(tui, editorTheme());
    editor.setAutocompleteProvider(createReplAutocomplete(runtime.workspace));
    editor.onSubmit = handleSubmit;
    editorContainer.clear();
    editorContainer.addChild(editor);
    tui.setFocus(editor);
  };

  /**
   * Wait for one submitted line. When `isInput` is set the question is shown in
   * the editor's top border (orange) to signal that the runtime needs input.
   */
  const ask = (label: string, isInput = false): Promise<string | null> => {
    activePrompt = label;
    pendingIsInput = isInput;
    // The solution pager consumes a line typed while the goal was running.
    if (label === ' ;') {
      const buffered = bufferedInput.shift();
      if (buffered !== undefined) return Promise.resolve(buffered);
    }
    return new Promise((resolve) => {
      if (editor) {
        const hint = isInput
          ? label
          : label === ' ;'
            ? 'Enter/; more · a all · . stop'
            : null;
        editor.setPrompt(hint);
        tui.setFocus(editor);
      }
      tui.requestRender();
      pendingResolve = resolve;
    });
  };

  // Route ask_user / input/2 through the editor prompt instead of the transcript.
  runtime.setOnInput(async (prompt, choices) => {
    const suffix = choices && choices.length > 0 ? ` (${choices.join('/')})` : '';
    const answer = await ask(`${prompt}${suffix}`, true);
    return answer === null ? undefined : answer;
  });

  /** Interrupt the running turn, if any. Returns whether there was one. */
  const interruptTurn = (): boolean => {
    if (!holder.turn) return false;
    holder.turn.abort(new Error('cancelled by user'));
    transcript.append('(interrupted — Ctrl-C exits)');
    tui.requestRender();
    return true;
  };

  /** Leave the REPL, aborting anything still running so nothing outlives the UI. */
  const exitRepl = (): void => {
    holder.turn?.abort(new Error('cancelled by user'));
    void (onExit ? onExit() : Promise.resolve()).finally(() => {
      tui.stop();
      process.exit(130);
    });
  };

  // Esc interrupts, Ctrl-C exits, Ctrl-D (on an empty editor) exits. Escape is
  // left alone while a completion list or an overlay owns it, so it keeps its
  // usual meaning there. Use pi's key matcher so kitty/meta encodings are
  // recognized, not just raw bytes.
  tui.addInputListener((data) => {
    if (matchesKey(data, 'ctrl+c')) {
      exitRepl();
      return { consume: true };
    }
    if (matchesKey(data, 'ctrl+d') && !holder.turn && (editor?.getText().length ?? 0) === 0) {
      exitRepl();
      return { consume: true };
    }
    if (
      matchesKey(data, 'escape')
      && !tui.hasOverlay()
      && !(editor?.isShowingAutocomplete() ?? false)
    ) {
      if (interruptTurn()) return { consume: true };
      return undefined;
    }
    // ctrl+o expands or collapses the most recent tool call, the way pi reveals
    // full tool output on demand.
    if (matchesKey(data, 'ctrl+o')) {
      if (renderer.toggleLastPane()) return { consume: true };
      return undefined;
    }
    return undefined;
  });
  // Fallback for terminals/raw-mode setups that deliver SIGINT instead of \x03:
  // the terminal's own Ctrl-C, which now exits.
  process.on('SIGINT', exitRepl);

  createEditor();
  for (const line of runtime.inputHistory) editor?.addToHistory(line);

  /**
   * Show a chooser over the transcript and resolve when it closes.
   *
   * The overlay owns input while it is up (pi-tui routes keys to the focused
   * overlay), so the editor does not see the keys the list is using.
   */
  const choose = (build: (done: (picked?: string) => void) => ChoicePicker): Promise<string | undefined> => {
    return new Promise((resolve) => {
      let handle: { hide(): void } | undefined;
      const done = (picked?: string): void => {
        handle?.hide();
        if (editor) tui.setFocus(editor);
        tui.requestRender();
        resolve(picked);
      };
      const picker = build(done);
      handle = tui.showOverlay(picker, {
        anchor: 'center',
        width: '70%',
        maxHeight: picker.contentHeight + 2,
      });
    });
  };

  const pickReasoningMode = async (): Promise<void> => {
    const modes: Array<'off' | 'summary' | 'full'> = ['off', 'summary', 'full'];
    const picked = await choose((done) => new ChoicePicker({
      title: 'Reasoning',
      hint: 'How much of the model\'s thinking to show in the transcript',
      current: runtime.reasoningMode,
      items: [
        { value: 'off', label: 'off', description: 'hide reasoning entirely' },
        { value: 'summary', label: 'summary', description: 'a bounded preview' },
        { value: 'full', label: 'full', description: 'everything the model emits' },
      ].filter((item) => modes.includes(item.value as 'off')),
      onSelect: (item) => done(item.value),
      onCancel: () => done(),
    }));
    if (!picked) return;
    const mode = picked as 'off' | 'summary' | 'full';
    runtime.reasoningMode = mode;
    runtime.renderer.setReasoningMode?.(mode);
    await persistSetting({ repl: { showReasoning: mode } }, 'reasoning display');
    renderer.writeLine(`reasoning display: ${mode}`);
  };

  /**
   * The model's reasoning level (its thinking effort), as opposed to how much of
   * that reasoning is displayed — `/show-reasoning` is the other one.
   */
  /**
   * Persist a setting, but never let that failure stop it applying.
   *
   * `updateConfig` requires an existing config file (`deepclause init`), and a
   * picker that threw before applying the change left the selection doing nothing at
   * all — which read as "I cannot adjust the reasoning".
   */
  const persistSetting = async (patch: Record<string, unknown>, label: string): Promise<void> => {
    try {
      await updateConfig(runtime.workspace, patch);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      renderer.writeLine(`${label} applied to this session, but not saved: ${message.split('\n')[0]}`);
    }
  };

  const pickReasoningLevel = async (level?: string): Promise<void> => {
    const backend = runtime.llmBackend;
    const supported = (backend?.supportedThinkingLevels?.() ?? THINKING_LEVELS.slice()) as string[];
    const apply = async (chosen: string): Promise<void> => {
      backend?.setThinkingLevel?.(chosen);
      runtime.thinkingLevel = chosen;
      runtime.setThinkingLevel?.(chosen);
      await persistSetting({ backend: { kind: 'pi', thinkingLevel: chosen } }, 'reasoning level');
      renderer.writeLine(`reasoning level: ${chosen}`);
    };

    if (level) {
      if (!supported.includes(level)) {
        renderer.writeLine(`unknown reasoning level '${level}'; supported: ${supported.join(', ')}`);
        return;
      }
      await apply(level);
      return;
    }

    const chosen = await choose((done) => new ChoicePicker({
      title: 'Reasoning level',
      hint: 'How hard the model is asked to think (not how much is shown)',
      current: runtime.thinkingLevel,
      items: supported.map((value) => ({
        value,
        label: value,
        description: value === 'off' ? 'no reasoning requested' : undefined,
      })),
      onSelect: (item) => done(item.value),
      onCancel: () => done(),
    }));
    if (chosen) await apply(chosen);
  };

  const pickModel = async (): Promise<void> => {
    const catalog = runtime.modelResolver?.catalog() ?? [];
    if (catalog.length === 0) {
      renderer.writeLine(`model: ${runtime.model} (no catalog to choose from; set one in config)`);
      return;
    }
    const current = runtime.model.includes(':') ? runtime.model : undefined;
    const picked = await choose((done) => new ChoicePicker({
      title: 'Model',
      hint: 'The default for this workspace (applies to the next session)',
      current,
      items: catalog.map((entry) => ({
        value: entry.model,
        label: entry.model,
        description: entry.inputCost !== undefined
          ? `$${entry.inputCost}/$${entry.outputCost ?? '?'} per 1M in/out`
          : (entry.contextWindow ? `${Math.round(entry.contextWindow / 1000)}k context` : undefined),
      })),
      onSelect: (item) => done(item.value),
      onCancel: () => done(),
    }));
    if (!picked) return;
    await persistSetting({ backend: { kind: 'pi', model: picked } }, 'model');
    renderer.writeLine(`default model set to ${picked} (takes effect on the next start)`);
  };
  // Transcript grows and scrolls; everything below it stays put.
  const scroll = new ScrollView(document, {
    follow: 'end',
    primary: true,
    scrollbar: 'auto',
    scrollbarTrackStyle: scrollbarStyles.track,
    scrollbarThumbStyle: scrollbarStyles.thumb,
  });
  // Margins: a blank row above the transcript and one between it and the editor, so
  // the first and last line of output are not jammed against the chrome. Fixed
  // spacers rather than padding inside the scroll view: they do not scroll away and
  // do not consume scrollback.
  const dock = new VStack([
    { component: working, basis: 'auto', grow: 0, shrink: 1, minSize: 0 },
    { component: new Spacer(1), basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
    { component: editorContainer, basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
    { component: statusBar, basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
  ]);
  tui.setLayoutRoot(new VStack([
    { component: new Spacer(1), basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
    { component: scroll, basis: 0, grow: 1, shrink: 1, minSize: 1 },
    { component: dock, basis: 'auto', grow: 0, shrink: 1, minSize: 1 },
  ]));
  tui.start();

  await printBanner(runtime, loaded);
  tui.requestRender();

  try {
    while (true) {
      const line = await ask(runtime.promptPrefix);
      if (line === null) break;
      const input = classifyReplInput(line);
      if (input.kind === 'halt') break;
      // The pickers are overlays over the transcript, so they need the TUI and are
      // handled here rather than in the command dispatcher the readline path shares.
      if (input.kind === 'model' && !input.model) { await pickModel(); continue; }
      if (input.kind === 'reasoning') { await pickReasoningLevel(input.level); continue; }
      if (input.kind === 'show-reasoning') { await pickReasoningMode(); continue; }

      const controller = new AbortController();
      holder.turn = controller;
      runState.running = true;
      runState.steerable = input.kind === 'prompt';
      bufferedInput.length = 0;
      tui.requestRender();
      try {
        if (input.kind === 'goal') {
          await runInteractiveGoal(runtime.repl, runtime.renderer, input.goal, options, ask, controller.signal);
        } else {
          const keepGoing = await dispatchReplInput(runtime, input, {
            first: options.first,
            exportPath: options.exportPath,
            signal: controller.signal,
          });
          if (!keepGoing) break;
        }
      } finally {
        holder.turn = null;
        runState.running = false;
        tui.requestRender();
      }
    }
  } finally {
    tui.stop();
    process.off('SIGINT', exitRepl);
    process.stdout.off('resize', onResize);
    working.stop();
    // Restore stdout so the exit footer and any later output are visible.
    runtime.renderer.setSink(
      (text) => process.stdout.write(text),
      (text) => process.stderr.write(text),
    );
  }
  return 0;
}

/** Entry point for `deepclause repl`. */
export async function startRepl(options: ReplCliOptions): Promise<number> {
  let runtime: ReplRuntime;
  try {
    runtime = await createReplRuntime(options);
    // Diagnostics from loading the runtime itself, rendered once and through the
    // renderer. Before this they were printed by Emscripten straight to the
    // terminal, in the middle of whatever the UI was drawing.
    for (const message of takeStartupPrologMessages()) {
      runtime.renderer.render({
        type: message.channel === 'stdout' ? 'output' : 'log',
        content: message.text,
        level: message.level,
        channel: message.channel,
      });
    }
  } catch (error) {
    process.stderr.write(`ERROR: ${(error as Error).message}\n`);
    return 1;
  }
  let bannerShown = false;

  // Save the harness to a temp bundle on exit (halt./Ctrl-D/Ctrl-C). Skipped
  // when an explicit --export path was given, when disabled, when already
  // saved, or when the harness has no predicates.
  let exitSaved = false;
  const saveHarnessOnExit = async (): Promise<void> => {
    if (exitSaved || options.exportPath || options.saveOnExit === false) return;
    exitSaved = true;
    try {
      if (runtime.repl.session.isEmpty()) return;
      const saved = await runtime.repl.session.saveCurrentHarness('exit', 'repl exit');
      if (saved) runtime.renderer.writeLine(`harness saved to ${saved}`);
    } catch {
      // best effort
    }
  };

  try {
    const loadExit = await executeLoad(runtime, options.load ?? []);
    if (loadExit !== 0) {
      return loadExit;
    }
    // A harness file loaded into a fresh harness becomes its baseline.
    if (options.load?.length) {
      await runtime.repl.session.finalizeHarnessBaseline();
    }

    // Freeze/lock only after all `-l` loads succeed (load before lock).
    if (options.locked) {
      await runtime.repl.lock();
    } else if (options.freeze) {
      await runtime.repl.freeze();
    }

    const goals = options.goal ?? [];
    if (goals.length > 0) {
      let exitCode = 0;
      const controller = new AbortController();
      const removeSigint = installSigint({ turn: controller });
      try {
        for (const goal of goals) {
          if (controller.signal.aborted) break;
          const code = await runGoalWithRenderer(runtime.repl, goal, {
            first: options.first,
            renderer: runtime.renderer,
            signal: controller.signal,
          });
          if (code !== 0) exitCode = code;
        }
      } finally {
        removeSigint();
      }
      return exitCode;
    }

    if (!process.stdin.isTTY) {
      return await runNonInteractive(runtime, options, saveHarnessOnExit);
    }
    bannerShown = true;
    const tuiMode = runtime.tuiMode;
    const wantTui = tuiMode !== 'off'
      && !options.plain
      && !options.json
      && (tuiMode === 'on' || Boolean(process.stdout.isTTY));
    if (wantTui) {
      return await runTui(runtime, options, saveHarnessOnExit, options.load ?? []);
    }
    await printBanner(runtime, options.load ?? []);
    return await runInteractive(runtime, options, saveHarnessOnExit);
  } finally {
    await saveHarnessOnExit();
    if (bannerShown) {
      const footer = runtime.historyEnabled
        ? `session ${runtime.sessionId}  ·  resume: deepclause repl --session ${runtime.sessionId}`
        : `session ${runtime.sessionId} (history disabled; this session is not resumable)`;
      runtime.renderer.writeLine(`\n${footer}`);
    }
    if (options.exportPath) {
      try {
        const result = await runtime.repl.export(options.exportPath);
        runtime.renderer.writeLine(
          `exported ${result.clauses} clauses, ${result.tools} tools, ${result.scripts} scripts → ${result.path}`,
        );
      } catch (error) {
        runtime.renderer.writeErrorLine(`ERROR: ${(error as Error).message}`);
      }
    }
    runtime.renderer.dispose();
    await runtime.repl.dispose();
    await runtime.toolset.dispose();
  }
}

/** Register the `repl` command on the CLI program. */
export function registerReplCommand(program: Command): void {
  program
    .command('repl')
    .description('Interactive DML REPL (goal mode; English prompts build the harness)')
    .option('-g, --goal <goal>', 'run one goal and exit (repeatable)', collect)
    .option('-l, --load <file>', 'load a DML file before starting (repeatable)', collect)
    .option('--session <id>', 'resume a previous session by id (or "last")')
    .option('--harness <name>', 'harness to edit (a project can hold several; default main)')
    .option('--first', 'with a goal, print only the first solution')
    .option('--plain', 'no TUI formatting (auto when stdout is not a TTY)')
    .option('--json', 'emit results as JSON lines')
    .option('--workspace <path>', 'workspace root for file operations', './')
    .option('--model <model>', 'override the configured run model')
    .option('--provider <provider>', 'override the configured provider')
    .option('--thinking <level>', 'thinking level for reasoning models (off|minimal|low|medium|high|xhigh|max)')
    .option('--temperature <number>', 'override temperature (0.0-2.0)', parseFloat)
    .option('--gas-limit <number>', 'per-frame step limit', parseInt)
    .option('--depth <number>', 'maximum nested frame depth (default 3)', parseInt)
    .option('--max-model-calls <number>', 'maximum model calls per turn (default 100)', parseInt)
    .option('--backend <kind>', 'override the configured backend (env|pi)')
    .option('--no-stream', 'hide streamed model text/reasoning (show only outputs and solutions)')
    .option('--show-reasoning <mode>', 'how much model reasoning to show: off|summary|full', (value: string) => {
      if (value !== 'off' && value !== 'summary' && value !== 'full') {
        throw new Error(`invalid --show-reasoning: ${value} (use off, summary, or full)`);
      }
      return value;
    })
    .option('--no-events', 'hide task activity, tool calls, usage, and logs')
    .option('--no-save-on-exit', 'do not save the harness to a temp bundle on exit')
    .option('--judge <name>', 'default judgment backend (llm|jev)')
    .option('--tools <list>', 'comma-separated capability host tools (default: bash)')
    .option('--no-tools', 'register no capability host tools')
    .option('--sandbox', 'run shell tools in AgentVM')
    .option('--require-sandbox', 'refuse to run unless the shell is sandboxed (bwrap/AgentVM/sandbox-exec)')
    .option('--no-ask', 'make ask_user / input/2 fail instead of prompting')
    .option('--image-model <provider:id>', 'image model for generation (default: first available)')
    .option('--no-open', 'do not hand images to the OS viewer on terminals without inline images')
    .option('--answer <text>', 'canned answer for any input request')
    .option('--interactive-input', 'allow input requests to read stdin in non-TTY/--json mode')
    .option('--tui <mode>', 'interactive UI: auto (TUI on a TTY), on, or off', (value: string) => {
      if (value !== 'auto' && value !== 'on' && value !== 'off') {
        throw new Error(`invalid --tui mode: ${value} (use auto, on, or off)`);
      }
      return value;
    })
    .option('-P, --param <key=value>', 'session parameter (repeatable)', collectParams, {})
    .option('-f, --freeze', 'start with the harness frozen (read-only lock)')
    .option('--locked', 'like --freeze, but forbid unfreeze for this process')
    .option('--export <file>', 'export the harness on exit')
    .option('--verbose', 'show debug output')
    .option('--debug', 'show debug output')
    .option('--no-color', 'disable ANSI color')
    .action(async (options) => {
      const exitCode = await startRepl({
        goal: options.goal,
        load: options.load,
        session: options.session,
        harness: options.harness,
        first: options.first,
        plain: options.plain,
        json: options.json,
        workspace: options.workspace,
        model: options.model,
        provider: options.provider,
        thinking: options.thinking,
        temperature: options.temperature,
        gasLimit: options.gasLimit,
        depth: options.depth,
        maxModelCalls: options.maxModelCalls,
        backend: options.backend,
        stream: options.stream,
        showReasoning: options.showReasoning,
        events: options.events,
        saveOnExit: options.saveOnExit,
        judge: options.judge,
        tools: typeof options.tools === 'string' ? options.tools : undefined,
        noTools: options.tools === false,
        sandbox: options.sandbox,
        requireSandbox: options.requireSandbox,
        noAsk: options.ask === false,
        imageModel: options.imageModel,
        open: options.open,
        answer: options.answer,
        interactiveInput: options.interactiveInput,
        tui: options.tui,
        param: options.param,
        verbose: Boolean(options.verbose || options.debug),
        color: options.color,
        freeze: options.freeze,
        locked: options.locked,
        exportPath: options.export,
      });
      if (exitCode !== 0) {
        process.exit(exitCode);
      }
    });
}

function collect(value: string, previous: string[] | undefined): string[] {
  return [...(previous ?? []), value];
}

/** Slug a harness name (letters, digits, `.`, `_`, `-`); undefined when empty. */
export function normalizeHarnessName(name?: string): string | undefined {
  const trimmed = name?.trim();
  if (!trimmed) return undefined;
  const slug = trimmed.replace(/[^A-Za-z0-9._-]+/g, '-').replace(/^-+|-+$/g, '');
  return slug || undefined;
}

/** Harness name implied by a `-l` file: its basename without the `.dml`/`.zip`. */
function harnessNameFromFile(file: string): string | undefined {
  return normalizeHarnessName(path.basename(file).replace(/\.(dml|zip)$/i, ''));
}

function collectParams(value: string, previous: Record<string, string>): Record<string, string> {
  const [key, val] = value.split('=');
  if (!key || val === undefined) {
    throw new Error(`Invalid param format: ${value}. Use key=value`);
  }
  return { ...previous, [key]: val };
}
