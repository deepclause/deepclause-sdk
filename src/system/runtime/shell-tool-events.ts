import type { DMLEvent } from '../../types.js';
import type { ShellExecObserver } from './agentvm-manager.js';

export function buildToolStartEvent(
  toolName: string,
  toolArgs: Record<string, unknown>,
): DMLEvent {
  return {
    type: 'tool_call',
    toolName,
    toolArgs,
    toolState: 'starting',
  };
}

export function buildToolCompletionEvent(
  toolName: string,
  toolArgs: Record<string, unknown>,
  toolResult: unknown,
): DMLEvent {
  return {
    type: 'tool_call',
    toolName,
    toolArgs,
    toolResult,
    ...inferToolLifecycleFields(toolResult),
  };
}

export function buildToolFailureEvent(
  toolName: string,
  toolArgs: Record<string, unknown>,
  error: unknown,
): DMLEvent {
  const message = error instanceof Error ? error.message : String(error);
  return {
    type: 'tool_call',
    toolName,
    toolArgs,
    toolState: 'failed',
    toolError: message,
    toolSummary: message,
  };
}

export function createShellToolEventBridge(options: {
  toolName: string;
  toolArgs: Record<string, unknown>;
  emit?: (event: DMLEvent) => void;
}): ShellExecObserver | undefined {
  if (!options.emit) {
    return undefined;
  }

  let stdoutRemainder = '';
  let stderrRemainder = '';
  let currentPid: number | undefined;
  let currentBackend: 'host' | 'sandbox' | undefined;

  const emitLine = (streamName: 'stdout' | 'stderr', line: string) => {
    if (!line) {
      return;
    }

    options.emit?.({
      type: 'log',
      content: formatShellStreamLine(options.toolName, streamName, line, currentPid, currentBackend),
    });
  };

  const flushBuffered = (streamName: 'stdout' | 'stderr') => {
    const remainder = streamName === 'stdout' ? stdoutRemainder : stderrRemainder;
    if (!remainder) {
      return;
    }
    emitLine(streamName, remainder);
    if (streamName === 'stdout') {
      stdoutRemainder = '';
      return;
    }
    stderrRemainder = '';
  };

  const pushChunk = (streamName: 'stdout' | 'stderr', chunk: string) => {
    const existing = streamName === 'stdout' ? stdoutRemainder : stderrRemainder;
    const normalized = `${existing}${chunk}`.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
    const parts = normalized.split('\n');
    const nextRemainder = parts.pop() ?? '';

    for (const line of parts) {
      emitLine(streamName, line);
    }

    if (streamName === 'stdout') {
      stdoutRemainder = nextRemainder;
      return;
    }
    stderrRemainder = nextRemainder;
  };

  return {
    onStart: (event) => {
      currentPid = event.pid;
      currentBackend = event.backend;
      options.emit?.({
        type: 'tool_call',
        toolName: options.toolName,
        toolArgs: options.toolArgs,
        toolState: 'running',
        toolPid: event.pid,
        toolBackend: event.backend,
      });
    },
    onStdout: (event) => {
      currentPid = event.pid ?? currentPid;
      currentBackend = event.backend ?? currentBackend;
      pushChunk('stdout', event.chunk);
    },
    onStderr: (event) => {
      currentPid = event.pid ?? currentPid;
      currentBackend = event.backend ?? currentBackend;
      pushChunk('stderr', event.chunk);
    },
    onExit: (event) => {
      currentPid = event.pid ?? currentPid;
      currentBackend = event.backend ?? currentBackend;
      flushBuffered('stdout');
      flushBuffered('stderr');
    },
  };
}

function inferToolLifecycleFields(
  toolResult: unknown,
): Pick<DMLEvent, 'toolState' | 'toolPid' | 'toolBackend' | 'toolExitCode' | 'toolSummary' | 'toolError' | 'toolDurationMs'> {
  let toolState: DMLEvent['toolState'] = 'completed';
  let toolPid: number | undefined;
  let toolBackend: 'host' | 'sandbox' | undefined;
  let toolExitCode: number | undefined;
  let toolSummary: string | undefined;
  let toolError: string | undefined;
  let toolDurationMs: number | undefined;

  if (toolResult && typeof toolResult === 'object') {
    const result = toolResult as Record<string, unknown>;

    if (result.success === false) {
      toolState = 'failed';
    }

    // A tool that refuses, or fails, by returning an error rather than throwing.
    // This was missing, so `{ error: "…" }` — including the runtime's own
    // "already running further up the call chain" refusal — was reported as a
    // completed call, and the transcript showed a tick for something that did not
    // happen. Only a non-empty string (or `{ message }`) counts, so a successful
    // result that carries `error: ""` or `error: null` is not mislabelled.
    const errorText = readErrorText(result.error);
    if (errorText) {
      toolState = 'failed';
      toolError = errorText;
    }
    if (typeof result.pid === 'number') {
      toolPid = result.pid;
    }
    if (result.backend === 'host' || result.backend === 'sandbox') {
      toolBackend = result.backend;
    }
    if (typeof result.exitCode === 'number') {
      toolExitCode = result.exitCode;
    }
    if (typeof result.summary === 'string' && result.summary.trim()) {
      toolSummary = result.summary;
    }
    if (typeof result.timeMs === 'number' && Number.isFinite(result.timeMs)) {
      toolDurationMs = result.timeMs;
    }
  }

  return {
    toolState,
    toolPid,
    toolBackend,
    toolExitCode,
    toolSummary: toolSummary ?? toolError,
    toolError,
    toolDurationMs,
  };
}

/** A non-empty error string from `error: "…"` or `error: { message: "…" }`. */
function readErrorText(value: unknown): string | undefined {
  if (typeof value === 'string') return value.trim() ? value : undefined;
  if (value && typeof value === 'object') {
    const message = (value as Record<string, unknown>).message;
    if (typeof message === 'string' && message.trim()) return message;
  }
  return undefined;
}

function formatShellStreamLine(
  toolName: string,
  streamName: 'stdout' | 'stderr',
  line: string,
  pid?: number,
  backend?: 'host' | 'sandbox',
): string {
  const pidSuffix = typeof pid === 'number' ? `[${pid}]` : backend === 'sandbox' ? '[sandbox]' : '';
  return `${toolName}${pidSuffix} ${streamName} ${line}`;
}