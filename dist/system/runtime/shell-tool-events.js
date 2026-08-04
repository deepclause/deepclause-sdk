export function buildToolStartEvent(toolName, toolArgs) {
    return {
        type: 'tool_call',
        toolName,
        toolArgs,
        toolState: 'starting',
    };
}
export function buildToolCompletionEvent(toolName, toolArgs, toolResult) {
    return {
        type: 'tool_call',
        toolName,
        toolArgs,
        toolResult,
        ...inferToolLifecycleFields(toolResult),
    };
}
export function buildToolFailureEvent(toolName, toolArgs, error) {
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
export function createShellToolEventBridge(options) {
    if (!options.emit) {
        return undefined;
    }
    let stdoutRemainder = '';
    let stderrRemainder = '';
    let currentPid;
    let currentBackend;
    const emitLine = (streamName, line) => {
        if (!line) {
            return;
        }
        options.emit?.({
            type: 'log',
            content: formatShellStreamLine(options.toolName, streamName, line, currentPid, currentBackend),
        });
    };
    const flushBuffered = (streamName) => {
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
    const pushChunk = (streamName, chunk) => {
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
function inferToolLifecycleFields(toolResult) {
    let toolState = 'completed';
    let toolPid;
    let toolBackend;
    let toolExitCode;
    let toolSummary;
    if (toolResult && typeof toolResult === 'object') {
        const result = toolResult;
        if (result.success === false) {
            toolState = 'failed';
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
    }
    return {
        toolState,
        toolPid,
        toolBackend,
        toolExitCode,
        toolSummary,
    };
}
function formatShellStreamLine(toolName, streamName, line, pid, backend) {
    const pidSuffix = typeof pid === 'number' ? `[${pid}]` : backend === 'sandbox' ? '[sandbox]' : '';
    return `${toolName}${pidSuffix} ${streamName} ${line}`;
}
//# sourceMappingURL=shell-tool-events.js.map