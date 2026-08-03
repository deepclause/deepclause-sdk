import * as fs from 'fs/promises';
import * as path from 'path';
import { emitKeypressEvents } from 'readline';
import { createInterface } from 'readline/promises';
import { stdin as input, stdout as output } from 'process';
import { listCommands } from './commands.js';
import { getConfigPath, loadConfig, resolveModelSlot, setModel } from './config.js';
import { run } from './run.js';
import { formatToolArgs } from './tool-args.js';
import { appendConductorSessionMessages, createSessionExecutionLogWriter, createLocalSkill, createConductorSession, getConductorSessionDetail, listConductorSessions, mergeSessionUsage, runConductorTurn, } from '../system/runtime/conductor.js';
import { getSystemAssetSourcePaths } from '../system/assets/index.js';
import { createShellManager, describeShellExecutionBackend, } from '../system/runtime/shell-manager.js';
import { buildToolCompletionEvent, buildToolFailureEvent, buildToolStartEvent, createShellToolEventBridge, } from '../system/runtime/shell-tool-events.js';
import { recordTokenUsage, mergeTokenUsageMaps } from '../system/runtime/token-usage.js';
const IGNORED_LIVE_LOG_TOOLS = new Set(['set_result', 'update_memory']);
const BUILTIN_SLASH_COMMANDS = ['new', 'sessions', 'help', 'run', 'compile', 'skill-creator', 'set-model', 'cancel', 'exit', 'quit'];
const MODEL_SLOTS = ['gateway', 'run', 'compile'];
const CHILD_EVENT_INDENT = '\t';
const CHILD_EVENT_TAB_WIDTH = 4;
const MAX_ACTIVITY_LINES = 400;
const SPINNER_FRAMES = ['|', '/', '-', '\\'];
const IDLE_RENDER_INTERVAL_MS = 16;
const BUSY_RENDER_INTERVAL_MS = 40;
const TYPING_RENDER_INTERVAL_MS = 90;
const COMBINING_MARK_RE = /\p{Mark}/u;
const EXTENDED_PICTOGRAPHIC_RE = /\p{Extended_Pictographic}/u;
const MAX_PERSISTED_SHELL_STREAM_CHARS = 4000;
const TEXT_FILE_EXTENSIONS = new Set([
    '.dml', '.md', '.txt', '.json', '.yaml', '.yml', '.toml', '.ts', '.tsx', '.js', '.jsx', '.mjs', '.cjs',
    '.css', '.scss', '.html', '.sql', '.py', '.sh', '.bash', '.zsh', '.pro', '.pl', '.csv', '.env', '.example',
]);
const TEXT_FILE_BASENAMES = new Set(['README', 'README.md', 'AGENTS.md', 'SPEC.md', 'ARCHITECTURE.md', 'CMD_LINE.md']);
const IGNORED_WORKSPACE_DIRS = new Set(['.git', 'node_modules', 'dist', '.next', 'coverage', 'agentvm-cache']);
const graphemeSegmenter = (() => {
    const Segmenter = Intl.Segmenter;
    return Segmenter ? new Segmenter(undefined, { granularity: 'grapheme' }) : null;
})();
const ANSI = {
    reset: '\u001b[0m',
    bold: '\u001b[1m',
    dim: '\u001b[2m',
    black: '\u001b[30m',
    red: '\u001b[31m',
    blue: '\u001b[34m',
    white: '\u001b[37m',
    green: '\u001b[32m',
    yellow: '\u001b[33m',
    cyan: '\u001b[36m',
    brightYellow: '\u001b[93m',
    brightCyan: '\u001b[96m',
    brightWhite: '\u001b[97m',
    bgBlue: '\u001b[48;2;0;0;95m',
    bgCyan: '\u001b[48;2;0;96;128m',
    bgWhite: '\u001b[47m',
};
export class LiveExecutionPrinter {
    write;
    writeLine;
    activeStreamKey = null;
    streamOpen = false;
    constructor(write = (text) => process.stdout.write(text), writeLine = (text) => console.log(text)) {
        this.write = write;
        this.writeLine = writeLine;
    }
    handle(logEvent) {
        const { event } = logEvent;
        if (event.type === 'usage' || event.type === 'finished' || event.type === 'task_activity') {
            return;
        }
        if (event.type === 'stream') {
            this.handleStream(logEvent);
            return;
        }
        this.flushStream();
        switch (event.type) {
            case 'tool_call':
                this.handleToolCall(logEvent);
                break;
            case 'memory_compaction':
                this.writeLine(formatMemoryCompactionLine(logEvent));
                break;
            case 'output':
                if (event.content) {
                    this.writeLine(`${formatEventPrefix(logEvent)}output ${event.content}`);
                }
                break;
            case 'answer':
                if (event.content) {
                    this.writeLine(`${formatEventPrefix(logEvent)}answer ${event.content}`);
                }
                break;
            case 'error':
                if (event.content) {
                    this.writeLine(`${formatEventPrefix(logEvent)}error ${event.content}`);
                }
                break;
            case 'log':
                if (event.content) {
                    this.writeLine(`${formatEventPrefix(logEvent)}log ${event.content}`);
                }
                break;
            case 'input_required':
                if (event.prompt) {
                    this.writeLine(`${formatEventPrefix(logEvent)}clarify ${event.prompt}`);
                }
                break;
        }
    }
    finish() {
        this.flushStream();
    }
    handleStream(logEvent) {
        const streamKey = logEvent.scope === 'child' ? `child:${logEvent.childSlug ?? '?'}` : 'main';
        if (this.activeStreamKey !== streamKey) {
            this.flushStream();
            this.write(`${streamLabel(logEvent)} `);
            this.activeStreamKey = streamKey;
            this.streamOpen = true;
        }
        if (logEvent.event.content) {
            this.write(logEvent.event.content);
        }
        if (logEvent.event.done) {
            this.flushStream();
        }
    }
    flushStream() {
        if (!this.streamOpen) {
            this.activeStreamKey = null;
            return;
        }
        this.write('\n');
        this.streamOpen = false;
        this.activeStreamKey = null;
    }
    handleToolCall(logEvent) {
        const line = formatToolEventLine(logEvent);
        if (!line) {
            return;
        }
        this.writeLine(line);
    }
}
export class ActivityBuffer {
    lines = [];
    activeTools = new Map();
    activeStreamKey = null;
    activeStreamLine = '';
    handle(logEvent) {
        const { event } = logEvent;
        if (event.type === 'usage' || event.type === 'finished' || event.type === 'task_activity') {
            return;
        }
        if (event.type === 'stream') {
            this.handleStream(logEvent);
            return;
        }
        this.flushStream();
        switch (event.type) {
            case 'tool_call':
                this.handleToolCall(logEvent);
                break;
            case 'memory_compaction':
                this.pushLine(formatMemoryCompactionLine(logEvent));
                break;
            case 'output':
                if (event.content) {
                    this.pushLine(`${formatEventPrefix(logEvent)}output ${event.content}`);
                }
                break;
            case 'answer':
                if (event.content) {
                    this.pushLine(`${formatEventPrefix(logEvent)}answer ${event.content}`);
                }
                break;
            case 'error':
                if (event.content) {
                    this.pushLine(`${formatEventPrefix(logEvent)}error ${event.content}`);
                }
                break;
            case 'log':
                if (event.content) {
                    this.pushLine(`${formatEventPrefix(logEvent)}log ${event.content}`);
                }
                break;
            case 'input_required':
                if (event.prompt) {
                    this.pushLine(`${formatEventPrefix(logEvent)}clarify ${event.prompt}`);
                }
                break;
        }
    }
    pushLine(line) {
        this.flushStream();
        if (!line) {
            return;
        }
        this.lines.push(line);
        this.trim();
    }
    finish() {
        this.flushStream();
        this.activeTools.clear();
    }
    clear() {
        this.lines.length = 0;
        this.activeTools.clear();
        this.activeStreamKey = null;
        this.activeStreamLine = '';
    }
    snapshot() {
        const body = this.activeStreamLine
            ? [...this.lines, this.activeStreamLine]
            : [...this.lines];
        const activeLines = this.buildActiveToolLines();
        if (activeLines.length === 0) {
            return body;
        }
        return body.length > 0
            ? [...activeLines, '', ...body]
            : activeLines;
    }
    snapshotTail(limit) {
        const boundedLimit = Math.max(1, limit);
        const body = this.activeStreamLine
            ? [...this.lines.slice(-boundedLimit), this.activeStreamLine]
            : this.lines.slice(-boundedLimit);
        const activeLines = this.buildActiveToolLines();
        if (activeLines.length === 0) {
            return body;
        }
        return body.length > 0
            ? [...activeLines, '', ...body]
            : activeLines;
    }
    handleStream(logEvent) {
        const streamKey = logEvent.scope === 'child' ? `child:${logEvent.childSlug ?? '?'}` : 'main';
        if (this.activeStreamKey !== streamKey) {
            this.flushStream();
            this.activeStreamKey = streamKey;
            this.activeStreamLine = `${streamLabel(logEvent)} `;
        }
        if (logEvent.event.content) {
            this.activeStreamLine += logEvent.event.content;
        }
        if (logEvent.event.done) {
            this.flushStream();
        }
    }
    flushStream() {
        if (!this.activeStreamLine) {
            this.activeStreamKey = null;
            return;
        }
        this.lines.push(this.activeStreamLine);
        this.activeStreamLine = '';
        this.activeStreamKey = null;
        this.trim();
    }
    trim() {
        if (this.lines.length > MAX_ACTIVITY_LINES) {
            this.lines.splice(0, this.lines.length - MAX_ACTIVITY_LINES);
        }
    }
    handleToolCall(logEvent) {
        const { event } = logEvent;
        if (!event.toolName || IGNORED_LIVE_LOG_TOOLS.has(event.toolName)) {
            return;
        }
        if (event.toolState) {
            const scopeKey = toolScopeKey(logEvent);
            if (event.toolState === 'starting' || event.toolState === 'running') {
                this.activeTools.set(scopeKey, {
                    scopeKey,
                    scopeLabel: toolScopeLabel(logEvent),
                    toolName: event.toolName,
                    toolState: event.toolState,
                    toolPid: event.toolPid,
                    toolBackend: event.toolBackend,
                });
            }
            else {
                this.activeTools.delete(scopeKey);
            }
        }
        const line = formatToolEventLine(logEvent);
        if (line) {
            this.pushLine(line);
        }
    }
    buildActiveToolLines() {
        if (this.activeTools.size === 0) {
            return [];
        }
        const lines = ['Active Tool Status'];
        const entries = [...this.activeTools.values()].sort((left, right) => left.scopeKey.localeCompare(right.scopeKey));
        for (const entry of entries) {
            lines.push(formatActiveToolStatus(entry));
        }
        return lines;
    }
}
function summarizeStepDescription(raw) {
    const firstLine = raw.split(/\r?\n/)[0] ?? '';
    const trimmed = firstLine.replace(/\s+/g, ' ').trim();
    return trimmed.length > 80 ? trimmed.slice(0, 77) + '\u2026' : trimmed;
}
export class TaskTracker {
    entries = [];
    activeStack = [];
    maxEntries = 200;
    handle(logEvent) {
        const { event } = logEvent;
        if (event.type !== 'task_activity')
            return;
        if (event.taskState === 'started' && event.taskId) {
            const depth = this.activeStack.length;
            this.activeStack.push(event.taskId);
            this.entries.push({
                id: event.taskId,
                description: summarizeStepDescription(event.taskDescription ?? ''),
                state: 'started',
                depth,
                startedAt: Date.now(),
            });
            this.trim();
        }
        else if ((event.taskState === 'completed' || event.taskState === 'failed') && event.taskId) {
            const idx = this.entries.findIndex(e => e.id === event.taskId);
            if (idx >= 0) {
                this.entries[idx].state = event.taskState;
                this.entries[idx].completedAt = Date.now();
            }
            const stackIdx = this.activeStack.lastIndexOf(event.taskId);
            if (stackIdx >= 0) {
                this.activeStack.splice(stackIdx, 1);
            }
        }
    }
    buildPaneBody(_width) {
        if (this.entries.length === 0) {
            return ['No steps yet.', 'Step activity renders here.'];
        }
        const lines = [];
        for (const entry of this.entries) {
            const indent = '  '.repeat(entry.depth);
            let marker;
            if (entry.state === 'started') {
                marker = '>';
            }
            else if (entry.state === 'completed') {
                marker = '\u2713';
            }
            else {
                marker = '\u2717';
            }
            lines.push(`${indent}${marker} ${entry.description}`);
        }
        return lines;
    }
    clear() {
        this.entries.length = 0;
        this.activeStack.length = 0;
    }
    trim() {
        if (this.entries.length > this.maxEntries) {
            this.entries.splice(0, this.entries.length - this.maxEntries);
        }
    }
}
export function releaseTuiInputStream(stream) {
    if (stream.isTTY && typeof stream.setRawMode === 'function') {
        stream.setRawMode(false);
    }
    stream.pause();
}
class MouseParser {
    buffer = '';
    feed(data, onEvent) {
        this.buffer += data.toString('utf8');
        while (this.buffer.length > 0) {
            const mouseStart = this.buffer.indexOf('\u001b[<');
            if (mouseStart === -1) {
                if (this.buffer.length > 0) {
                    this.buffer = '';
                }
                break;
            }
            this.buffer = this.buffer.slice(mouseStart);
            const parsed = this.tryParseMouse();
            if (parsed === null) {
                if (this.buffer.length > 256) {
                    this.buffer = '';
                }
                break;
            }
            onEvent(parsed);
        }
    }
    tryParseMouse() {
        const mIdx = this.buffer.indexOf('M', 3);
        const mIdxSgr = this.buffer.indexOf('m', 3);
        const end = mIdx !== -1 && (mIdxSgr === -1 || mIdx < mIdxSgr) ? mIdx : mIdxSgr;
        if (end === -1)
            return null;
        const payload = this.buffer.slice(3, end);
        const isRelease = this.buffer[end] === 'm';
        this.buffer = this.buffer.slice(end + 1);
        const semicolon = payload.indexOf(';');
        if (semicolon === -1)
            return null;
        const secondSemicolon = payload.indexOf(';', semicolon + 1);
        if (secondSemicolon === -1)
            return null;
        const cb = Number.parseInt(payload.slice(0, semicolon), 10);
        const col = Number.parseInt(payload.slice(semicolon + 1, secondSemicolon), 10);
        const row = Number.parseInt(payload.slice(secondSemicolon + 1), 10);
        if (Number.isNaN(cb) || Number.isNaN(col) || Number.isNaN(row))
            return null;
        const shift = (cb & 4) !== 0;
        const meta = (cb & 8) !== 0;
        const ctrl = (cb & 16) !== 0;
        const buttonCode = cb & 3;
        if (isRelease) {
            return { button: 'release', row, column: col, shift, ctrl, meta };
        }
        if ((cb & 64) !== 0) {
            return { button: buttonCode === 0 ? 'wheel-up' : 'wheel-down', row, column: col, shift, ctrl, meta };
        }
        const isMotion = (cb & 32) !== 0;
        if (isMotion) {
            return { button: 'motion', row, column: col, shift, ctrl, meta };
        }
        const buttonMap = { 0: 'left', 1: 'middle', 2: 'right' };
        return { button: buttonMap[buttonCode] ?? 'left', row, column: col, shift, ctrl, meta };
    }
}
class FullscreenTui {
    workspaceRoot;
    options;
    sessions = [];
    selectedSessionId = '';
    sessionDetail = null;
    liveUsageByModel = {};
    commands = [];
    activityBySessionId = new Map();
    ephemeralMessagesBySessionId = new Map();
    taskTrackerBySessionId = new Map();
    focusedPane = 'messages';
    uiMode = 'command';
    menuReturnMode = 'command';
    menuState = {
        activeIndex: 0,
        selectedIndex: 0,
        typeahead: '',
        typeaheadAt: 0,
    };
    paneScroll = {
        sessions: 0,
        messages: 0,
        process: 0,
        tasks: 0,
        context: 0,
    };
    lastPaneMetrics = {
        sessions: { maxStart: 0, viewportLines: 0, maxStartIsExact: true },
        messages: { maxStart: 0, viewportLines: 0, maxStartIsExact: true },
        process: { maxStart: 0, viewportLines: 0, maxStartIsExact: true },
        tasks: { maxStart: 0, viewportLines: 0, maxStartIsExact: true },
        context: { maxStart: 0, viewportLines: 0, maxStartIsExact: true },
    };
    currentPreview = null;
    currentAbortController = null;
    toolAbortController = null;
    toolAbortSignalRef = {};
    mouseParser = null;
    mouseDataHandler = null;
    mouseEscapeState = 'idle';
    selection = null;
    isDraggingSelection = false;
    inputValue = '';
    cursor = 0;
    busy = false;
    exitRequested = false;
    statusLine = 'Loading...';
    pendingPrompt = null;
    pickerState = null;
    viewerState = null;
    editorState = null;
    recentFiles = [];
    lastSubmittedInput = null;
    shellWorkspacePath;
    shellConfig = { wrapper: 'auto', strictIsolation: false };
    renderTimer = null;
    animationTimer = null;
    closeResolver = null;
    lastRenderAt = 0;
    previousFrameLines = null;
    previousFrameSize = null;
    resizeHandler = null;
    wrapCache = new Map();
    constructor(workspaceRoot, options = {}) {
        this.workspaceRoot = workspaceRoot;
        this.options = options;
        this.shellWorkspacePath = path.resolve(workspaceRoot, 'workspace');
    }
    async start() {
        try {
            await this.refreshCommands();
            await this.refreshSessions({ createIfMissing: true });
            this.statusLine = `Ready. Shell ${this.getShellBackend().backendLabel} in ${path.relative(this.workspaceRoot, this.shellWorkspacePath) || '.'}. F10 or Ctrl+G opens menus. Left/right changes pane when the menu is closed.`;
            if (input.isTTY) {
                input.setRawMode(true);
            }
            emitKeypressEvents(input);
            input.on('keypress', this.onKeypress);
            this.mouseParser = new MouseParser();
            this.mouseDataHandler = (chunk) => {
                this.mouseParser.feed(chunk, (event) => this.handleMouseEvent(event));
            };
            input.prependListener('data', this.mouseDataHandler);
            this.resizeHandler = () => {
                this.wrapCache.clear();
                this.requestRender();
            };
            output.on('resize', this.resizeHandler);
            output.write('\u001b[?1049h\u001b[?1002h\u001b[?1006h\u001b[2J\u001b[H');
            this.requestRender();
            await new Promise((resolve) => {
                this.closeResolver = resolve;
            });
        }
        finally {
            if (this.renderTimer) {
                clearTimeout(this.renderTimer);
                this.renderTimer = null;
            }
            if (this.animationTimer) {
                clearInterval(this.animationTimer);
                this.animationTimer = null;
            }
            if (this.mouseDataHandler) {
                input.removeListener('data', this.mouseDataHandler);
                this.mouseDataHandler = null;
            }
            this.mouseParser = null;
            input.removeListener('keypress', this.onKeypress);
            if (this.resizeHandler) {
                output.off('resize', this.resizeHandler);
                this.resizeHandler = null;
            }
            releaseTuiInputStream(input);
            this.previousFrameLines = null;
            this.previousFrameSize = null;
            output.write('\u001b[?25h\u001b[?1006l\u001b[?1002l\u001b[?1049l');
        }
    }
    onKeypress = (text, key) => {
        if (key.sequence === '\x1b[<') {
            this.mouseEscapeState = 'consuming';
            return;
        }
        if (this.mouseEscapeState === 'consuming') {
            if (text === 'M' || text === 'm') {
                this.mouseEscapeState = 'idle';
            }
            return;
        }
        void this.handleKeypress(text, key);
    };
    handleMouseEvent(event) {
        if (event.button === 'wheel-up' || event.button === 'wheel-down') {
            if (event.shift)
                return;
            const direction = event.button === 'wheel-up' ? 'up' : 'down';
            const paneKind = this.paneKindAtPosition(event.row, event.column);
            if (paneKind) {
                this.focusedPane = paneKind;
            }
            this.scrollFocusedPane(direction, 3);
            return;
        }
        if (event.button === 'left' && !event.shift && !event.ctrl && !event.meta) {
            const menuIndex = this.menuIndexAtColumn(event.column);
            if (menuIndex !== null && event.row === 1) {
                this.clearSelection();
                if (this.uiMode !== 'menu') {
                    this.uiMode = 'menu';
                }
                this.menuState.activeIndex = menuIndex;
                this.menuState.selectedIndex = 0;
                this.menuState.typeahead = '';
                this.requestRender();
                return;
            }
            if (this.uiMode === 'menu' && event.row >= 2) {
                const menuItemIndex = this.menuItemAtRow(event.row);
                if (menuItemIndex !== null) {
                    this.menuState.selectedIndex = menuItemIndex;
                    this.requestRender();
                    void this.activateCurrentMenuItem();
                    return;
                }
            }
            const paneKind = this.paneKindAtPosition(event.row, event.column);
            if (!paneKind) {
                this.clearSelection();
                this.requestRender();
                return;
            }
            const panePos = this.paneContentPosition(paneKind, event.row, event.column);
            if (!panePos) {
                this.clearSelection();
                this.focusedPane = paneKind;
                this.requestRender();
                return;
            }
            this.selection = { pane: paneKind, startRow: panePos.bodyRow, startCol: panePos.innerCol, endRow: panePos.bodyRow, endCol: panePos.innerCol };
            this.isDraggingSelection = true;
            this.focusedPane = paneKind;
            this.requestRender();
            return;
        }
        if (event.button === 'motion' && !event.shift && this.isDraggingSelection && this.selection) {
            const paneKind = this.paneKindAtPosition(event.row, event.column);
            if (paneKind && paneKind === this.selection.pane) {
                const panePos = this.paneContentPosition(paneKind, event.row, event.column);
                if (panePos) {
                    this.selection.endRow = panePos.bodyRow;
                    this.selection.endCol = panePos.innerCol;
                    this.requestRender();
                }
            }
            return;
        }
        if (event.button === 'release' && !event.shift && this.isDraggingSelection) {
            this.isDraggingSelection = false;
            if (this.selection) {
                const text = this.extractSelectionText();
                if (text) {
                    this.copyToClipboard(text);
                    this.statusLine = 'Copied selection to clipboard.';
                }
            }
            this.clearSelection();
            this.requestRender();
            return;
        }
        if (event.button === 'release') {
            this.isDraggingSelection = false;
            return;
        }
    }
    clearSelection() {
        if (this.selection) {
            this.selection = null;
        }
        this.isDraggingSelection = false;
    }
    paneContentPosition(paneKind, screenRow, screenCol) {
        const columns = output.columns ?? 120;
        const rows = output.rows ?? 40;
        if (screenRow < 2 || screenRow >= rows - 1)
            return null;
        if (this.uiMode !== 'command')
            return null;
        const contentHeight = rows - 3;
        const widths = computePaneWidths(columns);
        const rightHeights = computeRightPaneHeights(contentHeight);
        const contentRow = screenRow - 2;
        let paneLeft;
        let paneWidth;
        let paneTop;
        switch (paneKind) {
            case 'sessions':
                paneLeft = 0;
                paneWidth = widths.left;
                paneTop = 0;
                break;
            case 'messages':
                paneLeft = widths.left + 1;
                paneWidth = widths.center;
                paneTop = 0;
                break;
            case 'process':
                paneLeft = widths.left + 1 + widths.center + 1;
                paneWidth = widths.right;
                paneTop = 0;
                break;
            case 'tasks':
                paneLeft = widths.left + 1 + widths.center + 1;
                paneWidth = widths.right;
                paneTop = rightHeights.process;
                break;
            case 'context':
                paneLeft = widths.left + 1 + widths.center + 1;
                paneWidth = widths.right;
                paneTop = rightHeights.process + rightHeights.tasks;
                break;
        }
        if (screenCol < paneLeft + 2 || screenCol > paneLeft + paneWidth - 1)
            return null;
        const bodyRow = contentRow - paneTop - 1;
        if (bodyRow < 0)
            return null;
        const innerCol = screenCol - paneLeft - 2;
        return { bodyRow, innerCol };
    }
    extractSelectionText() {
        if (!this.selection)
            return '';
        const sel = this.selection;
        const paneBody = this.getPaneBody(sel.pane);
        const columns = output.columns ?? 120;
        const widths = computePaneWidths(columns);
        const width = sel.pane === 'sessions' ? widths.left : sel.pane === 'messages' ? widths.center : widths.right;
        const innerWidth = Math.max(1, width - 2);
        const wrappedLines = wrapBodyLines(paneBody, innerWidth);
        const startRow = Math.min(sel.startRow, sel.endRow);
        const endRow = Math.max(sel.startRow, sel.endRow);
        const startCol = sel.startRow <= sel.endRow ? sel.startCol : sel.endCol;
        const endCol = sel.startRow <= sel.endRow ? sel.endCol : sel.startCol;
        if (startRow === endRow) {
            const line = wrappedLines[startRow] ?? '';
            return this.stripAnsi(line.slice(startCol, endCol + 1));
        }
        const parts = [];
        const firstLine = wrappedLines[startRow] ?? '';
        parts.push(this.stripAnsi(firstLine.slice(startCol)));
        for (let row = startRow + 1; row < endRow; row++) {
            parts.push(this.stripAnsi(wrappedLines[row] ?? ''));
        }
        const lastLine = wrappedLines[endRow] ?? '';
        parts.push(this.stripAnsi(lastLine.slice(0, endCol + 1)));
        return parts.join('\n');
    }
    stripAnsi(text) {
        return text.replace(/\u001b\[[0-9;]*m/g, '');
    }
    copyToClipboard(text) {
        const osc52 = `\u001b]52;c;${Buffer.from(text).toString('base64')}\u0007`;
        output.write(osc52);
    }
    applySelectionHighlight(lines, widths, rightHeights, _contentHeight) {
        const sel = this.selection;
        if (!sel)
            return;
        const startRow = Math.min(sel.startRow, sel.endRow);
        const endRow = Math.max(sel.startRow, sel.endRow);
        const startCol = sel.startRow <= sel.endRow ? sel.startCol : sel.endCol;
        const endCol = sel.startRow <= sel.endRow ? sel.endCol : sel.startCol;
        let paneLeft;
        let paneWidth;
        let paneTop;
        switch (sel.pane) {
            case 'sessions':
                paneLeft = 0;
                paneWidth = widths.left;
                paneTop = 0;
                break;
            case 'messages':
                paneLeft = widths.left + 1;
                paneWidth = widths.center;
                paneTop = 0;
                break;
            case 'process':
                paneLeft = widths.left + 1 + widths.center + 1;
                paneWidth = widths.right;
                paneTop = 0;
                break;
            case 'tasks':
                paneLeft = widths.left + 1 + widths.center + 1;
                paneWidth = widths.right;
                paneTop = rightHeights.process;
                break;
            case 'context':
                paneLeft = widths.left + 1 + widths.center + 1;
                paneWidth = widths.right;
                paneTop = rightHeights.process + rightHeights.tasks;
                break;
        }
        const innerDisplayStart = paneLeft + 1;
        const innerDisplayEnd = paneLeft + paneWidth - 2;
        for (let bodyRow = startRow; bodyRow <= endRow; bodyRow++) {
            const screenLineIdx = 1 + paneTop + 1 + bodyRow;
            if (screenLineIdx >= lines.length)
                break;
            const line = lines[screenLineIdx];
            const rowStartCol = bodyRow === startRow ? innerDisplayStart + startCol : innerDisplayStart;
            const rowEndCol = bodyRow === endRow ? innerDisplayStart + endCol + 1 : innerDisplayEnd + 1;
            const clampedStartCol = Math.max(innerDisplayStart, rowStartCol);
            const clampedEndCol = Math.min(innerDisplayEnd + 1, rowEndCol);
            if (clampedStartCol >= clampedEndCol)
                continue;
            const strStart = displayColumnToStringIndex(line, clampedStartCol);
            const strEnd = displayColumnToStringIndex(line, clampedEndCol);
            const before = line.slice(0, strStart);
            const segment = line.slice(strStart, strEnd);
            const after = line.slice(strEnd);
            const highlighted = this.invertAnsiColors(segment);
            lines[screenLineIdx] = `${before}${highlighted}${after}`;
        }
    }
    invertAnsiColors(text) {
        return `\u001b[7m${text}\u001b[27m`;
    }
    paneKindAtPosition(row, column) {
        const columns = output.columns ?? 120;
        const rows = output.rows ?? 40;
        if (row < 2 || row >= rows - 1)
            return null;
        if (this.uiMode !== 'command')
            return null;
        const widths = computePaneWidths(columns);
        const rightHeights = computeRightPaneHeights(rows - 3);
        const contentRow = row - 1;
        if (column <= widths.left)
            return 'sessions';
        if (column <= widths.left + 1 + widths.center)
            return 'messages';
        if (contentRow <= rightHeights.process)
            return 'process';
        if (contentRow <= rightHeights.process + rightHeights.tasks)
            return 'tasks';
        return 'context';
    }
    menuIndexAtColumn(column) {
        const menuLabels = ['Session', 'Skills', 'Files', 'Run', 'View', 'Help'];
        let pos = 1 + measureDisplayWidth(' DeepClause ');
        for (let i = 0; i < menuLabels.length; i++) {
            const labelWidth = measureDisplayWidth(` [${i + 1}] ${menuLabels[i]} `);
            if (column >= pos && column < pos + labelWidth)
                return i;
            pos += labelWidth;
        }
        return null;
    }
    menuItemAtRow(row) {
        if (this.uiMode !== 'menu')
            return null;
        const contentRow = row - 2;
        if (contentRow < 2)
            return null;
        const itemRow = contentRow - 2;
        if (itemRow < 0)
            return null;
        const linesPerItem = 3;
        const itemIndex = Math.floor(itemRow / linesPerItem);
        const menu = this.getMenuDefinitions()[this.menuState.activeIndex];
        if (!menu || itemIndex >= menu.items.length || itemIndex < 0)
            return null;
        return itemIndex;
    }
    async activateCurrentMenuItem() {
        const menu = this.getMenuDefinitions()[this.menuState.activeIndex];
        if (!menu)
            return;
        const item = menu.items[this.menuState.selectedIndex];
        if (!item || item.enabled === false)
            return;
        await this.runMenuAction(item.id);
    }
    async handleKeypress(text, key) {
        if ((key.ctrl && key.name === 'g') || key.name === 'f10') {
            this.toggleMenuMode();
            return;
        }
        if (key.ctrl && key.name === 'p' && !this.pendingPrompt && !this.busy) {
            this.openActionPalette();
            return;
        }
        if (key.ctrl && key.name === 'w') {
            this.cyclePaneFocus(1);
            return;
        }
        if (key.ctrl && key.name === 'c') {
            if (this.pendingPrompt) {
                const pending = this.pendingPrompt;
                this.pendingPrompt = null;
                pending.resolve('');
                if (!this.busy) {
                    this.statusLine = 'Input cancelled.';
                    this.requestRender();
                    return;
                }
            }
            if (this.busy) {
                this.exitRequested = true;
                this.cancelCurrentExecution('Cancelling current execution, then quitting...');
                return;
            }
            this.close();
            return;
        }
        if (key.ctrl && key.name === 't' && this.busy) {
            if (this.toolAbortController && !this.toolAbortController.signal.aborted) {
                this.toolAbortController.abort('Aborted by user');
                this.toolAbortController = null;
                this.toolAbortSignalRef.signal = undefined;
                this.statusLine = 'Tool execution cancelled.';
            }
            else {
                this.statusLine = 'No active tool to cancel.';
            }
            this.requestRender();
            return;
        }
        if (this.uiMode === 'menu') {
            await this.handleMenuKeypress(text, key);
            return;
        }
        if (this.uiMode === 'picker' || this.uiMode === 'palette') {
            await this.handlePickerKeypress(text, key);
            return;
        }
        if (this.uiMode === 'viewer') {
            await this.handleViewerKeypress(key);
            return;
        }
        if (this.uiMode === 'editor') {
            await this.handleEditorKeypress(text, key);
            return;
        }
        switch (key.name) {
            case 'return':
                await this.submitInput();
                return;
            case 'backspace':
                this.deleteBackward();
                break;
            case 'delete':
                this.deleteForward();
                break;
            case 'left':
                if (!this.inputValue) {
                    this.cyclePaneFocus(-1);
                    return;
                }
                if (this.cursor > 0) {
                    this.cursor = this.graphemeLeftIndex(this.inputValue, this.cursor);
                }
                break;
            case 'right':
                if (!this.inputValue) {
                    this.cyclePaneFocus(1);
                    return;
                }
                if (this.cursor < this.inputValue.length) {
                    this.cursor = this.graphemeRightIndex(this.inputValue, this.cursor);
                }
                break;
            case 'home':
                if (!this.inputValue) {
                    this.jumpFocusedPane('top');
                    return;
                }
                this.cursor = 0;
                break;
            case 'end':
                if (!this.inputValue) {
                    this.jumpFocusedPane('bottom');
                    return;
                }
                this.cursor = this.inputValue.length;
                break;
            case 'up':
                if (!this.inputValue) {
                    await this.handleUpDown(-1);
                    return;
                }
                break;
            case 'down':
                if (!this.inputValue) {
                    await this.handleUpDown(1);
                    return;
                }
                break;
            case 'pageup':
                if (!this.inputValue) {
                    this.scrollFocusedPane('up', this.pageScrollAmount());
                    return;
                }
                break;
            case 'pagedown':
                if (!this.inputValue) {
                    this.scrollFocusedPane('down', this.pageScrollAmount());
                    return;
                }
                break;
            case 'tab':
                await this.applyCompletion();
                return;
            case 'escape':
                {
                    const previousInput = this.inputValue;
                    this.inputValue = '';
                    this.cursor = 0;
                    this.refreshCommandBarStatus(previousInput);
                }
                break;
            default:
                if (text && !key.ctrl && !key.meta) {
                    this.insertText(text);
                }
                break;
        }
        this.requestRender();
    }
    async handleMenuKeypress(text, key) {
        const menus = this.getMenuDefinitions();
        const activeMenu = menus[this.menuState.activeIndex];
        const activeItems = activeMenu?.items ?? [];
        if (key.name === 'escape') {
            this.closeMenuMode();
            return;
        }
        if (key.name && /^[1-6]$/.test(key.name)) {
            this.selectMenu(Number.parseInt(key.name, 10) - 1);
            return;
        }
        switch (key.name) {
            case 'left':
                this.moveMenu(-1);
                return;
            case 'right':
                this.moveMenu(1);
                return;
            case 'up':
                this.moveMenuItem(-1, activeItems.length);
                return;
            case 'down':
                this.moveMenuItem(1, activeItems.length);
                return;
            case 'home':
                this.menuState.selectedIndex = 0;
                this.statusLine = `${activeMenu?.label ?? 'Menu'} menu.`;
                this.requestRender();
                return;
            case 'end':
                this.menuState.selectedIndex = Math.max(0, activeItems.length - 1);
                this.statusLine = `${activeMenu?.label ?? 'Menu'} menu.`;
                this.requestRender();
                return;
            case 'return':
            case 'space': {
                const selected = activeItems[this.menuState.selectedIndex];
                if (selected) {
                    if (selected.enabled === false) {
                        this.statusLine = `${selected.label} is not available right now.`;
                        this.requestRender();
                        return;
                    }
                    await this.runMenuAction(selected.id);
                }
                return;
            }
            case 'backspace':
                if (this.menuState.typeahead) {
                    this.menuState.typeahead = this.menuState.typeahead.slice(0, -1);
                    this.menuState.typeaheadAt = Date.now();
                    this.requestRender();
                }
                return;
            default:
                break;
        }
        if (text && !key.ctrl && !key.meta && /\S/.test(text)) {
            const now = Date.now();
            this.menuState.typeahead = now - this.menuState.typeaheadAt > 900
                ? text.toLowerCase()
                : `${this.menuState.typeahead}${text.toLowerCase()}`;
            this.menuState.typeaheadAt = now;
            this.menuState.selectedIndex = selectMenuItemByTypeahead(activeItems, this.menuState.typeahead, this.menuState.selectedIndex);
            this.requestRender();
        }
    }
    async handlePickerKeypress(text, key) {
        const picker = this.pickerState;
        if (!picker) {
            this.closeWorkspaceMode();
            return;
        }
        switch (key.name) {
            case 'escape':
                this.closeWorkspaceMode();
                return;
            case 'return':
                await this.activatePickerSelection();
                return;
            case 'up':
                this.movePickerSelection(-1);
                return;
            case 'down':
                this.movePickerSelection(1);
                return;
            case 'pageup':
                this.movePickerSelection(-Math.max(3, Math.floor(this.workspaceViewportLines() / 2)));
                return;
            case 'pagedown':
                this.movePickerSelection(Math.max(3, Math.floor(this.workspaceViewportLines() / 2)));
                return;
            case 'home':
                picker.selectedIndex = 0;
                this.ensurePickerSelectionVisible();
                this.requestRender();
                return;
            case 'end':
                picker.selectedIndex = Math.max(0, picker.filteredItems.length - 1);
                this.ensurePickerSelectionVisible();
                this.requestRender();
                return;
            case 'backspace':
                this.deleteBackward();
                this.refreshPickerFilter();
                return;
            case 'delete':
                this.deleteForward();
                this.refreshPickerFilter();
                return;
            case 'left':
                if (this.cursor > 0) {
                    this.cursor = this.graphemeLeftIndex(this.inputValue, this.cursor);
                    this.requestRender();
                }
                return;
            case 'right':
                if (this.cursor < this.inputValue.length) {
                    this.cursor = this.graphemeRightIndex(this.inputValue, this.cursor);
                    this.requestRender();
                }
                return;
            default:
                break;
        }
        if (key.ctrl && key.name === 'e') {
            await this.activatePickerSelection('edit');
            return;
        }
        if (key.ctrl && key.name === 'r') {
            await this.activatePickerSelection('run');
            return;
        }
        if (key.ctrl && key.name === 'm') {
            await this.activatePickerSelection('meta');
            return;
        }
        if (key.ctrl && key.name === 's') {
            await this.activatePickerSelection('source');
            return;
        }
        if (text && !key.ctrl && !key.meta) {
            this.insertText(text);
            this.refreshPickerFilter();
        }
    }
    async handleViewerKeypress(key) {
        if (!this.viewerState) {
            this.closeWorkspaceMode();
            return;
        }
        if (key.ctrl && key.name === 'e' && this.viewerState.canEdit && this.viewerState.path) {
            await this.openEditorForFile(this.viewerState.path, { title: this.viewerState.title, preferredExtension: this.viewerState.preferredExtension });
            return;
        }
        if (key.ctrl && key.name === 'r' && this.viewerState.skillName) {
            this.closeWorkspaceMode();
            await this.executeSkill(this.viewerState.skillName, []);
            return;
        }
        switch (key.name) {
            case 'escape':
                this.closeWorkspaceMode();
                return;
            case 'up':
                this.viewerState.scrollTop = Math.max(0, this.viewerState.scrollTop - 1);
                this.requestRender();
                return;
            case 'down':
                this.viewerState.scrollTop += 1;
                this.requestRender();
                return;
            case 'pageup':
                this.viewerState.scrollTop = Math.max(0, this.viewerState.scrollTop - this.workspaceViewportLines());
                this.requestRender();
                return;
            case 'pagedown':
                this.viewerState.scrollTop += this.workspaceViewportLines();
                this.requestRender();
                return;
            case 'home':
                this.viewerState.scrollTop = 0;
                this.requestRender();
                return;
            default:
                return;
        }
    }
    async handleEditorKeypress(text, key) {
        if (!this.editorState) {
            this.closeWorkspaceMode();
            return;
        }
        if (key.ctrl && key.name === 's') {
            await this.saveCurrentEditor(false);
            return;
        }
        switch (key.name) {
            case 'escape':
                if (this.editorState.dirty && !this.editorState.discardPending) {
                    this.editorState.discardPending = true;
                    this.statusLine = 'Unsaved changes. Press Esc again to discard or Ctrl+S to save.';
                    this.requestRender();
                    return;
                }
                this.closeWorkspaceMode(true);
                return;
            case 'return':
                this.splitEditorLine();
                return;
            case 'backspace':
                this.deleteEditorBackward();
                return;
            case 'delete':
                this.deleteEditorForward();
                return;
            case 'left':
                this.moveEditorCursor('left');
                return;
            case 'right':
                this.moveEditorCursor('right');
                return;
            case 'up':
                this.moveEditorCursor('up');
                return;
            case 'down':
                this.moveEditorCursor('down');
                return;
            case 'pageup':
                this.pageEditorCursor(-1);
                return;
            case 'pagedown':
                this.pageEditorCursor(1);
                return;
            case 'home':
                this.editorState.cursorColumn = 0;
                this.ensureEditorCursorVisible();
                this.requestRender();
                return;
            case 'end':
                this.editorState.cursorColumn = this.currentEditorLine().length;
                this.ensureEditorCursorVisible();
                this.requestRender();
                return;
            case 'tab':
                this.insertEditorText('  ');
                return;
            default:
                break;
        }
        if (text && !key.ctrl && !key.meta) {
            this.insertEditorText(text);
        }
    }
    async submitInput() {
        const rawInput = this.inputValue;
        const trimmedInput = rawInput.trim();
        this.inputValue = '';
        this.cursor = 0;
        if (this.pendingPrompt) {
            const pending = this.pendingPrompt;
            this.pendingPrompt = null;
            pending.resolve(trimmedInput);
            this.statusLine = pending.kind === 'clarify' ? 'Continuing...' : 'Working...';
            this.requestRender();
            return;
        }
        if (!trimmedInput) {
            this.requestRender();
            return;
        }
        const parsed = parseCommandBarInput(rawInput);
        if (this.busy && !this.pendingPrompt && !canSubmitParsedInputWhileBusy(parsed)) {
            this.statusLine = 'Execution is still running. Use /cancel or Ctrl+C to stop it.';
            this.requestRender();
            return;
        }
        if (parsed.kind === 'builtin') {
            await this.executeBuiltin(parsed);
            return;
        }
        if (parsed.kind === 'skill') {
            await this.executeSkill(parsed.name, parsed.args);
            return;
        }
        if (parsed.kind === 'shell') {
            await this.executeShellCommand(parsed.command);
            return;
        }
        await this.executeTask(parsed.prompt);
    }
    async executeBuiltin(parsed) {
        switch (parsed.name) {
            case 'cancel':
                this.cancelCurrentExecution('Cancellation requested.');
                return;
            case 'exit':
            case 'quit':
                this.close();
                return;
            case 'help': {
                const activity = this.currentProcessActivity();
                for (const line of buildHelpLines()) {
                    activity.pushLine(line);
                }
                this.statusLine = 'Help added to the process pane.';
                this.requestRender();
                return;
            }
            case 'run':
                await this.executeRunFile(parsed.args);
                return;
            case 'compile':
            case 'skill-creator':
                await this.executeSkillCreator(parsed.name, parsed.rawArgs);
                return;
            case 'set-model':
                await this.executeSetModel(parsed.rawArgs);
                return;
            case 'sessions':
                await this.refreshSessions({ createIfMissing: true, selectedSessionId: this.selectedSessionId });
                await this.refreshCommands();
                this.statusLine = `Loaded ${this.sessions.length} session${this.sessions.length === 1 ? '' : 's'}.`;
                this.requestRender();
                return;
            case 'new': {
                const created = await createConductorSession(this.workspaceRoot, parsed.rawArgs || undefined);
                await this.refreshSessions({ createIfMissing: true, selectedSessionId: created.id });
                this.statusLine = `Created session ${created.title}.`;
                this.requestRender();
                return;
            }
        }
    }
    async executeTask(promptText) {
        const sessionId = this.selectedSessionId;
        const activity = this.activityFor(sessionId);
        this.lastSubmittedInput = { kind: 'text', prompt: promptText };
        this.focusedPane = 'messages';
        this.paneScroll.messages = 0;
        this.paneScroll.process = 0;
        this.beginExecutionPreview(sessionId, 'task', { role: 'user', content: promptText });
        activity.pushLine(`task ${promptText}`);
        this.busy = true;
        this.currentAbortController = new AbortController();
        this.toolAbortController = new AbortController();
        this.toolAbortSignalRef.signal = this.toolAbortController.signal;
        this.startAnimation();
        this.statusLine = 'Running conductor...';
        this.requestRender();
        try {
            const result = await runConductorTurn(promptText, {
                workspaceRoot: this.workspaceRoot,
                sessionId,
                stream: true,
                headless: true,
                sandbox: this.options.sandbox,
                signal: this.currentAbortController.signal,
                toolAbortSignalRef: this.toolAbortSignalRef,
                onUserInput: (question) => this.requestClarification(question),
                onEvent: (event) => {
                    this.handleLiveUsageEvent(event);
                    this.manageToolAbortSignal(event.event);
                    activity.handle(event);
                    this.taskTrackerFor(sessionId).handle(event);
                    this.updatePreviewFromEvent(event);
                    this.requestRender();
                },
            });
            activity.finish();
            this.completeCurrentPreview({
                finalText: result.answer,
                error: result.error,
            });
            await this.refreshSessions({ createIfMissing: true, selectedSessionId: result.sessionId });
            await this.refreshCommands();
            this.statusLine = result.error ? 'Task failed.' : 'Task finished.';
        }
        catch (error) {
            activity.finish();
            const message = error.message;
            activity.pushLine(`error ${message}`);
            this.finishExecutionPreview({ persist: true, error: message });
            this.statusLine = 'Task failed.';
        }
        finally {
            this.busy = false;
            this.currentAbortController = null;
            this.toolAbortController = null;
            this.toolAbortSignalRef.signal = undefined;
            this.stopAnimation();
            if (this.exitRequested) {
                this.close();
            }
            else {
                this.requestRender();
            }
        }
    }
    async executeSkill(skillName, args) {
        const sessionId = this.selectedSessionId || 'global';
        const activity = this.activityFor(sessionId);
        const commandText = `/${skillName}${args.length > 0 ? ` ${args.join(' ')}` : ''}`;
        this.lastSubmittedInput = { kind: 'skill', name: skillName, rawArgs: args.join(' '), args: [...args] };
        this.focusedPane = 'messages';
        this.paneScroll.messages = 0;
        this.paneScroll.process = 0;
        this.beginExecutionPreview(sessionId, 'skill', { role: 'system', content: commandText }, skillName);
        activity.pushLine(`skill /${skillName}${args.length > 0 ? ` ${args.join(' ')}` : ''}`);
        this.busy = true;
        this.currentAbortController = new AbortController();
        this.toolAbortController = new AbortController();
        this.toolAbortSignalRef.signal = this.toolAbortController.signal;
        this.startAnimation();
        this.statusLine = `Running /${skillName}...`;
        this.requestRender();
        try {
            const result = await runSlashCommand(this.workspaceRoot, skillName, args, {
                sessionId: this.selectedSessionId || undefined,
                sandbox: this.options.sandbox,
                signal: this.currentAbortController.signal,
                toolAbortSignalRef: this.toolAbortSignalRef,
                onUserInput: (question) => this.requestClarification(question),
                onEvent: (event) => {
                    this.handleLiveUsageEvent(event);
                    this.manageToolAbortSignal(event.event);
                    activity.handle(event);
                    this.taskTrackerFor(sessionId).handle(event);
                    this.updatePreviewFromEvent(event);
                    this.requestRender();
                },
            });
            activity.finish();
            await this.refreshSessions({ selectedSessionId: this.selectedSessionId });
            await this.refreshCommands();
            this.finishExecutionPreview({
                persist: true,
                finalText: result.answer || summarizeRunResult(result),
                error: result.error,
            });
            this.statusLine = result.error ? `/${skillName} failed.` : `/${skillName} finished.`;
        }
        catch (error) {
            activity.finish();
            const message = error.message;
            activity.pushLine(`[${skillName}] error ${message}`);
            this.finishExecutionPreview({ persist: true, error: message });
            this.statusLine = `/${skillName} failed.`;
        }
        finally {
            this.busy = false;
            this.currentAbortController = null;
            this.toolAbortController = null;
            this.toolAbortSignalRef.signal = undefined;
            this.stopAnimation();
            if (this.exitRequested) {
                this.close();
            }
            else {
                this.requestRender();
            }
        }
    }
    async executeRunFile(args) {
        if (args.length === 0) {
            this.statusLine = 'Provide a DML file after /run.';
            this.requestRender();
            return;
        }
        const [fileSpec, ...runArgs] = args;
        const sessionId = this.selectedSessionId || 'global';
        const activity = this.activityFor(sessionId);
        const commandText = `/run ${args.join(' ')}`;
        const previewTag = path.basename(fileSpec, path.extname(fileSpec)) || 'run';
        this.lastSubmittedInput = { kind: 'builtin', name: 'run', rawArgs: args.join(' '), args: [...args] };
        this.focusedPane = 'messages';
        this.paneScroll.messages = 0;
        this.paneScroll.process = 0;
        this.beginExecutionPreview(sessionId, 'skill', { role: 'system', content: commandText }, previewTag);
        activity.pushLine(`run ${commandText}`);
        this.busy = true;
        this.currentAbortController = new AbortController();
        this.toolAbortController = new AbortController();
        this.toolAbortSignalRef.signal = this.toolAbortController.signal;
        this.startAnimation();
        this.statusLine = `Running ${commandText}...`;
        this.requestRender();
        try {
            const result = await runFileCommand(this.workspaceRoot, fileSpec, runArgs, {
                sessionId: this.selectedSessionId || undefined,
                sandbox: this.options.sandbox,
                signal: this.currentAbortController.signal,
                toolAbortSignalRef: this.toolAbortSignalRef,
                onUserInput: (question) => this.requestClarification(question),
                onEvent: (event) => {
                    this.handleLiveUsageEvent(event);
                    this.manageToolAbortSignal(event.event);
                    activity.handle(event);
                    this.taskTrackerFor(sessionId).handle(event);
                    this.updatePreviewFromEvent(event);
                    this.requestRender();
                },
            });
            activity.finish();
            await this.refreshSessions({ selectedSessionId: this.selectedSessionId });
            await this.refreshCommands();
            this.finishExecutionPreview({
                persist: true,
                finalText: result.answer || summarizeRunResult(result),
                error: result.error,
            });
            this.statusLine = result.error ? `${commandText} failed.` : `${commandText} finished.`;
        }
        catch (error) {
            activity.finish();
            const message = error.message;
            activity.pushLine(`[run] error ${message}`);
            this.finishExecutionPreview({ persist: true, error: message });
            this.statusLine = `${commandText} failed.`;
        }
        finally {
            this.busy = false;
            this.currentAbortController = null;
            this.toolAbortController = null;
            this.toolAbortSignalRef.signal = undefined;
            this.stopAnimation();
            if (this.exitRequested) {
                this.close();
            }
            else {
                this.requestRender();
            }
        }
    }
    async executeSkillCreator(commandName, specText) {
        const trimmedSpec = specText.trim();
        if (!trimmedSpec) {
            this.statusLine = `Provide a skill specification after /${commandName}.`;
            this.requestRender();
            return;
        }
        const sessionId = this.selectedSessionId;
        const activity = this.activityFor(sessionId);
        const commandText = `/${commandName} ${trimmedSpec}`;
        this.lastSubmittedInput = { kind: 'builtin', name: commandName, rawArgs: trimmedSpec, args: [trimmedSpec] };
        this.focusedPane = 'messages';
        this.paneScroll.messages = 0;
        this.paneScroll.process = 0;
        this.beginExecutionPreview(sessionId, 'skill', { role: 'system', content: commandText }, 'skill-creator');
        activity.pushLine(`skill ${commandText}`);
        this.busy = true;
        this.currentAbortController = new AbortController();
        this.toolAbortController = new AbortController();
        this.toolAbortSignalRef.signal = this.toolAbortController.signal;
        this.startAnimation();
        this.statusLine = `Running /${commandName}...`;
        this.requestRender();
        try {
            const result = await runSkillCreatorCommand(this.workspaceRoot, sessionId, trimmedSpec, {
                sandbox: this.options.sandbox,
                signal: this.currentAbortController.signal,
                onUserInput: (question) => this.requestClarification(question),
                onEvent: (event) => {
                    this.handleLiveUsageEvent(event);
                    this.manageToolAbortSignal(event.event);
                    activity.handle(event);
                    this.taskTrackerFor(sessionId).handle(event);
                    this.updatePreviewFromEvent(event);
                    this.requestRender();
                },
            });
            activity.finish();
            await this.refreshSessions({ selectedSessionId: this.selectedSessionId });
            await this.refreshCommands();
            this.finishExecutionPreview({
                persist: true,
                finalText: formatSkillCreatorSummary(this.workspaceRoot, result),
            });
            this.statusLine = `/${commandName} finished.`;
        }
        catch (error) {
            activity.finish();
            const message = error.message;
            activity.pushLine(`[skill-creator] error ${message}`);
            this.finishExecutionPreview({ persist: true, error: message });
            this.statusLine = `/${commandName} failed.`;
        }
        finally {
            this.busy = false;
            this.currentAbortController = null;
            this.toolAbortController = null;
            this.toolAbortSignalRef.signal = undefined;
            this.stopAnimation();
            if (this.exitRequested) {
                this.close();
            }
            else {
                this.requestRender();
            }
        }
    }
    async executeShellCommand(commandText, persistOutput = false) {
        const trimmedCommand = commandText.trim();
        if (!trimmedCommand) {
            this.statusLine = 'Enter a shell command after ! or !!.';
            this.requestRender();
            return;
        }
        const sessionId = this.selectedSessionId || 'global';
        const activity = this.activityFor(sessionId);
        this.lastSubmittedInput = { kind: 'shell', command: trimmedCommand, persistOutput };
        this.focusedPane = 'messages';
        this.paneScroll.messages = 0;
        this.paneScroll.process = 0;
        this.beginExecutionPreview(sessionId, 'shell', {
            role: 'system',
            kind: 'output',
            tag: 'shell',
            content: `${persistOutput ? '!!' : '!'}${trimmedCommand}`,
        }, 'shell');
        activity.pushLine(`shell ${persistOutput ? '!!' : '!'}${trimmedCommand}`);
        this.busy = true;
        this.currentAbortController = new AbortController();
        this.toolAbortController = this.currentAbortController;
        this.toolAbortSignalRef.signal = this.toolAbortController.signal;
        this.startAnimation();
        this.statusLine = `Running ${persistOutput ? '!!' : '!'}${trimmedCommand}...`;
        this.requestRender();
        let persistenceStarted = false;
        try {
            const result = await runShellCommand(this.workspaceRoot, trimmedCommand, {
                sandbox: this.options.sandbox,
                signal: this.currentAbortController.signal,
                onEvent: (event) => {
                    this.handleLiveUsageEvent(event);
                    activity.handle(event);
                    this.taskTrackerFor(sessionId).handle(event);
                    this.requestRender();
                },
            });
            activity.finish();
            await this.refreshCommands();
            if (persistOutput) {
                persistenceStarted = true;
                const persistedSessionId = await this.persistShellCommandResult(trimmedCommand, result);
                await this.refreshSessions({ createIfMissing: true, selectedSessionId: persistedSessionId });
                this.finishExecutionPreview({ persist: false });
                this.statusLine = result.success
                    ? 'Shell command finished and was added to the session.'
                    : 'Shell command failed and its output was added to the session.';
            }
            else {
                this.finishExecutionPreview(result.success
                    ? { persist: true, finalText: formatShellCommandSummary(result) }
                    : { persist: true, error: formatShellCommandSummary(result) });
                this.statusLine = result.success ? 'Shell command finished.' : 'Shell command failed.';
            }
        }
        catch (error) {
            activity.finish();
            const message = error.message;
            this.finishExecutionPreview({ persist: true, error: message });
            this.statusLine = isAbortError(error)
                ? 'Shell command cancelled.'
                : persistOutput && persistenceStarted
                    ? 'Shell command finished, but saving its transcript failed.'
                    : 'Shell command failed.';
        }
        finally {
            this.busy = false;
            this.currentAbortController = null;
            this.toolAbortController = null;
            this.toolAbortSignalRef.signal = undefined;
            this.stopAnimation();
            if (this.exitRequested) {
                this.close();
            }
            else {
                this.requestRender();
            }
        }
    }
    async executeSetModel(rawArgs) {
        try {
            const { model, slot } = parseSetModelCommandArgs(rawArgs);
            const result = await setModel(this.workspaceRoot, model, slot);
            const slotsLabel = result.updatedSlots.join(', ');
            this.currentProcessActivity().pushLine(`config model ${result.modelId} slots=${slotsLabel}`);
            this.statusLine = `Model set to ${result.modelId} for ${slotsLabel}. Applies on the next turn.`;
        }
        catch (error) {
            this.statusLine = error.message;
        }
        this.requestRender();
    }
    async requestClarification(promptText) {
        this.ensureClarificationVisible(promptText);
        return this.promptForValue(promptText, 'clarify');
    }
    async promptForValue(promptText, kind = 'input') {
        this.inputValue = '';
        this.cursor = 0;
        this.statusLine = kind === 'clarify' ? 'Awaiting clarification...' : promptText;
        this.requestRender();
        return new Promise((resolve) => {
            this.pendingPrompt = { kind, prompt: promptText, resolve };
            this.requestRender();
        });
    }
    ensureClarificationVisible(promptText) {
        if (!this.currentPreview) {
            return;
        }
        const entry = previewQuestionMessage(promptText);
        const entries = this.currentPreview.entries;
        const pendingAssistantIndex = entries.length > 0 && entries[entries.length - 1].role === 'assistant'
            ? entries.length - 1
            : entries.length;
        const previous = pendingAssistantIndex > 0 ? entries[pendingAssistantIndex - 1] : undefined;
        if (sameDisplayMessage(previous, entry)) {
            return;
        }
        this.insertPreviewMessage(entry);
    }
    async applyCompletion() {
        if (!this.inputValue.startsWith('/')) {
            return;
        }
        await this.refreshCommands();
        const completion = completeSlashCommand(this.inputValue, [
            ...BUILTIN_SLASH_COMMANDS,
            ...this.commands.map((command) => command.name),
        ]);
        if (completion.applied) {
            this.inputValue = completion.value;
            this.cursor = this.inputValue.length;
        }
        if (completion.matches.length === 0) {
            this.statusLine = 'No matching commands or skills.';
        }
        else if (completion.matches.length === 1) {
            this.statusLine = `Completed /${completion.matches[0]}`;
        }
        else {
            const preview = completion.matches.slice(0, 6).join(', ');
            this.statusLine = `Matches: ${preview}${completion.matches.length > 6 ? ', ...' : ''}`;
        }
        this.requestRender();
    }
    async handleUpDown(offset) {
        if (this.focusedPane === 'sessions') {
            await this.moveSession(offset);
            return;
        }
        this.scrollFocusedPane(offset < 0 ? 'up' : 'down', 1);
    }
    cyclePaneFocus(direction) {
        const order = ['sessions', 'messages', 'process', 'tasks', 'context'];
        const currentIndex = order.indexOf(this.focusedPane);
        const nextIndex = (currentIndex + direction + order.length) % order.length;
        this.focusedPane = order[nextIndex];
        this.statusLine = `Focus: ${paneDisplayName(this.focusedPane)}. Left/right switches panes; PgUp/PgDn scrolls.`;
        this.requestRender();
    }
    toggleMenuMode() {
        if (this.pendingPrompt) {
            this.statusLine = this.pendingPrompt.kind === 'clarify'
                ? 'Finish the clarification prompt before opening menus.'
                : 'Finish the current input prompt before opening menus.';
            this.requestRender();
            return;
        }
        if (this.uiMode === 'menu') {
            this.closeMenuMode();
            return;
        }
        this.menuReturnMode = this.uiMode;
        this.uiMode = 'menu';
        this.menuState.typeahead = '';
        this.menuState.typeaheadAt = 0;
        this.menuState.selectedIndex = 0;
        this.statusLine = `${this.getMenuDefinitions()[this.menuState.activeIndex]?.label ?? 'Menu'} menu.`;
        this.requestRender();
    }
    closeMenuMode() {
        if (this.uiMode !== 'menu') {
            return;
        }
        this.uiMode = this.menuReturnMode;
        this.menuState.typeahead = '';
        this.menuState.typeaheadAt = 0;
        this.statusLine = `Focus: ${paneDisplayName(this.focusedPane)}. F10 or Ctrl+G reopens the menu bar.`;
        this.requestRender();
    }
    moveMenu(direction) {
        const menus = this.getMenuDefinitions();
        this.menuState.activeIndex = nextWrappedIndex(this.menuState.activeIndex, direction, menus.length);
        this.menuState.selectedIndex = 0;
        this.menuState.typeahead = '';
        this.menuState.typeaheadAt = 0;
        this.statusLine = `${menus[this.menuState.activeIndex]?.label ?? 'Menu'} menu.`;
        this.requestRender();
    }
    selectMenu(index) {
        const menus = this.getMenuDefinitions();
        if (index < 0 || index >= menus.length) {
            return;
        }
        this.menuState.activeIndex = index;
        this.menuState.selectedIndex = 0;
        this.menuState.typeahead = '';
        this.menuState.typeaheadAt = 0;
        this.statusLine = `${menus[index]?.label ?? 'Menu'} menu.`;
        this.requestRender();
    }
    moveMenuItem(direction, itemCount) {
        this.menuState.selectedIndex = nextWrappedIndex(this.menuState.selectedIndex, direction, itemCount);
        this.menuState.typeahead = '';
        this.menuState.typeaheadAt = 0;
        this.requestRender();
    }
    getMenuDefinitions() {
        const busy = this.busy || !!this.pendingPrompt;
        const menus = [
            {
                id: 'session',
                label: 'Session',
                items: [
                    { id: 'session.new', label: 'New Session', description: 'Create and switch to a fresh conductor session.', shortcut: 'Enter' },
                    { id: 'session.refresh', label: 'Refresh Sessions', description: 'Reload the session list and command catalog.' },
                    { id: 'session.next', label: 'Next Session', description: 'Move to the next session in the left pane.', shortcut: 'Down' },
                    { id: 'session.previous', label: 'Previous Session', description: 'Move to the previous session in the left pane.', shortcut: 'Up' },
                    { id: 'session.quit', label: 'Quit', description: 'Leave the fullscreen TUI.', shortcut: 'Ctrl+C' },
                ],
            },
            {
                id: 'skills',
                label: 'Skills',
                items: [
                    { id: 'skills.browse', label: 'Browse All Skills', description: 'Open a searchable catalog of compiled skills.', shortcut: 'Enter' },
                    { id: 'skills.run', label: 'Run Skill…', description: 'Pick a skill and execute it from the menu bar.' },
                    { id: 'skills.inspect', label: 'Inspect Skill Code', description: 'View the compiled DML for a skill.' },
                    { id: 'skills.edit', label: 'Edit Skill Code', description: 'Open a skill in the inline editor.' },
                    { id: 'skills.meta', label: 'Inspect Skill Metadata', description: 'View the metadata JSON for a compiled skill.' },
                    { id: 'skills.source', label: 'Open Skill Source Spec', description: 'Jump from a compiled skill back to its source Markdown.' },
                ],
            },
            {
                id: 'files',
                label: 'Files',
                items: [
                    { id: 'files.open', label: 'Open Text File…', description: 'Browse workspace text files in a searchable picker.', shortcut: 'Enter' },
                    { id: 'files.recent', label: 'Recent Files', description: 'Reopen a recently inspected or edited file.' },
                    { id: 'files.newMarkdown', label: 'New Markdown Note', description: 'Create a new Markdown buffer inside the workspace.' },
                    { id: 'files.save', label: 'Save', description: 'Save the current editor buffer.', shortcut: 'Ctrl+S', enabled: !!this.editorState },
                    { id: 'files.saveAs', label: 'Save As…', description: 'Save the current editor buffer to a new path.', enabled: !!this.editorState },
                    { id: 'files.readme', label: 'Open Workspace README', description: 'Inspect the top-level README.md.' },
                    { id: 'files.config', label: 'Open .deepclause Config', description: 'Inspect the local DeepClause config file.' },
                ],
            },
            {
                id: 'run',
                label: 'Run',
                items: [
                    { id: 'run.prompt', label: 'Send Prompt…', description: 'Return to the command bar and enter a freeform prompt.', shortcut: 'Enter' },
                    { id: 'run.shell', label: 'Run Shell Command…', description: 'Prefill the command bar for a direct workspace shell command.' },
                    { id: 'run.repeat', label: 'Repeat Last Command', description: 'Re-run the last prompt, shell command, or skill invocation.', enabled: !!this.lastSubmittedInput },
                    { id: 'run.cancel', label: 'Cancel Current Execution', description: 'Abort the running task or skill.', shortcut: '/cancel', enabled: this.busy },
                    { id: 'run.clear', label: 'Clear Execution Pane', description: 'Discard the current session execution log.' },
                    { id: 'run.refreshSkills', label: 'Refresh Skill Catalog', description: 'Re-scan the workspace for compiled skills.' },
                ],
            },
            {
                id: 'view',
                label: 'View',
                items: [
                    { id: 'view.focus.sessions', label: 'Focus Sessions Pane', description: 'Focus the session list pane.' },
                    { id: 'view.focus.messages', label: 'Focus Messages Pane', description: 'Focus the conversation history pane.' },
                    { id: 'view.focus.process', label: 'Focus Execution Pane', description: 'Focus the live execution log pane.' },
                    { id: 'view.focus.tasks', label: 'Focus Tasks Pane', description: 'Focus the step tracker pane.' },
                    { id: 'view.focus.context', label: 'Focus Context Pane', description: 'Focus the session context and token usage pane.' },
                    { id: 'view.follow', label: 'Follow Focused Pane', description: 'Jump back to the newest visible content in the active pane.' },
                    { id: 'view.palette', label: 'Open Action Palette', description: 'Search all menu actions from one picker.', shortcut: 'Ctrl+P' },
                ],
            },
            {
                id: 'help',
                label: 'Help',
                items: [
                    { id: 'help.shortcuts', label: 'Keyboard Shortcuts', description: 'Show the fullscreen keyboard map.' },
                    { id: 'help.slash', label: 'Commands', description: 'Show slash commands, shell commands, and direct skill invocation.' },
                    { id: 'help.about', label: 'About This TUI', description: 'Show a short explanation of the fullscreen workspace.' },
                ],
            },
        ];
        return menus.map((menu) => ({
            ...menu,
            items: menu.items.map((item) => ({
                ...item,
                enabled: item.enabled ?? (!busy || item.id === 'run.cancel'),
            })),
        }));
    }
    async runMenuAction(actionId) {
        switch (actionId) {
            case 'session.new': {
                const created = await createConductorSession(this.workspaceRoot);
                await this.refreshSessions({ createIfMissing: true, selectedSessionId: created.id });
                this.closeMenuMode();
                this.statusLine = `Created session ${created.title}.`;
                return;
            }
            case 'session.refresh':
                await this.refreshSessions({ createIfMissing: true, selectedSessionId: this.selectedSessionId });
                await this.refreshCommands();
                this.closeMenuMode();
                this.statusLine = `Loaded ${this.sessions.length} session${this.sessions.length === 1 ? '' : 's'}.`;
                return;
            case 'session.next':
                await this.moveSession(1);
                return;
            case 'session.previous':
                await this.moveSession(-1);
                return;
            case 'session.quit':
                this.close();
                return;
            case 'skills.browse':
                await this.openSkillPicker('skills.browse');
                return;
            case 'skills.run':
                await this.openSkillPicker('skills.run');
                return;
            case 'skills.inspect':
                await this.openSkillPicker('skills.inspect');
                return;
            case 'skills.edit':
                await this.openSkillPicker('skills.edit');
                return;
            case 'skills.meta':
                await this.openSkillPicker('skills.meta');
                return;
            case 'skills.source':
                await this.openSkillPicker('skills.source');
                return;
            case 'files.open':
                await this.openFilePicker('files.inspect');
                return;
            case 'files.recent':
                await this.openFilePicker('files.recent');
                return;
            case 'files.newMarkdown':
                this.openEditorBuffer('', { title: 'New Markdown Note', preferredExtension: '.md' });
                return;
            case 'files.save':
                await this.saveCurrentEditor(false);
                return;
            case 'files.saveAs':
                await this.saveCurrentEditor(true);
                return;
            case 'files.readme':
                await this.openFileViewer(path.join(this.workspaceRoot, 'README.md'), { title: 'README.md' });
                return;
            case 'files.config':
                await this.openFileViewer(getConfigPath(this.workspaceRoot), { title: '.deepclause/config.json' });
                return;
            case 'run.prompt':
                this.closeMenuMode();
                this.statusLine = 'Command bar ready.';
                return;
            case 'run.shell':
                this.closeMenuMode();
                this.inputValue = '!';
                this.cursor = this.inputValue.length;
                this.statusLine = formatShellModeStatus(this.workspaceRoot, this.shellWorkspacePath, this.options.sandbox, this.shellConfig);
                this.requestRender();
                return;
            case 'run.repeat':
                await this.repeatLastCommand();
                return;
            case 'run.cancel':
                this.closeMenuMode();
                this.cancelCurrentExecution('Cancellation requested.');
                return;
            case 'run.clear':
                this.currentProcessActivity().clear();
                this.closeMenuMode();
                this.statusLine = 'Execution pane cleared.';
                return;
            case 'run.refreshSkills':
                await this.refreshCommands();
                this.closeMenuMode();
                this.statusLine = `Loaded ${this.commands.length} skill${this.commands.length === 1 ? '' : 's'}.`;
                return;
            case 'view.focus.sessions':
            case 'view.focus.messages':
            case 'view.focus.process':
            case 'view.focus.tasks':
            case 'view.focus.context': {
                const pane = actionId.slice('view.focus.'.length);
                this.focusedPane = pane;
                this.closeMenuMode();
                this.statusLine = `Focus: ${paneDisplayName(this.focusedPane)}.`;
                return;
            }
            case 'view.follow':
                if (this.focusedPane !== 'sessions') {
                    this.paneScroll[this.focusedPane] = 0;
                }
                this.closeMenuMode();
                this.statusLine = `${paneDisplayName(this.focusedPane)} following newest content.`;
                return;
            case 'view.palette':
                this.openActionPalette();
                return;
            case 'help.shortcuts':
                this.openTextViewer('Keyboard Shortcuts', getShortcutHelpLines().join('\n'), { preferredExtension: '.txt' });
                return;
            case 'help.slash':
                this.openTextViewer('Commands', getSlashHelpLines().join('\n'), { preferredExtension: '.txt' });
                return;
            case 'help.about':
                this.openTextViewer('About This TUI', [
                    'DeepClause fullscreen TUI',
                    '',
                    'Use F10 or Ctrl+G for the menu bar, Ctrl+W to cycle panes, and /cancel to stop a running execution.',
                    'Type !<command> to run a direct shell command in the configured workspace shell.',
                    'Type !!<command> to run a shell command and append its transcript to the active session.',
                    'The Skills and Files menus open searchable pickers, and Ctrl+P opens the action palette directly.',
                ].join('\n'), { preferredExtension: '.txt' });
                return;
            default:
                this.closeMenuMode();
                this.statusLine = 'This menu action is being wired up next.';
                return;
        }
    }
    closeWorkspaceMode(force = false) {
        if (!force && !this.canReplaceWorkspaceSurface()) {
            return false;
        }
        this.uiMode = 'command';
        this.pickerState = null;
        this.viewerState = null;
        this.editorState = null;
        this.inputValue = '';
        this.cursor = 0;
        this.requestRender();
        return true;
    }
    canReplaceWorkspaceSurface() {
        if (this.editorState?.dirty) {
            this.statusLine = 'Save the current editor with Ctrl+S or press Esc again in the editor to discard changes.';
            this.requestRender();
            return false;
        }
        return true;
    }
    workspaceViewportLines() {
        return Math.max(1, (output.rows ?? 40) - 5);
    }
    openActionPalette() {
        if (!this.canReplaceWorkspaceSurface()) {
            return;
        }
        const items = this.getMenuDefinitions()
            .flatMap((menu) => menu.items)
            .filter((item) => item.enabled !== false)
            .map((item) => ({
            id: item.id,
            label: item.label,
            description: item.description,
            detail: item.shortcut,
            actionId: item.id,
        }));
        this.openPicker({
            mode: 'palette',
            title: 'Action Palette',
            emptyText: 'No actions match the current query.',
            items,
            filteredItems: items,
            selectedIndex: 0,
            scrollTop: 0,
        });
    }
    async openSkillPicker(mode) {
        if (!this.canReplaceWorkspaceSurface()) {
            return;
        }
        await this.refreshCommands();
        const items = await Promise.all(this.commands.map(async (command) => {
            const skillPath = this.resolveCommandDmlPath(command);
            const metaPath = this.resolveCommandMetaPath(command);
            let sourcePath;
            try {
                const meta = JSON.parse(await fs.readFile(metaPath, 'utf8'));
                if (meta.source) {
                    sourcePath = path.resolve(this.workspaceRoot, meta.source);
                }
            }
            catch {
                // Metadata is optional while browsing.
            }
            return {
                id: command.name,
                label: command.name,
                description: command.description,
                detail: command.path,
                path: skillPath,
                skillName: command.name,
                metaPath,
                sourcePath,
            };
        }));
        const titles = {
            'skills.browse': 'Browse Skills',
            'skills.run': 'Run Skill',
            'skills.inspect': 'Inspect Skill Code',
            'skills.edit': 'Edit Skill Code',
            'skills.meta': 'Inspect Skill Metadata',
            'skills.source': 'Open Skill Source Spec',
        };
        this.openPicker({
            mode,
            title: titles[mode],
            emptyText: 'No skills match the current query.',
            items,
            filteredItems: items,
            selectedIndex: 0,
            scrollTop: 0,
        });
    }
    async openFilePicker(mode) {
        if (!this.canReplaceWorkspaceSurface()) {
            return;
        }
        const relativePaths = mode === 'files.recent'
            ? [...this.recentFiles]
            : await listWorkspaceTextFiles(this.workspaceRoot);
        const items = relativePaths.map((relativePath) => ({
            id: relativePath,
            label: path.basename(relativePath),
            description: relativePath,
            detail: path.extname(relativePath) || 'text',
            path: path.join(this.workspaceRoot, relativePath),
        }));
        const titles = {
            'files.inspect': 'Open Text File',
            'files.edit': 'Edit Text File',
            'files.recent': 'Recent Files',
        };
        this.openPicker({
            mode,
            title: titles[mode],
            emptyText: mode === 'files.recent'
                ? 'No recent files yet. Open or edit a file first.'
                : 'No text files match the current query.',
            items,
            filteredItems: items,
            selectedIndex: 0,
            scrollTop: 0,
        });
    }
    openPicker(state) {
        this.closeMenuMode();
        this.uiMode = state.mode === 'palette' ? 'palette' : 'picker';
        this.pickerState = state;
        this.viewerState = null;
        this.editorState = null;
        this.inputValue = '';
        this.cursor = 0;
        this.ensurePickerSelectionVisible();
        this.statusLine = `${state.title}.`;
        this.requestRender();
    }
    refreshPickerFilter() {
        if (!this.pickerState) {
            return;
        }
        this.pickerState.filteredItems = filterPickerItems(this.pickerState.items, this.inputValue);
        this.pickerState.selectedIndex = clamp(this.pickerState.selectedIndex, 0, Math.max(0, this.pickerState.filteredItems.length - 1));
        this.pickerState.scrollTop = 0;
        this.ensurePickerSelectionVisible();
        this.requestRender();
    }
    movePickerSelection(delta) {
        if (!this.pickerState || this.pickerState.filteredItems.length === 0) {
            return;
        }
        this.pickerState.selectedIndex = clamp(this.pickerState.selectedIndex + delta, 0, Math.max(0, this.pickerState.filteredItems.length - 1));
        this.ensurePickerSelectionVisible();
        this.requestRender();
    }
    ensurePickerSelectionVisible() {
        if (!this.pickerState) {
            return;
        }
        const targetLine = 3 + (this.pickerState.selectedIndex * 3);
        const viewport = this.workspaceViewportLines();
        if (targetLine < this.pickerState.scrollTop) {
            this.pickerState.scrollTop = targetLine;
            return;
        }
        if (targetLine >= this.pickerState.scrollTop + viewport) {
            this.pickerState.scrollTop = Math.max(0, targetLine - viewport + 1);
        }
    }
    async activatePickerSelection(override) {
        const picker = this.pickerState;
        const item = picker?.filteredItems[picker.selectedIndex];
        if (!picker || !item) {
            return;
        }
        if (picker.mode === 'palette') {
            await this.runMenuAction(item.actionId ?? 'help.about');
            return;
        }
        const action = override ?? picker.mode;
        switch (action) {
            case 'skills.browse':
            case 'skills.inspect':
                await this.openSkillViewer(item, 'dml');
                return;
            case 'skills.run': {
                this.closeWorkspaceMode();
                const rawArgs = await this.promptForValue(`Arguments for /${item.skillName} (optional):`);
                const skillName = item.skillName ?? item.label;
                const commands = await listCommands(this.workspaceRoot, { detailed: true });
                const command = commands.find((entry) => entry.name === skillName);
                const collapsedArgs = command?.parameters?.length === 1 && rawArgs.length > 0
                    ? [rawArgs]
                    : parseCommandArgs(rawArgs);
                await this.executeSkill(skillName, collapsedArgs);
                return;
            }
            case 'run':
                if (item.skillName) {
                    this.closeWorkspaceMode();
                    const rawArgs = await this.promptForValue(`Arguments for /${item.skillName} (optional):`);
                    const commands = await listCommands(this.workspaceRoot, { detailed: true });
                    const command = commands.find((entry) => entry.name === item.skillName);
                    const collapsedArgs = command?.parameters?.length === 1 && rawArgs.length > 0
                        ? [rawArgs]
                        : parseCommandArgs(rawArgs);
                    await this.executeSkill(item.skillName, collapsedArgs);
                }
                return;
            case 'edit':
            case 'skills.edit':
            case 'files.edit':
                if (item.path) {
                    await this.openEditorForFile(item.path, { title: item.description || item.label });
                }
                return;
            case 'meta':
            case 'skills.meta':
                if (item.metaPath) {
                    await this.openSkillViewer(item, 'meta');
                }
                return;
            case 'source':
            case 'skills.source':
                if (item.sourcePath) {
                    await this.openSkillViewer(item, 'source');
                }
                else {
                    this.statusLine = `No source spec recorded for ${item.label}.`;
                    this.requestRender();
                }
                return;
            case 'files.inspect':
            case 'files.recent':
                if (item.path) {
                    await this.openFileViewer(item.path, { title: item.description || item.label });
                }
                return;
        }
    }
    async openSkillViewer(item, target) {
        const filePath = target === 'dml' ? item.path : target === 'meta' ? item.metaPath : item.sourcePath;
        if (!filePath) {
            this.statusLine = `No ${target} file available for ${item.label}.`;
            this.requestRender();
            return;
        }
        const title = target === 'dml'
            ? `Skill: ${item.label}`
            : target === 'meta'
                ? `Metadata: ${item.label}`
                : `Source Spec: ${item.label}`;
        await this.openFileViewer(filePath, {
            title,
            skillName: item.skillName,
            canEdit: target !== 'meta',
            preferredExtension: target === 'meta' ? '.json' : path.extname(filePath) || '.txt',
        });
    }
    openTextViewer(title, content, options = {}) {
        if (!this.canReplaceWorkspaceSurface()) {
            return;
        }
        this.closeMenuMode();
        this.uiMode = 'viewer';
        this.pickerState = null;
        this.editorState = null;
        this.viewerState = {
            title,
            content,
            path: options.path,
            canEdit: options.canEdit ?? false,
            scrollTop: 0,
            skillName: options.skillName,
            metaPath: options.metaPath,
            sourcePath: options.sourcePath,
            preferredExtension: options.preferredExtension,
        };
        this.inputValue = '';
        this.cursor = 0;
        if (options.path) {
            this.pushRecentFile(options.path);
        }
        this.statusLine = `Viewing ${title}.`;
        this.requestRender();
    }
    async openFileViewer(filePath, options = {}) {
        try {
            const content = await fs.readFile(filePath, 'utf8');
            this.openTextViewer(options.title ?? path.relative(this.workspaceRoot, filePath), content, {
                path: filePath,
                canEdit: options.canEdit ?? isEditableTextFile(filePath),
                skillName: options.skillName,
                preferredExtension: options.preferredExtension ?? (path.extname(filePath) || '.txt'),
            });
        }
        catch (error) {
            this.closeMenuMode();
            this.statusLine = `Unable to open ${path.relative(this.workspaceRoot, filePath)}: ${error.message}`;
            this.requestRender();
        }
    }
    async openEditorForFile(filePath, options = {}) {
        try {
            const content = await fs.readFile(filePath, 'utf8');
            this.openEditorBuffer(content, {
                title: options.title ?? path.relative(this.workspaceRoot, filePath),
                path: filePath,
                preferredExtension: options.preferredExtension ?? (path.extname(filePath) || '.txt'),
            });
        }
        catch (error) {
            this.statusLine = `Unable to edit ${path.relative(this.workspaceRoot, filePath)}: ${error.message}`;
            this.requestRender();
        }
    }
    openEditorBuffer(content, options) {
        if (!this.canReplaceWorkspaceSurface()) {
            return;
        }
        this.closeMenuMode();
        this.uiMode = 'editor';
        this.pickerState = null;
        this.viewerState = null;
        this.editorState = {
            title: options.title,
            path: options.path,
            lines: splitEditorContent(content),
            cursorLine: 0,
            cursorColumn: 0,
            scrollTop: 0,
            scrollLeft: 0,
            dirty: false,
            preferredExtension: options.preferredExtension ?? '.txt',
            discardPending: false,
        };
        this.inputValue = '';
        this.cursor = 0;
        if (options.path) {
            this.pushRecentFile(options.path);
        }
        this.ensureEditorCursorVisible();
        this.statusLine = `Editing ${options.title}.`;
        this.requestRender();
    }
    currentEditorLine() {
        return this.editorState?.lines[this.editorState.cursorLine] ?? '';
    }
    updateCurrentEditorLine(nextLine) {
        if (!this.editorState) {
            return;
        }
        this.editorState.lines[this.editorState.cursorLine] = nextLine;
        this.editorState.dirty = true;
        this.editorState.discardPending = false;
    }
    insertEditorText(text) {
        if (!this.editorState) {
            return;
        }
        const line = this.currentEditorLine();
        this.updateCurrentEditorLine(`${line.slice(0, this.editorState.cursorColumn)}${text}${line.slice(this.editorState.cursorColumn)}`);
        this.editorState.cursorColumn += text.length;
        this.ensureEditorCursorVisible();
        this.requestRender();
    }
    splitEditorLine() {
        if (!this.editorState) {
            return;
        }
        const line = this.currentEditorLine();
        const before = line.slice(0, this.editorState.cursorColumn);
        const after = line.slice(this.editorState.cursorColumn);
        this.editorState.lines.splice(this.editorState.cursorLine, 1, before, after);
        this.editorState.cursorLine += 1;
        this.editorState.cursorColumn = 0;
        this.editorState.dirty = true;
        this.editorState.discardPending = false;
        this.ensureEditorCursorVisible();
        this.requestRender();
    }
    deleteEditorBackward() {
        if (!this.editorState) {
            return;
        }
        if (this.editorState.cursorColumn > 0) {
            const line = this.currentEditorLine();
            this.updateCurrentEditorLine(`${line.slice(0, this.editorState.cursorColumn - 1)}${line.slice(this.editorState.cursorColumn)}`);
            this.editorState.cursorColumn -= 1;
        }
        else if (this.editorState.cursorLine > 0) {
            const previous = this.editorState.lines[this.editorState.cursorLine - 1] ?? '';
            const current = this.currentEditorLine();
            this.editorState.lines.splice(this.editorState.cursorLine - 1, 2, `${previous}${current}`);
            this.editorState.cursorLine -= 1;
            this.editorState.cursorColumn = previous.length;
            this.editorState.dirty = true;
            this.editorState.discardPending = false;
        }
        this.ensureEditorCursorVisible();
        this.requestRender();
    }
    deleteEditorForward() {
        if (!this.editorState) {
            return;
        }
        const line = this.currentEditorLine();
        if (this.editorState.cursorColumn < line.length) {
            this.updateCurrentEditorLine(`${line.slice(0, this.editorState.cursorColumn)}${line.slice(this.editorState.cursorColumn + 1)}`);
        }
        else if (this.editorState.cursorLine < this.editorState.lines.length - 1) {
            const next = this.editorState.lines[this.editorState.cursorLine + 1] ?? '';
            this.editorState.lines.splice(this.editorState.cursorLine, 2, `${line}${next}`);
            this.editorState.dirty = true;
            this.editorState.discardPending = false;
        }
        this.ensureEditorCursorVisible();
        this.requestRender();
    }
    moveEditorCursor(direction) {
        if (!this.editorState) {
            return;
        }
        switch (direction) {
            case 'left':
                if (this.editorState.cursorColumn > 0) {
                    this.editorState.cursorColumn -= 1;
                }
                else if (this.editorState.cursorLine > 0) {
                    this.editorState.cursorLine -= 1;
                    this.editorState.cursorColumn = this.currentEditorLine().length;
                }
                break;
            case 'right':
                if (this.editorState.cursorColumn < this.currentEditorLine().length) {
                    this.editorState.cursorColumn += 1;
                }
                else if (this.editorState.cursorLine < this.editorState.lines.length - 1) {
                    this.editorState.cursorLine += 1;
                    this.editorState.cursorColumn = 0;
                }
                break;
            case 'up':
                if (this.editorState.cursorLine > 0) {
                    this.editorState.cursorLine -= 1;
                    this.editorState.cursorColumn = Math.min(this.editorState.cursorColumn, this.currentEditorLine().length);
                }
                break;
            case 'down':
                if (this.editorState.cursorLine < this.editorState.lines.length - 1) {
                    this.editorState.cursorLine += 1;
                    this.editorState.cursorColumn = Math.min(this.editorState.cursorColumn, this.currentEditorLine().length);
                }
                break;
        }
        this.ensureEditorCursorVisible();
        this.requestRender();
    }
    pageEditorCursor(direction) {
        if (!this.editorState) {
            return;
        }
        const nextLine = clamp(this.editorState.cursorLine + (direction * this.workspaceViewportLines()), 0, Math.max(0, this.editorState.lines.length - 1));
        this.editorState.cursorLine = nextLine;
        this.editorState.cursorColumn = Math.min(this.editorState.cursorColumn, this.currentEditorLine().length);
        this.ensureEditorCursorVisible();
        this.requestRender();
    }
    ensureEditorCursorVisible() {
        if (!this.editorState) {
            return;
        }
        const viewportLines = this.workspaceViewportLines();
        if (this.editorState.cursorLine < this.editorState.scrollTop) {
            this.editorState.scrollTop = this.editorState.cursorLine;
        }
        else if (this.editorState.cursorLine >= this.editorState.scrollTop + viewportLines) {
            this.editorState.scrollTop = Math.max(0, this.editorState.cursorLine - viewportLines + 1);
        }
        const width = output.columns ?? 120;
        const innerWidth = Math.max(1, width - 2);
        const lineNumberWidth = Math.max(2, String(Math.max(1, this.editorState.lines.length)).length);
        const prefixWidth = 2 + lineNumberWidth + 3;
        const contentWidth = Math.max(1, innerWidth - prefixWidth);
        if (this.editorState.cursorColumn < this.editorState.scrollLeft) {
            this.editorState.scrollLeft = this.editorState.cursorColumn;
        }
        else if (this.editorState.cursorColumn >= this.editorState.scrollLeft + contentWidth) {
            this.editorState.scrollLeft = Math.max(0, this.editorState.cursorColumn - contentWidth + 1);
        }
    }
    async saveCurrentEditor(saveAs) {
        if (!this.editorState) {
            this.closeMenuMode();
            this.statusLine = 'No editor buffer is active.';
            this.requestRender();
            return;
        }
        let targetPath = this.editorState.path;
        if (saveAs || !targetPath) {
            const suggested = this.editorState.path
                ? path.relative(this.workspaceRoot, this.editorState.path)
                : `notes/${Date.now()}.${this.editorState.preferredExtension.replace(/^\./, '')}`;
            const requestedPath = (await this.promptForValue(`Save file as (relative path) [${suggested}]:`)).trim() || suggested;
            targetPath = resolveWorkspaceTextPath(this.workspaceRoot, requestedPath);
        }
        try {
            await fs.mkdir(path.dirname(targetPath), { recursive: true });
            await fs.writeFile(targetPath, this.editorState.lines.join('\n'), 'utf8');
            this.editorState.path = targetPath;
            this.editorState.title = path.relative(this.workspaceRoot, targetPath);
            this.editorState.dirty = false;
            this.editorState.discardPending = false;
            this.pushRecentFile(targetPath);
            this.statusLine = `Saved ${path.relative(this.workspaceRoot, targetPath)}.`;
            this.requestRender();
        }
        catch (error) {
            this.statusLine = `Unable to save file: ${error.message}`;
            this.requestRender();
        }
    }
    pushRecentFile(filePath) {
        const relativePath = path.relative(this.workspaceRoot, filePath).replace(/\\/g, '/');
        const existingIndex = this.recentFiles.indexOf(relativePath);
        if (existingIndex >= 0) {
            this.recentFiles.splice(existingIndex, 1);
        }
        this.recentFiles.unshift(relativePath);
        if (this.recentFiles.length > 20) {
            this.recentFiles.length = 20;
        }
    }
    async repeatLastCommand() {
        if (!this.lastSubmittedInput) {
            this.closeMenuMode();
            this.statusLine = 'No previous prompt, shell command, or skill to repeat yet.';
            this.requestRender();
            return;
        }
        if (!this.closeWorkspaceMode()) {
            return;
        }
        if (this.lastSubmittedInput.kind === 'text') {
            await this.executeTask(this.lastSubmittedInput.prompt);
            return;
        }
        if (this.lastSubmittedInput.kind === 'skill') {
            await this.executeSkill(this.lastSubmittedInput.name, this.lastSubmittedInput.args);
            return;
        }
        if (this.lastSubmittedInput.kind === 'shell') {
            await this.executeShellCommand(this.lastSubmittedInput.command, this.lastSubmittedInput.persistOutput);
            return;
        }
        if (this.lastSubmittedInput.kind === 'builtin') {
            await this.executeBuiltin(this.lastSubmittedInput);
        }
    }
    resolveCommandDmlPath(command) {
        return path.join(this.workspaceRoot, `${command.path}.dml`);
    }
    resolveCommandMetaPath(command) {
        return path.join(this.workspaceRoot, `${command.path}.meta.json`);
    }
    beginExecutionPreview(sessionId, kind, lead, rootTag) {
        this.currentPreview = {
            sessionId,
            kind,
            rootTag,
            activeChildTag: kind === 'skill' ? rootTag : undefined,
            entries: kind === 'shell'
                ? [lead]
                : [lead, { role: 'assistant', content: '', pending: true }],
        };
    }
    handleLiveUsageEvent(logEvent) {
        if (logEvent.event.type === 'usage' && logEvent.event.usage) {
            recordTokenUsage(this.liveUsageByModel, logEvent.modelId, logEvent.event.usage);
        }
    }
    updatePreviewFromEvent(logEvent) {
        if (!this.currentPreview) {
            return;
        }
        this.updatePreviewChildState(logEvent);
        const previewMessage = previewMessageFromEvent(logEvent);
        if (previewMessage) {
            this.insertPreviewMessage(previewMessage);
        }
        if (!this.isPrimaryPreviewEvent(logEvent)) {
            return;
        }
        const message = this.ensurePreviewAssistantMessage();
        switch (logEvent.event.type) {
            case 'stream':
                if (logEvent.event.content) {
                    message.content += logEvent.event.content;
                }
                break;
            case 'answer':
                if (logEvent.event.content) {
                    message.content = logEvent.event.content;
                }
                message.pending = false;
                message.error = false;
                break;
            case 'error':
                message.content = logEvent.event.content ?? (message.content || 'Execution failed');
                message.pending = false;
                message.error = true;
                break;
            default:
                break;
        }
    }
    isPrimaryPreviewEvent(logEvent) {
        if (!this.currentPreview) {
            return false;
        }
        if (this.currentPreview.kind === 'task') {
            return logEvent.scope === 'main';
        }
        if (this.currentPreview.kind === 'shell') {
            return false;
        }
        return logEvent.scope === 'child' && logEvent.childSlug === this.currentPreview.rootTag;
    }
    insertPreviewMessage(entry) {
        if (!this.currentPreview) {
            return;
        }
        const entries = this.currentPreview.entries;
        const last = entries[entries.length - 1];
        if (last && last.role === 'assistant' && last.pending && !last.content) {
            entries.splice(entries.length - 1, 0, entry);
            return;
        }
        entries.push(entry);
    }
    updatePreviewChildState(logEvent) {
        if (!this.currentPreview || this.currentPreview.kind !== 'task') {
            return;
        }
        if (logEvent.scope === 'child' && logEvent.childSlug) {
            this.currentPreview.activeChildTag = logEvent.childSlug;
            this.ensurePreviewChildSkillMessage(logEvent.childSlug);
            return;
        }
        if (logEvent.scope === 'main' && (logEvent.event.type === 'stream' || logEvent.event.type === 'answer' || logEvent.event.type === 'error')) {
            this.currentPreview.activeChildTag = undefined;
        }
    }
    ensurePreviewChildSkillMessage(childSlug) {
        if (!this.currentPreview) {
            return;
        }
        const message = previewChildSkillActivityMessage(childSlug);
        const exists = this.currentPreview.entries.some((entry) => sameDisplayMessage(entry, message));
        if (!exists) {
            this.insertPreviewMessage(message);
        }
    }
    ensurePreviewAssistantMessage() {
        if (!this.currentPreview) {
            return { role: 'assistant', content: '', pending: false };
        }
        const desiredTag = this.currentPreview.rootTag ?? this.currentPreview.activeChildTag;
        const last = this.currentPreview.entries[this.currentPreview.entries.length - 1];
        if (last && last.role === 'assistant') {
            last.tag = desiredTag;
            return last;
        }
        const created = { role: 'assistant', content: '', pending: true, tag: desiredTag };
        this.currentPreview.entries.push(created);
        return created;
    }
    finishExecutionPreview(options) {
        if (!this.currentPreview) {
            return;
        }
        this.currentPreview.activeChildTag = undefined;
        if (this.currentPreview.kind === 'shell') {
            const detail = typeof options.error === 'string' && options.error.trim()
                ? { role: 'system', kind: 'output', tag: 'shell', content: options.error, error: true }
                : typeof options.finalText === 'string' && options.finalText.trim()
                    ? { role: 'system', kind: 'output', tag: 'shell', content: options.finalText }
                    : null;
            if (detail) {
                this.currentPreview.entries.push(detail);
            }
            if (options.persist) {
                const existing = this.ephemeralMessagesBySessionId.get(this.currentPreview.sessionId) ?? [];
                existing.push(...this.currentPreview.entries.map((entry) => ({ ...entry, pending: false })));
                this.ephemeralMessagesBySessionId.set(this.currentPreview.sessionId, existing);
            }
            this.currentPreview = null;
            return;
        }
        const assistant = this.ensurePreviewAssistantMessage();
        if (typeof options.finalText === 'string' && options.finalText.trim()) {
            assistant.content = options.finalText;
        }
        if (typeof options.error === 'string' && options.error.trim()) {
            assistant.content = options.error;
            assistant.error = true;
        }
        assistant.pending = false;
        if (options.persist) {
            const existing = this.ephemeralMessagesBySessionId.get(this.currentPreview.sessionId) ?? [];
            existing.push(...this.currentPreview.entries.map((entry) => ({ ...entry, pending: false })));
            this.ephemeralMessagesBySessionId.set(this.currentPreview.sessionId, existing);
        }
        this.currentPreview = null;
    }
    completeCurrentPreview(options) {
        if (!this.currentPreview || this.currentPreview.kind === 'shell') {
            return;
        }
        this.currentPreview.activeChildTag = undefined;
        const assistant = this.ensurePreviewAssistantMessage();
        if (typeof options.finalText === 'string' && options.finalText.trim()) {
            assistant.content = options.finalText;
        }
        if (typeof options.error === 'string' && options.error.trim()) {
            assistant.content = options.error;
            assistant.error = true;
        }
        assistant.pending = false;
    }
    cancelCurrentExecution(message) {
        if (!this.busy || !this.currentAbortController) {
            this.statusLine = 'No execution is currently running.';
            this.requestRender();
            return;
        }
        if (this.currentAbortController.signal.aborted) {
            this.statusLine = 'Cancellation already requested.';
            this.requestRender();
            return;
        }
        this.currentAbortController.abort();
        this.statusLine = message;
        this.requestRender();
    }
    startAnimation() {
        if (this.animationTimer) {
            return;
        }
        this.animationTimer = setInterval(() => {
            if ((this.busy || this.currentPreview?.entries.some((entry) => entry.pending)) && !this.isEditingInput()) {
                this.requestRender();
            }
        }, 120);
    }
    stopAnimation() {
        if (!this.animationTimer) {
            return;
        }
        clearInterval(this.animationTimer);
        this.animationTimer = null;
    }
    scrollFocusedPane(direction, amount) {
        if (this.focusedPane === 'sessions') {
            const metrics = this.getLastPaneMetrics(this.focusedPane) ?? this.getPaneMetrics(this.focusedPane);
            if (metrics.maxStart === 0) {
                this.statusLine = `${paneDisplayName(this.focusedPane)} fits on screen.`;
                this.requestRender();
                return;
            }
            const delta = direction === 'down' ? amount : -amount;
            this.paneScroll.sessions = clamp(this.paneScroll.sessions + delta, 0, metrics.maxStart);
            this.statusLine = `Sessions scroll ${this.paneScroll.sessions + 1}/${metrics.maxStart + 1}.`;
            this.requestRender();
            return;
        }
        const pane = this.focusedPane;
        const metrics = this.getLastPaneMetrics(pane)
            ?? (this.paneScroll[pane] === 0 ? this.getRenderPaneMetrics(pane) : this.getPaneMetrics(pane));
        if (metrics.maxStart === 0) {
            this.paneScroll[pane] = 0;
            this.statusLine = `${paneDisplayName(pane)} fits on screen.`;
            this.requestRender();
            return;
        }
        const delta = direction === 'up' ? amount : -amount;
        const nextScroll = this.paneScroll[pane] + delta;
        this.paneScroll[pane] = metrics.maxStartIsExact
            ? clamp(nextScroll, 0, metrics.maxStart)
            : Math.max(0, nextScroll);
        this.statusLine = this.paneScroll[pane] === 0
            ? `${paneDisplayName(pane)} following newest content.`
            : `${paneDisplayName(pane)} scrolled ${this.paneScroll[pane]} line${this.paneScroll[pane] === 1 ? '' : 's'} above latest.`;
        this.requestRender();
    }
    jumpFocusedPane(target) {
        const metrics = this.getPaneMetrics(this.focusedPane);
        if (this.focusedPane === 'sessions') {
            this.paneScroll.sessions = target === 'top' ? 0 : metrics.maxStart;
            this.statusLine = target === 'top'
                ? 'Sessions at top.'
                : 'Sessions at bottom.';
            this.requestRender();
            return;
        }
        const pane = this.focusedPane;
        this.paneScroll[pane] = target === 'bottom' ? 0 : metrics.maxStart;
        this.statusLine = target === 'bottom'
            ? `${paneDisplayName(pane)} following newest content.`
            : `${paneDisplayName(pane)} at oldest visible content.`;
        this.requestRender();
    }
    pageScrollAmount() {
        const rows = output.rows ?? 40;
        return Math.max(3, Math.floor(Math.max(1, rows - 5) / 2));
    }
    async moveSession(offset) {
        if (this.sessions.length === 0) {
            return;
        }
        const currentIndex = Math.max(0, this.sessions.findIndex((session) => session.id === this.selectedSessionId));
        const nextIndex = (currentIndex + offset + this.sessions.length) % this.sessions.length;
        const nextSession = this.sessions[nextIndex];
        if (!nextSession || nextSession.id === this.selectedSessionId) {
            return;
        }
        this.selectedSessionId = nextSession.id;
        await this.loadSelectedSessionDetail();
        this.statusLine = `Selected session ${nextSession.title}.`;
        this.requestRender();
    }
    currentProcessActivity() {
        return this.activityFor(this.selectedSessionId || 'global');
    }
    graphemeLeftIndex(text, index) {
        if (index <= 0)
            return 0;
        if (!graphemeSegmenter)
            return index - 1;
        const graphemes = Array.from(graphemeSegmenter.segment(text), (g) => g.segment);
        let pos = 0;
        for (const g of graphemes) {
            if (pos + g.length >= index)
                break;
            pos += g.length;
        }
        return pos;
    }
    graphemeRightIndex(text, index) {
        if (index >= text.length)
            return text.length;
        if (!graphemeSegmenter)
            return index + 1;
        const graphemes = Array.from(graphemeSegmenter.segment(text), (g) => g.segment);
        let pos = 0;
        for (const g of graphemes) {
            if (pos >= index) {
                pos += g.length;
                break;
            }
            pos += g.length;
        }
        return Math.min(pos, text.length);
    }
    insertText(text) {
        const previousInput = this.inputValue;
        this.inputValue = `${this.inputValue.slice(0, this.cursor)}${text}${this.inputValue.slice(this.cursor)}`;
        this.cursor += text.length;
        this.refreshCommandBarStatus(previousInput);
    }
    deleteBackward() {
        if (this.cursor === 0) {
            return;
        }
        const previousInput = this.inputValue;
        const deleteFrom = this.graphemeLeftIndex(this.inputValue, this.cursor);
        this.inputValue = `${this.inputValue.slice(0, deleteFrom)}${this.inputValue.slice(this.cursor)}`;
        this.cursor = deleteFrom;
        this.refreshCommandBarStatus(previousInput);
    }
    deleteForward() {
        if (this.cursor >= this.inputValue.length) {
            return;
        }
        const previousInput = this.inputValue;
        this.inputValue = `${this.inputValue.slice(0, this.cursor)}${this.inputValue.slice(this.cursor + 1)}`;
        this.refreshCommandBarStatus(previousInput);
    }
    async refreshCommands() {
        this.commands = await listCommands(this.workspaceRoot);
        await this.refreshShellContext();
    }
    async refreshShellContext() {
        const config = await loadConfig(this.workspaceRoot);
        this.shellWorkspacePath = path.resolve(this.workspaceRoot, config.workspace || './workspace');
        this.shellConfig = config.shell;
    }
    getShellBackend() {
        return describeShellExecutionBackend(this.options.sandbox ?? false, this.shellConfig);
    }
    refreshCommandBarStatus(previousInput = '') {
        if (this.busy || this.pendingPrompt || this.uiMode !== 'command') {
            return;
        }
        const wasShell = previousInput.trimStart().startsWith('!');
        const isShell = this.inputValue.trimStart().startsWith('!');
        if (isShell) {
            this.statusLine = formatShellModeStatus(this.workspaceRoot, this.shellWorkspacePath, this.options.sandbox, this.shellConfig);
            return;
        }
        if (wasShell) {
            this.statusLine = 'Command bar ready.';
        }
    }
    async refreshSessions(options = {}) {
        let sessions = await listConductorSessions(this.workspaceRoot);
        if (sessions.length === 0 && options.createIfMissing) {
            const created = await createConductorSession(this.workspaceRoot);
            sessions = [created];
        }
        this.sessions = sessions;
        const desiredSessionId = options.selectedSessionId ?? this.selectedSessionId ?? sessions[0]?.id ?? '';
        this.selectedSessionId = sessions.find((session) => session.id === desiredSessionId)?.id ?? sessions[0]?.id ?? '';
        this.ensureSessionVisible();
        await this.loadSelectedSessionDetail();
    }
    async loadSelectedSessionDetail() {
        if (!this.selectedSessionId) {
            this.sessionDetail = null;
            this.liveUsageByModel = {};
            return;
        }
        this.sessionDetail = await getConductorSessionDetail(this.workspaceRoot, this.selectedSessionId);
        this.liveUsageByModel = {};
        if (this.currentPreview
            && this.currentPreview.kind === 'task'
            && this.currentPreview.sessionId === this.selectedSessionId
            && sessionMessagesContainCompletedTaskPreview(this.sessionDetail.messages, this.currentPreview.entries)) {
            this.currentPreview = null;
        }
        const existingEphemeral = this.ephemeralMessagesBySessionId.get(this.selectedSessionId);
        if (!existingEphemeral || existingEphemeral.length === 0) {
            return;
        }
        const reconciled = reconcileEphemeralMessages(this.sessionDetail.messages, existingEphemeral);
        if (reconciled.length === 0) {
            this.ephemeralMessagesBySessionId.delete(this.selectedSessionId);
            return;
        }
        this.ephemeralMessagesBySessionId.set(this.selectedSessionId, reconciled);
    }
    activityFor(key) {
        const existing = this.activityBySessionId.get(key);
        if (existing) {
            return existing;
        }
        const created = new ActivityBuffer();
        this.activityBySessionId.set(key, created);
        return created;
    }
    taskTrackerFor(sessionId) {
        let tracker = this.taskTrackerBySessionId.get(sessionId);
        if (!tracker) {
            tracker = new TaskTracker();
            this.taskTrackerBySessionId.set(sessionId, tracker);
        }
        return tracker;
    }
    async persistShellCommandResult(command, result) {
        let sessionId = this.selectedSessionId;
        if (!sessionId) {
            await this.refreshSessions({ createIfMissing: true });
            sessionId = this.selectedSessionId;
        }
        if (!sessionId) {
            throw new Error('No conductor session available to persist shell output.');
        }
        await appendConductorSessionMessages(this.workspaceRoot, sessionId, buildPersistedShellTranscript(command, result));
        return sessionId;
    }
    requestRender() {
        if (this.renderTimer) {
            return;
        }
        const minInterval = this.isEditingInput()
            ? TYPING_RENDER_INTERVAL_MS
            : this.busy
                ? BUSY_RENDER_INTERVAL_MS
                : IDLE_RENDER_INTERVAL_MS;
        const delay = Math.max(0, minInterval - (Date.now() - this.lastRenderAt));
        this.renderTimer = setTimeout(() => {
            this.renderTimer = null;
            this.render();
        }, delay);
    }
    render() {
        this.lastRenderAt = Date.now();
        const columns = output.columns ?? 120;
        const rows = output.rows ?? 40;
        const contentHeight = Math.max(1, rows - 3);
        const widths = computePaneWidths(columns);
        const rightHeights = computeRightPaneHeights(contentHeight);
        const sessionsPane = this.buildPaneView('sessions', 'Sessions', widths.left, contentHeight);
        const messagesPane = this.buildPaneView('messages', 'Messages', widths.center, contentHeight);
        const processPane = this.buildPaneView('process', 'Execution', widths.right, rightHeights.process);
        const tasksPane = this.buildPaneView('tasks', 'Tasks', widths.right, rightHeights.tasks);
        const contextPane = this.buildPaneView('context', 'Context', widths.right, rightHeights.context);
        const workspaceView = this.uiMode === 'command'
            ? null
            : this.buildWorkspacePaneView(columns, contentHeight);
        const header = renderMenuBar(this.buildHeaderLine(), columns, this.uiMode === 'menu' ? this.menuState.activeIndex : null);
        const status = renderStatusBar(this.buildStatusLine(), columns, this.buildHotkeys());
        const inputPrefix = this.buildInputPrefix();
        const inputValue = this.buildInputValue();
        const inputLine = renderCommandBar(inputPrefix, inputValue, columns);
        const gap = paint(' ', ANSI.bgBlue);
        const lines = [header];
        if (workspaceView) {
            lines.push(...workspaceView.lines);
        }
        else {
            for (let index = 0; index < contentHeight; index += 1) {
                let rightLine;
                if (index < rightHeights.process) {
                    rightLine = processPane[index];
                }
                else if (index < rightHeights.process + rightHeights.tasks) {
                    rightLine = tasksPane[index - rightHeights.process];
                }
                else {
                    rightLine = contextPane[index - rightHeights.process - rightHeights.tasks];
                }
                lines.push(`${sessionsPane[index]}${gap}${messagesPane[index]}${gap}${rightLine}`);
            }
        }
        lines.push(status);
        lines.push(inputLine);
        if (this.selection) {
            this.applySelectionHighlight(lines, widths, rightHeights, contentHeight);
        }
        this.paintFrame(lines, { columns, rows });
        if (workspaceView?.cursor && !this.pendingPrompt) {
            output.write(`\u001b[${workspaceView.cursor.row};${workspaceView.cursor.column}H`);
            return;
        }
        const textBeforeCursor = this.inputValue.slice(0, this.cursor);
        const cursorColumn = Math.min(columns, inputPrefix.length + measureDisplayWidth(textBeforeCursor) + 1);
        output.write(`\u001b[${rows};${cursorColumn}H`);
    }
    paintFrame(lines, size) {
        const paddedLines = lines.map((line) => padRight(line, size.columns));
        const patch = computeFramePatch(this.previousFrameLines, paddedLines, this.previousFrameSize, size);
        let buffer = '\u001b[0m';
        if (patch.fullRender) {
            buffer += `\u001b[H${paddedLines.join('\n')}\u001b[?25h`;
        }
        else if (patch.changedRows.length > 0) {
            for (const change of patch.changedRows) {
                buffer += `\u001b[${change.row};1H${change.line}`;
            }
            buffer += '\u001b[?25h';
        }
        else {
            buffer += '\u001b[?25h';
        }
        output.write(buffer);
        this.previousFrameLines = [...paddedLines];
        this.previousFrameSize = size;
    }
    buildHeaderLine() {
        const sessionLabel = this.sessionDetail
            ? `${this.sessionDetail.title} (${this.sessionDetail.id.slice(0, 8)})`
            : 'no session';
        const skillLabel = this.commands.length > 0
            ? `${this.commands.length} skill${this.commands.length === 1 ? '' : 's'}`
            : 'no skills';
        const mode = this.busy ? '[Running]' : '[Idle]';
        return `${sessionLabel}   ${mode}   ${skillLabel}`;
    }
    buildStatusLine() {
        const spinner = this.busy || this.currentPreview?.entries.some((entry) => entry.pending)
            ? ` ${currentSpinnerFrame()}`
            : '';
        if (this.uiMode === 'menu') {
            const menu = this.getMenuDefinitions()[this.menuState.activeIndex];
            return `${spinner} Menu ${menu?.label ?? ''} | ${menu?.items[this.menuState.selectedIndex]?.description ?? 'Choose an action.'}`;
        }
        if ((this.uiMode === 'picker' || this.uiMode === 'palette') && this.pickerState) {
            const selected = this.pickerState.filteredItems[this.pickerState.selectedIndex];
            return `${spinner} ${this.pickerState.title} | ${selected?.description ?? this.pickerState.emptyText}`;
        }
        if (this.uiMode === 'viewer' && this.viewerState) {
            return `${spinner} ${this.viewerState.title} | ${this.viewerState.path ? path.relative(this.workspaceRoot, this.viewerState.path) : 'read-only view'}`;
        }
        if (this.uiMode === 'editor' && this.editorState) {
            const dirty = this.editorState.dirty ? 'modified' : 'saved';
            return `${spinner} ${this.editorState.title} | ${dirty} | Ln ${this.editorState.cursorLine + 1}, Col ${this.editorState.cursorColumn + 1}`;
        }
        const focus = paneDisplayName(this.focusedPane);
        if (!this.pendingPrompt && this.focusedPane !== 'sessions') {
            const follow = this.paneScroll[this.focusedPane] === 0 ? 'follow' : `scroll ${this.paneScroll[this.focusedPane]}`;
            return `${spinner} ${this.statusLine} | Focus ${focus} | ${follow}`;
        }
        if (this.pendingPrompt) {
            return this.pendingPrompt.kind === 'clarify'
                ? `${spinner} Clarify: ${condenseWhitespace(this.pendingPrompt.prompt)} | Focus ${focus}`
                : `${spinner} Input: ${condenseWhitespace(this.pendingPrompt.prompt)} | Focus ${focus}`;
        }
        return `${spinner} ${this.statusLine} | Focus ${focus}`;
    }
    buildInputPrefix() {
        if (this.uiMode === 'menu') {
            return ' Menu > ';
        }
        if (this.uiMode === 'picker' || this.uiMode === 'palette') {
            return ' Search > ';
        }
        if (this.pendingPrompt?.kind === 'clarify') {
            return ' Clarify ? ';
        }
        if (this.pendingPrompt) {
            return ' Input > ';
        }
        if (this.uiMode === 'editor') {
            return ' Editor > ';
        }
        if (this.uiMode === 'viewer') {
            return ' Viewer > ';
        }
        return ' Command > ';
    }
    buildInputValue() {
        if (this.uiMode === 'editor') {
            return 'Ctrl+S save  Esc close  arrows move';
        }
        if (this.uiMode === 'viewer') {
            return this.viewerState?.canEdit ? 'Ctrl+E edit  Esc close  PgUp/PgDn scroll' : 'Esc close  PgUp/PgDn scroll';
        }
        return this.inputValue;
    }
    buildSessionPaneBody() {
        if (this.sessions.length === 0) {
            return ['No sessions yet.'];
        }
        const lines = [];
        for (const session of this.sessions) {
            const selected = session.id === this.selectedSessionId ? '>' : ' ';
            lines.push(`${selected} ${session.title}`);
            lines.push(`  ${formatTimestamp(session.updatedAt)}`);
        }
        return lines;
    }
    buildMessageEntries() {
        const entries = [];
        if (this.sessionDetail) {
            for (const message of this.sessionDetail.messages) {
                entries.push({ role: message.role, content: message.content });
            }
        }
        const persistedEphemeral = this.ephemeralMessagesBySessionId.get(this.selectedSessionId) ?? [];
        entries.push(...persistedEphemeral);
        if (this.currentPreview && this.currentPreview.sessionId === this.selectedSessionId) {
            entries.push(...this.currentPreview.entries);
        }
        return entries;
    }
    buildMessagesPaneBody(options = {}) {
        const entries = this.buildMessageEntries();
        if (entries.length === 0) {
            return [
                '[System] No messages yet.',
                '  Enter a prompt to start a conductor turn.',
                '  The center pane shows user and assistant messages.',
            ];
        }
        const visibleEntries = typeof options.recentEntryLimit === 'number' && options.recentEntryLimit > 0
            ? entries.slice(-options.recentEntryLimit)
            : entries;
        const lines = [];
        for (const entry of visibleEntries) {
            const header = this.formatMessageHeader(entry);
            lines.push(header);
            const bodyLines = formatDisplayMessageBodyLines(entry);
            for (const line of bodyLines) {
                lines.push(`  ${line}`);
            }
            lines.push('');
        }
        return lines;
    }
    buildProcessPaneBody(options = {}) {
        const activity = typeof options.tailLineLimit === 'number' && options.tailLineLimit > 0
            ? this.currentProcessActivity().snapshotTail(options.tailLineLimit)
            : this.currentProcessActivity().snapshot();
        if (activity.length > 0) {
            return activity;
        }
        return [
            'No execution activity yet.',
            'Tool calls, logs, outputs, and exceptions render here.',
            'Use /cancel to abort the current execution.',
        ];
    }
    buildTasksPaneBody() {
        const tracker = this.taskTrackerFor(this.selectedSessionId);
        const columns = output.columns ?? 120;
        const widths = computePaneWidths(columns);
        return tracker.buildPaneBody(widths.right);
    }
    buildContextPaneBody() {
        if (!this.sessionDetail) {
            return ['No session selected.'];
        }
        const contextSize = summarizeContextSize(this.sessionDetail);
        const systemAssetSources = getSystemAssetSourcePaths(this.workspaceRoot);
        const mergedUsage = mergeTokenUsageMaps(this.sessionDetail.usageByModel ?? {}, this.liveUsageByModel);
        const usageEntries = Object.entries(mergedUsage)
            .sort((left, right) => right[1].totalTokens - left[1].totalTokens || left[0].localeCompare(right[0]));
        const body = [
            'Context Size',
            `Messages ${formatInteger(contextSize.messageCount)}`,
            `Transcript ${formatByteSize(contextSize.messageBytes)} (~${formatInteger(contextSize.messageTokens)} tok)`,
            `Task Memory ${formatByteSize(contextSize.taskMemoryBytes)} (~${formatInteger(contextSize.taskMemoryTokens)} tok)`,
            `Assistant Memory ${formatByteSize(contextSize.assistantMemoryBytes)} (~${formatInteger(contextSize.assistantMemoryTokens)} tok)`,
            `Total ${formatByteSize(contextSize.totalBytes)} (~${formatInteger(contextSize.totalTokens)} tok)`,
            '',
            'System Asset Sources',
            `Conductor DML ${formatSystemAssetSourcePath(this.workspaceRoot, systemAssetSources.conductorDml)}`,
            `Conductor Prompt ${formatSystemAssetSourcePath(this.workspaceRoot, systemAssetSources.conductorPrompt)}`,
            `Plan DML ${formatSystemAssetSourcePath(this.workspaceRoot, systemAssetSources.planDml)}`,
            `Skill Creator DML ${formatSystemAssetSourcePath(this.workspaceRoot, systemAssetSources.skillCreatorDml)}`,
            `Skill Creator Prompt ${formatSystemAssetSourcePath(this.workspaceRoot, systemAssetSources.skillCreatorPrompt)}`,
            `Task Prompt ${formatSystemAssetSourcePath(this.workspaceRoot, systemAssetSources.taskPrompt)}`,
            'Changes apply on the next turn or /compile run. No TUI reload is required.',
            '',
            'Shell Execution',
            `Backend ${this.getShellBackend().backendLabel}`,
            `Mode ${this.getShellBackend().description}`,
            `CWD ${this.shellWorkspacePath}`,
            '',
            'Session Token Usage',
        ];
        if (usageEntries.length === 0) {
            body.push('(no usage recorded yet)');
            return body;
        }
        for (const [modelId, usage] of usageEntries) {
            body.push(modelId);
            body.push(`  total ${formatInteger(usage.totalTokens)}  in ${formatInteger(usage.inputTokens)}  out ${formatInteger(usage.outputTokens)}  calls ${formatInteger(usage.calls)}`);
            const extras = [];
            if (usage.cacheReadTokens > 0) {
                extras.push(`cache-read ${formatInteger(usage.cacheReadTokens)}`);
            }
            if (usage.cacheWriteTokens > 0) {
                extras.push(`cache-write ${formatInteger(usage.cacheWriteTokens)}`);
            }
            if (usage.reasoningTokens > 0) {
                extras.push(`reasoning ${formatInteger(usage.reasoningTokens)}`);
            }
            if (extras.length > 0) {
                body.push(`  ${extras.join('  ')}`);
            }
            body.push('');
        }
        return body;
    }
    buildPaneView(kind, title, width, height) {
        const metrics = this.getRenderPaneMetrics(kind, width, height);
        this.lastPaneMetrics[kind] = {
            maxStart: metrics.maxStart,
            viewportLines: metrics.viewportLines,
            maxStartIsExact: metrics.maxStartIsExact,
        };
        const paneTitle = this.buildPaneTitle(title, kind, metrics);
        return buildPane(paneTitle, metrics.lines.slice(metrics.start, metrics.start + metrics.viewportLines), width, height, kind, this.focusedPane === kind);
    }
    buildWorkspacePaneView(width, height) {
        if (this.uiMode === 'editor' && this.editorState) {
            return this.buildEditorPaneView(width, height, this.editorState);
        }
        if ((this.uiMode === 'picker' || this.uiMode === 'palette') && this.pickerState) {
            return {
                lines: buildPane(this.pickerState.title, this.buildPickerPaneBody(this.pickerState), width, height, 'workspace', true),
            };
        }
        if (this.uiMode === 'viewer' && this.viewerState) {
            return {
                lines: buildPane(this.viewerState.title, this.buildViewerPaneBody(this.viewerState), width, height, 'workspace', true),
            };
        }
        const menu = this.getMenuDefinitions()[this.menuState.activeIndex];
        return {
            lines: buildPane(`Menu: ${menu?.label ?? 'Menu'}`, this.buildMenuPaneBody(menu), width, height, 'workspace', true),
        };
    }
    buildMenuPaneBody(menu) {
        if (!menu) {
            return ['No menu available.'];
        }
        const lines = [
            'Use Left/Right or 1-6 to move across the menu bar, Up/Down to choose an item, and Enter to run it.',
            '',
        ];
        menu.items.forEach((item, index) => {
            const prefix = index === this.menuState.selectedIndex ? '>' : ' ';
            const suffix = item.enabled === false ? ' [disabled]' : item.shortcut ? ` [${item.shortcut}]` : '';
            lines.push(`${prefix} ${item.label}${suffix}`);
            lines.push(`  ${item.description}`);
            lines.push('');
        });
        return lines;
    }
    buildPickerPaneBody(state) {
        const lines = [
            `Search: ${this.inputValue || '(type to filter)'}`,
            `Results: ${state.filteredItems.length}/${state.items.length}`,
            '',
        ];
        if (state.filteredItems.length === 0) {
            lines.push(state.emptyText);
            return lines;
        }
        state.filteredItems.forEach((item, index) => {
            const prefix = index === state.selectedIndex ? '>' : ' ';
            lines.push(`${prefix} ${item.label}${item.detail ? `  ${item.detail}` : ''}`);
            lines.push(`  ${item.description}`);
            lines.push('');
        });
        return lines.slice(state.scrollTop);
    }
    buildViewerPaneBody(state) {
        const lines = [
            state.path ? `Path: ${path.relative(this.workspaceRoot, state.path)}` : 'Read-only view',
            state.canEdit ? 'Ctrl+E opens the editor for this file.' : 'Read-only content.',
            '',
            ...state.content.split(/\r?\n/).map((line, index) => `${String(index + 1).padStart(4, ' ')} | ${line}`),
        ];
        return lines.slice(state.scrollTop);
    }
    buildEditorPaneView(width, height, state) {
        const innerWidth = Math.max(1, width - 2);
        const viewportLines = Math.max(0, height - 2);
        const lineNumberWidth = Math.max(2, String(Math.max(1, state.lines.length)).length);
        const prefixWidth = 2 + lineNumberWidth + 3;
        const contentWidth = Math.max(1, innerWidth - prefixWidth);
        const maxStart = Math.max(0, state.lines.length - viewportLines);
        state.scrollTop = clamp(state.scrollTop, 0, maxStart);
        const lines = [renderWindowTop(`${state.title}${state.dirty ? ' [modified]' : ''}`, innerWidth, true)];
        for (let offset = 0; offset < viewportLines; offset += 1) {
            const lineIndex = state.scrollTop + offset;
            const lineNumber = String(lineIndex + 1).padStart(lineNumberWidth, ' ');
            const content = state.lines[lineIndex] ?? '';
            const visibleContent = content.slice(state.scrollLeft, state.scrollLeft + contentWidth);
            const prefix = lineIndex === state.cursorLine ? '> ' : '  ';
            const rowText = `${prefix}${lineNumber} | ${visibleContent}`;
            lines.push(renderWindowBody(padRight(rowText, innerWidth), innerWidth, 'workspace'));
        }
        if (height >= 2) {
            lines.push(renderWindowBottom(innerWidth, true));
        }
        const cursorRow = Math.min(2 + viewportLines, 3 + Math.max(0, state.cursorLine - state.scrollTop));
        const cursorColumn = Math.min(width, 2 + prefixWidth + Math.max(0, state.cursorColumn - state.scrollLeft));
        return {
            lines: lines.slice(0, height),
            cursor: { row: cursorRow, column: cursorColumn },
        };
    }
    buildHotkeys() {
        if (this.uiMode === 'menu') {
            return [
                ['1-6', 'Menu'],
                ['<- ->', 'Switch'],
                ['Up/Down', 'Item'],
                ['Enter', 'Run'],
                ['Esc', 'Close'],
            ];
        }
        if (this.uiMode === 'picker' || this.uiMode === 'palette') {
            return [
                ['type', 'Filter'],
                ['Up/Down', 'Pick'],
                ['Enter', 'Open'],
                ['Ctrl+E', 'Edit'],
                ['Esc', 'Close'],
            ];
        }
        if (this.uiMode === 'viewer') {
            return [
                ['PgUp/PgDn', 'Scroll'],
                ['Ctrl+E', 'Edit'],
                ['Ctrl+R', 'Run'],
                ['Esc', 'Close'],
            ];
        }
        if (this.uiMode === 'editor') {
            return [
                ['Arrows', 'Move'],
                ['Enter', 'Split'],
                ['Ctrl+S', 'Save'],
                ['Esc', 'Close'],
            ];
        }
        return [
            ['F10', 'Menu'],
            ['Ctrl+P', 'Palette'],
            ['<- ->', 'Pane'],
            [this.focusedPane === 'sessions' ? 'Up/Down' : 'PgUp/PgDn', this.focusedPane === 'sessions' ? 'Select' : 'Scroll'],
            ['End', this.focusedPane === 'sessions' ? 'Bottom' : 'Follow'],
            ['Drag', 'Select'],
            ...(this.busy ? [['Ctrl+T', 'Cancel']] : []),
            ['/cancel', 'Stop'],
        ];
    }
    buildPaneTitle(title, kind, metrics) {
        if (kind === 'sessions') {
            return this.focusedPane === kind ? `${title} [focus]` : title;
        }
        if (metrics.maxStart === 0) {
            return this.focusedPane === kind ? `${title} [focus]` : title;
        }
        return this.paneScroll[kind] === 0
            ? `${title}${this.focusedPane === kind ? ' [focus]' : ''} [follow]`
            : `${title}${this.focusedPane === kind ? ' [focus]' : ''} [-${this.paneScroll[kind]}]`;
    }
    cachedCollectTailWrappedLines(cacheKey, body, innerWidth, limit) {
        const cached = this.wrapCache.get(cacheKey);
        const bodyKey = `${body.length}:${body.length > 0 ? body[body.length - 1] : ''}:${limit}`;
        if (cached && cached.bodyRef === body && cached.bodyKey === bodyKey && cached.innerWidth === innerWidth) {
            return { lines: cached.result, truncated: body.length > 0 && cached.result.length < body.length };
        }
        const result = collectTailWrappedLines(body, innerWidth, limit);
        this.wrapCache.set(cacheKey, { bodyRef: body, bodyKey, innerWidth, result: result.lines });
        return result;
    }
    getRenderPaneMetrics(kind, width = paneDimensionsForKind(kind, output.columns ?? 120, output.rows ?? 40).width, height = paneDimensionsForKind(kind, output.columns ?? 120, output.rows ?? 40).height) {
        if ((kind === 'process' || kind === 'messages') && this.paneScroll[kind] === 0) {
            const innerWidth = Math.max(1, width - 2);
            const viewportLines = Math.max(0, height - 2);
            const rawBody = kind === 'messages'
                ? this.buildMessagesPaneBody({ recentEntryLimit: Math.max(8, viewportLines * 2) })
                : this.buildProcessPaneBody({ tailLineLimit: Math.max(16, viewportLines * 4) });
            const tail = this.cachedCollectTailWrappedLines(`${kind}:tail`, rawBody, innerWidth, viewportLines);
            return {
                lines: tail.lines,
                start: 0,
                maxStart: tail.truncated ? 1 : 0,
                viewportLines,
                maxStartIsExact: !tail.truncated,
            };
        }
        return this.getPaneMetrics(kind, width, height);
    }
    getLastPaneMetrics(kind) {
        const metrics = this.lastPaneMetrics[kind];
        return metrics.viewportLines > 0 ? metrics : null;
    }
    getPaneMetrics(kind, width = paneDimensionsForKind(kind, output.columns ?? 120, output.rows ?? 40).width, height = paneDimensionsForKind(kind, output.columns ?? 120, output.rows ?? 40).height) {
        const innerWidth = Math.max(1, width - 2);
        const rawBody = this.getPaneBody(kind);
        const lines = this.cachedWrapBodyLines(kind, rawBody, innerWidth);
        const viewportLines = Math.max(0, height - 2);
        const maxStart = Math.max(0, lines.length - viewportLines);
        if (kind === 'sessions') {
            this.paneScroll.sessions = clamp(this.paneScroll.sessions, 0, maxStart);
            return {
                lines,
                start: this.paneScroll.sessions,
                maxStart,
                viewportLines,
                maxStartIsExact: true,
            };
        }
        this.paneScroll[kind] = clamp(this.paneScroll[kind], 0, maxStart);
        return {
            lines,
            start: Math.max(0, maxStart - this.paneScroll[kind]),
            maxStart,
            viewportLines,
            maxStartIsExact: true,
        };
    }
    cachedWrapBodyLines(cacheKey, body, innerWidth) {
        const cached = this.wrapCache.get(cacheKey);
        const bodyKey = `${body.length}:${body.length > 0 ? body[body.length - 1] : ''}`;
        if (cached && cached.bodyRef === body && cached.bodyKey === bodyKey && cached.innerWidth === innerWidth) {
            return cached.result;
        }
        const result = wrapBodyLines(body, innerWidth);
        this.wrapCache.set(cacheKey, { bodyRef: body, bodyKey, innerWidth, result });
        return result;
    }
    getPaneBody(kind) {
        switch (kind) {
            case 'sessions':
                return this.buildSessionPaneBody();
            case 'messages':
                return this.buildMessagesPaneBody();
            case 'process':
                return this.buildProcessPaneBody({ tailLineLimit: MAX_ACTIVITY_LINES });
            case 'tasks':
                return this.buildTasksPaneBody();
            case 'context':
                return this.buildContextPaneBody();
        }
    }
    ensureSessionVisible() {
        const selectedIndex = Math.max(0, this.sessions.findIndex((session) => session.id === this.selectedSessionId));
        const contentRows = Math.max(1, (output.rows ?? 40) - 5);
        const selectedLine = selectedIndex * 2;
        if (selectedLine < this.paneScroll.sessions) {
            this.paneScroll.sessions = selectedLine;
            return;
        }
        if (selectedLine >= this.paneScroll.sessions + contentRows) {
            this.paneScroll.sessions = Math.max(0, selectedLine - contentRows + 1);
        }
    }
    formatMessageHeader(entry) {
        return formatDisplayMessageHeader(entry, '');
    }
    close() {
        if (!this.closeResolver) {
            return;
        }
        const resolve = this.closeResolver;
        this.closeResolver = null;
        resolve();
    }
    manageToolAbortSignal(event) {
        if (event.type !== 'tool_call' || !event.toolState) {
            return;
        }
        if (event.toolState === 'starting' || event.toolState === 'running') {
            if (!this.toolAbortController || this.toolAbortController.signal.aborted) {
                this.toolAbortController = new AbortController();
            }
            this.toolAbortSignalRef.signal = this.toolAbortController.signal;
        }
        else {
            this.toolAbortController = null;
            this.toolAbortSignalRef.signal = undefined;
        }
    }
    isEditingInput() {
        return this.uiMode !== 'editor' && this.inputValue.length > 0;
    }
}
export async function startTui(workspaceRoot = process.cwd(), options = {}) {
    if (input.isTTY && output.isTTY) {
        const app = new FullscreenTui(workspaceRoot, options);
        await app.start();
        return;
    }
    await startLineTui(workspaceRoot, options);
}
async function startLineTui(workspaceRoot = process.cwd(), options = {}) {
    const rl = createInterface({ input, output });
    try {
        let session = await chooseSession(rl, workspaceRoot);
        renderHeader(session);
        while (true) {
            const prompt = `${paint(session.title || session.id.slice(0, 8), ANSI.bold)} ${paint('>', ANSI.dim)} `;
            const line = (await rl.question(prompt)).trim();
            if (!line) {
                continue;
            }
            const parsed = parseCommandBarInput(line);
            if (parsed.kind === 'builtin') {
                switch (parsed.name) {
                    case 'cancel':
                        console.log('No execution is currently running in line mode.');
                        continue;
                    case 'exit':
                    case 'quit':
                        return;
                    case 'new':
                        session = parsed.rawArgs
                            ? await createConductorSession(workspaceRoot, parsed.rawArgs)
                            : await createSessionInteractive(rl, workspaceRoot);
                        console.log(`${paint('switched', ANSI.green)} ${session.title}`);
                        renderHeader(session);
                        continue;
                    case 'sessions':
                        session = await chooseSession(rl, workspaceRoot);
                        renderHeader(session);
                        continue;
                    case 'help':
                        for (const helpLine of buildHelpLines()) {
                            console.log(helpLine);
                        }
                        continue;
                    case 'run':
                        if (parsed.args.length === 0) {
                            console.log('Provide a DML file after /run.');
                            continue;
                        }
                        console.log(divider('-'));
                        console.log(`${paint('run', ANSI.dim)} /run ${parsed.args.join(' ')}`);
                        console.log(divider('-'));
                        {
                            const printer = new LiveExecutionPrinter();
                            try {
                                await runFileCommand(workspaceRoot, parsed.args[0], parsed.args.slice(1), {
                                    sessionId: session.id,
                                    sandbox: options.sandbox,
                                    onUserInput: async (question) => {
                                        printer.finish();
                                        console.log(`${paint('clarify', ANSI.yellow)} ${question}`);
                                        return (await rl.question('> ')).trim();
                                    },
                                    onEvent: (event) => printer.handle(event),
                                });
                            }
                            catch (error) {
                                printer.finish();
                                console.log(`${paint('error', ANSI.red)} ${error.message}`);
                            }
                            printer.finish();
                        }
                        console.log('');
                        console.log(divider('-'));
                        console.log('');
                        continue;
                    case 'compile':
                    case 'skill-creator':
                        if (!parsed.rawArgs.trim()) {
                            console.log(`Provide a skill specification after /${parsed.name}.`);
                            continue;
                        }
                        console.log(divider('-'));
                        console.log(`${paint('skill', ANSI.dim)} /${parsed.name} ${parsed.rawArgs}`);
                        console.log(divider('-'));
                        {
                            const printer = new LiveExecutionPrinter();
                            try {
                                const result = await runSkillCreatorCommand(workspaceRoot, session.id, parsed.rawArgs, {
                                    sandbox: options.sandbox,
                                    onUserInput: async (question) => {
                                        printer.finish();
                                        console.log(`${paint('clarify', ANSI.yellow)} ${question}`);
                                        return (await rl.question('> ')).trim();
                                    },
                                    onEvent: (event) => printer.handle(event),
                                });
                                printer.finish();
                                console.log(formatSkillCreatorSummary(workspaceRoot, result));
                            }
                            catch (error) {
                                printer.finish();
                                console.log(`${paint('error', ANSI.red)} ${error.message}`);
                            }
                        }
                        console.log('');
                        console.log(divider('-'));
                        console.log('');
                        continue;
                    case 'set-model': {
                        try {
                            const { model, slot } = parseSetModelCommandArgs(parsed.rawArgs);
                            const result = await setModel(workspaceRoot, model, slot);
                            console.log(`Model set to ${result.modelId} (${result.updatedSlots.join(', ')}).`);
                        }
                        catch (error) {
                            console.log(`${paint('error', ANSI.red)} ${error.message}`);
                        }
                        continue;
                    }
                }
            }
            if (parsed.kind === 'shell') {
                const config = await loadConfig(workspaceRoot);
                const shellWorkspacePath = path.resolve(workspaceRoot, config.workspace || './workspace');
                console.log(divider('-'));
                console.log(`${paint('shell', ANSI.dim)} !${parsed.command}`);
                console.log(formatShellModeStatus(workspaceRoot, shellWorkspacePath, options.sandbox, config.shell));
                console.log(divider('-'));
                const printer = new LiveExecutionPrinter();
                try {
                    const result = await runShellCommand(workspaceRoot, parsed.command, {
                        sandbox: options.sandbox,
                        onEvent: (event) => printer.handle(event),
                    });
                    printer.finish();
                    console.log(formatShellCommandSummary(result));
                }
                catch (error) {
                    printer.finish();
                    console.log(`${paint('error', ANSI.red)} ${error.message}`);
                }
                console.log('');
                console.log(divider('-'));
                console.log('');
                continue;
            }
            if (parsed.kind === 'skill') {
                console.log(divider('-'));
                console.log(`${paint('skill', ANSI.dim)} /${parsed.name}${parsed.args.length > 0 ? ` ${parsed.args.join(' ')}` : ''}`);
                console.log(divider('-'));
                const printer = new LiveExecutionPrinter();
                try {
                    await runSlashCommand(workspaceRoot, parsed.name, parsed.args, {
                        sessionId: session.id,
                        sandbox: options.sandbox,
                        onUserInput: async (question) => {
                            printer.finish();
                            console.log(`${paint('clarify', ANSI.yellow)} ${question}`);
                            return (await rl.question('> ')).trim();
                        },
                        onEvent: (event) => printer.handle(event),
                    });
                }
                catch (error) {
                    printer.finish();
                    console.log(`${paint('error', ANSI.red)} ${error.message}`);
                }
                printer.finish();
                console.log('');
                console.log(divider('-'));
                console.log('');
                continue;
            }
            console.log(divider('-'));
            console.log(`${paint('task', ANSI.dim)} ${parsed.prompt}`);
            console.log(divider('-'));
            const printer = new LiveExecutionPrinter();
            const result = await runConductorTurn(parsed.prompt, {
                workspaceRoot,
                sessionId: session.id,
                stream: true,
                headless: true,
                sandbox: options.sandbox,
                onUserInput: async (question) => {
                    printer.finish();
                    console.log(`${paint('clarify', ANSI.yellow)} ${question}`);
                    return (await rl.question('> ')).trim();
                },
                onEvent: (event) => printer.handle(event),
            });
            printer.finish();
            console.log('');
            console.log(divider('-'));
            console.log('');
            const sessions = await listConductorSessions(workspaceRoot);
            session = sessions.find((entry) => entry.id === result.sessionId) ?? session;
        }
    }
    finally {
        rl.close();
        releaseTuiInputStream(input);
    }
}
export async function runPromptHeadless(prompt, workspaceRoot = process.cwd(), options = {}) {
    const session = await createConductorSession(workspaceRoot);
    console.log(`Session: ${session.id}`);
    const result = await runConductorTurn(prompt, {
        workspaceRoot,
        sessionId: session.id,
        stream: false,
        headless: true,
        sandbox: options.sandbox,
    });
    if (result.output.length > 0 || result.answer) {
        console.log('');
    }
    for (const line of result.output) {
        console.log(line);
    }
    if (result.answer) {
        if (result.output.length > 0) {
            console.log('');
        }
        console.log(result.answer);
    }
    if (result.error) {
        throw new Error(result.error);
    }
}
export async function runSkillCommand(workspaceRoot, skillName, args, options = {}) {
    const commands = await listCommands(workspaceRoot, { detailed: true });
    const command = commands.find((entry) => entry.name === skillName);
    if (!command) {
        throw new Error(`Unknown skill: ${skillName}`);
    }
    // Collapse args for single-parameter skills
    const collapsedArgs = command.parameters?.length === 1 && args.length > 0
        ? [args.join(' ')]
        : args;
    return executeRunnableCommand(workspaceRoot, {
        inputText: `/${skillName}${args.length > 0 ? ` ${args.join(' ')}` : ''}`,
        runPath: command.path,
        childSlug: skillName,
        skillName,
    }, collapsedArgs, options);
}
export async function runSlashCommand(workspaceRoot, name, args, options = {}) {
    const commands = await listCommands(workspaceRoot, { detailed: true });
    const command = commands.find((entry) => entry.name === name);
    if (command) {
        // Collapse args for single-parameter skills
        const slashCmdCollapsedArgs = command.parameters?.length === 1 && args.length > 0
            ? [args.join(' ')]
            : args;
        return executeRunnableCommand(workspaceRoot, {
            inputText: `/${name}${args.length > 0 ? ` ${args.join(' ')}` : ''}`,
            runPath: command.path,
            childSlug: name,
            skillName: name,
        }, slashCmdCollapsedArgs, options);
    }
    const resolvedPlanPath = await resolveWorkspaceDmlPath(workspaceRoot, name, { mode: 'plans-only' });
    if (!resolvedPlanPath) {
        throw new Error(`Unknown skill or plan: ${name}`);
    }
    const childSlug = path.basename(resolvedPlanPath, path.extname(resolvedPlanPath)) || name;
    return executeRunnableCommand(workspaceRoot, {
        inputText: `/${name}${args.length > 0 ? ` ${args.join(' ')}` : ''}`,
        runPath: resolvedPlanPath,
        childSlug,
        skillName: childSlug,
    }, args, options);
}
export async function runFileCommand(workspaceRoot, fileSpec, args, options = {}) {
    const resolvedPath = await resolveWorkspaceDmlPath(workspaceRoot, fileSpec, { mode: 'direct' });
    if (!resolvedPath) {
        throw new Error(`DML file not found: ${fileSpec}`);
    }
    const childSlug = path.basename(resolvedPath, path.extname(resolvedPath)) || 'run';
    return executeRunnableCommand(workspaceRoot, {
        inputText: `/run ${[fileSpec, ...args].join(' ')}`.trim(),
        runPath: resolvedPath,
        childSlug,
        skillName: childSlug,
    }, args, options);
}
async function executeRunnableCommand(workspaceRoot, target, args, options = {}) {
    const usageByModel = {};
    const config = await loadConfig(workspaceRoot);
    const runSelection = resolveModelSlot(config, 'run');
    const modelId = runSelection.id;
    const executionLog = options.sessionId
        ? createSessionExecutionLogWriter({
            workspaceRoot,
            sessionId: options.sessionId,
            executionKind: 'skill',
            inputText: target.inputText,
            skillName: target.skillName,
            args,
        })
        : null;
    const emitLogEvent = (event) => {
        if (event.event.type === 'usage') {
            recordTokenUsage(usageByModel, event.modelId, event.event.usage);
        }
        executionLog?.recordEvent(event);
        options.onEvent?.(event);
    };
    try {
        const result = await run(target.runPath, args, {
            configRoot: workspaceRoot,
            headless: true,
            stream: true,
            sandbox: options.sandbox,
            signal: options.signal,
            toolAbortSignalRef: options.toolAbortSignalRef,
            onUserInput: options.onUserInput,
            onEvent: (event) => emitLogEvent({ scope: 'child', childSlug: target.childSlug, modelId, event }),
            onChildEvent: (childSlug, event) => emitLogEvent({ scope: 'child', childSlug, modelId, event }),
        });
        if (options.sessionId) {
            await appendConductorSessionMessages(workspaceRoot, options.sessionId, buildPersistedRunnableTranscript(target.inputText, result));
            await mergeSessionUsage(workspaceRoot, options.sessionId, usageByModel);
        }
        await executionLog?.finish({
            status: result.error ? 'error' : 'success',
            answer: result.answer,
            error: result.error,
            outputCount: result.output.length,
        });
        return result;
    }
    catch (error) {
        if (options.sessionId) {
            await appendConductorSessionMessages(workspaceRoot, options.sessionId, buildPersistedRunnableTranscript(target.inputText, {
                output: [],
                error: error.message,
            }));
            await mergeSessionUsage(workspaceRoot, options.sessionId, usageByModel);
        }
        await executionLog?.finish({
            status: 'error',
            error: error.message,
        });
        throw error;
    }
}
async function resolveWorkspaceDmlPath(workspaceRoot, fileSpec, options) {
    const trimmed = fileSpec.trim();
    if (!trimmed) {
        return null;
    }
    const candidates = options.mode === 'plans-only'
        ? buildPlanOnlyCandidates(workspaceRoot, trimmed)
        : buildDirectDmlCandidates(workspaceRoot, trimmed);
    for (const candidate of candidates) {
        if (await fileExists(candidate)) {
            return candidate;
        }
    }
    return null;
}
function buildDirectDmlCandidates(workspaceRoot, fileSpec) {
    const trimmed = fileSpec.trim();
    const candidates = new Set();
    const directPath = path.resolve(workspaceRoot, trimmed);
    candidates.add(directPath);
    if (!trimmed.endsWith('.dml')) {
        candidates.add(path.resolve(workspaceRoot, `${trimmed}.dml`));
    }
    const normalized = trimLeadingPathSeparators(trimmed).replace(/\\/g, '/');
    const planRelative = normalized.startsWith('plans/') ? normalized : path.posix.join('plans', normalized);
    candidates.add(path.resolve(workspaceRoot, planRelative));
    if (!planRelative.endsWith('.dml')) {
        candidates.add(path.resolve(workspaceRoot, `${planRelative}.dml`));
    }
    return Array.from(candidates);
}
function buildPlanOnlyCandidates(workspaceRoot, fileSpec) {
    const normalized = trimLeadingPathSeparators(fileSpec.trim()).replace(/\\/g, '/');
    const planRelative = normalized.startsWith('plans/') ? normalized : path.posix.join('plans', normalized);
    const plansRoot = path.resolve(workspaceRoot, 'plans');
    const candidates = [
        path.resolve(workspaceRoot, planRelative),
        ...(planRelative.endsWith('.dml') ? [] : [path.resolve(workspaceRoot, `${planRelative}.dml`)]),
    ];
    return candidates.filter((candidate, index, all) => {
        if (all.indexOf(candidate) !== index) {
            return false;
        }
        return candidate === plansRoot || candidate.startsWith(`${plansRoot}${path.sep}`);
    });
}
function trimLeadingPathSeparators(value) {
    return value.replace(/^[/\\]+/, '');
}
async function fileExists(filePath) {
    try {
        await fs.access(filePath);
        return true;
    }
    catch {
        return false;
    }
}
export async function runSkillCreatorCommand(workspaceRoot, sessionId, spec, options = {}) {
    const trimmedSpec = spec.trim();
    if (!trimmedSpec) {
        throw new Error('spec is required');
    }
    const config = await loadConfig(workspaceRoot);
    const compileSelection = resolveModelSlot(config, 'compile');
    const runSelection = resolveModelSlot(config, 'run');
    const executionLog = createSessionExecutionLogWriter({
        workspaceRoot,
        sessionId,
        executionKind: 'skill-creator',
        inputText: trimmedSpec,
        skillName: 'skill-creator',
    });
    const emitLogEvent = (event) => {
        executionLog.recordEvent(event);
        options.onEvent?.(event);
    };
    try {
        const result = await createLocalSkill({
            spec: trimmedSpec,
            workspaceRoot,
            workspacePath: path.resolve(workspaceRoot, config.workspace || './workspace'),
            config,
            compileSelection,
            runSelection,
            sessionId,
            sandbox: options.sandbox ?? false,
            signal: options.signal,
            onUserInput: options.onUserInput ?? (async () => ''),
            onEvent: emitLogEvent,
        });
        await executionLog.finish({ status: 'success' });
        return result;
    }
    catch (error) {
        await executionLog.finish({
            status: 'error',
            error: error.message,
        });
        throw error;
    }
}
async function runShellCommand(workspaceRoot, command, options = {}) {
    const trimmedCommand = command.trim();
    if (!trimmedCommand) {
        throw new Error('command is required');
    }
    const toolName = 'shell';
    const toolArgs = { command: trimmedCommand };
    const emit = (event) => options.onEvent?.({ scope: 'main', event });
    emit(buildToolStartEvent(toolName, toolArgs));
    let shellManager = null;
    try {
        const config = await loadConfig(workspaceRoot);
        const workspacePath = path.resolve(workspaceRoot, config.workspace || './workspace');
        shellManager = createShellManager({
            workspacePath,
            sandbox: options.sandbox,
            hostConfig: config.shell,
        });
        const observer = createShellToolEventBridge({
            toolName,
            toolArgs,
            emit,
        });
        const result = await shellManager.exec(trimmedCommand, options.signal, observer);
        emit(buildToolCompletionEvent(toolName, toolArgs, result));
        return result;
    }
    catch (error) {
        emit(buildToolFailureEvent(toolName, toolArgs, error));
        throw error;
    }
    finally {
        await shellManager?.dispose();
    }
}
export function formatSkillCreatorSummary(workspaceRoot, result) {
    const slug = typeof result.slug === 'string' ? result.slug : 'unknown-skill';
    const outputPath = typeof result.output_path === 'string' ? result.output_path : '';
    if (!outputPath) {
        return `Published /${slug}.`;
    }
    const relativeOutputPath = path.relative(workspaceRoot, outputPath);
    return `Published /${slug} to ${relativeOutputPath || outputPath}.`;
}
function formatShellCommandSummary(result) {
    if (result.success) {
        return result.summary || 'Shell command completed successfully.';
    }
    return result.summary || `Shell command failed with exit code ${result.exitCode}.`;
}
function buildPersistedRunnableTranscript(inputText, result) {
    const transcript = [];
    const trimmedInput = inputText.trim();
    if (trimmedInput) {
        transcript.push({ role: 'user', content: trimmedInput });
    }
    const assistantContent = summarizeRunResult(result).trim();
    if (assistantContent) {
        transcript.push({ role: 'assistant', content: assistantContent });
    }
    return transcript;
}
function buildPersistedShellTranscript(command, result) {
    const sections = [
        '[shell result]',
        `Command: ${command}`,
        `Summary: ${formatShellCommandSummary(result)}`,
        `Backend: ${result.backendLabel ?? result.backend}`,
        `Exit code: ${result.exitCode}`,
    ];
    const stdout = truncateShellTranscriptStream(result.stdout, 'stdout');
    const stderr = truncateShellTranscriptStream(result.stderr, 'stderr');
    if (stdout) {
        sections.push(`stdout:\n${stdout}`);
    }
    if (stderr) {
        sections.push(`stderr:\n${stderr}`);
    }
    if (!stdout && !stderr) {
        sections.push('No stdout or stderr output.');
    }
    return [
        { role: 'user', content: `[shell command]\n${command}` },
        { role: 'assistant', content: sections.join('\n\n') },
    ];
}
function truncateShellTranscriptStream(content, label) {
    const trimmed = content.trim();
    if (!trimmed) {
        return '';
    }
    if (trimmed.length <= MAX_PERSISTED_SHELL_STREAM_CHARS) {
        return trimmed;
    }
    const omitted = trimmed.length - MAX_PERSISTED_SHELL_STREAM_CHARS;
    return `${trimmed.slice(0, MAX_PERSISTED_SHELL_STREAM_CHARS).trimEnd()}\n\n[${label} truncated: ${omitted} more chars omitted]`;
}
function isAbortError(error) {
    return error instanceof Error && error.name === 'AbortError';
}
export function previewMessageFromEvent(logEvent) {
    switch (logEvent.event.type) {
        case 'output':
            if (!logEvent.event.content) {
                return null;
            }
            return {
                role: 'system',
                kind: 'output',
                tag: logEvent.scope === 'child' ? logEvent.childSlug : undefined,
                content: logEvent.event.content,
            };
        case 'input_required':
            if (!logEvent.event.prompt) {
                return null;
            }
            return previewQuestionMessage(logEvent.event.prompt, logEvent.scope === 'child' ? logEvent.childSlug : undefined);
        case 'memory_compaction':
            return {
                role: 'system',
                kind: 'output',
                tag: logEvent.scope === 'child' ? logEvent.childSlug : undefined,
                content: logEvent.event.content
                    ? `Memory compaction: ${logEvent.event.content}`
                    : 'Memory compaction triggered.',
            };
        default:
            return null;
    }
}
export function previewChildSkillActivityMessage(childSlug) {
    return {
        role: 'system',
        kind: 'output',
        tag: childSlug,
        content: 'Running child skill...',
    };
}
export function previewQuestionMessage(promptText, explicitTag) {
    const parsed = explicitTag ? { tag: explicitTag, content: promptText } : parseTaggedPrompt(promptText);
    return {
        role: 'system',
        kind: 'question',
        tag: parsed.tag,
        content: parsed.content,
    };
}
export function parseCommandArgs(rawArgs) {
    const args = [];
    let current = '';
    let quote = null;
    let escaping = false;
    for (const char of rawArgs.trim()) {
        if (escaping) {
            current += char;
            escaping = false;
            continue;
        }
        if (char === '\\') {
            escaping = true;
            continue;
        }
        if (quote) {
            if (char === quote) {
                quote = null;
            }
            else {
                current += char;
            }
            continue;
        }
        if (char === '"' || char === '\'') {
            quote = char;
            continue;
        }
        if (/\s/.test(char)) {
            if (current) {
                args.push(current);
                current = '';
            }
            continue;
        }
        current += char;
    }
    if (escaping) {
        current += '\\';
    }
    if (current) {
        args.push(current);
    }
    return args;
}
export function parseCommandBarInput(line) {
    const trimmedStart = line.trimStart();
    if (trimmedStart.startsWith('\\!')) {
        return { kind: 'text', prompt: trimmedStart.slice(1).trim() };
    }
    if (trimmedStart.startsWith('!!')) {
        return { kind: 'shell', command: trimmedStart.slice(2).trim(), persistOutput: true };
    }
    if (trimmedStart.startsWith('!')) {
        return { kind: 'shell', command: trimmedStart.slice(1).trim(), persistOutput: false };
    }
    const trimmed = line.trim();
    if (!trimmed.startsWith('/')) {
        return { kind: 'text', prompt: trimmed };
    }
    if (trimmed === '/') {
        return { kind: 'builtin', name: 'help', rawArgs: '', args: [] };
    }
    const match = trimmed.slice(1).match(/^(\S+)(?:\s+(.*))?$/);
    if (!match) {
        return { kind: 'builtin', name: 'help', rawArgs: '', args: [] };
    }
    const name = match[1];
    const rawArgs = (match[2] ?? '').trim();
    if (BUILTIN_SLASH_COMMANDS.includes(name)) {
        const passthroughRawArgBuiltins = new Set(['new', 'compile', 'skill-creator']);
        return {
            kind: 'builtin',
            name: name,
            rawArgs,
            args: passthroughRawArgBuiltins.has(name) && rawArgs
                ? [rawArgs]
                : parseCommandArgs(rawArgs),
        };
    }
    return {
        kind: 'skill',
        name,
        rawArgs,
        args: parseCommandArgs(rawArgs),
    };
}
export function parseSlashInput(line) {
    return parseCommandBarInput(line);
}
export function canSubmitParsedInputWhileBusy(parsed) {
    return parsed.kind === 'builtin' && (parsed.name === 'cancel' || parsed.name === 'exit' || parsed.name === 'quit');
}
export function reconcileEphemeralMessages(persistedMessages, ephemeralMessages) {
    if (ephemeralMessages.length === 0) {
        return [];
    }
    const persistedCounts = new Map();
    for (const message of persistedMessages) {
        const key = `${message.role}\u0000${message.content}`;
        persistedCounts.set(key, (persistedCounts.get(key) ?? 0) + 1);
    }
    const remaining = [];
    for (const message of ephemeralMessages) {
        if (message.pending || message.role === 'system') {
            remaining.push(message);
            continue;
        }
        const key = `${message.role}\u0000${message.content}`;
        const count = persistedCounts.get(key) ?? 0;
        if (count > 0) {
            persistedCounts.set(key, count - 1);
            continue;
        }
        remaining.push(message);
    }
    return remaining;
}
export function sessionMessagesContainCompletedTaskPreview(persistedMessages, previewEntries) {
    const previewUser = [...previewEntries].reverse().find((entry) => entry.role === 'user' && entry.content);
    const previewAssistant = [...previewEntries].reverse().find((entry) => entry.role === 'assistant' && !entry.pending && entry.content);
    if (!previewUser || !previewAssistant) {
        return false;
    }
    for (let index = persistedMessages.length - 1; index >= 1; index -= 1) {
        const assistant = persistedMessages[index];
        const user = persistedMessages[index - 1];
        if (user?.role === 'user'
            && assistant?.role === 'assistant'
            && user.content === previewUser.content
            && assistant.content === previewAssistant.content) {
            return true;
        }
    }
    return false;
}
export function computeFramePatch(previousLines, nextLines, previousSize, nextSize) {
    if (!previousLines
        || !previousSize
        || previousSize.columns !== nextSize.columns
        || previousSize.rows !== nextSize.rows
        || previousLines.length !== nextLines.length) {
        return { fullRender: true, changedRows: [] };
    }
    const changedRows = [];
    for (let index = 0; index < nextLines.length; index += 1) {
        if (previousLines[index] !== nextLines[index]) {
            changedRows.push({ row: index + 1, line: nextLines[index] });
        }
    }
    return { fullRender: false, changedRows };
}
export function completeSlashCommand(inputValue, candidates) {
    if (!inputValue.startsWith('/')) {
        return { value: inputValue, matches: [], applied: false };
    }
    const match = inputValue.match(/^\/([^\s]*)(.*)$/s);
    if (!match) {
        return { value: inputValue, matches: [], applied: false };
    }
    const prefix = match[1];
    const suffix = match[2] ?? '';
    const uniqueCandidates = Array.from(new Set(candidates)).sort();
    const matches = uniqueCandidates.filter((candidate) => candidate.startsWith(prefix));
    if (matches.length === 0) {
        return { value: inputValue, matches: [], applied: false };
    }
    if (matches.length === 1) {
        return {
            value: `/${matches[0]}${suffix.length > 0 ? suffix : ' '}`,
            matches,
            applied: true,
        };
    }
    const sharedPrefix = longestCommonPrefix(matches);
    if (sharedPrefix.length > prefix.length) {
        return {
            value: `/${sharedPrefix}${suffix}`,
            matches,
            applied: true,
        };
    }
    return { value: inputValue, matches, applied: false };
}
async function chooseSession(rl, workspaceRoot) {
    const sessions = await listConductorSessions(workspaceRoot);
    if (sessions.length === 0) {
        return createSessionInteractive(rl, workspaceRoot);
    }
    console.log(divider('='));
    console.log(paint('Sessions', ANSI.bold));
    sessions.slice(0, 9).forEach((session, index) => {
        console.log(`${paint(String(index + 1).padStart(2, ' '), ANSI.cyan)}  ${session.title} ${paint(session.updatedAt.replace('T', ' ').slice(0, 16), ANSI.dim)}`);
    });
    console.log(`${paint(' 0', ANSI.cyan)}  Start a new session`);
    console.log(divider('='));
    while (true) {
        const answer = (await rl.question(paint('Select a session [0]: ', ANSI.dim))).trim();
        if (!answer || answer === '0') {
            return createSessionInteractive(rl, workspaceRoot);
        }
        const numeric = Number.parseInt(answer, 10);
        if (!Number.isNaN(numeric) && numeric >= 1 && numeric <= Math.min(sessions.length, 9)) {
            return sessions[numeric - 1];
        }
        console.log('Enter a listed session number or 0 for a new session.');
    }
}
async function createSessionInteractive(rl, workspaceRoot) {
    const title = (await rl.question(paint('New session title (optional): ', ANSI.dim))).trim();
    return createConductorSession(workspaceRoot, title || undefined);
}
function renderHeader(session) {
    console.log(divider('='));
    console.log(`${paint('DeepClause', ANSI.bold, ANSI.cyan)} ${paint('interactive conductor', ANSI.dim)}`);
    console.log(`${paint('commands', ANSI.dim)} /compile <spec>  /set-model <model>  /new  /sessions  /help  /exit  /skill args`);
    console.log(`${paint('session', ANSI.dim)}  ${paint(session.id, ANSI.cyan)}`);
    console.log(divider('='));
    console.log('');
}
export function parseSetModelCommandArgs(rawArgs) {
    const args = parseCommandArgs(rawArgs);
    let model;
    let slot;
    for (let index = 0; index < args.length; index += 1) {
        const arg = args[index];
        if (arg === '--slot') {
            const candidate = args[index + 1];
            if (!candidate) {
                throw new Error('Provide a slot after --slot.');
            }
            if (!isModelSlot(candidate)) {
                throw new Error('Slot must be one of: gateway, run, compile.');
            }
            if (slot) {
                throw new Error('Provide --slot only once.');
            }
            slot = candidate;
            index += 1;
            continue;
        }
        if (arg.startsWith('--slot=')) {
            const candidate = arg.slice('--slot='.length);
            if (!isModelSlot(candidate)) {
                throw new Error('Slot must be one of: gateway, run, compile.');
            }
            if (slot) {
                throw new Error('Provide --slot only once.');
            }
            slot = candidate;
            continue;
        }
        if (arg.startsWith('--')) {
            throw new Error(`Unknown option: ${arg}`);
        }
        if (model) {
            throw new Error('Provide exactly one model for /set-model.');
        }
        model = arg;
    }
    if (!model) {
        throw new Error('Provide a model after /set-model.');
    }
    return { model, slot };
}
function streamLabel(logEvent) {
    return logEvent.scope === 'child'
        ? `${CHILD_EVENT_INDENT}[${logEvent.childSlug ?? '?'}] llm`
        : 'llm';
}
function formatEventPrefix(logEvent) {
    return logEvent.scope === 'child'
        ? `${CHILD_EVENT_INDENT}[${logEvent.childSlug ?? '?'}] `
        : '';
}
function toolScopeKey(logEvent) {
    return logEvent.scope === 'child'
        ? `child:${logEvent.childSlug ?? '?'}`
        : 'main';
}
function toolScopeLabel(logEvent) {
    return logEvent.scope === 'child'
        ? logEvent.childSlug ?? '?'
        : 'main';
}
function parseTaggedPrompt(promptText) {
    const match = promptText.match(/^\[([^\]]+)\]\s*(.*)$/);
    if (!match) {
        return { content: promptText };
    }
    return {
        tag: match[1],
        content: match[2] || promptText,
    };
}
function sameDisplayMessage(left, right) {
    return !!left
        && left.role === right.role
        && left.kind === right.kind
        && left.tag === right.tag
        && left.content === right.content;
}
function formatSystemMessageHeader(entry, spinner) {
    const base = entry.kind === 'output'
        ? 'Output'
        : entry.kind === 'question'
            ? 'Question'
            : 'System';
    return entry.tag
        ? `[${base}: ${entry.tag}${spinner}]`
        : `[${base}${spinner}]`;
}
export function formatDisplayMessageHeader(entry, spinner = '') {
    switch (entry.role) {
        case 'user':
            return '[You]';
        case 'assistant': {
            const base = entry.error
                ? 'Assistant Error'
                : entry.pending
                    ? 'Thinking'
                    : 'Assistant';
            return entry.tag
                ? `[${base}: ${entry.tag}${spinner}]`
                : `[${base}${spinner}]`;
        }
        case 'system':
            return formatSystemMessageHeader(entry, spinner);
    }
}
export function formatDisplayMessageBodyLines(entry) {
    const bodyLines = entry.content
        ? entry.content.split(/\r?\n/)
        : (entry.pending ? [''] : ['(empty)']);
    if (!entry.pending) {
        return bodyLines;
    }
    if (!entry.content) {
        return ['thinking> generating intermediate output...'];
    }
    return bodyLines.map((line) => line ? `thinking> ${line}` : 'thinking>');
}
function formatSystemAssetSourcePath(workspaceRoot, filePath) {
    const relativePath = path.relative(workspaceRoot, filePath);
    if (relativePath && !relativePath.startsWith('..') && !path.isAbsolute(relativePath)) {
        return relativePath || '.';
    }
    return filePath;
}
function formatToolEventLine(logEvent) {
    const { event } = logEvent;
    if (!event.toolName || IGNORED_LIVE_LOG_TOOLS.has(event.toolName)) {
        return null;
    }
    const prefix = formatEventPrefix(logEvent);
    const callLabel = `${event.toolName}(${formatToolArgs(event.toolArgs, 80)})`;
    if (!event.toolState) {
        return `${prefix}tool ${callLabel}`;
    }
    return `${prefix}tool ${callLabel} ${event.toolState}${formatToolEventDetails(event)}`;
}
function formatMemoryCompactionLine(logEvent) {
    const { event } = logEvent;
    const prefix = formatEventPrefix(logEvent);
    if (event.content) {
        return `${prefix}${event.content}`;
    }
    const action = event.compactionAction ?? 'skipped';
    const binding = event.compactionBindingName ?? 'compactor';
    const before = typeof event.beforeTokens === 'number' ? event.beforeTokens : '?';
    const after = typeof event.afterTokens === 'number' ? event.afterTokens : '?';
    return `${prefix}compact ${event.compactionScope ?? 'loop'}.${event.compactionTrigger ?? 'before_model_call'} ${action} via ${binding} ${before} -> ${after} tokens`;
}
function formatToolEventDetails(event) {
    const details = [];
    if (typeof event.toolPid === 'number') {
        details.push(`pid=${event.toolPid}`);
    }
    else if (event.toolBackend === 'sandbox') {
        details.push('sandbox');
    }
    if (typeof event.toolExitCode === 'number') {
        details.push(`exit=${event.toolExitCode}`);
    }
    const message = event.toolError
        ?? (event.toolSummary && event.toolSummary !== 'Command completed successfully' ? event.toolSummary : undefined);
    if (message) {
        details.push(message);
    }
    return details.length > 0 ? ` ${details.join(' ')}` : '';
}
function formatActiveToolStatus(entry) {
    const details = [];
    if (typeof entry.toolPid === 'number') {
        details.push(`pid=${entry.toolPid}`);
    }
    else if (entry.toolBackend === 'sandbox') {
        details.push('sandbox');
    }
    const detailSuffix = details.length > 0 ? ` ${details.join(' ')}` : '';
    return `${entry.scopeLabel} ${entry.toolName} ${entry.toolState}${detailSuffix}`;
}
function formatShellModeStatus(workspaceRoot, shellWorkspacePath, sandbox, shellConfig = { wrapper: 'auto', strictIsolation: false }) {
    const displayPath = path.relative(workspaceRoot, shellWorkspacePath) || '.';
    const shellBackend = describeShellExecutionBackend(sandbox ?? false, shellConfig);
    return `Shell mode: ${shellBackend.description} (${shellBackend.backendLabel}) in ${displayPath}. Enter runs, /cancel stops.`;
}
function buildHelpLines() {
    return [
        'commands',
        '/new [title]    create a new conductor session',
        '/sessions       refresh or choose sessions',
        '/run <file> [args] run a workspace DML file directly',
        '/compile <spec> compile and publish a new local skill',
        '/skill-creator <spec> alias for /compile',
        '/set-model <model> [--slot <slot>] update the configured model selection',
        '/cancel         abort the current execution',
        '/help           show this help',
        '/quit           exit the TUI',
        '/<skill> [args] run a compiled skill directly',
        '/<plan> [args]  run plans/<plan>.dml when no compiled skill matches',
        '!<command>     run a shell command directly in the workspace shell',
        '!!<command>    run a shell command and add its result to the session transcript',
        'keys',
        'F10 / Ctrl+G    open the menu bar',
        'Ctrl+P          open the action palette',
        'Left/Right      change focused pane',
        'Up/Down         select session or scroll focused pane',
        'PgUp/PgDn       page-scroll the focused pane',
        'End             jump to bottom or re-enable follow mode',
        'Tab             autocomplete /commands and /skills',
    ];
}
function getSlashHelpLines() {
    const lines = buildHelpLines();
    const keysIndex = lines.indexOf('keys');
    return keysIndex === -1 ? lines : lines.slice(0, keysIndex);
}
function getShortcutHelpLines() {
    const lines = buildHelpLines();
    const keysIndex = lines.indexOf('keys');
    return keysIndex === -1 ? [] : lines.slice(keysIndex);
}
function isModelSlot(value) {
    return MODEL_SLOTS.includes(value);
}
function computePaneWidths(columns) {
    const available = Math.max(36, columns - 2);
    let left = Math.max(18, Math.floor(available * 0.2));
    let right = Math.max(28, Math.floor(available * 0.3));
    let center = available - left - right;
    if (center < 30) {
        const needed = 30 - center;
        const reduceRight = Math.min(needed, Math.max(0, right - 18));
        right -= reduceRight;
        center += reduceRight;
        if (center < 30) {
            const reduceLeft = Math.min(30 - center, Math.max(0, left - 14));
            left -= reduceLeft;
            center += reduceLeft;
        }
    }
    center += available - left - center - right;
    return { left, center, right };
}
function computeRightPaneHeights(totalHeight) {
    if (totalHeight <= 12) {
        return { process: Math.max(1, totalHeight - 2), tasks: 1, context: 1 };
    }
    let process = Math.max(6, Math.floor(totalHeight * 0.40));
    let tasks = Math.max(4, Math.floor(totalHeight * 0.25));
    let context = totalHeight - process - tasks;
    if (context < 4) {
        const deficit = 4 - context;
        process = Math.max(4, process - deficit);
        context = totalHeight - process - tasks;
    }
    if (context < 4) {
        const deficit = 4 - context;
        tasks = Math.max(3, tasks - deficit);
        context = totalHeight - process - tasks;
    }
    return { process, tasks, context };
}
function buildPane(title, body, width, height, kind, active = false) {
    if (height <= 0) {
        return [];
    }
    const innerWidth = Math.max(1, width - 2);
    const lines = [renderWindowTop(title, innerWidth, active)];
    bodyLoop: for (const line of body) {
        for (const wrapped of wrapPlainText(line, innerWidth)) {
            if (lines.length >= height - 1) {
                break bodyLoop;
            }
            lines.push(renderWindowBody(wrapped, innerWidth, kind));
        }
    }
    while (lines.length < height - 1) {
        lines.push(renderWindowBody('', innerWidth, kind));
    }
    if (height >= 2) {
        lines.push(renderWindowBottom(innerWidth, active));
    }
    return lines.slice(0, height);
}
export function wrapPlainText(text, width) {
    if (width <= 0) {
        return [''];
    }
    const result = [];
    for (const rawLine of text.split(/\r?\n/)) {
        const line = expandTabs(rawLine || '', CHILD_EVENT_TAB_WIDTH);
        if (!line) {
            result.push('');
            continue;
        }
        let remaining = line;
        while (measureDisplayWidth(remaining) > width) {
            const { head: slice, tail } = splitByDisplayWidth(remaining, width, { forceOne: true });
            const breakIndex = slice.lastIndexOf(' ');
            const breakSlice = breakIndex === -1 ? '' : slice.slice(0, breakIndex);
            if (breakIndex > 0 && measureDisplayWidth(breakSlice) > Math.floor(width * 0.4)) {
                result.push(breakSlice);
                remaining = remaining.slice(breakIndex + 1);
            }
            else {
                result.push(slice);
                remaining = tail;
            }
        }
        result.push(remaining);
    }
    return result;
}
function expandTabs(text, tabWidth) {
    if (!text.includes('\t')) {
        return text;
    }
    return text.replace(/\t/g, ' '.repeat(tabWidth));
}
export function measureDisplayWidth(text) {
    return splitGraphemes(text).reduce((total, grapheme) => total + graphemeDisplayWidth(grapheme), 0);
}
function displayColumnToStringIndex(text, displayCol) {
    let col = 0;
    let i = 0;
    const len = text.length;
    while (i < len) {
        if (text.charCodeAt(i) === 0x1b && i + 1 < len && text[i + 1] === '[') {
            const end = text.indexOf('m', i + 2);
            if (end !== -1) {
                i = end + 1;
                continue;
            }
        }
        if (col >= displayCol)
            return i;
        const code = text.codePointAt(i);
        const charWidth = code >= 0x1100 && (code <= 0x115f || code === 0x2329 || code === 0x232a ||
            (code >= 0x2e80 && code <= 0xa4cf && code !== 0x303f) ||
            (code >= 0xac00 && code <= 0xd7a3) ||
            (code >= 0xf900 && code <= 0xfaff) ||
            (code >= 0xfe10 && code <= 0xfe19) ||
            (code >= 0xfe30 && code <= 0xfe6f) ||
            (code >= 0xff01 && code <= 0xff60) ||
            (code >= 0xffe0 && code <= 0xffe6) ||
            (code >= 0x1f300 && code <= 0x1f9ff) ||
            (code >= 0x20000 && code <= 0x2fffd) ||
            (code >= 0x30000 && code <= 0x3fffd)) ? 2 : 1;
        col += charWidth;
        i += code > 0xffff ? 2 : 1;
    }
    return i;
}
export function padRight(text, width) {
    const visibleWidth = measureDisplayWidth(text);
    return visibleWidth >= width ? text : `${text}${' '.repeat(width - visibleWidth)}`;
}
export function ellipsize(text, width) {
    if (measureDisplayWidth(text) <= width) {
        return text;
    }
    if (width <= 3) {
        return splitByDisplayWidth(text, width).head;
    }
    return `${splitByDisplayWidth(text, width - 3).head}...`;
}
function splitGraphemes(text) {
    if (!text) {
        return [];
    }
    if (!graphemeSegmenter) {
        return Array.from(text);
    }
    return Array.from(graphemeSegmenter.segment(text), (entry) => entry.segment);
}
function splitByDisplayWidth(text, width, options = {}) {
    if (width <= 0) {
        return { head: '', tail: text };
    }
    const graphemes = splitGraphemes(text);
    let consumedWidth = 0;
    let index = 0;
    while (index < graphemes.length) {
        const nextWidth = graphemeDisplayWidth(graphemes[index]);
        if (consumedWidth + nextWidth > width) {
            if (index === 0 && options.forceOne) {
                index = 1;
            }
            break;
        }
        consumedWidth += nextWidth;
        index += 1;
    }
    return {
        head: graphemes.slice(0, index).join(''),
        tail: graphemes.slice(index).join(''),
    };
}
function graphemeDisplayWidth(grapheme) {
    if (!grapheme) {
        return 0;
    }
    if (isEmojiGrapheme(grapheme)) {
        return 2;
    }
    let width = 0;
    for (const symbol of Array.from(grapheme)) {
        const codePoint = symbol.codePointAt(0);
        if (codePoint === undefined || isZeroWidthCodePoint(codePoint, symbol)) {
            continue;
        }
        width = Math.max(width, isFullWidthCodePoint(codePoint) ? 2 : 1);
    }
    return width;
}
function isEmojiGrapheme(grapheme) {
    if (EXTENDED_PICTOGRAPHIC_RE.test(grapheme) || grapheme.includes('\u200D') || grapheme.includes('\uFE0F') || grapheme.includes('\u20E3')) {
        return true;
    }
    return Array.from(grapheme).some((symbol) => {
        const codePoint = symbol.codePointAt(0);
        return codePoint !== undefined && isRegionalIndicatorCodePoint(codePoint);
    });
}
function isZeroWidthCodePoint(codePoint, symbol) {
    return COMBINING_MARK_RE.test(symbol)
        || codePoint === 0x200D
        || (codePoint >= 0xFE00 && codePoint <= 0xFE0F)
        || (codePoint >= 0xE0100 && codePoint <= 0xE01EF)
        || (codePoint >= 0x1F3FB && codePoint <= 0x1F3FF);
}
function isRegionalIndicatorCodePoint(codePoint) {
    return codePoint >= 0x1F1E6 && codePoint <= 0x1F1FF;
}
function isFullWidthCodePoint(codePoint) {
    if (codePoint < 0x1100) {
        return false;
    }
    return codePoint <= 0x115F
        || codePoint === 0x2329
        || codePoint === 0x232A
        || (codePoint >= 0x2E80 && codePoint <= 0x3247 && codePoint !== 0x303F)
        || (codePoint >= 0x3250 && codePoint <= 0x4DBF)
        || (codePoint >= 0x4E00 && codePoint <= 0xA4C6)
        || (codePoint >= 0xA960 && codePoint <= 0xA97C)
        || (codePoint >= 0xAC00 && codePoint <= 0xD7A3)
        || (codePoint >= 0xF900 && codePoint <= 0xFAFF)
        || (codePoint >= 0xFE10 && codePoint <= 0xFE19)
        || (codePoint >= 0xFE30 && codePoint <= 0xFE6B)
        || (codePoint >= 0xFF01 && codePoint <= 0xFF60)
        || (codePoint >= 0xFFE0 && codePoint <= 0xFFE6)
        || (codePoint >= 0x1F200 && codePoint <= 0x1F251)
        || (codePoint >= 0x20000 && codePoint <= 0x3FFFD);
}
function condenseWhitespace(text) {
    return text.replace(/\s+/g, ' ').trim();
}
function formatTimestamp(value) {
    return value.replace('T', ' ').slice(0, 16);
}
function summarizeContextSize(detail) {
    const transcript = detail.messages.map((message) => `${message.role}: ${message.content}`).join('\n');
    const taskMemory = detail.taskMemory ?? '';
    const assistantMemory = detail.assistantMemory ?? '';
    const messageBytes = Buffer.byteLength(transcript, 'utf8');
    const taskMemoryBytes = Buffer.byteLength(taskMemory, 'utf8');
    const assistantMemoryBytes = Buffer.byteLength(assistantMemory, 'utf8');
    const messageTokens = estimateTokenCount(transcript);
    const taskMemoryTokens = estimateTokenCount(taskMemory);
    const assistantMemoryTokens = estimateTokenCount(assistantMemory);
    return {
        messageCount: detail.messages.length,
        messageBytes,
        messageTokens,
        taskMemoryBytes,
        taskMemoryTokens,
        assistantMemoryBytes,
        assistantMemoryTokens,
        totalBytes: messageBytes + taskMemoryBytes + assistantMemoryBytes,
        totalTokens: messageTokens + taskMemoryTokens + assistantMemoryTokens,
    };
}
function estimateTokenCount(text) {
    const normalized = text.trim();
    if (!normalized) {
        return 0;
    }
    return Math.max(1, Math.ceil(normalized.length / 4));
}
function formatByteSize(bytes) {
    if (bytes < 1024) {
        return `${bytes} B`;
    }
    if (bytes < 1024 * 1024) {
        const kib = bytes / 1024;
        return `${kib >= 10 ? kib.toFixed(0) : kib.toFixed(1)} KB`;
    }
    const mib = bytes / (1024 * 1024);
    return `${mib.toFixed(1)} MB`;
}
function formatInteger(value) {
    return new Intl.NumberFormat('en-US').format(value);
}
function longestCommonPrefix(values) {
    if (values.length === 0) {
        return '';
    }
    let prefix = values[0];
    for (const value of values.slice(1)) {
        while (!value.startsWith(prefix) && prefix) {
            prefix = prefix.slice(0, -1);
        }
        if (!prefix) {
            break;
        }
    }
    return prefix;
}
function renderMenuBar(text, columns, activeMenuIndex) {
    const labels = ['Session', 'Skills', 'Files', 'Run', 'View', 'Help'];
    let rendered = paint(' DeepClause ', ANSI.black, ANSI.bgWhite, ANSI.bold);
    let usedWidth = measureDisplayWidth(' DeepClause ');
    labels.forEach((label, index) => {
        const raw = ` [${index + 1}] ${label} `;
        usedWidth += measureDisplayWidth(raw);
        rendered += paint(raw, ANSI.black, activeMenuIndex === index ? ANSI.bgCyan : ANSI.bgWhite, ANSI.bold);
    });
    const trailingWidth = Math.max(0, columns - usedWidth);
    rendered += paint(padRight(ellipsize(` ${text}`, trailingWidth), trailingWidth), ANSI.black, ANSI.bgWhite);
    return rendered;
}
function renderStatusBar(status, columns, hotkeys) {
    let hotkeyWidth = 0;
    const selected = [];
    for (const hotkey of hotkeys) {
        const width = measureDisplayWidth(hotkey[0]) + measureDisplayWidth(hotkey[1]) + 4;
        if (hotkeyWidth + width > Math.max(0, columns - 12)) {
            break;
        }
        hotkeyWidth += width;
        selected.push(hotkey);
    }
    const leftWidth = Math.max(0, columns - hotkeyWidth);
    const statusPart = paint(padRight(ellipsize(status, leftWidth), leftWidth), ANSI.black, ANSI.bgCyan);
    const hotkeyPart = selected.map(([key, label]) => (`${paint(` ${key} `, ANSI.black, ANSI.bgWhite, ANSI.bold)}${paint(` ${label} `, ANSI.black, ANSI.bgCyan)}`)).join('');
    return `${statusPart}${hotkeyPart}`;
}
function renderCommandBar(prefix, value, columns) {
    const prefixPart = paint(prefix, ANSI.black, ANSI.bgWhite, ANSI.bold);
    const remainingWidth = Math.max(0, columns - measureDisplayWidth(prefix));
    const valuePart = paint(padRight(ellipsize(value, remainingWidth), remainingWidth), ANSI.brightWhite, ANSI.bgBlue);
    return `${prefixPart}${valuePart}`;
}
function renderWindowTop(title, innerWidth, active) {
    const safeLabel = ` ${ellipsize(title, Math.max(1, innerWidth - 2))} `;
    const fillWidth = Math.max(0, innerWidth - measureDisplayWidth(safeLabel));
    const leftFill = Math.floor(fillWidth / 2);
    const rightFill = fillWidth - leftFill;
    return [
        paint(`╔${'═'.repeat(leftFill)}`, ANSI.brightCyan, ANSI.bgBlue),
        paint(safeLabel, ANSI.black, active ? ANSI.bgCyan : ANSI.bgWhite, ANSI.bold),
        paint(`${'═'.repeat(rightFill)}╗`, ANSI.brightCyan, ANSI.bgBlue),
    ].join('');
}
function renderWindowBottom(innerWidth, _active) {
    return paint(`╚${'═'.repeat(innerWidth)}╝`, ANSI.brightCyan, ANSI.bgBlue);
}
function renderWindowBody(text, innerWidth, kind) {
    const normalized = text.trim().toLowerCase();
    const content = padRight(ellipsize(text, innerWidth), innerWidth);
    const border = paint('║', ANSI.brightCyan, ANSI.bgBlue);
    if (kind === 'workspace' && text.startsWith('> ')) {
        return `${border}${paint(content, ANSI.black, ANSI.bgCyan, ANSI.bold)}${border}`;
    }
    if (kind === 'sessions' && text.startsWith('> ')) {
        return `${border}${paint(content, ANSI.black, ANSI.bgCyan, ANSI.bold)}${border}`;
    }
    if (kind === 'messages') {
        if (text.startsWith('[You]')) {
            return `${border}${paint(content, ANSI.brightCyan, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (text.startsWith('[Assistant Error]')) {
            return `${border}${paint(content, ANSI.red, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (text.startsWith('[Thinking')) {
            return `${border}${paint(content, ANSI.cyan, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (text.startsWith('[Assistant')) {
            return `${border}${paint(content, ANSI.brightYellow, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (text.startsWith('  thinking>')) {
            return `${border}${paint(content, ANSI.cyan, ANSI.bgBlue, ANSI.dim)}${border}`;
        }
        if (text.startsWith('[System')) {
            return `${border}${paint(content, ANSI.cyan, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
    }
    if (kind === 'process') {
        if (normalized.startsWith('error')) {
            return `${border}${paint(content, ANSI.red, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (normalized.startsWith('answer')) {
            return `${border}${paint(content, ANSI.brightYellow, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (normalized.startsWith('tool')) {
            return `${border}${paint(content, ANSI.brightCyan, ANSI.bgBlue)}${border}`;
        }
        if (normalized.startsWith('clarify')) {
            return `${border}${paint(content, ANSI.yellow, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (normalized.startsWith('task') || normalized.startsWith('skill')) {
            return `${border}${paint(content, ANSI.brightWhite, ANSI.bgBlue, ANSI.bold)}`;
        }
    }
    if (kind === 'tasks') {
        if (text.startsWith('>')) {
            return `${border}${paint(content, ANSI.brightCyan, ANSI.bgBlue, ANSI.bold)}${border}`;
        }
        if (text.startsWith('\u2713')) {
            return `${border}${paint(content, ANSI.green, ANSI.bgBlue)}${border}`;
        }
        if (text.startsWith('\u2717')) {
            return `${border}${paint(content, ANSI.red, ANSI.bgBlue)}${border}`;
        }
        return `${border}${paint(content, ANSI.white, ANSI.bgBlue)}${border}`;
    }
    if (normalized === 'task memory' ||
        normalized === 'assistant memory' ||
        normalized === 'recent messages' ||
        normalized === 'commands' ||
        normalized === 'keys') {
        return `${border}${paint(content, ANSI.brightYellow, ANSI.bgBlue, ANSI.bold)}${border}`;
    }
    if (kind === 'context' && (normalized.startsWith('created ') || normalized.startsWith('updated ') || normalized.startsWith('session '))) {
        return `${border}${paint(content, ANSI.cyan, ANSI.bgBlue)}${border}`;
    }
    if (kind === 'sessions' && text.startsWith('  ')) {
        return `${border}${paint(content, ANSI.cyan, ANSI.bgBlue)}${border}`;
    }
    return `${border}${paint(content, ANSI.white, ANSI.bgBlue)}${border}`;
}
function divider(character) {
    const glyph = character === '=' ? '═' : character === '-' ? '─' : character;
    return paint(glyph.repeat(72), ANSI.brightCyan, ANSI.bgBlue);
}
function paneDisplayName(kind) {
    switch (kind) {
        case 'sessions':
            return 'Sessions';
        case 'messages':
            return 'Messages';
        case 'process':
            return 'Execution';
        case 'tasks':
            return 'Tasks';
        case 'context':
            return 'Context';
    }
}
function paneDimensionsForKind(kind, columns, rows) {
    const widths = computePaneWidths(columns);
    const rightHeights = computeRightPaneHeights(Math.max(1, rows - 3));
    switch (kind) {
        case 'sessions':
            return { width: widths.left, height: Math.max(1, rows - 3) };
        case 'messages':
            return { width: widths.center, height: Math.max(1, rows - 3) };
        case 'process':
            return { width: widths.right, height: rightHeights.process };
        case 'tasks':
            return { width: widths.right, height: rightHeights.tasks };
        case 'context':
            return { width: widths.right, height: rightHeights.context };
    }
}
export function nextWrappedIndex(currentIndex, delta, length) {
    if (length <= 0) {
        return 0;
    }
    return (currentIndex + delta + length) % length;
}
export function selectMenuItemByTypeahead(items, query, currentIndex = -1) {
    const normalized = query.trim().toLowerCase();
    if (!normalized || items.length === 0) {
        return Math.max(0, currentIndex);
    }
    const matches = items
        .map((item, index) => ({ label: item.label.toLowerCase(), index }))
        .filter((item) => item.label.startsWith(normalized))
        .map((item) => item.index);
    if (matches.length === 0) {
        return Math.max(0, currentIndex);
    }
    const next = matches.find((index) => index > currentIndex);
    return next ?? matches[0];
}
function wrapBodyLines(body, innerWidth) {
    const lines = [];
    for (const line of body) {
        lines.push(...wrapPlainText(line, innerWidth));
    }
    return lines;
}
export function collectTailWrappedLines(body, innerWidth, limit) {
    if (limit <= 0) {
        return { lines: [], truncated: body.length > 0 };
    }
    const collected = [];
    let truncated = false;
    for (let index = body.length - 1; index >= 0; index -= 1) {
        const wrapped = wrapPlainText(body[index], innerWidth);
        const remaining = limit - collected.length;
        if (remaining <= 0) {
            truncated = true;
            break;
        }
        if (wrapped.length <= remaining) {
            collected.unshift(...wrapped);
            continue;
        }
        collected.unshift(...wrapped.slice(wrapped.length - remaining));
        truncated = true;
        break;
    }
    return { lines: collected, truncated };
}
async function listWorkspaceTextFiles(workspaceRoot) {
    const results = [];
    const visit = async (relativeDir) => {
        const absoluteDir = relativeDir ? path.join(workspaceRoot, relativeDir) : workspaceRoot;
        let entries;
        try {
            entries = await fs.readdir(absoluteDir, { withFileTypes: true });
        }
        catch {
            return;
        }
        for (const entry of entries) {
            const relativePath = relativeDir ? path.join(relativeDir, entry.name) : entry.name;
            if (entry.isDirectory()) {
                if (IGNORED_WORKSPACE_DIRS.has(entry.name)) {
                    continue;
                }
                await visit(relativePath);
                continue;
            }
            if (entry.isFile() && isEditableTextFile(relativePath)) {
                results.push(relativePath.replace(/\\/g, '/'));
            }
        }
    };
    await visit('');
    results.sort((left, right) => left.localeCompare(right));
    return results;
}
function isEditableTextFile(filePath) {
    const basename = path.basename(filePath);
    const extension = path.extname(filePath).toLowerCase();
    return TEXT_FILE_EXTENSIONS.has(extension) || TEXT_FILE_BASENAMES.has(basename);
}
function splitEditorContent(content) {
    const lines = content.split(/\r?\n/);
    return lines.length > 0 ? lines : [''];
}
function resolveWorkspaceTextPath(workspaceRoot, requestedPath) {
    const absolutePath = path.resolve(workspaceRoot, requestedPath);
    const resolvedRoot = path.resolve(workspaceRoot);
    if (absolutePath === resolvedRoot || absolutePath.startsWith(`${resolvedRoot}${path.sep}`)) {
        return absolutePath;
    }
    return path.join(resolvedRoot, path.basename(requestedPath));
}
export function filterPickerItems(items, query) {
    const normalized = query.trim().toLowerCase();
    if (!normalized) {
        return [...items];
    }
    const tokens = normalized.split(/\s+/).filter(Boolean);
    return [...items].filter((item) => {
        const haystack = `${item.label} ${item.description} ${item.detail ?? ''}`.toLowerCase();
        return tokens.every((token) => haystack.includes(token));
    });
}
function clamp(value, min, max) {
    if (value < min) {
        return min;
    }
    if (value > max) {
        return max;
    }
    return value;
}
function currentSpinnerFrame() {
    return SPINNER_FRAMES[Math.floor(Date.now() / 120) % SPINNER_FRAMES.length];
}
function summarizeRunResult(result) {
    if (result.answer?.trim()) {
        return result.answer;
    }
    const lastOutput = [...result.output].reverse().find((line) => line.trim());
    if (lastOutput) {
        return lastOutput;
    }
    if (result.error?.trim()) {
        return result.error;
    }
    return 'Execution completed without a final answer.';
}
function paint(text, ...codes) {
    if (!output.isTTY || codes.length === 0) {
        return text;
    }
    return `${codes.join('')}${text}${ANSI.reset}`;
}
//# sourceMappingURL=tui.js.map