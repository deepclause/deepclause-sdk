import { type ModelSlot } from './config.js';
import { type RunResult as CliRunResult } from './run.js';
import { type ConductorLogEvent } from '../system/runtime/conductor.js';
declare const BUILTIN_SLASH_COMMANDS: readonly ['new', 'sessions', 'help', 'run', 'compile', 'skill-creator', 'set-model', 'cancel', 'exit', 'quit'];
type BuiltinSlashCommand = (typeof BUILTIN_SLASH_COMMANDS)[number];
type TuiInputStream = {
    isTTY?: boolean;
    pause(): void;
    setRawMode?(mode: boolean): void;
};
export interface DisplayMessage {
    role: 'user' | 'assistant' | 'system';
    content: string;
    pending?: boolean;
    error?: boolean;
    tag?: string;
    kind?: 'output' | 'question';
}
export type ParsedTuiInput = {
    kind: 'text';
    prompt: string;
} | {
    kind: 'builtin';
    name: BuiltinSlashCommand;
    rawArgs: string;
    args: string[];
} | {
    kind: 'skill';
    name: string;
    rawArgs: string;
    args: string[];
} | {
    kind: 'shell';
    command: string;
    persistOutput: boolean;
};
export interface SlashCompletionResult {
    value: string;
    matches: string[];
    applied: boolean;
}
export declare class LiveExecutionPrinter {
    private readonly write;
    private readonly writeLine;
    private activeStreamKey;
    private streamOpen;
    constructor(write?: (text: string) => void, writeLine?: (text: string) => void);
    handle(logEvent: ConductorLogEvent): void;
    finish(): void;
    private handleStream;
    private flushStream;
    private handleToolCall;
}
export declare class ActivityBuffer {
    private readonly lines;
    private readonly activeTools;
    private activeStreamKey;
    private activeStreamLine;
    handle(logEvent: ConductorLogEvent): void;
    pushLine(line: string): void;
    finish(): void;
    clear(): void;
    snapshot(): string[];
    snapshotTail(limit: number): string[];
    private handleStream;
    private flushStream;
    private trim;
    private handleToolCall;
    private buildActiveToolLines;
}
export declare class TaskTracker {
    private readonly entries;
    private readonly activeStack;
    private readonly maxEntries;
    handle(logEvent: ConductorLogEvent): void;
    buildPaneBody(_width: number): string[];
    clear(): void;
    private trim;
}
export declare function releaseTuiInputStream(stream: TuiInputStream): void;
export declare function startTui(workspaceRoot?: any, options?: {
    sandbox?: boolean;
}): Promise<void>;
export declare function runPromptHeadless(prompt: string, workspaceRoot?: any, options?: {
    sandbox?: boolean;
}): Promise<void>;
export declare function runSkillCommand(workspaceRoot: string, skillName: string, args: string[], options?: {
    sessionId?: string;
    sandbox?: boolean;
    signal?: AbortSignal;
    onUserInput?: (prompt: string) => Promise<string>;
    onEvent?: (event: ConductorLogEvent) => void;
}): Promise<CliRunResult>;
export declare function runSlashCommand(workspaceRoot: string, name: string, args: string[], options?: {
    sessionId?: string;
    sandbox?: boolean;
    signal?: AbortSignal;
    toolAbortSignalRef?: {
        signal?: AbortSignal;
    };
    onUserInput?: (prompt: string) => Promise<string>;
    onEvent?: (event: ConductorLogEvent) => void;
}): Promise<CliRunResult>;
export declare function runFileCommand(workspaceRoot: string, fileSpec: string, args: string[], options?: {
    sessionId?: string;
    sandbox?: boolean;
    signal?: AbortSignal;
    toolAbortSignalRef?: {
        signal?: AbortSignal;
    };
    onUserInput?: (prompt: string) => Promise<string>;
    onEvent?: (event: ConductorLogEvent) => void;
}): Promise<CliRunResult>;
export declare function previewMessageFromEvent(logEvent: ConductorLogEvent): DisplayMessage | null;
export declare function previewChildSkillActivityMessage(childSlug: string): DisplayMessage;
export declare function previewQuestionMessage(promptText: string, explicitTag?: string): DisplayMessage;
export declare function parseCommandArgs(rawArgs: string): string[];
export declare function parseCommandBarInput(line: string): ParsedTuiInput;
export declare function parseSlashInput(line: string): ParsedTuiInput;
export declare function canSubmitParsedInputWhileBusy(parsed: ParsedTuiInput): boolean;
export declare function reconcileEphemeralMessages(persistedMessages: Array<{
    role: 'user' | 'assistant';
    content: string;
}>, ephemeralMessages: DisplayMessage[]): DisplayMessage[];
export declare function sessionMessagesContainCompletedTaskPreview(persistedMessages: Array<{
    role: 'user' | 'assistant';
    content: string;
}>, previewEntries: DisplayMessage[]): boolean;
export declare function computeFramePatch(previousLines: string[] | null, nextLines: string[], previousSize: {
    columns: number;
    rows: number;
} | null, nextSize: {
    columns: number;
    rows: number;
}): {
    fullRender: boolean;
    changedRows: Array<{
        row: number;
        line: string;
    }>;
};
export declare function completeSlashCommand(inputValue: string, candidates: string[]): SlashCompletionResult;
export declare function parseSetModelCommandArgs(rawArgs: string): {
    model: string;
    slot?: ModelSlot;
};
export declare function formatDisplayMessageHeader(entry: DisplayMessage, spinner?: string): string;
export declare function formatDisplayMessageBodyLines(entry: DisplayMessage): string[];
export declare function wrapPlainText(text: string, width: number): string[];
export declare function measureDisplayWidth(text: string): number;
export declare function padRight(text: string, width: number): string;
export declare function ellipsize(text: string, width: number): string;
export declare function nextWrappedIndex(currentIndex: number, delta: -1 | 1, length: number): number;
export declare function selectMenuItemByTypeahead(items: ReadonlyArray<{
    label: string;
}>, query: string, currentIndex?: number): number;
export declare function collectTailWrappedLines(body: string[], innerWidth: number, limit: number): {
    lines: string[];
    truncated: boolean;
};
export declare function filterPickerItems<T extends {
    label: string;
    description: string;
    detail?: string;
}>(items: ReadonlyArray<T>, query: string): T[];
export {};
//# sourceMappingURL=tui.d.ts.map