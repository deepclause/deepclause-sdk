export type CapturedConsoleLevel = 'log' | 'warn' | 'error';
export interface CapturedConsoleEntry {
    level: CapturedConsoleLevel;
    text: string;
}
type ConsoleSink = (entry: CapturedConsoleEntry) => void;
export declare function withCapturedConsole<T>(sink: ConsoleSink, run: () => Promise<T> | T): Promise<T>;
export {};
//# sourceMappingURL=console-capture.d.ts.map