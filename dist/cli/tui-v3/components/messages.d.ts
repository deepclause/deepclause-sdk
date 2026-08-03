/**
 * Messages component — chat message list with per-message render cache.
 *
 * Key optimization: historical messages are cached and never re-rendered
 * during streaming. Only the streaming (latest) message re-renders.
 */
import type { Component, RequestRender } from '../types.js';
import type { ExecutionPreview } from '../state/session-state.js';
export interface ChatMessage {
    role: 'user' | 'assistant' | 'system';
    content: string;
    pending?: boolean;
    error?: boolean;
}
export declare class Messages implements Component {
    dirty: boolean;
    minHeight: number;
    flexGrow: number;
    private requestRenderFn;
    private messages;
    private executionPreview;
    private cache;
    constructor(requestRender: RequestRender);
    /** Set the message list. */
    setMessages(messages: ChatMessage[]): void;
    /** Append a message. */
    appendMessage(message: ChatMessage): void;
    /** Set the current or most recently completed DML execution preview. */
    setExecutionPreview(preview: ExecutionPreview | null): void;
    invalidate(): void;
    render(width: number): string[];
    private renderExecution;
    private renderMessage;
    private getRoleLabel;
    private wrapText;
}
//# sourceMappingURL=messages.d.ts.map