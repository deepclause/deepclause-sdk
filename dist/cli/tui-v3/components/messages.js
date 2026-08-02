/**
 * Messages component — chat message list with per-message render cache.
 *
 * Key optimization: historical messages are cached and never re-rendered
 * during streaming. Only the streaming (latest) message re-renders.
 */
import { style, ANSI } from '../util/ansi.js';
export class Messages {
    dirty = true;
    minHeight = 1;
    flexGrow = 1;
    requestRenderFn;
    messages = [];
    streamingContent = null;
    cache = [];
    constructor(requestRender) {
        this.requestRenderFn = requestRender;
    }
    /** Set the message list. */
    setMessages(messages) {
        this.messages = messages;
        // Invalidate cache for any changed messages
        this.cache = this.cache.slice(0, Math.min(this.cache.length, messages.length));
        this.invalidate();
    }
    /** Append a message. */
    appendMessage(message) {
        this.messages.push(message);
        this.invalidate();
    }
    /** Set streaming content (the in-progress assistant message). */
    setStreaming(content) {
        this.streamingContent = content;
        this.invalidate();
    }
    invalidate() {
        this.dirty = true;
        this.requestRenderFn();
    }
    render(width) {
        const allRows = [];
        // Render each message (with caching)
        for (let i = 0; i < this.messages.length; i++) {
            const msg = this.messages[i];
            const cached = this.cache[i];
            if (cached && cached.message === msg && cached.width === width) {
                // Use cached render
                allRows.push(...cached.rows);
            }
            else {
                // Render and cache
                const rows = this.renderMessage(msg, width);
                this.cache[i] = { message: msg, rows, width };
                allRows.push(...rows);
            }
        }
        // Render streaming message (never cached)
        if (this.streamingContent !== null) {
            const streamMsg = { role: 'assistant', content: this.streamingContent, pending: true };
            const rows = this.renderMessage(streamMsg, width);
            allRows.push(...rows);
        }
        // If no messages, show a placeholder
        if (allRows.length === 0) {
            allRows.push('');
            allRows.push(style('  No messages yet. Type a message below to start.', ANSI.dim));
            allRows.push('');
        }
        this.dirty = false;
        return allRows;
    }
    renderMessage(msg, width) {
        const rows = [];
        const contentWidth = Math.max(10, width - 4); // 2 chars indent + 2 margin
        // Role header
        const roleLabel = this.getRoleLabel(msg);
        rows.push(roleLabel);
        // Wrap content into lines
        const lines = this.wrapText(msg.content, contentWidth);
        for (const line of lines) {
            const styled = msg.error
                ? style(`  ${line}`, ANSI.red)
                : msg.pending
                    ? style(`  ${line}`, ANSI.dim)
                    : `  ${line}`;
            rows.push(styled);
        }
        // Blank separator
        rows.push('');
        return rows;
    }
    getRoleLabel(msg) {
        switch (msg.role) {
            case 'user':
                return style('  You:', ANSI.bold, ANSI.green);
            case 'assistant':
                return msg.pending
                    ? style('  Assistant: (streaming…)', ANSI.bold, ANSI.blue)
                    : style('  Assistant:', ANSI.bold, ANSI.blue);
            case 'system':
                return msg.error
                    ? style('  System (error):', ANSI.bold, ANSI.red)
                    : style('  System:', ANSI.bold, ANSI.yellow);
        }
    }
    wrapText(text, maxWidth) {
        const lines = [];
        const rawLines = text.split('\n');
        for (const rawLine of rawLines) {
            if (rawLine.length <= maxWidth) {
                lines.push(rawLine);
            }
            else {
                // Simple word wrapping
                let remaining = rawLine;
                while (remaining.length > maxWidth) {
                    let breakAt = remaining.lastIndexOf(' ', maxWidth);
                    if (breakAt <= 0)
                        breakAt = maxWidth;
                    lines.push(remaining.slice(0, breakAt));
                    remaining = remaining.slice(breakAt).trimStart();
                }
                if (remaining)
                    lines.push(remaining);
            }
        }
        return lines;
    }
}
//# sourceMappingURL=messages.js.map