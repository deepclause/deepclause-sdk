import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Messages pane component.
 * Displays chat messages with streaming support using <Static> for completed messages.
 */
import React from 'react';
import { Box, Static, Text } from 'ink';
import { MessageBubble } from '../shared/MessageBubble.js';
export const MessagesPane = ({ messages, streamingContent, focused, }) => {
    // Completed messages use <Static> — rendered once, never re-diffed
    const completedMessages = streamingContent !== null
        ? messages.slice(0, -1)
        : messages;
    // Only the last message + streaming content needs dynamic rendering
    const lastMessage = streamingContent !== null && messages.length > 0
        ? messages[messages.length - 1]
        : null;
    return (_jsxs(Box, { flexDirection: "column", flexGrow: 1, borderStyle: "single", borderColor: focused ? 'blue' : 'gray', children: [_jsx(Text, { bold: true, color: focused ? 'blue' : 'white', children: "Messages" }), _jsx(Static, { items: completedMessages, children: (msg, index) => (_jsx(MessageBubble, { role: msg.role, content: msg.content, error: msg.error, tag: msg.tag }, index)) }), lastMessage && (_jsx(MessageBubble, { role: lastMessage.role, content: lastMessage.content, pending: lastMessage.pending, error: lastMessage.error, tag: lastMessage.tag })), streamingContent !== null && (_jsx(Box, { marginLeft: 2, children: _jsxs(Text, { color: "blue", wrap: "wrap", children: [streamingContent, _jsx(Text, { color: "cyan", children: "\u258A" })] }) })), messages.length === 0 && streamingContent === null && (_jsx(Box, { marginTop: 1, children: _jsx(Text, { dimColor: true, children: "No messages yet. Type a message below or use /help for commands." }) }))] }));
};
//# sourceMappingURL=MessagesPane.js.map