import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * MessageBubble component for rendering chat messages.
 * Memoized so only the active (streaming) message re-renders.
 */
import React from 'react';
import { Box, Text } from 'ink';
const ROLE_COLORS = {
    user: 'green',
    assistant: 'blue',
    system: 'yellow',
};
const ROLE_LABELS = {
    user: 'You',
    assistant: 'Assistant',
    system: 'System',
};
export const MessageBubble = React.memo(({ role, content, pending, error, tag }) => {
    const color = error ? 'red' : ROLE_COLORS[role] ?? 'white';
    const label = tag || ROLE_LABELS[role] || role;
    return (_jsxs(Box, { flexDirection: "column", marginBottom: 1, children: [_jsxs(Text, { bold: true, color: color, children: [label, pending ? ' …' : ''] }), _jsx(Box, { marginLeft: 2, children: _jsx(Text, { dimColor: role === 'system', color: error ? 'red' : undefined, wrap: "wrap", children: content || (pending ? '…' : '') }) })] }));
});
MessageBubble.displayName = 'MessageBubble';
//# sourceMappingURL=MessageBubble.js.map