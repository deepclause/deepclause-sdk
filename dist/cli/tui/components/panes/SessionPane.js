import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Session pane component.
 * Collapsible sidebar showing session list. Default collapsed to icon-width.
 */
import React from 'react';
import { Box, Text } from 'ink';
export const SessionPane = ({ sessions, activeSessionId, collapsed, focused, }) => {
    if (collapsed) {
        return (_jsxs(Box, { flexDirection: "column", borderStyle: "single", borderColor: focused ? 'blue' : 'gray', width: 3, height: "100%", children: [_jsx(Text, { color: "cyan", children: "S" }), sessions.map((s, i) => (_jsx(Text, { color: s.id === activeSessionId ? 'green' : 'gray', children: i + 1 }, s.id)))] }));
    }
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "single", borderColor: focused ? 'blue' : 'gray', width: 24, height: "100%", children: [_jsx(Text, { bold: true, color: focused ? 'blue' : 'white', children: "Sessions" }), sessions.length === 0 ? (_jsx(Text, { dimColor: true, children: "No sessions" })) : (sessions.map((s) => (_jsxs(Text, { color: s.id === activeSessionId ? 'green' : undefined, bold: s.id === activeSessionId, wrap: "truncate", children: [s.id === activeSessionId ? '▸ ' : '  ', s.title || s.id.slice(0, 8)] }, s.id))))] }));
};
//# sourceMappingURL=SessionPane.js.map