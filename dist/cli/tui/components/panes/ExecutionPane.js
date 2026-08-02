import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Execution/Process pane component.
 * Shows tool calls, activity logs, and active tool status.
 */
import React from 'react';
import { Box, Text } from 'ink';
import { Spinner } from '../shared/Spinner.js';
export const ExecutionPane = ({ activityLines, activeTools, running, focused, height, }) => {
    const visibleHeight = Math.max(1, height - 4); // Account for border + title + active tools
    const visibleLines = activityLines.slice(-visibleHeight);
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "single", borderColor: focused ? 'blue' : 'gray', height: height, children: [_jsxs(Box, { children: [_jsx(Text, { bold: true, color: focused ? 'blue' : 'white', children: "Activity" }), running && (_jsx(Box, { marginLeft: 1, children: _jsx(Spinner, {}) }))] }), activeTools.length > 0 && (_jsx(Box, { flexDirection: "column", marginBottom: 1, children: activeTools.map((tool) => (_jsxs(Text, { color: "yellow", wrap: "truncate", children: ["\u25CF ", tool.scopeLabel, ":", tool.toolName, " [", tool.toolState, "]"] }, tool.scopeKey))) })), _jsx(Box, { flexDirection: "column", flexGrow: 1, children: visibleLines.length === 0 ? (_jsx(Text, { dimColor: true, children: "No activity yet." })) : (visibleLines.map((line, i) => (_jsx(Text, { wrap: "truncate", dimColor: true, children: line }, i)))) })] }));
};
//# sourceMappingURL=ExecutionPane.js.map