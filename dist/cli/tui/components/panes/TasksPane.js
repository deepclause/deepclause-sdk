import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Tasks/Steps pane component.
 * Displays a tree of execution steps with status markers.
 */
import React from 'react';
import { Box, Text } from 'ink';
const STATE_MARKERS = {
    started: { char: '▸', color: 'yellow' },
    completed: { char: '✓', color: 'green' },
    failed: { char: '✗', color: 'red' },
};
export const TasksPane = ({ tasks, focused, height }) => {
    const visibleHeight = Math.max(1, height - 3);
    const visibleTasks = tasks.slice(-visibleHeight);
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "single", borderColor: focused ? 'blue' : 'gray', height: height, children: [_jsx(Text, { bold: true, color: focused ? 'blue' : 'white', children: "Steps" }), tasks.length === 0 ? (_jsx(Text, { dimColor: true, children: "No steps yet." })) : (visibleTasks.map((task) => {
                const marker = STATE_MARKERS[task.state] ?? STATE_MARKERS.started;
                const indent = '  '.repeat(task.depth);
                return (_jsxs(Text, { wrap: "truncate", children: [_jsxs(Text, { color: marker.color, children: [indent, marker.char] }), ' ', _jsx(Text, { dimColor: task.state === 'completed', children: task.description })] }, task.id));
            }))] }));
};
//# sourceMappingURL=TasksPane.js.map