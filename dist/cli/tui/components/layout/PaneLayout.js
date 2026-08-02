import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { Box } from 'ink';
export const PaneLayout = ({ sessionPaneCollapsed, children, height, columns, }) => {
    const sessionWidth = sessionPaneCollapsed ? 3 : 24;
    // Right column: allocate ~30% of remaining width, minimum 30, maximum 50
    const remainingWidth = columns - sessionWidth;
    const rightWidth = Math.max(30, Math.min(50, Math.floor(remainingWidth * 0.3)));
    const rightColumnHeight = Math.max(3, Math.floor(height * 0.5));
    const tasksHeight = Math.max(3, Math.floor(height * 0.25));
    return (_jsxs(Box, { flexDirection: "row", height: height, width: columns, children: [_jsx(Box, { width: sessionWidth, flexDirection: "column", height: height, children: children.sessions }), _jsx(Box, { flexGrow: 1, flexDirection: "column", height: height, children: children.messages }), _jsxs(Box, { width: rightWidth, flexDirection: "column", height: height, children: [_jsx(Box, { height: rightColumnHeight, children: children.process }), _jsx(Box, { height: tasksHeight, children: children.tasks }), _jsx(Box, { flexGrow: 1, children: children.context })] })] }));
};
//# sourceMappingURL=PaneLayout.js.map