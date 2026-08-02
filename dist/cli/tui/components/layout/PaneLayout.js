import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { Box } from 'ink';
export const PaneLayout = ({ sessionPaneCollapsed, children, height, }) => {
    const sessionWidth = sessionPaneCollapsed ? 3 : 24;
    const rightColumnHeight = Math.floor(height / 2);
    return (_jsxs(Box, { flexDirection: "row", height: height, width: "100%", children: [_jsx(Box, { width: sessionWidth, flexDirection: "column", children: children.sessions }), _jsx(Box, { flexGrow: 1, flexDirection: "column", children: children.messages }), _jsxs(Box, { width: 40, flexDirection: "column", children: [_jsx(Box, { height: rightColumnHeight, children: children.process }), _jsx(Box, { height: Math.floor(rightColumnHeight / 2), children: children.tasks }), _jsx(Box, { flexGrow: 1, children: children.context })] })] }));
};
//# sourceMappingURL=PaneLayout.js.map