import { jsxs as _jsxs, jsx as _jsx } from "react/jsx-runtime";
import { Box, Text } from 'ink';
export const StatusBar = ({ focusedPane, autoScroll, busy, mode }) => {
    return (_jsxs(Box, { height: 1, width: "100%", children: [_jsx(Box, { marginRight: 2, children: _jsxs(Text, { dimColor: true, children: ["[", focusedPane, "]"] }) }), _jsx(Box, { marginRight: 2, children: _jsx(Text, { dimColor: true, children: autoScroll ? '↓ follow' : '⏸ scroll' }) }), busy && (_jsx(Box, { marginRight: 2, children: _jsx(Text, { color: "yellow", children: "running\u2026" }) })), _jsx(Box, { flexGrow: 1 }), _jsx(Text, { dimColor: true, children: mode === 'normal' ? '? help | : command | Ctrl+C quit' : `[${mode}] ESC to exit` })] }));
};
//# sourceMappingURL=StatusBar.js.map