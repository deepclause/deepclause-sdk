import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Top menu bar component.
 */
import React from 'react';
import { Box, Text } from 'ink';
const MENU_ITEMS = ['Session', 'Skills', 'Files', 'Run', 'View', 'Help'];
export const MenuBar = ({ sessionTitle, busy }) => {
    return (_jsxs(Box, { height: 1, width: "100%", children: [_jsx(Box, { marginRight: 1, children: _jsx(Text, { bold: true, color: "cyan", children: "DeepClause" }) }), MENU_ITEMS.map((item) => (_jsx(Box, { marginRight: 1, children: _jsx(Text, { dimColor: true, children: item }) }, item))), _jsx(Box, { flexGrow: 1 }), _jsx(Text, { dimColor: true, children: sessionTitle }), busy && (_jsx(Box, { marginLeft: 1, children: _jsx(Text, { color: "yellow", children: "\u25CF" }) }))] }));
};
//# sourceMappingURL=MenuBar.js.map