import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * File viewer overlay component.
 * Displays file content in a scrollable view.
 */
import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
export const Viewer = ({ title, content, onClose }) => {
    const lines = content.split('\n');
    const [scrollTop, setScrollTop] = useState(0);
    const pageSize = 20;
    useInput((input, key) => {
        if (key.escape || input === 'q') {
            onClose();
            return;
        }
        if (key.upArrow || input === 'k') {
            setScrollTop((s) => Math.max(0, s - 1));
        }
        else if (key.downArrow || input === 'j') {
            setScrollTop((s) => Math.min(Math.max(0, lines.length - pageSize), s + 1));
        }
        else if (key.pageUp || (key.ctrl && input === 'b')) {
            setScrollTop((s) => Math.max(0, s - pageSize));
        }
        else if (key.pageDown || (key.ctrl && input === 'f')) {
            setScrollTop((s) => Math.min(Math.max(0, lines.length - pageSize), s + pageSize));
        }
    });
    const visibleLines = lines.slice(scrollTop, scrollTop + pageSize);
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "round", borderColor: "cyan", padding: 1, children: [_jsxs(Box, { children: [_jsx(Text, { bold: true, color: "cyan", children: title }), _jsx(Box, { flexGrow: 1 }), _jsxs(Text, { dimColor: true, children: ["[", scrollTop + 1, "-", Math.min(scrollTop + pageSize, lines.length), "/", lines.length, "]"] })] }), _jsx(Box, { flexDirection: "column", marginTop: 1, children: visibleLines.map((line, i) => (_jsxs(Text, { wrap: "truncate", children: [_jsxs(Text, { dimColor: true, children: [String(scrollTop + i + 1).padStart(4), " "] }), line] }, scrollTop + i))) }), _jsx(Box, { marginTop: 1, children: _jsx(Text, { dimColor: true, children: "j/k scroll | PgUp/PgDn page | q/Esc close" }) })] }));
};
//# sourceMappingURL=Viewer.js.map