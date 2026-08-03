import { jsxs as _jsxs, jsx as _jsx } from "react/jsx-runtime";
/**
 * Scrollable box component with content caching.
 * Renders a viewport into a list of lines, handling scroll position.
 */
import { useState, useEffect, useCallback } from 'react';
import { Box, Text, useInput } from 'ink';
export const ScrollableBox = ({ lines, height, focused = false, autoScroll = true, title, borderColor = 'gray', }) => {
    const [scrollTop, setScrollTop] = useState(0);
    const maxScroll = Math.max(0, lines.length - height);
    // Auto-scroll to bottom when new content arrives
    useEffect(() => {
        if (autoScroll) {
            setScrollTop(maxScroll);
        }
    }, [lines.length, maxScroll, autoScroll]);
    const scrollUp = useCallback(() => {
        setScrollTop((prev) => Math.max(0, prev - 1));
    }, []);
    const scrollDown = useCallback(() => {
        setScrollTop((prev) => Math.min(maxScroll, prev + 1));
    }, [maxScroll]);
    const pageUp = useCallback(() => {
        setScrollTop((prev) => Math.max(0, prev - height));
    }, [height]);
    const pageDown = useCallback(() => {
        setScrollTop((prev) => Math.min(maxScroll, prev + height));
    }, [maxScroll, height]);
    useInput((input, key) => {
        if (!focused)
            return;
        if (key.upArrow || (key.ctrl && input === 'p')) {
            scrollUp();
        }
        else if (key.downArrow || (key.ctrl && input === 'n')) {
            scrollDown();
        }
        else if (key.pageUp || (key.meta && input === 'v')) {
            pageUp();
        }
        else if (key.pageDown || (key.ctrl && input === 'v')) {
            pageDown();
        }
    }, { isActive: focused });
    const visibleLines = lines.slice(scrollTop, scrollTop + height);
    const scrollIndicator = lines.length > height
        ? ` [${scrollTop + 1}-${Math.min(scrollTop + height, lines.length)}/${lines.length}]`
        : '';
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "single", borderColor: focused ? 'blue' : borderColor, height: height + 2, children: [title && (_jsx(Box, { children: _jsxs(Text, { bold: true, color: focused ? 'blue' : 'white', children: [title, scrollIndicator] }) })), _jsxs(Box, { flexDirection: "column", flexGrow: 1, children: [visibleLines.map((line, i) => (_jsx(Text, { wrap: "truncate", children: line }, scrollTop + i))), visibleLines.length < height &&
                        Array.from({ length: height - visibleLines.length }).map((_, i) => (_jsx(Text, { children: " " }, `pad-${i}`)))] })] }));
};
//# sourceMappingURL=ScrollableBox.js.map