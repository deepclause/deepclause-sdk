import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Fuzzy picker overlay component.
 * Provides a searchable list for selecting sessions, skills, files, etc.
 */
import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
export const Picker = ({ title, items, onSelect, onCancel, emptyText = 'No items found.', }) => {
    const [query, setQuery] = useState('');
    const [selectedIndex, setSelectedIndex] = useState(0);
    const filteredItems = filterItems(items, query);
    useInput((input, key) => {
        if (key.escape) {
            onCancel();
            return;
        }
        if (key.return) {
            if (filteredItems.length > 0) {
                onSelect(filteredItems[selectedIndex]);
            }
            return;
        }
        if (key.upArrow || (key.ctrl && input === 'p')) {
            setSelectedIndex((i) => Math.max(0, i - 1));
            return;
        }
        if (key.downArrow || (key.ctrl && input === 'n')) {
            setSelectedIndex((i) => Math.min(filteredItems.length - 1, i + 1));
            return;
        }
        if (key.backspace || key.delete) {
            setQuery((q) => q.slice(0, -1));
            setSelectedIndex(0);
            return;
        }
        if (input && !key.ctrl && !key.meta) {
            setQuery((q) => q + input);
            setSelectedIndex(0);
        }
    });
    const maxVisible = 10;
    const startIdx = Math.max(0, selectedIndex - maxVisible + 1);
    const visibleItems = filteredItems.slice(startIdx, startIdx + maxVisible);
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "round", borderColor: "cyan", padding: 1, children: [_jsx(Text, { bold: true, color: "cyan", children: title }), _jsxs(Box, { marginTop: 1, children: [_jsx(Text, { color: "green", children: "> " }), _jsx(Text, { children: query }), _jsx(Text, { color: "green", children: "\u2588" })] }), _jsx(Box, { flexDirection: "column", marginTop: 1, children: visibleItems.length === 0 ? (_jsx(Text, { dimColor: true, children: emptyText })) : (visibleItems.map((item, i) => {
                    const isSelected = startIdx + i === selectedIndex;
                    return (_jsxs(Box, { children: [_jsxs(Text, { color: isSelected ? 'blue' : undefined, bold: isSelected, children: [isSelected ? '▸ ' : '  ', item.label] }), _jsxs(Text, { dimColor: true, children: [" \u2014 ", item.description] })] }, item.id));
                })) }), _jsx(Box, { marginTop: 1, children: _jsxs(Text, { dimColor: true, children: [filteredItems.length, " result", filteredItems.length !== 1 ? 's' : '', " | \u2191\u2193 navigate | Enter select | Esc cancel"] }) })] }));
};
function filterItems(items, query) {
    const normalized = query.trim().toLowerCase();
    if (!normalized)
        return items;
    const tokens = normalized.split(/\s+/).filter(Boolean);
    return items.filter((item) => {
        const haystack = `${item.label} ${item.description} ${item.detail ?? ''}`.toLowerCase();
        return tokens.every((token) => haystack.includes(token));
    });
}
//# sourceMappingURL=Picker.js.map