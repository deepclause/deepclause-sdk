import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { Box, Text } from 'ink';
export const ContextPane = ({ tokenUsage, focused, height }) => {
    const models = Object.entries(tokenUsage);
    return (_jsxs(Box, { flexDirection: "column", borderStyle: "single", borderColor: focused ? 'blue' : 'gray', height: height, children: [_jsx(Text, { bold: true, color: focused ? 'blue' : 'white', children: "Context" }), models.length === 0 ? (_jsx(Text, { dimColor: true, children: "No usage data." })) : (models.map(([model, usage]) => (_jsxs(Box, { flexDirection: "column", children: [_jsx(Text, { color: "cyan", wrap: "truncate", children: model }), _jsxs(Text, { dimColor: true, wrap: "truncate", children: ['  ', "in: ", formatTokenCount(usage.inputTokens), " | out: ", formatTokenCount(usage.outputTokens)] })] }, model))))] }));
};
function formatTokenCount(count) {
    if (count >= 1_000_000)
        return `${(count / 1_000_000).toFixed(1)}M`;
    if (count >= 1_000)
        return `${(count / 1_000).toFixed(1)}k`;
    return String(count);
}
//# sourceMappingURL=ContextPane.js.map