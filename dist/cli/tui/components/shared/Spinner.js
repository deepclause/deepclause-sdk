import { jsxs as _jsxs } from "react/jsx-runtime";
/**
 * Animated spinner component using Ink's built-in rendering.
 * Only re-renders when the frame changes (no setInterval abuse).
 */
import { useState, useEffect } from 'react';
import { Text } from 'ink';
const FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
const INTERVAL_MS = 80;
export const Spinner = ({ label }) => {
    const [frameIndex, setFrameIndex] = useState(0);
    useEffect(() => {
        const timer = setInterval(() => {
            setFrameIndex((prev) => (prev + 1) % FRAMES.length);
        }, INTERVAL_MS);
        return () => clearInterval(timer);
    }, []);
    return (_jsxs(Text, { color: "cyan", children: [FRAMES[frameIndex], label ? ` ${label}` : ''] }));
};
//# sourceMappingURL=Spinner.js.map