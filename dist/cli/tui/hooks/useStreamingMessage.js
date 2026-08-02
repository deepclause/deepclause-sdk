/**
 * Hook for throttled streaming message updates.
 * Accumulates tokens in a ref and flushes to state at most 30fps,
 * reducing React reconciliation overhead during fast streaming.
 */
import { useState, useRef, useEffect, useCallback } from 'react';
const FLUSH_INTERVAL_MS = 33; // ~30fps max
export function useStreamingMessage() {
    const [text, setText] = useState('');
    const bufferRef = useRef('');
    const timerRef = useRef(null);
    const startFlushing = useCallback(() => {
        if (timerRef.current !== null)
            return;
        timerRef.current = setInterval(() => {
            if (bufferRef.current !== '') {
                setText(bufferRef.current);
            }
        }, FLUSH_INTERVAL_MS);
    }, []);
    const stopFlushing = useCallback(() => {
        if (timerRef.current !== null) {
            clearInterval(timerRef.current);
            timerRef.current = null;
        }
        // Final flush
        if (bufferRef.current !== '') {
            setText(bufferRef.current);
        }
    }, []);
    const appendToken = useCallback((token) => {
        bufferRef.current += token;
        startFlushing();
    }, [startFlushing]);
    const reset = useCallback(() => {
        stopFlushing();
        bufferRef.current = '';
        setText('');
    }, [stopFlushing]);
    useEffect(() => {
        return () => {
            if (timerRef.current !== null) {
                clearInterval(timerRef.current);
            }
        };
    }, []);
    return { text, appendToken, reset };
}
//# sourceMappingURL=useStreamingMessage.js.map