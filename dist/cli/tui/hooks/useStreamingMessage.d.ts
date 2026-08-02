/**
 * Hook for throttled streaming message updates.
 * Accumulates tokens in a ref and flushes to state at most 30fps,
 * reducing React reconciliation overhead during fast streaming.
 */
export declare function useStreamingMessage(): {
    text: string;
    appendToken: (token: string) => void;
    reset: () => void;
};
//# sourceMappingURL=useStreamingMessage.d.ts.map