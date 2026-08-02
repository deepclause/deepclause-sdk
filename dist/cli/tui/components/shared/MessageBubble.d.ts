/**
 * MessageBubble component for rendering chat messages.
 * Memoized so only the active (streaming) message re-renders.
 */
import React from 'react';
interface MessageBubbleProps {
    role: 'user' | 'assistant' | 'system';
    content: string;
    pending?: boolean;
    error?: boolean;
    tag?: string;
}
export declare const MessageBubble: React.FC<MessageBubbleProps>;
export {};
//# sourceMappingURL=MessageBubble.d.ts.map