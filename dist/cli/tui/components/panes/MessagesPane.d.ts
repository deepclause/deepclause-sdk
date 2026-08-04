/**
 * Messages pane component.
 * Displays chat messages with streaming support using <Static> for completed messages.
 */
import React from 'react';
import type { DisplayMessage } from '../../store/session-store.js';
interface MessagesPaneProps {
    messages: DisplayMessage[];
    streamingContent: string | null;
    focused: boolean;
    autoScroll: boolean;
}
export declare const MessagesPane: React.FC<MessagesPaneProps>;
export {};
//# sourceMappingURL=MessagesPane.d.ts.map