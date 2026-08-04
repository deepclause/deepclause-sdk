/**
 * Session pane component.
 * Collapsible sidebar showing session list. Default collapsed to icon-width.
 */
import React from 'react';
import type { ConductorSessionSummary } from '../../../../system/runtime/conductor.js';
interface SessionPaneProps {
    sessions: ConductorSessionSummary[];
    activeSessionId: string | null;
    collapsed: boolean;
    focused: boolean;
    onSelect: (id: string) => void;
}
export declare const SessionPane: React.FC<SessionPaneProps>;
export {};
//# sourceMappingURL=SessionPane.d.ts.map