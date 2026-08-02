/**
 * Execution/Process pane component.
 * Shows tool calls, activity logs, and active tool status.
 */
import React from 'react';
import type { ActiveToolStatus } from '../../store/execution-store.js';
interface ExecutionPaneProps {
    activityLines: string[];
    activeTools: ActiveToolStatus[];
    running: boolean;
    focused: boolean;
    height: number;
}
export declare const ExecutionPane: React.FC<ExecutionPaneProps>;
export {};
//# sourceMappingURL=ExecutionPane.d.ts.map