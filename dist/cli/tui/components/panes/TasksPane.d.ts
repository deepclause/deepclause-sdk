/**
 * Tasks/Steps pane component.
 * Displays a tree of execution steps with status markers.
 */
import React from 'react';
import type { TaskEntry } from '../../store/execution-store.js';
interface TasksPaneProps {
    tasks: TaskEntry[];
    focused: boolean;
    height: number;
}
export declare const TasksPane: React.FC<TasksPaneProps>;
export {};
//# sourceMappingURL=TasksPane.d.ts.map