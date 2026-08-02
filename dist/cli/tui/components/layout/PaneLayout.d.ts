/**
 * Flexible pane layout component.
 * Arranges panes in a configurable grid based on terminal size.
 */
import React from 'react';
import type { PaneKind } from '../../store/app-state.js';
interface PaneLayoutProps {
    sessionPaneCollapsed: boolean;
    focusedPane: PaneKind;
    children: {
        sessions: React.ReactNode;
        messages: React.ReactNode;
        process: React.ReactNode;
        tasks: React.ReactNode;
        context: React.ReactNode;
    };
    height: number;
}
export declare const PaneLayout: React.FC<PaneLayoutProps>;
export {};
//# sourceMappingURL=PaneLayout.d.ts.map