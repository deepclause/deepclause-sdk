/**
 * Bottom status bar component.
 */
import React from 'react';
import type { PaneKind } from '../../store/app-state.js';
interface StatusBarProps {
    focusedPane: PaneKind;
    autoScroll: boolean;
    busy: boolean;
    mode: string;
}
export declare const StatusBar: React.FC<StatusBarProps>;
export {};
//# sourceMappingURL=StatusBar.d.ts.map