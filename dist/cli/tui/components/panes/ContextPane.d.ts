/**
 * Context pane component.
 * Displays token usage and cost information per model.
 */
import React from 'react';
import type { TokenUsageByModel } from '../../../../system/runtime/token-usage.js';
interface ContextPaneProps {
    tokenUsage: TokenUsageByModel;
    focused: boolean;
    height: number;
}
export declare const ContextPane: React.FC<ContextPaneProps>;
export {};
//# sourceMappingURL=ContextPane.d.ts.map