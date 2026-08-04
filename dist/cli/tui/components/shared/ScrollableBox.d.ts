/**
 * Scrollable box component with content caching.
 * Renders a viewport into a list of lines, handling scroll position.
 */
import React from 'react';
interface ScrollableBoxProps {
    lines: string[];
    height: number;
    focused?: boolean;
    autoScroll?: boolean;
    title?: string;
    borderColor?: string;
}
export declare const ScrollableBox: React.FC<ScrollableBoxProps>;
export {};
//# sourceMappingURL=ScrollableBox.d.ts.map