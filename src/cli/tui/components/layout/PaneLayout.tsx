/**
 * Flexible pane layout component.
 * Arranges panes in a configurable grid that uses the full terminal width.
 */

import React from 'react';
import { Box } from 'ink';
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
  columns: number;
}

export const PaneLayout: React.FC<PaneLayoutProps> = ({
  sessionPaneCollapsed,
  children,
  height,
  columns,
}) => {
  const sessionWidth = sessionPaneCollapsed ? 3 : 24;
  // Right column: allocate ~30% of remaining width, minimum 30, maximum 50
  const remainingWidth = columns - sessionWidth;
  const rightWidth = Math.max(30, Math.min(50, Math.floor(remainingWidth * 0.3)));
  const rightColumnHeight = Math.max(3, Math.floor(height * 0.5));
  const tasksHeight = Math.max(3, Math.floor(height * 0.25));

  return (
    <Box flexDirection="row" height={height} width={columns}>
      {/* Left: Session pane */}
      <Box width={sessionWidth} flexDirection="column" height={height}>
        {children.sessions}
      </Box>

      {/* Center: Messages pane (takes remaining space) */}
      <Box flexGrow={1} flexDirection="column" height={height}>
        {children.messages}
      </Box>

      {/* Right: Process, Tasks, Context stacked */}
      <Box width={rightWidth} flexDirection="column" height={height}>
        <Box height={rightColumnHeight}>
          {children.process}
        </Box>
        <Box height={tasksHeight}>
          {children.tasks}
        </Box>
        <Box flexGrow={1}>
          {children.context}
        </Box>
      </Box>
    </Box>
  );
};
