/**
 * Flexible pane layout component.
 * Arranges panes in a configurable grid based on terminal size.
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
}

export const PaneLayout: React.FC<PaneLayoutProps> = ({
  sessionPaneCollapsed,
  children,
  height,
}) => {
  const sessionWidth = sessionPaneCollapsed ? 3 : 24;
  const rightColumnHeight = Math.floor(height / 2);

  return (
    <Box flexDirection="row" height={height} width="100%">
      {/* Left: Session pane */}
      <Box width={sessionWidth} flexDirection="column">
        {children.sessions}
      </Box>

      {/* Center: Messages pane (takes remaining space) */}
      <Box flexGrow={1} flexDirection="column">
        {children.messages}
      </Box>

      {/* Right: Process, Tasks, Context stacked */}
      <Box width={40} flexDirection="column">
        <Box height={rightColumnHeight}>
          {children.process}
        </Box>
        <Box height={Math.floor(rightColumnHeight / 2)}>
          {children.tasks}
        </Box>
        <Box flexGrow={1}>
          {children.context}
        </Box>
      </Box>
    </Box>
  );
};
