/**
 * Execution/Process pane component.
 * Shows tool calls, activity logs, and active tool status.
 */

import React from 'react';
import { Box, Text } from 'ink';
import { Spinner } from '../shared/Spinner.js';
import type { ActiveToolStatus } from '../../store/execution-store.js';

interface ExecutionPaneProps {
  activityLines: string[];
  activeTools: ActiveToolStatus[];
  running: boolean;
  focused: boolean;
  height: number;
}

export const ExecutionPane: React.FC<ExecutionPaneProps> = ({
  activityLines,
  activeTools,
  running,
  focused,
  height,
}) => {
  const visibleHeight = Math.max(1, height - 4); // Account for border + title + active tools
  const visibleLines = activityLines.slice(-visibleHeight);

  return (
    <Box
      flexDirection="column"
      borderStyle="single"
      borderColor={focused ? 'blue' : 'gray'}
      height={height}
    >
      <Box>
        <Text bold color={focused ? 'blue' : 'white'}>
          Activity
        </Text>
        {running && (
          <Box marginLeft={1}>
            <Spinner />
          </Box>
        )}
      </Box>

      {/* Active tools */}
      {activeTools.length > 0 && (
        <Box flexDirection="column" marginBottom={1}>
          {activeTools.map((tool) => (
            <Text key={tool.scopeKey} color="yellow" wrap="truncate">
              ● {tool.scopeLabel}:{tool.toolName} [{tool.toolState}]
            </Text>
          ))}
        </Box>
      )}

      {/* Activity log */}
      <Box flexDirection="column" flexGrow={1}>
        {visibleLines.length === 0 ? (
          <Text dimColor>No activity yet.</Text>
        ) : (
          visibleLines.map((line, i) => (
            <Text key={i} wrap="truncate" dimColor>
              {line}
            </Text>
          ))
        )}
      </Box>
    </Box>
  );
};
