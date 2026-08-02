/**
 * Session pane component.
 * Collapsible sidebar showing session list. Default collapsed to icon-width.
 */

import React from 'react';
import { Box, Text } from 'ink';
import type { ConductorSessionSummary } from '../../../../system/runtime/conductor.js';

interface SessionPaneProps {
  sessions: ConductorSessionSummary[];
  activeSessionId: string | null;
  collapsed: boolean;
  focused: boolean;
  onSelect: (id: string) => void;
}

export const SessionPane: React.FC<SessionPaneProps> = ({
  sessions,
  activeSessionId,
  collapsed,
  focused,
}) => {
  if (collapsed) {
    return (
      <Box
        flexDirection="column"
        borderStyle="single"
        borderColor={focused ? 'blue' : 'gray'}
        width={3}
        height="100%"
      >
        <Text color="cyan">S</Text>
        {sessions.map((s, i) => (
          <Text key={s.id} color={s.id === activeSessionId ? 'green' : 'gray'}>
            {i + 1}
          </Text>
        ))}
      </Box>
    );
  }

  return (
    <Box
      flexDirection="column"
      borderStyle="single"
      borderColor={focused ? 'blue' : 'gray'}
      width={24}
      height="100%"
    >
      <Text bold color={focused ? 'blue' : 'white'}>Sessions</Text>
      {sessions.length === 0 ? (
        <Text dimColor>No sessions</Text>
      ) : (
        sessions.map((s) => (
          <Text
            key={s.id}
            color={s.id === activeSessionId ? 'green' : undefined}
            bold={s.id === activeSessionId}
            wrap="truncate"
          >
            {s.id === activeSessionId ? '▸ ' : '  '}
            {s.title || s.id.slice(0, 8)}
          </Text>
        ))
      )}
    </Box>
  );
};
