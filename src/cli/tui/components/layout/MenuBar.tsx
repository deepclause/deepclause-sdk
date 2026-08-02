/**
 * Top menu bar component.
 */

import React from 'react';
import { Box, Text } from 'ink';

interface MenuBarProps {
  sessionTitle: string;
  busy: boolean;
}

const MENU_ITEMS = ['Session', 'Skills', 'Files', 'Run', 'View', 'Help'];

export const MenuBar: React.FC<MenuBarProps> = ({ sessionTitle, busy }) => {
  return (
    <Box height={1} width="100%">
      <Box marginRight={1}>
        <Text bold color="cyan">DeepClause</Text>
      </Box>
      {MENU_ITEMS.map((item) => (
        <Box key={item} marginRight={1}>
          <Text dimColor>{item}</Text>
        </Box>
      ))}
      <Box flexGrow={1} />
      <Text dimColor>{sessionTitle}</Text>
      {busy && (
        <Box marginLeft={1}>
          <Text color="yellow">●</Text>
        </Box>
      )}
    </Box>
  );
};
