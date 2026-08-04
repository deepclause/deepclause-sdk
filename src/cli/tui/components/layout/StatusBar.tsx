/**
 * Bottom status bar component.
 */

import React from 'react';
import { Box, Text } from 'ink';
import type { PaneKind } from '../../store/app-state.js';

interface StatusBarProps {
  focusedPane: PaneKind;
  autoScroll: boolean;
  busy: boolean;
  mode: string;
}

export const StatusBar: React.FC<StatusBarProps> = ({ focusedPane, autoScroll, busy, mode }) => {
  return (
    <Box height={1} width="100%">
      <Box marginRight={2}>
        <Text dimColor>
          [{focusedPane}]
        </Text>
      </Box>
      <Box marginRight={2}>
        <Text dimColor>
          {autoScroll ? '↓ follow' : '⏸ scroll'}
        </Text>
      </Box>
      {busy && (
        <Box marginRight={2}>
          <Text color="yellow">running…</Text>
        </Box>
      )}
      <Box flexGrow={1} />
      <Text dimColor>
        {mode === 'normal' ? '? help | : command | Ctrl+C quit' : `[${mode}] ESC to exit`}
      </Text>
    </Box>
  );
};
