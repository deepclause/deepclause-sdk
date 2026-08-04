/**
 * File viewer overlay component.
 * Displays file content in a scrollable view.
 */

import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

interface ViewerProps {
  title: string;
  content: string;
  onClose: () => void;
}

export const Viewer: React.FC<ViewerProps> = ({ title, content, onClose }) => {
  const lines = content.split('\n');
  const [scrollTop, setScrollTop] = useState(0);
  const pageSize = 20;

  useInput((input, key) => {
    if (key.escape || input === 'q') {
      onClose();
      return;
    }

    if (key.upArrow || input === 'k') {
      setScrollTop((s) => Math.max(0, s - 1));
    } else if (key.downArrow || input === 'j') {
      setScrollTop((s) => Math.min(Math.max(0, lines.length - pageSize), s + 1));
    } else if (key.pageUp || (key.ctrl && input === 'b')) {
      setScrollTop((s) => Math.max(0, s - pageSize));
    } else if (key.pageDown || (key.ctrl && input === 'f')) {
      setScrollTop((s) => Math.min(Math.max(0, lines.length - pageSize), s + pageSize));
    }
  });

  const visibleLines = lines.slice(scrollTop, scrollTop + pageSize);

  return (
    <Box
      flexDirection="column"
      borderStyle="round"
      borderColor="cyan"
      padding={1}
    >
      <Box>
        <Text bold color="cyan">{title}</Text>
        <Box flexGrow={1} />
        <Text dimColor>[{scrollTop + 1}-{Math.min(scrollTop + pageSize, lines.length)}/{lines.length}]</Text>
      </Box>

      <Box flexDirection="column" marginTop={1}>
        {visibleLines.map((line, i) => (
          <Text key={scrollTop + i} wrap="truncate">
            <Text dimColor>{String(scrollTop + i + 1).padStart(4)} </Text>
            {line}
          </Text>
        ))}
      </Box>

      <Box marginTop={1}>
        <Text dimColor>j/k scroll | PgUp/PgDn page | q/Esc close</Text>
      </Box>
    </Box>
  );
};
