/**
 * MessageBubble component for rendering chat messages.
 * Memoized so only the active (streaming) message re-renders.
 */

import React from 'react';
import { Box, Text } from 'ink';

interface MessageBubbleProps {
  role: 'user' | 'assistant' | 'system';
  content: string;
  pending?: boolean;
  error?: boolean;
  tag?: string;
}

const ROLE_COLORS: Record<string, string> = {
  user: 'green',
  assistant: 'blue',
  system: 'yellow',
};

const ROLE_LABELS: Record<string, string> = {
  user: 'You',
  assistant: 'Assistant',
  system: 'System',
};

export const MessageBubble: React.FC<MessageBubbleProps> = React.memo(
  ({ role, content, pending, error, tag }) => {
    const color = error ? 'red' : ROLE_COLORS[role] ?? 'white';
    const label = tag || ROLE_LABELS[role] || role;

    return (
      <Box flexDirection="column" marginBottom={1}>
        <Text bold color={color}>
          {label}{pending ? ' …' : ''}
        </Text>
        <Box marginLeft={2}>
          <Text dimColor={role === 'system'} color={error ? 'red' : undefined} wrap="wrap">
            {content || (pending ? '…' : '')}
          </Text>
        </Box>
      </Box>
    );
  },
);

MessageBubble.displayName = 'MessageBubble';
