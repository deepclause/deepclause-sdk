/**
 * Messages pane component.
 * Displays chat messages with streaming support using <Static> for completed messages.
 */

import React from 'react';
import { Box, Static, Text } from 'ink';
import { MessageBubble } from '../shared/MessageBubble.js';
import type { DisplayMessage } from '../../store/session-store.js';

interface MessagesPaneProps {
  messages: DisplayMessage[];
  streamingContent: string | null;
  focused: boolean;
  autoScroll: boolean;
}

export const MessagesPane: React.FC<MessagesPaneProps> = ({
  messages,
  streamingContent,
  focused,
}) => {
  // Completed messages use <Static> — rendered once, never re-diffed
  const completedMessages = streamingContent !== null
    ? messages.slice(0, -1)
    : messages;

  // Only the last message + streaming content needs dynamic rendering
  const lastMessage = streamingContent !== null && messages.length > 0
    ? messages[messages.length - 1]
    : null;

  return (
    <Box
      flexDirection="column"
      flexGrow={1}
      borderStyle="single"
      borderColor={focused ? 'blue' : 'gray'}
    >
      <Text bold color={focused ? 'blue' : 'white'}>Messages</Text>

      {/* Static: completed messages — rendered once, never re-diffed */}
      <Static items={completedMessages}>
        {(msg, index) => (
          <MessageBubble
            key={index}
            role={msg.role}
            content={msg.content}
            error={msg.error}
            tag={msg.tag}
          />
        )}
      </Static>

      {/* Dynamic: active message + streaming */}
      {lastMessage && (
        <MessageBubble
          role={lastMessage.role}
          content={lastMessage.content}
          pending={lastMessage.pending}
          error={lastMessage.error}
          tag={lastMessage.tag}
        />
      )}

      {streamingContent !== null && (
        <Box marginLeft={2}>
          <Text color="blue" wrap="wrap">
            {streamingContent}
            <Text color="cyan">▊</Text>
          </Text>
        </Box>
      )}

      {messages.length === 0 && streamingContent === null && (
        <Box marginTop={1}>
          <Text dimColor>
            No messages yet. Type a message below or use /help for commands.
          </Text>
        </Box>
      )}
    </Box>
  );
};
