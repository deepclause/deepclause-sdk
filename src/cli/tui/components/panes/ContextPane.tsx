/**
 * Context pane component.
 * Displays token usage and cost information per model.
 */

import React from 'react';
import { Box, Text } from 'ink';
import type { TokenUsageByModel, TokenUsageTotals } from '../../../../system/runtime/token-usage.js';

interface ContextPaneProps {
  tokenUsage: TokenUsageByModel;
  focused: boolean;
  height: number;
}

export const ContextPane: React.FC<ContextPaneProps> = ({ tokenUsage, focused, height }) => {
  const models = Object.entries(tokenUsage) as [string, TokenUsageTotals][];

  return (
    <Box
      flexDirection="column"
      borderStyle="single"
      borderColor={focused ? 'blue' : 'gray'}
      height={height}
    >
      <Text bold color={focused ? 'blue' : 'white'}>Context</Text>

      {models.length === 0 ? (
        <Text dimColor>No usage data.</Text>
      ) : (
        models.map(([model, usage]) => (
          <Box key={model} flexDirection="column">
            <Text color="cyan" wrap="truncate">{model}</Text>
            <Text dimColor wrap="truncate">
              {'  '}in: {formatTokenCount(usage.inputTokens)} | out: {formatTokenCount(usage.outputTokens)}
            </Text>
          </Box>
        ))
      )}
    </Box>
  );
};

function formatTokenCount(count: number): string {
  if (count >= 1_000_000) return `${(count / 1_000_000).toFixed(1)}M`;
  if (count >= 1_000) return `${(count / 1_000).toFixed(1)}k`;
  return String(count);
}
