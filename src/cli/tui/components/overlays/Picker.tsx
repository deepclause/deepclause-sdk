/**
 * Fuzzy picker overlay component.
 * Provides a searchable list for selecting sessions, skills, files, etc.
 */

import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

export interface PickerItem {
  id: string;
  label: string;
  description: string;
  detail?: string;
}

interface PickerProps {
  title: string;
  items: PickerItem[];
  onSelect: (item: PickerItem) => void;
  onCancel: () => void;
  emptyText?: string;
}

export const Picker: React.FC<PickerProps> = ({
  title,
  items,
  onSelect,
  onCancel,
  emptyText = 'No items found.',
}) => {
  const [query, setQuery] = useState('');
  const [selectedIndex, setSelectedIndex] = useState(0);

  const filteredItems = filterItems(items, query);

  useInput((input, key) => {
    if (key.escape) {
      onCancel();
      return;
    }

    if (key.return) {
      if (filteredItems.length > 0) {
        onSelect(filteredItems[selectedIndex]);
      }
      return;
    }

    if (key.upArrow || (key.ctrl && input === 'p')) {
      setSelectedIndex((i) => Math.max(0, i - 1));
      return;
    }

    if (key.downArrow || (key.ctrl && input === 'n')) {
      setSelectedIndex((i) => Math.min(filteredItems.length - 1, i + 1));
      return;
    }

    if (key.backspace || key.delete) {
      setQuery((q) => q.slice(0, -1));
      setSelectedIndex(0);
      return;
    }

    if (input && !key.ctrl && !key.meta) {
      setQuery((q) => q + input);
      setSelectedIndex(0);
    }
  });

  const maxVisible = 10;
  const startIdx = Math.max(0, selectedIndex - maxVisible + 1);
  const visibleItems = filteredItems.slice(startIdx, startIdx + maxVisible);

  return (
    <Box
      flexDirection="column"
      borderStyle="round"
      borderColor="cyan"
      padding={1}
    >
      <Text bold color="cyan">{title}</Text>
      <Box marginTop={1}>
        <Text color="green">&gt; </Text>
        <Text>{query}</Text>
        <Text color="green">█</Text>
      </Box>

      <Box flexDirection="column" marginTop={1}>
        {visibleItems.length === 0 ? (
          <Text dimColor>{emptyText}</Text>
        ) : (
          visibleItems.map((item, i) => {
            const isSelected = startIdx + i === selectedIndex;
            return (
              <Box key={item.id}>
                <Text
                  color={isSelected ? 'blue' : undefined}
                  bold={isSelected}
                >
                  {isSelected ? '▸ ' : '  '}
                  {item.label}
                </Text>
                <Text dimColor> — {item.description}</Text>
              </Box>
            );
          })
        )}
      </Box>

      <Box marginTop={1}>
        <Text dimColor>
          {filteredItems.length} result{filteredItems.length !== 1 ? 's' : ''} | ↑↓ navigate | Enter select | Esc cancel
        </Text>
      </Box>
    </Box>
  );
};

function filterItems(items: PickerItem[], query: string): PickerItem[] {
  const normalized = query.trim().toLowerCase();
  if (!normalized) return items;

  const tokens = normalized.split(/\s+/).filter(Boolean);
  return items.filter((item) => {
    const haystack = `${item.label} ${item.description} ${item.detail ?? ''}`.toLowerCase();
    return tokens.every((token) => haystack.includes(token));
  });
}
