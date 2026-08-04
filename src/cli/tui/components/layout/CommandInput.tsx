/**
 * Command input bar with basic Emacs/Vi key bindings.
 * Always visible; activates on any character input when in normal mode.
 */

import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

interface CommandInputProps {
  onSubmit: (value: string) => void;
  onEscape: () => void;
  onActivate: () => void;
  placeholder?: string;
  prefix?: string;
  active: boolean;
  busy?: boolean;
}

export const CommandInput: React.FC<CommandInputProps> = ({
  onSubmit,
  onEscape,
  onActivate,
  placeholder = 'Type a message or /command…',
  prefix = '> ',
  active,
  busy,
}) => {
  const [value, setValue] = useState('');
  const [cursorPos, setCursorPos] = useState(0);

  useInput((input, key) => {
    // If not active, activate on any printable key or ':' or '/'
    if (!active) {
      if (input && !key.ctrl && !key.meta && !key.escape) {
        onActivate();
        // Also capture this first character
        setValue(input);
        setCursorPos(input.length);
      }
      return;
    }

    if (key.escape) {
      onEscape();
      setValue('');
      setCursorPos(0);
      return;
    }

    if (key.return) {
      if (value.trim()) {
        onSubmit(value);
        setValue('');
        setCursorPos(0);
      }
      return;
    }

    if (key.backspace || key.delete) {
      if (cursorPos > 0) {
        setValue((v) => v.slice(0, cursorPos - 1) + v.slice(cursorPos));
        setCursorPos((p) => p - 1);
      }
      return;
    }

    // Emacs bindings
    if (key.ctrl && input === 'a') {
      setCursorPos(0);
      return;
    }
    if (key.ctrl && input === 'e') {
      setCursorPos(value.length);
      return;
    }
    if (key.ctrl && input === 'k') {
      setValue((v) => v.slice(0, cursorPos));
      return;
    }
    if (key.ctrl && input === 'u') {
      setValue((v) => v.slice(cursorPos));
      setCursorPos(0);
      return;
    }

    if (key.leftArrow) {
      setCursorPos((p) => Math.max(0, p - 1));
      return;
    }
    if (key.rightArrow) {
      setCursorPos((p) => Math.min(value.length, p + 1));
      return;
    }

    // Normal character input
    if (input && !key.ctrl && !key.meta) {
      setValue((v) => v.slice(0, cursorPos) + input + v.slice(cursorPos));
      setCursorPos((p) => p + input.length);
    }
  });

  const displayPrefix = busy ? '⏳ ' : prefix;
  const displayValue = value || (active ? '' : placeholder);
  const isPlaceholder = !value && !active;

  return (
    <Box height={1} width="100%">
      <Text color={active ? 'green' : 'gray'}>{displayPrefix}</Text>
      <Text dimColor={isPlaceholder}>
        {displayValue}
      </Text>
      {active && <Text color="green">█</Text>}
    </Box>
  );
};
