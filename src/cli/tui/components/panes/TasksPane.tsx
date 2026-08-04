/**
 * Tasks/Steps pane component.
 * Displays a tree of execution steps with status markers.
 */

import React from 'react';
import { Box, Text } from 'ink';
import type { TaskEntry } from '../../store/execution-store.js';

interface TasksPaneProps {
  tasks: TaskEntry[];
  focused: boolean;
  height: number;
}

const STATE_MARKERS: Record<string, { char: string; color: string }> = {
  started: { char: '▸', color: 'yellow' },
  completed: { char: '✓', color: 'green' },
  failed: { char: '✗', color: 'red' },
};

export const TasksPane: React.FC<TasksPaneProps> = ({ tasks, focused, height }) => {
  const visibleHeight = Math.max(1, height - 3);
  const visibleTasks = tasks.slice(-visibleHeight);

  return (
    <Box
      flexDirection="column"
      borderStyle="single"
      borderColor={focused ? 'blue' : 'gray'}
      height={height}
    >
      <Text bold color={focused ? 'blue' : 'white'}>Steps</Text>

      {tasks.length === 0 ? (
        <Text dimColor>No steps yet.</Text>
      ) : (
        visibleTasks.map((task) => {
          const marker = STATE_MARKERS[task.state] ?? STATE_MARKERS.started;
          const indent = '  '.repeat(task.depth);
          return (
            <Text key={task.id} wrap="truncate">
              <Text color={marker.color}>{indent}{marker.char}</Text>
              {' '}
              <Text dimColor={task.state === 'completed'}>{task.description}</Text>
            </Text>
          );
        })
      )}
    </Box>
  );
};
