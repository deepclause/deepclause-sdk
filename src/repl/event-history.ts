/** Browser-safe persisted-event decoder. Historical input/forms are never reopened. */
import type { DMLEvent } from '../types.js';

export function historyEventFromRecord(record: Record<string, unknown>): DMLEvent | undefined {
  const type = typeof record.type === 'string' ? record.type as DMLEvent['type'] : undefined;
  const frameKind = typeof record.frame === 'string' ? record.frame as DMLEvent['frameKind'] : undefined;
  const content = typeof record.content === 'string' ? record.content : undefined;
  const base = { type: type!, frameKind, content };
  switch (type) {
    case 'stream': return content ? base : undefined;
    case 'output': case 'answer': case 'error': case 'log':
      return { ...base, level: record.level as DMLEvent['level'] };
    case 'workflow_result': return { ...base, workflow: record.workflow as DMLEvent['workflow'] };
    case 'suspend': return { ...base, suspend: record.suspend as DMLEvent['suspend'] };
    case 'html': return { ...base, html: record.html as DMLEvent['html'] };
    case 'media': return { ...base, media: record.media as DMLEvent['media'] };
    case 'tool_call': return { ...base,
      toolName: record.toolName as string | undefined,
      toolState: record.toolState as DMLEvent['toolState'],
      toolSummary: record.toolSummary as string | undefined,
      toolArgs: record.toolArgs as Record<string, unknown> | undefined,
      toolResult: record.toolResult,
      toolError: record.toolError as string | undefined,
      toolDurationMs: record.toolDurationMs as number | undefined,
      toolExitCode: record.toolExitCode as number | undefined,
    };
    case 'solution': return { ...base, solution: { bindings: (Array.isArray(record.bindings) ? record.bindings : []) as NonNullable<DMLEvent['solution']>['bindings'], deterministic: false } };
    case 'task_activity': return { ...base, taskState: record.taskState as DMLEvent['taskState'], taskDescription: record.taskDescription as string | undefined };
    case 'usage': return { ...base, usage: record.usage as DMLEvent['usage'], usageSource: record.usageSource as DMLEvent['usageSource'], usageModel: record.usageModel as string | undefined };
    default: return undefined;
  }
}
