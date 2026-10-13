import { describe, expect, it, vi, beforeEach } from 'vitest';
import { z } from 'zod';
import { mkdir, mkdtemp, rm, writeFile } from 'fs/promises';
import { tmpdir } from 'os';
import { join } from 'path';

const aiMocks = vi.hoisted(() => ({
  generateText: vi.fn(),
  streamText: vi.fn(),
  hasToolCall: vi.fn(() => false),
  tool: vi.fn((definition: unknown) => definition),
}));

const bridgeMocks = vi.hoisted(() => ({
  onRawResponse: undefined as undefined | ((snapshot: Promise<{
    requestId: string;
    url: string;
    status: number;
    contentType: string | null;
    transport: 'https-one-shot' | 'undici';
    bodyText: string;
    captureError?: string;
  }>) => void),
  createModelProvider: vi.fn((
    _provider?: string,
    _model?: string,
    _baseUrl?: string,
    _debugLog?: (...args: unknown[]) => void,
    onRawResponse?: (snapshot: Promise<{
      requestId: string;
      url: string;
      status: number;
      contentType: string | null;
      transport: 'https-one-shot' | 'undici';
      bodyText: string;
      captureError?: string;
    }>) => void,
  ) => {
    bridgeMocks.onRawResponse = onRawResponse;
    return { id: 'mock-model' };
  }),
}));

vi.mock('ai', () => ({
  generateText: aiMocks.generateText,
  streamText: aiMocks.streamText,
  hasToolCall: aiMocks.hasToolCall,
  tool: aiMocks.tool,
}));

vi.mock('../src/prolog/bridge.js', () => ({
  createModelProvider: bridgeMocks.createModelProvider,
}));

import { runAgentLoop } from '../src/agent.js';

describe('runAgentLoop', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('tolerates streamed responses that omit response.messages', async () => {
    let callCount = 0;

    aiMocks.streamText.mockImplementation(({ tools }: { tools: Record<string, { execute: (input: { success: boolean }) => Promise<unknown> }> }) => {
      callCount += 1;

      if (callCount === 1) {
        return {
          fullStream: (async function* () {
            yield { type: 'tool-call', toolName: 'list_skills', input: {} };
            yield { type: 'finish-step', finishReason: 'tool-calls' };
          })(),
          response: Promise.resolve({}),
        };
      }

      return {
        fullStream: (async function* () {
          await tools.finish.execute({ success: false });
          yield { type: 'finish-step', finishReason: 'stop' };
        })(),
        response: Promise.resolve({ messages: [] }),
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Create a skill',
      outputVars: [],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: true,
    });

    expect(callCount).toBe(2);
    expect(result.success).toBe(false);
  });

  it('logs the raw upstream response when a streamed call returns empty text with finishReason other', async () => {
    let callCount = 0;
    const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

    aiMocks.streamText.mockImplementation(({ tools }: { tools: Record<string, { execute: (input: { success: boolean }) => Promise<unknown> }> }) => {
      callCount += 1;

      if (callCount === 1) {
        bridgeMocks.onRawResponse?.(Promise.resolve({
          requestId: 'req-empty-other',
          url: 'https://open.bigmodel.cn/api/paas/v4/chat/completions',
          status: 200,
          contentType: 'text/event-stream',
          transport: 'https-one-shot',
          bodyText: 'data: {"choices":[{"delta":{},"finish_reason":"other"}]}',
        }));

        return {
          fullStream: (async function* () {
            yield { type: 'finish-step', finishReason: 'other' };
          })(),
          response: Promise.resolve({ messages: [] }),
        };
      }

      return {
        fullStream: (async function* () {
          await tools.finish.execute({ success: false });
          yield { type: 'finish-step', finishReason: 'stop' };
        })(),
        response: Promise.resolve({ messages: [] }),
      };
    });

    try {
      const result = await runAgentLoop({
        taskDescription: 'Debug a provider returning empty output.',
        outputVars: [],
        memory: [],
        tools: new Map(),
        modelOptions: {
          model: 'mock-model',
          provider: 'openai',
          temperature: 0,
          maxOutputTokens: 1024,
        },
        onOutput: vi.fn(),
        onStream: vi.fn(),
        onAskUser: vi.fn(),
        streaming: true,
        debug: true,
      });

      expect(result.success).toBe(false);
      expect(consoleSpy.mock.calls.some((call) => call.map((part) => String(part)).join(' ').includes('Raw upstream provider response requestId=req-empty-other'))).toBe(true);
      expect(consoleSpy.mock.calls.some((call) => call.map((part) => String(part)).join(' ').includes('data: {"choices":[{"delta":{},"finish_reason":"other"}]}'))).toBe(true);
    } finally {
      consoleSpy.mockRestore();
    }
  });

  it('continues when streamed result.response never resolves after text output', async () => {
    let callCount = 0;
    const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
    const previousTimeout = process.env.DC_STREAM_RESPONSE_TIMEOUT_MS;
    process.env.DC_STREAM_RESPONSE_TIMEOUT_MS = '1';

    aiMocks.streamText.mockImplementation(({ tools }: { tools: Record<string, { execute: (input: { success: boolean }) => Promise<unknown> }> }) => {
      callCount += 1;

      if (callCount === 1) {
        return {
          fullStream: (async function* () {
            yield { type: 'text-delta', text: 'I have enough info.' };
            yield { type: 'finish-step', finishReason: 'stop' };
          })(),
          response: new Promise(() => {}),
        };
      }

      return {
        fullStream: (async function* () {
          await tools.finish.execute({ success: false });
          yield { type: 'finish-step', finishReason: 'stop' };
        })(),
        response: Promise.resolve({ messages: [] }),
      };
    });

    try {
      const result = await runAgentLoop({
        taskDescription: 'Continue after a streamed response metadata hang.',
        outputVars: [],
        memory: [],
        tools: new Map(),
        modelOptions: {
          model: 'mock-model',
          provider: 'openai',
          temperature: 0,
          maxOutputTokens: 1024,
        },
        onOutput: vi.fn(),
        onStream: vi.fn(),
        onAskUser: vi.fn(),
        streaming: true,
        debug: true,
      });

      expect(callCount).toBe(2);
      expect(result.success).toBe(false);
      expect(consoleSpy.mock.calls.some((call) => call.map((part) => String(part)).join(' ').includes('timed out waiting'))).toBe(true);
    } finally {
      if (previousTimeout === undefined) {
        delete process.env.DC_STREAM_RESPONSE_TIMEOUT_MS;
      } else {
        process.env.DC_STREAM_RESPONSE_TIMEOUT_MS = previousTimeout;
      }
      consoleSpy.mockRestore();
    }
  });

  it('uses a simple object schema for set_result and accepts explicit variable/value arguments', async () => {
    aiMocks.generateText.mockImplementation(async ({ tools }: { tools: Record<string, { inputSchema: z.ZodTypeAny; execute: (input: unknown) => Promise<unknown> }> }) => {
      expect(tools.set_result.inputSchema).toBeInstanceOf(z.ZodObject);
      expect(tools.set_result.inputSchema.safeParse({ variable: 'FinalAnswer', value: 'done' }).success).toBe(true);
      expect(tools.set_result.inputSchema.safeParse({}).success).toBe(false);

      await tools.set_result.execute({ variable: 'FinalAnswer', value: 'done' });
      await tools.set_result.execute({ variable: 'MemoryUpdate', value: 'NONE' });
      await tools.finish.execute({ success: true });

      return {
        text: '',
        toolCalls: [
          { toolName: 'set_result', input: { variable: 'FinalAnswer', value: 'done' } },
          { toolName: 'set_result', input: { variable: 'MemoryUpdate', value: 'NONE' } },
          { toolName: 'finish', input: { success: true } },
        ],
        toolResults: [],
        response: { messages: [] },
        finishReason: 'tool-calls',
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Return both final answer and memory update.',
      outputVars: [
        { name: 'FinalAnswer', type: 'string' },
        { name: 'MemoryUpdate', type: 'string' },
      ],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: false,
    });

    expect(result.success).toBe(true);
    expect(result.variables).toEqual({
      FinalAnswer: 'done',
      MemoryUpdate: 'NONE',
    });
  });

  it('persists a synthesized summary for successful tasks without named results', async () => {
    aiMocks.generateText.mockImplementation(async ({ tools }: { tools: Record<string, { execute: (input: unknown) => Promise<unknown> }> }) => {
      const projectSetupResult = await tools.setup_project.execute({ path: 'game-demo', scope: 'local' });
      await tools.finish.execute({ success: true });

      return {
        text: '',
        toolCalls: [
          { toolName: 'setup_project', input: { path: 'game-demo', scope: 'local' } },
          { toolName: 'finish', input: { success: true } },
        ],
        toolResults: [
          { toolName: 'setup_project', output: projectSetupResult },
        ],
        response: { messages: [] },
        finishReason: 'tool-calls',
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Create a local project folder for the generated game.',
      outputVars: [],
      memory: [],
      tools: new Map([
        ['setup_project', {
          description: 'Create a local project folder.',
          parameters: {
            type: 'object',
            properties: {
              path: { type: 'string' },
              scope: { type: 'string' },
            },
            required: ['path', 'scope'],
          },
          execute: async () => ({ path: 'game-demo', scope: 'local', created: true }),
        }],
      ]),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: false,
    });

    expect(result.success).toBe(true);
    expect(result.messages[result.messages.length - 1]?.content).toContain('Task completed. Summary:');
    expect(result.messages[result.messages.length - 1]?.content).toContain('setup_project');
    expect(result.messages[result.messages.length - 1]?.content).toContain('game-demo');
    expect(result.messages[result.messages.length - 1]?.content).toContain('local');
  });

  it('rejects wrong typed values in set_result execution', async () => {
    aiMocks.generateText.mockImplementation(async ({ tools }: { tools: Record<string, { execute: (input: unknown) => Promise<unknown> }> }) => {
      const response = await tools.set_result.execute({ variable: 'Count', value: 'not-a-number' });
      expect(response).toEqual({
        success: false,
        error: 'Expected an integer value for Count',
      });
      await tools.finish.execute({ success: false });

      return {
        text: '',
        toolCalls: [
          { toolName: 'set_result', input: { variable: 'Count', value: 'not-a-number' } },
          { toolName: 'finish', input: { success: false } },
        ],
        toolResults: [],
        response: { messages: [] },
        finishReason: 'tool-calls',
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Return a numeric count.',
      outputVars: [{ name: 'Count', type: 'integer' }],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: false,
    });

    expect(result.success).toBe(false);
    expect(result.variables).toEqual({});
  });

  it('coerces stringified JSON arrays for list(string(...)) outputs', async () => {
    aiMocks.generateText.mockImplementation(async ({ tools }: { tools: Record<string, { execute: (input: unknown) => Promise<unknown> }> }) => {
      const response = await tools.set_result.execute({
        variable: 'Categories',
        value: '["Decorations", "Food", "Games"]',
      });
      expect(response).toEqual({
        success: true,
        variable: 'Categories',
        value: ['Decorations', 'Food', 'Games'],
      });
      await tools.finish.execute({ success: true });

      return {
        text: '',
        toolCalls: [
          { toolName: 'set_result', input: { variable: 'Categories', value: '["Decorations", "Food", "Games"]' } },
          { toolName: 'finish', input: { success: true } },
        ],
        toolResults: [],
        response: { messages: [] },
        finishReason: 'tool-calls',
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Return a list of categories.',
      outputVars: [{ name: 'Categories', type: 'array', itemType: 'string' }],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: false,
    });

    expect(result.success).toBe(true);
    expect(result.variables).toEqual({
      Categories: ['Decorations', 'Food', 'Games'],
    });
  });

  it('loads the task prompt from the nearest .deepclause/system override', async () => {
    const workspaceRoot = await mkdtemp(join(tmpdir(), 'deepclause-agent-task-'));
    const systemDir = join(workspaceRoot, '.deepclause', 'system');
    const nestedWorkspacePath = join(workspaceRoot, 'workspace', 'nested');
    let systemPrompt = '';

    await mkdir(systemDir, { recursive: true });
    await mkdir(nestedWorkspacePath, { recursive: true });
    await writeFile(
      join(systemDir, 'TASK_PROMPT.md'),
      '# custom task prompt\n\n{TASK_DESCRIPTION}\n\n{TOOL_DESCRIPTIONS}\n\n{RESULT_SECTION}\n\n{STALL_GUIDANCE}\n',
      'utf8',
    );

    aiMocks.generateText.mockImplementation(async ({ messages, tools }: {
      messages: Array<{ role: string; content: string }>;
      tools: Record<string, { execute: (input: { success: boolean }) => Promise<unknown> }>;
    }) => {
      systemPrompt = messages[0]?.content ?? '';
      await tools.finish.execute({ success: false });

      return {
        text: '',
        toolCalls: [{ toolName: 'finish', input: { success: false } }],
        toolResults: [],
        response: { messages: [] },
        finishReason: 'tool-calls',
      };
    });

    try {
      await runAgentLoop({
        taskDescription: 'Use the custom task prompt.',
        outputVars: [],
        memory: [],
        tools: new Map(),
        workspacePath: nestedWorkspacePath,
        modelOptions: {
          model: 'mock-model',
          provider: 'openai',
          temperature: 0,
          maxOutputTokens: 1024,
        },
        onOutput: vi.fn(),
        onStream: vi.fn(),
        onAskUser: vi.fn(),
        streaming: false,
      });

      expect(systemPrompt).toContain('# custom task prompt');
      expect(systemPrompt).toContain('Use the custom task prompt.');
    } finally {
      await rm(workspaceRoot, { recursive: true, force: true });
    }
  });

  it('stops early when the model repeats the same non-progressing response', async () => {
    let callCount = 0;

    aiMocks.generateText.mockImplementation(async () => {
      callCount += 1;
      return {
        text: 'I should think a bit more before acting.',
        toolCalls: [],
        toolResults: [],
        response: { messages: [] },
        finishReason: 'stop',
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Avoid spinning forever.',
      outputVars: [],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: false,
    });

    expect(callCount).toBe(4);
    expect(result.success).toBe(false);
    expect(result.outputs).toContain('Agent loop detected repeated non-progressing responses and stopped early.');
  });

  it('inserts a continuation user turn before retrying a failed task/N non-streaming step', async () => {
    let callCount = 0;
    let trailingRolesOnFinalAttempt: string[] = [];

    aiMocks.generateText.mockImplementation(async ({ messages, tools }: {
      messages: Array<{ role: string; content: unknown }>;
      tools: Record<string, { execute: (input: unknown) => Promise<unknown> }>;
    }) => {
      callCount += 1;

      if (callCount < 3) {
        await tools.set_result.execute({ variable: 'Count', value: 'not-a-number' });
        return {
          text: '',
          toolCalls: [
            { toolName: 'set_result', input: { variable: 'Count', value: 'not-a-number' } },
          ],
          toolResults: [],
          response: { messages: [{ role: 'assistant', content: `Attempt ${callCount} failed.` }] },
          finishReason: 'tool-calls',
        };
      }

      trailingRolesOnFinalAttempt = messages.slice(-2).map((message) => message.role);
      await tools.finish.execute({ success: false });
      return {
        text: '',
        toolCalls: [
          { toolName: 'finish', input: { success: false } },
        ],
        toolResults: [],
        response: { messages: [] },
        finishReason: 'tool-calls',
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Return a numeric count.',
      outputVars: [{ name: 'Count', type: 'integer' }],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: false,
    });

    expect(callCount).toBe(3);
    expect(trailingRolesOnFinalAttempt).toEqual(['assistant', 'user']);
    expect(result.success).toBe(false);
  });

  it('inserts a continuation user turn before retrying a failed task/N streaming step', async () => {
    let callCount = 0;
    let trailingRolesOnFinalAttempt: string[] = [];

    aiMocks.streamText.mockImplementation(({ messages, tools }: {
      messages: Array<{ role: string; content: unknown }>;
      tools: Record<string, { execute: (input: unknown) => Promise<unknown> }>;
    }) => {
      callCount += 1;

      if (callCount < 3) {
        return {
          fullStream: (async function* () {
            await tools.set_result.execute({ variable: 'Count', value: 'not-a-number' });
            yield { type: 'tool-call', toolName: 'set_result', input: { variable: 'Count', value: 'not-a-number' } };
            yield { type: 'finish-step', finishReason: 'tool-calls' };
          })(),
          response: Promise.resolve({ messages: [{ role: 'assistant', content: `Attempt ${callCount} failed.` }] }),
        };
      }

      trailingRolesOnFinalAttempt = messages.slice(-2).map((message) => message.role);
      return {
        fullStream: (async function* () {
          await tools.finish.execute({ success: false });
          yield { type: 'tool-call', toolName: 'finish', input: { success: false } };
          yield { type: 'finish-step', finishReason: 'stop' };
        })(),
        response: Promise.resolve({ messages: [] }),
      };
    });

    const result = await runAgentLoop({
      taskDescription: 'Return a numeric count.',
      outputVars: [{ name: 'Count', type: 'integer' }],
      memory: [],
      tools: new Map(),
      modelOptions: {
        model: 'mock-model',
        provider: 'openai',
        temperature: 0,
        maxOutputTokens: 1024,
      },
      onOutput: vi.fn(),
      onStream: vi.fn(),
      onAskUser: vi.fn(),
      streaming: true,
    });

    expect(callCount).toBe(3);
    expect(trailingRolesOnFinalAttempt).toEqual(['assistant', 'user']);
    expect(result.success).toBe(false);
  });
});
