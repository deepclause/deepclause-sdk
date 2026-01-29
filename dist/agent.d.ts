/**
 * Agent Loop Implementation
 * Runs an LLM agent loop for task() predicate execution
 */
import type { ToolDefinition, MemoryMessage } from './types.js';
export interface AgentLoopOptions {
    taskDescription: string;
    outputVars: string[];
    memory: MemoryMessage[];
    tools: Map<string, ToolDefinition>;
    modelOptions: {
        model: string;
        provider: string;
        temperature: number;
        maxTokens: number;
        baseUrl?: string;
    };
    onOutput: (text: string) => void;
    onStream?: (chunk: string, done: boolean) => void;
    onAskUser: (prompt: string) => Promise<string>;
    signal?: AbortSignal;
    streaming?: boolean;
    debug?: boolean;
}
export interface AgentLoopResult {
    success: boolean;
    outputs: string[];
    variables: Record<string, unknown>;
    /** Conversation messages from the agent loop (excludes system messages which are task-specific) */
    messages: Array<{
        role: 'user' | 'assistant';
        content: string;
    }>;
}
/**
 * Run an agent loop for a task
 */
export declare function runAgentLoop(options: AgentLoopOptions): Promise<AgentLoopResult>;
//# sourceMappingURL=agent.d.ts.map