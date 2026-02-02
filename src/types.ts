/**
 * Type definitions for DeepClause SDK
 */

import { z } from 'zod';

/**
 * Options for creating a DeepClause SDK instance
 */
export interface CreateOptions {
  /** API key for the LLM provider */
  apiKey?: string;
  /** LLM model to use (e.g., 'gpt-4o', 'claude-3-sonnet') */
  model: string;
  /** LLM provider (auto-detected from model if not specified) */
  provider?: 'openai' | 'anthropic' | 'google' | 'openrouter';
  /** Temperature for LLM calls (0-1) */
  temperature?: number;
  /** Maximum tokens for LLM responses */
  maxTokens?: number;
  /** Base URL for API (for custom endpoints) */
  baseUrl?: string;
  /** Enable execution trace logging */
  trace?: boolean;
  /** Enable streaming for LLM responses (task/prompt predicates) */
  streaming?: boolean;
  /** Enable debug logging (shows prompts, tool calls, etc.) */
  debug?: boolean;
}

/**
 * Options for running DML code
 */
export interface RunOptions {
  /** Positional arguments for agent_main (argv-style) */
  args?: unknown[];
  /** Named parameters available via param/2 and string interpolation */
  params?: Record<string, unknown>;
  /** Path to workspace directory */
  workspacePath?: string;
  /** Handler for user input requests */
  onUserInput?: (prompt: string) => Promise<string>;
  /** Abort signal for cancellation */
  signal?: AbortSignal;
}

/**
 * Trace entry for execution logging
 */
export interface TraceEntry {
  /** Timestamp in milliseconds */
  timestamp: number;
  /** Type of trace event */
  type: 'call' | 'exit' | 'fail' | 'output' | 'llm_call' | 'exec';
  /** Predicate or operation name */
  predicate?: string;
  /** Arguments or details */
  args?: unknown[];
  /** Result or additional info */
  result?: unknown;
  /** Call depth for indentation */
  depth?: number;
}

/**
 * Events emitted during DML execution
 */
export interface DMLEvent {
  type: 'output' | 'log' | 'answer' | 'input_required' | 'error' | 'finished' | 'stream' | 'tool_call';
  content?: string;
  prompt?: string;
  /** Execution trace (only present in 'finished' event when trace mode enabled) */
  trace?: TraceEntry[];
  /** Whether this is the final chunk of a stream (only for 'stream' events) */
  done?: boolean;
  /** Tool name (only for 'tool_call' events) */
  toolName?: string;
  /** Tool arguments (only for 'tool_call' events) */
  toolArgs?: Record<string, unknown>;
  /** Tool result (only for 'tool_call' events, set after execution) */
  toolResult?: unknown;
}

/**
 * Tool definition for registering external tools
 */
export interface ToolDefinition {
  /** Human-readable description of what the tool does */
  description: string;
  /** JSON Schema for tool parameters */
  parameters: JsonSchema;
  /** Function to execute the tool */
  execute: (args: Record<string, unknown>) => Promise<unknown>;
}

/**
 * JSON Schema type for tool parameters
 */
export interface JsonSchema {
  type: 'object';
  properties: Record<string, {
    type: string;
    description?: string;
    enum?: string[];
  }>;
  required?: string[];
}

/**
 * Tool access control policy
 */
export interface ToolPolicy {
  /** Whether to whitelist or blacklist tools */
  mode: 'whitelist' | 'blacklist';
  /** Tool names to allow/deny (supports wildcards like 'file_*') */
  tools: string[];
}

/**
 * Main SDK interface
 */
export interface DeepClauseSDK {
  /**
   * Execute DML code
   * @param code - DML/Prolog source code
   * @param options - Execution options
   * @yields DML events (output, log, answer, etc.)
   */
  runDML(code: string, options?: RunOptions): AsyncGenerator<DMLEvent>;

  /**
   * Register an external tool
   * @param name - Tool name (used in exec())
   * @param tool - Tool definition
   */
  registerTool(name: string, tool: ToolDefinition): void;

  /**
   * Set tool access policy
   * @param policy - Whitelist/blacklist configuration
   */
  setToolPolicy(policy: ToolPolicy): void;

  /**
   * Clear tool policy (allow all tools)
   */
  clearToolPolicy(): void;

  /**
   * Get current tool policy
   */
  getToolPolicy(): ToolPolicy | null;

  /**
   * Get list of registered tool names
   */
  getTools(): string[];

  /**
   * Provide input in response to input_required event
   * @param input - User's input string
   */
  provideInput(input: string): void;

  /**
   * Get the current conversation memory
   * @returns Array of memory messages (system, user, assistant)
   */
  getMemory(): MemoryMessage[];

  /**
   * Clean up resources
   */
  dispose(): Promise<void>;
}

/**
 * Internal types for Prolog bridge communication
 */
export interface PrologRequest {
  type: 'agent_loop' | 'exec' | 'wait_input';
  payload: unknown;
}

export interface PrologResponse {
  type: 'output' | 'log' | 'answer' | 'request' | 'finished' | 'error';
  content?: string;
  request?: PrologRequest;
}

/**
 * Memory message structure
 */
export interface MemoryMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

/**
 * Agent loop tool definitions
 */
export interface AgentTool {
  name: string;
  description: string;
  parameters: z.ZodSchema;
}
