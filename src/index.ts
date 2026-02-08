/**
 * DeepClause SDK - Simplified DML Language Runtime
 * 
 * A neurosymbolic AI system combining Prolog-based symbolic reasoning
 * with LLM-powered task execution.
 */

export { createDeepClause } from './sdk.js';
export type { 
  CreateOptions,
  DeepClauseSDK,
  RunOptions,
  CompileOptions,
  CompileResult,
  CompileTool,
  DMLEvent,
  TraceEntry,
  ToolDefinition,
  ToolPolicy 
} from './types.js';
