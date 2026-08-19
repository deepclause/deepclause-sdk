/**
 * DeepClause SDK - Simplified DML Language Runtime
 * 
 * A neurosymbolic AI system combining Prolog-based symbolic reasoning
 * with LLM-powered task execution.
 */

export { createDeepClause } from './sdk.js';
export type { 
  CompactionAction,
  CompactionOptions,
  CompactionScope,
  CompactionTrigger,
  CompactorBinding,
  CompactorDefinition,
  CompactorSourceType,
  CreateOptions,
  DeepClauseSDK,
  LLMBackend,
  LLMBackendMessage,
  LLMBackendRequest,
  LLMBackendResponse,
  LLMBackendTool,
  LLMBackendToolCall,
  LLMUsage,
  MemoryMessage,
  RunOptions,
  CompileOptions,
  CompileResult,
  CompileTool,
  DMLEvent,
  TraceEntry,
  ToolDefinition,
  ToolPolicy 
} from './types.js';

// Compiler utilities — also available via the 'deepclause-sdk/compiler' subpath
export {
  compileToDML,
  extractParameters,
  extractToolDependencies,
  extractDescription,
  validateWithProlog,
  analyzeDML,
} from './compiler.js';
