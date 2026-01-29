/**
 * JavaScript-Prolog Bridge utilities
 */
import type { LanguageModelV1 } from 'ai';
/**
 * Create a model provider for the Vercel AI SDK
 */
export declare function createModelProvider(provider: string, model: string, baseUrl?: string): LanguageModelV1;
/**
 * Convert a JavaScript value to a Prolog term string
 */
export declare function jsToPrologTerm(value: unknown): string;
/**
 * Convert a Prolog term to a JavaScript value
 */
export declare function prologTermToJs(term: unknown): unknown;
/**
 * Parse a Prolog term string into arguments
 */
export declare function parsePrologArgs(termStr: string): unknown[];
//# sourceMappingURL=bridge.d.ts.map