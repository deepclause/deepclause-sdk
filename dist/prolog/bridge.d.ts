/**
 * JavaScript-Prolog Bridge utilities
 */
import type { LanguageModel } from 'ai';
import type { MemoryMessage, LLMUsage } from '../types.js';
export interface RawProviderResponseSnapshot {
    requestId: string;
    url: string;
    status: number;
    contentType: string | null;
    transport: 'https-one-shot' | 'undici';
    bodyText: string;
    captureError?: string;
}
export interface SampleSingleTokenOptions {
    prompt: string;
    allowedTokens?: string[];
    modelOptions: {
        provider: string;
        model: string;
        temperature: number;
        baseUrl?: string;
        providerOptions?: Record<string, Record<string, any>>;
    };
    signal?: AbortSignal;
    debugLog?: (...args: unknown[]) => void;
    onRawResponse?: (snapshot: Promise<RawProviderResponseSnapshot>) => void;
}
export interface GenerateLlmReplyOptions {
    messages: MemoryMessage[];
    modelOptions: {
        provider: string;
        model: string;
        temperature: number;
        maxOutputTokens?: number;
        baseUrl?: string;
        providerOptions?: Record<string, Record<string, any>>;
    };
    signal?: AbortSignal;
    debugLog?: (...args: unknown[]) => void;
    onRawResponse?: (snapshot: Promise<RawProviderResponseSnapshot>) => void;
}
/**
 * Create a model provider for the Vercel AI SDK
 */
export declare function createModelProvider(provider: string, model: string, baseUrl?: string, debugLog?: (...args: unknown[]) => void, onRawResponse?: (snapshot: Promise<RawProviderResponseSnapshot>) => void): LanguageModel;
export declare function sampleSingleToken(options: SampleSingleTokenOptions): Promise<{
    token: string;
    usage?: LLMUsage;
}>;
export declare function generateLlmReply(options: GenerateLlmReplyOptions): Promise<{
    text: string;
    usage?: LLMUsage;
}>;
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