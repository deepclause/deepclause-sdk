/**
 * DeepClause CLI - Brave Search Integration
 *
 * Provides web and news search functionality using Brave Search API.
 * Requires BRAVE_API_KEY or BRAVE_KEY environment variable.
 *
 * Returns plain text results for easier LLM consumption.
 */
export interface WebSearchParams {
    query: string;
    count?: number;
    freshness?: string;
    country?: string;
}
/**
 * Perform a web search using Brave Search API
 * Returns plain text formatted results
 */
export declare function webSearch(params: WebSearchParams): Promise<string>;
/**
 * Perform a news search using Brave Search API
 * Returns plain text formatted results
 */
export declare function newsSearch(params: WebSearchParams): Promise<string>;
//# sourceMappingURL=search.d.ts.map