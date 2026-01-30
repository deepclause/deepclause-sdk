/**
 * DeepClause CLI - Brave Search Integration
 *
 * Provides web and news search functionality using Brave Search API.
 * Requires BRAVE_API_KEY or BRAVE_KEY environment variable.
 */
export interface BraveWebResult {
    title: string;
    url: string;
    description: string;
    published?: string;
}
export interface BraveSearchResponse {
    search_type: string;
    results: BraveWebResult[];
    message?: string;
}
export interface WebSearchParams {
    query: string;
    count?: number;
    freshness?: string;
    country?: string;
}
/**
 * Perform a web search using Brave Search API
 */
export declare function webSearch(params: WebSearchParams): Promise<BraveSearchResponse>;
/**
 * Perform a news search using Brave Search API
 */
export declare function newsSearch(params: WebSearchParams): Promise<BraveSearchResponse>;
//# sourceMappingURL=search.d.ts.map