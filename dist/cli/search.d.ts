/**
 * DeepClause CLI - Search Integration
 *
 * Provides web and news search functionality.
 * Primary: Brave Search API (requires BRAVE_API_KEY or BRAVE_KEY).
 * Fallback: Bing web scraping (no API key, works globally including China).
 *
 * Returns plain text results for easier LLM consumption.
 */
export interface WebSearchParams {
    query: string;
    count?: number;
    freshness?: string;
    country?: string;
    signal?: AbortSignal;
}
export declare function webSearch(params: WebSearchParams): Promise<string>;
export declare function newsSearch(params: WebSearchParams): Promise<string>;
//# sourceMappingURL=search.d.ts.map