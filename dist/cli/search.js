/**
 * DeepClause CLI - Search Integration
 *
 * Provides web and news search functionality.
 * Primary: Brave Search API (requires BRAVE_API_KEY or BRAVE_KEY).
 * Fallback: Bing web scraping (no API key, works globally including China).
 *
 * Returns plain text results for easier LLM consumption.
 */
// =============================================================================
// Public API
// =============================================================================
export async function webSearch(params) {
    const apiKey = process.env.BRAVE_KEY || process.env.BRAVE_API_KEY;
    if (apiKey) {
        try {
            return await braveSearch(params.query, 'web', params.count ?? 10, params.country ?? 'us', params.freshness, params.signal);
        }
        catch (error) {
            if (isAbortError(error))
                throw error;
            console.warn('Brave search failed, falling back to Bing:', error instanceof Error ? error.message : error);
        }
    }
    return bingSearch(params.query, params.count ?? 10, params.signal);
}
export async function newsSearch(params) {
    const apiKey = process.env.BRAVE_KEY || process.env.BRAVE_API_KEY;
    if (apiKey) {
        try {
            return await braveSearch(params.query, 'news', params.count ?? 10, params.country ?? 'us', params.freshness, params.signal);
        }
        catch (error) {
            if (isAbortError(error))
                throw error;
            console.warn('Brave search failed, falling back to Bing:', error instanceof Error ? error.message : error);
        }
    }
    return bingSearch(`${params.query} latest news`, params.count ?? 10, params.signal);
}
// =============================================================================
// Formatting
// =============================================================================
function formatResult(index, title, url, description, published) {
    const lines = [
        `[${index}] ${title}`,
        `    URL: ${url}`,
        `    ${description}`,
    ];
    if (published) {
        lines.push(`    Published: ${published}`);
    }
    return lines.join('\n');
}
// =============================================================================
// Bing Search (Scraping — no API key required)
// =============================================================================
const BING_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36';
async function bingSearch(query, count, signal) {
    const encodedQuery = encodeURIComponent(query.length > 400 ? query.substring(0, 400) : query);
    const url = `https://www.bing.com/search?q=${encodedQuery}`;
    const response = await fetch(url, {
        headers: {
            'User-Agent': BING_USER_AGENT,
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.9',
        },
        signal,
    });
    if (!response.ok) {
        throw new Error(`Bing search failed: HTTP ${response.status}`);
    }
    const html = await response.text();
    const results = parseBingResults(html, count);
    if (results.length === 0) {
        return `No web results found for: ${query}`;
    }
    const header = `Web search results for: ${query}\n${'='.repeat(50)}\n`;
    const formatted = results.map((r, i) => formatResult(i + 1, r.title, r.url, r.snippet)).join('\n\n');
    return header + formatted;
}
function parseBingResults(html, maxCount) {
    const results = [];
    const algoRegex = /<li[^>]*class="b_algo"[^>]*>([\s\S]*?)<\/li>/g;
    let match;
    while ((match = algoRegex.exec(html)) !== null && results.length < maxCount) {
        const block = match[1];
        const h2Match = block.match(/<h2[^>]*>([\s\S]*?)<\/h2>/);
        if (!h2Match)
            continue;
        const h2Content = h2Match[1];
        const linkMatch = h2Content.match(/href="(https?:\/\/[^"]+)"/);
        const title = stripHtml(h2Content).trim();
        const href = linkMatch ? linkMatch[1] : '';
        if (!title || !href)
            continue;
        const snippetMatch = block.match(/<p[^>]*>([\s\S]*?)<\/p>/) ||
            block.match(/class="b_caption"[^>]*>([\s\S]*?)<\/div>/);
        const snippet = snippetMatch ? stripHtml(snippetMatch[1]).trim() : '';
        results.push({ title, url: href, snippet: snippet || 'No description available' });
    }
    return results;
}
function stripHtml(html) {
    return html
        .replace(/<[^>]+>/g, '')
        .replace(/&amp;/g, '&')
        .replace(/&lt;/g, '<')
        .replace(/&gt;/g, '>')
        .replace(/&quot;/g, '"')
        .replace(/&#39;/g, "'")
        .replace(/&ensp;/g, ' ')
        .replace(/&#\d+;/g, ' ')
        .replace(/\s+/g, ' ')
        .trim();
}
// =============================================================================
// Brave Search (API — requires key)
// =============================================================================
async function braveSearch(query, searchType = 'web', count = 10, country = 'us', freshness, signal) {
    const apiKey = process.env.BRAVE_KEY || process.env.BRAVE_API_KEY;
    if (!apiKey) {
        return bingSearch(query, count, signal);
    }
    const headers = {
        'Accept': 'application/json',
        'Accept-Encoding': 'gzip, deflate, br',
        'X-Subscription-Token': apiKey,
    };
    const baseUrl = 'https://api.search.brave.com/res/v1';
    try {
        const truncatedQuery = query.length > 400 ? query.substring(0, 400) : query;
        const searchParams = new URLSearchParams({
            q: truncatedQuery,
            count: String(Math.min(count, 20)),
            country: country,
            search_lang: 'en',
            safesearch: 'moderate',
        });
        if (freshness) {
            searchParams.set('freshness', freshness);
        }
        let endpoint;
        switch (searchType.toLowerCase()) {
            case 'news':
                endpoint = `${baseUrl}/news/search?${searchParams}`;
                break;
            case 'web':
            default:
                endpoint = `${baseUrl}/web/search?${searchParams}`;
                break;
        }
        const response = await fetch(endpoint, { headers, signal });
        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(`HTTP ${response.status}: ${response.statusText}. Details: ${errorText}`);
        }
        const data = await response.json();
        if (searchType === 'web' && data.web && data.web.results) {
            const webResults = data.web.results;
            if (webResults.length === 0) {
                return `No web results found for: ${query}`;
            }
            const header = `Web search results for: ${query}\n${'='.repeat(50)}\n`;
            const formattedResults = webResults.map((page, i) => formatResult(i + 1, page.title || 'Untitled', page.url || '', page.description || 'No description', page.age)).join('\n\n');
            return header + formattedResults;
        }
        if (searchType === 'news' && data.results) {
            const newsResults = data.results;
            if (newsResults.length === 0) {
                return `No news results found for: ${query}`;
            }
            const header = `News search results for: ${query}\n${'='.repeat(50)}\n`;
            const formattedResults = newsResults.map((article, i) => formatResult(i + 1, article.title || 'Untitled', article.url || '', article.description || 'No description', article.age)).join('\n\n');
            return header + formattedResults;
        }
        return `No results found for: ${query}`;
    }
    catch (error) {
        if (isAbortError(error)) {
            throw error;
        }
        throw error;
    }
}
function isAbortError(error) {
    return error instanceof Error && error.name === 'AbortError';
}
//# sourceMappingURL=search.js.map