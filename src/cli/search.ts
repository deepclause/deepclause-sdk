/**
 * DeepClause CLI - Brave Search Integration
 * 
 * Provides web and news search functionality using Brave Search API.
 * Requires BRAVE_API_KEY or BRAVE_KEY environment variable.
 * 
 * Returns plain text results for easier LLM consumption.
 */

// =============================================================================
// Types
// =============================================================================

export interface WebSearchParams {
  query: string;
  count?: number;
  freshness?: string;
  country?: string;
}

// =============================================================================
// Brave Search Implementation
// =============================================================================

/**
 * Perform a web search using Brave Search API
 * Returns plain text formatted results
 */
export async function webSearch(params: WebSearchParams): Promise<string> {
  return braveSearch(params.query, 'web', params.count ?? 10, params.country ?? 'us', params.freshness);
}

/**
 * Perform a news search using Brave Search API
 * Returns plain text formatted results
 */
export async function newsSearch(params: WebSearchParams): Promise<string> {
  return braveSearch(params.query, 'news', params.count ?? 10, params.country ?? 'us', params.freshness);
}

/**
 * Format a single search result as plain text
 */
function formatResult(index: number, title: string, url: string, description: string, published?: string): string {
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

/**
 * Core Brave Search implementation
 * Returns plain text formatted results
 */
async function braveSearch(
  query: string, 
  searchType: string = 'web',
  count: number = 10,
  country: string = 'us',
  freshness?: string
): Promise<string> {
  const apiKey = process.env.BRAVE_KEY || process.env.BRAVE_API_KEY;
  
  if (!apiKey) {
    console.warn('⚠️  No BRAVE_API_KEY found, using mock search results');
    return generateMockSearchResults(query, count);
  }

  const headers = {
    'Accept': 'application/json',
    'Accept-Encoding': 'gzip, deflate, br',
    'X-Subscription-Token': apiKey,
  };

  const baseUrl = 'https://api.search.brave.com/res/v1';
  
  try {
    // Truncate query to max 400 characters (Brave API limit)
    const truncatedQuery = query.length > 400 ? query.substring(0, 400) : query;
    
    const searchParams = new URLSearchParams({
      q: truncatedQuery,
      count: String(Math.min(count, 20)), // Max 20 for web search
      country: country,
      search_lang: 'en',
      safesearch: 'moderate',
    });

    if (freshness) {
      searchParams.set('freshness', freshness);
    }

    let endpoint: string;
    switch (searchType.toLowerCase()) {
      case 'news':
        endpoint = `${baseUrl}/news/search?${searchParams}`;
        break;
      case 'web':
      default:
        endpoint = `${baseUrl}/web/search?${searchParams}`;
        break;
    }

    const response = await fetch(endpoint, { headers });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`HTTP ${response.status}: ${response.statusText}. Details: ${errorText}`);
    }

    const data = await response.json() as Record<string, unknown>;
    
    // Format web results as plain text
    if (searchType === 'web' && data.web && (data.web as Record<string, unknown>).results) {
      const webResults = (data.web as { results: Array<{
        title?: string;
        url?: string;
        description?: string;
        age?: string;
      }> }).results;
      
      if (webResults.length === 0) {
        return `No web results found for: ${query}`;
      }
      
      const header = `Web search results for: ${query}\n${'='.repeat(50)}\n`;
      const formattedResults = webResults.map((page, i) => 
        formatResult(
          i + 1,
          page.title || 'Untitled',
          page.url || '',
          page.description || 'No description',
          page.age
        )
      ).join('\n\n');
      
      return header + formattedResults;
    }
    
    // Format news results as plain text
    if (searchType === 'news' && data.results) {
      const newsResults = data.results as Array<{
        title?: string;
        url?: string;
        description?: string;
        age?: string;
        source?: { name: string };
      }>;
      
      if (newsResults.length === 0) {
        return `No news results found for: ${query}`;
      }
      
      const header = `News search results for: ${query}\n${'='.repeat(50)}\n`;
      const formattedResults = newsResults.map((article, i) => 
        formatResult(
          i + 1,
          article.title || 'Untitled',
          article.url || '',
          article.description || 'No description',
          article.age
        )
      ).join('\n\n');
      
      return header + formattedResults;
    }

    return `No results found for: ${query}`;

  } catch (error) {
    console.error('Brave search failed:', error);
    // Fall back to mock results
    return generateMockSearchResults(query, count);
  }
}

/**
 * Generate mock search results for demo when no API key is available
 */
function generateMockSearchResults(query: string, numResults: number): string {
  const topics = query.toLowerCase().split(' ').slice(0, 3);
  
  const header = `Web search results for: ${query} (MOCK DATA - No API key)\n${'='.repeat(50)}\n`;
  
  const results: string[] = [];
  for (let i = 0; i < numResults; i++) {
    results.push(formatResult(
      i + 1,
      `Research Article ${i + 1}: Understanding ${topics.join(' ')}`,
      `https://example.com/research/${topics[0] || 'topic'}-${i + 1}`,
      `This comprehensive study examines the various aspects of ${query}. Key findings suggest important implications for the field. The research methodology involved analyzing multiple data sources.`,
      `${Math.floor(Math.random() * 30) + 1} days ago`
    ));
  }
  
  return header + results.join('\n\n');
}
