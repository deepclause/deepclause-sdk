/**
 * DeepClause CLI - Brave Search Integration
 * 
 * Provides web and news search functionality using Brave Search API.
 * Requires BRAVE_API_KEY or BRAVE_KEY environment variable.
 */

// =============================================================================
// Types
// =============================================================================

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

// =============================================================================
// Brave Search Implementation
// =============================================================================

/**
 * Perform a web search using Brave Search API
 */
export async function webSearch(params: WebSearchParams): Promise<BraveSearchResponse> {
  return braveSearch(params.query, 'web', params.count ?? 10, params.country ?? 'us', params.freshness);
}

/**
 * Perform a news search using Brave Search API
 */
export async function newsSearch(params: WebSearchParams): Promise<BraveSearchResponse> {
  return braveSearch(params.query, 'news', params.count ?? 10, params.country ?? 'us', params.freshness);
}

/**
 * Core Brave Search implementation
 */
async function braveSearch(
  query: string, 
  searchType: string = 'web',
  count: number = 10,
  country: string = 'us',
  freshness?: string
): Promise<BraveSearchResponse> {
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
    
    // Format web results
    if (searchType === 'web' && data.web && (data.web as Record<string, unknown>).results) {
      const webResults = (data.web as { results: Array<{
        title?: string;
        url?: string;
        description?: string;
        age?: string;
      }> }).results;
      
      return {
        search_type: 'web',
        results: webResults.map((page) => ({
          title: page.title || 'Untitled',
          url: page.url || '',
          description: page.description || '',
          published: page.age,
        })),
      };
    }
    
    // Format news results
    if (searchType === 'news' && data.results) {
      const newsResults = data.results as Array<{
        title?: string;
        url?: string;
        description?: string;
        age?: string;
        source?: { name: string };
      }>;
      
      return {
        search_type: 'news',
        results: newsResults.map((article) => ({
          title: article.title || 'Untitled',
          url: article.url || '',
          description: article.description || '',
          published: article.age,
        })),
      };
    }

    return {
      search_type: searchType,
      results: [],
      message: 'No results found',
    };

  } catch (error) {
    console.error('Brave search failed:', error);
    // Fall back to mock results
    return generateMockSearchResults(query, count);
  }
}

/**
 * Generate mock search results for demo when no API key is available
 */
function generateMockSearchResults(query: string, numResults: number): BraveSearchResponse {
  const topics = query.toLowerCase().split(' ').slice(0, 3);
  const results: BraveWebResult[] = [];
  
  for (let i = 0; i < numResults; i++) {
    results.push({
      title: `Research Article ${i + 1}: Understanding ${topics.join(' ')}`,
      url: `https://example.com/research/${topics[0] || 'topic'}-${i + 1}`,
      description: `This comprehensive study examines the various aspects of ${query}. Key findings suggest important implications for the field. The research methodology involved analyzing multiple data sources.`,
      published: `${Math.floor(Math.random() * 30) + 1} days ago`,
    });
  }
  
  return {
    search_type: 'web',
    results,
  };
}
