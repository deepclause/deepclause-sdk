/**
 * DeepClause SDK - Deep Research Agent Example
 * 
 * This example demonstrates a sophisticated research agent that:
 * 1. Accepts a research topic from the user
 * 2. Performs iterative web searches using Brave Search API
 * 3. Synthesizes findings into a structured report
 * 4. Saves the report to a file
 * 
 * Requirements:
 *   - Set GOOGLE_GENERATIVE_AI_API_KEY environment variable
 *   - Set BRAVE_API_KEY for real web search (falls back to mock)
 */

import { createDeepClause } from '../src/index.js';
import { z } from 'zod';
import * as fs from 'fs';
import * as path from 'path';
import * as readline from 'readline';

// =============================================================================
// Brave Search Implementation
// =============================================================================

interface BraveWebResult {
  title: string;
  url: string;
  description: string;
  published?: string;
}

interface BraveSearchResponse {
  search_type: string;
  results: BraveWebResult[];
  message?: string;
}

/**
 * Brave Search Tool - performs web searches using Brave Search API
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
    console.log('⚠️  No BRAVE_API_KEY found, using mock search results');
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
      url: `https://example.com/research/${topics[0]}-${i + 1}`,
      description: `This comprehensive study examines the various aspects of ${query}. Key findings suggest that ${topics[0]} plays a crucial role in modern applications. The research methodology involved analyzing multiple data sources and conducting expert interviews.`,
      published: `${Math.floor(Math.random() * 30) + 1} days ago`,
    });
  }
  
  return {
    search_type: 'web',
    results,
  };
}

// =============================================================================
// DML Code for Deep Research Agent
// =============================================================================

const DEEP_RESEARCH_DML = `
% ============================================================
% Deep Research Agent - Conversational Version
% ============================================================
% This agent performs iterative research on a given topic,
% gathering information from multiple searches and synthesizing
% it into a comprehensive report. After the report, it enters
% a conversation mode to answer follow-up questions.

% ============================================================
% Tool Definitions
% ============================================================

% Web search tool - searches the web using Brave Search API
tool(search_web(Query, Results),
     "Search the web for information using Brave Search. Returns structured search results with titles, URLs, and descriptions.") :-
    exec(brave_search(Query), Results).

% News search tool - searches for recent news articles
tool(search_news(Query, Results),
     "Search for recent news articles on a topic. Returns news results with titles, URLs, descriptions, and publication dates.") :-
    exec(brave_news(Query), Results).

% File writing tool - uses built-in write_file/2 predicate
tool(save_report(FilePath, Content, Status),
     "Save text content to a file. Arguments in order: 1) FilePath (string) - short filename like 'report.md', 2) Content (string) - the text to write, 3) Status (output) - returns 'success' or 'failed'. IMPORTANT: FilePath must be a short filename, NOT the content!") :-
    (write_file(FilePath, Content) -> Status = success ; Status = failed).

% ============================================================
% Main Research Agent - Success Path
% ============================================================

agent_main(Topic) :-
    % Set up the research context with detailed instructions
    format(string(TopicMsg), "RESEARCH TOPIC: ~w\\n\\nResearch this topic thoroughly.", [Topic]),
    system("You are an expert research analyst conducting deep research on behalf of the user.

Your methodology:
1. Start with a broad search to understand the topic landscape
2. Identify key themes, subtopics, and questions that need investigation  
3. Perform targeted searches on the most important subtopics
4. Synthesize findings into a coherent, well-structured report
5. Always cite sources using [N] notation where N is the source number

Your reports must:
- Be well-organized with clear sections (Introduction, main body sections, Conclusion)
- Present factual, balanced information from multiple sources
- Include proper citations [N] throughout the text
- End with a numbered Sources section listing all URLs
- Be written in clear, professional language

IMPORTANT: When storing values in output variables, use simple strings only, not complex objects."),
    user(TopicMsg),

    log("📖 PHASE 1: Initial topic exploration and subtopic identification"),
    % Phase 1: Initial broad search to understand the topic
    task("Perform an initial web search on the research topic using the search_web tool.

After reviewing the results:
1. Identify 3-4 key subtopics that need deeper investigation
2. Perform 2-3 additional targeted searches on those subtopics
3. Keep track of all source URLs for citations

When done, report what subtopics you identified and searched."),

    log("📝 PHASE 2: Writing and saving the research report"),
    % Phase 2: Write and save the report  
    task("Based on all the search results you gathered, write a comprehensive research report.

Requirements:
1. Write 600-1000 words of substantive content
2. Include clear sections: Introduction, 2-3 body sections, Conclusion
3. Include specific facts, data, and findings from your searches
4. Insert citation markers [N] wherever you reference information
5. End with a 'Sources' section listing all URLs in numbered format

Format the report in Markdown, then save it to 'research_report.md' using the save_report tool."),

    log("📋 PHASE 3: Creating executive summary"),
    % Phase 3: Provide summary to user
    task("Create a brief executive summary (2-4 sentences) of the research report you wrote.
The summary should highlight the most important findings and conclusions.
Store ONLY a simple text summary string in Summary.", Summary),

    log("✅ Research complete - entering conversation mode"),
    
    % Output the summary
    output("\\n═══════════════════════════════════════════════════════════"),
    output("📊 RESEARCH COMPLETE - EXECUTIVE SUMMARY"),
    output("═══════════════════════════════════════════════════════════"),
    output(Summary),
    output("═══════════════════════════════════════════════════════════"),
    output("\\n📄 Full report saved to: research_report.md"),
    output("\\n💬 You can now ask follow-up questions about the research."),
    output("   Type 'quit' or 'exit' to end the conversation.\\n"),
    
    % Enter conversation loop for follow-up questions
    conversation_loop(Topic).

% ============================================================
% Conversation Loop - Answer follow-up questions
% ============================================================

conversation_loop(Topic) :-
    % Get user's follow-up question
    input("\\n🔬 Follow-up question: ", UserInput),
    
    % Check for exit commands
    (   (UserInput = "quit" ; UserInput = "exit" ; UserInput = "q")
    ->  output("\\n👋 Thank you for using the Deep Research Agent. Goodbye!"),
        answer("Session ended by user.")
    ;   % Process the follow-up question
        format(string(FollowUpMsg), "The user has a follow-up question about the research on '~w': ~w", [Topic, UserInput]),
        user(FollowUpMsg),
        
        % Use task to answer - can search for more info if needed
        task("Answer the user's follow-up question based on your research findings.
If you need more information, you can use the search_web tool.
Provide a helpful and detailed response."),
        
        % Continue the conversation loop
        conversation_loop(Topic)
    ).

% ============================================================
% Fallback: Research difficulties
% ============================================================

agent_main(Topic) :-
    log("⚠️ FALLBACK: Attempting limited research due to difficulties"),
    format(string(TopicMsg), "Research topic: ~w", [Topic]),
    system("You are a research assistant explaining limitations in your findings."),
    user(TopicMsg),
    
    task("Attempt to search the web for information on the research topic.
If you find limited information, provide a high-level summary of what you found.
Store a brief explanation in Summary.", Summary),

    log("📋 Fallback complete - presenting limited results"),
    output(Summary),
    output("\\n💬 You can ask follow-up questions or try a different search."),
    conversation_loop(Topic).

% ============================================================
% Final Fallback: Critical errors
% ============================================================

agent_main(_Topic) :-
    log("❌ FINAL FALLBACK: Critical error encountered"),
    answer("I apologize, but I encountered an unexpected error during the research process and cannot complete your request. Please try again or rephrase your research topic.").
`;

// =============================================================================
// Interactive CLI
// =============================================================================

async function createReadlineInterface(): Promise<readline.Interface> {
  return readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
}

async function askQuestion(rl: readline.Interface, question: string): Promise<string> {
  return new Promise((resolve) => {
    rl.question(question, (answer) => {
      resolve(answer);
    });
  });
}

async function runResearchAgent(topic: string, debugMemory: boolean = false): Promise<void> {
  console.log('\n📚 Starting Deep Research Agent...');
  console.log(`📋 Research Topic: "${topic}"\n`);
  console.log('=' .repeat(60));
  
  // Check for API key
  const apiKey = process.env.GOOGLE_GENERATIVE_AI_API_KEY;
  if (!apiKey) {
    console.error('❌ Error: GOOGLE_GENERATIVE_AI_API_KEY environment variable not set');
    process.exit(1);
  }
  
  // Check for Brave API key
  const braveKey = process.env.BRAVE_KEY || process.env.BRAVE_API_KEY;
  if (!braveKey) {
    console.log('⚠️  Warning: BRAVE_API_KEY not set - using mock search results');
    console.log('   Set BRAVE_API_KEY for real web search capabilities\n');
  } else {
    console.log('✓ Brave Search API configured\n');
  }
  
  // Create DeepClause instance with streaming enabled
  const dc = await createDeepClause({
    model: 'gemini-2.5-flash',
    apiKey,
    temperature: 0.7,
    maxTokens: 8192,
    streaming: true,
  });
  
  // Register Brave web search tool
  dc.registerTool('brave_search', {
    description: 'Search the web using Brave Search API',
    parameters: z.object({
      query: z.string().describe('The search query'),
    }),
    execute: async (args) => {
      const { query } = args as { query: string };
      console.log(`\n🔍 Web Search: "${query.substring(0, 50)}${query.length > 50 ? '...' : ''}"...`);
      const results = await braveSearch(query, 'web', 10);
      console.log(`   ✓ Found ${results.results.length} results`);
      // Return as formatted string for the LLM
      return results.results.map((r, i) => 
        `[${i + 1}] ${r.title}\nURL: ${r.url}\n${r.description}${r.published ? `\nPublished: ${r.published}` : ''}`
      ).join('\n\n---\n\n');
    },
  });
  
  // Register Brave news search tool
  dc.registerTool('brave_news', {
    description: 'Search for recent news articles using Brave Search API',
    parameters: z.object({
      query: z.string().describe('The news search query'),
    }),
    execute: async (args) => {
      const { query } = args as { query: string };
      console.log(`\n📰 News Search: "${query.substring(0, 50)}${query.length > 50 ? '...' : ''}"...`);
      const results = await braveSearch(query, 'news', 10);
      console.log(`   ✓ Found ${results.results.length} news articles`);
      return results.results.map((r, i) => 
        `[${i + 1}] ${r.title}\nURL: ${r.url}\n${r.description}${r.published ? `\nPublished: ${r.published}` : ''}`
      ).join('\n\n---\n\n');
    },
  });
  
  try {
    console.log('\n🚀 Starting research...\n');
    
    // Pass the topic as an argument to agent_main(Topic)
    // Set workspacePath to current directory for file I/O
    for await (const event of dc.runDML(DEEP_RESEARCH_DML, {
      args: [topic],
      workspacePath: process.cwd(),
      onUserInput: async (prompt) => {
        // For follow-up questions, get input from stdin
        // Note: prompt is already displayed via output event
        const rl = await import('readline');
        const readline = rl.createInterface({
          input: process.stdin,
          output: process.stdout,
          terminal: false, // Disable terminal mode to prevent double echo
        });
        
        return new Promise((resolve) => {
          // Show a prompt indicator for user input
          process.stdout.write('>> ');
          readline.question('', (answer) => {
            readline.close();
            resolve(answer);
          });
        });
      },
    })) {
      switch (event.type) {
        case 'stream':
          // Stream LLM response tokens as they arrive
          if (event.content) {
            process.stdout.write(event.content);
          }
          if (event.done) {
            // Add newline when stream chunk completes
            process.stdout.write('\n');
          }
          break;

        case 'output':
          // Show agent outputs - includes the conversation loop messages
          if (event.content) {
            console.log(event.content);
          }
          break;

        case 'input_required':
          // Input is handled by onUserInput callback
          // This event is informational
          break;
          
        case 'answer':
          console.log('\n' + '═'.repeat(60));
          console.log('📊 RESEARCH COMPLETE - EXECUTIVE SUMMARY');
          console.log('═'.repeat(60));
          console.log('\n' + event.content);
          console.log('\n' + '═'.repeat(60));
          break;
          
        case 'error':
          console.error(`\n❌ Error: ${event.content}`);
          break;
          
        case 'log':
          // Always show log messages - they include phase markers
          console.log(`\n📌 ${event.content}`);
          break;
          
        case 'finished':
          // Check if report was created
          if (fs.existsSync('research_report.md')) {
            const stats = fs.statSync('research_report.md');
            console.log('\n✅ Research complete!');
            console.log(`📄 Full report saved to: ${path.resolve('research_report.md')}`);
            console.log(`   Size: ${stats.size} bytes`);
          }
          break;
      }
    }
    
  } catch (error) {
    console.error('\n❌ Research failed:', error);
  } finally {
    // Dump memory if debug flag is set
    if (debugMemory) {
      const memory = dc.getMemory();
      const memoryFile = 'debug_memory.json';
      fs.writeFileSync(memoryFile, JSON.stringify(memory, null, 2));
      console.log(`\n🔍 Debug: Memory dumped to ${path.resolve(memoryFile)}`);
      console.log(`   Total messages: ${memory.length}`);
    }
    await dc.dispose();
  }
}

// =============================================================================
// Main Entry Point
// =============================================================================

async function main(): Promise<void> {
  // Check for --debug-memory flag
  const debugMemory = process.argv.includes('--debug-memory');
  // Filter out flags from args
  const args = process.argv.slice(2).filter(arg => !arg.startsWith('--'));
  
  console.log('╔═══════════════════════════════════════════════════════════╗');
  console.log('║          DeepClause Deep Research Agent                   ║');
  console.log('║                                                           ║');
  console.log('║  This agent will research any topic you provide using    ║');
  console.log('║  Brave Search and generate a comprehensive report.        ║');
  console.log('╚═══════════════════════════════════════════════════════════╝');
  
  // Show API key status
  console.log('\n📋 Configuration:');
  console.log(`   • LLM: ${process.env.GOOGLE_GENERATIVE_AI_API_KEY ? '✓ Gemini API configured' : '❌ GOOGLE_GENERATIVE_AI_API_KEY not set'}`);
  console.log(`   • Search: ${process.env.BRAVE_API_KEY || process.env.BRAVE_KEY ? '✓ Brave Search API configured' : '⚠️ Using mock search (set BRAVE_API_KEY for real search)'}`);
  if (debugMemory) {
    console.log('   • Debug: ✓ Memory dump enabled (--debug-memory)');
  }
  
  // Check for command line argument
  const topicArg = args[0];
  
  if (topicArg) {
    // Topic provided as command line argument
    await runResearchAgent(topicArg, debugMemory);
  } else {
    // Interactive mode
    const rl = await createReadlineInterface();
    
    try {
      while (true) {
        console.log('\n');
        const topic = await askQuestion(rl, '🔬 Enter a research topic (or "quit" to exit): ');
        
        if (topic.toLowerCase() === 'quit' || topic.toLowerCase() === 'exit' || topic === '') {
          console.log('\n👋 Goodbye!');
          break;
        }
        
        await runResearchAgent(topic, debugMemory);
        
        console.log('\n');
        const another = await askQuestion(rl, '🔄 Research another topic? (y/n): ');
        if (another.toLowerCase() !== 'y' && another.toLowerCase() !== 'yes') {
          console.log('\n👋 Goodbye!');
          break;
        }
      }
    } finally {
      rl.close();
    }
  }
}

// Run the main function
main().catch(console.error);
