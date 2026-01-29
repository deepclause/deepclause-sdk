/**
 * DeepClause SDK - Neurosymbolic AI Example
 * 
 * This example demonstrates the power of combining Prolog's 
 * symbolic reasoning with LLM capabilities.
 */

import { createDeepClause } from '../src/index.js';
import { z } from 'zod';

// =============================================================================
// Example: Product Recommendation System
// =============================================================================
// 
// This showcases:
// - Prolog rules for constraint-based filtering
// - LLM tasks for natural language understanding and generation
// - Tool integration for external data

async function productRecommendation() {
  console.log('=== Neurosymbolic Product Recommendation ===\n');

 // Check for API key
  const apiKey = process.env.GOOGLE_GENERATIVE_AI_API_KEY;
  if (!apiKey) {
    console.error('❌ Error: GOOGLE_GENERATIVE_AI_API_KEY environment variable not set');
    process.exit(1);
  }
  
  // Create DeepClause instance
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey,
    temperature: 0.7,
    maxTokens: 8192,
  });

  // Register product database tool
  dc.registerTool('get_products', {
    description: 'Get all available products from the database',
    parameters: z.object({
      category: z.string().optional(),
    }),
    execute: async (args) => {
      const { category } = args as { category?: string };
      const products = [
        { id: 1, name: 'MacBook Pro', category: 'laptop', price: 2499, rating: 4.8 },
        { id: 2, name: 'Dell XPS 15', category: 'laptop', price: 1799, rating: 4.6 },
        { id: 3, name: 'ThinkPad X1', category: 'laptop', price: 1599, rating: 4.5 },
        { id: 4, name: 'iPhone 15 Pro', category: 'phone', price: 1199, rating: 4.7 },
        { id: 5, name: 'Galaxy S24', category: 'phone', price: 999, rating: 4.5 },
        { id: 6, name: 'AirPods Pro', category: 'audio', price: 249, rating: 4.6 },
        { id: 7, name: 'Sony WH-1000XM5', category: 'audio', price: 349, rating: 4.8 },
      ];
      
      if (category) {
        return products.filter(p => p.category === category);
      }
      return products;
    },
  });

  // DML combining Prolog logic with LLM tasks
  const code = `
    % Tool wrapper
    tool(fetch_products(Category, Products)) :-
        exec(get_products(Category), Products).

    % Prolog rules for filtering (using get_dict for reliable field access)
    within_budget(Product, MaxPrice) :-
        get_dict(price, Product, Price),
        Price =< MaxPrice.

    high_rated(Product) :-
        get_dict(rating, Product, Rating),
        Rating >= 4.5.

    % Combined filter predicate for include/3
    good_product(MaxPrice, Product) :-
        within_budget(Product, MaxPrice),
        high_rated(Product).

    % Filter products using include/3 meta-predicate
    filter_products(Products, MaxPrice, Filtered) :-
        include(good_product(MaxPrice), Products, Filtered).

    % Main recommendation workflow
    % Note: Parameters must be in alphabetical order since Prolog dicts sort keys alphabetically
    agent_main(Budget, Query) :-
        system("You are a product recommendation assistant."),
        
        % Step 1: Understand user intent with LLM
        format(string(TaskDesc), 
            "Analyze this query: '~w'. Extract the product category (laptop/phone/audio) the user wants. Store in Category.",
            [Query]),
        task(TaskDesc, Category),
        log("Identified category"),
        
        % Step 2: Fetch products (external tool)
        tool(fetch_products(Category, AllProducts)),
        yield("Fetched product catalog..."),
        
        % Step 3: Filter with Prolog rules (symbolic reasoning)
        filter_products(AllProducts, Budget, FilteredProducts),
        length(FilteredProducts, Count),
        format(string(FilterLog), "Found ~w products within budget with high ratings", [Count]),
        log(FilterLog),
        
        % Step 4: Generate recommendation with LLM
        format(string(RecTask),
            "Here are the filtered products: ~w. Create a friendly recommendation explaining why these are good choices for someone looking for a ~w within $~w budget.",
            [FilteredProducts, Category, Budget]),
        task(RecTask, Recommendation),
        
        answer(Recommendation).
  `;

  try {
    for await (const event of dc.runDML(code, {
      params: {
        Budget: 2000,
        Query: "I need a good laptop for programming",
      },
    })) {
      switch (event.type) {
        case 'log':
          console.log('📋', event.content);
          break;
        case 'output':
          console.log('💬', event.content);
          break;
        case 'answer':
          console.log('\n✨ Recommendation:\n', event.content);
          break;
        case 'error':
          console.error('❌', event.content);
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example: Logical Puzzle Solver with LLM Explanation
// =============================================================================

async function puzzleSolver() {
  console.log('\n=== Logical Puzzle Solver ===\n');

  // Check for API key
  const apiKey = process.env.GOOGLE_GENERATIVE_AI_API_KEY;
  if (!apiKey) {
    console.error('❌ Error: GOOGLE_GENERATIVE_AI_API_KEY environment variable not set');
    process.exit(1);
  }
  
  // Create DeepClause instance
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey,
    temperature: 0.7,
    maxTokens: 8192,
  });
  // Classic logic puzzle (simplified - uses pure Prolog without library predicates)
  const code = `
    % Simple logic puzzle: Who owns the fish?
    % Given facts about three people and their pets
    solve_puzzle(brit-Pet1, swede-Pet2, dane-Pet3) :-
        % The Swede keeps dogs
        Pet2 = dogs,
        % The Brit does not have cats
        member(Pet1, [dogs, fish]),
        Pet1 \\= dogs,  % Can't be dogs because Swede has dogs
        Pet1 = fish,
        % Dane gets the remaining pet
        Pet3 = cats.

    agent_main :-
        system("You are a puzzle-solving assistant that explains logical solutions."),
        
        log("Solving puzzle with Prolog constraints..."),
        
        % Use Prolog's constraint solver
        (   solve_puzzle(BritPet, SwedePet, DanePet)
        ->  format(string(SolStr), "Brit owns ~w, Swede owns ~w, Dane owns ~w", [BritPet, SwedePet, DanePet]),
            format(string(TaskDesc),
                "I solved a logic puzzle. ~w. Please explain this solution clearly, showing the reasoning.",
                [SolStr]),
            task(TaskDesc, Explanation),
            answer(Explanation)
        ;   answer("Could not find a solution to the puzzle.")
        ).
  `;

  try {
    for await (const event of dc.runDML(code)) {
      switch (event.type) {
        case 'log':
          console.log('🧩', event.content);
          break;
        case 'answer':
          console.log('\n📝 Solution Explanation:\n', event.content);
          break;
        case 'error':
          console.error('❌', event.content);
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Example: Multi-step Reasoning with Backtracking
// =============================================================================

async function backtrackingReasoning() {
  console.log('\n=== Backtracking Reasoning ===\n');

// Check for API key
  const apiKey = process.env.GOOGLE_GENERATIVE_AI_API_KEY;
  if (!apiKey) {
    console.error('❌ Error: GOOGLE_GENERATIVE_AI_API_KEY environment variable not set');
    process.exit(1);
  }
  
  // Create DeepClause instance
  const dc = await createDeepClause({
    model: 'gemini-2.0-flash',
    apiKey,
    temperature: 0.7,
    maxTokens: 8192,
  });

  // Register a fallible search tool
  dc.registerTool('search_database', {
    description: 'Search a database for information',
    parameters: z.object({
      query: z.string(),
      source: z.enum(['primary', 'secondary', 'archive']),
    }),
    execute: async (args) => {
      const { query, source } = args as { query: string; source: string };
      // Simulate: primary fails, secondary succeeds
      if (source === 'primary') {
        return { success: false, error: 'Primary database offline' };
      }
      if (source === 'secondary') {
        return { 
          success: true, 
          data: `Found information about "${query}" in secondary database` 
        };
      }
      return { 
        success: true, 
        data: `Found archived information about "${query}"` 
      };
    },
  });

  const code = `
    % Tool wrapper
    tool(search(Query, Source, Result)) :-
        exec(search_database(Query, Source), Result).

    % Try different sources with backtracking
    % Memory state is automatically backtracked when this predicate fails
    try_search(Query, Result) :-
        member(Source, [primary, secondary, archive]),
        format(string(SourceMsg), "Trying source: ~w", [Source]),
        log(SourceMsg),
        tool(search(Query, Source, Response)),
        get_dict(success, Response, Success),
        Success == true,  % Fail if not successful, triggering backtracking
        get_dict(data, Response, Result),
        log("Success!").

    agent_main(Query) :-
        system("You are a helpful research assistant."),
        
        log("Starting search with automatic fallback..."),
        
        % Prolog backtracking will try each source until success
        (   try_search(Query, Data)
        ->  format(string(TaskDesc),
                "I found this data: ~w. Summarize it for the user.",
                [Data]),
            task(TaskDesc, Summary),
            answer(Summary)
        ;   answer("Could not find information in any database.")
        ).
  `;

  try {
    for await (const event of dc.runDML(code, {
      params: { Query: 'quantum computing breakthroughs' },
    })) {
      switch (event.type) {
        case 'log':
          console.log('🔍', event.content);
          break;
        case 'answer':
          console.log('\n📄 Result:\n', event.content);
          break;
        case 'error':
          console.error('❌', event.content);
          break;
      }
    }
  } finally {
    await dc.dispose();
  }
}

// =============================================================================
// Main
// =============================================================================

async function main() {
  const examples = [
    { name: 'Product Recommendation', fn: productRecommendation },
    { name: 'Puzzle Solver', fn: puzzleSolver },
    { name: 'Backtracking Reasoning', fn: backtrackingReasoning },
  ];

  const exampleNum = process.argv[2] ? parseInt(process.argv[2], 10) : null;

  if (exampleNum !== null && exampleNum >= 1 && exampleNum <= examples.length) {
    await examples[exampleNum - 1].fn();
  } else {
    console.log('Neurosymbolic AI Examples');
    console.log('=========================\n');
    console.log('These examples demonstrate combining Prolog\'s symbolic reasoning');
    console.log('with LLM capabilities for powerful neurosymbolic AI.\n');
    console.log('Available examples:');
    examples.forEach((ex, i) => {
      console.log(`  ${i + 1}. ${ex.name}`);
    });
    console.log('\nUsage: npx ts-node examples/neurosymbolic.ts [example_number]');
  }
}

main().catch(console.error);
