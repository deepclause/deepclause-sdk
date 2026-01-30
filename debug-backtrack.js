// Quick debug script for backtracking test
import { createDeepClause } from './dist/index.js';
import SWIPL from 'swipl-wasm';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function testPrologBacktrack() {
  const swipl = await SWIPL();
  
  // Define test predicates - using engine_yield instead of writeln
  swipl.prolog.query("assertz((test_pred :- engine_yield(clause_1), fail))").once();
  swipl.prolog.query("assertz((test_pred :- engine_yield(clause_2)))").once();
  
  // Create engine and store it
  swipl.prolog.query("engine_create(_, test_pred, E), assertz(my_engine(E))").once();
  console.log('Engine created and stored');
  
  // Step 1
  console.log('--- Step 1 ---');
  const step1 = swipl.prolog.query("my_engine(E), engine_next(E, R)").once();
  console.log('Step 1 result:', step1?.R);
  
  // Step 2
  console.log('--- Step 2 ---');
  const step2 = swipl.prolog.query("my_engine(E), engine_next(E, R)").once();
  console.log('Step 2 result:', step2?.R);
  
  // Step 3
  console.log('--- Step 3 ---');
  const step3 = swipl.prolog.query("my_engine(E), engine_next(E, R)").once();
  console.log('Step 3 result:', step3);
}

async function testMIDirectly() {
  const swipl = await SWIPL();
  
  // Load the MI from the right path
  const miPath = path.join(__dirname, 'src/prolog/deepclause_mi.pl');
  console.log('Loading MI from:', miPath);
  const loadResult = swipl.prolog.query(`use_module('${miPath}')`).once();
  console.log('Load result:', loadResult?.success);
  
  // Create a session
  swipl.prolog.query("nb_setval(current_session_id, test_session)").once();
  
  // Assert test predicates in the session module
  swipl.prolog.query("assertz(test_session:agent_main :- (output('Trying clause 1'), fail))").once();
  swipl.prolog.query("assertz(test_session:agent_main :- output('Trying clause 2'))").once();
  
  // Check what clauses exist
  console.log('\nChecking clauses:');
  const clauses = swipl.prolog.query("clause(test_session:agent_main, Body)");
  let clauseCount = 0;
  for (const sol of clauses) {
    clauseCount++;
    console.log(`  Clause ${clauseCount} Body:`, sol.Body?.toString?.() || JSON.stringify(sol.Body));
  }
  
  // Test if we can backtrack through clause/2 with engine
  console.log('\nTesting clause/2 backtracking in engine:');
  swipl.prolog.query("engine_create(_, (clause(test_session:agent_main, B), engine_yield(B), fail), E), assertz(clause_engine(E))").once();
  
  console.log('--- Clause step 1 ---');
  const cs1 = swipl.prolog.query("clause_engine(E), engine_next(E, R)").once();
  console.log('Result:', cs1?.R?.toString?.() || JSON.stringify(cs1?.R));
  
  console.log('--- Clause step 2 ---');
  const cs2 = swipl.prolog.query("clause_engine(E), engine_next(E, R)").once();
  console.log('Result:', cs2?.R?.toString?.() || JSON.stringify(cs2?.R));
  
  // Now test mi_call directly
  console.log('\nCreating engine with mi_call(Module:Goal):');
  const initState = "state{memory: [], params: _{}}";
  const createResult = swipl.prolog.query(`engine_create(_, deepclause_mi:mi_call(test_session:agent_main, ${initState}, _), E), assertz(test_engine(E))`).once();
  console.log('Engine created:', createResult?.success);
  
  console.log('--- Step 1 ---');
  const step1 = swipl.prolog.query("test_engine(E), engine_next(E, R)").once();
  console.log('Step 1 result:', step1?.R?.toString?.() || JSON.stringify(step1));
  
  console.log('--- Step 2 ---');
  const step2 = swipl.prolog.query("test_engine(E), engine_next(E, R)").once();
  console.log('Step 2 result:', step2?.R?.toString?.() || JSON.stringify(step2));
}

async function test() {
  // First test pure Prolog
  console.log('=== Pure Prolog Test ===');
  await testPrologBacktrack();
  
  console.log('\n=== Direct MI Test ===');
  await testMIDirectly();
  
  console.log('\n=== DML Test: Backtrack on fail ===');
  const dc1 = await createDeepClause({ model: 'gpt-4o-mini' });
  
  const dmlBacktrack = `
    agent_main :-
        output("Trying clause 1"),
        fail.
    
    agent_main :-
        output("Trying clause 2"),
        answer("success from clause 2").
  `;

  console.log('Events:');
  for await (const event of dc1.runDML(dmlBacktrack)) {
    console.log(JSON.stringify(event));
  }
  await dc1.dispose();

  console.log('\n=== DML Test: First clause succeeds (no backtrack) ===');
  const dc2 = await createDeepClause({ model: 'gpt-4o-mini' });
  
  const dmlNoBacktrack = `
    agent_main :-
        output("Clause 1 executing"),
        answer("from clause 1").
    
    agent_main :-
        output("Clause 2 executing"),
        answer("from clause 2").
  `;

  console.log('Events:');
  for await (const event of dc2.runDML(dmlNoBacktrack)) {
    console.log(JSON.stringify(event));
  }
  await dc2.dispose();
}

test().catch(console.error);
