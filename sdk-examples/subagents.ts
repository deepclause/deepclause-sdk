/**
 * Session-scoped subagents in the REPL — a programmatic demo.
 *
 * Runs three subagents with the three shapes (one-shot, message-waiting,
 * interactive), sends them messages, prints the process table, and disposes.
 * No API key is needed: the goals use `output`, `receive`, and `input`, not
 * `task/N`.
 *
 *   npx tsx sdk-examples/subagents.ts            # L1 (in-process)
 *   npx tsx sdk-examples/subagents.ts --worker   # L2 (worker thread)
 */
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { ProcessEvent } from '../src/repl/processes.js';

const useWorker = process.argv.includes('--worker');

async function main(): Promise<void> {
  const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-subagents-'));
  const repl: HarnessRepl = await createHarnessRepl({
    workspacePath: workspace,
    model: process.env.DC_MODEL ?? 'test-model',
    provider: 'openai',
    execution: useWorker ? 'worker' : 'inline',
    // A subagent's `input/2` routes here; in the REPL it is the editor.
    onInput: async (prompt) => {
      console.log(`\n  [human input] ${prompt}`);
      return 'from the demo';
    },
  });

  repl.session.setProcessEventSink((event: ProcessEvent) => {
    if (event.type === 'output') console.log(`  ⚙ ${event.name}  ${event.content}`);
    else if (event.type === 'answer') console.log(`  ⚙ ${event.name} › ${String(event.answer)}`);
    else if (event.type === 'exited' && event.state !== 'done') {
      console.log(`  ⚙ ${event.name} ${event.state} (${String(event.reason)})`);
    }
  });

  await repl.session.beginTurn();
  await repl.session.define('narrate/1', 'narrate(Topic) :- output("narrating {Topic}"), answer(done(Topic)).');
  await repl.session.define('keeper/1', [
    'keeper(Notes) :-',
    '    receive(Msg),',
    '    (   Msg = stop -> output("keeper stopping"), answer(Notes)',
    '    ;   output(Msg), keeper([Msg|Notes])',
    '    ).',
  ].join('\n'));
  await repl.session.define('asker/1', [
    'asker(Question) :-',
    '    input(Question, Answer),',
    '    output("user said: {Answer}"),',
    '    answer(Answer).',
  ].join('\n'));
  await repl.session.endTurn();

  console.log(`mode: ${useWorker ? 'L2 worker' : 'L1 inline'}\n`);

  // 1. one-shot: runs and exits
  await repl.session.spawnProcess('narrate("quantum error correction")', { name: 'narrate' });
  // 2. message-waiting: stays alive
  await repl.session.spawnProcess('keeper([])', { name: 'keeper' });
  // 3. interactive: asks, waits, then acts
  await repl.session.spawnProcess('asker("Which deadline should I assume?")', { name: 'asker' });

  await new Promise((resolve) => setTimeout(resolve, 1500));

  console.log('\n  -- /send keeper "the migration finished" --');
  repl.session.sendProcess('keeper', '"the migration finished"');
  repl.session.sendProcess('keeper', 'stop');
  await new Promise((resolve) => setTimeout(resolve, 1500));

  console.log('\n  -- /ps --watch (process table) --');
  for (const process of repl.session.processes()) {
    console.log(`     ${process.name}\t${process.state}\t${process.goal}`);
  }

  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
