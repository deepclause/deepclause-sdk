/**
 * DeepClause in 30 lines.
 *
 *   npx tsx sdk-examples/quick-start.ts
 *
 * One DML program: ask the model for a sentence, then commit it with `answer/1`.
 * With no provider key it runs against a scripted model, so it always works.
 */
import { exampleSdk, modelLabel } from './_offline.js';

const DML = `
agent_main :-
    system("Answer in exactly one sentence, for a developer."),
    task("What is DeepClause?", string(Summary)),
    answer(Summary).
`;

async function main(): Promise<void> {
  console.log(`model: ${modelLabel()}\n`);
  const dc = await exampleSdk(
    () => 'DeepClause is a neuro-symbolic runtime that gives a model a Prolog engine to reason with, not just to talk.',
  );

  try {
    for await (const event of dc.runDML(DML)) {
      if (event.type === 'answer') console.log(event.content);
      else if (event.type === 'error') console.error('error:', event.content);
    }
  } finally {
    await dc.dispose();
  }
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
