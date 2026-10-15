/**
 * Two LLM-backed subagents debating each other.
 *
 * Runs `sdk-examples/debate.dml`. By default it injects a deterministic mock
 * model, so it runs anywhere with no credentials; set `DC_REAL=1` (and a
 * provider key / configured backend) to use a real model.
 *
 *   npx tsx sdk-examples/debate.ts             # L1, mock model
 *   npx tsx sdk-examples/debate.ts --worker    # L2 worker, mock model
 *   DC_REAL=1 DC_MODEL=gpt-4o OPENAI_API_KEY=… npx tsx sdk-examples/debate.ts
 *
 * In the REPL, load the file and run the goal; the REPL supplies the real model:
 *   deepclause repl -l sdk-examples/debate.dml
 *   /goal debate("Nuclear power should expand to meet climate targets", 2, Verdict).
 */
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

import { createHarnessRepl } from '../src/repl/index.js';
import type { LLMBackend, LLMBackendRequest, LLMBackendResponse } from '../src/types.js';

const useWorker = process.argv.includes('--worker');
const useReal = process.env.DC_REAL === '1';

/**
 * A stand-in model: it reads the requested result variable from the system
 * prompt and answers `set_result` + `finish`, which is how an injected backend
 * returns a `task/N` value. Enough to exercise the full exchange offline.
 */
function mockBackend(): LLMBackend {
  let turn = 0;
  return {
    async complete(request: LLMBackendRequest): Promise<LLMBackendResponse> {
      turn += 1;
      const system = request.messages.filter((m) => m.role === 'system').map((m) => m.content).join('\n');
      const user = request.messages.filter((m) => m.role === 'user').map((m) => m.content).join('\n');
      const prompt = `${system}\n${user}`;
      const variable = /Variable must be one of: "([^"]+)"/.exec(system)?.[1] ?? 'Result';

      let text: string;
      if (/judging a debate/i.test(prompt)) {
        text = 'The PRO side is more convincing: it engaged the mechanism and the trade-offs, '
          + 'while CON relied on a general precautionary claim. CON was not wrong, but PRO '
          + 'carried the specific burden of the question.';
      } else {
        const side = /the pro side/i.test(prompt) ? 'PRO' : /the con side/i.test(prompt) ? 'CON' : 'SIDE';
        text = `${side} turn ${turn}: the evidence supports this side, and the strongest `
          + 'opposing point is answerable with a bounded, reversible step.';
      }

      return {
        text: '',
        toolCalls: [
          { id: `set-${turn}`, name: 'set_result', arguments: { variable, value: text } },
          { id: `finish-${turn}`, name: 'finish', arguments: { success: true } },
        ],
        usage: { inputTokens: 12, outputTokens: 6, totalTokens: 18 },
      };
    },
  };
}

async function main(): Promise<void> {
  const here = path.dirname(fileURLToPath(import.meta.url));
  const dml = await fs.readFile(path.join(here, 'debate.dml'), 'utf8');
  const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-debate-'));

  const repl = await createHarnessRepl({
    workspacePath: workspace,
    model: process.env.DC_MODEL ?? 'gpt-4o',
    provider: 'openai',
    execution: useWorker ? 'worker' : 'inline',
    llmBackend: useReal ? undefined : mockBackend(),
  });

  repl.session.setProcessEventSink((event) => {
    if (event.type === 'output') console.log(`  ⚙ ${event.name}  ${event.content}`);
    else if (event.type === 'exited' && event.state !== 'done') {
      console.log(`  ⚙ ${event.name} ${event.state} (${String(event.reason)})`);
    }
  });

  await repl.session.loadCode(dml);

  const topic = 'Nuclear power should expand to meet climate targets';
  console.log(`mode: ${useWorker ? 'L2 worker' : 'L1 inline'} · ${useReal ? 'real model' : 'mock model'}`);
  console.log(`topic: ${topic}\n`);

  const started = Date.now();
  let verdict = '';
  for await (const event of repl.query(`debate("${topic}", 2, Verdict)`)) {
    if (event.type === 'solution') {
      verdict = event.solution?.bindings.find((binding) => binding.name === 'Verdict')?.value ?? '';
    } else if (event.type === 'answer') {
      verdict = event.content ?? verdict;
    } else if (event.type === 'error') {
      console.error('error:', event.content);
    }
  }

  console.log(`\nverdict: ${verdict}`);
  console.log(`processes: ${repl.session.processes().map((p) => `${p.name}:${p.state}`).join(', ')}`);
  console.log(`elapsed: ${((Date.now() - started) / 1000).toFixed(1)}s`);

  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
