#!/usr/bin/env node
/** Opt-in real-model evaluation. Never uses credentials, network or host execution by default. */
import { readFile, mkdtemp, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { AUTHORING_EVAL_CASES, scoreAuthoringCase } from './authoring-eval-cases.mjs';

const args = process.argv.slice(2);
if (!args.includes('--run')) {
  console.log(JSON.stringify({ mode: 'inventory-only; no network calls', cases: AUTHORING_EVAL_CASES.map(({ id, requiredReference }) => ({ id, requiredReference })),
    run: 'DML_EVAL_ENDPOINT=<full chat/completions URL> DML_EVAL_MODEL=<id> [DML_EVAL_KEY=...] npm run eval:authoring -- --run [--case=id] [--prompt-file=path]' }, null, 2));
} else {
  const endpoint = process.env.DML_EVAL_ENDPOINT;
  const model = process.env.DML_EVAL_MODEL;
  if (!endpoint || !model) throw new Error('Explicit DML_EVAL_ENDPOINT and DML_EVAL_MODEL are required; no credentials or provider are inferred.');
  const { createHarnessRepl } = await import('../dist/repl/index.js');
  const file = args.find((arg) => arg.startsWith('--prompt-file='))?.slice('--prompt-file='.length);
  const prompt = await readFile(file ?? new URL('../src/system/assets/docs/AGENT_PROMPT.md', import.meta.url), 'utf8');
  const selected = args.find((arg) => arg.startsWith('--case='))?.slice('--case='.length);
  const cases = selected ? AUTHORING_EVAL_CASES.filter((test) => test.id === selected) : AUTHORING_EVAL_CASES;
  if (!cases.length) throw new Error(`Unknown case: ${selected}`);
  const reports = [];
  for (const test of cases) {
    const workspace = await mkdtemp(join(tmpdir(), 'dc-authoring-eval-'));
    const events = [];
    let modelCalls = 0;
    let inputTokens = 0;
    let outputTokens = 0;
    let repl;
    const start = Date.now();
    const abort = new AbortController();
    const timer = setTimeout(() => abort.abort(), 90000);
    try {
      const backend = { complete: async (request) => {
        modelCalls++;
        const messages = request.messages.map((message) => ({
          role: message.role, content: message.content || null,
          ...(message.toolCallId ? { tool_call_id: message.toolCallId } : {}),
          ...(message.toolCalls?.length ? { tool_calls: message.toolCalls.map((call) => ({ id: call.id, type: 'function', function: { name: call.name, arguments: JSON.stringify(call.arguments) } })) } : {}),
        }));
        const response = await fetch(endpoint, {
          method: 'POST', signal: request.signal,
          headers: { 'Content-Type': 'application/json', ...(process.env.DML_EVAL_KEY ? { Authorization: `Bearer ${process.env.DML_EVAL_KEY}` } : {}) },
          body: JSON.stringify({ model, messages, max_tokens: 3072, temperature: 0,
            ...(request.tools?.length ? { tools: request.tools.map((tool) => ({ type: 'function', function: tool })) } : {}),
          }),
        });
        if (!response.ok) throw new Error(`Model endpoint returned ${response.status}`);
        const data = await response.json();
        const choice = data.choices?.[0];
        const text = choice?.message?.content ?? '';
        if (text) request.onText?.(text);
        inputTokens += data.usage?.prompt_tokens ?? 0;
        outputTokens += data.usage?.completion_tokens ?? 0;
        return { text, finishReason: choice?.finish_reason,
          toolCalls: (choice?.message?.tool_calls ?? []).map((call) => ({ id: call.id, name: call.function.name, arguments: JSON.parse(call.function.arguments) })),
        };
      } };
      // Python here is a schema-only stub. This evaluation never grants native
      // shell/Python execution; canonical runtime tests cover actual capabilities.
      const python = { description: 'Schema-only Python capability for authoring evaluation; execution is disabled.',
        parameters: { type: 'object', properties: { code: { type: 'string' }, script: { type: 'string' }, argv: { type: 'array' }, timeoutMs: { type: 'number' }, packages: { type: 'array' } } },
        execute: async () => ({ ok: false, error: 'Execution intentionally disabled in authoring contract evaluation.' }),
      };
      repl = await createHarnessRepl({ workspacePath: workspace, model, provider: 'openai', llmBackend: backend,
        agentSystemPrompt: prompt, maxModelCalls: 12, execution: 'inline', executionBackend: 'wasm', capabilityTool: 'python', tools: new Map([['python', python]]),
        hostToolsCatalog: 'Browser evaluation: Python schema only, no shell, no actual host execution. Forms run in foreground, not scratch execute.',
        onForm: (form) => { if (form.op === 'open') repl.session.submitForm(form.formId, { action: 'cancel', values: {} }); },
      });
      if (test.initial) await repl.session.loadCode(test.initial);
      for await (const event of repl.prompt(test.prompt, { signal: abort.signal })) events.push(event);
      const source = await repl.session.harnessSource();
      let probe = '';
      if (test.probe) {
        for await (const event of repl.query(test.probe, { signal: abort.signal })) {
          if (event.type === 'solution') probe = event.solution?.bindings?.find((binding) => binding.name === 'R')?.value ?? '';
        }
      }
      const answer = events.filter((event) => event.type === 'stream' || event.type === 'output').map((event) => event.content ?? '').join('');
      const score = scoreAuthoringCase(test, { source, shell: repl.session.shell(), answer, events, probe });
      reports.push({ id: test.id, ...score, modelCalls, inputTokens, outputTokens, toolCalls: events.filter((event) => event.type === 'tool_call' && event.toolState === 'starting').length, durationMs: Date.now() - start });
    } catch (error) { reports.push({ id: test.id, passed: false, error: String(error), modelCalls, inputTokens, outputTokens }); }
    finally { clearTimeout(timer); await repl?.dispose(); await rm(workspace, { recursive: true, force: true }); }
  }
  console.log(JSON.stringify({ model, profile: file ?? 'current SDK core', systemCharacters: prompt.length,
    note: 'Automatic contract rubrics, not semantic correctness proofs. Review outputs for nuanced cases. Canonical tests use deterministic model stubs.',
    passed: reports.filter((report) => report.passed).length, total: reports.length, reports }, null, 2));
  if (reports.some((report) => !report.passed)) process.exitCode = 1;
}
