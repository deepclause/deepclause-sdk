/** Terminal adapter for the shared Work API. All execution/typing stays in SDK. */
import type { HarnessRepl } from '../repl/index.js';
import type { DMLEvent } from '../types.js';

export function parseWorkflowCommand(text: string): { name: string; inputs?: Record<string, unknown> } {
  const quoted = /^("(?:[^"\\]|\\.)*")\s*/.exec(text.trim());
  let name: string;
  let rest: string;
  if (quoted) { name = JSON.parse(quoted[1]); rest = text.trim().slice(quoted[0].length).trim(); }
  else {
    const split = text.indexOf(' {');
    name = (split < 0 ? text : text.slice(0, split)).trim();
    rest = split < 0 ? '' : text.slice(split + 1).trim();
  }
  if (!name) throw new Error('usage: /workflow "Workflow name" {"Input":"value"}');
  if (!rest) return { name };
  const inputs = JSON.parse(rest);
  if (!inputs || typeof inputs !== 'object' || Array.isArray(inputs)) throw new Error('inputs must be a JSON object');
  return { name, inputs };
}

export async function runDeclaredWorkflow(options: {
  repl: HarnessRepl;
  name: string;
  inputs?: Record<string, unknown>;
  signal?: AbortSignal;
  ask?: (prompt: string, choices?: string[]) => Promise<string | undefined>;
  render: (event: DMLEvent) => void;
}): Promise<boolean> {
  const spec = await options.repl.workSpec();
  const entry = spec.shell?.workflows.find(workflow => workflow.name === options.name);
  if (!entry) throw new Error(spec.shell ? `Unknown workflow '${options.name}'. Use /workflows.` : spec.diagnostics.join('\n'));
  const inputs = options.inputs ?? {};
  if (options.inputs === undefined) {
    for (const field of entry.inputs) {
      const value = await options.ask?.(`${field.name} (${field.type}${field.type === 'file' ? ', workspace-relative path' : ''})`, field.type === 'boolean' ? ['true', 'false'] : undefined);
      if (value === undefined) throw new Error('Workflow input cancelled or unavailable. Supply a JSON input object in noninteractive mode.');
      Object.defineProperty(inputs, field.name, { value, enumerable: true });
    }
  }
  let ok = false;
  for await (const event of options.repl.runWorkflow(options.name, inputs, { signal: options.signal })) {
    options.render(event);
    if (event.workflow) ok = event.workflow.result.ok;
  }
  return ok;
}
