import { afterEach, describe, expect, it } from 'vitest';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { executeWorkEntry, validateWorkShell, workInput, workInvocation, workResult } from '../src/work/index.js';
import type { DMLEvent, LLMBackendRequest, ShellSpec } from '../src/types.js';

const spec: ShellSpec = { kind: 'agent', system: 'Be concise.', hostTools: [], tools: [], welcome: '', workflows: [
  { name: 'Echo', goal: 'echo', inputs: [{ name: 'File', type: 'file' }], outputs: [{ name: 'Text', type: 'string' }, { name: 'Count', type: 'integer' }, { name: 'Valid', type: 'boolean' }] },
] };
const code = `echo(File, File, 7, true).
no_result(_) :- fail.
unbound(_).
side_effect.
shell(_{kind:agent, system:"Be concise.", host_tools:[], tools:[], workflows:[
 _{name:"Echo",goal:"echo",inputs:[_{name:"File",type:file}],outputs:[_{name:"Text",type:string},_{name:"Count",type:integer},_{name:"Valid",type:boolean}]},
 _{name:"False",goal:"no_result",inputs:[],outputs:[_{name:"Value",type:string}]},
 _{name:"Unbound",goal:"unbound",inputs:[],outputs:[_{name:"Value",type:string}]},
 _{name:"Side effect",goal:"side_effect",inputs:[],outputs:[]}
],welcome:"Hi"}).`;
const sessions: HarnessRepl[] = [];
const directories: string[] = [];
afterEach(async () => {
  for (const session of sessions.splice(0)) await session.dispose();
  for (const directory of directories.splice(0)) await fs.rm(directory, { recursive: true, force: true });
});
async function open(options: Partial<Parameters<typeof createHarnessRepl>[0]> = {}) {
  const repl = await createHarnessRepl({ model: 'test', provider: 'openai', mode: 'work', ...options });
  sessions.push(repl);
  if (!options.resume) expect((await repl.session.loadCode(code)).error).toBeUndefined();
  return repl;
}
async function run(repl: HarnessRepl, name: string, inputs: Record<string, unknown> = {}) {
  const events: DMLEvent[] = [];
  for await (const event of repl.runWorkflow(name, inputs)) events.push(event);
  return events;
}

describe('shared Work contract', () => {
  it('quotes file paths and strings as data, never as source', () => {
    const value = 'uploads/a b\\report"), halt. %雪.csv';
    const invocation = workInvocation(spec.workflows[0], { File: value });
    expect(invocation.goal).toBe(`echo(${JSON.stringify(value.replace(/\\/g, '/'))}, Out1, Out2, Out3)`);
    for (const invalid of ['../secret', '/etc/passwd', 'C:\\secret', 'https://example.org/a', 'foo\u0000bar']) expect(() => workInput('file', invalid)).toThrow();
  });

  it('rejects missing inputs, fractional integers, bad booleans and advertised-but-unsupported complex types', () => {
    expect(() => workInput('integer', 1.5)).toThrow();
    expect(() => workInput('integer', '')).toThrow();
    expect(workInput('number', '1e3')).toBe(1000);
    expect(workInput('boolean', 'false')).toBe(false);
    for (const value of ['no', 0, null]) expect(() => workInput('boolean', value)).toThrow();
    expect(() => workInput('object', {})).toThrow();
    expect(() => workInvocation(spec.workflows[0], {})).toThrow('missing');
    expect(() => workInvocation(spec.workflows[0], { File: 'a', extra: 1 })).toThrow('unknown');
    expect(validateWorkShell({ ...spec, tools: [{ name: 'execute', goal: 'echo', description: '', inputs: [], outputs: [] }] }).join()).toContain('reserved');
    expect(validateWorkShell(spec, [{ name: 'echo', arity: 1 }, { name: 'echo', arity: 4 }])).toEqual([]);
  });

  it('does not execute invalid input and does not treat no solution or unbound output as success', async () => {
    let calls = 0;
    const invalid = await executeWorkEntry(spec.workflows[0], {}, async () => { calls++; throw new Error('must not run'); });
    expect(invalid.status).toBe('failed');
    expect(calls).toBe(0);
    expect(workResult(spec.workflows[0], { bindings: [], solutions: [], output: '', elapsedMs: 0 }).ok).toBe(false);
    const repl = await open();
    for (const name of ['False', 'Unbound']) expect((await run(repl, name)).find(e => e.workflow)?.workflow?.result.status).toBe('failed');
    expect((await run(repl, 'Side effect')).find(e => e.workflow)?.workflow?.result).toMatchObject({ ok: true, outputs: {} });
  });

  it('manual starts run exactly the selected goal with no assistant call, and retain typed outputs', async () => {
    const requests: LLMBackendRequest[] = [];
    const repl = await open({ llmBackend: { complete: async request => { requests.push(request); return { text: 'Ready' }; } },
      authoringTools: new Map([['dangerous_extra', { description: 'Must not leak', parameters: { type: 'object', properties: {} }, execute: async () => ({}) }]]) });
    const events = await run(repl, 'Echo', { File: 'uploads/雪 report.csv' });
    expect(requests).toHaveLength(0);
    expect(events.find(e => e.workflow)?.workflow?.result).toMatchObject({ ok: true, outputs: { Text: 'uploads/雪 report.csv', Count: 7, Valid: true } });
    for await (const _ of repl.prompt('What did we just run?')) { /* consume */ }
    expect(requests[0].messages[0].content).toContain('Work mode');
    expect(requests[0].messages.some(message => message.content.includes('User-started workflow'))).toBe(true);
    expect(requests[0].tools?.map(tool => tool.name)).toEqual(['run_workflow']);
  });

  it('requires an explicit shell instead of synthesizing helper workflows', async () => {
    const repl = await createHarnessRepl({ model: 'test', provider: 'openai', mode: 'work' });
    sessions.push(repl);
    await repl.session.loadCode('internal_helper(secret).');
    expect((await repl.workSpec()).shell).toBeNull();
    expect((await run(repl, 'internal_helper'))[0].content).toContain('No Work shell');
    const events = [];
    for await (const event of repl.query('internal_helper(X)')) events.push(event);
    expect(events[0].content).toContain('Develop');
  });

  it('persists mode/harness identity and resumes Work safely without changing HEAD', async () => {
    const workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-work-contract-'));
    directories.push(workspace);
    const first = await open({ workspacePath: workspace, harness: 'support', history: {} });
    await run(first, 'Echo', { File: 'a.csv' });
    const id = first.session.getSessionId();
    const snapshot = await first.workSpec();
    await first.dispose(); sessions.splice(sessions.indexOf(first), 1);
    const resumed = await createHarnessRepl({ workspacePath: workspace, sessionId: id, resume: true, model: 'test', provider: 'openai', history: {} });
    sessions.push(resumed);
    expect(await resumed.sessionInfo()).toMatchObject({ mode: 'work', harness: 'support' });
    expect((await resumed.workSpec()).hash).toBe(snapshot.hash);
    expect((await run(resumed, 'Echo', { File: 'again.csv' })).find(e => e.workflow)?.workflow?.result.ok).toBe(true);
    await expect(createHarnessRepl({ workspacePath: workspace, sessionId: id, resume: true, mode: 'develop', model: 'test', provider: 'openai', history: {} })).rejects.toThrow('separate');
    expect((await resumed.session.replaceHarness('replaced.')).committed).toBe(false);
  });
});
