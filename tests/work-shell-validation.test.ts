import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import type { DMLEvent, LLMBackendRequest, ShellSpec } from '../src/types.js';
import { renderWorkShell } from '../src/work/index.js';

const PREDICATES = `web_search(Query, Report) :- string_concat("search: ", Query, Report).
web_fetch(Url, Topic, Report) :- string_concat(Url, Topic, Report).`;
const BAD_SHELL = `shell(_{kind:agent,system:"Scout",host_tools:[],workflows:[],welcome:"Scout",tools:[
 _{name:"web_search",goal:"web_search",description:"Search",inputs:[_{name:Query,type:string}],outputs:[_{name:Report,type:string}]},
 _{name:"web_fetch",goal:"web_fetch",description:"Fetch",inputs:[_{name:Url,type:string},_{name:Topic,type:string}],outputs:[_{name:Report,type:string}]}
]}).`;
const GOOD: ShellSpec = { kind: 'agent', system: 'Scout', hostTools: [], workflows: [], welcome: 'Scout', tools: [
  { name: 'web_search', goal: 'web_search', description: 'Search', inputs: [{ name: 'Query', type: 'string' }], outputs: [{ name: 'Report', type: 'string' }] },
  { name: 'web_fetch', goal: 'web_fetch', description: 'Fetch', inputs: [{ name: 'Url', type: 'string' }, { name: 'Topic', type: 'string' }], outputs: [{ name: 'Report', type: 'string' }] },
] };
let workspace: string;
let file: string;
const sessions: HarnessRepl[] = [];
beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-shell-validation-'));
  file = path.join(workspace, '.deepclause/repl/harnesses/scout/harness.dml');
  await fs.mkdir(path.dirname(file), { recursive: true });
  await fs.writeFile(file, PREDICATES);
});
afterEach(async () => {
  for (const repl of sessions.splice(0)) await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});
async function open(options: Partial<Parameters<typeof createHarnessRepl>[0]> = {}) {
  const repl = await createHarnessRepl({ workspacePath: workspace, harness: 'scout', history: {}, model: 'test', provider: 'openai', ...options });
  sessions.push(repl);
  return repl;
}

describe('Work shell validation before publication', () => {
  it('refuses unbound names before commit and allows correcting the same open transaction', async () => {
    const develop = await open();
    await develop.session.beginTurn();
    expect((await develop.session.define('shell(_)', BAD_SHELL)).ok).toBe(true);
    expect((await develop.workSpec()).status).toBe('invalid');
    const invalid = await develop.session.endTurn();
    expect(invalid.committed).toBe(false);
    expect(invalid.error).toContain('name: "Query"');
    expect(invalid.error).toContain('Prolog variables');
    expect(invalid.error?.match(/empty\/unbound/g)).toHaveLength(5);
    expect(develop.session.harnessView().transactionOpen).toBe(true);
    expect(await fs.readFile(file, 'utf8')).toBe(PREDICATES);
    await develop.session.define('shell(_)', renderWorkShell(GOOD));
    expect((await develop.session.endTurn()).committed).toBe(true);
    const fresh = await open({ mode: 'work' });
    expect((await fresh.workSpec()).status).toBe('ready');
    expect((await fresh.workSpec()).shell?.tools[0].inputs[0].name).toBe('Query');
  });

  it('also rejects a full-source editor save, preserving the previous live and published code', async () => {
    const develop = await open();
    const result = await develop.session.replaceHarness(`${PREDICATES}\n${BAD_SHELL}`);
    expect(result.committed).toBe(false);
    expect(result.error).toContain('quoted string');
    expect(await fs.readFile(file, 'utf8')).toBe(PREDICATES);
    expect((await develop.workSpec()).status).toBe('missing');
    expect(develop.session.predicateArities('web_search')).toEqual([2]);
  });

  it('runs correctly quoted tool declarations end-to-end through the Work assistant', async () => {
    const develop = await open();
    const spec = await develop.workSpec();
    expect((await develop.updateWorkShell(GOOD, spec.hash)).committed).toBe(true);
    const requests: LLMBackendRequest[] = [];
    const work = await open({ mode: 'work', llmBackend: { complete: async request => {
      const index = requests.push(request) - 1;
      if (index === 0) return { text: '', toolCalls: [{ id: 'search', name: 'web_search', arguments: { Query: 'Pyodide' } }] };
      if (index === 1) {
        expect(JSON.parse(request.messages.at(-1)!.content)).toMatchObject({ ok: true, outputs: { Report: 'search: Pyodide' } });
        return { text: '', toolCalls: [{ id: 'fetch', name: 'web_fetch', arguments: { Url: 'https://example.com/', Topic: 'summary' } }] };
      }
      expect(JSON.parse(request.messages.at(-1)!.content)).toMatchObject({ ok: true, outputs: { Report: 'https://example.com/summary' } });
      request.onText?.('Verified tools.');
      return { text: 'Verified tools.' };
    } } });
    const events: DMLEvent[] = [];
    for await (const event of work.prompt('Search, then fetch the result')) events.push(event);
    expect(Object.keys(requests[0].tools!.find(tool => tool.name === 'web_search')!.parameters.properties as object)).toEqual(['Query']);
    expect(Object.keys(requests[0].tools!.find(tool => tool.name === 'web_fetch')!.parameters.properties as object)).toEqual(['Url', 'Topic']);
    expect(events.filter(event => event.type === 'tool_call' && event.toolState === 'failed')).toHaveLength(0);
    expect(events.some(event => event.type === 'stream' && event.content === 'Verified tools.')).toBe(true);
  });
});
