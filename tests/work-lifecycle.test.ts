import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { resolveWorkbenchPolicy } from '../src/work/index.js';
import { boundCompactionSettings } from '../src/repl/compaction.js';
import type { DMLEvent } from '../src/types.js';

const SOURCE = `
echo(Text, Text).
ask(Name) :- input("Name?", Name).
intake(Name) :- form(_{fields:[_{name:name,type:string,required:true}]}, F), await_form(F, C), close_form(F), get_dict(values,C,V), get_dict(name,V,Name).
shell(_{kind:agent,system:"Original persona",host_tools:[],tools:[],workflows:[
 _{name:"Echo",goal:"echo",inputs:[_{name:"Text",type:string}],outputs:[_{name:"Result",type:string}]},
 _{name:"Ask",goal:"ask",inputs:[],outputs:[_{name:"Name",type:string}]},
 _{name:"Intake",goal:"intake",inputs:[],outputs:[_{name:"Name",type:string}]}
],welcome:"Ready"}).`;
let workspace: string;
const sessions: HarnessRepl[] = [];
beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-work-lifecycle-'));
  const dir = path.join(workspace, '.deepclause/repl/harnesses/support');
  await fs.mkdir(dir, { recursive: true });
  await fs.writeFile(path.join(dir, 'harness.dml'), SOURCE);
});
afterEach(async () => {
  for (const session of sessions.splice(0)) await session.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});
async function open(options: Partial<Parameters<typeof createHarnessRepl>[0]> = {}) {
  const repl = await createHarnessRepl({ workspacePath: workspace, history: {}, harness: 'support', mode: 'work', model: 'test', provider: 'openai', ...options });
  sessions.push(repl);
  return repl;
}
async function consume(events: AsyncGenerator<DMLEvent>) {
  const result: DMLEvent[] = [];
  for await (const event of events) result.push(event);
  return result;
}

describe('Work lifecycle and reconciliation', () => {
  it('cancels an open form, rejects overlap, and recovers without an orphan form', async () => {
    let opened!: () => void;
    const ready = new Promise<void>(resolve => { opened = resolve; });
    const repl = await open({ onForm: form => { if (form.op === 'open') opened(); } });
    const abort = new AbortController();
    const pending = consume(repl.runWorkflow('Intake', {}, { signal: abort.signal }));
    await ready;
    const overlap = await consume(repl.runWorkflow('Echo', { Text: 'must not run' }));
    expect(overlap[0].content).toContain('busy');
    abort.abort();
    const events = await pending;
    expect(events.find(event => event.workflow)?.workflow?.result.status).toBe('cancelled');
    expect(repl.session.formRegistry().list().filter(form => form.state === 'open')).toHaveLength(0);
    expect((await consume(repl.runWorkflow('Echo', { Text: 'recovered' }))).find(event => event.workflow)?.workflow?.result.outputs.Result).toBe('recovered');
  });

  it('does not leave input/2 or the global human queue blocked after cancellation', async () => {
    const repl = await open({ onInput: () => new Promise(() => {}) });
    const abort = new AbortController();
    const pending = consume(repl.runWorkflow('Ask', {}, { signal: abort.signal }));
    setTimeout(() => abort.abort(), 30);
    expect((await pending).find(event => event.workflow)?.workflow?.result.status).toBe('cancelled');
    repl.setOnInput(async () => 'Ada');
    const next = await consume(repl.runWorkflow('Ask'));
    expect(next.find(event => event.workflow)?.workflow?.result.outputs.Name, JSON.stringify(next)).toBe('Ada');
  });

  it('keeps old Work sessions pinned while Develop commits a newer program', async () => {
    const work = await open();
    const spec = await work.workSpec();
    const oldSource = await work.session.harnessSource();
    const develop = await open({ mode: 'develop' });
    await develop.session.beginTurn();
    await develop.session.define('echo(_, _)', 'echo(_, "new version").');
    expect((await develop.session.endTurn()).committed).toBe(true);
    expect((await consume(work.runWorkflow('Echo', { Text: 'original' }))).find(event => event.workflow)?.workflow?.result.outputs.Result).toBe('original');
    const pinned = await open({ harnessSnapshot: { source: oldSource, version: spec.version, hash: spec.hash } });
    expect((await consume(pinned.runWorkflow('Echo', { Text: 'still original' }))).find(event => event.workflow)?.workflow?.result.outputs.Result).toBe('still original');
    const fresh = await open();
    expect((await consume(fresh.runWorkflow('Echo', { Text: 'ignored' }))).find(event => event.workflow)?.workflow?.result.outputs.Result).toBe('new version');
  });

  it('saves the shell transactionally and refuses stale editor versions or Work-side edits', async () => {
    const probe = await open({ mode: 'develop' });
    const spec = await probe.workSpec();
    const develop = await open({ mode: 'develop' });
    expect((await develop.workSpec()).hash).toBe(spec.hash); // anonymous dict tags must not create cross-session drift
    expect((await develop.updateWorkShell({ ...spec.shell!, system: 'New persona' }, spec.hash)).committed).toBe(true);
    expect((await develop.workSpec()).shell?.system).toBe('New persona');
    expect(develop.session.predicateArities('echo')).toContain(2);
    expect((await develop.updateWorkShell(spec.shell!, spec.hash)).error).toContain('changed');
    const work = await open();
    expect((await work.updateWorkShell(spec.shell!)).committed).toBe(false);
    expect((await work.session.beginTurn()).ok).toBe(false);
  });

  it('refuses stale editors and transactions after another Develop session commits', async () => {
    const first = await open({ mode: 'develop' });
    const stale = await open({ mode: 'develop' });
    const base = await first.workSpec();
    expect((await first.updateWorkShell({ ...base.shell!, system: 'Committed elsewhere' }, base.hash)).committed).toBe(true);
    expect((await stale.updateWorkShell({ ...base.shell!, system: 'Must not overwrite' }, base.hash)).error).toContain('another session');
    expect((await stale.session.replaceHarness('lost_changes.')).committed).toBe(false);
    expect((await stale.session.beginTurn()).ok).toBe(false);
    const source = await fs.readFile(path.join(workspace, '.deepclause/repl/harnesses/support/harness.dml'), 'utf8');
    expect(source).toContain('Committed elsewhere');
    expect(source).not.toContain('Must not overwrite');
  });

  it('preserves private DML state without changing the pinned program or exposing new predicates', async () => {
    const source = `put(Text, N) :- assertz(stored(Text)), count_store(N).
      count_store(N) :- ( current_predicate(stored/1) -> findall(X,stored(X),Xs), length(Xs,N) ; N = 0 ).
      shell(_{kind:workflow,host_tools:[],tools:[],workflows:[
        _{name:"Put",goal:"put",inputs:[_{name:"Text",type:string}],outputs:[_{name:"Count",type:integer}]},
        _{name:"Count",goal:"count_store",inputs:[],outputs:[_{name:"Count",type:integer}]}
      ],system:"",welcome:""}).`;
    await fs.writeFile(path.join(workspace, '.deepclause/repl/harnesses/support/harness.dml'), source);
    const first = await open();
    const identity = await first.workSpec();
    expect((await consume(first.runWorkflow('Put', { Text: 'Ada' }))).find(event => event.workflow)?.workflow?.result.outputs.Count).toBe(1);
    expect((await first.workSpec()).hash).toBe(identity.hash);
    expect((await first.workSpec()).shell?.workflows).toHaveLength(2);
    const id = first.session.getSessionId();
    await first.dispose(); sessions.splice(sessions.indexOf(first), 1);
    const resumed = await open({ mode: undefined, harness: undefined, sessionId: id, resume: true });
    expect((await consume(resumed.runWorkflow('Count'))).find(event => event.workflow)?.workflow?.result.outputs.Count).toBe(1);
    const fresh = await open({ harnessSnapshot: identity.snapshot });
    expect((await consume(fresh.runWorkflow('Count'))).find(event => event.workflow)?.workflow?.result.outputs.Count).toBe(0);
  });

  it('persists structured results and titles so both hosts can replay them', async () => {
    const repl = await open();
    await repl.setTitle('Portable session');
    await consume(repl.runWorkflow('Echo', { Text: 'saved' }));
    const id = repl.session.getSessionId();
    await repl.dispose(); sessions.splice(sessions.indexOf(repl), 1);
    const resumed = await open({ harness: undefined, mode: undefined, sessionId: id, resume: true });
    expect(await resumed.sessionInfo()).toMatchObject({ harness: 'support', mode: 'work', title: 'Portable session' });
    const events = await resumed.resumedTranscript();
    expect(events.find(event => event.workflow)?.workflow?.result.outputs.Result).toBe('saved');
  });

  it('rejects invalid durable purpose/identity instead of promoting it to authoring', async () => {
    await expect(open({ sessionId: '../outside' })).rejects.toThrow('session id');
    await expect(open({ harness: '../outside' })).rejects.toThrow('directory name');
    await expect(open({ mode: 'invalid' as never })).rejects.toThrow('purpose');
  });

  it('materializes missing embedded scripts on Work open without overwriting user files', async () => {
    const source = `script_file(job_py,"scripts/portable.py"). embedded_file("scripts/portable.py","print(123)").
      content(Text) :- read_file("scripts/portable.py",Text).
      shell(_{kind:workflow,host_tools:[],tools:[],system:"",welcome:"",workflows:[_{name:"Read",goal:"content",inputs:[],outputs:[_{name:"Text",type:string}]}]}).`;
    await fs.writeFile(path.join(workspace, '.deepclause/repl/harnesses/support/harness.dml'), source);
    const probe = await open({ mode: 'develop' });
    const snapshot = (await probe.workSpec()).snapshot;
    const work = await open({ harnessSnapshot: snapshot });
    expect((await consume(work.runWorkflow('Read'))).find(event => event.workflow)?.workflow?.result.outputs.Text).toBe('print(123)');
    await fs.writeFile(path.join(workspace, 'scripts/portable.py'), 'user-owned');
    const fresh = await open();
    expect((await consume(fresh.runWorkflow('Read'))).find(event => event.workflow)?.workflow?.result.outputs.Text).toBe('user-owned');
  });

  it.skipIf(process.platform === 'win32')('does not bootstrap embedded files outside the workspace through a symlink', async () => {
    const outside = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-embedded-outside-'));
    try {
      await fs.symlink(outside, path.join(workspace, 'scripts'), 'dir');
      await fs.writeFile(path.join(workspace, '.deepclause/repl/harnesses/support/harness.dml'), `embedded_file("scripts/created.py","unwanted"). ok. shell(_{kind:workflow,host_tools:[],tools:[],system:"",welcome:"",workflows:[_{name:"OK",goal:"ok",inputs:[],outputs:[]}]}).`);
      await expect(open()).rejects.toThrow('symlink');
      await expect(fs.stat(path.join(outside, 'created.py'))).rejects.toMatchObject({ code: 'ENOENT' });
    } finally { await fs.rm(outside, { recursive: true, force: true }); }
  });

  it('bounds CPU-only Work goals and recovers after gas exhaustion', async () => {
    await fs.writeFile(path.join(workspace, '.deepclause/repl/harnesses/support/harness.dml'), `spin :- spin. ok.
      shell(_{kind:workflow,host_tools:[],tools:[],system:"",welcome:"",workflows:[
        _{name:"Spin",goal:"spin",inputs:[],outputs:[]},_{name:"OK",goal:"ok",inputs:[],outputs:[]}
      ]}).`);
    const repl = await open({ gasLimit: 100 });
    const failed = (await consume(repl.runWorkflow('Spin'))).find(event => event.workflow)?.workflow?.result;
    expect(failed?.status).toBe('failed');
    expect(failed?.error?.toLowerCase()).toContain('gas');
    expect((await consume(repl.runWorkflow('OK'))).find(event => event.workflow)?.workflow?.result.ok).toBe(true);
  });

  it('shares non-secret limits and bounds compaction to small context windows', () => {
    const policy = resolveWorkbenchPolicy({ providers: { secret: 'never-forward-this' }, repl: { maxModelCalls: 0, providerConcurrency: 2, maxDepth: 4, compaction: { auto: false } } });
    expect(policy).toMatchObject({ maxModelCalls: 0, maxDepth: 4, providerConcurrency: 2, compaction: { auto: false } });
    expect(JSON.stringify(policy)).not.toContain('never-forward-this');
    expect(() => resolveWorkbenchPolicy({ repl: { maxModelCalls: -1 } })).toThrow();
    expect(boundCompactionSettings({ enabled: true, reserveTokens: 16384, keepRecentTokens: 20000 }, 4096)).toEqual({ enabled: true, reserveTokens: 1024, keepRecentTokens: 1536 });
  });
});
