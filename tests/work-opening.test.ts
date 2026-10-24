import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { harnessHash } from '../src/repl/history.js';

// Same annotation-order shape as the web's seeded Example. The human-readable
// view sorts keyed annotations; its order differs from canonical export.
const SOURCE = `
capability(zebra/2).
capability(uppercase/2).
doc(zebra/2,"Second helper").
doc(uppercase/2,"Uppercase text").
zebra(Text, Text).
uppercase(Text, Upper) :- string_upper(Text, Upper).
shell(_{kind:agent,system:"Writing assistant",host_tools:[],tools:[],workflows:[
 _{name:"Uppercase",goal:"uppercase",inputs:[_{name:"Text",type:string}],outputs:[_{name:"Upper",type:string}]}
],welcome:"Ready"}).`;
let workspace: string;
const sessions: HarnessRepl[] = [];
beforeEach(async () => { workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'dc-work-opening-')); });
afterEach(async () => {
  for (const repl of sessions.splice(0)) await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});
async function inspect(source = SOURCE) {
  const repl = await createHarnessRepl({ workspacePath: workspace, model: 'test', provider: 'openai', autoLoadHarness: false });
  sessions.push(repl);
  expect((await repl.session.loadCode(source)).error).toBeUndefined();
  return repl.workSpec();
}

describe('web inspect → open Work handoff', () => {
  it('checksums the source actually transferred and opens/runs an unchanged annotated harness', async () => {
    const spec = await inspect();
    expect(spec.snapshot!.hash).toBe(harnessHash(spec.snapshot!.source, []));
    const work = await createHarnessRepl({ workspacePath: workspace, model: 'test', provider: 'openai', mode: 'work', harnessSnapshot: spec.snapshot });
    sessions.push(work);
    const events = [];
    for await (const event of work.runWorkflow('Uppercase', { Text: 'hello' })) events.push(event);
    expect(events.find(event => event.workflow)?.workflow?.result).toMatchObject({ ok: true, outputs: { Upper: 'HELLO' } });
  });

  it('still rejects a genuinely changed linked script', async () => {
    await fs.mkdir(path.join(workspace, 'scripts'));
    await fs.writeFile(path.join(workspace, 'scripts/job.py'), 'print(1)');
    const spec = await inspect(`script_file(job_py,"scripts/job.py").\n${SOURCE}`);
    await fs.writeFile(path.join(workspace, 'scripts/job.py'), 'print(2)');
    await expect(createHarnessRepl({ workspacePath: workspace, model: 'test', provider: 'openai', mode: 'work', harnessSnapshot: spec.snapshot })).rejects.toThrow('linked scripts changed');
  });
});
