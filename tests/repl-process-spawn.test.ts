/**
 * End-to-end: DML `spawn`, `output`, `send`, and `receive` through the REPL
 * session's process host. Each process runs in a private child session.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js';
import { createHarnessTools } from '../src/repl/harness-tools.js';
import type { ProcessEvent } from '../src/repl/processes.js';

let workspace: string;
let repl: HarnessRepl;
let events: ProcessEvent[];

async function define(nameArity: string, clauses: string): Promise<void> {
  await repl.session.beginTurn();
  await repl.session.define(nameArity, clauses);
  await repl.session.endTurn();
}

async function waitForDone(repl: HarnessRepl, id: string, timeoutMs = 15_000): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const info = repl.session.processes().find((p) => p.pid.id === id);
    if (info && ['done', 'crashed', 'killed'].includes(info.state)) return;
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  throw new Error('process did not finish in time');
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-proc-'));
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' });
  events = [];
  repl.session.setProcessEventSink((event) => events.push(event));
});

afterEach(async () => {
  await repl.dispose();
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('session-scoped subagents', () => {
  it('runs a one-shot goal and reports its output', async () => {
    await define('narrate/0', 'narrate :- output("hello from subagent").');
    const pid = await repl.session.spawnProcess('narrate');
    await waitForDone(repl, pid.id);

    const info = repl.session.processes().find((p) => p.pid.id === pid.id);
    expect(info?.state).toBe('done');
    expect(events.some((e) => e.type === 'output' && e.content?.includes('hello from subagent'))).toBe(true);
    expect(events.some((e) => e.type === 'started')).toBe(true);
    expect(events.some((e) => e.type === 'exited')).toBe(true);
  });

  it('waits for messages and handles them', async () => {
    await define('keeper/1', [
      'keeper(Acc) :-',
      '    receive(Msg),',
      '    (   Msg = stop -> output("stopped")',
      '    ;   output(Msg), keeper(Acc)',
      '    ).',
    ].join('\n'));

    const pid = await repl.session.spawnProcess('keeper([])');
    await new Promise((resolve) => setTimeout(resolve, 300)); // let the process start

    repl.session.sendProcess(pid, '"first"');
    repl.session.sendProcess(pid, 'stop');
    await waitForDone(repl, pid.id);

    const outputs = events.filter((e) => e.type === 'output').map((e) => e.content);
    expect(outputs.some((text) => text?.includes('first'))).toBe(true);
    expect(outputs.some((text) => text?.includes('stopped'))).toBe(true);
  });

  it('reports the whole process state', async () => {
    await define('quick/0', 'quick :- output("done").');
    await repl.session.spawnProcess('quick', { name: 'quick-one' });
    const infos = repl.session.processes();
    expect(infos).toHaveLength(1);
    expect(infos[0]?.name).toBe('quick-one');
  });

  it('shuts processes down on dispose', async () => {
    await define('linger/0', 'linger :- receive(_, 60000), output("unreachable").');
    const pid = await repl.session.spawnProcess('linger');
    await new Promise((resolve) => setTimeout(resolve, 200));
    await repl.dispose();
    const info = repl.session.processes().find((p) => p.pid.id === pid.id);
    expect(['killed', 'crashed', 'done']).toContain(info?.state);
  });

  it('serves request/reply through service_call and serve', async () => {
    await define('echo/1', [
      'echo(State) :-',
      '    receive(Msg),',
      '    (   Msg = request(From, Ref, Value) -> send(From, reply(Ref, Value)), echo(State)',
      '    ;   echo(State)',
      '    ).',
    ].join('\n'));
    await define('start_echo/0', 'start_echo :- serve(echo, echo(ok)).');
    await define('ask_echo/1', 'ask_echo(Reply) :- service_call(echo, hello, Reply).');

    await repl.session.executeGoal('start_echo', 1);
    await new Promise((resolve) => setTimeout(resolve, 250));

    const result = await repl.session.executeGoal('ask_echo(Reply)', 1);
    expect(result.error).toBeUndefined();
    expect(result.bindings.some((binding) => binding.name === 'Reply' && binding.value === 'hello')).toBe(true);

    const service = repl.session.processes().find((p) => p.name === 'echo');
    expect(service).toBeDefined();
  });

  it('spawns and lists through the real harness tools', async () => {
    await define('narrate/0', 'narrate :- output("via tool").');
    const tools = createHarnessTools(repl.session);
    const spawned = await tools.get('spawn')!.execute({ goal: 'narrate', name: 'narrator' });
    expect(spawned).toMatchObject({ name: 'narrator' });
    const listed = (await tools.get('ps')!.execute({})) as { text: string };
    expect(listed.text).toContain('narrator');
    await tools.get('stop')!.execute({ target: 'narrator' });
  });
});
