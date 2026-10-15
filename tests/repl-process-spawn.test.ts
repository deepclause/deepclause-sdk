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
import { pidTermText, type ProcessEvent } from '../src/repl/processes.js';

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

  it('fans a worker over a list with swarm, preserving order', async () => {
    await define('mark/2', 'mark(X, marked(X)).');
    await define('run_swarm/1', 'run_swarm(Results) :- swarm(mark, [a, b, c], Results).');
    const result = await repl.session.executeGoal('run_swarm(R)', 1);
    expect(result.error).toBeUndefined();
    const value = result.bindings.find((binding) => binding.name === 'R')?.value ?? '';
    expect(value).toContain('ok(marked(a))');
    expect(value).toContain('ok(marked(c))');
    expect(value.indexOf('marked(a)')).toBeLessThan(value.indexOf('marked(b)'));
    expect(value.indexOf('marked(b)')).toBeLessThan(value.indexOf('marked(c)'));
  });

  it('pmap returns bare values and fails on any failure', async () => {
    await define('mark/2', 'mark(X, marked(X)).');
    await define('run_pmap/1', 'run_pmap(Values) :- pmap(mark, [x, y], Values).');
    const result = await repl.session.executeGoal('run_pmap(Vs)', 1);
    expect(result.error).toBeUndefined();
    const value = result.bindings.find((binding) => binding.name === 'Vs')?.value ?? '';
    expect(value).toContain('marked(x)');
    expect(value).not.toContain('ok(');
  });

  it('selects a message by pattern with receive_match and requeues the rest', async () => {
    await define('launch/0', 'launch :- self(Me), spawn(selective(Me), _), output("launched").');
    await define('selective/1', 'selective(Parent) :- receive_match(wanted(X), _, 1000), receive(Next), send(Parent, got(X, Next)).');
    await define('await_msg/1', 'await_msg(R) :- receive(R, 3000).');

    await repl.session.executeGoal('launch', 1);
    await new Promise((resolve) => setTimeout(resolve, 250));
    const proc = repl.session.processes().find((p) => p.name === 'selective');
    expect(proc).toBeDefined();
    repl.session.sendProcess(proc!.pid, 'other');
    repl.session.sendProcess(proc!.pid, 'wanted(1)');

    const result = await repl.session.executeGoal('await_msg(R)', 1);
    const value = result.bindings.find((binding) => binding.name === 'R')?.value ?? '';
    expect(value).toContain('got(1,other)');
  });

  it('serializes concurrent input requests onto one channel', async () => {
    const order: string[] = [];
    let active = 0;
    let maxActive = 0;
    repl.session.setInputHandler(async (prompt) => {
      active += 1;
      maxActive = Math.max(maxActive, active);
      order.push(prompt);
      await new Promise((resolve) => setTimeout(resolve, 40));
      active -= 1;
      return 'ok';
    });
    await define('asker/1', 'asker(N) :- input(N, _), output("answered").');
    await repl.session.spawnProcess('asker("first")');
    await repl.session.spawnProcess('asker("second")');
    await new Promise((resolve) => setTimeout(resolve, 400));
    expect(maxActive).toBe(1);
    expect(order).toHaveLength(2);
  });

  it('records subagent output and answers for the /ps --logs view', async () => {
    await define('narrate/0', 'narrate :- output("logged line"), answer("done").');
    const pid = await repl.session.spawnProcess('narrate', { name: 'narrate' });
    await waitForDone(repl, pid.id);
    const log = repl.session.processLog();
    expect(log.some((entry) => entry.text.includes('logged line'))).toBe(true);
    expect(log.some((entry) => entry.kind === 'answer' && entry.text.includes('done'))).toBe(true);
  });

  it('fans out with a bounded concurrency window', async () => {
    await define('mark/2', 'mark(X, marked(X)).');
    await define('run_swarm1/1', 'run_swarm1(Results) :- swarm(mark, [a, b, c], Results, 1).');
    const result = await repl.session.executeGoal('run_swarm1(R)', 1);
    expect(result.error).toBeUndefined();
    const value = result.bindings.find((binding) => binding.name === 'R')?.value ?? '';
    expect(value).toContain('ok(marked(a))');
    expect(value).toContain('ok(marked(b))');
    expect(value).toContain('ok(marked(c))');
  });

  it('waits for a subagent and returns its outcome', async () => {
    await define('answer_loud/0', 'answer_loud :- output("the answer"), answer("the answer").');
    const pid = await repl.session.spawnProcess('answer_loud', { name: 'answer_loud' });
    const result = await repl.session.executeGoal(`wait(${pidTermText(pid)}, Outcome)`, 1);
    expect(result.error).toBeUndefined();
    const value = result.bindings.find((binding) => binding.name === 'Outcome')?.value ?? '';
    expect(value).toContain('ok(');
    expect(value).toContain('the answer');
  });
});
