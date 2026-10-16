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
  // Tests run on the in-process host (L1) for determinism; worker mode (L2) is
  // covered by repl-process-worker.test.ts against a scripted worker.
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai', execution: 'inline' });
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

  it('lists processes, and does not expose spawn/call as agent tools', async () => {
    await define('narrate/0', 'narrate :- output("via tool").');
    const tools = createHarnessTools(repl.session);
    await repl.session.spawnProcess('narrate', { name: 'narrator' });
    const listed = (await tools.get('ps')!.execute({})) as { text: string };
    expect(listed.text).toContain('narrator');
    await tools.get('stop')!.execute({ target: 'narrator' });
    // spawn and call are DML (`spawn/2`, `service_call/3`); an authoring turn uses
    // `execute`, so they are not offered as tools.
    expect(tools.has('spawn')).toBe(false);
    expect(tools.has('call')).toBe(false);
  });

  it('quotes a prose call message instead of sending a malformed goal', async () => {
    await define('echo/1', [
      'echo(State) :-',
      '    receive(request(From, Ref, Payload), 5000),',
      '    send(From, reply(Ref, got(Payload))),',
      '    echo(State).',
    ].join('\n'));
    await define('start_echo/0', 'start_echo :- serve(echo, echo(ok)).');
    await repl.session.executeGoal('start_echo', 1);
    await new Promise((resolve) => setTimeout(resolve, 250));

    // Prose is not a Prolog term; it must arrive as a payload, not a syntax error.
    const reply = await repl.session.callProcess('echo', 'hello from the REPL', 5000);
    expect(reply).toContain('hello from the REPL');
  });

  it('fails a call when the service exits before replying', async () => {
    await define('dying/0', 'dying :- receive(_, 5000), answer(bye).');
    await repl.session.spawnProcess('dying', { name: 'dying' });
    await new Promise((resolve) => setTimeout(resolve, 200));

    await expect(repl.session.callProcess('dying', 'ping', 10_000)).rejects.toThrow(/exited|before replying/);
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

  it('reports a failed swarm item instead of hanging when the worker is undefined', async () => {
    // A worker that throws used to crash its process without sending a result, so
    // the parent's `swarm_receive` waited forever and the `execute` never stopped.
    await define('use_missing/1', 'use_missing(R) :- swarm(does_not_exist, [a, b], R).');
    const started = Date.now();
    const result = await repl.session.executeGoal('use_missing(R)', 1);
    expect(Date.now() - started).toBeLessThan(15_000);
    const value = result.bindings.find((binding) => binding.name === 'R')?.value ?? '';
    expect(value).toContain('failed(');
    expect(value).toContain('does_not_exist');
  });

  it('runs a swarm larger than the live-process limit', async () => {
    // The limit is on live processes, so 25 items at concurrency 3 all complete.
    await define('noop/2', 'noop(X, X).');
    await define('count_swarm/1', 'count_swarm(N) :- numlist(1, 25, L), swarm(noop, L, R, 3), length(R, N).');
    const result = await repl.session.executeGoal('count_swarm(N)', 1);
    expect(result.error).toBeUndefined();
    expect(result.bindings.find((binding) => binding.name === 'N')?.value).toBe('25');
  });

  it('reports a failed item when the spawn limit is actually reached', async () => {
    await define('noop/2', 'noop(X, X).');
    // Hold 15 of the 16 live slots, then a concurrency-4 swarm cannot spawn them all.
    for (let index = 0; index < 15; index += 1) {
      await repl.session.spawnProcess('receive(_, 60000)', { name: `hold${index}` });
    }
    const result = await repl.session.executeGoal('swarm(noop, [1, 2, 3, 4], R, 4)', 1);
    const value = result.bindings.find((binding) => binding.name === 'R')?.value ?? '';
    expect(value).toContain('failed(');
    expect(value).toContain('process limit');
  });

  it('kills another process with exit/2', async () => {
    await define('linger/0', 'linger :- receive(_, 60000).');
    const result = await repl.session.executeGoal(
      'self(Me), spawn(linger, Q), sleep(100), exit(Q, stop), output(killed(Q))',
      1,
    );
    expect(result.output).toContain('killed(');
    await new Promise((resolve) => setTimeout(resolve, 100));
    expect(repl.session.processes().find((process) => process.name === 'linger')?.state).toBe('killed');
  });
});
