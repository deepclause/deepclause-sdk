import { describe, expect, it } from 'vitest';
import { AUTHORING_EVAL_CASES, scoreAuthoringCase } from '../scripts/authoring-eval-cases.mjs';

const byId = (id: string) => AUTHORING_EVAL_CASES.find((test) => test.id === id)!;

describe('authoring evaluation rubrics (no network)', () => {
  it('covers basics, shells, task memory/tools, forms, processes, media and host schemas', () => {
    expect(AUTHORING_EVAL_CASES).toHaveLength(10);
    for (const id of ['pure-skill', 'shell-default', 'scoped-task', 'form-workflow', 'process-result', 'vision-boundaries', 'python-input']) expect(byId(id)).toBeTruthy();
  });
  it('rejects a default custom chat loop even if a shell is also present', () => {
    const shell = { kind: 'agent', tools: [{ inputs: [], outputs: [] }], workflows: [{ inputs: [], outputs: [] }] };
    expect(scoreAuthoringCase(byId('shell-default'), { source: 'loop :- input(X), chat(X), loop.', shell }).passed).toBe(false);
    expect(scoreAuthoringCase(byId('shell-default'), { source: 'length_of(T,N):-string_length(T,N).', shell }).passed).toBe(true);
  });
  it('requires the focused process reference and message/cleanup explanation', () => {
    const answer = 'Copies are isolated, so send a message then wait/reap the child.';
    expect(scoreAuthoringCase(byId('process-result'), { answer }).passed).toBe(false);
    const events = [{ type: 'tool_call', toolName: 'reference', toolArgs: { topic: 'processes' } }];
    expect(scoreAuthoringCase(byId('process-result'), { answer, events }).passed).toBe(true);
  });
  it('catches an alias workaround for stale docs and exact-arithmetic model calls', () => {
    expect(scoreAuthoringCase(byId('annotation-cleanup'), { source: 'doc(keep/1,"keep").\nstale(_).', }).passed).toBe(false);
    expect(scoreAuthoringCase(byId('pure-skill'), { source: 'triple(N,R):-task("multiply",R).', probe: '12' }).passed).toBe(false);
  });
});
