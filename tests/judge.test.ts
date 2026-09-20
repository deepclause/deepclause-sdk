import { describe, expect, it } from 'vitest';
import {
  createDeepClause,
  createMockJevJudgeBackend,
  createMockJudgeBackend,
  type DMLEvent,
  type JudgeBackend,
  type JudgeBackendRequest,
} from '../src/index.js';

async function run(
  backend: JudgeBackend,
  code: string,
  args: string[] = [],
  defaultJudge = backend.id,
): Promise<DMLEvent[]> {
  const sdk = await createDeepClause({
    model: 'test-model',
    judgeBackends: { [backend.id]: backend },
    defaultJudge,
  });
  try {
    const events: DMLEvent[] = [];
    for await (const event of sdk.runDML(code, { args })) events.push(event);
    return events;
  } finally {
    await sdk.dispose();
  }
}

function answer(events: DMLEvent[]): string | undefined {
  return events.find((event) => event.type === 'answer')?.content;
}

describe('judge primitives (mock Jev backend)', () => {
  it('binds a batch of choose/rate/verify/probability answers', async () => {
    const events = await run(
      createMockJevJudgeBackend(),
      `
      agent_main(Message) :-
        judge(Message, [
          choose("Which team should handle this?", [billing, orders, account]) - Team,
          rate("How frustrated is the customer?", [calm, frustrated, angry]) - Frustration,
          verify("Does the message ask for a refund?") - Refund,
          probability("Is this urgent?") - Urgency
        ]),
        format(string(Result), "~w|~w|~w|~w", [Team, Frustration, Refund, Urgency]),
        answer(Result).
      `,
      ['my invoice was charged twice'],
    );

    expect(answer(events)).toBe('billing|calm|yes|0.5');
  });

  it('supports the one-off predicates', async () => {
    const events = await run(
      createMockJevJudgeBackend(),
      `
      agent_main(Message) :-
        choose(Message, "Which team?", [billing, orders], Team),
        rate(Message, "How frustrated?", [calm, angry], Level),
        verify(Message, "Is it urgent?", Truth),
        probability(Message, "Is it urgent?", P),
        format(string(Result), "~w|~w|~w|~w", [Team, Level, Truth, P]),
        answer(Result).
      `,
      ['x'],
    );

    expect(answer(events)).toBe('billing|calm|yes|0.5');
  });

  it('emits judge activity events', async () => {
    const events = await run(
      createMockJevJudgeBackend(),
      `
      agent_main(Message) :-
        choose(Message, "Which team?", [billing, orders], Team),
        answer(Team).
      `,
      ['x'],
    );

    const judgeEvents = events.filter((event) => event.type === 'task_activity' && event.judgeBackend);
    expect(judgeEvents.some((event) => event.judgeState === 'started')).toBe(true);
    expect(judgeEvents.some((event) => event.judgeState === 'completed')).toBe(true);
  });

  it('memoizes repeated judgments within a run', async () => {
    let calls = 0;
    const backend = createMockJevJudgeBackend({
      answers: (request) => {
        calls++;
        return request.questions.map((question) => ({
          id: question.id,
          kind: question.kind,
          value: 'billing',
          basis: 'calibrated' as const,
        }));
      },
    });

    const events = await run(
      backend,
      `
      agent_main(Message) :-
        choose(Message, "Which team?", [billing, orders], First),
        choose(Message, "Which team?", [billing, orders], Second),
        format(string(Result), "~w|~w", [First, Second]),
        answer(Result).
      `,
      ['x'],
    );

    expect(answer(events)).toBe('billing|billing');
    expect(calls).toBe(1);
  });

  it('parses option descriptions and Key-Value state objects', async () => {
    let captured: JudgeBackendRequest | undefined;
    const backend = createMockJudgeBackend({
      answers: (request) => {
        captured = request;
        return request.questions.map((question) => ({
          id: question.id,
          kind: question.kind,
          value: question.kind === 'choose' ? 'billing' : 'calm',
          basis: 'mock' as const,
        }));
      },
    });

    const events = await run(
      backend,
      `
      agent_main(Message) :-
        State = [ message-Message, customer-[plan-pro, tickets-3] ],
        judge(State, [
          choose("Which team?", [billing-"Charges and refunds", orders-"Delivery"]) - Team,
          rate("Tone?", [level(calm, ["no urgency"]), angry]) - Tone
        ]),
        format(string(R), "~w|~w", [Team, Tone]),
        answer(R).
      `,
      ['hello'],
    );

    expect(answer(events)).toBe('billing|calm');
    expect(captured?.state).toEqual({ message: 'hello', customer: { plan: 'pro', tickets: 3 } });
    expect(captured?.questions[0]?.options).toEqual([
      { id: 'billing', description: 'Charges and refunds' },
      { id: 'orders', description: 'Delivery' },
    ]);
    expect(captured?.questions[1]?.levels?.[0]).toEqual({ what: 'calm', examples: ['no urgency'] });
  });

  it('gates a goal on a calibrated backend', async () => {
    const code = `
      agent_main(Message) :-
        require_judgment(calibrated, (
          verify(Message, "Is it urgent?", Truth),
          Truth == yes
        )),
        answer("calibrated path").
      agent_main(_) :-
        answer("fallback path").
    `;

    const uncalibrated = await run(createMockJudgeBackend({ probability: 0.9 }), code, ['x']);
    expect(answer(uncalibrated)).toBe('fallback path');

    const calibrated = await run(createMockJevJudgeBackend({ probability: 0.9 }), code, ['x']);
    expect(answer(calibrated)).toBe('calibrated path');
  });

  it('selects a specific backend with with_judgment/2', async () => {
    const sdk = await createDeepClause({
      model: 'test-model',
      judgeBackends: {
        llm: createMockJudgeBackend({ id: 'llm' }),
        jev: createMockJevJudgeBackend(),
      },
      defaultJudge: 'llm',
    });
    try {
      const events: DMLEvent[] = [];
      for await (const event of sdk.runDML(`
        agent_main :-
          with_judgment(jev, choose("state", "Which team?", [billing, orders], Team)),
          answer(Team).
      `)) events.push(event);
      expect(answer(events)).toBe('billing');
      expect(events.some((event) => event.judgeBackend === 'jev')).toBe(true);
    } finally {
      await sdk.dispose();
    }
  });

  it('reports judge capabilities through the SDK', async () => {
    const sdk = await createDeepClause({
      model: 'test-model',
      judgeBackends: {
        jev: createMockJevJudgeBackend(),
        mock: createMockJudgeBackend(),
      },
      defaultJudge: 'jev',
    });
    try {
      expect(sdk.getJudgeBackends().sort()).toEqual(['jev', 'mock']);
      expect(sdk.getJudgeCapabilities('jev').calibrated).toBe(true);
      expect(sdk.getJudgeCapabilities().calibrated).toBe(true);
      expect(sdk.getJudgeCapabilities('mock').calibrated).toBe(false);
    } finally {
      await sdk.dispose();
    }
  });
});
