import { describe, expect, it } from 'vitest';
import { createDeepClause, createJevJudgeBackend, type DMLEvent } from '../src/index.js';

/**
 * Live integration against the real TypeSafe System One API.
 *
 * These tests only run when TYPESAFE_API_KEY is set, so CI without a key skips
 * them. They are the only place the real HTTP round trip is exercised; the
 * mapping, retry, and error paths are covered offline by jev-backend.test.ts.
 */
const apiKey = process.env.TYPESAFE_API_KEY;
const live = apiKey ? describe : describe.skip;

async function run(code: string, args: string[], key = apiKey ?? ''): Promise<DMLEvent[]> {
  const sdk = await createDeepClause({
    model: 'jev',
    judgeBackends: { jev: createJevJudgeBackend({ apiKey: key, maxRetries: 0 }) },
    defaultJudge: 'jev',
  });
  try {
    const events: DMLEvent[] = [];
    for await (const event of sdk.runDML(code, { args })) events.push(event);
    return events;
  } finally {
    await sdk.dispose();
  }
}

live('Jev live integration', () => {
  it('answers a choose/verify/probability batch', async () => {
    const events = await run(
      `
      agent_main(Message) :-
        judge([message-Message], [
          choose("Which team should handle this message?",
                 [ billing-"Charges, invoices, refunds",
                   orders-"Delivery, returns, cancellations",
                   account-"Login, profile, security" ]) - Team,
          verify("Does the message ask for money back or a credit?") - Refund,
          probability("Is the message time-sensitive?") - Urgency
        ]),
        format(string(R), "~w|~w|~w", [Team, Refund, Urgency]),
        answer(R).
      `,
      ['I was charged twice and need a refund today!'],
    );

    const answer = events.find((event) => event.type === 'answer')?.content ?? '';
    const [team, refund, urgency] = answer.split('|');
    expect(['billing', 'orders', 'account']).toContain(team);
    expect(['yes', 'no', 'unknown']).toContain(refund);
    expect(Number(urgency)).toBeGreaterThanOrEqual(0);
    expect(Number(urgency)).toBeLessThanOrEqual(1);
    expect(events.some((event) => event.type === 'usage' && event.usageSource === 'judge')).toBe(true);
  });

  it('surfaces a bad key as a judge failure rather than a crash', async () => {
    const events = await run(
      `agent_main(M) :- verify(M, "Is this urgent?", T), answer(T).`,
      ['x'],
      'invalid-key',
    );
    expect(events.some((event) => event.type === 'log' && /judgment failed/.test(event.content ?? ''))).toBe(true);
    expect(events.some((event) => event.type === 'finished')).toBe(true);
  });
});
