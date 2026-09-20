import { describe, expect, it, vi } from 'vitest';
import { createDeepClause, createJevJudgeBackend, type DMLEvent } from '../src/index.js';

const JEV_RESPONSE = {
  model: 'jev-1.13.0',
  answers: {
    q1: {
      type: 'choice',
      choice: 'orders',
      confidence: 0.82,
      probabilities: { billing: 0.08, orders: 0.85, account: 0.07 },
    },
    q2: {
      type: 'score',
      score: 1.2,
      confidence: 0.78,
      legend: { '0': 'calm', '1': 'frustrated', '2': 'angry' },
      probabilities: { '0': 0.1, '1': 0.6, '2': 0.3 },
    },
    q3: { type: 'noul', noul: 0.92 },
    q4: { type: 'noul', noul: 0.77 },
  },
  usage: { input_tokens: 300, output_tokens: 40 },
};

async function runWithBackend(backend: ReturnType<typeof createJevJudgeBackend>): Promise<DMLEvent[]> {
  const sdk = await createDeepClause({
    model: 'test-model',
    judgeBackends: { jev: backend },
    defaultJudge: 'jev',
  });
  try {
    const events: DMLEvent[] = [];
    for await (const event of sdk.runDML(
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
      { args: ['my order has not arrived'] },
    )) {
      events.push(event);
    }
    return events;
  } finally {
    await sdk.dispose();
  }
}

function answer(events: DMLEvent[]): string | undefined {
  return events.find((event) => event.type === 'answer')?.content;
}

describe('Jev judge backend', () => {
  it('maps Jev answers to judge values', async () => {
    const fetchImpl = vi.fn(async () =>
      new Response(JSON.stringify(JEV_RESPONSE), {
        status: 200,
        headers: { 'content-type': 'application/json' },
      }),
    );
    const events = await runWithBackend(
      createJevJudgeBackend({ apiKey: 'test-key', fetch: fetchImpl as unknown as typeof fetch }),
    );

    expect(answer(events)).toBe('orders|frustrated|yes|0.77');
    expect(fetchImpl).toHaveBeenCalledTimes(1);

    const usage = events.find((event) => event.type === 'usage');
    expect(usage?.usageSource).toBe('judge');
    expect(usage?.usageModel).toBe('jev-1.13.0');
    expect(usage?.usage?.inputTokens).toBe(300);
  });

  it('sends the state, model, and typed questions to System One', async () => {
    let captured: { url: string; body: unknown; auth?: string } | undefined;
    const fetchImpl = vi.fn(async (url: string | URL | Request, init?: RequestInit) => {
      captured = {
        url: String(url),
        body: JSON.parse(String(init?.body ?? '{}')) as unknown,
        auth: (init?.headers as Record<string, string> | undefined)?.Authorization,
      };
      return new Response(JSON.stringify(JEV_RESPONSE), { status: 200 });
    });
    await runWithBackend(
      createJevJudgeBackend({ apiKey: 'secret', fetch: fetchImpl as unknown as typeof fetch }),
    );

    expect(captured?.url).toBe('https://api.typesafe.ai/v1/systemone');
    expect(captured?.auth).toBe('Bearer secret');
    const body = captured?.body as { state?: string; model?: string; questions?: Record<string, { type: string }> };
    expect(body.state).toBe('my order has not arrived');
    expect(body.model).toBe('jev-latest');
    expect(body.questions?.q1?.type).toBe('choice');
    expect(body.questions?.q2?.type).toBe('score');
    expect(body.questions?.q3?.type).toBe('noul');
  });

  it('retries on 429 and then succeeds', async () => {
    let calls = 0;
    const fetchImpl = vi.fn(async () => {
      calls++;
      if (calls === 1) return new Response('rate limited', { status: 429 });
      return new Response(JSON.stringify(JEV_RESPONSE), { status: 200 });
    });
    const events = await runWithBackend(
      createJevJudgeBackend({ apiKey: 'test-key', fetch: fetchImpl as unknown as typeof fetch, maxRetries: 1 }),
    );
    expect(calls).toBe(2);
    expect(answer(events)).toBe('orders|frustrated|yes|0.77');
  });

  it('fails clearly without an API key', async () => {
    const events = await runWithBackend(createJevJudgeBackend({ fetch: vi.fn() as unknown as typeof fetch }));
    expect(answer(events)).toBeUndefined();
    expect(events.some((event) => event.type === 'log' && /API key/.test(event.content ?? ''))).toBe(true);
    expect(events.some((event) => event.type === 'error')).toBe(false);
  });
});
