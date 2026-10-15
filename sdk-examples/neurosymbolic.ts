/**
 * Neuro-symbolic: the model reads the prose, the logic makes the decision.
 *
 *   npx tsx sdk-examples/neurosymbolic.ts
 *
 * A refund policy arrives as English. The model extracts it into a declared
 * shape (`schema/2`); Prolog then applies it to every order exactly, so the
 * model never sees the order table and cannot miscount a refund. Swap the policy
 * text and the same rules produce a different, still-exact answer.
 */
import { backendFrom, exampleSdk, modelLabel, textResponse } from './_offline.js';

const POLICY = `
Refunds: approve any order over $75 placed within the last 30 days.
Enterprise customers are refunded regardless of amount, but only within 60 days.
`;

const DML = `
%% The shape of the model's answer. A missing required field, or a field of the
%% wrong type, fails the task instead of quietly becoming empty.
schema(policy, [
    field(threshold,         integer, required),
    field(consumer_days,     integer, required),
    field(enterprise_days,   integer, required),
    field(enterprise_always, boolean, default(false))
]).

%% The orders. The model is never shown these.
order(o1, alice, enterprise,  40, 10).
order(o2, bob,   consumer,   120, 45).
order(o3, carol, consumer,    90,  5).
order(o4, dave,  enterprise,  15, 80).
order(o5, erin,  consumer,    75, 30).

refund(Policy, Tier, Amount, Days) :-
    get_dict(threshold, Policy, Threshold),
    get_dict(consumer_days, Policy, ConsumerDays),
    get_dict(enterprise_days, Policy, EnterpriseDays),
    get_dict(enterprise_always, Policy, EnterpriseAlways),
    (   Tier == enterprise
    ->  ( EnterpriseAlways == true ; Amount >= Threshold ),
        Days =< EnterpriseDays
    ;   Amount >= Threshold,
        Days =< ConsumerDays
    ).

refunded(Policy, Customers) :-
    findall(Customer, (
        order(_, Customer, Tier, Amount, Days),
        refund(Policy, Tier, Amount, Days)
    ), Customers).

agent_main(PolicyText) :-
    system("Read the refund policy. Extract the numeric threshold, the two day windows, and whether enterprise customers are always refunded."),
    task("Policy: {PolicyText}", schema(policy, Policy)),
    refunded(Policy, Customers),
    format(string(Summary), "refund ~w", [Customers]),
    answer(Summary).
`;

async function main(): Promise<void> {
  console.log(`model: ${modelLabel()}\n`);
  const dc = await exampleSdk(
    backendFrom((request) =>
      textResponse(request, {
        threshold: 75,
        consumer_days: 30,
        enterprise_days: 60,
        enterprise_always: false,
      }),
    ),
  );

  try {
    for await (const event of dc.runDML(DML, { args: [POLICY] })) {
      if (event.type === 'output' || event.type === 'answer') console.log(event.content);
      else if (event.type === 'error') console.error('error:', event.content);
    }
  } finally {
    await dc.dispose();
  }
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
