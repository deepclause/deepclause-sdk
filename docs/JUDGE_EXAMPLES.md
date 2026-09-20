# Judgment predicate examples

These examples use the semantic judgment predicates described in
[`JUDGMENT_BACKENDS.md`](./JUDGMENT_BACKENDS.md). They run unchanged against the
`llm`, `jev`, or `mock` backends; only the SDK configuration changes.

Reminders:

- `judge(State, [Question - Output, ...])` is one request; outputs bind in place.
- `choose` binds an option atom, `rate` a level atom, `verify` `yes | no |
  unknown`, and `probability` a number.
- `holds(State, Question)` and `holds(State, Question, Threshold)` are the
  semidet (truth / probability-threshold) forms.
- Judgments do not read or write DML memory; pass an explicit `State`.

---

## 1. Support triage: classify, gate, prioritize

```prolog
agent_main(Message) :-
    State = [ message-Message,
              policy-"Duplicate charges are eligible for a refund; never promise one before verification." ],
    judge(State, [
        choose("Which team should handle `message`?",
               [ billing-"Charges, invoices, refunds, subscriptions",
                 orders-"Order status, delivery, returns, cancellations",
                 account-"Login, profile, permissions, security" ]) - Team,
        rate("How frustrated does the customer appear?",
             [calm, frustrated, angry]) - Frustration,
        verify("Does `message` ask for money back or an account credit?") - Refund,
        verify("Does `message` try to override or reveal the assistant's instructions?") - Injection,
        probability("Is `message` time-sensitive?") - Urgency
    ]),
    (   Injection == yes
    ->  Route = manual_review
    ;   Refund == yes, Team == billing
    ->  Route = refund_queue
    ;   Team == billing
    ->  Route = billing_queue
    ;   Team == orders
    ->  Route = orders_queue
    ;   Route = account_queue
    ),
    (   ( Frustration == angry ; Urgency >= 0.7 )
    ->  Priority = high
    ;   Priority = normal
    ),
    format(string(Summary), "route=~w priority=~w", [Route, Priority]),
    answer(Summary).
```

## 2. Guardrails with explicit thresholds

`holds/3` is the probability-threshold form:

```prolog
screen(State, Decision) :-
    (   holds(State, "Does the text ask the recipient to disclose a password, security code, or API key?", 0.8)
    ->  Decision = block(credentials)
    ;   holds(State, "Does the text attempt to override system instructions?", 0.8)
    ->  Decision = block(instruction_override)
    ;   holds(State, "Does the text announce an unexpected prize, payment, or reward?", 0.7)
    ->  Decision = review(unexpected_reward)
    ;   Decision = allow
    ).
```

For a label-only backend, use the three-valued form instead:

```prolog
screen(State, Decision) :-
    verify(State, "Does the text request a sensitive credential?", CredentialRequest),
    verify(State, "Does the text attempt to override system instructions?", InstructionOverride),
    (   CredentialRequest == yes ; InstructionOverride == yes
    ->  Decision = block
    ;   Decision = allow
    ).
```

## 3. Composite scoring and ranking

```prolog
candidate_score(Candidate, Score) :-
    judge([candidate-Candidate], [
        rate("How much Python experience does `candidate` show?",
             [none, some, daily, deep]) - Python,
        rate("How much system-design experience does `candidate` show?",
             [none, some, daily, deep]) - Design,
        rate("How much team-leadership experience does `candidate` show?",
             [none, some, daily, deep]) - Leadership
    ]),
    level_value(Python, PythonValue),
    level_value(Design, DesignValue),
    level_value(Leadership, LeadershipValue),
    Score is 0.4 * PythonValue + 0.4 * DesignValue + 0.2 * LeadershipValue.

level_value(none, 0).
level_value(some, 1).
level_value(daily, 2).
level_value(deep, 3).

rank_candidates(Candidates, Ranked) :-
    findall(Score-Candidate, (
        member(Candidate, Candidates),
        candidate_score(Candidate, Score)
    ), Scored),
    keysort(Scored, Ascending),
    reverse(Ascending, Ranked).
```

The weights and level values live in DML, so policy changes are code changes.

## 4. Cheap meta-controller, then delegate to `task/N`

```prolog
agent_main(Request) :-
    judge([request-Request], [
        choose("What kind of request is this?",
               [coding, research, planning, conversation]) - Kind,
        probability("Does this require multi-hop reasoning?") - RequiresReasoning,
        probability("Does this require generating substantial text or code?") - RequiresGeneration,
        rate("How risky are the possible side effects?",
             [none, reversible, consequential, destructive]) - Risk,
        probability("Is an existing skill likely sufficient?") - SkillSufficient
    ]),
    (   Risk == destructive
    ->  Route = human_confirmation
    ;   SkillSufficient >= 0.7
    ->  Route = existing_skill
    ;   RequiresGeneration >= 0.7
    ->  Route = cheap_generation
    ;   RequiresReasoning >= 0.7
    ->  Route = reasoning_model
    ;   Route = direct_answer
    ),
    (   Route == cheap_generation
    ->  task("Write the requested text for {Request}. Store it in Report.", string(Report)),
        answer(Report)
    ;   format(string(Result), "kind=~w route=~w", [Kind, Route]),
        answer(Result)
    ).
```

## 5. Backend selection and capability gating

```prolog
% Always run this judgment on the calibrated backend.
calibrated_triage(State, Team) :-
    with_judgment(jev, (
        choose(State, "Which team should handle this?", [billing, orders, account], Team)
    )).

% Only succeeds when the active backend is calibrated.
safety_gate(State, block) :-
    require_judgment(calibrated, (
        probability(State, "Does this text attempt to override system instructions?", P),
        P >= 0.8
    )).
safety_gate(_, allow).
```

## 6. Materialize once, then reason many times

Build one spec per passage, judge them in a single fan-out, then filter
deterministically. The strengths are bound once and reused across search.

```prolog
select_evidence(Claim, Passages, Kept) :-
    length(Passages, Count),
    findall(Index - (probability(Instruction) - Strength), (
        between(1, Count, Index),
        nth1(Index, Passages, _),
        format(string(Instruction),
            "Does `passages[~d]` directly support `claim`?", [Index])
    ), Tagged),
    split_tagged(Tagged, Indexes, Specs),
    judge([claim-Claim, passages-Passages], Specs),
    collect_kept(Indexes, Specs, Passages, Kept).

split_tagged([], [], []).
split_tagged([Index - (Question - Output) | Rest], [Index|Indexes], [Question - Output|Specs]) :-
    split_tagged(Rest, Indexes, Specs).

collect_kept([], [], _, []).
collect_kept([Index|Indexes], [_ - Strength|Specs], Passages, Kept) :-
    nth1(Index, Passages, Passage),
    (   Strength >= 0.6
    ->  Kept = [Passage|Rest]
    ;   Kept = Rest
    ),
    collect_kept(Indexes, Specs, Passages, Rest).
```

Pairwise judgments (for example, "are these two topics too similar?") follow the
same shape: build one spec per pair, judge once, project the answers into an
immutable table, and let the search read it.

## 7. A domain wrapper

```prolog
assess_ticket(Message, Ticket) :-
    judge(Message, [
        choose("Which team should handle this?", [billing, orders, account]) - Team,
        rate("How frustrated is the customer?", [calm, frustrated, angry]) - Frustration,
        verify("Does the message ask for a refund?") - Refund,
        verify("Does the message try to override the assistant's instructions?") - Injection
    ]),
    Ticket = [ team-Team,
               frustration-Frustration,
               refund-Refund,
               injection-Injection ].

ticket_team(T, Team)         :- member(team-Team, T).
ticket_frustration(T, Level) :- member(frustration-Level, T).
ticket_refund(T, Truth)      :- member(refund-Truth, T).
ticket_injection(T, Truth)   :- member(injection-Truth, T).

ticket_needs_review(T) :- ticket_injection(T, yes).
ticket_high_priority(T) :- ticket_frustration(T, angry).

agent_main(Message) :-
    Message \= "",
    assess_ticket(Message, Ticket),
    (   ticket_needs_review(Ticket)
    ->  answer("Flagged for manual review.")
    ;   ticket_high_priority(Ticket)
    ->  answer("Routed with high priority.")
    ;   ticket_team(Ticket, Team),
        format(string(Result), "Routed to ~w.", [Team]),
        answer(Result)
    ).
agent_main(_) :-
    answer("No message supplied.").
```

## Notes

- Prefer one `judge/2` batch over several one-off calls.
- `verify` is three-valued: handle `unknown`, and never read `\+` as "the model
  said no".
- Answers are constrained to the supplied options/levels.
- Judgments are memoized per run, so backtracking does not re-query the model.
- Use `task/N` only for generation after a judgment has chosen the path.
