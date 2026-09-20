import { describe, expect, it } from "vitest";
import { createDeepClause } from "../src/index.js";

/**
 * Regression coverage for `docs/META_INTERPRETER_CUTS.md`.
 *
 * The first test is marked `it.fails` because the meta-interpreter does not
 * currently honor `!` inside user-defined predicates. When that is fixed,
 * change `it.fails` to `it`.
 */
async function answerOf(code: string): Promise<string | undefined> {
  const sdk = await createDeepClause({ model: "cut-test" });
  try {
    let answer: string | undefined;
    for await (const event of sdk.runDML(code)) {
      if (event.type === "answer") answer = event.content;
    }
    return answer;
  } finally {
    await sdk.dispose();
  }
}

describe("meta-interpreter cut semantics (known issue)", () => {
  it.fails("honors a cut inside a user-defined predicate", async () => {
    const answer = await answerOf(`
      first(X) :- X = 1, !.
      first(X) :- X = 2.
      agent_main :-
        findall(X, first(X), Xs),
        format(string(R), "~w", [Xs]),
        answer(R).
    `);
    expect(answer).toBe("[1]");
  });

  it("still backtracks through every solution of a helper rule", async () => {
    // Guards against reintroducing the first-solution commit.
    const answer = await answerOf(`
      pair(1). pair(2). pair(3).
      agent_main :-
        findall(X, pair(X), Xs),
        length(Xs, N),
        format(string(R), "~w", [N]),
        answer(R).
    `);
    expect(answer).toBe("3");
  });
});
