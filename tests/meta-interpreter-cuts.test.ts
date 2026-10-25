import { describe, expect, it } from "vitest";
import { createDeepClause } from "../src/index.js";

/**
 * Cut semantics for user-defined predicates.
 *
 * `!` cannot be interpreted faithfully (see docs/META_INTERPRETER_CUTS.md), so a
 * *pure* predicate that contains a cut runs natively with call/1. A predicate
 * that also needs state threading (task/N, exec/2, output/1, ...) stays
 * interpreted, where the cut remains inert; the last test pins that limitation.
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

/** Run goals through one REPL session, returning each goal's solution list. */
async function solutionsOf(code: string, ...goals: string[]): Promise<string[][]> {
  const { createHarnessRepl } = await import("../src/repl/index.js");
  const repl = await createHarnessRepl({ model: "cut-test", provider: "openai", autoLoadHarness: false });
  try {
    const loaded = await repl.session.loadCode(code);
    expect(loaded.error).toBeUndefined();
    const results: string[][] = [];
    for (const goal of goals) {
      const out: string[] = [];
      for await (const event of repl.query(goal)) {
        if (event.type === "solution") out.push(event.solution?.bindings.map((b) => `${b.name}=${b.value}`).join(",") ?? "");
        if (event.type === "error") out.push(`ERROR ${event.content}`);
      }
      results.push(out);
    }
    return results;
  } finally {
    await repl.dispose();
  }
}

describe("meta-interpreter cut semantics", () => {
  it("honors a cut inside a user-defined predicate", async () => {
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

  it("commits the clause set, so an effectful clause does not replay", async () => {
    const code = `
      p(1). p(2).
      effect(X) :- assertz(ran(one)), p(X), !.
      effect(_) :- assertz(ran(two)).
    `;
    const [effect, log] = await solutionsOf(code, "effect(X)", "findall(R, ran(R), L)");
    expect(effect).toEqual(["X=1"]);
    expect(log).toEqual(["R=_,L=[one]"]);
  });

  it("keeps a cut local to its clause and committed inside call/1 and findall/3", async () => {
    const code = `
      p(1). p(2). p(3).
      q(X) :- p(X), !.
      g(X) :- q(X).
      f(L) :- findall(X, (p(X), !), L).
      c(X) :- call((p(X), !)).
      n :- \\+ (p(_), !).
      d(X) :- ( p(X), ! ; X = 99 ).
    `;
    const [g, f, c, n, d] = await solutionsOf(code, "g(X)", "f(L)", "c(X)", "n", "d(X)");
    expect(g).toEqual(["X=1"]);
    expect(f).toEqual(["L=[1]"]);
    expect(c).toEqual(["X=1"]);
    expect(n).toEqual([]);   // \+ (p(_), !) succeeds, so n fails
    expect(d).toEqual(["X=1"]);
  });

  it("commits a cut typed directly as a query", async () => {
    const code = "p(1). p(2). p(3).";
    expect(await solutionsOf(code, "p(X), !")).toEqual([["X=1"]]);
  });

  it("honours a cut in a predicate that needs the interpreter (host predicate)", async () => {
    // output/1 requires state threading, so the body is interpreted; the cut is
    // lowered to once(...) + a per-call commit marker instead.
    const code = `
      p(1). p(2). p(3).
      imp(X)  :- p(X), output(seen), !.
      imp(_)  :- p(wrong).
    `;
    expect(await solutionsOf(code, "imp(X)")).toEqual([["X=1"]]);
  });

  it("scopes lowered cuts like Prolog: disjunction, call/1, negation, mid-body", async () => {
    const code = `
      p(1). p(2). p(3).
      dis(X)  :- ( p(X), output(seen), ! ; X = 99 ).
      cal(X)  :- call(( p(X), output(seen), ! )).
      cal(99).
      neg     :- not(( p(_), output(seen), ! )).
      mid(X,Y):- p(X), output(seen), !, p(Y).
      plain(X):- p(X), output(seen).
    `;
    const [dis, cal, neg, mid, plain] = await solutionsOf(code, "dis(X)", "cal(X)", "neg", "mid(X,Y)", "plain(X)");
    expect(dis).toEqual(["X=1"]);                       // right branch pruned
    expect(cal).toEqual(["X=1", "X=99"]);               // call/1 is its own scope
    expect(neg).toEqual([]);                            // the cut goal succeeds, so neg fails
    expect(mid).toEqual(["X=1,Y=1", "X=1,Y=2", "X=1,Y=3"]); // post-cut goals enumerate
    expect(plain).toEqual(["X=1", "X=2", "X=3"]);       // no cut: unchanged
  });
});
