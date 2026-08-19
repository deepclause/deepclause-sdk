import { describe, expect, it } from "vitest";
import { createDeepClause } from "../src/index.js";
import type { LLMBackend, LLMBackendRequest } from "../src/index.js";

function answer(events: Array<{ type: string; content?: string }>): string | undefined {
  return events.find((event) => event.type === "answer")?.content;
}

describe("injectable LLM backend", () => {
  it("executes llm/2 without provider credentials", async () => {
    const requests: LLMBackendRequest[] = [];
    const backend: LLMBackend = {
      async complete(request) {
        requests.push(request);
        return {
          text: "Hosted response",
          usage: { inputTokens: 4, outputTokens: 2, totalTokens: 6 },
        };
      },
    };
    const sdk = await createDeepClause({ model: "pi-active-model", llmBackend: backend });
    try {
      const events = [];
      for await (const event of sdk.runDML(`
        agent_main :-
          user("Question from DML"),
          get_memory(Messages),
          llm(Messages, Reply),
          answer(Reply).
      `)) events.push(event);

      expect(answer(events)).toBe("Hosted response");
      expect(requests).toHaveLength(1);
      expect(requests[0]?.messages.some((message) => message.content === "Question from DML")).toBe(true);
      expect(events.some((event) => event.type === "usage")).toBe(true);
    } finally {
      await sdk.dispose();
    }
  });

  it("supports task output through backend tool calls", async () => {
    const backend: LLMBackend = {
      async complete(request) {
        const resultTool = request.tools?.find((tool) => tool.name === "set_result");
        expect(resultTool).toBeDefined();
        return {
          text: "",
          toolCalls: [
            { id: "set-1", name: "set_result", arguments: { variable: "Result", value: "hosted" } },
            { id: "finish-1", name: "finish", arguments: { success: true } },
          ],
          usage: { inputTokens: 8, outputTokens: 3, totalTokens: 11 },
        };
      },
    };
    const sdk = await createDeepClause({ model: "pi-active-model", llmBackend: backend });
    try {
      const events = [];
      for await (const event of sdk.runDML(`
        agent_main :-
          task("Return a hosted result", Result),
          answer(Result).
      `)) events.push(event);
      expect(answer(events)).toBe("hosted");
    } finally {
      await sdk.dispose();
    }
  });
});
