# Changelog

## 0.0.89 - 2026-09-20

- Include the meta-interpreter correctness fixes: `assertz/1`, `asserta/1`,
  `retract/1`, and `retractall/1` now operate on the session module, and
  user-defined predicates backtrack through every solution again (previously
  an if-then-else committed to the first).
- Add generic semantic judgment predicates: `judge/2` (batch), `choose/4`,
  `rate/4`, `verify/3`, `probability/3`, `holds/2,3`, `with_judgment/2`, and
  `require_judgment/2`. Answers are constrained to the supplied options and
  levels, and `verify` is three-valued (`yes` / `no` / `unknown`).
- Add a `JudgeBackend` extension point next to `LLMBackend`, with `llm`, `jev`
  (TypeSafe System One), and `mock` implementations. `llm` is the default, so
  judgment predicates work without new credentials; Jev is opt-in and
  calibrated.
- Memoize judgments per run by backend, model, state, and questions, so
  backtracking reuses an answer instead of re-querying the model.
- Emit judge activity (`task_activity`) and usage (`usageSource: "judge"`)
  events; support `with_judgment/2` scoping, per-run backend overrides, and
  `require_judgment/2` capability gating.

## 0.0.87 - 2026-08-19

- Add an injectable `LLMBackend` for host-managed models, credentials, cancellation, tool calls, streaming callbacks, and usage.
- Preserve opaque provider-native assistant data across multi-step task loops.
- Route `llm/2`, token sampling, `task/N`, `prompt/N`, and nested execution through the injected backend.
- Support structured typed task results through injected backend tool calls.
- Keep DML-defined tool predicates visible inside task loops while enforcing runtime policy on their nested `exec/2` calls.
- Export backend, usage, and memory types from the package entry point.
