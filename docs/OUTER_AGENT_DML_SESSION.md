# Outer Agent + DML Session + Quarantined KB

Status: **design draft — not implemented.**

This document describes a DML-native architecture for a general conversational
agent that can collect external data into a quarantined KB and execute generated
DML in a separate session. It is deliberately not a new DSL or tool framework:
skills are DML predicates, the KB is DML facts, policy is DML predicates, and the
meta-interpreter (MI) is the enforcement point.

## 1. Goals

- Keep raw external data out of the outer agent's conversational context by
  default.
- Let the outer agent orchestrate collection and execution without letting it
  assign trust or bypass policy.
- Let the inner DML session run generated code with explicit capabilities.
- Track labels and provenance from ingestion through prompts, memory, tool calls,
  and results.
- Block exfiltration, destructive actions, memory poisoning, and capability
  escalation at the MI boundary.
- Preserve utility: a single `task/N` can run as a one-off DML goal; recurring
  work can become a DML predicate.

## 2. Non-goals

- A proof of information-flow security. This is runtime enforcement with
  conservative policies.
- A model-correctness or calibration guarantee.
- Multi-tenant isolation beyond per-session scoping (future work).
- A policy editor or a visual KB browser (future work).
- A separate IR, tool catalog, or skill registry. All composition is DML.

## 3. Trust model

| Principal | Trust | Notes |
|---|---|---|
| User | Trusted for policy, approvals, declassification | May still supply untrusted data |
| Outer agent | **Untrusted** | Can be prompt-injected; generates untrusted code and prompts |
| `collect` tool | Trusted ingestion boundary | Assigns labels by source and policy |
| `execute` tool | Trusted execution boundary | Enforces pre-checks and capabilities |
| Inner DML session | Trusted interpreter, untrusted inputs | Runs generated DML; cannot mutate policy/KB protected facts |
| KB | Mixed | Values carry labels and provenance |
| Tools/models | Sinks | Capability-gated; external sinks restricted by labels |
| Audit | Trusted record | Append-only from the runtime's perspective |

The outer agent is not trusted to label data, declassify, or grant itself
capabilities. Its flags are advisory; its generated DML and prompts are
untrusted.

## 4. Components

### 4.1 Outer agent

- Owns the outer system prompt.
- Receives user messages and data references.
- Can call `collect` and `execute`.
- Sees KB handles, schemas, labels, provenance, and result summaries — not raw
  sensitive/untrusted values by default.
- Orchestrates a sequence of collect/execute calls for one user request.

### 4.2 `collect`

- Trusted ingestion tool exposed to the outer agent.
- Input: a source specification (`web`, `file`, `email`, `user_upload`, ...).
- Fetches or accepts raw content, hashes it, validates/sanitizes it, runs
  detectors, assigns labels, stores the raw value, and returns a handle.
- The outer agent cannot supply or override labels.
- Detector flags (prompt injection, PII, malware, suspicious content) become
  additional labels; `untrusted` remains the default for external sources.

### 4.3 `execute`

- Trusted execution tool exposed to the outer agent.
- Input: DML code or a DML goal, arguments, capability request, optional
  `session_id` and `reset_memory`.
- Performs pre-execution checks, loads the code into the inner session sandbox,
  runs it, and returns a labeled result envelope.
- Persists the inner session's conversational memory by default; code is
  ephemeral unless an explicit persistence capability is granted.
- Never exposes `collect` or `execute` to the inner session.

### 4.4 Inner DML session

- Separate DML module and memory.
- Initialized with a derived system prompt: the outer prompt's safety/policy
  context, minus `collect`/`execute` and orchestration controls, plus the task
  instructions, inner tool catalog, and accessible KB handles/schemas.
- Receives only the tools granted for this execution.
- Runs generated DML (`task/N`, `prompt/N`, `judge/N`, tools, CLP, etc.).
- Cannot call `collect`/`execute`, mutate policy or KB protected facts, access
  protected modules, or recurse into the outer agent.
- Persists memory across execute calls, with labels and provenance.

### 4.5 KB

- Protected DML facts:

  ```prolog
  kb(Handle, Value, Labels, Provenance).
  ```

- Accessed through capability-checked predicates:

  ```prolog
  kb_labels/2, kb_provenance/2, kb_schema/2,
  kb_ref/2, kb_derive/3, kb_write/3.
  ```

- Values are never read directly by the outer agent; the outer agent receives
  handles and metadata.

### 4.6 Policy

- Protected DML predicates, e.g.:

  ```prolog
  deny/2, allow/2, accepts/2, requires_approval/1, declassify/3.
  ```

- Enforced by the MI before effects.
- Not editable by the outer or inner agent.

### 4.7 Audit

- Records ingestion, execution, prompts, KB access, labels, model/tool calls,
  denials, approvals, declassifications, usage, and result labels.
- Stored with the session or harness history; redacted.

## 5. Session model

- One inner DML session per outer conversation/user by default.
- Session scope is user/tenant/conversation; never shared across users.
- Inner conversational memory persists across `execute` calls unless
  `reset_memory` is requested.
- Memory messages carry labels. Sensitive data remains sensitive; untrusted data
  remains untrusted.
- Generated code runs in a per-execute sandbox module. Code persistence or skill
  promotion requires an explicit capability and review.
- Inner tools are an allowlist per execute call. `collect`/`execute`, policy/KB
  mutation, protected modules, and recursive control tools are never installed.
- Inner session TTL and KB scratch TTL are policy-controlled.

## 6. Data and label model

Labels are multi-dimensional:

| Dimension | Values | Meaning |
|---|---|---|
| Integrity | `trusted`, `untrusted` | May this influence instructions/control flow? |
| Confidentiality | `public`, `internal`, `sensitive`, `secret` | Who may see it? External sinks? |
| Source | `user`, `file`, `web`, `email`, `tool`, `model`, `erp`, `crm` | Where did it come from? |
| Flags | `flagged_injection`, `pii`, `malware`, ... | Detector findings; advisory additions |
| Provenance | source URI, hash, auth, timestamp | Audit/replay |

Rules:

- Labels are joined on derivation: `kb_derive` joins all input labels.
- `untrusted` is sticky.
- `sensitive`/`secret` are sticky.
- The outer agent cannot assign or lower labels.
- Declassification is privileged and audited.
- Prompt labels are part of the task context; a prompt generated by the outer
  agent is `untrusted`.

## 7. Tool contracts

### 7.1 `collect`

```prolog
collect(SourceSpec, Handle)
```

Examples:

```prolog
collect(web("https://example.com/report"), H).
collect(file("inbox/invoice.pdf"), H).
collect(email("msg_77"), H).
collect(user_upload, H).
```

Behavior:

1. Identify/authenticate the source.
2. Fetch/accept the raw content.
3. Record hash, MIME, size, encoding, timestamp, source metadata.
4. Validate/sanitize; never interpret content as instructions.
5. Run injection/PII/malware detectors.
6. Assign labels by source/policy:
   - external web/email → `untrusted` + source + confidentiality,
   - user upload → `untrusted`, `user` + confidentiality,
   - authenticated internal system → `trusted`, `internal` only if policy
     allows; content is still not automatically instruction-safe,
   - detector hits add flags.
7. Store the raw value in the appropriate partition.
8. Return `handle`, `labels`, `flags`, `schema`, `provenance`.
9. Audit the ingestion.

If the outer agent passes raw text instead of a source spec, it has already seen
that text; the value is labeled `untrusted, agent` and the session should treat
it as exposed. Prefer source specs so the outer agent never sees raw content.

### 7.2 `execute`

```prolog
execute(CodeOrGoal, Args, CapabilityRequest, Result)
```

Capability request fields:

- `kb_read`: handles or query scopes,
- `kb_write`: session partition(s),
- `tools`: allowed inner tool names,
- `models`: allowed models (local/external),
- `network`: false/allowlist,
- `budget`: tokens, time, tool calls,
- `approvals`: requested dangerous operations,
- `reset_memory`: optional.

The runtime may narrow or deny the request. The code cannot exceed the grant.

### 7.3 Inner tools

- DML predicates and `tool/2` annotations installed only for the granted set.
- No `collect`, `execute`, policy/KB mutation, protected modules, or recursive
  control tools.
- Tool outputs are labeled `tool, untrusted` by default.
- Model-generated tool calls are checked against the task context labels and the
  tool's action policy.

## 8. Request flow

```
User
  → Outer agent
    → collect(source) → handle + labels + flags
    → execute(DML, args, capabilities) → inner DML session
        → pre-execution checks
        → MI execution
        → result envelope
    → more collect/execute calls as needed
  → Outer agent summarizes for user
```

## 9. Pre-execution checks (`execute`)

Before loading or running generated DML:

### 9.1 Code provenance and syntax

- Mark code from the outer agent `untrusted`.
- Parse and validate DML.
- Reject protected predicates and protected modules (`kb`, `policy`, `audit`,
  runtime internals).
- Reject `collect`/`execute` and recursive outer-agent calls.
- `consult`/`use_module`/`assert`/`retract` only if explicitly allowed and only
  inside the session sandbox.
- No runtime self-modification.

### 9.2 Capability request

- Validate requested KB handles/partitions, tools, models, network, budget, and
  approvals.
- Grant, narrow, or deny.
- The code cannot exceed the grant.

### 9.3 Inner tools

- Install only the granted tools.
- Remove `collect`/`execute`.
- No policy/KB mutation.
- Tool schemas/descriptions available to the inner model.

### 9.4 Data and labels

- Requested KB handles exist and are readable.
- Handles, labels, schemas, and provenance are declared.
- Sensitive/untrusted handles are only accessible to allowed paths.
- No direct `assert`/`retract` on KB facts.
- No declassification unless a capability/authority is requested.

### 9.5 Prompt/task declarations

- Inspect `task/N`/`prompt/N` prompts:
  - generated prompt → `untrusted_instruction`,
  - trusted template/skill → `trusted`,
  - interpolated data labels are included.
- Untrusted prompts are restricted by policy (tools/models/memory).
- Prefer trusted templates with labeled data slots.
- No raw sensitive data in instruction positions without policy.

### 9.6 Policy

- Declared effects vs policy.
- External model/network with sensitive/untrusted data → deny.
- Destructive tools → approval.
- Memory writes with sensitive/untrusted labels → policy.
- Declassification → authority.
- Recursive control tools → deny.

### 9.7 Session state

- Inner memory labels/size.
- No cross-user/tenant state.
- No policy/KB mutation.
- No credentials in the inner session.
- No access to outer conversation memory except declared input handles/args.

### 9.8 Resources

- Gas, time, tokens, tool calls, process count, KB write size.
- Concurrency limits.
- Cancellation signal.

### 9.9 Audit and transaction

- Record code hash, capabilities, KB snapshot/version, session id, prompt hashes,
  requested approvals.
- Open a KB write transaction.
- On failure, roll back KB writes; external effects are logged, not undone.

### 9.10 Approval

- Dangerous tools, external network, declassification, or high-risk untrusted
  prompts may require user approval.
- The outer agent cannot self-approve.

If a check fails, `execute` returns a structured denial; the outer agent can
repair the code, request a capability, or ask the user.

## 10. MI enforcement during execution

### 10.1 Label propagation

- Values, handles, memory messages, prompts, task context, and tool-call
  arguments carry labels.
- `kb_derive` joins input labels.
- `task`/`prompt` output joins prompt/data/model labels.
- Tool output is labeled `tool, untrusted` by default.
- Control-flow labels join on branches.

### 10.2 `task/N` and `prompt/N`

- Prompt trust: `trusted` template/skill vs `untrusted` generated prompt.
- Data labels: interpolated handles/values.
- Task labels = prompt labels + memory labels + data-read labels.
- Tool calls checked against task labels + args + model.
- Untrusted prompts cannot write global memory; use fresh/quarantine memory.
- Output labels join task context + model.

### 10.3 Tool calls

```prolog
CallLabels = join(TaskLabels, ArgLabels, [model]).
accepts(Tool, CallLabels).
requires_approval(Tool).
```

- External tools accept only `public` (or policy-approved) labels.
- Local tools may accept sensitive/untrusted data.
- Destructive tools require approval regardless of data labels.
- Denied calls return structured tool errors; the model can adapt.
- Every allow/deny/approval is audited.

### 10.4 KB access

- `kb_ref`/`kb_write`/`kb_derive` check capabilities.
- Labels join; no silent declassification.
- Protected facts cannot be asserted/retracted.
- Quarantined tasks may read raw untrusted data; the trusted planner may not.

### 10.5 Memory

- Messages carry labels.
- Sensitive data cannot go to an external model.
- Untrusted instructions cannot be written to `system/1` or global memory.
- Quarantined tasks use fresh memory.
- Persistent inner memory keeps labels; future tool calls are gated.
- Compaction/summarization preserves labels; sensitive data cannot go to an
  external summarizer.

### 10.6 Sinks

Checked at `exec`/host tools, `task`/`prompt`, `judge`, `output`/`answer`,
`write_file`, `email_send`/`http_post`, `spawn`/`send`/`receive`,
`assert`/`retract`/`consult`, and `kb_write`.

### 10.7 Control flow

- Branch labels join.
- Untrusted data cannot control a branch leading to a sensitive action.
- Conservative option: if task context is sensitive/untrusted, deny external
  tools for the whole task, even if arguments look public.

### 10.8 Subagents and processes

- Labels copy on `spawn`/`send`.
- Capabilities intersect with the parent; no escalation.
- No `collect`/`execute`.
- No policy/KB mutation.
- Audit process tree and labels.

### 10.9 Declassification

- Requires capability/authority.
- User approval or named trusted validator only.
- Produces a new handle/label; provenance and audit.
- The model cannot declassify.

### 10.10 Resources and cancellation

- Enforce gas, time, tokens, tool calls, KB size, process count.
- On budget exhaustion, stop and return partial results.
- On cancellation, stop, clean up, audit.

### 10.11 Errors

- Structured, redacted errors.
- No raw sensitive values in messages/logs.
- No silent success on denial.
- KB transaction rollback; external effects logged.

## 11. Result envelope

Return to the outer agent:

```json
{
  "status": "ok",
  "answer": "...",
  "values": [
    {"handle": "...", "labels": ["sensitive"], "schema": "..."}
  ],
  "labels": ["public"],
  "events": ["kb.read", "local.judge", "tool.denied:email_send"],
  "usage": {"tokens": 1234, "time_ms": 900},
  "audit_id": "aud_42"
}
```

The outer agent sees handles, labels, summaries, and events — not raw sensitive
data.

## 12. Approval, declassification, and failure

- **Pre-check failure:** deny before execution; structured reason; no side
  effects.
- **Tool denial:** model gets a tool error; can adapt; audit.
- **Budget/timeout:** stop; partial results; audit.
- **KB write failure:** rollback transaction; external effects logged.
- **Approval:** user approves a specific tool call, declassification, or
  high-risk operation; recorded.
- **Declassification:** trusted validator or user; new handle; labels lowered;
  audit.
- **Injection flag:** `collect` adds `flagged_injection`; data remains
  `untrusted`; task is quarantined; tools are gated.

## 13. What stays in DML

- Skills are predicates.
- Tools are predicates / `tool/2` annotations.
- The KB is `kb/4` facts.
- Labels and provenance are facts.
- Policy is `deny/2`, `allow/2`, `accepts/2`, `requires_approval/1`,
  `declassify/3`.
- Quarantine is `prompt/N`/`task/N` with `with_fresh_memory`/`with_tools`.
- The living module or loaded DML source is the skill library.
- The MI is the policy point.
- A one-off `task/N` can run as a DML goal; a recurring task can become a DML
  predicate.

No IR, no tool catalog, no separate skill format.

## 14. Open questions

1. Inner session lifetime: per request, per conversation, or per user?
2. Default inner memory reset policy.
3. Code persistence/skill promotion policy.
4. Exact derivation of the inner system prompt.
5. Detector set/thresholds for `collect`, and how flags affect policy.
6. Whether untrusted prompts may ever use tools (default: no).
7. External model policy for `internal` vs `sensitive` data.
8. Cross-user/tenant isolation and KB partitioning.
9. Result redaction defaults.
10. Audit retention and storage location.

## 15. Summary

The design is:

- The outer agent orchestrates `collect` and `execute`.
- `collect` is a trusted ingestion tool that labels data; the outer agent cannot
  label or declassify.
- `execute` runs generated DML in a separate inner session with a derived prompt,
  granted tools, persistent labeled memory, and no control tools.
- Pre-execution checks enforce code/capability/tool/data/prompt/policy/session/
  resource constraints.
- The MI enforces dynamic label propagation, task/tool/memory policy, control-
  flow taint, KB access, declassification, budgets, and redacted results.
- The outer agent sees handles, labels, summaries, and events — not raw sensitive
  data.

This keeps the system DML-native: skills are predicates, data is facts, policy is
predicates, and the MI is the enforcement point.
