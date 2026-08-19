# Pi Codebase Analysis and DeepClause Integration Plan

## Document Status

- **Status:** Draft for iteration
- **Repository:** `deepclause/deepclause-sdk`
- **Purpose:** Maintain one authoritative plan for analyzing Pi and designing, implementing, and validating a Pi integration with DeepClause.
- **Last updated:** 2026-08-19
- **Current gate:** The Pi repository, revision, and intended integration outcome must be confirmed before implementation begins.

## How to Use This Plan

This is the canonical planning document for the work. Update it as evidence is gathered rather than creating parallel plans.

- Mark completed checklist items.
- Replace assumptions with links to concrete files, symbols, tests, and upstream revisions.
- Record material choices in the decision log.
- Keep implementation status, validation evidence, and unresolved questions current.
- Do not begin an implementation phase until its entry criteria are satisfied.

## Objective

Produce a maintainable integration between DeepClause and Pi that preserves each system's useful boundaries:

- DeepClause remains responsible for DML execution, Prolog control flow, model-slot selection, local skill orchestration, and execution events.
- Pi contributes only the explicitly selected agent capabilities established by the analysis phase.
- Existing CLI, TUI, SDK, and DML behavior remains backward compatible unless a separately approved change says otherwise.
- The integration has a narrow public contract, deterministic lifecycle management, cancellation support, useful observability, and focused tests.

## Scope

### In Scope

- A source-level analysis of the selected Pi repository and revision.
- A source-level analysis of relevant DeepClause execution paths.
- Selection of the smallest useful integration boundary.
- An adapter that prevents Pi-specific details from leaking across DeepClause.
- Configuration, lifecycle, event, error, cancellation, and usage-accounting behavior required by the chosen boundary.
- Unit, integration, CLI, and regression coverage appropriate to externally visible behavior.
- User and maintainer documentation.

### Out of Scope Until Explicitly Approved

- Replacing the Prolog/DML runtime.
- Rewriting the DeepClause conductor, compiler, or TUI wholesale.
- Mirroring Pi's complete API.
- Supporting multiple Pi revisions at once.
- Adopting Pi internals that are not part of a stable public contract.
- Broad dependency upgrades unrelated to the integration.
- Changing default model behavior for existing users.

## Known Facts About DeepClause

The following findings are confirmed from the current repository.

### Product Surfaces

- `src/sdk.ts` and `src/index.ts` expose the TypeScript SDK.
- `src/cli/index.ts`, `src/cli/commands.ts`, and `src/cli/run.ts` provide command-line entry points.
- `src/cli/tui.ts` and `src/cli/tui/index.ts` implement the interactive terminal interface.
- `src/system/runtime/conductor.ts` runs the built-in conductor and persists session-facing results.
- `src/system/assets/skills/conductor.dml` defines the conductor's DML behavior and available high-level tools.
- `src/system/assets/skills/plan.dml` and `deep-planner.dml` generate executable plans.

### Execution Architecture

- `src/system/runtime/dml-executor.ts` is the central runtime composition point for a DML execution.
- It resolves model configuration, creates a shell manager, constructs the SDK, registers runtime tools, streams DML events, records usage, and disposes resources.
- Nested local skills reuse `executeDml` with inherited execution settings and invocation metadata.
- `src/agent.ts` implements the LLM tool loop used by `task/N`, including tool calls, typed output variables, retry behavior, recovery from stalled iterations, message handling, cancellation, and streaming.
- `src/sdk.ts`, `src/runner.ts`, `src/prolog/bridge.ts`, and `src/prolog/loader.ts` connect TypeScript execution to the Prolog runtime.

### Extension Points

- `ExecuteDmlOptions.registerAdditionalTools` can register execution-specific tools without changing the built-in catalog.
- `src/system/runtime/runtime-tools.ts` owns the standard local runtime tool registration.
- `src/system/runtime/catalog-skills.ts` owns local child-skill discovery and invocation.
- `src/system/runtime/shell-manager.ts` owns shell execution and isolation behavior.
- `src/types.ts` defines shared SDK events, messages, tools, and execution contracts.
- `src/cli/config.ts` and `src/system/config/model-slots.ts` resolve persisted configuration and model slots.

### Existing Quality Gates

- `npm run build` compiles TypeScript and copies Prolog and system assets.
- `npm test` runs the Vitest suite.
- `npm run lint` runs ESLint on TypeScript sources.
- Tests are organized by runtime surface, including agent, runner, conductor, shell, cancellation, events, configuration, catalogs, CLI, and TUI behavior.

## Pi Context Required

No Pi source, package dependency, git submodule, vendored snapshot, or prior Pi plan is present in this checkout. The following inputs are therefore mandatory before claiming a Pi codebase analysis is complete:

- [ ] Canonical Pi repository URL or local source path.
- [ ] Exact commit, tag, or version to analyze.
- [ ] License and redistribution constraints.
- [ ] Intended outcome: embedded agent runtime, delegated child agent, tool-loop replacement, CLI bridge, interoperability layer, or another explicit goal.
- [ ] Required Pi features and explicitly excluded features.
- [ ] Supported Node.js/runtime/platform matrix.
- [ ] Whether Pi may be a production dependency, optional peer dependency, subprocess, or vendored component.
- [ ] Compatibility and migration expectations for existing DeepClause users.

## Working Integration Hypothesis

Until the intended outcome is confirmed, the safest hypothesis is:

> Add Pi as an optional delegated coding-agent backend behind a small DeepClause-owned interface, invoked through the existing runtime-tool and DML execution machinery, without replacing DML, the conductor, or the default `task/N` loop.

This hypothesis is provisional. Phase 1 must compare it against alternatives using source evidence.

## Architecture Principles

1. **DeepClause owns the boundary.** Public DeepClause APIs must use DeepClause types rather than expose Pi implementation types.
2. **Optional means optional.** Existing installs and execution paths must work when Pi is absent or disabled.
3. **One lifecycle owner.** A single adapter owns Pi initialization, execution, cancellation, event translation, and disposal.
4. **One-way event translation.** Pi events are normalized into existing DeepClause event concepts where possible; new event types require a demonstrated need.
5. **Configuration is explicit.** No hidden environment-variable discovery or fallback may silently alter existing behavior.
6. **Cancellation is end-to-end.** Abort signals must stop model work, tools, subprocesses, and streams without leaking resources.
7. **Errors retain cause and phase.** Callers must be able to distinguish configuration, initialization, model, tool, cancellation, and cleanup failures.
8. **Usage is not double counted.** Token and cost accounting must have one authoritative source per invocation.
9. **Security follows the stricter system.** Tool permissions, filesystem boundaries, network policy, and secret handling must not be weakened by the adapter.
10. **Test public behavior.** Avoid tests coupled to Pi internals unless necessary to protect a known compatibility assumption.

## Phase 0: Confirm Inputs and Freeze the Analysis Target

### Tasks

- [ ] Record the Pi repository and exact revision in this document.
- [ ] Record the integration objective in one sentence.
- [ ] Define the minimum user journey that proves the integration is useful.
- [ ] Define non-goals and compatibility constraints.
- [ ] Confirm dependency and license acceptability.
- [ ] Create a comparison baseline using current DeepClause behavior.

### Deliverables

- Confirmed source coordinates and revision.
- Approved integration objective and non-goals.
- Minimal end-to-end acceptance scenario.
- Baseline command, output, events, and usage behavior.

### Exit Criteria

- Every item in “Pi Context Required” is resolved or explicitly deferred by an owner.
- The working hypothesis is accepted or replaced.

## Phase 1: Deep Analysis of Pi

### 1.1 Repository Topology

- [ ] Map packages, entry points, generated outputs, examples, tests, and build tooling.
- [ ] Identify which packages are public, internal, experimental, or application-only.
- [ ] Trace package dependencies and identify the minimum importable unit.
- [ ] Record module format, runtime assumptions, supported Node.js versions, and browser dependencies.
- [ ] Identify install-time and runtime side effects.

### 1.2 Agent Runtime

- [ ] Trace the primary agent construction and execution call graph.
- [ ] Document conversation state ownership and message normalization.
- [ ] Document the tool definition, invocation, result, and retry lifecycle.
- [ ] Identify loop termination, step limits, stall handling, and failure semantics.
- [ ] Document streaming and backpressure behavior.
- [ ] Document cancellation propagation and cleanup guarantees.
- [ ] Identify extension hooks intended for third-party integrations.

### 1.3 Models and Providers

- [ ] Map model/provider abstractions and configuration resolution.
- [ ] Identify how API keys, base URLs, temperatures, reasoning controls, and token limits are represented.
- [ ] Determine whether model instances can be supplied externally.
- [ ] Document provider-specific message or tool transformations.
- [ ] Document usage and cost reporting.

### 1.4 Tools and Execution Environment

- [ ] Map built-in tools and their permission model.
- [ ] Trace filesystem, shell, process, and network boundaries.
- [ ] Identify workspace assumptions and path-validation behavior.
- [ ] Document user-input and approval flows.
- [ ] Identify concurrency constraints and tool-result size limits.

### 1.5 Sessions, Persistence, and UI Coupling

- [ ] Determine whether the agent runtime is separable from Pi's CLI/TUI.
- [ ] Map session serialization and restoration.
- [ ] Identify event contracts used by rendering layers.
- [ ] Separate reusable runtime features from presentation-only behavior.

### 1.6 Quality, Stability, and Legal Review

- [ ] Locate tests covering public agent APIs and important failure paths.
- [ ] Review release history and breaking-change policy.
- [ ] Identify internal imports that must not be relied upon.
- [ ] Review license, notices, bundled assets, and transitive dependency concerns.
- [ ] Record security-sensitive findings relevant to embedding.

### Phase 1 Deliverable

Add a concise evidence table here:

| Concern | Pi evidence | DeepClause counterpart | Compatibility implication |
|---|---|---|---|
| Entry point | Pending | `src/system/runtime/dml-executor.ts` | Pending |
| Agent loop | Pending | `src/agent.ts` | Pending |
| Tool contract | Pending | `src/types.ts`, `src/tools.ts` | Pending |
| Events | Pending | `DMLEvent` in `src/types.ts` | Pending |
| Cancellation | Pending | `AbortSignal` execution options | Pending |
| Model config | Pending | model slots and CLI config | Pending |
| Persistence | Pending | conductor sessions | Pending |
| Isolation | Pending | shell and AgentVM managers | Pending |

### Exit Criteria

- Pi's relevant call graph is documented with file and symbol references.
- All adapter assumptions are tied to source or tests.
- Unstable/private Pi APIs are excluded or explicitly accepted as risks.

## Phase 2: Select the Integration Boundary

Evaluate the following options rather than committing to the first technically possible approach.

### Option A: Delegated Runtime Tool — Preferred Starting Point

Expose a DeepClause runtime tool that invokes a Pi-backed coding task through an adapter.

- Preserves DML and conductor control flow.
- Keeps Pi optional and task-scoped.
- Reuses runtime tool registration, events, workspace policy, and cancellation.
- Risks duplicate agent abstractions and requires careful usage accounting.

### Option B: Pluggable `task/N` Agent Backend

Allow selected DML tasks to use Pi's agent loop instead of `src/agent.ts`.

- Offers deeper integration and consistent DML syntax.
- Risks semantic differences in typed variables, backtracking, memory, retries, and tool exposure.
- Requires a formal backend contract before implementation.

### Option C: External Process Bridge

Invoke Pi through a subprocess or CLI boundary.

- Strong isolation and loose package coupling.
- Adds process lifecycle, serialization, streaming, startup, and packaging complexity.
- Appropriate only if Pi lacks a stable embeddable API.

### Option D: Replace the DeepClause Agent Loop

Replace `src/agent.ts` globally.

- Not recommended initially.
- Highest compatibility risk and broadest regression surface.
- Consider only after a delegated integration proves clear value and semantic parity.

### Decision Tasks

- [ ] Score each option for capability fit, coupling, compatibility, security, observability, testability, and maintenance.
- [ ] Prototype only uncertain contract edges; do not build full parallel implementations.
- [ ] Select one boundary and record the rationale in the decision log.
- [ ] Define the public and internal contracts before changing production behavior.

### Exit Criteria

- One integration option is approved.
- Its API, lifecycle, error, cancellation, and event contracts are written down.

## Phase 3: Detailed Implementation Plan

The following steps assume Option A. Revise them if Phase 2 selects another boundary.

### Step 1: Introduce DeepClause-Owned Adapter Contracts

- [ ] Add a small internal interface for agent request, result, event callback, cancellation, and disposal.
- [ ] Represent workspace, prompt, model selection, allowed tools, and invocation metadata without importing Pi types into shared public modules.
- [ ] Define normalized error phases and cancellation behavior.
- [ ] Add contract-level tests using a fake adapter.

Expected DeepClause surfaces:

- New focused module under `src/system/runtime/`.
- Shared types in the new module unless they must be public.
- Focused tests under `tests/`.

### Step 2: Resolve Optional Pi Availability

- [ ] Choose the dependency mechanism based on Phase 1 packaging evidence.
- [ ] Ensure ordinary DeepClause startup never imports Pi eagerly.
- [ ] Return an actionable configuration error when Pi is selected but unavailable.
- [ ] Validate compatible Pi versions without accepting unknown breaking versions silently.
- [ ] Add tests for absent, supported, and unsupported Pi installations.

### Step 3: Implement the Pi Adapter

- [ ] Map DeepClause request fields into Pi's stable public API.
- [ ] Supply the workspace and model configuration explicitly.
- [ ] Restrict Pi tools to the approved capability set.
- [ ] Translate Pi lifecycle and stream events into the adapter contract.
- [ ] Propagate the caller's abort signal.
- [ ] Normalize results, usage, and errors.
- [ ] Guarantee cleanup in success, failure, and cancellation paths.
- [ ] Keep Pi imports contained within the adapter.

### Step 4: Register a Runtime Tool

- [ ] Add a narrowly named runtime tool through `registerLocalRuntimeTools` or an adjacent optional registrar.
- [ ] Define a strict argument schema with bounded prompt and result handling.
- [ ] Pass the existing workspace, shell/network policy, signal, and event callback.
- [ ] Avoid granting capabilities beyond those available to the parent DML execution.
- [ ] Return structured results suitable for DML rather than raw Pi objects.

### Step 5: Add Configuration

- [ ] Add an explicit enablement and backend selection setting.
- [ ] Reuse resolved model slots where semantic mapping is safe.
- [ ] Add only Pi-specific settings that cannot be derived from existing configuration.
- [ ] Validate configuration early and provide precise diagnostics.
- [ ] Preserve existing defaults when the setting is absent.
- [ ] Cover config-file, CLI, and environment precedence if all are supported.

### Step 6: Integrate Events and Usage

- [ ] Map child activity to existing `DMLEvent` variants whenever semantics match.
- [ ] Add a new event variant only if consumers require information that cannot be represented safely.
- [ ] Include invocation identity so TUI and headless consumers can group child activity.
- [ ] Record model usage through the existing token-usage path exactly once.
- [ ] Preserve event ordering and completion/error terminal semantics.
- [ ] Add event-sequence tests for success, failure, cancellation, and streamed output.

### Step 7: Expose the Capability to DML

- [ ] Decide whether the tool is conductor-only, system-skill-only, or generally available.
- [ ] Update the relevant DML tool declarations and descriptions.
- [ ] Keep delegation explicit in prompts; do not silently redirect existing tasks.
- [ ] Add one focused system skill or recipe only if it improves discoverability without duplicating orchestration logic.
- [ ] Validate generated/copied assets in `src/system/assets/index.ts` and build output.

### Step 8: CLI and TUI Behavior

- [ ] Show concise start, progress, completion, cancellation, and failure states.
- [ ] Ensure `/cancel` reaches the Pi invocation.
- [ ] Prevent verbose child output from overwhelming the execution pane or session transcript.
- [ ] Preserve headless machine-readable behavior.
- [ ] Add CLI/TUI regression tests only for behavior changed by the integration.

### Step 9: Documentation

- [ ] Document installation and optional dependency requirements.
- [ ] Document configuration, supported Pi version, and model mapping.
- [ ] Add one minimal delegated-task example.
- [ ] Document security boundaries and unsupported features.
- [ ] Add troubleshooting for missing dependency, configuration, cancellation, and tool-permission failures.
- [ ] Update this plan with final file and symbol references.

## Phase 4: Validation Plan

### Static and Local Validation

- [ ] Run focused tests after each implementation slice.
- [ ] Run `npm run lint`.
- [ ] Run `npm run build`.
- [ ] Run `npm test`.
- [ ] Verify package contents do not accidentally bundle prohibited Pi files or omit required adapter assets.

### Behavioral Scenarios

- [ ] Pi disabled: all existing SDK, CLI, TUI, conductor, and DML behavior remains unchanged.
- [ ] Pi selected but absent: fail early with an actionable message.
- [ ] Successful delegated task: files, response, events, and usage are correct.
- [ ] Tool failure: error cause and phase are preserved.
- [ ] Model failure: no speculative diagnosis and no leaked resources.
- [ ] Cancellation during model generation: execution terminates promptly.
- [ ] Cancellation during a tool call: child work and process resources terminate.
- [ ] Nested skill invocation: invocation metadata and event grouping remain correct.
- [ ] Concurrent invocations: state, output, cancellation, and usage do not cross-contaminate.
- [ ] Large output: context and event storage remain bounded.
- [ ] Restricted shell/network mode: Pi cannot bypass parent policy.
- [ ] Session persistence: only intended user-facing and execution data is retained.

### Security Validation

- [ ] Review new dependency advisories before adding Pi packages.
- [ ] Scan changed files for secrets before every commit.
- [ ] Verify prompt and tool inputs cannot escape the workspace or capability policy.
- [ ] Verify credentials are passed only to the selected provider.
- [ ] Verify logs and events redact secrets and sensitive headers.
- [ ] Run code review and CodeQL checks after changes are committed.

### Acceptance Criteria

- The agreed end-to-end user journey succeeds.
- Existing behavior passes regression tests with Pi disabled.
- Cancellation and cleanup are proven by tests.
- Tool permissions are no broader than the parent execution.
- Usage is correct and not duplicated.
- Pi remains behind a DeepClause-owned adapter.
- Documentation identifies the supported Pi revision and limitations.

## Phase 5: Rollout and Maintenance

- [ ] Release behind explicit opt-in.
- [ ] Mark the integration experimental until compatibility is demonstrated across agreed scenarios.
- [ ] Capture the supported Pi version range.
- [ ] Add a repeatable compatibility test for future Pi upgrades.
- [ ] Define deprecation behavior for adapter contract changes.
- [ ] Review telemetry or user feedback only through existing privacy-respecting mechanisms.
- [ ] Promote from experimental only after stability, security, and maintenance criteria are met.

## Expected Change Map

This map is provisional and must be narrowed after Phases 1 and 2.

| Area | Likely action | Reason |
|---|---|---|
| `src/system/runtime/` | Add adapter and optional registrar | Contain Pi lifecycle and integration logic |
| `src/system/runtime/dml-executor.ts` | Pass existing execution context if required | Reuse workspace, signal, events, and usage |
| `src/system/runtime/runtime-tools.ts` | Register optional tool | Make delegation callable from DML |
| `src/cli/config.ts` | Add validated opt-in settings | Keep behavior explicit and backward compatible |
| `src/types.ts` | Change only if public events/config require it | Avoid unnecessary public API expansion |
| `src/system/assets/skills/` | Add or update explicit tool declaration | Make capability discoverable to selected skills |
| `src/system/assets/docs/` | Document DML-facing behavior | Keep seeded documentation aligned |
| `tests/` | Add focused adapter, runtime, cancellation, event, and config coverage | Protect the integration contract |
| `README.md` or `docs/` | Add user setup and limitations | Support adoption and troubleshooting |
| `package.json` | Add dependency only after review | Keep Pi optional and versioned deliberately |

## Risks and Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Pi target remains ambiguous | Incorrect architecture | Block implementation at Phase 0 |
| Pi exposes only unstable internals | Upgrade fragility | Use a subprocess boundary or pin an explicit revision |
| Two agent loops have conflicting semantics | Incorrect retries, memory, or completion | Keep delegation task-scoped and specify the adapter contract |
| Tool policies diverge | Security regression | Enforce DeepClause policy at the boundary and test denial paths |
| Cancellation does not cross the boundary | Resource leaks and hung sessions | Require abort-aware APIs and cleanup tests |
| Event volume overwhelms TUI/session storage | Poor usability and storage growth | Normalize, group, and bound child events |
| Usage is reported by both systems | Incorrect accounting | Choose one authoritative accounting source |
| Optional dependency loads eagerly | Existing users break | Dynamic resolution and absent-dependency tests |
| Provider configuration differs | Wrong model behavior | Explicit mapping with validation and documented unsupported fields |
| Pi license or transitive package conflicts | Release blocker | Complete legal/dependency review before adoption |

## Open Questions

1. Which Pi project and revision is the analysis target?
2. What concrete capability should Pi add to DeepClause?
3. Should Pi run in-process, in AgentVM, or as an external process?
4. Must users install Pi separately, or may it be an optional package dependency?
5. Which system owns the model provider instance and credentials?
6. Which system owns tool execution and approval?
7. Should delegated events be persisted in session history or only execution logs?
8. Is Pi invocation available to all skills or only the conductor and selected system skills?
9. What compatibility promise is required for Pi upgrades?
10. What benchmark will demonstrate that the integration is better than the current DeepClause path?

## Decision Log

| Date | Decision | Rationale | Status |
|---|---|---|---|
| 2026-08-19 | Use this file as the single iterative plan | Avoid fragmented and contradictory planning documents | Accepted |
| 2026-08-19 | Do not claim Pi analysis without source coordinates | No Pi source or dependency exists in this checkout | Accepted |
| Pending | Select Pi repository and revision | Required for reproducible analysis | Open |
| Pending | Select integration boundary | Must follow comparative Phase 2 review | Open |

## Progress Tracker

- [x] Establish one canonical plan file.
- [x] Document confirmed DeepClause architecture and extension points.
- [x] Identify missing Pi inputs without inventing source findings.
- [x] Define analysis, decision, implementation, validation, and rollout phases.
- [ ] Complete Phase 0 input confirmation.
- [ ] Complete Pi source analysis.
- [ ] Select and approve the integration boundary.
- [ ] Implement the approved minimal slice.
- [ ] Validate and document the integration.
- [ ] Complete rollout criteria.
