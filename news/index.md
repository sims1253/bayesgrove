# Changelog

## bayesgrove 0.4.3

- Added process-guidance, model-taxonomy, Stan-workflow, and
  dagitty-backed causal workflow packs that extend the review protocol
  with primary-source-backed Bayesian workflow semantics.
- Added causal selection contracts so DAG-derived required, forbidden,
  and ranked admissible terms can constrain formulas and
  projection-oriented review in causal branches.
- Added the primed-prior case study vignette and refreshed the README
  plus package vignettes so the documented workflow-pack surface matches
  the current package behavior.

## bayesgrove 0.4.2

- Added
  [`bg_execute_action()`](https://sims1253.github.io/bayesgrove/reference/bg_execute_action.md)
  and
  [`bg_extension_registry()`](https://sims1253.github.io/bayesgrove/reference/bg_extension_registry.md)
  to the pkgdown reference so the site builds cleanly in CI.
- Reworked the affected protocol and test files to satisfy `air format`
  and updated the jarl-facing assertions to avoid redundant logical
  equality checks.

## bayesgrove 0.4.1

- Added five optional built-in workflow packs:
  `bayesgrove.prior_workflow`, `bayesgrove.model_checks`,
  `bayesgrove.model_selection`, `bayesgrove.causal_minimal`, and
  `bayesgrove.pad_scaffold`.
- Extended the review-decision template and workflow registry so those
  packs can emit prior rationale, prior and posterior predictive review,
  SBC review, model-selection review, causal framing prompts, and PAD
  annotation prompts.
- Added an `extensions` vignette covering node sets, backend plugins,
  domain modules, summary emission, and the current status of
  workflow-pack extension.
- Refreshed the README, getting-started, guided-review-loop,
  simulation-study, and demo documentation so the optional workflow-pack
  surface matches the current code.

## bayesgrove 0.4.0

- Added a richer, dashboard-style TUI workflow to
  [`bg_repl()`](https://sims1253.github.io/bayesgrove/reference/bg_repl.md)
  featuring a new `dashboard` command and a default initialization view
  showing workflow health, obligations, and pending gates.

- Added a [`next`](https://rdrr.io/r/base/Control.html) command to the
  guided client that automatically previews and prompts to execute the
  top recommended workflow action without manual ID lookup.

- Improved post-action clarity and safety inside the REPL by rendering
  detailed action previews before execution and adding inline
  workflow-branch scope navigation.

- Added `lineage` and `export` REPL commands so users can view
  structural branch history and generate full workflow reports directly
  from the interactive operator console.

- Extended interactive REPL tests to cover new guided-client rendering
  helpers, preserving decoupled CLI state modeling for future GUI
  consumption.

- Added an experimental
  [`bg_serve()`](https://sims1253.github.io/bayesgrove/reference/bg_serve.md)
  websocket IPC layer with versioned `GraphSnapshot`, `ProtocolEvent`,
  `Command`, and `CommandResult` schemas under `inst/protocol/`.

- Marked the bounded remote command/query surface directly in
  [`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md)
  via `remote_accessible`, and added validation that keeps the server
  dispatch registry aligned with that API boundary.

- Added protocol-schema coverage for the new IPC messages plus websocket
  integration tests that exercise connect, command dispatch, reconnect,
  and async responsiveness when local sockets are available.

- Simplified async execution support to rely on `mirai` only, removing
  the parallel `callr` backend branch from submission, cancellation,
  reconciliation, package metadata, and tests.

## bayesgrove 0.3.13

- Added
  [`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md)
  plus a machine-readable registry covering every exported `bg_*`
  function with explicit `stable`, `experimental`, or
  `internal_exported` classifications and `remote_accessible` flags.
- Added regression tests that enforce exact set equality between the API
  boundary registry and the exported namespace so the public surface
  cannot drift silently.
- Added `inst/protocol/` schema metadata for `bg_next_actions_result`,
  `bg_obligation_item`, `bg_action_item`, `bg_partitioned_result`, and
  `bg_template_descriptor`, then tightened the validation helpers to
  check nested fields, object maps, and local schema references.
- Aligned the live workflow-protocol return shapes with the checked-in
  schema so empty protocol maps serialize as JSON objects instead of
  arrays, and surfaced
  [`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md)
  in the pkgdown reference.

## bayesgrove 0.3.11

- Moved the contributor-facing `dagriculture` boundary note out of
  `docs/` so pkgdown can clean and rebuild the GitHub Pages output
  directory in CI.
- Disabled `indentation_linter` in `.lintr` so formatting remains
  enforced by `air format` without a conflicting lint failure.

## bayesgrove 0.3.10

- Removed stale default-workflow comparison action code, moved
  parameter-suggestion helpers into the default pack layer, and
  tightened comparison/disposition identity handling around compact
  `comparison_signature` values.
- Hardened the internal `dagriculture` adapter helpers with clearer
  internal markers, deterministic edge-ID handling for graph diffs, and
  regression coverage for unnamed-edge diffs plus comparison/disposition
  staleness.
- Narrowed lint policy by disabling `object_length_linter` for the
  package’s deliberate protocol helper names while fixing the remaining
  indentation and brace warnings in tests and vignettes.

## bayesgrove 0.3.9

- Added an explicit Phase 7 architecture note at
  `architecture/dagriculture-boundary.md` plus a small internal adapter
  layer that isolates bayesgrove’s graph-generic dependency on
  `dagriculture`.
- Refactored repeated inline structural graph queries into focused
  `bg_dagri_*` helpers for edge selection, descendant lookup,
  deterministic edge ordering, state recomputation, and structural graph
  diffs without moving workflow semantics out of bayesgrove.
- Added regression coverage that distinguishes graph-generic helper
  behavior from bayesgrove-specific workflow, branch, and provenance
  logic.
- Synced the README and pkgdown-facing site sources for the phase-7
  boundary work, including a dedicated architecture page that pkgdown
  can render without a broken navbar link.

## bayesgrove 0.3.8

- Added a deterministic `guided-review-loop` vignette plus regression
  coverage for one full case-study loop covering workflow holds, branch
  lineage, criticism and comparison decisions, branch dispositions,
  stale summaries, and report export.
- Updated the README and pkgdown article navigation to surface the new
  case study alongside the lower-level workflow examples.
- Fixed the `DESCRIPTION` remote metadata so `pak` can parse the GitHub
  remotes correctly during CI lockfile creation.
- Replaced `:::` test and demo accesses with exported calls or namespace
  lookups to clear the internal-function lint warnings without changing
  behavior.

## bayesgrove 0.3.7

- Added a first-class built-in template registry plus
  [`bg_list_templates()`](https://sims1253.github.io/bayesgrove/reference/bg_list_templates.md)
  for `diagnostic_check`, `branch_comparison`, `branch_and_modify_fit`,
  and `review_decision`.
- Reworked the guided REPL and workflow action payloads to execute
  template-backed node creation, branching, and review decisions through
  the shared registry while preserving `branch_comparison`
  compatibility.
- Split the checked-in REPL demo surface into reusable checkpoints plus
  focused remediation, comparison, and disposition tapes while keeping
  the original full walkthrough.

## bayesgrove 0.3.6

- Added explicit retirement semantics with
  [`bg_retire_node()`](https://sims1253.github.io/bayesgrove/reference/bg_retire_node.md)
  and
  [`bg_retire_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_retire_branch.md)
  so stale analysis paths can be removed from planning and workflow
  guidance without changing
  [`bg_invalidate()`](https://sims1253.github.io/bayesgrove/reference/bg_invalidate.md)’s
  recompute-oriented behavior.
- Updated planning, workflow contexts, pending gates, and REPL workflow
  commands to exclude retired nodes and branches from runnable work
  while preserving cached artifacts for inspection.
- Refreshed the guided REPL demo and regression coverage so the stronger
  default workflow pack now demonstrates remediation, retirement,
  comparison, and branch acceptance without rerunning retired warning
  paths.

## bayesgrove 0.3.5

- Strengthened the built-in `bayesguide.default_bayesian` workflow pack
  with explicit fit-criticism review, comparison obligations,
  model-comparison decisions, and branch accept/reject dispositions
  keyed to current evidence.
- Added deterministic comparison/disposition basis tracking plus
  explicit hold targeting so the stronger default workflow blocks
  downstream work without blocking the comparison step needed to satisfy
  it.
- Added a guided, branch-aware REPL workflow with scope switching,
  action execution, richer graph/status rendering, and helper APIs for
  branch labels, branch listings, and scope-partitioned protocol
  results.
- Added branch-with-continuation support for guided workflow branching
  and refreshed the REPL demo, README, and reference docs to match the
  new protocol-driven workflow loop.
- Tightened workflow scope behavior so branch contexts inherit project
  summaries, comparison actions only target fit nodes, and branch-local
  graph views no longer leak cross-scope edges.

## bayesgrove 0.3.4

- Enforced workflow holds inside
  [`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md)
  and
  [`bg_submit()`](https://sims1253.github.io/bayesgrove/reference/bg_submit.md)
  so blocking obligations now affect real execution, not just advisory
  planning.
- Tightened workflow-context scoping, transactional gate answering, sync
  job logging, and severity merging to match the workflow protocol and
  persistence contracts.
- Added locking around shared artifact-index, summary, decision, job,
  gate-spec, and graph writes to harden async and multi-session
  behavior.
- Aligned public APIs and docs for status reporting, branching returns,
  bundling/export arguments, workflow reference topics, and report
  examples.

## bayesgrove 0.3.3

- Added the first end-to-end workflow-hold integration slice covering
  summary persistence, workflow context construction, obligation
  derivation, planner hold propagation, and next-action clearing after
  fit replacement.
- Updated
  [`bg_plan()`](https://sims1253.github.io/bayesgrove/reference/bg_plan.md)
  to accept caller-supplied `external_holds`, surface
  `external_blocked`, and keep externally held nodes out of `to_execute`
  without conflating them with structural blockers.
- Refreshed the README, getting-started vignette, and workflow-protocol
  reference docs to document summary-driven workflow guidance and
  planner holds.

## bayesgrove 0.3.2

- Added the first protocol-facing workflow APIs:
  [`bg_workflow_packs()`](https://sims1253.github.io/bayesgrove/reference/bg_workflow_packs.md),
  [`bg_workflow_context()`](https://sims1253.github.io/bayesgrove/reference/bg_workflow_context.md),
  and deterministic
  [`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md).
- Added workflow-pack provider dispatch plus canonical runtime-side ID
  generation and deduplication for obligations and curated actions.
- Added a minimal built-in default Bayesian pack that emits branch
  goal-setting and computation-review workflow guidance from decisions
  and summaries.
- Extended workflow context evidence with lightweight artifact
  availability and added tests for empty packs, obligations, actions,
  deduplication, and project-vs-branch scope behavior.

## bayesgrove 0.3.1

- Expanded project snapshots, handoff bundles, and workflow reports to
  include decisions, artifacts, richer pause/resume state, and gate
  context.
- Added a simulation-study vignette and pkgdown article navigation for
  the workflow example.

## bayesgrove 0.3.0

- Added persisted workflow context plumbing for branch registries, goal
  registries, and summary logs.
- Added branch-aware scope resolution plus
  [`bg_build_workflow_context()`](https://sims1253.github.io/bayesgrove/reference/bg_build_workflow_context.md)
  with structural, execution, and evidence partitions.
- Added summary freshness/staleness derivation from predicted
  fingerprints and active artifact-index bindings.
- Updated branching, goal-setting, invalidation, and execution paths to
  persist workflow summaries and registries without reopening heavy
  artifacts.
- Simplified async workers to always load the installed package,
  removing source-tree autodetection from background execution.

## bayesgrove 0.2.0

- **Phase 2 (Async Execution Layer) Implementation**
- Added
  [`bg_submit()`](https://sims1253.github.io/bayesgrove/reference/bg_submit.md),
  [`bg_wait()`](https://sims1253.github.io/bayesgrove/reference/bg_wait.md),
  and
  [`bg_cancel()`](https://sims1253.github.io/bayesgrove/reference/bg_cancel.md)
  to orchestrate true non-blocking background execution of workflows.
- Integrated `callr` and `mirai` execution backends for parallel
  asynchronous job dispatch.
- Added
  [`bg_jobs()`](https://sims1253.github.io/bayesgrove/reference/bg_jobs.md)
  and JSONL logging (`.bayesgrove/runs/jobs.jsonl`) to durably track job
  status, start times, completion, and task progress.
- Updated
  [`bg_status()`](https://sims1253.github.io/bayesgrove/reference/bg_status.md)
  to double as a reconciliation poller
  ([`bg_reconcile_daemon_jobs()`](https://sims1253.github.io/bayesgrove/reference/bg_reconcile_daemon_jobs.md)),
  updating crashed or finished daemon jobs automatically without
  blocking the main session.
- Ensured isolated failure containment: if a single background task
  fails, other independent node jobs continue executing.
- Exported internal artifact/caching methods conditionally to support
  the standalone worker contract across process boundaries.

## bayesgrove 0.1.1

- **Greenfield Architecture Rewrite**: The `bayesguide` prototype has
  been renamed and completely rebuilt as `bayesgrove`.
- **Graph Engine Split**: The pure graph execution logic has been moved
  to a separate, foundational package called `dagriculture`
  (`sims1253/dagriculture`), ensuring strict separation of computation
  from Bayesian semantics.
- **S7 Object Model**: Core workflow states are now backed by explicit
  reference-semantic `S7` classes (like `bg_handle`).
- **Decision Provenance Layer**: Decisions, alternative choices, and
  rationales are now explicitly captured through semantic “gates”
  layered over the topological graph.
- **Deterministic Fingerprinting & Caching**: Cache keys are now built
  from upstream fingerprints and structured backend signatures, creating
  a robust local Content-Addressed Storage (CAS) mechanism.
- **Async & Plugin Foundations**: Synchronous runtime execution
  ([`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md))
  and backend registry
  ([`bg_register_backend()`](https://sims1253.github.io/bayesgrove/reference/bg_register_backend.md))
  are now fully operational, including the initial `cmdstanr` MVP
  plugin.
