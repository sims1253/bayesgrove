# bayesgrove (development version)

## New features

* Added an experimental research-search interface: `bg_research_init()`,
  `bg_research_state()`, `bg_research_propose()`, and `bg_research_apply()`.
  It records P/A/D candidate lineages, evidence, cross-lineage comparisons,
  decisions, and policy changes. Record, Guide, and Enforce modes share the
  same research state. Snapshots, bundles, and reports include that state.

## Breaking changes

* Removed `bg_branch_with_continuation()`. Use `bg_branch(continue = )`.
* Removed the ignored `auto_advance` argument from `bg_status()`.
* Removed `bg_export_report(out_file = )`; use `path = ` instead.
* Removed the ineffective `include_data` argument from `bg_bundle()`. Bundles
  include attached data but do not copy external files referenced by node params.
* JSON storage preserves double precision. Fingerprint format 3 invalidates
  previous cache keys, so the first run after upgrading recomputes results.

## Bug fixes

* Bundles retain data attached with `bg_set_node_data()`, even before a node runs.
* Relative bundle paths resolve from the caller's working directory, not the
  temporary staging directory.

# bayesgrove 0.7.0

## Breaking changes

* Removed the `data_fn_source` and `check_fn_source` node params, which
  evaluated R source text at run time and so reopened a params-as-code
  channel. Attach data with the new `bg_set_node_data()`; prior-predictive
  checking with a custom function now uses the trusted-executor pattern
  (register a node kind wrapping `prior_fit` via `bg_register_node_kind()` or
  `bg_restore_executors(trust = TRUE)`).

## Bug fixes

* `bg_cmdstanr_hmc_metrics()` now reads `num_divergent` / `num_max_treedepth`
  from `diagnostic_summary()` (was `divergent` / `max_treedepth`, which
  silently always read zero divergences).
* E-BFMI handling no longer collapses to `Inf` when a single chain is `NA`;
  finite chains are filtered before taking the minimum (both cmdstanr and
  brms backends).
* `bg_executor_ppc()` computes the posterior-predictive p-value on the
  per-draw margin (`apply(yrep, 1, ...)`), not the per-observation margin.
  PPC statistics are restricted to an allowlist (`mean`, `sd`, `median`,
  `min`, `max`, `mad`).
* brms `bg_brms_hmc_metrics()` computes real per-chain E-BFMI from the
  `energy__` sampler parameter (was the raw energy minimum, never comparable
  to the threshold); treedepth hits compare against the configured
  `max_treedepth` rather than a hardcoded 10.
* brms executors build the `brm()` call via `do.call` with NULL args dropped,
  so a missing `seed` no longer passes `seed = NULL` (brms' default is `NA`).
* The fingerprint now hashes `stan_file` CONTENTS, not the path, so editing a
  Stan program invalidates the cache at a fixed path. The dataless
  `brms::stancode()` source-hash branch has been removed.
* Mid-run summaries are annotated `is_fresh = TRUE`, so freshness-strict
  workflow packs (fit criticism) see evidence emitted within the same
  `bg_run()` call and hold downstream nodes correctly.
* `bg_executor_compare()` reports the runner-up `elpd_diff` and a plain-data
  comparison table (was the always-zero best-model value); stacking weights
  use `loo::loo_model_weights(method = "stacking")` and surface errors in
  summary metadata instead of being silently dropped.
* `bg_executor_loo()` computes `r_eff` via `loo::relative_eff()` from
  chain-shaped draws, eliminating the missing-`r_eff` warning.
* The built-in `sbc` executor no longer evaluates persisted `data_fn` source.
  Simulation cases are plain data supplied by an upstream generator executor,
  so project code remains behind `bg_restore_executors(trust = TRUE)`.
* LOO-PIT now uses weighted `posterior::pit()` (including reproducible
  randomized PIT for discrete outcomes) and the dependence-aware PIET
  uniformity test. SBC ranks are bounded by bulk ESS and are graded only when
  every chi-squared bin has at least five expected observations.
* Graph tree output preserves child indentation, and the README's eight-schools
  example now uses Stan programs shipped with the package.
* Parallel dispatch validates minimum versions of mirai, purrr, and carrier
  before starting. Parallel fit waves warn when daemon-local sampler CSV files
  may not be durable.

## New features

* `bg_use_default_workflow()`, `bg_use_workflow_packs()`, and
  `bg_next_actions()` are reclassified as stable in `bg_api_boundary()`.
* `bg_read_decisions()` is exported: the read-only accessor for the
  decision log, completing the reader family alongside `bg_read_summaries()`
  and the registry readers.
* Workflow packs recognize fit nodes by the `*_fit` naming convention
  (`bg_pack_is_fit_node()`), so `cmdstanr_fit` and `brms_fit` nodes
  participate in comparison candidacy and criticism review; previously only
  the literal kind `fit` did, which meant the comparison loop never fired
  for real backends. Prior-predictive fit kinds are excluded from
  candidacy. LOO diagnostic summaries are reviewed as computation evidence
  rather than fit criticism.
* The `ppc` node kind supports `prop_zero` (proportion of zeros) as a test
  statistic, the standard posterior-predictive check for zero-heavy count
  data. Statistics remain an allowlist resolved to internal functions;
  params stay data, never code.
* Registering brms support via `bg_use_brms()` now also provides a neutral
  `data` node kind (an alias of the `stan_data` executor), and
  `bg_fit_brms()` labels its data node with it.
* Parallel execution: `bg_run()` gains a `parallel` argument
  (`"auto"`/`"never"`/`"always"`). Execution proceeds in topological waves;
  with active `mirai::daemons()` a wave of independent nodes is dispatched
  concurrently via `purrr::in_parallel()`. Workers write content-addressed
  blobs only; the main process remains the single writer of the artifact
  index, summaries, and jobs. Protocol holds and `bg_pause()` apply at wave
  boundaries. `mirai`, `carrier`, and `purrr` are soft dependencies; the
  sequential path works without them.
* New built-in node kinds `loo_pit` (PSIS-LOO PIT calibration graded by a
  dependence-aware uniformity test) and `sbc` (simulation-based
  calibration graded by a chi-squared rank-uniformity test), closing the gap
  between what the workflow packs ask for and what the shipped executors can
  produce. The `ppc` executor now stores plot-ready data (observed `y` plus a
  capped `yrep` subsample) in its artifact.
* Visualization: `bg_graph_mermaid()` renders the active graph as Mermaid
  flowchart text with state coloring, and `bg_plot()` plots a node's
  diagnostics (`ppc` density overlay, LOO-PIT ECDF, SBC rank histogram, fit
  traces) via the soft `bayesplot` dependency. `bg_export_report()` embeds
  the Mermaid graph in both output formats.
* Practitioner shortcuts `bg_fit_stan()` and `bg_fit_brms()` collapse the
  data-node + fit-node + run sequence into one call (experimental).
* `bg_bundle()` records a reproducibility manifest (R version, platform,
  package versions, CmdStan version) in the bundle; `bg_open()` warns when a
  restored bundle was produced in a different environment.
* `bg_next_actions()` results and blocked `bg_run()` handles print as
  protocol-aware checklists: held nodes show their hold reason, obligations
  render with severity glyphs and a copy-pasteable resolving call.
* New `?bayesgrove-concepts` help topic and `vignette("concepts")`: the
  node -> summary -> obligation -> decision -> hold mental model in one
  place.
* `bg_branch()` gains a `continue` argument and absorbs
  `bg_branch_with_continuation()`: `continue = TRUE` clones all immediate
  children onto the branch, a character vector clones only children of those
  kinds, and the returned record always carries `$continuation_nodes`.
  `bg_branch_with_continuation()` is deprecated (warns once per session,
  keeps its legacy return shape, classified `deprecated` in
  `bg_api_boundary()`) and will be removed in a future release.
* `bg_update_node()` now merges `params` into the existing set via
  `utils::modifyList()` instead of replacing the whole list; pass
  `replace = TRUE` for intentional wholesale replacement.
* Added `inst/CITATION` (package + Gelman et al. 2020, "Bayesian Workflow").
* `bg_set_node_data()` attaches an R data object to a node via the
  content-addressed store, preserving types that JSON serialization would
  mangle (e.g. Stan integers).
* Project-level `workflow_strictness` config key: packs without an explicit
  per-ref `strictness` inherit the project default (e.g. `"advisory"`).

## Performance

* The jobs log is cached per handle and invalidated by file mtime+size, so a
  run of N nodes does O(N) full parses of `jobs.jsonl` instead of O(N^2);
  appends reuse the cached line count for seq stamping.
* `bg_status()` reuses its plan seed via an internal plan refresh instead of
  computing the full plan twice, and protocol hold evaluation threads the
  already-loaded graph through instead of re-reading it from disk once per
  executed node.

## Additional bug fixes

* The divergence rate uses the total post-warmup transitions across all
  chains as its denominator (it was inflated by the chain count), and the
  ESS warning threshold scales as 100 per chain (Vehtari et al. 2021) when
  the chain count is known.
* Draw-variable selection matches Stan's indexed-variable form exactly
  (`log_lik` matches `log_lik[1]` but no longer `log_lik_saturated[1]`).

## Documentation

* New flagship case study `vignette("case-study-roaches")`: the full review
  loop on the Gelman-and-Hill roaches trial, from a cleanly sampling but
  badly misfitting Poisson model through an enforced criticism, branch,
  comparison, and disposition cycle to an exported report. The dataset and
  all three Stan programs ship with the package.
* `vignette("getting-started")` was rewritten around a real cmdstanr
  lifecycle: one small fit, a failing posterior-predictive check, the
  obligation it raises, and the branch that repairs it — with captured
  output. Mock-executor material moved to `vignette("extensions")`.
* Removed the `guided-review-loop` vignette (superseded by the reworked
  getting-started and the case study) and the `primed-priors-case-studies`
  vignette (a workflow mapping, superseded by the real case study). The
  `dagriculture-boundary` page was folded into `vignette("concepts")`.
* PPC documentation now states explicitly that posterior-predictive p-values
  are conservative tripwires and that graphical checks via `bg_plot()` are
  the primary posterior-predictive diagnostic.

# bayesgrove 0.6.0

## Breaking changes

* Removed the IPC server (`bg_serve()`), async execution (`bg_submit()`,
  `bg_wait()`, `bg_cancel()`, mirai dispatch), and the backend-plugin
  machinery (`bg_register_backend()`, `bg_cmdstanr_plugin()`, etc.).
  Execution is now synchronous and in-process. The job log is retained.
* `bg_run()` and `bg_plan()` no longer accept `mode` or `backend` arguments.
* `bg_open()` now requires `force = TRUE` to steal a locked project.
* Pack ids renamed from `bayesguide.*` to `bayesgrove.*` (deprecation alias
  warns and redirects for one release).
* Summary records are stamped `schema_version = 2`; fingerprint format bumped
  to `"2"`, invalidating all 0.x caches.

## New features

* Built-in cmdstanr/brms executors (`bg_use_cmdstanr()`, `bg_use_brms()`)
  that compute HMC, LOO, and PPC diagnostics themselves. Severity rules
  (`bg_hmc_severity()`) follow Vehtari et al. (2021) with overridable
  thresholds.
* Summary-kind vocabulary (`bg_summary_vocabulary()`,
  `bg_register_summary_kind()`): executors emitting an unknown summary kind
  get a warning with a typo suggestion; malformed summaries abort at write
  time.
* Pack composition by `includes`: `bayesgrove.stan_workflow` now includes its
  constituent packs instead of duplicating providers. Cycle detection and
  dedup ensure each provider runs at most once.
* Advisory mode: packs with `config = list(strictness = "advisory")` produce
  obligations that surface in the REPL but never create blocking holds.
* Real single-writer project lock (directory-based with pid/host reporting,
  `force` takeover, GC finalizer, readonly exempt).
* Opening a project never runs project-supplied code; `bg_restore_executors()`
  is the explicit trust gate for persisted executor source.
* Per-run state object caches decisions/summaries so the external-holds
  check between nodes is O(1) parses per run, not O(N).

## Improvements

* Fingerprint now includes the executor body (user executors) or executor_ref
  + package version (built-ins) and the environment manifest (R, bayesgrove,
  cmdstanr, brms versions).
* JSONL records carry a monotonic `seq` field; latest-record selection uses
  `seq` with `created_at` tiebreak.
* Artifact store temp files are written inside the CAS directory (no
  cross-device rename failures); cache-hit leak fixed.
* cmdstanr fit CSV output files are copied into `.bayesgrove/runs/<job_id>/`
  as a durable record alongside the cached RDS artifact.
* `bg_status()` uses a one-shot state cache, avoiding redundant JSONL parses.

# bayesgrove 0.5.1

* Fixed CodeRabbit review issues: wired unused `choice` param into `bg_record_decision_from_action` metadata, stopped `bg_phase10_source_node_id` from widening caller-provided `node_ids`, guarded NULL `goal_kind` before `switch()` in `bg_phase10_taxonomy_evaluation_modes`, expanded character vectors to individual specs in `bg_phase10_causal_allowed_formulas`, and selected most recently created contract in `bg_phase10_current_causal_contract`.

# bayesgrove 0.5.0

* Split 5 large production files into focused helper modules to improve file health:
  `27a-workflow-packs-bayesian-helpers.R`, `22a-workflow-packs-default-helpers.R`,
  `19a-protocol-helpers.R`, `31a-workflow-packs-causal-dagitty-helpers.R`,
  `29a-workflow-packs-guides-helpers.R`.
* Extracted `bg_record_decision_from_action()` helper to deduplicate the common
  `bg_build_decision_metadata` + `bg_record_decision` pattern in REPL and protocol layers.

# bayesgrove 0.4.9

* `bg_execute_node()` now transitions job state to "failed" when no executor is registered for a node kind, preventing stuck "running" jobs.
* `bg_repl_choose_action()` defensively coerces `idx` to integer to produce friendly CLI errors on non-numeric input.
* `bg_close()` now guards `mirai::daemons()` with `requireNamespace("mirai")` for consistency with the rest of the codebase.
* Gate answer rollback now restores only the specific gate being modified, avoiding clobbering concurrent updates to other gates.
* Removed trailing space in `bg_read_graph()` error message.
* Updated closed/readonly error-case tests to register a valid node kind and assert on specific error messages.

# bayesgrove 0.4.8

* Fixed CI failures across R-CMD-check, pkgdown, test-coverage, and format-check workflows.
* Renamed `bg_add_gate()` parameter `options` to `alternatives` consistently across all call sites (vignettes, tests, demo scripts).
* Fixed `bg_bundle()` error on all platforms: replaced unsupported `chdir` argument to `utils::tar()` with `setwd()`/`on.exit()` pattern.
* Qualified all `hedgehog::` namespace references in property-based tests to avoid unbound symbol errors.
* `bg_read_graph()` now aborts with a clear error when `graph.json` is missing instead of silently returning an empty graph.
* Added backward-compatible `auto_advance` parameter to `bg_status()` with a deprecation warning.
* Aligned API boundary registry with current exports: added `bg_execute_node` and `bg_worker_log`, removed `bg_submit` and `bg_worker_process`.
* Replaced `bg_submit` with `bg_run` in the IPC server registry and protocol schema.
* `bg_write_gate_specs()` now uses atomic writes for the empty-specs case.
* `bg_worker_log()` now creates the `.bayesgrove/runs` directory if it does not exist.
* `bg_resolve_node_ref()` now uses fixed-string matching for partial label lookups to avoid regex metacharacter issues.
* Added section headers for node update/removal operations in `R/22-dagri-adapters.R`.
* Fixed protocol object validator to reject unnamed (bare) lists for object-type schemas.
* Removed orphaned `bg_submit.Rd` man page.
* Ran `air format` across all affected files.
* Updated dagriculture dependency to `>= 0.1.5`.
* Fixed dagriculture API mismatches: `dagri_update_node` and `dagri_remove_node` now pass `id` instead of `node_id`; `dagri_add_gate` now passes `edge` instead of `edge_id`.
* Converted default workflow `input_contract` values from bare strings to proper named lists to satisfy dagriculture 0.1.4+ validation.

# bayesgrove 0.4.7

* Eliminated all `<<-` super-assignments in production R code by refactoring to explicit environment-based state (`R/13-repl.R`, `R/19-workflow-protocol.R`, `R/26-serve.R`).
* Removed redundant `library(bayesgrove)` call in the mirai worker (`R/11-async.R`); the function is already resolved via `get()` from `asNamespace()`.
* Extracted `bg_build_decision_metadata()` helper to deduplicate decision metadata construction across `R/13d-repl-exec.R`, `R/28-protocol-contract.R`, and `R/23-templates.R`.
* Replaced `requireNamespace()` guard in test properties helper (`tests/testthat/test-09-properties.R`).
* Refactored `<<-` in test fixtures and test callbacks to use environment-based mutable state.

# bayesgrove 0.4.6

* Fixed mirai worker argument handling to properly pass `lib_paths` and worker args to background processes.
* Fixed mirai daemon cleanup on project close to prevent resource leaks.
* Used `mirai::is_error_value()` for proper error detection in job reconciliation.
* Auto-create project directory in `bg_init()` if the target path does not exist.
* Refactored REPL module into separate files for better organization (`13a-repl-utils.R`, `13b-repl-display.R`, `13c-repl-guide.R`, `13d-repl-exec.R`).
* Added property-based tests and error case coverage.
* Removed stale `README.html` from the repository.

# bayesgrove 0.4.5

* Fix problem to return proper empty graph for empty project

# bayesgrove 0.4.4

* Changed `bg_init()` so new projects start empty by default instead of automatically enabling `bayesguide.default_bayesian`.
* Added `bg_use_workflow_packs()` and `bg_use_default_workflow()` so projects can opt into the built-in review stack after creation.
* Persisted workflow-pack activations and node-kind registrations in project config, and taught `bg_open()` to rehydrate persisted node-kind runtime registrations automatically on reopen.
* Refreshed the README and package vignettes so the documented setup flow matches the new empty-by-default project model.

# bayesgrove 0.4.3

* Added process-guidance, model-taxonomy, Stan-workflow, and dagitty-backed causal workflow packs that extend the review protocol with primary-source-backed Bayesian workflow semantics.
* Added causal selection contracts so DAG-derived required, forbidden, and ranked admissible terms can constrain formulas and projection-oriented review in causal branches.
* Added the primed-prior case study vignette and refreshed the README plus package vignettes so the documented workflow-pack surface matches the current package behavior.

# bayesgrove 0.4.2

* Added `bg_execute_action()` and `bg_extension_registry()` to the pkgdown reference so the site builds cleanly in CI.
* Reworked the affected protocol and test files to satisfy `air format` and updated the jarl-facing assertions to avoid redundant logical equality checks.

# bayesgrove 0.4.1

* Added five optional built-in workflow packs: `bayesgrove.prior_workflow`, `bayesgrove.model_checks`, `bayesgrove.model_selection`, `bayesgrove.causal_minimal`, and `bayesgrove.pad_scaffold`.
* Extended the review-decision template and workflow registry so those packs can emit prior rationale, prior and posterior predictive review, SBC review, model-selection review, causal framing prompts, and PAD annotation prompts.
* Added an `extensions` vignette covering node sets, backend plugins, domain modules, summary emission, and the current status of workflow-pack extension.
* Refreshed the README, getting-started, guided-review-loop, simulation-study, and demo documentation so the optional workflow-pack surface matches the current code.

# bayesgrove 0.4.0

* Added a richer, dashboard-style TUI workflow to `bg_repl()` featuring a new `dashboard` command and a default initialization view showing workflow health, obligations, and pending gates.
* Added a `next` command to the guided client that automatically previews and prompts to execute the top recommended workflow action without manual ID lookup.
* Improved post-action clarity and safety inside the REPL by rendering detailed action previews before execution and adding inline workflow-branch scope navigation.
* Added `lineage` and `export` REPL commands so users can view structural branch history and generate full workflow reports directly from the interactive operator console.
* Extended interactive REPL tests to cover new guided-client rendering helpers, preserving decoupled CLI state modeling for future GUI consumption.

* Added an experimental `bg_serve()` websocket IPC layer with versioned `GraphSnapshot`, `ProtocolEvent`, `Command`, and `CommandResult` schemas under `inst/protocol/`.
* Marked the bounded remote command/query surface directly in `bg_api_boundary()` via `remote_accessible`, and added validation that keeps the server dispatch registry aligned with that API boundary.
* Added protocol-schema coverage for the new IPC messages plus websocket integration tests that exercise connect, command dispatch, reconnect, and async responsiveness when local sockets are available.
* Simplified async execution support to rely on `mirai` only, removing the parallel `callr` backend branch from submission, cancellation, reconciliation, package metadata, and tests.

# bayesgrove 0.3.13

* Added `bg_api_boundary()` plus a machine-readable registry covering every exported `bg_*` function with explicit `stable`, `experimental`, or `internal_exported` classifications and `remote_accessible` flags.
* Added regression tests that enforce exact set equality between the API boundary registry and the exported namespace so the public surface cannot drift silently.
* Added `inst/protocol/` schema metadata for `bg_next_actions_result`, `bg_obligation_item`, `bg_action_item`, `bg_partitioned_result`, and `bg_template_descriptor`, then tightened the validation helpers to check nested fields, object maps, and local schema references.
* Aligned the live workflow-protocol return shapes with the checked-in schema so empty protocol maps serialize as JSON objects instead of arrays, and surfaced `bg_api_boundary()` in the pkgdown reference.

# bayesgrove 0.3.11

* Moved the contributor-facing `dagriculture` boundary note out of `docs/` so pkgdown can clean and rebuild the GitHub Pages output directory in CI.
* Disabled `indentation_linter` in `.lintr` so formatting remains enforced by `air format` without a conflicting lint failure.

# bayesgrove 0.3.10

* Removed stale default-workflow comparison action code, moved parameter-suggestion helpers into the default pack layer, and tightened comparison/disposition identity handling around compact `comparison_signature` values.
* Hardened the internal `dagriculture` adapter helpers with clearer internal markers, deterministic edge-ID handling for graph diffs, and regression coverage for unnamed-edge diffs plus comparison/disposition staleness.
* Narrowed lint policy by disabling `object_length_linter` for the package's deliberate protocol helper names while fixing the remaining indentation and brace warnings in tests and vignettes.

# bayesgrove 0.3.9

* Added an explicit Phase 7 architecture note at `architecture/dagriculture-boundary.md` plus a small internal adapter layer that isolates bayesgrove's graph-generic dependency on `dagriculture`.
* Refactored repeated inline structural graph queries into focused `bg_dagri_*` helpers for edge selection, descendant lookup, deterministic edge ordering, state recomputation, and structural graph diffs without moving workflow semantics out of bayesgrove.
* Added regression coverage that distinguishes graph-generic helper behavior from bayesgrove-specific workflow, branch, and provenance logic.
* Synced the README and pkgdown-facing site sources for the phase-7 boundary work, including a dedicated architecture page that pkgdown can render without a broken navbar link.

# bayesgrove 0.3.8

* Added a deterministic `guided-review-loop` vignette plus regression coverage for one full case-study loop covering workflow holds, branch lineage, criticism and comparison decisions, branch dispositions, stale summaries, and report export.
* Updated the README and pkgdown article navigation to surface the new case study alongside the lower-level workflow examples.
* Fixed the `DESCRIPTION` remote metadata so `pak` can parse the GitHub remotes correctly during CI lockfile creation.
* Replaced `:::` test and demo accesses with exported calls or namespace lookups to clear the internal-function lint warnings without changing behavior.

# bayesgrove 0.3.7

* Added a first-class built-in template registry plus `bg_list_templates()` for `diagnostic_check`, `branch_comparison`, `branch_and_modify_fit`, and `review_decision`.
* Reworked the guided REPL and workflow action payloads to execute template-backed node creation, branching, and review decisions through the shared registry while preserving `branch_comparison` compatibility.
* Split the checked-in REPL demo surface into reusable checkpoints plus focused remediation, comparison, and disposition tapes while keeping the original full walkthrough.

# bayesgrove 0.3.6

* Added explicit retirement semantics with `bg_retire_node()` and `bg_retire_branch()` so stale analysis paths can be removed from planning and workflow guidance without changing `bg_invalidate()`'s recompute-oriented behavior.
* Updated planning, workflow contexts, pending gates, and REPL workflow commands to exclude retired nodes and branches from runnable work while preserving cached artifacts for inspection.
* Refreshed the guided REPL demo and regression coverage so the stronger default workflow pack now demonstrates remediation, retirement, comparison, and branch acceptance without rerunning retired warning paths.

# bayesgrove 0.3.5

* Strengthened the built-in `bayesguide.default_bayesian` workflow pack with explicit fit-criticism review, comparison obligations, model-comparison decisions, and branch accept/reject dispositions keyed to current evidence.
* Added deterministic comparison/disposition basis tracking plus explicit hold targeting so the stronger default workflow blocks downstream work without blocking the comparison step needed to satisfy it.
* Added a guided, branch-aware REPL workflow with scope switching, action execution, richer graph/status rendering, and helper APIs for branch labels, branch listings, and scope-partitioned protocol results.
* Added branch-with-continuation support for guided workflow branching and refreshed the REPL demo, README, and reference docs to match the new protocol-driven workflow loop.
* Tightened workflow scope behavior so branch contexts inherit project summaries, comparison actions only target fit nodes, and branch-local graph views no longer leak cross-scope edges.

# bayesgrove 0.3.4

* Enforced workflow holds inside `bg_run()` and `bg_submit()` so blocking obligations now affect real execution, not just advisory planning.
* Tightened workflow-context scoping, transactional gate answering, sync job logging, and severity merging to match the workflow protocol and persistence contracts.
* Added locking around shared artifact-index, summary, decision, job, gate-spec, and graph writes to harden async and multi-session behavior.
* Aligned public APIs and docs for status reporting, branching returns, bundling/export arguments, workflow reference topics, and report examples.

# bayesgrove 0.3.3

* Added the first end-to-end workflow-hold integration slice covering summary persistence, workflow context construction, obligation derivation, planner hold propagation, and next-action clearing after fit replacement.
* Updated `bg_plan()` to accept caller-supplied `external_holds`, surface `external_blocked`, and keep externally held nodes out of `to_execute` without conflating them with structural blockers.
* Refreshed the README, getting-started vignette, and workflow-protocol reference docs to document summary-driven workflow guidance and planner holds.

# bayesgrove 0.3.2

* Added the first protocol-facing workflow APIs: `bg_workflow_packs()`, `bg_workflow_context()`, and deterministic `bg_next_actions()`.
* Added workflow-pack provider dispatch plus canonical runtime-side ID generation and deduplication for obligations and curated actions.
* Added a minimal built-in default Bayesian pack that emits branch goal-setting and computation-review workflow guidance from decisions and summaries.
* Extended workflow context evidence with lightweight artifact availability and added tests for empty packs, obligations, actions, deduplication, and project-vs-branch scope behavior.

# bayesgrove 0.3.1

* Expanded project snapshots, handoff bundles, and workflow reports to include decisions, artifacts, richer pause/resume state, and gate context.
* Added a simulation-study vignette and pkgdown article navigation for the workflow example.

# bayesgrove 0.3.0

* Added persisted workflow context plumbing for branch registries, goal registries, and summary logs.
* Added branch-aware scope resolution plus `bg_build_workflow_context()` with structural, execution, and evidence partitions.
* Added summary freshness/staleness derivation from predicted fingerprints and active artifact-index bindings.
* Updated branching, goal-setting, invalidation, and execution paths to persist workflow summaries and registries without reopening heavy artifacts.
* Simplified async workers to always load the installed package, removing source-tree autodetection from background execution.

# bayesgrove 0.2.0

* Added `bg_submit()`, `bg_wait()`, and `bg_cancel()` for non-blocking background execution of workflows (removed again in 0.6.0).
* Integrated `callr` and `mirai` execution backends for parallel job dispatch.
* Added `bg_jobs()` and JSONL logging (`.bayesgrove/runs/jobs.jsonl`) to track job status, start times, completion, and task progress.
* `bg_status()` doubles as a reconciliation poller (`bg_reconcile_daemon_jobs()`), updating crashed or finished daemon jobs without blocking the main session.
* A failing background task no longer stops other independent node jobs.
* Exported internal artifact/caching methods conditionally to support the standalone worker contract across process boundaries.

# bayesgrove 0.1.1

* Renamed and rewrote the `bayesguide` prototype as `bayesgrove`.
* Moved the pure graph execution logic to a separate package, `dagriculture` (`sims1253/dagriculture`), separating computation from Bayesian semantics.
* Core workflow state is backed by reference-semantic `S7` classes (e.g. `bg_handle`).
* Decisions, alternative choices, and rationales are captured through "gates" layered over the graph.
* Cache keys are built from upstream fingerprints and structured backend signatures, backing a local content-addressed store.
* Synchronous execution (`bg_run()`) and a backend registry (`bg_register_backend()`, removed in 0.6.0), including an initial `cmdstanr` plugin.
