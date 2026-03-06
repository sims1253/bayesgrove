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

* **Phase 2 (Async Execution Layer) Implementation**
* Added `bg_submit()`, `bg_wait()`, and `bg_cancel()` to orchestrate true non-blocking background execution of workflows.
* Integrated `callr` and `mirai` execution backends for parallel asynchronous job dispatch.
* Added `bg_jobs()` and JSONL logging (`.bayesgrove/runs/jobs.jsonl`) to durably track job status, start times, completion, and task progress.
* Updated `bg_status()` to double as a reconciliation poller (`bg_reconcile_daemon_jobs()`), updating crashed or finished daemon jobs automatically without blocking the main session.
* Ensured isolated failure containment: if a single background task fails, other independent node jobs continue executing.
* Exported internal artifact/caching methods conditionally to support the standalone worker contract across process boundaries.

# bayesgrove 0.1.1

* **Greenfield Architecture Rewrite**: The `bayesguide` prototype has been renamed and completely rebuilt as `bayesgrove`.
* **Graph Engine Split**: The pure graph execution logic has been moved to a separate, foundational package called `dagriculture` (`sims1253/dagriculture`), ensuring strict separation of computation from Bayesian semantics.
* **S7 Object Model**: Core workflow states are now backed by explicit reference-semantic `S7` classes (like `bg_handle`).
* **Decision Provenance Layer**: Decisions, alternative choices, and rationales are now explicitly captured through semantic "gates" layered over the topological graph.
* **Deterministic Fingerprinting & Caching**: Cache keys are now built from upstream fingerprints and structured backend signatures, creating a robust local Content-Addressed Storage (CAS) mechanism.
* **Async & Plugin Foundations**: Synchronous runtime execution (`bg_run()`) and backend registry (`bg_register_backend()`) are now fully operational, including the initial `cmdstanr` MVP plugin.
