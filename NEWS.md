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
