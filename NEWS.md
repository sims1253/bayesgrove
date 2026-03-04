# bayesgrove 0.1.1

* **Greenfield Architecture Rewrite**: The `bayesguide` prototype has been renamed and completely rebuilt as `bayesgrove`.
* **Graph Engine Split**: The pure graph execution logic has been moved to a separate, foundational package called `dagriculture` (`sims1253/dagriculture`), ensuring strict separation of computation from Bayesian semantics.
* **S7 Object Model**: Core workflow states are now backed by explicit reference-semantic `S7` classes (like `bg_handle`).
* **Decision Provenance Layer**: Decisions, alternative choices, and rationales are now explicitly captured through semantic "gates" layered over the topological graph. 
* **Deterministic Fingerprinting & Caching**: Cache keys are now built from upstream fingerprints and structured backend signatures, creating a robust local Content-Addressed Storage (CAS) mechanism.
* **Async & Plugin Foundations**: Synchronous runtime execution (`bg_run()`) and backend registry (`bg_register_backend()`) are now fully operational, including the initial `cmdstanr` MVP plugin.
