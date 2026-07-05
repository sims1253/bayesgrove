# bayesgrove restructuring plan (post thermonuclear review, 2026-07)

This plan replaces the round-3 punch list, which is done (verified:
`active_handle` is gone, `bg_set_node_data` is registered in the API boundary,
`modifyList` is qualified, brms E-BFMI rows are sorted, the `expect_setequal`
warning is fixed). The review found a package with a genuinely novel core —
the enforced review protocol with decision provenance — wrapped in a runtime
that does too much, re-reads disk too often, and under-delivers on the
teaching material that is its reason to exist.

Strategic direction the milestones implement:

- bayesgrove's moat is the **protocol layer** (summaries → obligations →
  decisions → holds), not the graph runtime. Graph-generic code migrates
  toward `dagriculture` (same owner) over time.
- Execution goes parallel via **`purrr::in_parallel()` + `mirai`** with a
  wave-based scheduler; the protocol hold-check is the natural barrier
  between waves.
- Three audiences carry equal weight, with distinct needs the milestones
  must serve: **practitioners** need a low-friction happy path, fast runs,
  and reports they can hand to collaborators and reviewers; **researchers**
  need scale (parallel SBC and simulation studies), reproducibility
  manifests, and a machine-readable protocol trace they can cite in methods
  sections; **educators/students** need visualization, a concepts vignette,
  a real case study, and `bg_explain()`. All three matter more than new
  protocol packs.
- The packs must never demand evidence no shipped executor can produce.

Ground rules (unchanged from previous plans):

- Never commit or push. The user reviews and commits.
- Run `air format` on touched files.
- Run `devtools::document()` if roxygen changes.
- After each milestone: run
  `Rscript -e 'devtools::test(reporter = "summary")'` and
  `Rscript -e 'devtools::check(document = FALSE, vignettes = FALSE, args = c("--no-tests", "--no-manual", "--no-build-vignettes"), quiet = TRUE)'`
  and report the actual numbers.
- Milestones are ordered by priority but are independent unless noted. Do
  NOT attempt them all in one pass; each is a reviewable unit.

---

## Milestone 1 — Correctness fixes in the diagnostic backends (small, first)

1. **Divergence-rate denominator is inconsistent by a factor of `n_chains`.**
   In `bg_cmdstanr_hmc_metrics()` (`R/backends-cmdstanr.R:96`),
   `num_transitions` is computed as total post-warmup draws divided by
   `num_chains()` (per-chain count), but `divergences` is summed **across**
   chains. `bg_hmc_severity()` then computes
   `divergence_rate = divergences / num_transitions`, inflating the rate by
   the chain count (4 chains, 1000 iters, 20 divergences → reported 2%
   instead of 0.5%). Fix: define `num_transitions` as the **total**
   post-warmup transitions across chains (`nrow(as_draws_matrix(draws))`),
   update the roxygen on `bg_hmc_severity()`, and align the brms path
   (`R/backends-brms.R` currently sets `num_transitions = NA_integer_` —
   compute it there too from the draws). Add a ground-truth unit test:
   4 chains × 1000 draws with 20 divergences must yield rate 0.005 and
   severity `"warning"`, not `"error"`, at the default 1% threshold.
   Existing fixtures in `test-backends.R` pass `num_transitions = 4000L`
   directly and should remain valid.

2. **Substring matching of draw variables.** `bg_executor_loo()` uses
   `grepl(log_lik_var, colnames(draws), fixed = TRUE)` and
   `bg_executor_ppc()` does the same for `yrep_var`
   (`R/backends-cmdstanr.R:375,533`). `"log_lik"` matches
   `log_lik_extra[1]`; `"yrep"` matches `yrep_new[3]`. Replace with anchored
   matching on Stan's indexed-variable form: select columns matching
   `^<var>(\[|$)` (escape the var with `rex`-style quoting or
   `utils::glob2rx`-free manual regex), or better, use
   `posterior::subset_draws(draws, variable = var)`. Add unit tests with a
   decoy variable (`log_lik_saturated`) present.

3. **ESS warning threshold ignores chain count.** `bg_hmc_severity()`
   hardcodes `ess_warn = 400`. Vehtari et al. (2021) recommend ~100 per
   chain. Add an optional `n_chains` entry to `metrics`; when present,
   default `ess_warn` to `100 * n_chains`, otherwise keep 400. Explicit
   `thresholds$ess_warn` always wins. Update both backends to pass
   `n_chains`, plus unit tests.

4. **`bg_status()` computes the plan twice** (`R/status.R:19` builds
   `plan_seed`, then line 36 calls `bg_plan()` again with holds). Reuse the
   seed via `bg_refresh_run_plan(project, plan_seed, graph, external_holds)`
   instead of a second full plan. Behavior must be identical; the run-loop
   performance test should show fewer plan constructions.

## Milestone 2 — Persistence layer stops being O(n²)

The jobs log currently: appends a **full record snapshot** per update
(`bg_update_job` → `bg_log_job`), re-reads and re-parses the whole
`jobs.jsonl` on every update (`bg_update_job` calls `bg_jobs`), and counts
every line of the file on every append (`bg_append_jsonl` seq stamping).
Each executed node does this several times, so a run of N nodes is O(N²)
file work, and `jobs.jsonl` grows without bound across a project's life.

1. Introduce an internal jobs handle/cache: an environment on `bg_handle`
   (`@.state$jobs_cache`) holding the parsed jobs list plus the current line
   count, invalidated by file mtime+size. `bg_jobs()` consults it;
   `bg_update_job()` updates it in place after appending. `bg_append_jsonl`
   accepts an optional known line count to skip the recount.
2. Keep the append-only snapshot format on disk (it is crash-safe and the
   recovery code depends on last-record-wins); do not change the schema.
3. `bg_blocking_obligation_holds()` (`R/workflow-protocol.R:446`) re-reads
   the graph from disk on every protocol evaluation, including once per
   executed node inside `bg_run`. Thread the already-loaded graph through
   (`bg_next_actions_impl` has access to plan/state; add a `graph` argument
   defaulting to `bg_read_graph(project)`).
4. Acceptance: extend `test-run-loop-performance.R` to assert the number of
   full `jobs.jsonl` parses during a 20-node run is O(N) (count via a
   mockable read hook or by instrumenting temporarily and asserting
   wall-clock scaling stays near-linear).

## Milestone 3 — Parallel execution with mirai (wave scheduler)

Async was removed in the 0.6 rework; reintroduce parallelism with modern
primitives instead of the old bespoke layer. Use `purrr::in_parallel()`
(purrr >= 1.1.0) backed by `mirai` daemons. Add `mirai`, `carrier`, and
`purrr` to `Suggests` (soft dependency: sequential path must work without
them).

Design (wave-based, matches protocol semantics):

1. `bg_run(project, targets = NULL, parallel = c("auto", "never", "always"))`.
   With `parallel = "auto"`, parallel dispatch is used when
   `mirai::daemons_set()` (or equivalent status check) reports active
   daemons; otherwise fall back to the current sequential loop unchanged.
2. Scheduler: compute the plan; take the current **wave** = all nodes in
   `plan$to_execute` whose upstream artifacts are already available (they
   are mutually independent by construction). Dispatch the wave with
   `purrr::map(waves, in_parallel(...))` or `mirai::mirai_map()`. After the
   wave resolves, write summaries/jobs, re-evaluate protocol holds (the
   existing between-node logic), refresh the plan, and dispatch the next
   wave. Holds discovered mid-run therefore take effect at wave boundaries —
   document this semantic.
3. Worker contract: workers receive (node, resolved inputs, kind executor,
   project path) and (a) run the executor, (b) write the artifact blob
   directly into the CAS via `bg_store_cas_blob` — content addressing makes
   concurrent writes idempotent, and the tempfile+rename pattern is already
   atomic — and (c) return `list(ref, summaries, error)`. The **main process
   only** writes the artifact index, summaries JSONL, and job records, so
   the single-writer lock model is preserved.
4. Executor shipping: built-in executors (`builtin:*`) are package functions
   and resolve on the daemon by name after `library(bayesgrove)`. User
   executors must be self-contained closures; crate them with
   `carrier::crate()` at dispatch time and produce an actionable error
   message when crating fails ("executor captures objects from the global
   environment; make it self-contained"). This aligns with the existing
   trust model, which already persists executor source as inert text.
5. `bg_pause()` becomes meaningful here: check the pause flag at wave
   boundaries. (See Milestone 6 item 2.)
6. Tests: a two-daemon integration test (`mirai::daemons(2)` in the test,
   `skip_on_cran()`, skip when mirai unavailable) asserting: independent
   siblings run in one wave, a blocking summary from wave 1 holds the
   dependent node before wave 2, and results/fingerprints/artifacts are
   byte-identical to a sequential run of the same graph.
7. Documentation: a short "Parallel execution" section in the
   getting-started vignette: `mirai::daemons(4)` then `bg_run()`. This is
   the payoff milestone for SBC and simulation studies (hundreds of
   independent fits), so say so.

## Milestone 4 — Close the evidence gap between packs and backends

Packs currently derive obligations that reference summary kinds no shipped
executor emits (`loo_pit_calibration`, SBC ranks, projection-predictive
review). A student following the guidance hits a dead end.

1. Add a built-in `loo_pit` node kind to the cmdstanr and brms backends:
   compute LOO-PIT values (loo::loo + PIT via `loo::loo_pit()` or manual
   ECDF), emit a `loo_pit_calibration` summary (severity by a simple
   uniformity check, e.g. Kolmogorov–Smirnov distance thresholds), and store
   plot-ready PIT values in the artifact.
2. Add a minimal built-in `sbc` node kind: given a `stan_file`, a data
   generator expressed as a prior-predictive `prior_fit` node upstream, and
   `n_sims`, run the rank-statistic SBC loop (Talts et al. 2018) and emit an
   `sbc_results` summary with rank histogram counts and a chi-squared-based
   severity. Keep it honest about cost: document that this is the node to
   run under mirai daemons (Milestone 3). If full SBC is judged out of scope
   for one pass, implement the executor for `n_sims` small and file the
   scale-out as future work — but the node kind must exist.
3. Make the `ppc` executor store plot-ready data (observed `y` plus a
   capped subsample of `yrep` rows, e.g. 100 draws) in its artifact instead
   of only p-values.
4. Add a vocabulary/coverage test: every `summary_kinds` string referenced
   by any pack provider (grep the pack sources for `summary_kinds =`) must
   be present in `bg_summary_vocabulary()`, and every kind that a pack asks
   the user to *produce* must map to either a built-in node kind or an
   explicitly documented "bring your own executor" note in the extensions
   vignette. Table both mappings in one roxygen topic
   (`?bg_summary_vocabulary`).

## Milestone 5 — Move graph-generic code to dagriculture

`architecture/dagriculture-boundary.md` already names the candidates; the
user owns both packages, so execute the move now rather than documenting it
again.

1. In dagriculture (separate repo/PR): add `dagri_incoming_edges()`,
   `dagri_outgoing_edges()`, `dagri_order_edges()`, `dagri_edge_ids()`, and
   `dagri_graph_diff()` — the bodies can be lifted nearly verbatim from
   `R/dagri-adapters.R`, which documents them as migration candidates.
   Release as dagriculture 0.2.0.
2. In bayesgrove: bump the `dagriculture (>= 0.2.0)` pin, turn the adapter
   helpers into one-line pass-throughs (keep the `bg_dagri_*` names so call
   sites don't churn), and delete the local implementations and their
   now-duplicated unit tests (keep boundary tests that assert the adapter
   layer's behavior contract).
3. Update `architecture/dagriculture-boundary.md`: the moved helpers switch
   from "candidate" to "owned by dagriculture"; add a decision note on the
   larger question — whether fingerprinting + CAS + plan-state derivation +
   jobs should become a generic dagriculture execution layer, leaving
   bayesgrove purely semantic. Record the recommendation (yes, eventually,
   once a second consumer exists) but do NOT implement it in this pass.

## Milestone 6 — API and UX consolidation

1. **Stop teaching the plumbing.** `bg_run()` already applies protocol holds
   itself; the README/vignette pattern of manually threading
   `guide$metadata$external_holds` into `bg_plan()` is advanced usage.
   Sweep vignettes for it and replace with `bg_run()` + `bg_next_actions()`.
   Keep `bg_plan(external_holds =)` documented for programmatic callers.
2. **`bg_pause`/`bg_resume` are near-no-ops in a synchronous runtime** (they
   only gate the next `bg_run` call). Reclassify both from `stable` to
   `experimental` in `R/api-boundary.R` with a roxygen note that they gate
   wave boundaries once parallel execution lands (Milestone 3). Do not
   delete.
3. **Print methods are the console UI — make them carry the protocol.**
   - `print.bg_run_handle`: show status, executed/cached counts, and — when
     status is `"blocked"` — each held node with its hold reason and the
     single next call to make (`bg_next_actions(handle)`).
   - Give `bg_next_actions()` results a class (`bg_next_actions_result`)
     and a print method rendering obligations as a numbered checklist:
     severity glyph, kind, title, one-line why, and a copy-pasteable
     resolving call (`bg_record_decision(handle, kind = "computation_review",
     ...)`). The REPL display code in `R/repl-display.R` already formats
     most of this; extract shared formatters instead of duplicating.
   - This must not break the plain-data contract: the result stays a list;
     only a class attribute is added (verify `inst/protocol` fixture tests
     still pass).
4. **Merge `bg_branch_with_continuation()` into `bg_branch()`** as
   `bg_branch(project, node_id, label, continue = character())`, returning
   the same shape either way. Soft-deprecate the long name (one release,
   `lifecycle`-style warning), update vignettes/README.
5. **One concepts page for the question surfaces.** Gates (user-authored
   checkpoints on edges), obligations (evidence-driven, from packs), and
   decisions (the answers) overlap confusingly. Write a `?bayesgrove-concepts`
   roxygen topic (and see Milestone 8's concepts vignette) with a single
   table: what asks, what answers, what blocks. Do not remove gates — they
   serve pre-planned checkpoints that evidence-driven obligations cannot.
6. **REPL performance and teach-ability.** Each REPL command currently
   triggers multiple full plan+protocol evaluations (`bg_repl_node_rows`,
   `bg_repl_status_rows`, dashboard). Compute one `(plan, holds, status)`
   bundle per command cycle and pass it through the display helpers. Add an
   `explain <n>` command that prints the selected obligation's `why` plus
   its `references` (already carried on every obligation via
   `bg_workflow_references()` — surface them, they are the teaching hook).
7. **Error message audit:** every abort that blocks a user action must name
   the resolving function (most already do; sweep `cli_abort` calls in
   `run.R`, `decisions.R`, `branch.R`).
8. **`bg_update_node()` silently drops unspecified params.** It passes
   `params` straight to `dagriculture::dagri_update_node()`, which replaces
   the whole list — so the documented repair flow
   `bg_update_node(handle, id, params = list(stan_file = f))` wipes the
   node's `chains`, `seed`, and iteration params (the vignettes only work
   because the built-in defaults happen to match). Make `bg_update_node()`
   merge via `utils::modifyList()` (matching `bg_set_node_data()`), add a
   `replace = FALSE` escape hatch for intentional wholesale replacement,
   document both, and add a regression test asserting that updating one
   param preserves the others. Coordinate with dagriculture's PLAN.md
   Milestone 3, which keeps replace semantics in the primitive and
   documents that merging is the consumer's job.
9. **Practitioner happy path.** The minimal fit today takes five calls
   (`bg_init`, `bg_use_cmdstanr`, `bg_add_node` x2, `bg_set_node_data`,
   `bg_run`). Add two experimental convenience helpers in a new
   `R/sugar.R`: `bg_fit_stan(handle, stan_file, data, label = NULL, ...)`
   and `bg_fit_brms(handle, formula, data, label = NULL, ...)`, each
   creating the data node (via `bg_set_node_data`), the fit node, running
   it, and returning the fit node id invisibly with the run handle printed.
   Pure sugar over existing verbs — no new semantics, params pass through
   to the fit node. Register both as `experimental` in the API boundary and
   use `bg_fit_stan()` as the first code a practitioner sees in
   getting-started.

## Milestone 7 — Visualization (table stakes for every audience)

A Bayesian-workflow package with zero plots is a whiteboard with no markers:
students learn diagnostics visually, practitioners screen fits visually, and
researchers put these figures in papers. Two thin, high-leverage additions;
keep heavy deps in `Suggests`.

1. `bg_graph_mermaid(handle)`: emit Mermaid flowchart text for the active
   graph — node label + kind, state coloring (cached/ready/held/blocked/
   failed), branch grouping via subgraphs, decision counts as edge notes.
   Pure string manipulation, zero new dependencies, renders on GitHub and
   in Quarto. Embed it in the `bg_export_report(format = "md")` output.
2. `bg_plot(handle, node_id)`: S3 dispatch on the node's artifact/summary
   kind, using `bayesplot` (Suggests) — `ppc_dens_overlay` for ppc artifacts
   (needs Milestone 4 item 3), PIT ECDF for `loo_pit`, rank histogram for
   `sbc`, `mcmc_trace`/`np_divergences` for fits with warning diagnostics.
   Abort with an informative message when bayesplot is missing or the node
   kind has no plot.
3. HTML report export embeds the mermaid graph (as rendered SVG via a
   `<script>` include or pre-rendered text block) and any plots for nodes
   with fresh summaries.

## Milestone 8 — Documentation overhaul for three audiences

Every item below names its primary audience; the milestone is done when a
practitioner, a researcher, and a student each have an obvious entry path
from the pkgdown front page.

1. **README**: already rewritten by the reviewer (README.Rmd). If it drifts,
   regenerate `README.md` with `devtools::build_readme()`; all chunks are
   `eval = FALSE` with captured output, so no CmdStan is needed.
2. **New vignette `concepts.Rmd`** (all audiences; goes first in the
   navbar): the mental model in one place — node/executor/summary → obligation → action →
   decision → hold; scopes (`project` vs `branch:*`); evidence freshness and
   staleness; gates vs obligations; a mermaid diagram of the loop; a
   glossary table. Most of this text exists scattered across roxygen and
   the extensions vignette; consolidate, don't rewrite.
3. **New case-study vignette** (practitioners and students) with a real
   dataset and a real inferential question (not mock executors, not
   eight-schools again — e.g. a hierarchical logistic regression on a
   bundled dataset), following the precomputed-output pattern of
   `eight-schools.Rmd` (`tools/precompute-vignettes.R`). It must include
   plots (Milestone 7) and end with `bg_export_report()`. This is the
   flagship narrative artifact: a practitioner should recognize their own
   daily loop in it.
4. **"Reproducible research with bayesgrove" article** (researchers;
   pkgdown-only, `vignettes/articles/`): how to report a bayesgrove-backed
   analysis in a methods section — what the bundle contains, how the
   decision log and summaries JSONL serve as a machine-readable audit
   trail, how fingerprints pin the environment, and how to run an SBC or
   simulation study at scale under mirai daemons (Milestone 3). Include a
   worked "data and code availability" statement template.
5. **Reproducibility manifest in `bg_bundle()`** (researchers): capture
   `sessioninfo::session_info()` output (or `utils::sessionInfo()` if the
   extra dependency is unwanted) plus CmdStan/Stan versions into the bundle
   as `manifest.json`, and verify round-trip in the existing bundle tests.
   Cross-machine restore of a bundle must surface manifest mismatches as a
   warning, not silently rerun everything.
6. **"Teaching with bayesgrove" article** (educators; pkgdown-only, `vignettes/articles/`):
   how to run a course assignment on top of the decision log — students
   submit the exported report + bundle; instructors verify the decision
   trail (every prior change has a rationale, every warning has a review).
   Include a grading-checklist mapping obligations to rubric items.
7. **Positioning section** ("Why not targets / workflowr?") in the README or
   concepts vignette: targets caches computation but records no decisions;
   workflowr versions notebooks but does not read diagnostics; bayesgrove's
   claim is the enforced link between evidence and recorded judgment.
   Reviewers and instructors will ask; answer once, in writing. (The
   rewritten README already carries a first version; keep it in sync.)
8. **Roxygen pass over the stable API**: every `stable` function gets an
   `@examples` block (runnable, tempdir-based) and an `@family` tag mirroring
   the pkgdown reference groups. `bg_api_boundary()` is currently the
   best-documented export; the core verbs deserve at least that.
9. Add `inst/CITATION` (package + the Gelman et al. 2020 workflow paper) so
   researchers can cite the tool.
10. pkgdown navbar: Concepts first under "Get Started", then Getting Started,
   the case study, Guided Review Loop, Reproducible Research, Teaching,
   Extensions. Note:
   `eight-schools.Rmd` and `primed-priors-case-studies.Rmd` are missing from
   the README vignette table — the rewritten README fixes this; keep them in
   `_pkgdown.yml` articles listing too.

## Milestone 9 — Repo hygiene (opportunistic, last)

1. Split `R/workflow-context.R` (1476 lines of mixed concerns) into
   `R/persist-io.R` (timestamps, atomic JSON, JSONL, locks),
   `R/persist-registries.R` (branch/goal/summary registries),
   `R/summary-freshness.R`, and `R/workflow-context.R` (context building
   only). Pure moves, no behavior change; run the full suite after.
2. Remove the `bayesguide.*` pack-id deprecation alias
   (`R/workflow-protocol.R:213`) in the release after next (leave a TODO
   with the version number now).
3. Decision recorded here so no agent "fixes" it: the S7 `bg_handle` with an
   environment-backed state is intentional (reference semantics with S7
   validation); do not migrate to R6 or plain environments.

---

## Acceptance (per milestone and overall)

1. `Rscript -e 'devtools::test(reporter = "summary")'` — 0 failures; only
   the known low-ESS sampler warnings from tiny integration models remain.
2. `Rscript -e 'devtools::check(document = FALSE, vignettes = FALSE, args = c("--no-tests", "--no-manual", "--no-build-vignettes"), quiet = TRUE)'`
   — 0 errors, 0 warnings, 0 notes.
3. Report final pass/fail/skip counts and any deviations from this plan to
   the user. The user decides version bumps and commits; do not delete this
   file.
