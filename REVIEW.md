# bayesgrove — Remaining Review Items

Updated 2026-07-11 after the case-study/vignette-consolidation work on
`plan-implementation`.

Resolved this round (see NEWS.md for user-facing entries):

- **Flagship case study** — `vignettes/case-study-roaches.Rmd`: real data
  (Gelman-and-Hill roaches trial, shipped in `inst/extdata/`), a cleanly
  sampling but badly misfitting Poisson, enforced criticism, two branches,
  a three-way LOO comparison, dispositions, and an exported report. The
  case study also exposed and fixed a real defect: the default pack only
  recognized the literal node kind `fit`, so the comparison loop had never
  fired for `cmdstanr_fit`/`brms_fit` (now `bg_pack_is_fit_node()`).
- **Workflow-pack constructor adoption** — `workflow-packs-default.R` now
  uses `bg_pack_obligation()`/`bg_pack_action()`; constructors live in
  `workflow-packs-constructors.R`.
- **Getting-started rework** — opens with a real `bg_fit_stan()` loop
  (precomputed outputs), lifecycle details demoted to reference material.
  `guided-review-loop` and `primed-priors-case-studies` vignettes deleted;
  `dagriculture-boundary` folded into `concepts`.
- **API stability** — `bg_use_default_workflow()`, `bg_use_workflow_packs()`,
  `bg_next_actions()` reclassified as stable; `bg_read_decisions()` newly
  exported. Pre-1.0, classifications are informational rather than a
  compatibility promise; the promise question returns at the 1.0 release.
- **Practitioner polish** — neutral `data` kind for brms data nodes;
  `prop_zero` PPC statistic; PPC docs state p-values are conservative
  tripwires and `bg_plot()` graphics are primary.

## 1. CRAN dependency path

Unchanged: blocked on publishing `dagriculture` to CRAN, then removing the
`Remotes:` entry here. External coordination; order recorded in ROADMAP.md.

## 2. Optional SBC grading upgrade

Unchanged: a future upgrade could replace scalar chi-squared grading with
the simultaneous ECDF confidence-band method of Säilynoja, Bürkner, and
Vehtari (2022). Design sketch: `method = c("chi_squared", "ecdf")` on the
grader, bands computed over the ESS-bounded common rank support, same
`graded`/ungraded contract, band-violation locations in metadata for
plotting. Not a correctness blocker.

## 3. Longer-term protocol adapter

Unchanged: a targets adapter remains the most useful experiment for showing
the obligation/decision protocol is separable from the graph runtime. Keep
as an experiment until a second real consumer exists; recorded in
ROADMAP.md.

## 4. New follow-ups from this round

- The `compare` executor's `comparison_table` carries no model identity
  (rows are positional). Carry input node labels (or at least node ids)
  into the table so readers and reports can map rows to models.
- Backend registration order matters: activating workflow packs before
  `bg_use_cmdstanr()` leaves generic structural contracts active (the
  getting-started vignette now registers the backend first and says so).
  Consider making pack activation re-resolve contracts when backends
  register later.
- brms fits cannot feed the shared `ppc`/`loo` node kinds (no `yrep`/
  `log_lik` in brmsfit draws). Either special-case brmsfit in the shared
  executors (posterior_predict/log_lik in R) or document the limitation
  prominently.
- Stray compiled binaries `inst/stan/roaches_{poisson,negbinomial,zinb}`
  (from an exploratory run) are excluded via `.Rbuildignore` but should be
  deleted from the working tree.
- Archive root `PLAN.md` after the implementation branch is merged.
