# bayesgrove

Bayesian analysis is iterative. Models get fit, diagnostics reviewed,
priors revised, alternatives branched and compared. Most of that process
disappears into scripts, scratch notes, and memory. You can rerun the
code, but you cannot recover *why* it changed.

**bayesgrove** keeps the execution graph and the decision record
together, and it enforces the review loop instead of merely suggesting
it:

- **Executors emit evidence.** Built-in cmdstanr and brms executors
  compute HMC, LOO, and posterior-predictive diagnostics themselves and
  return them as typed summaries. You can also register your own
  executors.
- **Evidence creates obligations.** A fit with divergences raises a
  blocking `review_computation_validity` obligation; downstream nodes
  are held until you record a reviewed decision with a rationale.
- **Decisions are part of the workflow.** Prior changes,
  reparametrizations, data exclusions, comparisons, and branch
  accept/reject calls are recorded next to the computation they justify,
  with citations into the workflow literature.
- **Everything is cached and reproducible.** Each node carries a SHA-256
  fingerprint over its params, inputs, executor, Stan program contents,
  and environment, so only the affected subtree reruns when something
  changes. Branching from any node preserves the baseline and its full
  history.

The result is a workflow you can hand to a collaborator, a reviewer, or
a grader: the exported report shows not just what was computed, but
which diagnostics fired, what was decided about them, and why.

## Installation

``` r

# install.packages("pak")
pak::pkg_install("sims1253/bayesgrove")
```

For real model fitting you also need
[cmdstanr](https://mc-stan.org/cmdstanr/) (plus CmdStan) or
[brms](https://paulbuerkner.com/brms/).

## The review loop in five minutes

Eight schools, centered parametrization: the canonical model that
samples poorly and demands a reparametrization. bayesgrove’s job is to
make sure the “samples poorly” part cannot be silently ignored.

``` r

library(bayesgrove)

handle <- bg_init(
  path = file.path(tempdir(), "bg-readme"),
  project_name = "eight_schools",
  workflow_packs = list("bayesgrove.default_bayesian")
)
bg_use_cmdstanr(handle)
```

The built-in cmdstanr backend registers node kinds (`stan_data`,
`cmdstanr_fit`, `loo`, `compare`, `ppc`, …) whose executors compute the
diagnostics for you. Build the graph:

``` r

schools_data <- list(
  J = 8L,
  y = c(28, 8, -3, 7, -1, 1, 18, 12),
  sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
)

n_data <- bg_add_node(handle, "stan_data", label = "Schools data")
bg_set_node_data(handle, n_data, schools_data)

# `stan_file` points at the centered eight-schools program.
n_fit <- bg_add_node(
  handle,
  "cmdstanr_fit",
  label = "Centered fit",
  inputs = n_data,
  params = list(stan_file = stan_file, chains = 4, seed = 42)
)
```

``` r

bg_run(handle, targets = n_fit)
```

The funnel geometry produces divergent transitions. The executor emits
an `hmc_diagnostics` summary, the protocol converts it into a blocking
obligation, and downstream work is held:

``` r

guide <- bg_next_actions(handle)
vapply(guide$obligations, `[[`, character(1), "kind")
#>                  obl_bff2a185
#> "review_computation_validity"

bg_read_summaries(handle)[[1]]$metrics[c("divergences", "max_rhat")]
#> $divergences
#> [1] 291
#>
#> $max_rhat
#> [1] 1.39
```

You cannot proceed past this by rerunning harder. You resolve it by
recording the criticism and branching to a repair — and the original
fit, its diagnostics, and your rationale all stay in the record:

``` r

bg_record_decision(
  handle,
  scope = "project",
  prompt = "How should the centered fit be handled?",
  choice = "reparametrize_to_non_centered",
  rationale = "Divergences indicate the funnel is poorly explored.",
  kind = "fit_criticism"
)

branch <- bg_branch(handle, n_fit, label = "Non-centered")
bg_update_node(
  handle,
  branch$root_node_id,
  params = list(stan_file = ncp_stan_file)
)
bg_run(handle, targets = branch$root_node_id)
```

Once two candidates are clean, the protocol requires an explicit
comparison decision and an accept-or-reject disposition for each branch
before the loop closes. The full lineage remains visible:

``` r

print(bg_read_graph(handle))
#> ── Execution Graph ──
#>
#> └── Schools data <stan_data> [cached]
#>     ├── Centered fit <cmdstanr_fit> [cached]
#>     └── Non-centered <cmdstanr_fit> [cached]
```

Finally, export the whole trail — graph, diagnostics, decisions,
rationales — as a report or a portable bundle:

``` r

bg_export_report(handle, format = "md", path = "eight-schools-report.md")
```

The `eight-schools` vignette walks this exact loop with real captured
output; the `guided-review-loop` vignette covers the complete
criticism–repair–compare–accept cycle.

## Guided terminal client

`bg_repl(handle)` drives the same loop interactively: a dashboard of
workflow state, active obligations, held nodes, recent decisions, and
branch lineage, with [`next`](https://rdrr.io/r/base/Control.html) /
`do <n>` to execute the top recommended action and `export md report.md`
to write a report from the session.

## Workflow packs

Projects start empty. The built-in review loop and its extensions are
opt-in packs, persisted in the project config:

``` r

bg_use_workflow_packs(
  handle,
  c(
    "bayesgrove.default_bayesian", # computation review, criticism, comparison
    "bayesgrove.process_guidance", # preflight, iteration planning
    "bayesgrove.model_taxonomy", # PAD classification, utility trade-offs
    "bayesgrove.prior_workflow", # prior rationale, prior predictive review
    "bayesgrove.model_checks", # posterior predictive, SBC, LOO-PIT review
    "bayesgrove.model_selection", # comparison evidence, stacking weights
    "bayesgrove.stan_workflow", # bundle of the above + Stan-specific review
    "bayesgrove.causal_dagitty" # DAG-backed adjustment and selection contracts
  )
)
```

Every obligation a pack raises carries an explanation and citations into
the workflow literature (Gelman et al. 2020; Vehtari et al. 2021; Talts
et al. 2018; and more), so the guidance doubles as teaching material.

All packs read plain-data summaries. If you register your own executors,
you emit the same summary kinds and the protocol treats your evidence
like the built-ins’. Opening a project never runs project-supplied code;
`bg_restore_executors(handle, trust = TRUE)` is the explicit opt-in. See
[`vignette("extensions", package = "bayesgrove")`](https://sims1253.github.io/bayesgrove/articles/extensions.md).

## How it relates to targets and workflowr

[`targets`](https://docs.ropensci.org/targets/) caches computation but
records no judgment;
[`workflowr`](https://workflowr.github.io/workflowr/) versions notebooks
but does not read diagnostics. bayesgrove’s claim is the enforced link
between the two: diagnostic evidence creates obligations, obligations
hold downstream computation, and only recorded decisions release them.
Use targets for generic pipelines; use bayesgrove when the *review
process* is the thing you need to be reproducible.

## Conceptual structure

| Layer | Contents |
|----|----|
| **Execution DAG** | Nodes, edges, cached results, content fingerprints |
| **Decision log** | Gates, decisions with rationales, branch lineage |
| **Workflow protocol** | Obligations derived from executor summaries; holds passed to the planner |

The structural graph layer is
[`dagriculture`](https://github.com/sims1253/dagriculture), a purely
functional graph library. Workflow semantics — holds, branch scope,
obligations, decision provenance — are bayesgrove’s layer on top.

The package makes its public surface explicit:
[`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md)
classifies every export as stable, experimental, or internal.

## Vignettes

| Vignette | What it covers |
|----|----|
| `concepts` | The mental model: node -\> summary -\> obligation -\> decision -\> hold, scopes, freshness, gates vs obligations |
| `getting-started` | Full project lifecycle: gates, caching, branching, comparison, disposition |
| `eight-schools` | The real cmdstanr loop shown above, with captured output |
| `guided-review-loop` | Complete criticism-and-repair cycle with deterministic executors |
| `simulation-study` | Simulation-based workflow with confounded treatment and gate decisions |
| `primed-priors-case-studies` | Mapping the primed-priors SBC case studies onto workflow graphs |
| `extensions` | Writing node sets, backend plugins, and summary-driven extensions |
| `dagriculture-boundary` | How the graph primitives relate to workflow semantics |

The [pkgdown site](https://sims1253.github.io/bayesgrove/) additionally
carries two articles: *Reproducible research with bayesgrove* (what a
bundle contains, how the decision log and summaries serve as a
machine-readable audit trail, running SBC at scale) and *Teaching with
bayesgrove* (running a course assignment on the decision log, with a
grading checklist).

## Background

bayesgrove encodes the iterative model-building perspective of Gelman et
al. (2020, *Bayesian Workflow*,
[arXiv:2011.01808](https://arxiv.org/abs/2011.01808)) and the
simulation-based calibration workflow of Talts et al. (2018,
[arXiv:1804.06788](https://arxiv.org/abs/1804.06788)) directly in the
runtime: the rationale for each analysis step is treated as part of the
analysis itself.
