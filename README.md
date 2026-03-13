
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesgrove

<!-- badges: start -->

[![R-CMD-check](https://github.com/sims1253/bayesgrove/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/bayesgrove/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sims1253/bayesgrove/graph/badge.svg)](https://app.codecov.io/gh/sims1253/bayesgrove)
[![lint](https://github.com/sims1253/bayesgrove/actions/workflows/lint.yaml/badge.svg)](https://github.com/sims1253/bayesgrove/actions/workflows/lint.yaml)
[![format-check](https://github.com/sims1253/bayesgrove/actions/workflows/format-check.yaml/badge.svg)](https://github.com/sims1253/bayesgrove/actions/workflows/format-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Bayesian analysis is iterative. Models get fit, diagnostics reviewed,
priors revised, alternatives branched and compared. Most of that process
disappears into scripts, scratch notes, and memory. You can rerun the
code, but you cannot recover why it changed.

**bayesgrove** keeps the execution graph and the decision record
together. Executor summaries drive workflow obligations. Diagnostic
warnings block downstream work, fit criticism opens repair branches, and
clean candidates require a formal comparison before a branch can be
accepted.

Decisions are recorded inside the workflow. When you change a prior,
reparametrize a model, or exclude data, the rationale lives alongside
the computation. Each node carries a SHA-256 fingerprint of its inputs
and parameters, so bayesgrove reruns only the affected subtree when
something changes.

The review protocol is enforced rather than advisory. HMC warning
diagnostics trigger a `review_computation_validity` obligation.
Branch-scoped fit warnings trigger `review_fit_criticism`. When two
candidates are clean, bayesgrove requires an explicit comparison and an
accept-or-reject disposition for each branch before the loop closes.

You can fork from any node without discarding the baseline. Branch
goals, decisions, and diagnostics stay attached, so the full development
history is recoverable. Workflows export as portable bundles or
structured markdown reports.

The default review loop is intentionally small. Optional packs add
workflow planning, PAD taxonomy, predictive checks, SBC, LOO-PIT
calibration, stacking-aware model selection, Stan diagnostics, and
DAG-constrained causal workflows without touching the graph runtime.

## Installation

``` r
# install.packages("pak")
pak::pkg_install("sims1253/bayesgrove")
```

## API stability

The package has a stable core, an experimental layer, and a smaller set
of internal-but-exported helpers. Use `bg_api_boundary()` to inspect
that boundary from R, including the smaller remote IPC contract exposed
through the experimental `bg_serve()` server.

Async execution is narrow on purpose. Synchronous execution is always
available, and background execution relies on `mirai`.

``` r
boundary <- bg_api_boundary()
subset(boundary, remote_accessible)

server <- bg_serve(handle)
server$protocol_version
server$url
server$stop()
```

## Guided terminal client

Use `bg_repl(handle)` when you want the package to lead you through the
review loop instead of managing low-level calls by hand. The REPL opens
with a dashboard showing workflow state, active obligations, held nodes,
recent decisions, branch-scoped gates, and branch lineage.

The main guided commands are:

- `dashboard` to refresh the operator view
- `next` to preview and optionally execute the top recommended action
- `do <n>` to execute a specific suggested action by number
- `lineage` to inspect the current branch with its ancestors
- `export md report.md` or `export html report.html` to write a workflow
  report

## Optional workflow packs

Projects now start empty by default. Opt into the built-in review stack
when you want it, and persist those choices directly in the project
config:

``` r
handle <- bg_init(
  path = file.path(tempdir(), "bg-advanced"),
  project_name = "advanced_workflow"
)

bg_use_workflow_packs(
  handle,
  c(
    "bayesguide.default_bayesian",
    "bayesgrove.process_guidance",
    "bayesgrove.model_taxonomy",
    "bayesgrove.prior_workflow",
    "bayesgrove.model_checks",
    "bayesgrove.model_selection",
    "bayesgrove.stan_workflow",
    "bayesgrove.causal_dagitty"
  )
)
```

If you want the starter setup in one step, call
`bg_use_default_workflow(handle)`. That helper enables
`bayesguide.default_bayesian` and registers the built-in starter node
kinds (`source`, `fit`, `ppc`, and `compare`). Additional node kinds you
register with `bg_register_node_kind()` are also persisted, so reopening
the project restores the runtime registry automatically for
self-contained executor functions.

- `bayesgrove.process_guidance` adds workflow preflight, iteration
  planning, and out-of-sample stability review.
- `bayesgrove.model_taxonomy` asks for explicit PAD classification and
  utility trade-off review.
- `bayesgrove.prior_workflow` asks for prior rationale and review of
  fresh `prior_predictive_check` summaries.
- `bayesgrove.model_checks` adds posterior predictive review, and on
  branch-scoped `latent_inference` goals it also asks for SBC and can
  review fresh `loo_pit_calibration` summaries.
- `bayesgrove.model_selection` reviews fresh comparison evidence from
  `compare` or `comparison` nodes, including `comparison_results` and
  optional `stacking_weights`.
- `bayesgrove.stan_workflow` bundles the default loop, process guidance,
  taxonomy, prior/model checks, model selection, and Stan-specific
  diagnostic plus projection-predictive review.
- `bayesgrove.causal_dagitty` adds DAG-backed adjustment review,
  implication review, and causal selection contracts that can lock
  required terms, exclude forbidden controls, and constrain formula or
  projection-based selection.

All packs read plain-data summaries returned by your executors, so you
can adopt them incrementally without changing the graph runtime. See
`vignette("extensions", package = "bayesgrove")` for a guide to writing
node sets, domain modules, and backend plugins around that contract.

A causal branch, for example, can emit a `causal_selection_contract`
summary with `required_terms`, `forbidden_terms`, and
`ranked_candidate_terms`. Once reviewed, `bayesgrove.causal_dagitty`
blocks downstream work until formulas respect that contract.
`bayesgrove.stan_workflow` surfaces the same constraints inside
projection-predictive review.

## A minimal example

The executor is a plain R function. You write the same `cmdstanr` code
you already use and return a `summaries` list alongside the fit.
bayesgrove reads those summaries to derive workflow obligations, cache
validity, and holds. DAG management, fingerprinting, and branching stay
out of the way until you need them. Future versions will include
built-in `cmdstanr` and `brms` executors; this example builds one from
scratch so the contract stays visible.

You need a working `cmdstanr` installation to run this example. It uses
the eight-schools model in the centered parametrization, which reliably
produces divergent transitions and is the standard motivation for
non-centered reparametrization.

``` stan
// eight_schools_centered.stan (saved as `stan_file` path)
data {
  int<lower=1> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] theta;     // centered — causes funnel geometry
}
model {
  mu    ~ normal(0, 5);
  tau   ~ normal(0, 1);
  theta ~ normal(mu, tau);
  y     ~ normal(theta, sigma);
}
```

``` r
library(bayesgrove)

handle <- bg_init(
  path           = file.path(tempdir(), "bg-readme"),
  project_name   = "eight_schools"
)

bg_use_default_workflow(handle)

bg_register_node_kind(handle, "data", executor = function(node, inputs) {
  list(
    J     = 8L,
    y     = c(28, 8, -3, 7, -1, 1, 18, 12),
    sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
  )
})

bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
  mod <- cmdstanr::cmdstan_model(node$params$stan_file, quiet = TRUE)
  fit <- mod$sample(
    data            = inputs[[1]],
    chains          = 2,
    parallel_chains = 2,
    iter_warmup     = 500,
    iter_sampling   = 500,
    seed            = 42L,
    refresh         = 0
  )

  diag     <- fit$diagnostic_summary(quiet = TRUE)
  rhat_max <- max(fit$summary()$rhat, na.rm = TRUE)
  n_div    <- sum(diag$num_divergent)
  passed   <- n_div == 0 && rhat_max < 1.01

  list(
    result    = fit,   # stored in the content-addressed cache
    summaries = list(list(
      summary_kind = "hmc_diagnostics",
      passed       = passed,
      severity     = if (passed) "ok" else "warning",
      metrics      = list(divergences = n_div, rhat_max = rhat_max)
    ))
  )
})

bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
  list(plot = "posterior_predictive_check")
})

n_data <- bg_add_node(handle, "data", label = "Eight schools data")
n_fit  <- bg_add_node(handle, "fit",  label = "Centered fit",
                       inputs = n_data,
                       params = list(stan_file = stan_file))
n_ppc  <- bg_add_node(handle, "ppc",  label = "Posterior check", inputs = n_fit)
```

``` r
bg_run(handle, targets = n_fit, mode = "sync")
```

The funnel geometry of the centered parametrization produces divergent
transitions. bayesgrove reads the `hmc_diagnostics` summary returned by
the executor and surfaces a blocking obligation automatically. We can
inspect the summaries that triggered the hold:

``` r
summaries <- bg_read_summaries(handle)
summaries[[1]]$metrics
#> $divergences
#> [1] 291
#> 
#> $rhat_max
#> [1] 1.3927
```

The posterior check is held from executing:

``` r
guide <- bg_next_actions(handle, scope = "project")

vapply(guide$obligations, `[[`, character(1), "kind")
#>                  obl_9daa323c 
#> "review_computation_validity"

held <- bg_plan(handle, external_holds = guide$metadata$external_holds)
length(held$external_blocked)
#> [1] 1
```

To resolve this, you record the criticism decision, branch from the
problematic fit, point the branch at a non-centered Stan file, and
rerun. bayesgrove tracks the lineage and keeps the original fit and its
diagnostic history intact:

``` r
# The protocol surfaced an obligation on the project because this is the main branch
bg_record_decision(
  handle,
  scope     = "project",
  prompt    = "How should the centered fit be handled?",
  choice    = "reparametrize_to_non_centered",
  rationale = "Divergences indicate the funnel geometry is poorly explored. Switch to non-centered.",
  kind      = "fit_criticism"
)

branch <- bg_branch_with_continuation(
  project            = handle,
  node_id            = n_fit,
  label              = "Non-centered reparametrization",
  continuation_kinds = "ppc"
)

# `ncp_stan_file` is the path to your non-centered Stan model
bg_update_node(handle, branch$branch$root_node_id,
               params = list(stan_file = ncp_stan_file))
bg_run(handle, targets = branch$branch$root_node_id, mode = "sync")
```

Once two clean candidates exist, the protocol requires an explicit
comparison and an accept/reject disposition for each branch before the
loop closes:

``` r
# We can now view the full lineage of both branches in the execution graph:
print(bg_read_graph(handle))
#> ── Execution Graph ──
#> 
#> └── Eight schools data <data> [new]
#>     ├── Centered fit <fit> [new]
#>     │   └── Posterior check <ppc> [new]
#>     └── Non-centered reparametrization <fit> [new]
#>         └── Posterior check (from branch) <ppc> [new]

# The protocol surfaces an explicit comparison step
bg_record_decision(
  handle,
  scope     = "project",
  prompt    = "Which branch should anchor the final report?",
  choice    = "prefer_non_centered_branch",
  rationale = "The non-centered branch resolves the divergences.",
  kind      = "model_comparison"
)

# Accept the non-centered branch
bg_record_decision(
  handle,
  scope     = branch$branch$branch_id,
  prompt    = "Should this branch be accepted?",
  choice    = "accept",
  rationale = "This is the clean branch preferred by the comparison step.",
  kind      = "branch_disposition"
)
```

The `guided-review-loop` vignette walks this full cycle end-to-end with
deterministic executors, covering branch lineage, stale evidence
tracking, comparison, and report export.

## Conceptual structure

bayesgrove isolates three layers:

| Layer | Contents |
|----|----|
| **Execution DAG** | Nodes (data, fit, check, compare), edges, cached results, content fingerprints |
| **Decision log** | Gates, explicit rationales, choices, timestamps, branch lineage |
| **Workflow protocol** | Obligations derived from executor summaries; external holds passed to the planner |

The execution DAG relies on
[`dagriculture`](https://github.com/sims1253/dagriculture), a purely
functional graph library. Workflow semantics—holds, branch scope,
obligations, decision provenance—are bayesgrove’s layer on top of the
graph primitives.

## Vignettes

| Vignette | What it covers |
|----|----|
| `getting-started` | Full project lifecycle: gates, caching, branching, comparison, and disposition |
| `guided-review-loop` | Complete criticism-and-repair cycle with deterministic mock executors |
| `simulation-study` | Simulation-based workflow with confounded treatment and explicit gate decisions |
| `extensions` | How to build node sets, backend plugins, domain modules, and summary-driven extensions |
| `dagriculture-boundary` | How the graph primitives relate to workflow semantics |

``` r
vignette("guided-review-loop", package = "bayesgrove")
```

## Background

bayesgrove implements the iterative model-building perspective described
in Gelman et al. (2020, *Bayesian Workflow*,
[arXiv:2011.01808](https://arxiv.org/abs/2011.01808)) and the
simulation-based calibration workflow of Talts et al. (2018,
[arXiv:1804.06788](https://arxiv.org/abs/1804.06788)). The package
encodes this perspective in the runtime. It treats the rationale for
each analysis step as an integral part of the workflow.
