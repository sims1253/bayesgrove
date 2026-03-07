
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesGrove

<!-- badges: start -->

[![R-CMD-check](https://github.com/sims1253/bayesgrove/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/bayesgrove/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**BayesGrove** is a graph-based Bayesian workflow engine for R. It helps
you design, track, and document your statistical analyses by completely
separating the *execution graph* (the code that runs) from the *decision
provenance* (why you ran it).

Built on the purely functional `dagriculture` graph library, BayesGrove
allows you to explicitly encode modeling decisions, enforce workflow
checks, seamlessly cache intermediate computations like MCMC fits, and
branch models asynchronously—all without losing the rationale behind
your choices.

## Why BayesGrove?

Modern Bayesian analysis requires iterative model building, checking,
and revision. Typically, this process gets lost in unstructured scripts
or implicit notebook states.

BayesGrove solves this by giving you:

1.  **Explicit Provenance**: Record qualitative rationales (e.g., “Why
    did we use a heavy-tailed prior?”) directly into the workflow using
    structural gates.
2.  **Intent-Based Caching**: Skip redundant computations. If an
    upstream data step doesn’t change semantically, your costly
    `cmdstanr` or `brms` fits won’t rerun.
3.  **Workflow Obligations**: The built-in protocol engine automatically
    flags divergent models or diagnostic warnings (like divergent
    transitions or high $\hat{R}$), pausing downstream comparisons until
    they are reviewed.
4.  **Reproducible Handoffs**: Export your entire decision tree and
    causal graph into a reproducible bundle or markdown report with one
    command.

## Installation

You can install the development version of BayesGrove (which includes
its `dagriculture` foundation) via [`pak`](https://pak.r-lib.org/):

``` r
# install.packages("pak")
pak::pkg_install("sims1253/bayesgrove")
```

## Example: Protocol-Driven Workflow

BayesGrove separates the execution graph from decision provenance. When
computations produce diagnostic warnings, the protocol engine
automatically creates blocking obligations and holds downstream work
until you resolve them.

``` r
library(bayesgrove)

# 1. Initialize a workflow workspace
project_path <- file.path(tempdir(), "bayesgrove-intro")
handle <- bg_init(
  path = project_path,
  project_name = "hierarchical_analysis",
  workflow_packs = list("bayesguide.default_bayesian")
)

# 2. Register executors that return diagnostic summaries
bg_register_node_kind(handle, "data_prep", executor = function(node, inputs) {
  data.frame(group = rep(1:5, each = 10), y = rnorm(50))
})

bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
  # Mock fit that returns warning diagnostics (e.g., divergent transitions)
  list(
    status = "fit_completed",
    summaries = list(
      list(
        summary_kind = "hmc_diagnostics",
        severity = "warning",
        passed = FALSE,
        metrics = list(divergences = 15, rhat_max = 1.02)
      )
    )
  )
})

bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
  list(plot = "ppc_density_plot")
})

# 3. Construct the graph
n_data <- bg_add_node(handle, "data_prep", label = "Load Data")
n_fit <- bg_add_node(handle, "fit", label = "Hierarchical Fit", inputs = n_data)
n_ppc <- bg_add_node(handle, "ppc", label = "Posterior Check", inputs = n_fit)
```

Run the workflow. The fit completes but produces warning diagnostics:

``` r
bg_run(handle, mode = "sync")
#> Starting run "run_d16d7bd3" with 1 node to execute.
#> Running node "node_2bd9c9d1"...
#> Running node "node_ca6c40c2"...
#> <bg_run_handle> run_d16d7bd3
#> 
#> • Status: blocked
#> 
#> • Mode: sync
#> 
#> • Executed Nodes: 2
```

The protocol engine detects the warning and creates a blocking
obligation. Downstream work (the PPC) is held:

``` r
status <- bg_status(handle)
cat(sprintf("Workflow State: %s\n", status$workflow_state))
#> Workflow State: blocked
```

Inspect active obligations and suggested actions:

``` r
actions <- bg_next_actions(handle)
cat(sprintf("Blocking obligations: %d\n", length(actions$obligations)))
#> Blocking obligations: 1
if (length(actions$actions) > 0) {
  cat(sprintf("First suggested action: %s\n", actions$actions[[1]]$title))
}
#> First suggested action: Record a computation review
```

To resolve the issue, you can branch and modify the fit with a
non-centered parametrization. The interactive REPL provides a guided
interface for this workflow:

``` r
bg_repl(handle)
```

## Interactive REPL

The REPL provides a guided loop for working through obligations:

- `status` - Show workflow state (blocked, idle, etc.)
- `guide` - See active obligations and suggested actions
- `actions` - List detailed actions with payloads
- `do <n>` - Execute action \#n (branch and modify, record decision,
  etc.)
- `run` - Execute ready nodes
- `nodes` - View the execution graph with states
- `result <label>` - Inspect cached results and diagnostics

<img src="tools/demo/repl-workflow/repl-workflow.gif" width="100%" alt="BayesGrove Interactive REPL Demo"/>

The demo shows the full guided loop: a fit with divergent transitions
triggers a blocking obligation, you branch and modify to use
non-centered parametrization, rerun, and the workflow advances.
