---
title: BayesGrove
---
<!-- README.md is generated from README.Rmd. Please edit that file -->



[![CRAN status](https://img.shields.io/cran/v/release/bayesgrove)](https://cran.r-project.org/package/bayesgrove)
[![R-CMD-check](https://github.com/mscholz/bayesgrove/actions/workflows/R-CMD-check/badge.svg)](https://github.com/mscholz/bayesgrove/actions/workflows/R-CMD-check)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# BayesGrove: Graph-Based Bayesian Workflow Scaffolding

**BayesGrove** is an R-native, graph-based Bayesian workflow orchestrator. It helps you structure, track, and document your Bayesian analyses by separating the execution DAG (directed acyclic graph) from a rich decision-provenance layer.

Built on top of the pure functional graph engine [`dagriculture`](https://github.com/sims1253/dagriculture), BayesGrove provides explicit decision capture, branching, and reproducible workflow management.

## Key Features

- **Graph-Based Workflow**: Structure your analysis as a reproducible DAG of explicitly typed steps.
- **Decision Tracking (Provenance)**: Record all modeling choices, rationales, and literature references using structural "gates".
- **Deterministic Fingerprinting**: Precise, intent-based cache invalidation for partial reruns.
- **Workflow Guidance**: Derive obligations and suggested next actions from lightweight summaries via `bg_next_actions()`.
- **Reference-Semantic Handles**: Safely mutate and manage your workflow state using explicit S7-based project handles.

## Installation

```r
# Install dagriculture foundation first
# devtools::install_github("sims1253/dagriculture")

# Install BayesGrove from GitHub
# devtools::install_github("mscholz/bayesgrove")
```

## Quickstart

This minimal example shows how to initialize a project, build a simple graph, and execute it using the synchronous runner.


``` r
library(bayesgrove)

# Initialize a project directory
project_path <- file.path(tempdir(), "bayesgrove-demo")
unlink(project_path, recursive = TRUE)

handle <- bg_init(
  project_path,
  project_name = "demo-project",
  workflow_packs = list("bayesguide.default_bayesian")
)

# Register simple mock executors for the demo
bg_register_node_kind(handle, "data", executor = function(node, inputs) {
  message("Loading data...")
  return(data.frame(x = 1:10))
})

bg_register_node_kind(handle, "model", executor = function(node, inputs) {
  message("Fitting model...")
  list(
    result = "mock_fit_result",
    summaries = list(list(
      summary_kind = "optimizer_diagnostics",
      passed = FALSE,
      severity = "warning",
      metrics = list(max_gradient = 0.1)
    ))
  )
})

bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
  sprintf("Compared downstream result: %s", inputs[[1]])
})

# Build the workflow graph
n_data <- bg_add_node(handle, kind = "data", label = "Raw Data")
n_model <- bg_add_node(handle, kind = "model", label = "Baseline Model", inputs = n_data)
n_compare <- bg_add_node(
  handle,
  kind = "compare",
  label = "Diagnostic Comparison",
  inputs = n_model
)

# Create a decision gate
gate <- bg_add_gate(
  project = handle,
  from = n_data,
  to = n_model,
  prompt = "Does the data look ready for modeling?",
  options = c("yes", "no")
)

# Observe that the graph is blocked by the pending gate
plan <- bg_plan(handle)
print(plan$blocked)
#> $node_252774bb
#> [1] "gate"
#> 
#> $node_e8780551
#> [1] "upstream_blocked"
print(bg_pending_gates(handle))
#> $gate_4687b6e0
#> $gate_4687b6e0$id
#> [1] "gate_4687b6e0"
#> 
#> $gate_4687b6e0$edge_id
#> [1] "edge_c7a8a3a9"
#> 
#> $gate_4687b6e0$prompt
#> [1] "Does the data look ready for modeling?"
#> 
#> $gate_4687b6e0$options
#> $gate_4687b6e0$options[[1]]
#> [1] "yes"
#> 
#> $gate_4687b6e0$options[[2]]
#> [1] "no"
#> 
#> 
#> $gate_4687b6e0$refs
#> list()
#> 
#> $gate_4687b6e0$created_at
#> [1] "2026-03-06T19:36:06Z"
#> 
#> $gate_4687b6e0$metadata
#> list()
#> 
#> $gate_4687b6e0$from_node_id
#> [1] "node_3f96184c"
#> 
#> $gate_4687b6e0$to_node_id
#> [1] "node_252774bb"
#> 
#> $gate_4687b6e0$from_label
#> [1] "Raw Data"
#> 
#> $gate_4687b6e0$to_label
#> [1] "Baseline Model"

# Answer the gate explicitly
bg_answer_gate(
  project = handle,
  id = gate$id,
  choice = "yes",
  rationale = "No missing values found, ready to fit."
)
#> $decision_id
#> [1] "dec_741125fa"
#> 
#> $scope
#> [1] "gate:gate_4687b6e0"
#> 
#> $kind
#> [1] "gate_answer"
#> 
#> $prompt
#> [1] "Does the data look ready for modeling?"
#> 
#> $choice
#> [1] "yes"
#> 
#> $alternatives
#> [1] "no"
#> 
#> $rationale
#> [1] "No missing values found, ready to fit."
#> 
#> $refs
#> list()
#> 
#> $evidence
#> character(0)
#> 
#> $status
#> [1] "active"
#> 
#> $created_at
#> [1] "2026-03-06T19:36:06Z"
#> 
#> $supersedes
#> NULL
#> 
#> $metadata
#> $metadata$gate_id
#> [1] "gate_4687b6e0"
#> 
#> $metadata$edge_id
#> [1] "edge_c7a8a3a9"
#> 
#> $metadata$from_node_id
#> [1] "node_3f96184c"
#> 
#> $metadata$to_node_id
#> [1] "node_252774bb"
#> 
#> $metadata$options
#> [1] "yes" "no" 
#> 
#> $metadata$gate_metadata
#> list()

# Run the workflow
bg_run(handle, targets = n_model, mode = "sync")
#> Starting run "run_a963272f" with 1 node to execute.
#> Running node "node_3f96184c"...
#> Loading data...
#> 
#> Running node "node_252774bb"...
#> Fitting model...
#> $run_id
#> [1] "run_a963272f"
#> 
#> $status
#> [1] "succeeded"
#> 
#> $mode
#> [1] "sync"
#> 
#> $targets
#> [1] "node_252774bb"
#> 
#> $job_ids
#> character(0)
#> 
#> $submitted_at
#> [1] "2026-03-06T19:36:06Z"
#> 
#> $started_at
#> [1] "2026-03-06T19:36:06Z"
#> 
#> $finished_at
#> [1] "2026-03-06T19:36:06Z"
#> 
#> $summary
#> $summary$total_executed
#> [1] 2
#> 
#> 
#> $error
#> NULL
#> 
#> $metadata
#> list()

# Ask the workflow protocol what needs attention next
next_steps <- bg_next_actions(handle)
next_steps$obligations[[1]]$kind
#> [1] "review_computation_validity"

# Feed blocking obligations back into planning as external holds
held_plan <- bg_plan(
  handle,
  external_holds = next_steps$metadata$external_holds
)
held_plan$external_blocked
#> $node_e8780551
#> [1] "Review computation validity"
```

The final `bg_plan()` call is the key distinction: `held_plan$blocked` still
contains only structural blockers, while `held_plan$external_blocked` captures
workflow holds derived from summaries and obligations.

## Vignettes

For a detailed introduction, see our vignettes:
- `vignette("getting-started", package = "bayesgrove")`

## License

MIT © Maximilian Scholz
