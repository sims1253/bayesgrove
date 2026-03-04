---
title: Readme
---
<!-- README.md is generated from README.Rmd. Please edit that file -->



# Graph-Based Bayesian Workflow Prototype for R

<!-- badges: start -->
<!-- badges: end -->

This repository contains the current MVP package for an R-native, graph-based
Bayesian workflow tool.

What already works:

- project initialization with persistent on-disk state
- typed graph construction for data, transforms, model specs, compile, fit,
  diagnostic, and compare nodes
- decision gates that block downstream execution until a rationale-backed choice
  is recorded
- workflow controls for pausing, resuming, branching, and backtracking
- a minimal text REPL that wraps status, decisions, and workflow commands
- deterministic fingerprints and content-addressed artifact storage
- shareable workflow bundles with recipe-first data export
- synchronous and async execution with `mirai`/`callr` backends plus a `cmdstanr` compile/fit split
- best-effort cancellation for active async runs
- extension registries for backends, env providers, suggesters, and custom node
  kinds
- direct comparison helpers and a basic radar plot for comparison summaries
- manual cache garbage collection for orphaned artifacts
- `bg_status()` summaries plus `bg_jobs()` inspection for async work and on-disk project footprint
- automatic metadata reconciliation on `bg_open()` after interrupted writes or missing cache artifacts
- workflow export plus focused walkthrough vignettes

The package name, public repository URL, and release/distribution path are
still provisional, so this README avoids hard-coding installation instructions
that are likely to change.

## Current development workflow

For now, use the package from the local source tree while the package name and
publication target are still being finalized.

- Build docs with `devtools::document()`
- Run package checks with `devtools::check()`
- Render this file with `devtools::build_readme()`
- Build the site locally with `pkgdown::build_site()`

## Minimal example

This example shows the lightweight graph and decision-gate workflow that is
already implemented.


``` r
project_root <- file.path(tempdir(), "bayesgrove-readme")
unlink(project_root, recursive = TRUE, force = TRUE)
dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

bg_init(project_root, project_name = "readme-demo")

source_node <- bg_add_node(
  kind = "data_source",
  label = "Raw value",
  params = list(value = 10L)
)

transform_node <- bg_add_node(
  kind = "transform",
  label = "Increment value",
  params = list(fn = function(x) x + 1L)
)

bg_connect(source_node, transform_node, edge_type = "data")
#> [1] "edge_0001"
gate_id <- bg_add_decision_gate(source_node, transform_node, "Allow increment?")

bg_run()
#> $run_id
#> [1] "run_20260302_213207_001"
#> 
#> $executed
#>   node_0001   node_0002 
#> "succeeded"     "ready"
bg_pending_decisions()
#> $gate_001
#> $gate_001$gate_id
#> [1] "gate_001"
#> 
#> $gate_001$decision_id
#> [1] "dec_00001"
#> 
#> $gate_001$prompt
#> [1] "Allow increment?"
#> 
#> $gate_001$from_node
#> [1] "node_0001"
#> 
#> $gate_001$to_node
#> [1] "node_0002"
#> 
#> $gate_001$status
#> [1] "pending"
#> 
#> $gate_001$created_at
#> [1] "2026-03-02T21:32:07Z"

bg_decide(gate_id, choice = "yes", rationale = "Use the default branch.")
#> $decision_id
#> [1] "dec_00001"
#> 
#> $timestamp
#> [1] "2026-03-02T21:32:07Z"
#> 
#> $scope_node_id
#> [1] "node_0001"
#> 
#> $prompt
#> [1] "Allow increment?"
#> 
#> $choice
#> [1] "yes"
#> 
#> $alternatives
#> character(0)
#> 
#> $rationale
#> [1] "Use the default branch."
#> 
#> $references
#> list()
#> 
#> $status
#> [1] "resolved"
#> 
#> $resolved_at
#> [1] "2026-03-02T21:32:07Z"
bg_run()
#> $run_id
#> [1] "run_20260302_213207_002"
#> 
#> $executed
#>   node_0001   node_0002 
#> "succeeded" "succeeded"
bg_graph()
```

## REPL example

The package now also includes a minimal text REPL wrapper. In non-interactive
contexts, pass a scripted command vector.


``` r
repl_root <- file.path(tempdir(), "bayesgrove-readme-repl")
unlink(repl_root, recursive = TRUE, force = TRUE)
dir.create(repl_root, recursive = TRUE, showWarnings = FALSE)

bg_init(repl_root, project_name = "repl-demo")
bg_add_node(
  kind = "data_source",
  label = "Seed value",
  params = list(value = 6L)
)
#> [1] "node_0001"

repl_session <- bg_repl(
  commands = c("status", "next", "quit"),
  echo = FALSE
)

vapply(repl_session$history, `[[`, character(1), "action")
#> [1] "status" "next"   "quit"
bg_close()
```

## Workflow control example

The package also now supports lightweight workflow controls on top of the
current runtime.


``` r
workflow_root <- file.path(tempdir(), "bayesgrove-readme-workflow")
unlink(workflow_root, recursive = TRUE, force = TRUE)
dir.create(workflow_root, recursive = TRUE, showWarnings = FALSE)

bg_init(workflow_root, project_name = "workflow-demo")

seed_node <- bg_add_node(
  kind = "data_source",
  label = "Seed",
  params = list(value = 4L)
)

base_node <- bg_add_node(
  kind = "transform",
  label = "Add one",
  params = list(fn = function(x) x + 1L)
)

bg_connect(seed_node, base_node, edge_type = "data")
#> [1] "edge_0001"

pause_info <- bg_pause()
pause_info$state
#> [1] "paused"

resume_info <- bg_resume()
resume_info$workflow_state
#> [1] "idle"

branch_node <- bg_branch(base_node, label = "Multiply by ten")
bg_set_params(branch_node, list(fn = function(x) x * 10L))
#> $id
#> [1] "node_0003"
#> 
#> $kind
#> [1] "transform"
#> 
#> $label
#> [1] "Multiply by ten"
#> 
#> $version
#> [1] 1
#> 
#> $params
#> $params$fn
#> function (x) 
#> x * 10L
#> 
#> 
#> $depends_on
#> character(0)
#> 
#> $execution_state
#> [1] "stale"
#> 
#> $block_reason
#> [1] "none"
#> 
#> $fingerprint
#> NULL
#> 
#> $artifacts
#> character(0)
#> 
#> $created_at
#> [1] "2026-03-02T21:32:08Z"
#> 
#> $updated_at
#> [1] "2026-03-02T21:32:08Z"

bg_run()
#> $run_id
#> [1] "run_20260302_213208_002"
#> 
#> $executed
#>   node_0001   node_0002   node_0003 
#> "succeeded" "succeeded" "succeeded"
bg_backtrack(seed_node)
bg_status(verbose = TRUE)$pending_decisions
#> list()
```

## Async execution example

Independent runnable nodes can also be submitted in the background with
`bg_run(async = TRUE)`. `bg_status()` doubles as a lightweight poller and
reconciles completed jobs, while `bg_jobs()` exposes per-job manifests and
`bg_cancel()` can stop the active async queue. When `mirai` is available, it is
the primary async backend; `callr` remains the fallback path.


``` r
async_root <- file.path(tempdir(), "bayesgrove-readme-async")
unlink(async_root, recursive = TRUE, force = TRUE)
dir.create(async_root, recursive = TRUE, showWarnings = FALSE)

bg_init(
  async_root,
  project_name = "async-demo",
  config = list(execution = list(max_workers = 2L))
)

bg_register_node_kind(
  "delay_value_readme",
  validator = function(project, node, node_id) TRUE,
  executor = function(project, node, node_id, upstream_values, run_id, job_dir) {
    Sys.sleep(bg_null_coalesce(node$params$delay, 0.05))
    node$params$value
  }
)

bg_add_node(
  kind = "delay_value_readme",
  label = "A",
  params = list(value = 1L, delay = 0.1)
)
#> [1] "node_0001"
bg_add_node(
  kind = "delay_value_readme",
  label = "B",
  params = list(value = 2L, delay = 0.1)
)
#> [1] "node_0002"

bg_run(async = TRUE, backend = "callr")
#> $run_id
#> [1] "run_20260302_213208_001"
#> 
#> $workflow_state
#> [1] "executing"
#> 
#> $active_jobs
#> [1] 2
#> 
#> $queued
#> [1] 0
Sys.sleep(0.05)
bg_jobs()
#> [[1]]
#> [[1]]$job_id
#> [1] "job_001_node_0001"
#> 
#> [[1]]$run_id
#> [1] "run_20260302_213208_001"
#> 
#> [[1]]$node_id
#> [1] "node_0001"
#> 
#> [[1]]$backend
#> [1] "callr"
#> 
#> [[1]]$status
#> [1] "running"
#> 
#> [[1]]$pid
#> [1] 365561
#> 
#> [[1]]$started_at
#> [1] "2026-03-02T21:32:08Z"
#> 
#> [[1]]$finished_at
#> NULL
#> 
#> [[1]]$error
#> NULL
#> 
#> [[1]]$artifact_ref
#> NULL
#> 
#> [[1]]$progress
#> [1] 0.05
#> 
#> [[1]]$stage
#> [1] "running"
#> 
#> 
#> [[2]]
#> [[2]]$job_id
#> [1] "job_002_node_0002"
#> 
#> [[2]]$run_id
#> [1] "run_20260302_213208_001"
#> 
#> [[2]]$node_id
#> [1] "node_0002"
#> 
#> [[2]]$backend
#> [1] "callr"
#> 
#> [[2]]$status
#> [1] "running"
#> 
#> [[2]]$pid
#> [1] 365569
#> 
#> [[2]]$started_at
#> [1] "2026-03-02T21:32:08Z"
#> 
#> [[2]]$finished_at
#> NULL
#> 
#> [[2]]$error
#> NULL
#> 
#> [[2]]$artifact_ref
#> NULL
#> 
#> [[2]]$progress
#> [1] 0.05
#> 
#> [[2]]$stage
#> [1] "running"
bg_cancel(wait = TRUE)
#> $run_id
#> [1] "run_20260302_213208_001"
#> 
#> $cancelled_jobs
#> [1] 2
#> 
#> $cancelled_nodes
#> [1] "node_0001" "node_0002"
#> 
#> $workflow_state
#> [1] "idle"
bg_status()
#> $project_name
#> [1] "async-demo"
#> 
#> $root
#> [1] "/tmp/RtmpVSH9Zb/bayesgrove-readme-async"
#> 
#> $workflow_state
#> [1] "idle"
#> 
#> $last_checkpoint_id
#> NULL
#> 
#> $node_count
#> [1] 2
#> 
#> $edge_count
#> [1] 0
#> 
#> $states
#> $states$ready
#> [1] 2
#> 
#> 
#> $job_states
#> $job_states$cancelled
#> [1] 2
#> 
#> 
#> $active_jobs
#> [1] 0
#> 
#> $storage
#> $storage$bytes
#> [1] 7640
#> 
#> $storage$megabytes
#> [1] 0.007286072
#> 
#> $storage$warn_threshold_mb
#> [1] 500
#> 
#> $storage$near_threshold
#> [1] FALSE
bg_close()
```

## Bundle export example

The package can also export a shareable bundle of the current graph state,
decision log, artifacts, and recipe metadata.


``` r
bundle_root <- file.path(tempdir(), "bayesgrove-readme-bundle")
unlink(bundle_root, recursive = TRUE, force = TRUE)
dir.create(bundle_root, recursive = TRUE, showWarnings = FALSE)

bg_init(bundle_root, project_name = "bundle-demo")

bundle_source <- bg_add_node(
  kind = "data_source",
  label = "Value",
  params = list(value = 8L)
)

bundle_step <- bg_add_node(
  kind = "transform",
  label = "Square",
  params = list(fn = function(x) x * x)
)

bg_connect(bundle_source, bundle_step, edge_type = "data")
#> [1] "edge_0001"
bg_run()
#> $run_id
#> [1] "run_20260302_213209_001"
#> 
#> $executed
#>   node_0001   node_0002 
#> "succeeded" "succeeded"

bundle_dir <- bg_bundle(include_data = "recipe_only")
basename(bundle_dir)
#> [1] "bundle_20260302_213209_1206"
```

## More complete walkthroughs

For longer examples:

- `phase-one-walkthrough` covers the phase-0/phase-1 MVP, including
  Stan fitting, diagnostics, direct or graph-based LOO comparison,
  and bundle export
- `workflow-controls` covers pause/resume, branching, backtracking, and local
  async execution, plus a lightweight bundle handoff
- `ui-case-study` is a UI-first REPL walkthrough that shows a guided session
  transcript from decision gate to pause/resume and backtrack

## Extensibility

The package now also has early extension hooks:

- `bg_register_backend()` for alternative model backends
- `bg_register_env_provider()` for custom environment manifests
- `bg_register_suggester()` for suggestion providers
- `bg_register_node_kind()` for custom executable node kinds

Those registries are intentionally minimal, but they are already wired into the
current runtime.
