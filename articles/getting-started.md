# Getting Started with bayesgrove

## Introduction

**bayesgrove** is a graph-based Bayesian workflow orchestrator designed
to track the iterative nature of model building. It separates your
analysis into two distinct layers: 1. **The Execution Graph (DAG)**:
Built using `dagriculture`, representing the step-by-step
transformations and computations (data cleaning, compiling, fitting,
diagnosing). 2. **The Decision Layer**: An annotation overlay that
captures the provenance of your choices (why did you change the prior?
why did you filter outliers?) via decision gates.

This separation ensures that your computations remain reproducible while
giving you a rich historical record of your decision-making process. The
current package also includes a workflow-protocol layer: executors can
emit summaries,
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
can derive obligations and suggested actions from those summaries, and
callers can pass the resulting planner holds into
[`bg_plan()`](https://sims1253.github.io/bayesgrove/reference/bg_plan.md)
without conflating them with structural graph blockers. The built-in
`bayesguide.default_bayesian` pack now covers a narrow review loop:
computation review, branch-scoped fit criticism, candidate comparison,
and explicit branch acceptance or rejection.

That review loop is exposed through a small built-in template registry.
The registry keeps common next steps explicit instead of hardcoding
one-off REPL behaviors:

``` r
library(bayesgrove)

names(bg_list_templates())
#> [1] "diagnostic_check"      "branch_comparison"     "branch_and_modify_fit"
#> [4] "review_decision"
bg_list_templates("branch_comparison")$operation_type
#> [1] "create_node"
```

The package also makes its public boundary explicit.
[`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md)
distinguishes stable, experimental, and internal-but-exported functions,
and marks the smaller remote IPC contract exposed by the experimental
[`bg_serve()`](https://sims1253.github.io/bayesgrove/reference/bg_serve.md)
server. In-process execution is always available; background execution
uses `mirai`.

``` r
boundary <- bg_api_boundary()
subset(boundary, remote_accessible)[, c("fn", "classification")]
#>                    fn classification
#> 5         bg_add_node         stable
#> 19     bg_answer_gate         stable
#> 13  bg_branch_lineage         stable
#> 26          bg_cancel         stable
#> 6          bg_connect         stable
#> 14   bg_list_branches         stable
#> 51    bg_next_actions   experimental
#> 21 bg_record_decision         stable
#> 8      bg_remove_node         stable
#> 42        bg_snapshot         stable
#> 29          bg_status         stable
#> 24          bg_submit         stable
#> 7      bg_update_node         stable
```

The same workflow protocol also powers the built-in interactive terminal
client.
[`bg_repl()`](https://sims1253.github.io/bayesgrove/reference/bg_repl.md)
opens with a dashboard-oriented view so you can inspect the current
workflow state, active obligations, held nodes, branch-scoped gates,
recent decisions, and branch lineage without dropping into low-level
calls.

``` r
bg_repl(handle)
```

The main guided REPL commands are:

- `dashboard` to refresh the full operator view
- [`next`](https://rdrr.io/r/base/Control.html) to preview and
  optionally execute the top recommended action
- `do <n>` to execute a specific suggested action
- `lineage` to inspect the current branch and its ancestors
- `export md workflow_report.md` to write a report from the same session

### 1. Initializing a Project

Every bayesgrove analysis lives inside an explicitly initialized project
handle. This handle manages state, cache, and decision logs inside a
`.bayesgrove/` directory.

``` r
# Use a temporary directory for this vignette
project_root <- file.path(tempdir(), "bg-quickstart")
unlink(project_root, recursive = TRUE)

handle <- bg_init(
  path = project_root,
  project_name = "Quickstart",
  workflow_packs = list("bayesguide.default_bayesian")
)
print(handle)
#> <bayesgrove::bg_handle>
#>  @ .state              :<environment: 0x561a28a79198> 
#>  @ project_id          : chr "proj_79663245"
#>  @ path                : chr "/tmp/Rtmp2Gur8d/bg-quickstart"
#>  @ readonly            : logi FALSE
#>  @ closed              : logi FALSE
#>  @ loaded_graph_version: int 0
#>  @ lock_token          : chr NA
#>  @ registries          : list()
#>  @ metadata            : list()
```

### 2. Defining the Workflow Structure

In bayesgrove, everything is a node. For this demo, let’s register some
mock executors to simulate the workflow.

``` r
# Register a simple data provider
bg_register_node_kind(handle, "data", executor = function(node, inputs) {
  # In a real workflow, this would read from disk or a database
  data.frame(x = rnorm(100), y = rnorm(100))
})

# Register a simple model fitter
bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
  # Extract the upstream data
  raw_data <- inputs[[1]]
  list(
    result = sprintf("Fitted model on %d rows of data!", nrow(raw_data)),
    summaries = list(list(
      summary_kind = "optimizer_diagnostics",
      passed = FALSE,
      severity = "warning",
      metrics = list(max_gradient = 0.1)
    ))
  )
})

bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
  sprintf("Compared downstream artifact: %s", inputs[[1]])
})
```

Now we build the graph:

``` r
n_data <- bg_add_node(handle, kind = "data", label = "Simulated Dataset")
n_model <- bg_add_node(handle, kind = "fit", label = "Baseline Fit", inputs = n_data)
n_compare <- bg_add_node(
  handle,
  kind = "compare",
  label = "Comparison Step",
  inputs = n_model
)
```

### 3. Decision Gates

Instead of just running the model directly, let’s add a “Decision Gate”
to force us to explicitly approve the data before moving on. This
captures provenance.

``` r
gate <- bg_add_gate(
  project = handle,
  from = n_data,
  to = n_model,
  prompt = "Does the simulated data look ready for modeling?",
  options = c("yes", "no")
)
```

If we try to plan the workflow now, the runner will see that the graph
is blocked by an unresolved gate:

``` r
plan <- bg_plan(handle)
print(plan$blocked)
#> $node_7ecd6878
#> [1] "gate"
#> 
#> $node_cf82482e
#> [1] "upstream_blocked"
print(bg_pending_gates(handle))
#> $gate_2fcde878
#> $gate_2fcde878$id
#> [1] "gate_2fcde878"
#> 
#> $gate_2fcde878$edge_id
#> [1] "edge_9b922665"
#> 
#> $gate_2fcde878$prompt
#> [1] "Does the simulated data look ready for modeling?"
#> 
#> $gate_2fcde878$options
#> $gate_2fcde878$options[[1]]
#> [1] "yes"
#> 
#> $gate_2fcde878$options[[2]]
#> [1] "no"
#> 
#> 
#> $gate_2fcde878$refs
#> list()
#> 
#> $gate_2fcde878$created_at
#> [1] "2026-03-08T02:43:26Z"
#> 
#> $gate_2fcde878$metadata
#> list()
#> 
#> $gate_2fcde878$from_node_id
#> [1] "node_8cfeb1ca"
#> 
#> $gate_2fcde878$to_node_id
#> [1] "node_7ecd6878"
#> 
#> $gate_2fcde878$from_label
#> [1] "Simulated Dataset"
#> 
#> $gate_2fcde878$to_label
#> [1] "Baseline Fit"
```

We must explicitly answer the gate and provide a rationale. This
rationale is logged permanently.

``` r
bg_answer_gate(
  project = handle,
  id = gate$id,
  choice = "yes",
  rationale = "The simulated distributions match our expectations."
)
#> <bg_decision_record> dec_e22e00ae
#> • Prompt: Does the simulated data look ready for modeling?
#> • Choice: yes
#> • Rationale: The simulated distributions match our expectations.
```

### 4. Execution

With the gate resolved, the path is unblocked. We now run only through
the fit node so that we can inspect the workflow protocol before
scheduling downstream comparison work.

``` r
run_result <- bg_run(handle, targets = n_model, mode = "sync")
#> Starting run "run_8d060132" with 1 node to execute.
#> Running node "node_8cfeb1ca"...
#> Running node "node_7ecd6878"...
print(run_result$summary)
#> $total_executed
#> [1] 2
```

### 5. Workflow Guidance And External Holds

The fit executor emitted a lightweight warning summary.
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
turns that evidence into an obligation plus a matching next action:

``` r
next_steps <- bg_next_actions(handle)
next_steps$obligations[[1]]$kind
#> [1] "review_computation_validity"
next_steps$actions[[1]]$payload$decision_type
#> [1] "computation_review"
```

If the same warning appears on a branch-scoped fit, the stronger default
pack also emits `review_fit_criticism` and keeps `branch_and_modify`
available so you can preserve provenance while revising the branch.

Those blocking obligations can be passed back into planning as explicit
external holds. This keeps workflow guidance distinct from structural
graph state:

``` r
held_plan <- bg_plan(
  handle,
  external_holds = next_steps$metadata$external_holds
)

held_plan$blocked
#> named list()
held_plan$external_blocked
#> $node_cf82482e
#> [1] "Review computation validity"
```

In this example, the downstream comparison node is not structurally
broken. It is simply being held until the warning summary is reviewed or
replaced by a clean rerun.

### 6. Comparison And Branch Disposition

The stronger default pack also supports a project-level comparison loop
once multiple fit candidates are clean. This second example uses a fresh
project so that the comparison state is easy to inspect.

``` r
comparison_root <- file.path(tempdir(), "bg-comparison")
unlink(comparison_root, recursive = TRUE)

comparison_handle <- bg_init(
  path = comparison_root,
  project_name = "Comparison",
  workflow_packs = list("bayesguide.default_bayesian")
)

bg_register_node_kind(
  comparison_handle,
  "source",
  executor = function(node, inputs) {
    list(rows = 100L)
  }
)
bg_register_node_kind(
  comparison_handle,
  "fit",
  executor = function(node, inputs) {
    variant <- node$params$variant %||% "baseline"
    list(
      result = list(ok = TRUE, variant = variant, rows = inputs[[1]]$rows),
      summaries = list(list(
        summary_kind = "hmc_diagnostics",
        passed = TRUE,
        severity = "ok",
        metrics = list(variant = variant, divergences = 0)
      ))
    )
  }
)
bg_register_node_kind(
  comparison_handle,
  "compare",
  executor = function(node, inputs) {
    list(
      result = list(compared = names(inputs)),
      summaries = list(list(
        summary_kind = "comparison_results",
        passed = TRUE,
        severity = "ok"
      ))
    )
  }
)

n_source2 <- bg_add_node(comparison_handle, kind = "source", label = "Data")
n_fit_base <- bg_add_node(
  comparison_handle,
  kind = "fit",
  label = "Baseline Fit",
  inputs = n_source2,
  params = list(variant = "baseline")
)
bg_run(comparison_handle, mode = "sync")
#> Starting run "run_8ad43b9f" with 1 node to execute.
#> Running node "node_959058e6"...
#> Running node "node_493a9522"...
#> <bg_run_handle> run_8ad43b9f
#> 
#> • Status: succeeded
#> 
#> • Mode: sync
#> 
#> • Executed Nodes: 2

branch <- bg_branch(comparison_handle, n_fit_base, label = "Alternative Fit")
bg_set_goal(
  project = comparison_handle,
  branch_id = branch$branch_id,
  kind = "observable_prediction",
  label = "Compare alternatives",
  rationale = "This branch exists to compare two clean fits."
)
#> <bg_decision_record> dec_7795c883
#> • Prompt: Set inferential goal
#> • Choice: Compare alternatives
#> • Rationale: This branch exists to compare two clean fits.
bg_update_node(
  comparison_handle,
  branch$root_node_id,
  params = list(variant = "alternative")
)
#> [1] "node_5eca0997"
bg_run(comparison_handle, targets = branch$root_node_id, mode = "sync")
#> Starting run "run_1ec05569" with 1 node to execute.
#> Running node "node_5eca0997"...
#> <bg_run_handle> run_1ec05569
#> 
#> • Status: succeeded
#> 
#> • Mode: sync
#> 
#> • Executed Nodes: 1
```

With two clean fits, project scope now requires an explicit comparison
step:

``` r
comparison_guide <- bg_next_actions(comparison_handle, scope = "project")
vapply(comparison_guide$obligations, `[[`, character(1), "kind")
#>                 obl_65f0ae8b 
#> "compare_candidate_branches"
Filter(
  function(x) identical(x$kind, "create_node_from_template"),
  comparison_guide$actions
)[[1]]$payload$template_ref
#> [1] "branch_comparison"
```

That `template_ref` is stable and maps to the built-in descriptor
returned by `bg_list_templates("branch_comparison")`. The same registry
is what the REPL uses when you execute a suggested action interactively.

Create and run the comparison node, then record a `model_comparison`
decision using the payload fields supplied by the workflow action:

``` r
comparison_node_id <- bg_add_node(
  comparison_handle,
  kind = "compare",
  label = "Compare Fits",
  inputs = c(n_fit_base, branch$root_node_id)
)
bg_run(comparison_handle, targets = comparison_node_id, mode = "sync")
#> Starting run "run_08de37dd" with 1 node to execute.
#> Running node "node_ece6822f"...
#> <bg_run_handle> run_08de37dd
#> 
#> • Status: succeeded
#> 
#> • Mode: sync
#> 
#> • Executed Nodes: 1

comparison_guide <- bg_next_actions(comparison_handle, scope = "project")
comparison_action <- Filter(
  function(x) {
    identical(x$kind, "record_decision") &&
      identical(x$payload$decision_type, "model_comparison")
  },
  comparison_guide$actions
)[[1]]

bg_record_decision(
  comparison_handle,
  scope = "project",
  prompt = "Compare candidate branches",
  choice = "prefer_alternative",
  rationale = "The alternative fit is the preferred clean candidate.",
  kind = "model_comparison",
  metadata = comparison_action$payload[c(
    "fit_node_ids",
    "summary_ids",
    "candidate_signature",
    "comparison_signature",
    "comparison_context"
  )]
)
#> <bg_decision_record> dec_3184f92e
#> • Prompt: Compare candidate branches
#> • Choice: prefer_alternative
#> • Rationale: The alternative fit is the preferred clean candidate.
```

After a current comparison exists, branch scope asks for an explicit
accept or reject disposition:

``` r
branch_guide <- bg_next_actions(
  comparison_handle,
  scope = "branch",
  branch_id = branch$branch_id
)
vapply(branch_guide$obligations, `[[`, character(1), "kind")
#>              obl_c5bd240d 
#> "accept_or_reject_branch"

disposition_action <- Filter(
  function(x) {
    identical(x$kind, "record_decision") &&
      identical(x$payload$decision_type, "branch_disposition")
  },
  branch_guide$actions
)[[1]]

bg_record_decision(
  comparison_handle,
  scope = branch$branch_id,
  prompt = "Accept or reject branch",
  choice = "accept",
  rationale = "Keep the alternative branch as the accepted analysis path.",
  kind = "branch_disposition",
  metadata = list(
    disposition = "accept",
    summary_ids = disposition_action$payload$summary_ids,
    comparison_signature = disposition_action$payload$comparison_signature,
    comparison_context = disposition_action$payload$comparison_context
  )
)
#> <bg_decision_record> dec_d2645068
#> • Prompt: Accept or reject branch
#> • Choice: accept
#> • Rationale: Keep the alternative branch as the accepted analysis path.

bg_next_actions(
  comparison_handle,
  scope = "branch",
  branch_id = branch$branch_id
)$obligations
#> named list()
```

### 7. Next Steps

As you advance, you can use
[`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
to create alternative model paths (e.g., trying a different prior
without losing your baseline),
[`bg_invalidate()`](https://sims1253.github.io/bayesgrove/reference/bg_invalidate.md)
to supersede outdated results, and
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
plus `bg_plan(..., external_holds = ...)` to keep workflow obligations
visible during scheduling. For the built-in default pack, that means you
can move through the full loop without leaving the runtime contract:
review the computation, review branch criticism when needed, compare
clean candidates, and explicitly accept or reject the surviving branch.
