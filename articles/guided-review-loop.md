# Guided Review Loop

## Why this vignette exists

This vignette shows one deterministic, lightweight product loop on top
of the current workflow protocol. It does not fit a real Stan model.
Instead, it uses small mock executors so that the whole loop is
reproducible on contributor machines and in CI.

The loop covers the current package strengths:

- branch lineage,
- workflow holds on downstream work,
- explicit decision provenance,
- comparison and final branch disposition,
- report export after iteration.

It intentionally exercises only the narrow `bayesgrove.default_bayesian`
pack. If you want process guidance, taxonomy review, predictive checks,
SBC review, stacking-aware model selection, Stan-specific review, or
DAG-constrained causal selection, add the richer packs described in
[`vignette("extensions", package = "bayesgrove")`](https://sims1253.github.io/bayesgrove/articles/extensions.md).

## Setup

``` r

library(bayesgrove)

project_root <- file.path(tempdir(), "bg-guided-review-loop")
unlink(project_root, recursive = TRUE, force = TRUE)

handle <- bg_init(
  path = project_root,
  project_name = "Guided Review Loop",
  workflow_packs = list("bayesgrove.default_bayesian")
)

node_label <- function(node_id) {
  graph <- bg_read_graph(handle)
  graph$nodes[[node_id]]$label %||% node_id
}

scope_label <- function(scope) {
  bg_scope_label(handle, scope)
}

find_action <- function(actions, kind, decision_type = NULL, template_ref = NULL) {
  for (action in actions) {
    if (!identical(action$kind, kind)) {
      next
    }

    if (!is.null(decision_type) &&
          !identical(action$payload$decision_type %||% NULL, decision_type)) {
      next
    }

    if (!is.null(template_ref) &&
          !identical(action$payload$template_ref %||% NULL, template_ref)) {
      next
    }

    return(action)
  }

  NULL
}

protocol_overview <- function(result) {
  partitioned <- bg_partition_protocol_by_scope(result, project = handle)
  scopes <- setdiff(names(partitioned), "summary")

  if (length(scopes) == 0) {
    return(data.frame())
  }

  data.frame(
    scope = vapply(scopes, function(scope) partitioned[[scope]]$scope_label, character(1)),
    obligations = vapply(
      scopes,
      function(scope) {
        items <- vapply(
          partitioned[[scope]]$obligations,
          `[[`,
          character(1),
          "kind"
        )
        if (length(items) == 0) "none" else paste(items, collapse = ", ")
      },
      character(1)
    ),
    actions = vapply(
      scopes,
      function(scope) {
        items <- vapply(
          partitioned[[scope]]$actions,
          `[[`,
          character(1),
          "kind"
        )
        if (length(items) == 0) "none" else paste(items, collapse = ", ")
      },
      character(1)
    ),
    row.names = NULL
  )
}

summary_overview <- function(summaries) {
  if (length(summaries) == 0) {
    return(data.frame())
  }

  data.frame(
    severity = vapply(summaries, `[[`, character(1), "severity"),
    variant = vapply(summaries, function(x) x$metrics$variant %||% "", character(1)),
    parametrization = vapply(
      summaries,
      function(x) x$metrics$parametrization %||% "",
      character(1)
    ),
    is_fresh = vapply(summaries, function(x) isTRUE(x$is_fresh), logical(1)),
    is_stale = vapply(summaries, function(x) isTRUE(x$is_stale), logical(1)),
    row.names = NULL
  )
}
```

## Register deterministic node kinds

``` r

bg_register_node_kind(handle, "source", executor = function(node, inputs) {
  data.frame(
    group = rep(c("a", "b", "c"), each = 4),
    x = c(-1.2, -0.8, -0.3, 0.0, 0.1, 0.4, 0.8, 1.0, -0.5, -0.1, 0.6, 1.2),
    y = c(0.4, 0.8, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 0.6, 0.9, 1.4, 1.8)
  )
})

bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
  parametrization <- node$params$parametrization %||% "non-centered"
  variant <- node$params$variant %||% "baseline"

  severity <- if (identical(parametrization, "centered")) "warning" else "ok"
  divergences <- if (identical(severity, "warning")) 11L else 0L
  score <- switch(
    variant,
    baseline = 0.90,
    centered_review = 0.82,
    repaired_branch = 0.95,
    robust_branch = 0.91,
    0.88
  )

  list(
    result = list(
      variant = variant,
      parametrization = parametrization,
      expected_elpd = score,
      rows = nrow(inputs[[1]])
    ),
    summaries = list(list(
      summary_kind = "hmc_diagnostics",
      passed = identical(severity, "ok"),
      severity = severity,
      metrics = list(
        divergences = divergences,
        expected_elpd = score,
        variant = variant,
        parametrization = parametrization
      )
    ))
  )
})

bg_register_node_kind(handle, "ppc", executor = function(node, inputs) {
  list(
    result = list(
      branch_variant = inputs[[1]]$variant,
      conclusion = "Posterior predictive check placeholder"
    )
  )
})

bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
  scores <- vapply(inputs, function(x) x$expected_elpd, numeric(1))
  labels <- names(inputs)
  ranking <- data.frame(
    fit = labels,
    expected_elpd = unname(scores),
    row.names = NULL
  )
  ranking <- ranking[order(ranking$expected_elpd, decreasing = TRUE), ]

  list(
    ranking = ranking,
    recommended = ranking$fit[[1]],
    summaries = list(list(
      summary_kind = "comparison_results",
      passed = TRUE,
      severity = "ok",
      metrics = list(recommended = ranking$fit[[1]])
    ))
  )
})
```

## Create the baseline graph and two branch candidates

The project contains one source node and one baseline fit. We then
create two branches:

- a centered branch that will trigger criticism,
- a robust branch that stays clean and gives us a comparison target
  later.

``` r

source_id <- bg_add_node(handle, kind = "source", label = "Synthetic cohort")
baseline_fit_id <- bg_add_node(
  handle,
  kind = "fit",
  label = "Baseline fit",
  inputs = source_id,
  params = list(
    variant = "baseline",
    parametrization = "non-centered"
  )
)
baseline_ppc_id <- bg_add_node(
  handle,
  kind = "ppc",
  label = "Baseline PPC",
  inputs = baseline_fit_id
)

warning_branch <- bg_branch(
  handle,
  baseline_fit_id,
  label = "Centered branch",
  continue = "ppc"
)
warning_fit_id <- warning_branch$root_node_id
warning_ppc_id <- warning_branch$continuation_nodes[[baseline_ppc_id]]$clone_id

bg_update_node(
  handle,
  warning_fit_id,
  label = "Centered branch",
  params = list(
    variant = "centered_review",
    parametrization = "centered"
  )
)
#> [1] "node_e22e00ae"
bg_update_node(handle, warning_ppc_id, label = "Centered branch PPC")
#> [1] "node_95d7b92f"

bg_set_goal(
  project = handle,
  branch_id = warning_branch$branch_id,
  kind = "observable_prediction",
  label = "Repair the centered branch without losing forecast skill",
  rationale = "This branch explores a questionable fit before accepting it."
)
#> <bg_decision_record> dec_da036e34
#> • Prompt: Set inferential goal
#> • Choice: Repair the centered branch without losing forecast skill
#> • Rationale: This branch explores a questionable fit before accepting it.

robust_branch <- bg_branch(
  handle,
  baseline_fit_id,
  label = "Robust branch",
  continue = "ppc"
)
robust_fit_id <- robust_branch$root_node_id
robust_ppc_id <- robust_branch$continuation_nodes[[baseline_ppc_id]]$clone_id

bg_update_node(
  handle,
  robust_fit_id,
  label = "Robust branch",
  params = list(
    variant = "robust_branch",
    parametrization = "non-centered"
  )
)
#> [1] "node_0bf76a9f"
bg_update_node(handle, robust_ppc_id, label = "Robust branch PPC")
#> [1] "node_dab33fa3"

bg_set_goal(
  project = handle,
  branch_id = robust_branch$branch_id,
  kind = "observable_prediction",
  label = "Keep a clean alternative for later comparison",
  rationale = "A second branch lets the project compare explicit alternatives."
)
#> <bg_decision_record> dec_edb00da8
#> • Prompt: Set inferential goal
#> • Choice: Keep a clean alternative for later comparison
#> • Rationale: A second branch lets the project compare explicit alternatives.

data.frame(
  branch = c(
    scope_label(warning_branch$branch_id),
    scope_label(robust_branch$branch_id)
  ),
  goal = vapply(
    c(warning_branch$branch_id, robust_branch$branch_id),
    function(branch_id) bg_get_goal(handle, branch_id)$label,
    character(1)
  ),
  row.names = NULL
)
#>            branch                                                     goal
#> 1 Centered branch Repair the centered branch without losing forecast skill
#> 2   Robust branch            Keep a clean alternative for later comparison
```

## Run the centered branch and inspect the hold

Running the centered branch fit produces a warning summary. The workflow
pack turns that warning into a blocking branch-scoped obligation, and
downstream work on that branch is held by policy.

``` r

bg_run(handle, targets = warning_fit_id)
#> Starting run "run_68a250c5" with 1 node to execute.
#> Running node "node_7ecd6878"...
#> Running node "node_e22e00ae"...
#> <bg_run_handle> run_68a250c5
#> 
#> • Status: succeeded
#> 
#> • Executed Nodes: 2

warning_protocol <- bg_next_actions(handle, scope = "project")
protocol_overview(warning_protocol)
#>             scope                                       obligations
#> 1         Project                                              none
#> 2   Robust branch                                              none
#> 3 Centered branch review_computation_validity, review_fit_criticism
#>                                               actions
#> 1                                                none
#> 2                                                none
#> 3 record_decision, branch_and_modify, record_decision
```

The downstream PPC is structurally ready, but it should not run yet:

``` r

held_plan <- bg_plan(
  handle,
  external_holds = warning_protocol$metadata$external_holds
)

data.frame(
  node = vapply(names(held_plan$held_by_policy), node_label, character(1)),
  reason = unname(vapply(held_plan$held_by_policy, as.character, character(1))),
  row.names = NULL
)
#>                  node               reason
#> 1 Centered branch PPC Review fit criticism
```

## Record criticism, then branch and modify

The branch-scoped action set now includes an explicit fit-criticism
review. We record that decision before creating a repair branch from the
criticized fit.

``` r

warning_branch_protocol <- bg_next_actions(
  handle,
  scope = "branch",
  branch_id = warning_branch$branch_id
)
criticism_action <- find_action(
  warning_branch_protocol$actions,
  kind = "record_decision",
  decision_type = "fit_criticism"
)

bg_record_decision(
  handle,
  scope = warning_branch$branch_id,
  prompt = "How should the centered branch be handled?",
  choice = "needs_reparametrization",
  rationale = paste(
    "The warning summary blocks downstream review, so the fit should be",
    "repaired before it participates in branch comparison."
  ),
  kind = "fit_criticism",
  metadata = criticism_action$payload[c("node_ids", "summary_ids")]
)
#> <bg_decision_record> dec_abe165f4
#> • Prompt: How should the centered branch be handled?
#> • Choice: needs_reparametrization
#> • Rationale: The warning summary blocks downstream review, so the fit should be
#>   repaired before it participates in branch comparison.

repaired_branch <- bg_branch(
  handle,
  warning_fit_id,
  label = "Repaired branch",
  continue = "ppc"
)
repaired_fit_id <- repaired_branch$root_node_id
repaired_ppc_id <- repaired_branch$continuation_nodes[[warning_ppc_id]]$clone_id

bg_update_node(
  handle,
  repaired_fit_id,
  label = "Repaired branch",
  params = list(
    variant = "centered_review",
    parametrization = "centered",
    revision = 1L
  )
)
#> [1] "node_86892a1e"
bg_update_node(handle, repaired_ppc_id, label = "Repaired branch PPC")
#> [1] "node_28679b29"

bg_set_goal(
  project = handle,
  branch_id = repaired_branch$branch_id,
  kind = "observable_prediction",
  label = "Resolve the warning and keep the branch comparable",
  rationale = "This branch is the repaired continuation of the criticism loop."
)
#> <bg_decision_record> dec_9813439a
#> • Prompt: Set inferential goal
#> • Choice: Resolve the warning and keep the branch comparable
#> • Rationale: This branch is the repaired continuation of the criticism loop.

data.frame(
  branch = scope_label(repaired_branch$branch_id),
  lineage = paste(
    vapply(
      bg_branch_lineage(handle, repaired_branch$branch_id),
      scope_label,
      character(1)
    ),
    collapse = " -> "
  ),
  row.names = NULL
)
#>            branch         lineage
#> 1 Repaired branch Centered branch
```

## Rerun the repaired branch and show stale versus current evidence

The repaired branch starts by inheriting the criticized centered
parameters. After one run, we update the branched fit, invalidate it,
and rerun. The old warning summary becomes stale and the new clean
summary becomes current.

``` r

bg_run(handle, targets = repaired_fit_id)
#> Starting run "run_d79763f6" with 1 node to execute.
#> Running node "node_86892a1e"...
#> <bg_run_handle> run_d79763f6
#> 
#> • Status: succeeded
#> 
#> • Executed Nodes: 1

bg_update_node(
  handle,
  repaired_fit_id,
  label = "Repaired branch",
  params = list(
    variant = "repaired_branch",
    parametrization = "non-centered",
    revision = 2L
  )
)
#> [1] "node_86892a1e"
bg_invalidate(handle, repaired_fit_id, recursive = TRUE)
#> Invalidated 2 nodes; superseded 1 cache binding.
bg_run(handle, targets = repaired_fit_id)
#> Starting run "run_22d06b87" with 1 node to execute.
#> Running node "node_86892a1e"...
#> <bg_run_handle> run_22d06b87
#> 
#> • Status: succeeded
#> 
#> • Executed Nodes: 1

repaired_summaries <- bg_read_summaries(
  handle,
  scope = repaired_branch$branch_id
)
repaired_summaries <- Filter(
  function(x) identical(x$node_id, repaired_fit_id),
  repaired_summaries
)
summary_overview(repaired_summaries)
#>   severity         variant parametrization is_fresh is_stale
#> 1  warning centered_review        centered    FALSE     TRUE
#> 2       ok repaired_branch    non-centered     TRUE    FALSE
```

## Surface the comparison step across branches

The robust branch runs cleanly on the first try. Once both branch
candidates are clean, project scope surfaces a comparison action.

``` r

bg_run(handle, targets = robust_fit_id)
#> Starting run "run_c3ae03a7" with 1 node to execute.
#> Running node "node_0bf76a9f"...
#> <bg_run_handle> run_c3ae03a7
#> 
#> • Status: succeeded
#> 
#> • Executed Nodes: 1

project_protocol <- bg_next_actions(handle, scope = "project")
protocol_overview(project_protocol)
#>             scope                 obligations
#> 1         Project  compare_candidate_branches
#> 2   Robust branch                        none
#> 3 Repaired branch review_computation_validity
#> 4 Centered branch review_computation_validity
#>                              actions
#> 1          create_node_from_template
#> 2                               none
#> 3 record_decision, branch_and_modify
#> 4 record_decision, branch_and_modify
```

We create the comparison node from the surfaced template payload, run
it, and inspect the deterministic ranking:

``` r

comparison_action <- find_action(
  project_protocol$actions,
  kind = "create_node_from_template",
  template_ref = "branch_comparison"
)

comparison_node_id <- bg_add_node(
  handle,
  kind = "compare",
  label = "Compare repaired vs robust branch",
  inputs = comparison_action$payload$inputs
)
bg_run(handle, targets = comparison_node_id)
#> Starting run "run_7373fff5" with 1 node to execute.
#> Running node "node_b44d27ce"...
#> <bg_run_handle> run_7373fff5
#> 
#> • Status: succeeded
#> 
#> • Executed Nodes: 1

bg_result(handle, comparison_node_id)$ranking
#>             fit expected_elpd
#> 2 node_86892a1e          0.95
#> 1 node_0bf76a9f          0.91
```

## Record model comparison and branch disposition decisions

The current comparison now unlocks two layers of explicit provenance:

- one project-scoped comparison decision,
- one accept/reject decision for each candidate branch.

``` r

comparison_protocol <- bg_next_actions(handle, scope = "project")
comparison_decision_action <- find_action(
  comparison_protocol$actions,
  kind = "record_decision",
  decision_type = "model_comparison"
)

bg_record_decision(
  handle,
  scope = "project",
  prompt = "Which branch should anchor the final report?",
  choice = "prefer_repaired_branch",
  rationale = paste(
    "The repaired branch resolves the diagnostic warning and keeps the best",
    "expected predictive score in this deterministic example."
  ),
  kind = "model_comparison",
  metadata = comparison_decision_action$payload[c(
    "fit_node_ids",
    "branch_ids",
    "summary_ids",
    "candidate_signature",
    "comparison_signature",
    "comparison_context"
  )]
)
#> <bg_decision_record> dec_dfe990f6
#> • Prompt: Which branch should anchor the final report?
#> • Choice: prefer_repaired_branch
#> • Rationale: The repaired branch resolves the diagnostic warning and keeps the
#>   best expected predictive score in this deterministic example.

repaired_disposition_action <- find_action(
  bg_next_actions(
    handle,
    scope = "branch",
    branch_id = repaired_branch$branch_id
  )$actions,
  kind = "record_decision",
  decision_type = "branch_disposition"
)

bg_record_decision(
  handle,
  scope = repaired_branch$branch_id,
  prompt = "Should the repaired branch be accepted?",
  choice = "accept",
  rationale = "This is the clean branch preferred by the comparison step.",
  kind = "branch_disposition",
  metadata = list(
    disposition = "accept",
    summary_ids = repaired_disposition_action$payload$summary_ids,
    comparison_signature = repaired_disposition_action$payload$comparison_signature,
    comparison_context = repaired_disposition_action$payload$comparison_context
  )
)
#> <bg_decision_record> dec_e3453114
#> • Prompt: Should the repaired branch be accepted?
#> • Choice: accept
#> • Rationale: This is the clean branch preferred by the comparison step.

robust_disposition_action <- find_action(
  bg_next_actions(
    handle,
    scope = "branch",
    branch_id = robust_branch$branch_id
  )$actions,
  kind = "record_decision",
  decision_type = "branch_disposition"
)

bg_record_decision(
  handle,
  scope = robust_branch$branch_id,
  prompt = "Should the robust branch be accepted?",
  choice = "reject",
  rationale = "Keep it as a documented alternative, but not the final path.",
  kind = "branch_disposition",
  metadata = list(
    disposition = "reject",
    summary_ids = robust_disposition_action$payload$summary_ids,
    comparison_signature = robust_disposition_action$payload$comparison_signature,
    comparison_context = robust_disposition_action$payload$comparison_context
  )
)
#> <bg_decision_record> dec_ad1005cd
#> • Prompt: Should the robust branch be accepted?
#> • Choice: reject
#> • Rationale: Keep it as a documented alternative, but not the final path.

snapshot <- bg_snapshot(handle)

data.frame(
  scope = vapply(snapshot$decisions, function(x) scope_label(x$scope), character(1)),
  kind = vapply(snapshot$decisions, `[[`, character(1), "kind"),
  choice = vapply(snapshot$decisions, `[[`, character(1), "choice"),
  row.names = NULL
)
#>             scope               kind
#> 1 Centered branch        goal_update
#> 2   Robust branch        goal_update
#> 3 Centered branch      fit_criticism
#> 4 Repaired branch        goal_update
#> 5         Project   model_comparison
#> 6 Repaired branch branch_disposition
#> 7   Robust branch branch_disposition
#>                                                     choice
#> 1 Repair the centered branch without losing forecast skill
#> 2            Keep a clean alternative for later comparison
#> 3                                  needs_reparametrization
#> 4       Resolve the warning and keep the branch comparable
#> 5                                   prefer_repaired_branch
#> 6                                                   accept
#> 7                                                   reject
```

## Export the report

The project can now export a report that includes graph topology and
decision provenance. The path below is shown relative to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) to keep the vignette
output stable across machines.

``` r

report_path <- bg_export_report(
  handle,
  path = "case-study-report.md",
  format = "md"
)
#> Report exported to
#> /tmp/RtmpiUVONE/bg-guided-review-loop/case-study-report.md

report_path_relative <- sub(
  paste0("^", normalizePath(tempdir(), winslash = "/"), "/?"),
  "",
  normalizePath(report_path, winslash = "/")
)

cat(report_path_relative, sep = "\n")
#> bg-guided-review-loop/case-study-report.md
cat(readLines(report_path, n = 12, warn = FALSE), sep = "\n")
#> # bayesgrove Workflow Report: Guided Review Loop
#> **Project ID:** `proj_79663245`
#> **Generated:** 2026-07-06 14:44:17
#> **Workflow state:** `blocked`
#> 
#> ## Graph Topology
#> - **node_7ecd6878** (`source`): Synthetic cohort
#> - **node_9b922665** (`fit`): Baseline fit
#>   - *Inputs:* node_7ecd6878
#> - **node_e22e00ae** (`fit`): Centered branch
#>   - *Inputs:* node_7ecd6878
#> - **node_0bf76a9f** (`fit`): Robust branch
```

## What this loop demonstrates

This one case study stays inside the current runtime semantics while
still showing the full product loop:

1.  a branch warning becomes a blocking workflow obligation,
2.  downstream work is held without changing the structural graph,
3.  criticism is recorded explicitly before repair work,
4.  branch lineage remains visible after the repair branch is created,
5.  stale evidence is preserved for auditability after rerunning the
    revised fit,
6.  comparison and branch-disposition decisions are recorded explicitly,
7.  the same state can be exported into a report artifact.
