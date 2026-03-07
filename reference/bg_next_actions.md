# Compute deterministic next workflow actions

Evaluates the active workflow packs against a derived workflow context
and returns obligations, suggested actions, and planner-ready external
holds. Callers can pass `result$metadata$external_holds` into
[`bg_plan()`](https://sims1253.github.io/bayesgrove/reference/bg_plan.md)
to keep workflow holds distinct from structural blockers.

## Usage

``` r
bg_next_actions(project, scope = c("project", "branch"), branch_id = NULL)
```

## Arguments

- project:

  A `bg_handle`.

- scope:

  One of `project` or `branch`.

- branch_id:

  Optional branch id, required for branch-scoped queries.

## Value

A `bg_next_actions_result` plain-data list with `context`,
`obligations`, `actions`, and `metadata$external_holds`.

## Details

For the built-in `bayesguide.default_bayesian` pack, the returned
obligations and actions can include:

- `review_computation_validity` and matching `computation_review`
  actions for fresh warning/error summaries,

- `review_fit_criticism` plus `fit_criticism` and `branch_and_modify`
  actions for branch-scoped fit or diagnostic problems,

- `compare_candidate_branches` plus comparison-node creation or
  `model_comparison` decision actions when multiple clean fit candidates
  exist, and

- `accept_or_reject_branch` plus `branch_disposition` actions after a
  current comparison exists for an active candidate set.
