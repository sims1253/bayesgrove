# Guides Workflow Pack - Helpers
# ----------------------------------
# Summary, taxonomy, and Stan diagnostic helper functions for the
# guides workflow pack.

bg_phase10_summary_basis <- function(summaries, branch_ids = NULL) {
  summaries <- summaries %||% list()

  if (length(summaries) == 0) {
    return(list(
      node_ids = character(),
      summary_ids = character(),
      branch_ids = bg_phase10_sort_ids(branch_ids)
    ))
  }

  list(
    node_ids = bg_phase10_sort_ids(vapply(
      summaries,
      `[[`,
      character(1),
      "node_id"
    )),
    summary_ids = bg_phase10_sort_ids(vapply(
      summaries,
      `[[`,
      character(1),
      "summary_id"
    )),
    branch_ids = bg_phase10_sort_ids(branch_ids)
  )
}

bg_phase10_failing_summaries <- function(summaries) {
  Filter(
    function(summary) {
      severity <- summary$severity %||%
        if (isFALSE(summary$passed)) "warning" else "ok"

      !isTRUE(summary$passed) || severity %in% c("warning", "error")
    },
    summaries %||% list()
  )
}

bg_phase10_taxonomy_utility_dimensions <- function() {
  c(
    "causal_consistency",
    "parameter_recoverability",
    "predictive_performance",
    "fairness",
    "structural_faithfulness",
    "parsimony",
    "interpretability",
    "convergence",
    "estimation_speed",
    "robustness"
  )
}

bg_phase10_taxonomy_evaluation_modes <- function(context) {
  goal_kind <- bg_phase10_goal_kind(context)

  switch(
    goal_kind,
    observable_prediction = c(
      "prior_predictive",
      "posterior_predictive",
      "out_of_sample"
    ),
    latent_inference = c(
      "prior_predictive",
      "posterior_predictive",
      "simulation_based_calibration"
    ),
    c(
      "prior_predictive",
      "posterior_predictive",
      "out_of_sample"
    )
  )
}

bg_phase10_stan_metric_value <- function(metrics, keys, default = NULL) {
  metrics <- metrics %||% list()

  for (key in keys) {
    value <- metrics[[key]] %||% NULL
    if (!is.null(value)) {
      return(value)
    }
  }

  default
}

bg_phase10_stan_repair_hints <- function(summaries) {
  hints <- character()

  for (summary in summaries %||% list()) {
    metrics <- summary$metrics %||% list()
    summary_kind <- summary$summary_kind %||% NULL

    if (identical(summary_kind, "hmc_diagnostics")) {
      if (
        isTRUE(bg_phase10_stan_metric_value(
          metrics,
          c("divergences_present", "divergent"),
          FALSE
        )) ||
          (bg_phase10_stan_metric_value(
            metrics,
            c("divergent_transitions", "divergences"),
            0
          ) >
            0)
      ) {
        hints <- c(hints, "reparameterize", "raise_adapt_delta")
      }

      if (
        isTRUE(bg_phase10_stan_metric_value(
          metrics,
          c("treedepth_saturated", "max_treedepth_exceeded"),
          FALSE
        )) ||
          (bg_phase10_stan_metric_value(
            metrics,
            c("max_treedepth_hits", "treedepth_hits"),
            0
          ) >
            0)
      ) {
        hints <- c(hints, "increase_max_treedepth")
      }

      if (
        (bg_phase10_stan_metric_value(
          metrics,
          c("max_rhat", "rhat_max"),
          1
        ) >
          1.01) ||
          (bg_phase10_stan_metric_value(
            metrics,
            c("min_ess_bulk", "ess_bulk_min"),
            Inf
          ) <
            400)
      ) {
        hints <- c(hints, "increase_iterations")
      }
    }

    if (identical(summary_kind, "optimizer_diagnostics")) {
      hints <- c(hints, "rescale_parameters", "adjust_tolerances")
    }
  }

  sort(unique(hints))
}
