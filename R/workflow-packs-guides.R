# Guides Workflow Pack - Obligations and Actions
# ---------------------------------------------
# Summary, taxonomy, and Stan helper functions follow below in this file.

bg_pack_process_obligations <- function(
  context,
  pack_config = list()
) {
  fit_node_ids <- bg_pack_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  if (
    startsWith(context$scope, "branch:") &&
      is.null(bg_pack_goal_kind(context))
  ) {
    return(list())
  }

  obligations <- list()

  if (
    !bg_pack_has_scope_decision(
      context,
      kind = "workflow_preflight",
      node_ids = fit_node_ids
    )
  ) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_workflow_preflight",
        title = "Record workflow preflight",
        why = paste0(
          "Bayesian workflow begins by making the initial model, parameter ",
          "scaling, simulation rehearsal, and acceptance criteria explicit before ",
          "the branch treats later fits as trustworthy evidence."
        ),
        severity = "advisory",
        basis = list(node_ids = fit_node_ids),
        metadata = list(
          source_keys = "workflow_core",
          utility_dimensions = c(
            "structural_faithfulness",
            "robustness",
            "interpretability"
          ),
          pad_model_classes = c("P", "PD", "PAD")
        )
      ))
    )
  }

  diagnostic_summaries <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "workflow_iteration_plan",
    summary_kinds = c("hmc_diagnostics", "optimizer_diagnostics"),
    node_ids = fit_node_ids
  )
  diagnostic_summaries <- bg_pack_failing_summaries(diagnostic_summaries)

  if (length(diagnostic_summaries) > 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "plan_workflow_iteration",
        title = "Record the next workflow iteration",
        why = paste0(
          "Bayesian workflow is iterative. When computation raises fresh Stan or ",
          "optimizer warnings, record the simplification, reparameterization, or ",
          "rehearsal step that should happen next."
        ),
        severity = bg_pack_review_severity(diagnostic_summaries),
        basis = bg_pack_summary_basis(diagnostic_summaries),
        metadata = list(
          source_keys = c("workflow_core", "stan_diagnostics"),
          summary_kinds = sort(unique(vapply(
            diagnostic_summaries,
            `[[`,
            character(1),
            "summary_kind"
          ))),
          utility_dimensions = c(
            "convergence",
            "estimation_speed",
            "robustness"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  generalization_summaries <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "workflow_generalization_review",
    summary_kinds = c("loo_diagnostics", "pareto_k_diagnostics")
  )

  if (length(generalization_summaries) > 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_out_of_sample_stability",
        title = "Review out-of-sample stability",
        why = paste0(
          "Cross-validation and Pareto-k diagnostics should be interpreted as part ",
          "of the workflow, not as final answers. Record what the current ",
          "generalization evidence says and what follow-up it requires."
        ),
        severity = bg_pack_review_severity(generalization_summaries),
        basis = bg_pack_summary_basis(generalization_summaries),
        metadata = list(
          source_keys = c("workflow_core", "model_comparison", "loo_pit"),
          summary_kinds = sort(unique(vapply(
            generalization_summaries,
            `[[`,
            character(1),
            "summary_kind"
          ))),
          utility_dimensions = c(
            "predictive_performance",
            "robustness",
            "parsimony"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  obligations
}

bg_pack_process_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  actions <- list()

  preflight_obligation <- bg_find_obligation(
    obligations,
    kind = "review_workflow_preflight",
    scope = context$scope
  )
  if (!is.null(preflight_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = preflight_obligation,
        title = "Record workflow preflight",
        decision_type = "workflow_preflight",
        why_now = paste0(
          "The workflow benefits from an explicit preflight note before later ",
          "review packs interpret the branch's summaries."
        ),
        payload = list(
          suggested_fields = c(
            "initial_model",
            "parameter_scaling",
            "simulation_plan",
            "acceptance_criteria"
          ),
          suggested_workflow_stages = c(
            "prior_predictive",
            "fake_data",
            "small_fit",
            "full_fit"
          )
        )
      ))
    )
  }

  iteration_obligation <- bg_find_obligation(
    obligations,
    kind = "plan_workflow_iteration",
    scope = context$scope
  )
  if (!is.null(iteration_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = iteration_obligation,
        title = "Record workflow iteration plan",
        decision_type = "workflow_iteration_plan",
        why_now = paste0(
          "Fresh computational warnings should be turned into a concrete next ",
          "iteration instead of remaining as context-free diagnostics."
        ),
        payload = list(
          suggested_fields = c(
            "failure_mode",
            "simplification",
            "reparameterization",
            "fake_data_rehearsal",
            "next_check"
          )
        )
      ))
    )
  }

  stability_obligation <- bg_find_obligation(
    obligations,
    kind = "review_out_of_sample_stability",
    scope = context$scope
  )
  if (!is.null(stability_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = stability_obligation,
        title = "Record generalization review",
        decision_type = "workflow_generalization_review",
        why_now = paste0(
          "Cross-validation diagnostics are most useful when the workflow records ",
          "what follow-up they imply for comparison, influence, or refitting."
        ),
        payload = list(
          suggested_fields = c(
            "held_out_target",
            "pareto_k_follow_up",
            "comparison_action",
            "acceptance_rule"
          )
        )
      ))
    )
  }

  actions
}

bg_pack_taxonomy_obligations <- function(
  context,
  pack_config = list()
) {
  if (!startsWith(context$scope, "branch:")) {
    return(list())
  }

  fit_node_ids <- bg_pack_fit_node_ids(context)
  if (length(fit_node_ids) == 0 || is.null(bg_pack_goal_kind(context))) {
    return(list())
  }

  obligations <- list()

  if (
    !bg_pack_has_scope_decision(
      context,
      kind = "model_taxonomy",
      node_ids = fit_node_ids
    )
  ) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "classify_model_taxonomy",
        title = "Classify the branch in model-taxonomy terms",
        why = paste0(
          "The PAD taxonomy distinguishes whether the branch is acting mainly as ",
          "a P, PA, PD, or PAD model. Record that identity explicitly before later ",
          "workflow steps flatten important modeling assumptions."
        ),
        severity = "advisory",
        basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
        metadata = list(
          source_keys = "taxonomy",
          utility_dimensions = bg_pack_primary_utilities(context),
          pad_model_classes = c("P", "PA", "PD", "PAD")
        )
      ))
    )
  }

  if (
    !bg_pack_has_scope_decision(
      context,
      kind = "utility_tradeoff_review",
      node_ids = fit_node_ids
    )
  ) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_utility_tradeoffs",
        title = "Record utility priorities",
        why = paste0(
          "The model taxonomy paper treats model quality as a trade-off over ",
          "utilities. Record which utilities are primary for this branch and ",
          "which evaluations should dominate model criticism."
        ),
        severity = "advisory",
        basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
        metadata = list(
          source_keys = "taxonomy",
          utility_dimensions = bg_pack_taxonomy_utility_dimensions(),
          pad_model_classes = c("P", "PA", "PD", "PAD")
        )
      ))
    )
  }

  obligations
}

bg_pack_taxonomy_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  actions <- list()

  taxonomy_obligation <- bg_find_obligation(
    obligations,
    kind = "classify_model_taxonomy",
    scope = context$scope
  )
  if (!is.null(taxonomy_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = taxonomy_obligation,
        title = "Record model taxonomy classification",
        decision_type = "model_taxonomy",
        why_now = paste0(
          "Making the branch's PAD class explicit keeps later predictive, ",
          "calibration, and comparison summaries anchored to the intended model ",
          "identity."
        ),
        payload = list(
          allowed_pad_model_classes = c("P", "PA", "PD", "PAD"),
          suggested_fields = c(
            "model_class",
            "approximator",
            "training_data_role",
            "falsification_targets"
          ),
          suggested_primary_utilities = bg_pack_primary_utilities(context)
        )
      ))
    )
  }

  utility_obligation <- bg_find_obligation(
    obligations,
    kind = "review_utility_tradeoffs",
    scope = context$scope
  )
  if (!is.null(utility_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = utility_obligation,
        title = "Record utility trade-offs",
        decision_type = "utility_tradeoff_review",
        why_now = paste0(
          "The workflow should be clear about which utilities dominate when the ",
          "branch faces trade-offs between prediction, calibration, convergence, ",
          "and interpretability."
        ),
        payload = list(
          allowed_utility_dimensions = bg_pack_taxonomy_utility_dimensions(),
          suggested_primary_utilities = bg_pack_primary_utilities(context),
          suggested_evaluation_modes = bg_pack_taxonomy_evaluation_modes(
            context
          )
        )
      ))
    )
  }

  actions
}

bg_pack_causal_stan_obligations <- function(
  context,
  pack_config = list()
) {
  fit_node_ids <- bg_pack_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  obligations <- list()

  diagnostic_summaries <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "stan_diagnostic_review",
    summary_kinds = c("hmc_diagnostics", "optimizer_diagnostics"),
    node_ids = fit_node_ids
  )
  diagnostic_summaries <- bg_pack_failing_summaries(diagnostic_summaries)

  if (length(diagnostic_summaries) > 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_stan_diagnostics",
        title = "Review Stan diagnostics",
        why = paste0(
          "Stan-focused workflows need an explicit remediation note when HMC or ",
          "optimizer diagnostics fail. Record the dominant failure mode and the ",
          "repair plan before trusting downstream fit criticism or comparison."
        ),
        severity = bg_pack_review_severity(diagnostic_summaries),
        basis = bg_pack_summary_basis(diagnostic_summaries),
        metadata = list(
          source_keys = c("workflow_core", "stan_diagnostics"),
          summary_kinds = sort(unique(vapply(
            diagnostic_summaries,
            `[[`,
            character(1),
            "summary_kind"
          ))),
          repair_hints = bg_pack_causal_stan_repair_hints(diagnostic_summaries),
          utility_dimensions = c(
            "convergence",
            "parameter_recoverability",
            "robustness"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  projection_summaries <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "projection_selection_review",
    summary_kinds = c(
      "projpred_selection",
      "projection_predictive_selection"
    )
  )

  if (length(projection_summaries) > 0) {
    causal_contract <- bg_pack_current_causal_contract(context)

    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_projection_predictive_selection",
        title = "Review projection-predictive selection",
        why = paste0(
          "Projection-predictive selection can compress a Stan workflow, but the ",
          "selected submodel and its role in the broader workflow should still be ",
          "reviewed explicitly."
        ),
        severity = bg_pack_review_severity(projection_summaries),
        basis = bg_pack_summary_basis(projection_summaries),
        metadata = list(
          source_keys = c("model_comparison", "projection_predictive"),
          summary_kinds = sort(unique(vapply(
            projection_summaries,
            `[[`,
            character(1),
            "summary_kind"
          ))),
          causal_required_terms = causal_contract$required_terms %||%
            character(),
          causal_forbidden_terms = causal_contract$forbidden_terms %||%
            character(),
          causal_ranked_candidate_terms = causal_contract$ranked_candidate_terms %||%
            character(),
          utility_dimensions = c(
            "predictive_performance",
            "parsimony",
            "interpretability"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  obligations
}

bg_pack_causal_stan_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  actions <- list()

  diagnostic_obligation <- bg_find_obligation(
    obligations,
    kind = "review_stan_diagnostics",
    scope = context$scope
  )
  if (!is.null(diagnostic_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = diagnostic_obligation,
        title = "Record Stan diagnostic review",
        decision_type = "stan_diagnostic_review",
        why_now = paste0(
          "The Stan workflow benefits from a concrete repair note that names the ",
          "failure mode and the next sampler or optimizer intervention."
        ),
        payload = list(
          suggested_fields = c(
            "dominant_failure_mode",
            "repair_strategy",
            "rerun_plan",
            "acceptance_rule"
          ),
          suggested_repairs = diagnostic_obligation$metadata$repair_hints %||%
            character()
        )
      ))
    )
  }

  projection_obligation <- bg_find_obligation(
    obligations,
    kind = "review_projection_predictive_selection",
    scope = context$scope
  )
  if (!is.null(projection_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = projection_obligation,
        title = "Record projection-predictive review",
        decision_type = "projection_selection_review",
        why_now = paste0(
          "Projection-predictive summaries should end in an explicit decision ",
          "about the reference model, the selected submodel, and any refit or ",
          "comparison follow-up."
        ),
        payload = list(
          suggested_fields = c(
            "reference_model",
            "selected_submodel",
            "selection_rule",
            "refit_plan"
          ),
          required_terms = projection_obligation$metadata$causal_required_terms %||%
            character(),
          forbidden_terms = projection_obligation$metadata$causal_forbidden_terms %||%
            character(),
          ranked_candidate_terms = projection_obligation$metadata$causal_ranked_candidate_terms %||%
            character(),
          selection_policy = if (
            length(
              projection_obligation$metadata$causal_required_terms %||%
                character()
            ) >
              0 ||
              length(
                projection_obligation$metadata$causal_forbidden_terms %||%
                  character()
              ) >
                0
          ) {
            "lock required terms, exclude forbidden terms, and search only across ranked admissible candidates"
          } else {
            NULL
          }
        )
      ))
    )
  }

  actions
}


# Guides Workflow Pack - Helpers
# ----------------------------------
# Summary, taxonomy, and Stan diagnostic helper functions for the
# guides workflow pack.

bg_pack_summary_basis <- function(summaries, branch_ids = NULL) {
  summaries <- summaries %||% list()

  if (length(summaries) == 0) {
    return(list(
      node_ids = character(),
      summary_ids = character(),
      branch_ids = bg_pack_sort_ids(branch_ids)
    ))
  }

  list(
    node_ids = bg_pack_sort_ids(vapply(
      summaries,
      `[[`,
      character(1),
      "node_id"
    )),
    summary_ids = bg_pack_sort_ids(vapply(
      summaries,
      `[[`,
      character(1),
      "summary_id"
    )),
    branch_ids = bg_pack_sort_ids(branch_ids)
  )
}

bg_pack_failing_summaries <- function(summaries) {
  Filter(
    function(summary) {
      # An explicit severity is authoritative over a missing `passed` field
      # (a user executor may set severity without also setting
      # passed); fall back to `passed` only when severity is absent too.
      if (!is.null(summary$severity)) {
        return(summary$severity %in% c("warning", "error"))
      }

      !isTRUE(summary$passed)
    },
    summaries %||% list()
  )
}

bg_pack_taxonomy_utility_dimensions <- function() {
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

bg_pack_taxonomy_evaluation_modes <- function(context) {
  goal_kind <- bg_pack_goal_kind(context)
  if (is.null(goal_kind)) {
    goal_kind <- ""
  }

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

bg_pack_causal_stan_metric_value <- function(metrics, keys, default = NULL) {
  metrics <- metrics %||% list()

  for (key in keys) {
    value <- metrics[[key]] %||% NULL
    if (!is.null(value)) {
      return(value)
    }
  }

  default
}

bg_pack_causal_stan_repair_hints <- function(summaries) {
  hints <- character()

  for (summary in summaries %||% list()) {
    metrics <- summary$metrics %||% list()
    summary_kind <- summary$summary_kind %||% NULL

    if (identical(summary_kind, "hmc_diagnostics")) {
      if (
        isTRUE(bg_pack_causal_stan_metric_value(
          metrics,
          c("divergences_present", "divergent"),
          FALSE
        )) ||
          (bg_pack_causal_stan_metric_value(
            metrics,
            c("divergent_transitions", "divergences"),
            0
          ) >
            0)
      ) {
        hints <- c(hints, "reparameterize", "raise_adapt_delta")
      }

      if (
        isTRUE(bg_pack_causal_stan_metric_value(
          metrics,
          c("treedepth_saturated", "max_treedepth_exceeded"),
          FALSE
        )) ||
          (bg_pack_causal_stan_metric_value(
            metrics,
            c("max_treedepth_hits", "treedepth_hits"),
            0
          ) >
            0)
      ) {
        hints <- c(hints, "increase_max_treedepth")
      }

      if (
        (bg_pack_causal_stan_metric_value(
          metrics,
          c("max_rhat", "rhat_max"),
          1
        ) >
          1.01) ||
          (bg_pack_causal_stan_metric_value(
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
