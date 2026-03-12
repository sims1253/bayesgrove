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
    node_ids = bg_phase10_sort_ids(vapply(summaries, `[[`, character(1), "node_id")),
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

bg_phase10_process_guidance_obligations <- function(
  context,
  pack_config = list()
) {
  fit_node_ids <- bg_phase10_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  if (startsWith(context$scope, "branch:") && is.null(bg_phase10_goal_kind(context))) {
    return(list())
  }

  obligations <- list()

  if (!bg_phase10_has_scope_decision(
    context,
    kind = "workflow_preflight",
    node_ids = fit_node_ids
  )) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
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
          references = "Gelman et al. (2020)",
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

  diagnostic_summaries <- bg_phase10_pending_review_summaries(
    context,
    decision_kind = "workflow_iteration_plan",
    summary_kinds = c("hmc_diagnostics", "optimizer_diagnostics"),
    node_ids = fit_node_ids
  )
  diagnostic_summaries <- bg_phase10_failing_summaries(diagnostic_summaries)

  if (length(diagnostic_summaries) > 0) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "plan_workflow_iteration",
        title = "Record the next workflow iteration",
        why = paste0(
          "Bayesian workflow is iterative. When computation raises fresh Stan or ",
          "optimizer warnings, record the simplification, reparameterization, or ",
          "rehearsal step that should happen next."
        ),
        severity = bg_phase10_review_severity(diagnostic_summaries),
        basis = bg_phase10_summary_basis(diagnostic_summaries),
        metadata = list(
          references = "Gelman et al. (2020)",
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

  generalization_summaries <- bg_phase10_pending_review_summaries(
    context,
    decision_kind = "workflow_generalization_review",
    summary_kinds = c("loo_diagnostics", "pareto_k_diagnostics")
  )

  if (length(generalization_summaries) > 0) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "review_out_of_sample_stability",
        title = "Review out-of-sample stability",
        why = paste0(
          "Cross-validation and Pareto-k diagnostics should be interpreted as part ",
          "of the workflow, not as final answers. Record what the current ",
          "generalization evidence says and what follow-up it requires."
        ),
        severity = bg_phase10_review_severity(generalization_summaries),
        basis = bg_phase10_summary_basis(generalization_summaries),
        metadata = list(
          references = "Gelman et al. (2020)",
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

bg_phase10_process_guidance_actions <- function(
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
      list(bg_phase10_review_action(
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
      list(bg_phase10_review_action(
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
      list(bg_phase10_review_action(
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

bg_phase10_model_taxonomy_obligations <- function(
  context,
  pack_config = list()
) {
  if (!startsWith(context$scope, "branch:")) {
    return(list())
  }

  fit_node_ids <- bg_phase10_fit_node_ids(context)
  if (length(fit_node_ids) == 0 || is.null(bg_phase10_goal_kind(context))) {
    return(list())
  }

  obligations <- list()

  if (!bg_phase10_has_scope_decision(
    context,
    kind = "model_taxonomy",
    node_ids = fit_node_ids
  )) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
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
          references = "Bürkner et al. (2023)",
          utility_dimensions = bg_phase10_primary_utilities(context),
          pad_model_classes = c("P", "PA", "PD", "PAD")
        )
      ))
    )
  }

  if (!bg_phase10_has_scope_decision(
    context,
    kind = "utility_tradeoff_review",
    node_ids = fit_node_ids
  )) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
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
          references = "Bürkner et al. (2023)",
          utility_dimensions = bg_phase10_taxonomy_utility_dimensions(),
          pad_model_classes = c("P", "PA", "PD", "PAD")
        )
      ))
    )
  }

  obligations
}

bg_phase10_model_taxonomy_actions <- function(
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
      list(bg_phase10_review_action(
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
          suggested_primary_utilities = bg_phase10_primary_utilities(context)
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
      list(bg_phase10_review_action(
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
          allowed_utility_dimensions = bg_phase10_taxonomy_utility_dimensions(),
          suggested_primary_utilities = bg_phase10_primary_utilities(context),
          suggested_evaluation_modes = bg_phase10_taxonomy_evaluation_modes(context)
        )
      ))
    )
  }

  actions
}
