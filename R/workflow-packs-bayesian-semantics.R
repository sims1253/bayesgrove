# Bayesian Semantics Workflow Pack - Obligations and Actions
# -----------------------------------------------------------
# Obligation and action constructors for phase-10 workflow phases.
# Helper functions are in workflow-packs-bayesian-helpers.R.

# --- Obligation and Action Constructors ---

bg_pack_obligation <- function(
  context,
  kind,
  title,
  why,
  severity = "blocking",
  basis = list(),
  metadata = list()
) {
  metadata <- utils::modifyList(
    list(hold_node_ids = character(), source_keys = character()),
    metadata %||% list()
  )

  basis <- bg_protocol_normalize_value(utils::modifyList(
    list(
      node_ids = character(),
      summary_ids = character(),
      decision_ids = character(),
      branch_ids = character()
    ),
    basis
  ))

  list(
    kind = kind,
    scope = context$scope,
    severity = severity,
    title = title,
    basis = basis,
    explanation = list(
      why = why,
      references = bg_workflow_references(
        source_keys = metadata$source_keys,
        references = metadata$references
      )
    ),
    metadata = metadata[setdiff(names(metadata), "references")]
  )
}

bg_pack_action <- function(
  context,
  kind,
  title,
  why_now,
  basis = list(),
  payload = list(),
  metadata = list()
) {
  metadata <- utils::modifyList(
    list(source_keys = character()),
    metadata %||% list()
  )

  list(
    kind = kind,
    scope = context$scope,
    title = title,
    basis = bg_protocol_normalize_value(basis %||% list()),
    payload = bg_protocol_normalize_value(payload %||% list()),
    explanation = list(
      why_now = why_now,
      references = bg_workflow_references(
        source_keys = metadata$source_keys,
        references = metadata$references
      )
    ),
    metadata = metadata[setdiff(names(metadata), "references")]
  )
}

bg_pack_check_action <- function(
  context,
  obligation,
  title,
  source_node_id = NULL,
  node_kind = "check",
  default_label_prefix,
  why_now,
  metadata = list()
) {
  metadata <- utils::modifyList(
    metadata %||% list(),
    list(
      source_keys = unique(c(
        obligation$metadata$source_keys %||% character(),
        (metadata %||% list())$source_keys %||% character()
      )),
      references = unique(c(
        obligation$explanation$references %||% character(),
        (metadata %||% list())$references %||% character()
      ))
    )
  )

  source_node_id <- source_node_id %||%
    bg_pack_source_node_id(context, obligation$basis$node_ids)
  if (is.null(source_node_id)) {
    return(NULL)
  }

  bg_pack_action(
    context = context,
    kind = "create_node_from_template",
    title = title,
    why_now = why_now,
    basis = list(
      obligation_refs = list(list(
        kind = obligation$kind,
        scope = obligation$scope
      )),
      node_ids = obligation$basis$node_ids %||% source_node_id,
      summary_ids = obligation$basis$summary_ids %||% character()
    ),
    payload = list(
      template_ref = "diagnostic_check",
      source_node_id = source_node_id,
      node_kind = node_kind,
      default_label = paste(
        default_label_prefix,
        bg_pack_node_label(context, source_node_id)
      )
    ),
    metadata = metadata
  )
}

bg_pack_review_action <- function(
  context,
  obligation,
  title,
  decision_type,
  why_now,
  payload = list(),
  metadata = list()
) {
  metadata <- utils::modifyList(
    metadata %||% list(),
    list(
      source_keys = unique(c(
        obligation$metadata$source_keys %||% character(),
        (metadata %||% list())$source_keys %||% character()
      )),
      references = unique(c(
        obligation$explanation$references %||% character(),
        (metadata %||% list())$references %||% character()
      ))
    )
  )

  bg_pack_action(
    context = context,
    kind = "record_decision",
    title = title,
    why_now = why_now,
    basis = list(
      obligation_refs = list(list(
        kind = obligation$kind,
        scope = obligation$scope
      )),
      node_ids = obligation$basis$node_ids %||% character(),
      summary_ids = obligation$basis$summary_ids %||% character()
    ),
    payload = utils::modifyList(
      list(
        template_ref = "review_decision",
        decision_type = decision_type,
        node_ids = obligation$basis$node_ids %||% character(),
        summary_ids = obligation$basis$summary_ids %||% character(),
        branch_ids = obligation$basis$branch_ids %||% character()
      ),
      payload
    ),
    metadata = metadata
  )
}

# --- Prior Workflow Pack ---

bg_pack_prior_obligations <- function(
  context,
  pack_config = list()
) {
  fit_node_ids <- bg_pack_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  obligations <- list()

  prior_spec_summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = "prior_spec",
    node_ids = fit_node_ids
  )
  if (
    length(prior_spec_summaries) == 0 &&
      !bg_pack_has_scope_decision(
        context,
        kind = "prior_rationale",
        node_ids = fit_node_ids
      )
  ) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "record_prior_rationale",
        title = "Record prior rationale",
        why = paste0(
          "This workflow uses the PAD taxonomy and utility vocabulary. ",
          "Before trusting downstream PD or PAD results, record how the ",
          "current priors support the branch's inferential goal."
        ),
        basis = list(node_ids = fit_node_ids),
        metadata = list(
          source_keys = c("workflow_core", "prior_predictive", "taxonomy"),
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

  prior_check_summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = "prior_predictive_check"
  )
  if (length(prior_check_summaries) == 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "run_prior_predictive_check",
        title = "Run prior predictive checks",
        why = paste0(
          "Prior predictive checks test whether the current P-model assumptions ",
          "generate plausible observables before posterior updating."
        ),
        basis = list(node_ids = fit_node_ids),
        metadata = list(
          source_keys = c("workflow_core", "prior_predictive"),
          summary_kinds = "prior_predictive_check",
          utility_dimensions = c(
            "structural_faithfulness",
            "predictive_performance"
          ),
          pad_model_classes = c("P", "PA")
        )
      ))
    )
  }

  pending_prior_reviews <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "prior_check_review",
    summary_kinds = "prior_predictive_check"
  )
  if (length(pending_prior_reviews) > 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_prior_predictive_check",
        title = "Review prior predictive evidence",
        why = paste0(
          "Fresh prior predictive summaries should be reviewed explicitly before ",
          "treating the current prior specification as adequate."
        ),
        severity = bg_pack_review_severity(pending_prior_reviews),
        basis = list(
          node_ids = fit_node_ids,
          summary_ids = bg_pack_sort_ids(vapply(
            pending_prior_reviews,
            `[[`,
            character(1),
            "summary_id"
          ))
        ),
        metadata = list(
          source_keys = c("workflow_core", "prior_predictive"),
          summary_kinds = "prior_predictive_check",
          utility_dimensions = c(
            "structural_faithfulness",
            "predictive_performance"
          ),
          pad_model_classes = c("P", "PA")
        )
      ))
    )
  }

  obligations
}

bg_pack_prior_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  actions <- list()

  rationale_obligation <- bg_find_obligation(
    obligations,
    kind = "record_prior_rationale",
    scope = context$scope
  )
  if (!is.null(rationale_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = rationale_obligation,
        title = "Record prior rationale",
        decision_type = "prior_rationale",
        why_now = paste0(
          "The prior workflow stays incomplete until the branch explains how the ",
          "current priors relate to its inferential goal."
        ),
        payload = list(
          pad_model_classes = c("P", "PD", "PAD"),
          utility_dimensions = c(
            "structural_faithfulness",
            "robustness",
            "interpretability"
          )
        )
      ))
    )
  }

  prior_check_obligation <- bg_find_obligation(
    obligations,
    kind = "run_prior_predictive_check",
    scope = context$scope
  )
  if (!is.null(prior_check_obligation)) {
    action <- bg_pack_check_action(
      context = context,
      obligation = prior_check_obligation,
      title = "Create prior predictive check",
      node_kind = pack_config$prior_check_node_kind %||% "prior_check",
      default_label_prefix = "Prior predictive:",
      why_now = paste0(
        "Create a lightweight prior predictive node so the workflow can record ",
        "plain-data evidence about prior behavior."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  review_obligation <- bg_find_obligation(
    obligations,
    kind = "review_prior_predictive_check",
    scope = context$scope
  )
  if (!is.null(review_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = review_obligation,
        title = "Review prior predictive evidence",
        decision_type = "prior_check_review",
        why_now = paste0(
          "Current prior predictive evidence should be accepted, rejected, or ",
          "qualified explicitly before the branch moves on."
        )
      ))
    )
  }

  actions
}

# --- Model Checks Pack ---

bg_pack_checks_obligations <- function(
  context,
  pack_config = list()
) {
  fit_node_ids <- bg_pack_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  obligations <- list()

  posterior_summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = "posterior_predictive_check"
  )
  if (length(posterior_summaries) == 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "run_posterior_predictive_check",
        title = "Run posterior predictive checks",
        why = paste0(
          "Posterior predictive checks evaluate whether the current PAD model ",
          "captures relevant observable structure after fitting."
        ),
        basis = list(node_ids = fit_node_ids),
        metadata = list(
          source_keys = c("workflow_core", "posterior_predictive"),
          summary_kinds = "posterior_predictive_check",
          utility_dimensions = c(
            "predictive_performance",
            "structural_faithfulness"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  pending_posterior_reviews <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "posterior_check_review",
    summary_kinds = "posterior_predictive_check"
  )
  if (length(pending_posterior_reviews) > 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_posterior_predictive_check",
        title = "Review posterior predictive evidence",
        why = paste0(
          "Fresh posterior predictive summaries should be reviewed explicitly so ",
          "the workflow records how the current fit fares against the observed ",
          "data structure."
        ),
        severity = bg_pack_review_severity(pending_posterior_reviews),
        basis = list(
          node_ids = fit_node_ids,
          summary_ids = bg_pack_sort_ids(vapply(
            pending_posterior_reviews,
            `[[`,
            character(1),
            "summary_id"
          ))
        ),
        metadata = list(
          source_keys = c("workflow_core", "posterior_predictive"),
          summary_kinds = "posterior_predictive_check",
          utility_dimensions = c(
            "predictive_performance",
            "structural_faithfulness"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  loo_pit_summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = "loo_pit_calibration"
  )
  if (length(loo_pit_summaries) == 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "run_loo_pit_calibration",
        title = "Run LOO-PIT calibration checks",
        why = paste0(
          "Bayesian workflow treats leave-one-out predictive calibration as a ",
          "separate model-criticism step. Record LOO-PIT evidence so the workflow ",
          "can distinguish distributional fit from conditional predictive ",
          "calibration."
        ),
        basis = list(node_ids = fit_node_ids),
        metadata = list(
          source_keys = c("workflow_core", "loo_pit"),
          summary_kinds = "loo_pit_calibration",
          utility_dimensions = c(
            "predictive_performance",
            "structural_faithfulness",
            "robustness"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  pending_loo_pit_reviews <- bg_pack_pending_review_summaries(
    context,
    decision_kind = "loo_pit_review",
    summary_kinds = "loo_pit_calibration"
  )
  if (length(pending_loo_pit_reviews) > 0) {
    obligations <- c(
      obligations,
      list(bg_pack_obligation(
        context = context,
        kind = "review_loo_pit_calibration",
        title = "Review LOO-PIT calibration",
        why = paste0(
          "Fresh LOO-PIT summaries should be reviewed explicitly so the workflow ",
          "records whether conditional predictive calibration is acceptable or ",
          "requires model revision."
        ),
        severity = bg_pack_review_severity(pending_loo_pit_reviews),
        basis = list(
          node_ids = fit_node_ids,
          summary_ids = bg_pack_sort_ids(vapply(
            pending_loo_pit_reviews,
            `[[`,
            character(1),
            "summary_id"
          ))
        ),
        metadata = list(
          source_keys = c("workflow_core", "loo_pit"),
          summary_kinds = "loo_pit_calibration",
          utility_dimensions = c(
            "predictive_performance",
            "structural_faithfulness",
            "robustness"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  sbc_goal_kinds <- pack_config$sbc_goal_kinds %||% "latent_inference"
  if (bg_pack_goal_kind_allowed(context, allowed_goal_kinds = sbc_goal_kinds)) {
    sbc_summaries <- bg_pack_fresh_summaries(
      context,
      summary_kinds = "sbc_result"
    )

    if (length(sbc_summaries) == 0) {
      obligations <- c(
        obligations,
        list(bg_pack_obligation(
          context = context,
          kind = "run_sbc",
          title = "Run simulation-based calibration",
          why = paste0(
            "For latent inferential goals, the PAD framework treats calibration of ",
            "the posterior approximation as a primary concern. Record an SBC run ",
            "before trusting parameter recovery claims."
          ),
          basis = list(node_ids = fit_node_ids),
          metadata = list(
            source_keys = c("workflow_core", "sbc", "taxonomy"),
            summary_kinds = "sbc_result",
            utility_dimensions = c(
              "parameter_recoverability",
              "convergence",
              "robustness"
            ),
            pad_model_classes = c("PA", "PAD")
          )
        ))
      )
    }

    pending_sbc_reviews <- bg_pack_pending_review_summaries(
      context,
      decision_kind = "sbc_review",
      summary_kinds = "sbc_result"
    )
    if (length(pending_sbc_reviews) > 0) {
      obligations <- c(
        obligations,
        list(bg_pack_obligation(
          context = context,
          kind = "review_sbc",
          title = "Review SBC evidence",
          why = paste0(
            "Fresh SBC summaries should be reviewed explicitly before treating ",
            "the approximate posterior as trustworthy for latent inference."
          ),
          severity = bg_pack_review_severity(pending_sbc_reviews),
          basis = list(
            node_ids = fit_node_ids,
            summary_ids = bg_pack_sort_ids(vapply(
              pending_sbc_reviews,
              `[[`,
              character(1),
              "summary_id"
            ))
          ),
          metadata = list(
            source_keys = c("workflow_core", "sbc", "taxonomy"),
            summary_kinds = "sbc_result",
            utility_dimensions = c(
              "parameter_recoverability",
              "convergence",
              "robustness"
            ),
            pad_model_classes = c("PA", "PAD")
          )
        ))
      )
    }
  }

  obligations
}

bg_pack_checks_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  actions <- list()

  posterior_check_obligation <- bg_find_obligation(
    obligations,
    kind = "run_posterior_predictive_check",
    scope = context$scope
  )
  if (!is.null(posterior_check_obligation)) {
    action <- bg_pack_check_action(
      context = context,
      obligation = posterior_check_obligation,
      title = "Create posterior predictive check",
      node_kind = pack_config$posterior_check_node_kind %||% "ppc",
      default_label_prefix = "Posterior predictive:",
      why_now = paste0(
        "Create a posterior predictive node so the workflow can consume ",
        "plain-data posterior check summaries instead of reopening fit objects."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  posterior_review_obligation <- bg_find_obligation(
    obligations,
    kind = "review_posterior_predictive_check",
    scope = context$scope
  )
  if (!is.null(posterior_review_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = posterior_review_obligation,
        title = "Review posterior predictive evidence",
        decision_type = "posterior_check_review",
        why_now = paste0(
          "The current posterior predictive evidence should be reviewed before ",
          "the branch is treated as an acceptable PAD model."
        )
      ))
    )
  }

  loo_pit_obligation <- bg_find_obligation(
    obligations,
    kind = "run_loo_pit_calibration",
    scope = context$scope
  )
  if (!is.null(loo_pit_obligation)) {
    action <- bg_pack_check_action(
      context = context,
      obligation = loo_pit_obligation,
      title = "Create LOO-PIT calibration check",
      node_kind = pack_config$loo_pit_node_kind %||% "calibration",
      default_label_prefix = "LOO-PIT:",
      why_now = paste0(
        "Create a calibration node so the workflow can capture leave-one-out ",
        "predictive calibration summaries as plain data."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  loo_pit_review_obligation <- bg_find_obligation(
    obligations,
    kind = "review_loo_pit_calibration",
    scope = context$scope
  )
  if (!is.null(loo_pit_review_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = loo_pit_review_obligation,
        title = "Review LOO-PIT calibration",
        decision_type = "loo_pit_review",
        why_now = paste0(
          "The current LOO-PIT evidence should be reviewed before the workflow ",
          "treats the model's conditional predictive calibration as acceptable."
        )
      ))
    )
  }

  sbc_obligation <- bg_find_obligation(
    obligations,
    kind = "run_sbc",
    scope = context$scope
  )
  if (!is.null(sbc_obligation)) {
    action <- bg_pack_check_action(
      context = context,
      obligation = sbc_obligation,
      title = "Create SBC check",
      node_kind = pack_config$sbc_node_kind %||% "sbc",
      default_label_prefix = "SBC:",
      why_now = paste0(
        "Create an SBC node so the workflow can record calibration summaries ",
        "for the current approximation strategy."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  sbc_review_obligation <- bg_find_obligation(
    obligations,
    kind = "review_sbc",
    scope = context$scope
  )
  if (!is.null(sbc_review_obligation)) {
    actions <- c(
      actions,
      list(bg_pack_review_action(
        context = context,
        obligation = sbc_review_obligation,
        title = "Review SBC evidence",
        decision_type = "sbc_review",
        why_now = paste0(
          "Current SBC evidence should be reviewed explicitly before latent ",
          "recovery claims are treated as calibrated."
        )
      ))
    )
  }

  actions
}

# --- Model Selection Pack ---

bg_pack_selection_summary_kinds <- function() {
  c("model_comparison", "stacking_weights", "comparison_results")
}

bg_pack_selection_evidence <- function(context, candidate_basis) {
  if (is.null(candidate_basis) || length(candidate_basis$node_ids) < 2) {
    return(NULL)
  }

  nodes <- bg_default_bayesian_all_nodes(context)
  edges <- bg_default_bayesian_all_edges(context)
  summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = bg_pack_selection_summary_kinds(),
    include_cross_scope = TRUE
  )
  comparison_nodes <- Filter(
    function(node) (node$kind %||% NULL) %in% c("compare", "comparison"),
    nodes
  )
  if (length(comparison_nodes) == 0 || length(summaries) == 0) {
    return(NULL)
  }

  fit_node_ids <- sort(candidate_basis$node_ids)
  matches <- list()

  for (node_id in sort(names(comparison_nodes))) {
    input_node_ids <- sort(unique(vapply(
      Filter(function(edge) identical(edge$to %||% NULL, node_id), edges),
      `[[`,
      character(1),
      "from"
    )))
    if (!identical(input_node_ids, fit_node_ids)) {
      next
    }

    node_summaries <- Filter(
      function(summary) identical(summary$node_id %||% NULL, node_id),
      summaries
    )
    if (length(node_summaries) == 0) {
      next
    }

    matches[[node_id]] <- list(
      node_id = node_id,
      label = comparison_nodes[[node_id]]$label %||% node_id,
      summary_ids = bg_pack_sort_ids(vapply(
        node_summaries,
        `[[`,
        character(1),
        "summary_id"
      )),
      summary_kinds = bg_pack_sort_ids(vapply(
        node_summaries,
        `[[`,
        character(1),
        "summary_kind"
      )),
      latest_at = max(vapply(
        node_summaries,
        `[[`,
        character(1),
        "created_at"
      ))
    )
  }

  if (length(matches) == 0) {
    return(NULL)
  }

  ordering <- order(
    vapply(matches, `[[`, character(1), "latest_at"),
    names(matches),
    decreasing = TRUE
  )
  matches[[ordering[[1]]]]
}

bg_pack_selection_obligations <- function(
  context,
  pack_config = list()
) {
  if (!identical(context$scope, "project")) {
    return(list())
  }

  candidate_basis <- bg_default_bayesian_candidate_basis(
    bg_default_bayesian_clean_fit_candidates(context)
  )
  if (is.null(candidate_basis) || length(candidate_basis$node_ids) < 2) {
    return(list())
  }

  comparison_evidence <- bg_pack_selection_evidence(
    context,
    candidate_basis
  )
  comparison_context <- bg_default_bayesian_comparison_context(
    candidate_basis,
    comparison_evidence
  )

  has_decision <- any(vapply(
    bg_default_bayesian_all_decisions(context),
    bg_default_bayesian_model_comparison_matches,
    logical(1),
    comparison_context = comparison_context
  ))
  if (has_decision) {
    return(list())
  }

  list(bg_pack_obligation(
    context = context,
    kind = "review_model_selection",
    title = "Review model comparison evidence",
    why = paste0(
      "The PAD workflow now treats model comparison and stacking as explicit ",
      "summary-driven evidence. Compare the clean candidate fits, then record ",
      "a decision using predictive performance and parsimony as the guiding ",
      "utility dimensions."
    ),
    basis = list(
      node_ids = candidate_basis$node_ids,
      branch_ids = candidate_basis$branch_ids,
      summary_ids = candidate_basis$summary_ids
    ),
    metadata = list(
      source_keys = c("workflow_core", "model_comparison", "stacking"),
      candidate_signature = candidate_basis$candidate_signature,
      comparison_context = comparison_context,
      comparison_signature = comparison_context$comparison_signature,
      comparison_summary_ids = comparison_evidence$summary_ids %||% character(),
      comparison_summary_kinds = comparison_evidence$summary_kinds %||%
        character(),
      utility_dimensions = c("predictive_performance", "parsimony"),
      pad_model_classes = c("PD", "PAD")
    )
  ))
}

bg_pack_selection_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  if (!identical(context$scope, "project")) {
    return(list())
  }

  obligation <- bg_find_obligation(
    obligations,
    kind = "review_model_selection",
    scope = context$scope
  )
  if (is.null(obligation)) {
    return(list())
  }

  candidate_basis <- bg_default_bayesian_obligation_candidate_basis(obligation)
  comparison_evidence <- bg_pack_selection_evidence(
    context,
    candidate_basis
  )
  if (is.null(comparison_evidence)) {
    nodes <- bg_default_bayesian_all_nodes(context)
    fit_labels <- vapply(
      candidate_basis$node_ids,
      function(node_id) {
        node <- nodes[[node_id]] %||% list()
        node$label %||% node_id
      },
      character(1)
    )

    return(list(bg_pack_action(
      context = context,
      kind = "create_node_from_template",
      title = "Create model comparison node",
      why_now = paste0(
        "No fresh model-comparison summaries exist yet. Create the comparison ",
        "node so the workflow can evaluate model_comparison or stacking_weights ",
        "records as plain data."
      ),
      basis = list(
        obligation_refs = list(list(
          kind = obligation$kind,
          scope = obligation$scope
        )),
        node_ids = candidate_basis$node_ids,
        summary_ids = obligation$basis$summary_ids %||% character()
      ),
      payload = list(
        template_ref = "branch_comparison",
        inputs = candidate_basis$node_ids,
        default_label = paste(
          "Compare:",
          paste(fit_labels, collapse = " vs ")
        )
      ),
      metadata = list(
        source_keys = obligation$metadata$source_keys %||% character(),
        references = obligation$explanation$references %||% character()
      )
    )))
  }

  comparison_context <- bg_default_bayesian_comparison_context(
    candidate_basis,
    comparison_evidence
  )
  has_stacking <- "stacking_weights" %in%
    (comparison_evidence$summary_kinds %||% character())

  list(bg_pack_review_action(
    context = context,
    obligation = obligation,
    title = if (has_stacking) {
      "Record model comparison and stacking review"
    } else {
      "Record model comparison review"
    },
    decision_type = "model_comparison",
    why_now = if (has_stacking) {
      paste0(
        "Current comparison summaries include stacking weights. Record how the ",
        "project interprets the predictive evidence before selecting a working ",
        "model family."
      )
    } else {
      paste0(
        "Current comparison summaries support an explicit model-comparison ",
        "decision based on predictive performance and parsimony."
      )
    },
    payload = list(
      fit_node_ids = candidate_basis$node_ids,
      branch_ids = candidate_basis$branch_ids,
      summary_ids = bg_pack_sort_ids(c(
        candidate_basis$summary_ids,
        comparison_evidence$summary_ids %||% character()
      )),
      comparison_context = comparison_context,
      comparison_signature = comparison_context$comparison_signature,
      candidate_signature = comparison_context$candidate_signature,
      utility_dimensions = c("predictive_performance", "parsimony"),
      stacking_available = has_stacking
    )
  ))
}

# --- Causal Minimal Pack ---

bg_pack_causal_minimal_obligations <- function(
  context,
  pack_config = list()
) {
  if (!startsWith(context$scope, "branch:")) {
    return(list())
  }

  allowed_goal_kinds <- pack_config$goal_kinds %||% "latent_inference"
  if (!bg_pack_goal_kind_allowed(context, allowed_goal_kinds)) {
    return(list())
  }

  fit_node_ids <- bg_pack_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  causal_summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = "causal_framing",
    node_ids = fit_node_ids
  )
  if (
    length(causal_summaries) > 0 ||
      bg_pack_has_scope_decision(
        context,
        kind = "causal_question",
        node_ids = fit_node_ids
      )
  ) {
    return(list())
  }

  list(bg_pack_obligation(
    context = context,
    kind = "frame_causal_question",
    title = "Record causal framing",
    why = paste0(
      "This is an honest scaffold, not a full causal framework. If this branch ",
      "will support causal claims, record the causal question, estimand, or DAG ",
      "review before treating the analysis as causally consistent."
    ),
    severity = "advisory",
    basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
    metadata = list(
      source_keys = c("taxonomy", "causal_scaffold"),
      utility_dimensions = "causal_consistency",
      pad_model_classes = c("P", "PD", "PAD")
    )
  ))
}

bg_pack_causal_minimal_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  obligation <- bg_find_obligation(
    obligations,
    kind = "frame_causal_question",
    scope = context$scope
  )
  if (is.null(obligation)) {
    return(list())
  }

  list(bg_pack_review_action(
    context = context,
    obligation = obligation,
    title = "Record causal question or estimand",
    decision_type = "causal_question",
    why_now = paste0(
      "Capture the branch's causal framing explicitly so later workflow packs ",
      "can distinguish causal claims from purely predictive ones."
    ),
    payload = list(
      suggested_fields = c("causal_question", "estimand", "dag_review"),
      utility_dimensions = "causal_consistency"
    )
  ))
}

# --- PAD Scaffold Pack ---

bg_pack_taxonomy_pad_obligations <- function(
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

  pad_summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = "pad_annotation",
    node_ids = fit_node_ids
  )
  if (
    length(pad_summaries) > 0 ||
      bg_pack_has_scope_decision(
        context,
        kind = "pad_annotation_review",
        node_ids = fit_node_ids
      )
  ) {
    return(list())
  }

  list(bg_pack_obligation(
    context = context,
    kind = "review_pad_annotation",
    title = "Record PAD taxonomy annotation",
    why = paste0(
      "This scaffold keeps the PAD taxonomy explicit without pretending to be a ",
      "complete ontology. Record how this branch should be described in PAD and ",
      "utility-language terms."
    ),
    severity = "advisory",
    basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
    metadata = list(
      source_keys = "taxonomy",
      utility_dimensions = bg_pack_primary_utilities(context),
      pad_model_classes = c("P", "PA", "PD", "PAD")
    )
  ))
}

bg_pack_taxonomy_pad_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  obligation <- bg_find_obligation(
    obligations,
    kind = "review_pad_annotation",
    scope = context$scope
  )
  if (is.null(obligation)) {
    return(list())
  }

  list(bg_pack_review_action(
    context = context,
    obligation = obligation,
    title = "Record PAD annotation",
    decision_type = "pad_annotation_review",
    why_now = paste0(
      "Classifying the branch in PAD terms makes later protocol extensions more ",
      "transparent without claiming a full ontology implementation today."
    ),
    payload = list(
      allowed_pad_model_classes = c("P", "PA", "PD", "PAD"),
      suggested_primary_utilities = bg_pack_primary_utilities(context)
    )
  ))
}
