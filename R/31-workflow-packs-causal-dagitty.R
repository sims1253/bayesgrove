bg_phase10_causal_dagitty_obligations <- function(
  context,
  pack_config = list()
) {
  if (!startsWith(context$scope, "branch:")) {
    return(list())
  }

  allowed_goal_kinds <- pack_config$goal_kinds %||% "latent_inference"
  if (!bg_phase10_goal_kind_allowed(context, allowed_goal_kinds)) {
    return(list())
  }

  fit_node_ids <- bg_phase10_fit_node_ids(context)
  if (length(fit_node_ids) == 0) {
    return(list())
  }

  obligations <- list()

  adjustment_summaries <- bg_phase10_fresh_summaries(
    context,
    summary_kinds = "dagitty_adjustment"
  )
  has_adjustment_review <- bg_phase10_has_scope_decision(
    context,
    kind = "causal_adjustment_review",
    node_ids = fit_node_ids
  )

  if (length(adjustment_summaries) == 0 && !has_adjustment_review) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "derive_causal_adjustment",
        title = "Derive a DAG-based adjustment strategy",
        why = paste0(
          "If this branch aims at causal interpretation, record an identification ",
          "strategy from a DAG-based adjustment analysis before treating the fit ",
          "as causally informative."
        ),
        basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
        metadata = list(
          source_keys = c("taxonomy", "causal_scaffold", "causal_identification"),
          summary_kinds = "dagitty_adjustment",
          utility_dimensions = "causal_consistency",
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  pending_adjustment_reviews <- bg_phase10_pending_review_summaries(
    context,
    decision_kind = "causal_adjustment_review",
    summary_kinds = "dagitty_adjustment"
  )
  if (length(pending_adjustment_reviews) > 0) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "review_causal_adjustment",
        title = "Review the DAG-based adjustment set",
        why = paste0(
          "Fresh DAG-derived adjustment summaries should be reviewed explicitly ",
          "before the branch commits to an estimand or adjustment strategy."
        ),
        severity = bg_phase10_review_severity(pending_adjustment_reviews),
        basis = list(
          node_ids = fit_node_ids,
          summary_ids = bg_phase10_sort_ids(vapply(
            pending_adjustment_reviews,
            `[[`,
            character(1),
            "summary_id"
          )),
          branch_ids = context$scope
        ),
        metadata = list(
          source_keys = c("taxonomy", "causal_scaffold", "causal_identification"),
          summary_kinds = "dagitty_adjustment",
          utility_dimensions = "causal_consistency",
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  implication_summaries <- bg_phase10_fresh_summaries(
    context,
    summary_kinds = "dagitty_implications"
  )
  has_implication_review <- bg_phase10_has_scope_decision(
    context,
    kind = "causal_implication_review",
    node_ids = fit_node_ids
  )

  if (
    (length(adjustment_summaries) > 0 || has_adjustment_review) &&
      length(implication_summaries) == 0 &&
      !has_implication_review
  ) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "check_causal_implications",
        title = "Check DAG implications",
        why = paste0(
          "A DAG should also be checked through its implied conditional ",
          "independencies so the workflow records what evidence could falsify the ",
          "causal story."
        ),
        severity = "advisory",
        basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
        metadata = list(
          source_keys = c("causal_scaffold", "causal_identification"),
          summary_kinds = "dagitty_implications",
          utility_dimensions = c("causal_consistency", "structural_faithfulness"),
          pad_model_classes = c("P", "PD", "PAD")
        )
      ))
    )
  }

  pending_implication_reviews <- bg_phase10_pending_review_summaries(
    context,
    decision_kind = "causal_implication_review",
    summary_kinds = "dagitty_implications"
  )
  if (length(pending_implication_reviews) > 0) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "review_causal_implications",
        title = "Review DAG implications",
        why = paste0(
          "Fresh implied-independence summaries should be reviewed so the branch ",
          "records which qualitative predictions the causal graph actually makes."
        ),
        severity = bg_phase10_review_severity(pending_implication_reviews),
        basis = list(
          node_ids = fit_node_ids,
          summary_ids = bg_phase10_sort_ids(vapply(
            pending_implication_reviews,
            `[[`,
            character(1),
            "summary_id"
          )),
          branch_ids = context$scope
        ),
        metadata = list(
          source_keys = c("causal_scaffold", "causal_identification"),
          summary_kinds = "dagitty_implications",
          utility_dimensions = c("causal_consistency", "structural_faithfulness"),
          pad_model_classes = c("P", "PD", "PAD")
        )
      ))
    )
  }

  obligations
}

bg_phase10_causal_dagitty_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  actions <- list()

  adjustment_obligation <- bg_find_obligation(
    obligations,
    kind = "derive_causal_adjustment",
    scope = context$scope
  )
  if (!is.null(adjustment_obligation)) {
    action <- bg_phase10_check_action(
      context = context,
      obligation = adjustment_obligation,
      title = "Create DAG-based adjustment check",
      node_kind = pack_config$adjustment_node_kind %||% "dagitty_adjustment",
      default_label_prefix = "Adjustment set:",
      why_now = paste0(
        "Create a dagitty-backed adjustment node so the workflow can record a ",
        "candidate identification strategy as plain data."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  review_adjustment_obligation <- bg_find_obligation(
    obligations,
    kind = "review_causal_adjustment",
    scope = context$scope
  )
  if (!is.null(review_adjustment_obligation)) {
    actions <- c(
      actions,
      list(bg_phase10_review_action(
        context = context,
        obligation = review_adjustment_obligation,
        title = "Record causal adjustment review",
        decision_type = "causal_adjustment_review",
        why_now = paste0(
          "The workflow should record which adjustment set, if any, will anchor ",
          "the branch's causal estimand."
        ),
        payload = list(
          suggested_fields = c(
            "estimand",
            "adjustment_set",
            "identification_status",
            "notes"
          )
        )
      ))
    )
  }

  implication_obligation <- bg_find_obligation(
    obligations,
    kind = "check_causal_implications",
    scope = context$scope
  )
  if (!is.null(implication_obligation)) {
    action <- bg_phase10_check_action(
      context = context,
      obligation = implication_obligation,
      title = "Create DAG implication check",
      node_kind = pack_config$implications_node_kind %||% "dagitty_implications",
      default_label_prefix = "DAG implications:",
      why_now = paste0(
        "Create a dagitty-backed implication node so the workflow can record the ",
        "graph's implied conditional independencies and test targets."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  review_implication_obligation <- bg_find_obligation(
    obligations,
    kind = "review_causal_implications",
    scope = context$scope
  )
  if (!is.null(review_implication_obligation)) {
    actions <- c(
      actions,
      list(bg_phase10_review_action(
        context = context,
        obligation = review_implication_obligation,
        title = "Record causal implication review",
        decision_type = "causal_implication_review",
        why_now = paste0(
          "The workflow should record which conditional independencies follow ",
          "from the DAG and whether current evidence is consistent with them."
        ),
        payload = list(
          suggested_fields = c(
            "testable_implications",
            "falsification_status",
            "next_check"
          )
        )
      ))
    )
  }

  actions
}
