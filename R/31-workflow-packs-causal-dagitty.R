# Causal Dagitty Workflow Pack - Obligations and Actions
# -----------------------------------------------------
# Formula/dagitty helpers are in 31a-workflow-packs-causal-dagitty-helpers.R.

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
          source_keys = c(
            "taxonomy",
            "causal_scaffold",
            "causal_identification"
          ),
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
          source_keys = c(
            "taxonomy",
            "causal_scaffold",
            "causal_identification"
          ),
          summary_kinds = "dagitty_adjustment",
          utility_dimensions = "causal_consistency",
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  contract_summaries <- bg_phase10_fresh_summaries(
    context,
    summary_kinds = "causal_selection_contract"
  )
  has_contract_review <- bg_phase10_has_scope_decision(
    context,
    kind = "causal_selection_contract_review",
    node_ids = fit_node_ids
  )

  if (
    (length(adjustment_summaries) > 0 || has_adjustment_review) &&
      length(contract_summaries) == 0 &&
      !has_contract_review
  ) {
    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "derive_causal_selection_contract",
        title = "Derive a causal selection contract",
        why = paste0(
          "A causal branch should make its model-building rules explicit. Record ",
          "which terms are required for identification, which are forbidden for ",
          "bias control, and which remaining terms can be explored for precision ",
          "or projection-based reduction."
        ),
        severity = "advisory",
        basis = list(node_ids = fit_node_ids, branch_ids = context$scope),
        metadata = list(
          source_keys = c(
            "taxonomy",
            "causal_scaffold",
            "causal_identification",
            "projection_predictive"
          ),
          summary_kinds = "causal_selection_contract",
          utility_dimensions = c(
            "causal_consistency",
            "predictive_performance",
            "parsimony",
            "interpretability"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  pending_contract_reviews <- bg_phase10_pending_review_summaries(
    context,
    decision_kind = "causal_selection_contract_review",
    summary_kinds = "causal_selection_contract"
  )
  if (length(pending_contract_reviews) > 0) {
    contract <- bg_phase10_causal_contract_from_summary(
      pending_contract_reviews[[length(pending_contract_reviews)]],
      response = {
        formula_nodes <- bg_phase10_formula_nodes(context)
        if (length(formula_nodes) == 0) {
          NULL
        } else {
          bg_phase10_formula_response(
            (formula_nodes[[1]]$params %||% list())$formula
          )
        }
      }
    )

    obligations <- c(
      obligations,
      list(bg_phase10_obligation(
        context = context,
        kind = "review_causal_selection_contract",
        title = "Review the causal selection contract",
        why = paste0(
          "Before fitting or reducing causal models further, review the ordered ",
          "candidate list and confirm which terms are locked in for ",
          "identification, excluded for bias control, and optional for precision."
        ),
        severity = "advisory",
        basis = list(
          node_ids = fit_node_ids,
          summary_ids = bg_phase10_sort_ids(vapply(
            pending_contract_reviews,
            `[[`,
            character(1),
            "summary_id"
          )),
          branch_ids = context$scope
        ),
        metadata = list(
          source_keys = c(
            "taxonomy",
            "causal_scaffold",
            "causal_identification",
            "projection_predictive"
          ),
          summary_kinds = "causal_selection_contract",
          required_terms = contract$required_terms,
          forbidden_terms = contract$forbidden_terms,
          optional_terms = contract$optional_terms,
          ranked_candidate_terms = contract$ranked_candidate_terms,
          utility_dimensions = c(
            "causal_consistency",
            "predictive_performance",
            "parsimony",
            "interpretability"
          ),
          pad_model_classes = c("PD", "PAD")
        )
      ))
    )
  }

  reviewed_contract <- bg_phase10_current_causal_contract(context)
  if (!is.null(reviewed_contract)) {
    formula_nodes <- bg_phase10_formula_nodes(context)
    violations <- Map(
      function(node_id, node) {
        bg_phase10_formula_contract_violation(
          node_id,
          node,
          reviewed_contract
        )
      },
      names(formula_nodes),
      formula_nodes
    )
    violations <- Filter(
      function(violation) !isTRUE(violation$is_consistent),
      violations
    )

    if (length(violations) > 0) {
      obligations <- c(
        obligations,
        list(bg_phase10_obligation(
          context = context,
          kind = "enforce_causal_formula_contract",
          title = "Revise formulas to respect the causal selection contract",
          why = paste0(
            "Causal branches should only use formulas that include the required ",
            "identification terms, exclude forbidden controls, and stay inside ",
            "the DAG-approved candidate set."
          ),
          severity = "blocking",
          basis = list(
            node_ids = bg_phase10_sort_ids(vapply(
              violations,
              `[[`,
              character(1),
              "node_id"
            )),
            summary_ids = reviewed_contract$summary_id %||% character(),
            branch_ids = context$scope
          ),
          metadata = list(
            hold_node_ids = bg_phase10_sort_ids(vapply(
              violations,
              `[[`,
              character(1),
              "node_id"
            )),
            source_keys = c(
              "taxonomy",
              "causal_identification",
              "projection_predictive"
            ),
            required_terms = reviewed_contract$required_terms,
            forbidden_terms = reviewed_contract$forbidden_terms,
            ranked_candidate_terms = reviewed_contract$ranked_candidate_terms,
            utility_dimensions = c(
              "causal_consistency",
              "predictive_performance",
              "parsimony"
            ),
            pad_model_classes = c("PD", "PAD")
          )
        ))
      )
    }
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
          utility_dimensions = c(
            "causal_consistency",
            "structural_faithfulness"
          ),
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
          utility_dimensions = c(
            "causal_consistency",
            "structural_faithfulness"
          ),
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

  contract_obligation <- bg_find_obligation(
    obligations,
    kind = "derive_causal_selection_contract",
    scope = context$scope
  )
  if (!is.null(contract_obligation)) {
    action <- bg_phase10_check_action(
      context = context,
      obligation = contract_obligation,
      title = "Create causal selection contract",
      node_kind = pack_config$selection_contract_node_kind %||%
        "causal_selection_contract",
      default_label_prefix = "Causal selection:",
      why_now = paste0(
        "Create a DAG-backed contract node so the workflow can record required, ",
        "forbidden, and precision-oriented candidate terms as plain data."
      )
    )
    if (!is.null(action)) {
      actions <- c(actions, list(action))
    }
  }

  review_contract_obligation <- bg_find_obligation(
    obligations,
    kind = "review_causal_selection_contract",
    scope = context$scope
  )
  if (!is.null(review_contract_obligation)) {
    actions <- c(
      actions,
      list(bg_phase10_review_action(
        context = context,
        obligation = review_contract_obligation,
        title = "Record causal selection contract review",
        decision_type = "causal_selection_contract_review",
        why_now = paste0(
          "The workflow should record which terms are locked in for ",
          "identification, which are prohibited, and which remaining variables ",
          "are admissible for precision or projection-based reduction."
        ),
        payload = list(
          suggested_fields = c(
            "estimand",
            "required_terms",
            "forbidden_terms",
            "ranked_candidate_terms",
            "formula_policy"
          ),
          required_terms = review_contract_obligation$metadata$required_terms %||%
            character(),
          forbidden_terms = review_contract_obligation$metadata$forbidden_terms %||%
            character(),
          ranked_candidate_terms = review_contract_obligation$metadata$ranked_candidate_terms %||%
            character()
        )
      ))
    )
  }

  formula_obligation <- bg_find_obligation(
    obligations,
    kind = "enforce_causal_formula_contract",
    scope = context$scope
  )
  if (!is.null(formula_obligation)) {
    graph_nodes <- context$structural$nodes %||% list()
    source_node_id <- bg_phase10_source_node_id(
      context,
      formula_obligation$basis$node_ids %||% character()
    )
    source_node <- graph_nodes[[source_node_id]] %||% NULL
    contract <- bg_phase10_current_causal_contract(context)

    if (!is.null(source_node) && !is.null(contract)) {
      suggested_formula <- bg_phase10_contract_suggested_formula(
        source_node,
        contract
      )
      actions <- c(
        actions,
        list(bg_phase10_action(
          context = context,
          kind = "branch_and_modify",
          title = "Branch and revise formula to satisfy the causal contract",
          why_now = paste0(
            "Create a revision branch whose formula keeps required adjustment ",
            "terms, excludes forbidden controls, and leaves later precision ",
            "choices inside the admissible candidate set."
          ),
          basis = list(
            obligation_refs = list(list(
              kind = formula_obligation$kind,
              scope = formula_obligation$scope
            )),
            node_ids = formula_obligation$basis$node_ids %||% character(),
            summary_ids = formula_obligation$basis$summary_ids %||% character()
          ),
          payload = list(
            template_ref = "branch_and_modify_fit",
            source_node_id = source_node_id,
            modification_hint = "respect_causal_contract",
            default_label = bg_revised_label(source_node),
            parameter_suggestions = stats::setNames(
              list(suggested_formula),
              "formula"
            ),
            continuation_kinds = c("check", "ppc"),
            auto_run = TRUE
          ),
          metadata = list(
            source_keys = c(
              "taxonomy",
              "causal_identification",
              "projection_predictive"
            )
          )
        ))
      )
    }
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
      node_kind = pack_config$implications_node_kind %||%
        "dagitty_implications",
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
