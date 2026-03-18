# Default Bayesian Workflow Pack - Obligation and Action Evaluation
# -----------------------------------------------------------------
# Obligation and action constructors that produce protocol obligations
# and actions for the default Bayesian workflow pack.
# Constructor helpers are in 22-workflow-packs-default.R.
# Context accessors are in 22a-workflow-packs-default-helpers.R.

#' @keywords internal
bg_default_bayesian_fit_criticism_obligations <- function(
  context,
  pack_config = list()
) {
  pending <- bg_default_bayesian_pending_fit_criticism_summaries(context)
  if (length(pending) == 0) {
    return(list())
  }

  list(list(
    kind = "review_fit_criticism",
    scope = context$scope,
    severity = "blocking",
    title = "Review fit criticism",
    basis = list(
      summary_ids = sort(unique(vapply(
        pending,
        `[[`,
        character(1),
        "summary_id"
      ))),
      node_ids = sort(unique(vapply(
        pending,
        `[[`,
        character(1),
        "node_id"
      ))),
      decision_ids = character()
    ),
    explanation = list(
      why = paste0(
        "Fresh warning or error summaries from fit or diagnostic nodes ",
        "need an explicit fit-criticism review."
      ),
      references = bg_workflow_references(c(
        "workflow_core",
        "stan_diagnostics"
      ))
    ),
    metadata = list(
      source_keys = c("workflow_core", "stan_diagnostics"),
      summary_kinds = sort(unique(vapply(
        pending,
        `[[`,
        character(1),
        "summary_kind"
      )))
    )
  ))
}

#' @keywords internal
bg_default_bayesian_comparison_obligations <- function(
  context,
  pack_config = list()
) {
  if (!identical(context$scope, "project")) {
    return(list())
  }

  comparison_state <- bg_default_bayesian_comparison_state(context)
  candidate_basis <- comparison_state$candidate_basis %||% NULL
  if (is.null(candidate_basis) || length(candidate_basis$node_ids) < 2) {
    return(list())
  }

  comparison_evidence <- comparison_state$comparison_evidence
  comparison_context <- comparison_state$comparison_context

  has_decision <- any(vapply(
    bg_default_bayesian_all_decisions(context),
    bg_default_bayesian_model_comparison_matches,
    logical(1),
    comparison_context = comparison_context
  ))
  if (has_decision) {
    return(list())
  }

  list(list(
    kind = "compare_candidate_branches",
    scope = "project",
    severity = "blocking",
    title = "Compare candidate branches",
    basis = list(
      node_ids = candidate_basis$node_ids,
      branch_ids = candidate_basis$branch_ids,
      summary_ids = candidate_basis$summary_ids,
      decision_ids = character()
    ),
    explanation = list(
      why = paste0(
        "At least two fit candidates are clean and comparable. ",
        "Run or review a comparison, then record a model comparison decision."
      ),
      references = bg_workflow_references(c(
        "workflow_core",
        "model_comparison",
        "stacking"
      ))
    ),
    metadata = list(
      source_keys = c("workflow_core", "model_comparison", "stacking"),
      candidate_signature = candidate_basis$candidate_signature,
      hold_node_ids = bg_default_bayesian_hold_node_ids(comparison_evidence)
    )
  ))
}

#' @keywords internal
bg_default_bayesian_disposition_obligations <- function(
  context,
  pack_config = list()
) {
  if (!startsWith(context$scope, "branch:")) {
    return(list())
  }

  comparison_state <- bg_default_bayesian_comparison_state(context)
  candidate_basis <- comparison_state$candidate_basis %||% NULL
  if (is.null(candidate_basis) || length(candidate_basis$node_ids) < 2) {
    return(list())
  }

  candidate_scopes <- sort(unique(vapply(
    candidate_basis$candidates,
    `[[`,
    character(1),
    "scope"
  )))
  if (!context$scope %in% candidate_scopes) {
    return(list())
  }

  comparison_evidence <- comparison_state$comparison_evidence
  if (is.null(comparison_evidence)) {
    return(list())
  }

  comparison_context <- comparison_state$comparison_context
  decisions <- bg_default_bayesian_all_decisions(context)

  has_disposition <- any(vapply(
    decisions,
    bg_default_bayesian_branch_disposition_matches,
    logical(1),
    branch_id = context$scope,
    comparison_context = comparison_context
  ))
  if (has_disposition) {
    return(list())
  }

  branch_candidates <- Filter(
    function(candidate) identical(candidate$scope, context$scope),
    candidate_basis$candidates
  )

  list(list(
    kind = "accept_or_reject_branch",
    scope = context$scope,
    severity = "blocking",
    title = "Accept or reject branch",
    basis = list(
      branch_ids = context$scope,
      node_ids = sort(unique(vapply(
        branch_candidates,
        `[[`,
        character(1),
        "node_id"
      ))),
      summary_ids = sort(unique(unlist(
        lapply(
          branch_candidates,
          function(candidate) candidate$ok_summary_ids %||% character()
        ),
        use.names = FALSE
      ))),
      decision_ids = character(),
      comparison_signature = comparison_context$comparison_signature %||% NULL
    ),
    explanation = list(
      why = paste0(
        "A current comparison exists for this branch's candidate set. ",
        "Record whether the branch is accepted or rejected."
      ),
      references = bg_workflow_references(c(
        "workflow_core",
        "model_comparison",
        "stacking"
      ))
    ),
    metadata = list(
      source_keys = c("workflow_core", "model_comparison", "stacking"),
      comparison_node_id = comparison_evidence$node_id,
      hold_node_ids = bg_default_bayesian_hold_node_ids(comparison_evidence),
      comparison_context = comparison_context,
      comparison_signature = comparison_context$comparison_signature %||% NULL
    )
  ))
}

#' @keywords internal
bg_default_bayesian_fit_criticism_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  obligation <- bg_find_obligation(obligations, "review_fit_criticism")
  if (is.null(obligation)) {
    return(list())
  }
  node_ids <- obligation$basis$node_ids %||% character()
  summary_ids <- obligation$basis$summary_ids %||% character()

  actions <- list(list(
    kind = "record_decision",
    scope = context$scope,
    title = "Record a fit criticism review",
    basis = list(
      obligation_refs = list(list(
        kind = "review_fit_criticism",
        scope = context$scope
      )),
      node_ids = node_ids,
      summary_ids = summary_ids
    ),
    payload = list(
      template_ref = "review_decision",
      decision_type = "fit_criticism",
      node_ids = node_ids,
      summary_ids = summary_ids
    ),
    explanation = list(
      why_now = paste0(
        "The current fit or diagnostic warnings need an explicit criticism ",
        "decision tied to the fresh summaries."
      ),
      references = bg_workflow_references(c(
        "workflow_core",
        "stan_diagnostics"
      ))
    ),
    metadata = list(source_keys = c("workflow_core", "stan_diagnostics"))
  ))

  if (length(node_ids) == 0) {
    return(actions)
  }

  source_node_id <- node_ids[[1]]
  source_node <- context$structural$nodes[[source_node_id]] %||% NULL

  modification_hint <- NULL
  if (
    "hmc_diagnostics" %in% (obligation$metadata$summary_kinds %||% character())
  ) {
    modification_hint <- "reparametrize"
  } else if (
    "optimizer_diagnostics" %in%
      (obligation$metadata$summary_kinds %||% character())
  ) {
    modification_hint <- "adjust_tolerances"
  }

  actions <- c(
    actions,
    list(list(
      kind = "branch_and_modify",
      scope = context$scope,
      title = "Branch and modify to resolve diagnostics",
      basis = list(
        obligation_refs = list(list(
          kind = "review_fit_criticism",
          scope = context$scope
        )),
        node_ids = node_ids
      ),
      payload = list(
        template_ref = "branch_and_modify_fit",
        source_node_id = source_node_id,
        modification_hint = modification_hint,
        default_label = if (!is.null(source_node)) {
          bg_revised_label(source_node)
        } else {
          NULL
        },
        parameter_suggestions = bg_parameter_suggestions_from_hint(
          hint = modification_hint,
          current_params = source_node$params %||% list()
        ),
        continuation_kinds = c("check", "ppc"),
        auto_run = TRUE
      ),
      explanation = list(
        why_now = "A new branch keeps the diagnostic revision loop explicit.",
        references = bg_workflow_references(c(
          "workflow_core",
          "stan_diagnostics"
        ))
      ),
      metadata = list(source_keys = c("workflow_core", "stan_diagnostics"))
    ))
  )

  actions
}

#' @keywords internal
bg_default_bayesian_comparison_decision_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  if (!identical(context$scope, "project")) {
    return(list())
  }

  obligation <- bg_find_obligation(obligations, "compare_candidate_branches")
  if (is.null(obligation)) {
    return(list())
  }
  candidate_basis <- bg_default_bayesian_obligation_candidate_basis(obligation)

  comparison_evidence <- bg_default_bayesian_comparison_evidence(
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

    return(list(list(
      kind = "create_node_from_template",
      scope = "project",
      title = "Create comparison node",
      basis = list(
        node_ids = candidate_basis$node_ids
      ),
      payload = list(
        template_ref = "branch_comparison",
        inputs = candidate_basis$node_ids,
        default_label = paste("Compare:", paste(fit_labels, collapse = " vs "))
      ),
      explanation = list(
        why_now = paste0(
          "These fit candidates are ready for formal comparison. ",
          "Create the comparison node before recording the project decision."
        ),
        references = bg_workflow_references(c(
          "workflow_core",
          "model_comparison",
          "stacking"
        ))
      ),
      metadata = list(
        source_keys = c("workflow_core", "model_comparison", "stacking")
      )
    )))
  }

  comparison_context <- bg_default_bayesian_comparison_context(
    candidate_basis,
    comparison_evidence
  )

  list(list(
    kind = "record_decision",
    scope = "project",
    title = "Record model comparison decision",
    basis = list(
      obligation_refs = list(list(
        kind = "compare_candidate_branches",
        scope = "project"
      )),
      node_ids = candidate_basis$node_ids,
      summary_ids = sort(unique(c(
        candidate_basis$summary_ids,
        comparison_evidence$summary_ids %||% character()
      )))
    ),
    payload = list(
      template_ref = "review_decision",
      decision_type = "model_comparison",
      fit_node_ids = candidate_basis$node_ids,
      branch_ids = candidate_basis$branch_ids,
      summary_ids = sort(unique(c(
        candidate_basis$summary_ids,
        comparison_evidence$summary_ids %||% character()
      ))),
      comparison_context = comparison_context,
      comparison_signature = comparison_context$comparison_signature,
      candidate_signature = comparison_context$candidate_signature
    ),
    explanation = list(
      why_now = paste0(
        "A current comparison exists for the clean candidate set. ",
        "Record the explicit model comparison decision."
      ),
      references = bg_workflow_references(c(
        "workflow_core",
        "model_comparison",
        "stacking"
      ))
    ),
    metadata = list(
      source_keys = c("workflow_core", "model_comparison", "stacking")
    )
  ))
}

#' @keywords internal
bg_default_bayesian_disposition_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  obligation <- bg_find_obligation(
    obligations,
    "accept_or_reject_branch",
    scope = context$scope
  )
  if (is.null(obligation)) {
    return(list())
  }
  comparison_context <- obligation$metadata$comparison_context %||% NULL
  if (is.null(comparison_context)) {
    cli::cli_abort(
      paste0(
        "Disposition obligation {.val ",
        obligation$obligation_id %||% "<unknown>",
        "} is missing `metadata$comparison_context`."
      )
    )
  }

  comparison_signature <- bg_default_bayesian_obligation_comparison_signature(
    obligation,
    comparison_context = comparison_context
  )

  list(list(
    kind = "record_decision",
    scope = context$scope,
    title = "Record branch disposition",
    basis = list(
      obligation_refs = list(list(
        kind = "accept_or_reject_branch",
        scope = context$scope
      )),
      node_ids = obligation$basis$node_ids %||% character(),
      summary_ids = obligation$basis$summary_ids %||% character()
    ),
    payload = list(
      template_ref = "review_decision",
      decision_type = "branch_disposition",
      allowed_dispositions = c("accept", "reject"),
      node_ids = obligation$basis$node_ids %||% character(),
      summary_ids = obligation$basis$summary_ids %||% character(),
      branch_ids = obligation$basis$branch_ids %||% character(),
      comparison_context = comparison_context,
      comparison_signature = comparison_signature
    ),
    explanation = list(
      why_now = paste0(
        "The branch is part of a current comparison context. ",
        "Record an explicit accept or reject disposition."
      ),
      references = bg_workflow_references(c(
        "workflow_core",
        "model_comparison",
        "stacking"
      ))
    ),
    metadata = list(
      source_keys = c("workflow_core", "model_comparison", "stacking")
    )
  ))
}
