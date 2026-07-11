# Default Bayesian workflow pack helpers.
# These internal utilities implement the stronger
# `bayesgrove.default_bayesian` review/comparison/disposition loop.

#' @keywords internal
bg_parameter_suggestions_from_hint <- function(hint, current_params = list()) {
  if (is.null(hint) || !nzchar(hint)) {
    return(list())
  }

  suggestions <- switch(
    hint,
    "reparametrize" = {
      if ("parametrization" %in% names(current_params)) {
        current <- current_params$parametrization
        if (identical(current, "centered")) {
          list(parametrization = "non-centered")
        } else if (identical(current, "non-centered")) {
          list(parametrization = "centered")
        } else {
          list(parametrization = "non-centered")
        }
      } else {
        list()
      }
    },
    "adjust_tolerances" = {
      if ("tolerance" %in% names(current_params)) {
        current_tol <- current_params$tolerance
        if (is.numeric(current_tol) && current_tol < 1e-4) {
          list(tolerance = 1e-4)
        } else {
          list(tolerance = 1e-3)
        }
      } else if ("adapt_delta" %in% names(current_params)) {
        current_delta <- current_params$adapt_delta
        if (is.numeric(current_delta) && current_delta < 0.95) {
          list(adapt_delta = 0.95)
        } else {
          list(adapt_delta = 0.99)
        }
      } else {
        list()
      }
    },
    "increase_iterations" = {
      if ("iter" %in% names(current_params)) {
        current_iter <- current_params$iter
        if (is.numeric(current_iter)) {
          list(iter = as.integer(current_iter * 2))
        } else {
          list()
        }
      } else if ("iterations" %in% names(current_params)) {
        current_iter <- current_params$iterations
        if (is.numeric(current_iter)) {
          list(iterations = as.integer(current_iter * 2))
        } else {
          list()
        }
      } else {
        list()
      }
    },
    list()
  )

  suggestions
}

# Default Workflow Pack - Obligations and Actions
# -----------------------------------------------
# Context accessors are in workflow-packs-default-helpers.R.

#' @keywords internal
bg_default_bayesian_obligation_candidate_basis <- function(obligation) {
  node_ids <- sort(unique(obligation$basis$node_ids %||% character()))
  branch_ids <- sort(unique(obligation$basis$branch_ids %||% character()))
  summary_ids <- sort(unique(obligation$basis$summary_ids %||% character()))

  list(
    node_ids = node_ids,
    branch_ids = branch_ids,
    summary_ids = summary_ids,
    candidate_signature = obligation$metadata$candidate_signature %||%
      bg_protocol_identity_string(list(
        node_ids = node_ids,
        branch_ids = branch_ids,
        summary_ids = summary_ids
      ))
  )
}

#' @keywords internal
bg_default_bayesian_obligation_comparison_signature <- function(
  obligation,
  comparison_context = NULL
) {
  known_signatures <- unique(Filter(
    Negate(is.null),
    list(
      obligation$metadata$comparison_signature %||% NULL,
      obligation$basis$comparison_signature %||% NULL,
      comparison_context$comparison_signature %||% NULL
    )
  ))

  if (length(known_signatures) == 0) {
    cli::cli_abort(
      paste0(
        "Disposition obligation {.val ",
        obligation$obligation_id %||% "<unknown>",
        "} is missing a comparison signature."
      )
    )
  }

  if (length(known_signatures) > 1) {
    cli::cli_abort(
      paste0(
        "Disposition obligation {.val ",
        obligation$obligation_id %||% "<unknown>",
        "} has mismatched comparison signatures."
      )
    )
  }

  known_signatures[[1]]
}

#' @keywords internal
bg_default_bayesian_node_has_active_artifact <- function(context, node_id) {
  fingerprint <- context$metadata$predicted_fingerprints[[node_id]] %||% NULL
  if (is.null(fingerprint)) {
    return(FALSE)
  }

  binding <- bg_artifact_binding(
    context$metadata$artifact_index,
    fingerprint,
    node_id
  )

  !is.null(binding) && identical(binding$status %||% NULL, "active")
}

#' @keywords internal
bg_default_bayesian_clean_fit_candidates <- function(context) {
  nodes <- bg_default_bayesian_all_nodes(context)
  summaries <- bg_default_bayesian_fresh_summaries(
    bg_default_bayesian_all_summaries(context)
  )
  decisions <- bg_default_bayesian_all_decisions(context)

  fit_nodes <- Filter(
    bg_pack_is_fit_node,
    nodes
  )
  if (length(fit_nodes) == 0 || length(summaries) == 0) {
    return(list())
  }

  candidates <- list()

  for (node_id in sort(names(fit_nodes))) {
    node_summaries <- Filter(
      function(summary) identical(summary$node_id %||% NULL, node_id),
      summaries
    )
    if (length(node_summaries) == 0) {
      next
    }

    ok_diagnostics <- Filter(
      function(summary) {
        identical(summary$severity %||% NULL, "ok") &&
          bg_default_bayesian_diagnostic_summary(summary)
      },
      node_summaries
    )
    if (length(ok_diagnostics) == 0) {
      next
    }

    node_scope <- sort(unique(vapply(
      ok_diagnostics,
      function(summary) summary$scope %||% "project",
      character(1)
    )))[[1]]

    warning_summaries <- Filter(
      function(summary) {
        (summary$severity %||% NULL) %in%
          c("warning", "error") &&
          identical(summary$scope %||% NULL, node_scope)
      },
      node_summaries
    )

    warning_summary_ids <- sort(unique(vapply(
      warning_summaries,
      `[[`,
      character(1),
      "summary_id"
    )))

    reviewed_summary_ids <- bg_default_bayesian_current_decision_summary_ids(
      decisions = decisions,
      scope = node_scope,
      kind = "computation_review",
      fresh_summary_ids = warning_summary_ids
    )

    criticism_target_ids <- character()
    criticism_summary_ids <- character()
    if (startsWith(node_scope, "branch:")) {
      criticism_target_ids <- sort(unique(vapply(
        Filter(
          function(summary) {
            bg_default_bayesian_fit_criticism_summary(
              summary,
              node = fit_nodes[[node_id]]
            )
          },
          warning_summaries
        ),
        `[[`,
        character(1),
        "summary_id"
      )))

      criticism_summary_ids <- bg_default_bayesian_current_decision_summary_ids(
        decisions = decisions,
        scope = node_scope,
        kind = "fit_criticism",
        fresh_summary_ids = criticism_target_ids
      )
    }

    unresolved_review <- setdiff(warning_summary_ids, reviewed_summary_ids)
    unresolved_criticism <- if (startsWith(node_scope, "branch:")) {
      setdiff(criticism_target_ids, criticism_summary_ids)
    } else {
      character()
    }

    if (length(unresolved_review) > 0 || length(unresolved_criticism) > 0) {
      next
    }

    candidates[[node_id]] <- list(
      node_id = node_id,
      scope = node_scope,
      label = fit_nodes[[node_id]]$label %||% node_id,
      ok_summary_ids = sort(unique(vapply(
        ok_diagnostics,
        `[[`,
        character(1),
        "summary_id"
      )))
    )
  }

  candidates
}

#' @keywords internal
bg_default_bayesian_candidate_basis <- function(candidates) {
  if (length(candidates) == 0) {
    return(NULL)
  }

  ordered <- candidates[order(names(candidates))]
  node_ids <- names(ordered)
  branch_ids <- sort(unique(vapply(
    ordered,
    `[[`,
    character(1),
    "scope"
  )))
  summary_ids <- sort(unique(unlist(
    lapply(
      ordered,
      function(candidate) candidate$ok_summary_ids %||% character()
    ),
    use.names = FALSE
  )))

  list(
    candidates = ordered,
    node_ids = node_ids,
    branch_ids = branch_ids,
    summary_ids = summary_ids,
    candidate_signature = bg_protocol_identity_string(list(
      node_ids = node_ids,
      branch_ids = branch_ids,
      summary_ids = summary_ids
    ))
  )
}

#' @keywords internal
bg_default_bayesian_comparison_evidence <- function(context, candidate_basis) {
  if (is.null(candidate_basis) || length(candidate_basis$node_ids) < 2) {
    return(NULL)
  }

  nodes <- bg_default_bayesian_all_nodes(context)
  edges <- bg_default_bayesian_all_edges(context)
  summaries <- bg_default_bayesian_fresh_summaries(
    bg_default_bayesian_all_summaries(context)
  )

  comparison_nodes <- Filter(
    function(node) {
      (node$kind %||% NULL) %in% c("compare", "comparison")
    },
    nodes
  )
  if (length(comparison_nodes) == 0) {
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

    resolved_fit_node_ids <- sort(unique(vapply(
      input_node_ids,
      function(input_id) {
        input_node <- nodes[[input_id]] %||% list()
        if (!identical(input_node$kind %||% NULL, "loo")) {
          return(input_id)
        }
        loo_inputs <- vapply(
          Filter(function(edge) identical(edge$to %||% NULL, input_id), edges),
          `[[`,
          character(1),
          "from"
        )
        if (length(loo_inputs) == 1L) loo_inputs[[1L]] else input_id
      },
      character(1)
    )))

    if (!identical(resolved_fit_node_ids, fit_node_ids)) {
      next
    }

    fresh_summary_ids <- sort(unique(vapply(
      Filter(
        function(summary) identical(summary$node_id %||% NULL, node_id),
        summaries
      ),
      `[[`,
      character(1),
      "summary_id"
    )))

    if (
      length(fresh_summary_ids) == 0 &&
        !bg_default_bayesian_node_has_active_artifact(context, node_id)
    ) {
      next
    }

    matches[[node_id]] <- list(
      node_id = node_id,
      label = comparison_nodes[[node_id]]$label %||% node_id,
      summary_ids = fresh_summary_ids,
      latest_at = if (length(fresh_summary_ids) > 0) {
        max(vapply(
          summaries[vapply(
            summaries,
            function(summary) {
              identical(summary$node_id %||% NULL, node_id)
            },
            logical(1)
          )],
          `[[`,
          character(1),
          "created_at"
        ))
      } else {
        ""
      }
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
  current <- matches[[ordering[[1]]]]

  list(
    node_id = current$node_id,
    label = current$label,
    summary_ids = current$summary_ids
  )
}

#' @keywords internal
bg_default_bayesian_comparison_context <- function(
  candidate_basis,
  comparison_evidence = NULL
) {
  if (is.null(candidate_basis)) {
    return(NULL)
  }

  context <- list(
    candidate_signature = candidate_basis$candidate_signature,
    fit_node_ids = candidate_basis$node_ids,
    branch_ids = candidate_basis$branch_ids,
    candidate_summary_ids = candidate_basis$summary_ids,
    comparison_node_id = comparison_evidence$node_id %||% NULL,
    comparison_summary_ids = comparison_evidence$summary_ids %||% character()
  )
  context$comparison_signature <- bg_protocol_identity_string(context)
  context
}

#' @keywords internal
bg_default_bayesian_model_comparison_matches <- function(
  decision,
  comparison_context
) {
  if (!identical(decision$kind %||% NULL, "model_comparison")) {
    return(FALSE)
  }

  decision_signature <- decision$metadata$comparison_signature %||%
    decision$metadata$candidate_signature %||%
    decision$metadata$comparison_context$comparison_signature %||%
    NULL

  if (!is.null(decision_signature)) {
    return(identical(
      as.character(decision_signature),
      as.character(comparison_context$comparison_signature)
    ))
  }

  decision_fit_node_ids <- sort(unique(
    decision$metadata$fit_node_ids %||% character()
  ))
  if (
    length(decision_fit_node_ids) == 0 ||
      !identical(decision_fit_node_ids, comparison_context$fit_node_ids)
  ) {
    return(FALSE)
  }

  decision_summary_ids <- bg_default_bayesian_decision_summary_ids(decision)
  if (length(decision_summary_ids) == 0) {
    return(FALSE)
  }

  identical(
    decision_summary_ids,
    sort(unique(c(
      comparison_context$candidate_summary_ids,
      comparison_context$comparison_summary_ids
    )))
  )
}

#' @keywords internal
bg_default_bayesian_normalize_disposition <- function(decision) {
  disposition <- decision$metadata$disposition %||% decision$choice %||% NULL
  if (is.null(disposition)) {
    return(NULL)
  }

  disposition <- tolower(trimws(as.character(disposition)))
  if (!disposition %in% c("accept", "reject")) {
    return(NULL)
  }

  disposition
}

#' @keywords internal
bg_default_bayesian_branch_disposition_matches <- function(
  decision,
  branch_id,
  comparison_context
) {
  if (
    !identical(decision$kind %||% NULL, "branch_disposition") ||
      !identical(decision$scope %||% NULL, branch_id) ||
      is.null(bg_default_bayesian_normalize_disposition(decision))
  ) {
    return(FALSE)
  }

  decision_signature <- decision$metadata$comparison_signature %||%
    decision$metadata$comparison_context$comparison_signature %||%
    NULL

  identical(
    as.character(decision_signature),
    as.character(comparison_context$comparison_signature)
  )
}

#' @keywords internal
bg_default_bayesian_pending_fit_criticism_summaries <- function(context) {
  if (!startsWith(context$scope, "branch:")) {
    return(list())
  }

  summaries <- Filter(
    function(summary) {
      identical(summary$scope %||% NULL, context$scope) &&
        isTRUE(summary$is_fresh) &&
        !isTRUE(summary$is_stale) &&
        (summary$severity %||% NULL) %in% c("warning", "error")
    },
    context$evidence$summaries %||% list()
  )
  if (length(summaries) == 0) {
    return(list())
  }

  nodes <- context$structural$nodes %||% list()
  relevant <- Filter(
    function(summary) {
      node <- nodes[[summary$node_id]] %||% NULL
      bg_default_bayesian_fit_criticism_summary(summary, node = node)
    },
    summaries
  )
  if (length(relevant) == 0) {
    return(list())
  }

  fresh_summary_ids <- sort(unique(vapply(
    relevant,
    `[[`,
    character(1),
    "summary_id"
  )))
  criticized_summary_ids <- bg_default_bayesian_current_decision_summary_ids(
    decisions = context$evidence$decisions %||% list(),
    scope = context$scope,
    kind = "fit_criticism",
    fresh_summary_ids = fresh_summary_ids
  )

  Filter(
    function(summary) {
      !summary$summary_id %in% criticized_summary_ids
    },
    relevant
  )
}

#' @keywords internal
bg_default_bayesian_fit_criticism_obligations <- function(
  context,
  pack_config = list()
) {
  pending <- bg_default_bayesian_pending_fit_criticism_summaries(context)
  if (length(pending) == 0) {
    return(list())
  }

  list(bg_pack_obligation(
    context = context,
    kind = "review_fit_criticism",
    title = "Review fit criticism",
    why = paste0(
      "Fresh warning or error summaries from fit or diagnostic nodes ",
      "need an explicit fit-criticism review."
    ),
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
      branch_ids = NULL
    ),
    metadata = list(
      source_keys = c("workflow_core", "stan_diagnostics"),
      hold_node_ids = NULL,
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

  list(bg_pack_obligation(
    context = context,
    kind = "compare_candidate_branches",
    title = "Compare candidate branches",
    why = paste0(
      "At least two fit candidates are clean and comparable. ",
      "Run or review a comparison, then record a model comparison decision."
    ),
    basis = list(
      node_ids = candidate_basis$node_ids,
      branch_ids = candidate_basis$branch_ids,
      summary_ids = candidate_basis$summary_ids
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

  list(bg_pack_obligation(
    context = context,
    kind = "accept_or_reject_branch",
    title = "Accept or reject branch",
    why = paste0(
      "A current comparison exists for this branch's candidate set. ",
      "Record whether the branch is accepted or rejected."
    ),
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
      comparison_signature = comparison_context$comparison_signature %||% NULL
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

  actions <- list(bg_pack_action(
    context = context,
    kind = "record_decision",
    title = "Record a fit criticism review",
    why_now = paste0(
      "The current fit or diagnostic warnings need an explicit criticism ",
      "decision tied to the fresh summaries."
    ),
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
    list(bg_pack_action(
      context = context,
      kind = "branch_and_modify",
      title = "Branch and modify to resolve diagnostics",
      why_now = "A new branch keeps the diagnostic revision loop explicit.",
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

    return(list(bg_pack_action(
      context = context,
      kind = "create_node_from_template",
      title = "Create comparison node",
      why_now = paste0(
        "These fit candidates are ready for formal comparison. ",
        "Create the comparison node before recording the project decision."
      ),
      basis = list(
        node_ids = candidate_basis$node_ids
      ),
      payload = list(
        template_ref = "branch_comparison",
        inputs = candidate_basis$node_ids,
        default_label = paste("Compare:", paste(fit_labels, collapse = " vs "))
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

  list(bg_pack_action(
    context = context,
    kind = "record_decision",
    title = "Record model comparison decision",
    why_now = paste0(
      "A current comparison exists for the clean candidate set. ",
      "Record the explicit model comparison decision."
    ),
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

  list(bg_pack_action(
    context = context,
    kind = "record_decision",
    title = "Record branch disposition",
    why_now = paste0(
      "The branch is part of a current comparison context. ",
      "Record an explicit accept or reject disposition."
    ),
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
    metadata = list(
      source_keys = c("workflow_core", "model_comparison", "stacking")
    )
  ))
}
