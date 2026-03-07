# Default Bayesian workflow pack helpers.
# These internal utilities implement the stronger
# `bayesguide.default_bayesian` review/comparison/disposition loop.

#' @keywords internal
bg_default_bayesian_all_nodes <- function(context) {
  context$metadata$cross_scope_nodes %||%
    context$structural$nodes %||%
    list()
}

#' @keywords internal
bg_default_bayesian_all_edges <- function(context) {
  context$metadata$cross_scope_edges %||%
    context$structural$edges %||%
    list()
}

#' @keywords internal
bg_default_bayesian_all_summaries <- function(context) {
  context$metadata$cross_scope_summaries %||%
    context$evidence$summaries %||%
    list()
}

#' @keywords internal
bg_default_bayesian_all_decisions <- function(context) {
  context$metadata$cross_scope_decisions %||%
    context$evidence$decisions %||%
    list()
}

#' @keywords internal
bg_default_bayesian_fresh_summaries <- function(summaries) {
  Filter(
    function(summary) {
      isTRUE(summary$is_fresh) && !isTRUE(summary$is_stale)
    },
    summaries %||% list()
  )
}

#' @keywords internal
bg_default_bayesian_diagnostic_summary <- function(summary) {
  grepl(
    "diagnostics",
    summary$summary_kind %||% "",
    ignore.case = TRUE
  )
}

#' @keywords internal
bg_default_bayesian_fit_criticism_summary <- function(summary, node = NULL) {
  summary_kind <- summary$summary_kind %||% ""
  node_kind <- node$kind %||% ""

  identical(node_kind, "fit") ||
    grepl("diagnostics|fit|criticism", summary_kind, ignore.case = TRUE) ||
    grepl("diagnostic|check", node_kind, ignore.case = TRUE)
}

#' @keywords internal
bg_default_bayesian_decision_summary_ids <- function(decision) {
  unique(sort(c(
    decision$metadata$summary_ids %||% character(),
    decision$metadata$summary_id %||% character()
  )))
}

#' @keywords internal
bg_default_bayesian_scope_decisions <- function(decisions, scope, kind = NULL) {
  Filter(
    function(decision) {
      identical(decision$scope %||% NULL, scope) &&
        (is.null(kind) || identical(decision$kind %||% NULL, kind))
    },
    decisions %||% list()
  )
}

#' @keywords internal
bg_default_bayesian_current_decision_summary_ids <- function(
  decisions,
  scope,
  kind,
  fresh_summary_ids
) {
  current <- lapply(
    bg_default_bayesian_scope_decisions(decisions, scope = scope, kind = kind),
    function(decision) {
      decision_summary_ids <- bg_default_bayesian_decision_summary_ids(decision)
      if (
        length(decision_summary_ids) == 0 ||
          !all(decision_summary_ids %in% fresh_summary_ids)
      ) {
        return(character())
      }

      decision_summary_ids
    }
  )

  sort(unique(unlist(current, use.names = FALSE)))
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
    function(node) identical(node$kind %||% NULL, "fit"),
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

    if (!identical(input_node_ids, fit_node_ids)) {
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
      references = character()
    ),
    metadata = list(
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

  candidate_basis <- bg_default_bayesian_candidate_basis(
    bg_default_bayesian_clean_fit_candidates(context)
  )
  if (is.null(candidate_basis) || length(candidate_basis$node_ids) < 2) {
    return(list())
  }

  comparison_evidence <- bg_default_bayesian_comparison_evidence(
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
      references = character()
    ),
    metadata = list(
      candidate_signature = candidate_basis$candidate_signature,
      hold_node_ids = comparison_evidence$node_id %||% character()
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

  candidate_basis <- bg_default_bayesian_candidate_basis(
    bg_default_bayesian_clean_fit_candidates(context)
  )
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

  comparison_evidence <- bg_default_bayesian_comparison_evidence(
    context,
    candidate_basis
  )
  if (is.null(comparison_evidence)) {
    return(list())
  }

  comparison_context <- bg_default_bayesian_comparison_context(
    candidate_basis,
    comparison_evidence
  )
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
      comparison_context = comparison_context
    ),
    explanation = list(
      why = paste0(
        "A current comparison exists for this branch's candidate set. ",
        "Record whether the branch is accepted or rejected."
      ),
      references = character()
    ),
    metadata = list(
      comparison_node_id = comparison_evidence$node_id,
      hold_node_ids = comparison_evidence$node_id %||% character()
    )
  ))
}

#' @keywords internal
bg_default_bayesian_fit_criticism_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  criticism_obligation <- Filter(
    function(obligation) {
      identical(obligation$kind %||% NULL, "review_fit_criticism")
    },
    obligations
  )
  if (length(criticism_obligation) == 0) {
    return(list())
  }

  obligation <- criticism_obligation[[1]]
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
      references = character()
    ),
    metadata = list()
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
          paste0(source_node$label %||% source_node$kind, " (revised)")
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
        references = character()
      ),
      metadata = list()
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

  comparison_obligation <- Filter(
    function(obligation) {
      identical(obligation$kind %||% NULL, "compare_candidate_branches")
    },
    obligations
  )
  if (length(comparison_obligation) == 0) {
    return(list())
  }

  obligation <- comparison_obligation[[1]]
  candidate_basis <- list(
    node_ids = sort(unique(obligation$basis$node_ids %||% character())),
    branch_ids = sort(unique(obligation$basis$branch_ids %||% character())),
    summary_ids = sort(unique(obligation$basis$summary_ids %||% character())),
    candidate_signature = obligation$metadata$candidate_signature %||%
      bg_protocol_identity_string(list(
        node_ids = obligation$basis$node_ids %||% character(),
        branch_ids = obligation$basis$branch_ids %||% character(),
        summary_ids = obligation$basis$summary_ids %||% character()
      ))
  )

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
        references = character()
      ),
      metadata = list()
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
      references = character()
    ),
    metadata = list()
  ))
}

#' @keywords internal
bg_default_bayesian_disposition_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  disposition_obligation <- Filter(
    function(obligation) {
      identical(obligation$kind %||% NULL, "accept_or_reject_branch") &&
        identical(obligation$scope %||% NULL, context$scope)
    },
    obligations
  )
  if (length(disposition_obligation) == 0) {
    return(list())
  }

  obligation <- disposition_obligation[[1]]

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
      comparison_context = obligation$basis$comparison_context %||% list(),
      comparison_signature = obligation$basis$comparison_context$comparison_signature %||%
        NULL
    ),
    explanation = list(
      why_now = paste0(
        "The branch is part of a current comparison context. ",
        "Record an explicit accept or reject disposition."
      ),
      references = character()
    ),
    metadata = list()
  ))
}
