#' @keywords internal
bg_builtin_workflow_registry <- function() {
  list(
    "bayesguide.default_bayesian" = list(
      pack_id = "bayesguide.default_bayesian",
      version = "0.1.0",
      obligation_providers = list(
        bg_default_bayesian_goal_obligations,
        bg_default_bayesian_summary_obligations
      ),
      action_providers = list(
        bg_default_bayesian_goal_actions,
        bg_default_bayesian_summary_actions
      )
    )
  )
}

#' @keywords internal
bg_default_workflow_pack_refs <- function() {
  list("bayesguide.default_bayesian")
}

#' @keywords internal
bg_lookup_workflow_pack <- function(pack_id) {
  bg_builtin_workflow_registry()[[pack_id]] %||% NULL
}

#' @keywords internal
bg_normalize_workflow_pack_ref <- function(spec) {
  if (is.character(spec) && length(spec) == 1) {
    spec <- list(pack_id = spec)
  }

  if (!is.list(spec) || is.null(spec$pack_id) || !is.character(spec$pack_id)) {
    cli::cli_abort("Workflow packs must be strings or lists with a `pack_id`.")
  }

  pack <- bg_lookup_workflow_pack(spec$pack_id)
  if (is.null(pack)) {
    cli::cli_abort("Unknown workflow pack {.val {spec$pack_id}}.")
  }

  list(
    pack_id = spec$pack_id,
    version = spec$version %||% pack$version,
    config = spec$config %||% list()
  )
}

#' @keywords internal
bg_normalize_workflow_pack_refs <- function(specs) {
  specs <- specs %||% list()
  if (length(specs) == 0) {
    return(list())
  }

  unname(lapply(specs, bg_normalize_workflow_pack_ref))
}

#' List active workflow packs
#'
#' @param project A `bg_handle`.
#'
#' @return A list of active workflow-pack descriptors.
#' @export
bg_workflow_packs <- function(project) {
  S7::check_is_S7(project, bg_handle)
  bg_normalize_workflow_pack_refs(
    bg_read_project_config(project)$workflow_packs %||% list()
  )
}

#' @keywords internal
bg_resolve_workflow_scope <- function(project, scope, branch_id = NULL) {
  scope <- match.arg(scope, c("project", "branch"))

  if (identical(scope, "project")) {
    return("project")
  }

  if (
    is.null(branch_id) || !is.character(branch_id) || length(branch_id) != 1
  ) {
    cli::cli_abort("`branch_id` is required when `scope = \"branch\"`.")
  }

  branches <- bg_read_branch_registry(project)$branches
  if (is.null(branches[[branch_id]])) {
    cli::cli_abort("Branch {.val {branch_id}} not found in branch registry.")
  }

  branch_id
}

#' Build a workflow context for protocol evaluation
#'
#' @param project A `bg_handle`.
#' @param scope One of `project` or `branch`.
#' @param branch_id Optional branch id, required for branch-scoped queries.
#'
#' @return A `bg_workflow_context` plain-data list.
#' @export
bg_workflow_context <- function(
  project,
  scope = c("project", "branch"),
  branch_id = NULL
) {
  resolved_scope <- bg_resolve_workflow_scope(project, scope, branch_id)
  bg_build_workflow_context(project, scope = resolved_scope)
}

#' @keywords internal
bg_protocol_normalize_value <- function(x, field = NULL) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.atomic(x)) {
    if (
      !is.null(field) &&
        field %in%
          c(
            "node_ids",
            "summary_ids",
            "decision_ids",
            "branch_ids"
          )
    ) {
      return(sort(unique(as.character(x))))
    }
    return(x)
  }

  if (!is.list(x)) {
    return(x)
  }

  if (identical(field, "obligation_refs")) {
    normalized <- lapply(x, bg_protocol_normalize_value)
    ordering <- vapply(normalized, bg_protocol_identity_string, character(1))
    return(normalized[order(ordering)])
  }

  if (!is.null(names(x))) {
    nm <- sort(names(x))
    normalized <- stats::setNames(
      lapply(nm, function(name) {
        bg_protocol_normalize_value(x[[name]], field = name)
      }),
      nm
    )
    return(normalized)
  }

  lapply(x, bg_protocol_normalize_value)
}

#' @keywords internal
bg_protocol_identity_string <- function(x) {
  jsonlite::toJSON(
    bg_protocol_normalize_value(x),
    auto_unbox = TRUE,
    null = "null"
  )
}

#' @keywords internal
bg_obligation_id <- function(kind, scope, basis) {
  sprintf(
    "obl_%s",
    digest::digest(
      paste(kind, scope, bg_protocol_identity_string(basis), sep = "|"),
      algo = "xxhash32"
    )
  )
}

#' @keywords internal
bg_action_id <- function(kind, scope, payload) {
  sprintf(
    "act_%s",
    digest::digest(
      paste(kind, scope, bg_protocol_identity_string(payload), sep = "|"),
      algo = "xxhash32"
    )
  )
}

#' @keywords internal
bg_canonicalize_obligation <- function(obligation, pack_ref) {
  basis <- bg_protocol_normalize_value(obligation$basis %||% list())
  metadata <- obligation$metadata %||% list()
  metadata$pack_id <- metadata$pack_id %||% pack_ref$pack_id

  list(
    obligation_id = bg_obligation_id(
      obligation$kind,
      obligation$scope,
      basis
    ),
    kind = obligation$kind,
    scope = obligation$scope,
    severity = obligation$severity,
    title = obligation$title,
    basis = basis,
    explanation = bg_protocol_normalize_value(
      obligation$explanation %||% list()
    ),
    metadata = metadata
  )
}

#' @keywords internal
bg_canonicalize_action <- function(action, pack_ref) {
  payload <- bg_protocol_normalize_value(action$payload %||% list())
  metadata <- action$metadata %||% list()
  metadata$pack_id <- metadata$pack_id %||% pack_ref$pack_id

  list(
    action_id = bg_action_id(
      action$kind,
      action$scope,
      payload
    ),
    kind = action$kind,
    scope = action$scope,
    title = action$title,
    basis = bg_protocol_normalize_value(action$basis %||% list()),
    payload = payload,
    explanation = bg_protocol_normalize_value(action$explanation %||% list()),
    metadata = metadata
  )
}

#' @keywords internal
bg_merge_protocol_items <- function(items, id_field) {
  merged <- list()

  for (item in items) {
    item_id <- item[[id_field]]
    existing <- merged[[item_id]] %||% NULL

    if (is.null(existing)) {
      merged[[item_id]] <- item
      next
    }

    pack_ids <- unique(c(
      existing$metadata$pack_id %||% character(),
      existing$metadata$pack_ids %||% character(),
      item$metadata$pack_id %||% character(),
      item$metadata$pack_ids %||% character()
    ))
    existing$metadata$pack_ids <- sort(pack_ids)
    merged[[item_id]] <- existing
  }

  merged
}

#' @keywords internal
bg_collect_workflow_contexts <- function(project, resolved_scope) {
  if (!identical(resolved_scope, "project")) {
    return(list(bg_build_workflow_context(project, scope = resolved_scope)))
  }

  branch_ids <- sort(names(
    bg_read_branch_registry(project)$branches %||% list()
  ))
  contexts <- list(bg_build_workflow_context(project, scope = "project"))
  branch_contexts <- lapply(branch_ids, function(branch_id) {
    bg_build_workflow_context(project, scope = branch_id)
  })

  c(contexts, branch_contexts)
}

#' @keywords internal
bg_dispatch_obligation_providers <- function(context, pack_ref) {
  pack <- bg_lookup_workflow_pack(pack_ref$pack_id)
  providers <- pack$obligation_providers %||% list()
  obligations <- unlist(
    lapply(providers, function(provider) {
      provider(context, pack_config = pack_ref$config %||% list())
    }),
    recursive = FALSE,
    use.names = FALSE
  )

  lapply(obligations, bg_canonicalize_obligation, pack_ref = pack_ref)
}

#' @keywords internal
bg_dispatch_action_providers <- function(context, obligations, pack_ref) {
  pack <- bg_lookup_workflow_pack(pack_ref$pack_id)
  providers <- pack$action_providers %||% list()
  actions <- unlist(
    lapply(providers, function(provider) {
      provider(
        context,
        obligations = obligations,
        pack_config = pack_ref$config %||% list()
      )
    }),
    recursive = FALSE,
    use.names = FALSE
  )

  lapply(actions, bg_canonicalize_action, pack_ref = pack_ref)
}

#' @keywords internal
bg_blocking_obligation_holds <- function(project, obligations) {
  graph <- bg_read_graph(project)
  holds <- list()

  for (obligation in obligations) {
    if (!identical(obligation$severity, "blocking")) {
      next
    }

    node_ids <- obligation$basis$node_ids %||% character()
    for (node_id in node_ids) {
      descendants <- dagriculture::dagri_descendants(graph, node_id)
      if (length(descendants) == 0) {
        next
      }

      reason <- obligation$title %||% obligation$kind
      for (descendant in descendants) {
        holds[[descendant]] <- reason
      }
    }
  }

  holds
}

#' Compute deterministic next workflow actions
#'
#' @param project A `bg_handle`.
#' @param scope One of `project` or `branch`.
#' @param branch_id Optional branch id, required for branch-scoped queries.
#'
#' @return A `bg_next_actions_result` plain-data list.
#' @export
bg_next_actions <- function(
  project,
  scope = c("project", "branch"),
  branch_id = NULL
) {
  S7::check_is_S7(project, bg_handle)

  resolved_scope <- bg_resolve_workflow_scope(project, scope, branch_id)
  active_packs <- bg_workflow_packs(project)
  contexts <- bg_collect_workflow_contexts(project, resolved_scope)

  obligation_items <- list()
  for (context in contexts) {
    for (pack_ref in active_packs) {
      obligation_items <- c(
        obligation_items,
        bg_dispatch_obligation_providers(context, pack_ref)
      )
    }
  }
  obligations <- bg_merge_protocol_items(obligation_items, "obligation_id")

  action_items <- list()
  for (context in contexts) {
    scoped_obligations <- Filter(
      function(obligation) identical(obligation$scope, context$scope),
      obligations
    )
    for (pack_ref in active_packs) {
      action_items <- c(
        action_items,
        bg_dispatch_action_providers(context, scoped_obligations, pack_ref)
      )
    }
  }
  actions <- bg_merge_protocol_items(action_items, "action_id")

  list(
    context = bg_build_workflow_context(project, scope = resolved_scope),
    obligations = obligations,
    actions = actions,
    metadata = list(
      evaluated_scopes = vapply(contexts, `[[`, character(1), "scope"),
      external_holds = bg_blocking_obligation_holds(project, obligations)
    )
  )
}

#' @keywords internal
bg_default_bayesian_reviewed_summary_ids <- function(decisions) {
  reviewed <- character()

  for (decision in decisions) {
    if (!identical(decision$kind, "computation_review")) {
      next
    }

    reviewed <- c(
      reviewed,
      decision$metadata$summary_ids %||% character(),
      decision$metadata$summary_id %||% character()
    )
  }

  unique(reviewed)
}

#' @keywords internal
bg_default_bayesian_goal_obligations <- function(
  context,
  pack_config = list()
) {
  if (
    !startsWith(context$scope, "branch:") || !is.null(context$inferential_goal)
  ) {
    return(list())
  }

  list(list(
    kind = "set_inferential_goal",
    scope = context$scope,
    severity = "blocking",
    title = "Set an inferential goal",
    basis = list(
      node_ids = context$scope_context$branch_root %||% character(),
      decision_ids = character()
    ),
    explanation = list(
      why = "This branch has no active inferential goal decision.",
      references = character()
    ),
    metadata = list()
  ))
}

#' @keywords internal
bg_default_bayesian_summary_obligations <- function(
  context,
  pack_config = list()
) {
  summaries <- Filter(
    function(summary) {
      !isTRUE(summary$is_stale) &&
        summary$severity %in% c("warning", "error")
    },
    context$evidence$summaries %||% list()
  )
  if (length(summaries) == 0) {
    return(list())
  }

  reviewed_ids <- bg_default_bayesian_reviewed_summary_ids(
    context$evidence$decisions %||% list()
  )
  pending <- Filter(
    function(summary) {
      !summary$summary_id %in% reviewed_ids
    },
    summaries
  )
  if (length(pending) == 0) {
    return(list())
  }

  list(list(
    kind = "review_computation_validity",
    scope = context$scope,
    severity = "blocking",
    title = "Review computation validity",
    basis = list(
      summary_ids = vapply(pending, `[[`, character(1), "summary_id"),
      decision_ids = character(),
      node_ids = unique(vapply(pending, `[[`, character(1), "node_id"))
    ),
    explanation = list(
      why = "Fresh warning or error summaries are active in this scope.",
      references = character()
    ),
    metadata = list(
      summary_kinds = unique(vapply(
        pending,
        `[[`,
        character(1),
        "summary_kind"
      ))
    )
  ))
}

#' @keywords internal
bg_default_bayesian_goal_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  goal_obligation <- Filter(
    function(obligation) {
      identical(obligation$kind, "set_inferential_goal")
    },
    obligations
  )
  if (length(goal_obligation) == 0) {
    return(list())
  }

  list(list(
    kind = "record_decision",
    scope = context$scope,
    title = "Record an inferential goal",
    basis = list(
      obligation_refs = list(list(
        kind = "set_inferential_goal",
        scope = context$scope
      )),
      node_ids = context$scope_context$branch_root %||% character()
    ),
    payload = list(
      decision_type = "goal_update",
      allowed_goal_kinds = pack_config$goal_kinds %||%
        c(
          "observable_prediction",
          "latent_inference"
        )
    ),
    explanation = list(
      why_now = "Branch-scoped workflow guidance stays blocked until a goal is set.",
      references = character()
    ),
    metadata = list()
  ))
}

#' @keywords internal
bg_default_bayesian_summary_actions <- function(
  context,
  obligations,
  pack_config = list()
) {
  review_obligation <- Filter(
    function(obligation) {
      identical(obligation$kind, "review_computation_validity")
    },
    obligations
  )
  if (length(review_obligation) == 0) {
    return(list())
  }

  obligation <- review_obligation[[1]]
  list(list(
    kind = "record_decision",
    scope = context$scope,
    title = "Record a computation review",
    basis = list(
      obligation_refs = list(list(
        kind = "review_computation_validity",
        scope = context$scope
      )),
      node_ids = obligation$basis$node_ids %||% character()
    ),
    payload = list(
      decision_type = "computation_review",
      summary_ids = obligation$basis$summary_ids %||% character(),
      node_ids = obligation$basis$node_ids %||% character()
    ),
    explanation = list(
      why_now = "A blocking computation-validity obligation is active.",
      references = character()
    ),
    metadata = list()
  ))
}
