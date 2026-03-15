# Protocol Value Normalization & Canonicalization
# -----------------------------------------------
# Shared normalization, identity, canonicalization, merging, and
# context collection helpers for the workflow protocol layer.

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

    if (
      identical(id_field, "obligation_id") &&
        !is.null(existing$severity) &&
        !is.null(item$severity)
    ) {
      severity_order <- c(
        advisory = 1L,
        warning = 2L,
        blocking = 3L
      )
      existing_rank <- severity_order[[existing$severity]] %||% 0L
      item_rank <- severity_order[[item$severity]] %||% 0L
      if (item_rank > existing_rank) {
        existing$severity <- item$severity
      }
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

  bg_protocol_named_list(merged)
}

# --- Workflow Context Collection ---

#' @keywords internal
bg_collect_workflow_contexts <- function(project, resolved_scope, plan = NULL) {
  graph <- bg_active_graph(project)
  predicted_fingerprints <- plan$metadata$fingerprints %||% NULL
  artifact_index <- plan$metadata$artifact_index %||% NULL
  all_summaries <- bg_read_summaries(
    project,
    include_stale = TRUE,
    include_inactive = FALSE,
    predicted_fingerprints = predicted_fingerprints,
    artifact_index = artifact_index
  )
  all_decisions <- bg_read_decisions(project)

  enrich_context <- function(context) {
    context$metadata$cross_scope_summaries <- all_summaries
    context$metadata$cross_scope_nodes <- graph$nodes %||% list()
    context$metadata$cross_scope_edges <- graph$edges %||% list()
    context$metadata$cross_scope_decisions <- all_decisions
    context
  }

  if (!identical(resolved_scope, "project")) {
    return(list(enrich_context(
      bg_build_workflow_context_impl(
        project,
        scope = resolved_scope,
        plan = plan
      )
    )))
  }

  branch_ids <- sort(names(
    bg_read_branch_registry(project)$branches %||% list()
  ))
  branch_ids <- intersect(branch_ids, bg_active_branch_ids(project))
  project_context <- enrich_context(
    bg_build_workflow_context_impl(project, scope = "project", plan = plan)
  )
  branch_contexts <- lapply(branch_ids, function(branch_id) {
    enrich_context(bg_build_workflow_context_impl(
      project,
      scope = branch_id,
      plan = plan
    ))
  })

  c(list(project_context), branch_contexts)
}
