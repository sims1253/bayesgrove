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
    null = "null",
    digits = I(17)
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

  # Advisory mode: downgrade blocking obligations to advisory so they surface
  # in bg_next_actions() and the REPL but never produce external holds.
  severity <- obligation$severity
  config <- pack_ref$config %||% list()
  strictness <- config$strictness %||% "blocking"
  if (
    identical(severity, "blocking") &&
      identical(strictness, "advisory")
  ) {
    severity <- "advisory"
  }

  canonical <- list(
    obligation_id = bg_obligation_id(
      obligation$kind,
      obligation$scope,
      basis
    ),
    kind = obligation$kind,
    scope = obligation$scope,
    severity = severity,
    title = obligation$title,
    basis = basis,
    explanation = bg_protocol_normalize_value(
      obligation$explanation %||% list()
    ),
    metadata = metadata
  )
  bg_assert_protocol_item(canonical, "obligation")
  canonical
}

#' @keywords internal
bg_canonicalize_action <- function(action, pack_ref) {
  payload <- bg_protocol_normalize_value(action$payload %||% list())
  metadata <- action$metadata %||% list()
  metadata$pack_id <- metadata$pack_id %||% pack_ref$pack_id

  canonical <- list(
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
  bg_assert_protocol_item(canonical, "action")
  canonical
}

#' Validate the stable shape at the protocol canonicalization boundary.
#'
#' Checks the fields used by downstream code without rereading the on-disk
#' JSON schema for each provider result. Reports misspelled or missing fields
#' before downstream code receives an obligation or action.
#' @keywords internal
#' @noRd
bg_assert_protocol_item <- function(item, type = c("obligation", "action")) {
  type <- match.arg(type)
  required <- if (identical(type, "obligation")) {
    c("obligation_id", "kind", "scope", "severity", "title", "basis")
  } else {
    c("action_id", "kind", "scope", "title", "basis", "payload")
  }
  missing <- required[vapply(
    required,
    function(field) {
      is.null(item[[field]])
    },
    logical(1)
  )]
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Canonical {type} is missing required field{?s}: {.field {missing}}."
    )
  }

  scalar_strings <- intersect(c("kind", "scope", "severity", "title"), required)
  invalid_strings <- scalar_strings[
    !vapply(
      scalar_strings,
      function(field) {
        value <- item[[field]]
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          nzchar(value)
      },
      logical(1)
    )
  ]
  if (length(invalid_strings) > 0L) {
    cli::cli_abort(
      "Canonical {type} field{?s} must be non-empty strings: {.field {invalid_strings}}."
    )
  }
  if (!is.list(item$basis)) {
    cli::cli_abort("Canonical {type} {.field basis} must be a list.")
  }
  basis_fields <- c(
    "node_ids",
    "summary_ids",
    "decision_ids",
    "branch_ids",
    "obligation_refs",
    "comparison_signature"
  )
  unexpected_basis <- setdiff(names(item$basis) %||% character(), basis_fields)
  if (length(unexpected_basis) > 0L) {
    cli::cli_abort(
      "Canonical {type} {.field basis} has unexpected field{?s}: {.field {unexpected_basis}}."
    )
  }
  id_fields <- intersect(names(item$basis) %||% character(), basis_fields[1:4])
  invalid_ids <- id_fields[
    !vapply(
      id_fields,
      function(field) {
        is.character(item$basis[[field]])
      },
      logical(1)
    )
  ]
  if (length(invalid_ids) > 0L) {
    cli::cli_abort(
      "Canonical {type} basis id field{?s} must be character vectors: {.field {invalid_ids}}."
    )
  }
  if (identical(type, "action") && !is.list(item$payload)) {
    cli::cli_abort("Canonical action {.field payload} must be a list.")
  }
  if (!is.list(item$metadata) || !is.list(item$explanation)) {
    cli::cli_abort(
      "Canonical {type} {.field metadata} and {.field explanation} must be lists."
    )
  }
  if (
    !is.null(item$explanation$references) &&
      !is.character(item$explanation$references)
  ) {
    cli::cli_abort(
      "Canonical {type} {.field explanation$references} must be a character vector."
    )
  }
  if (identical(type, "obligation")) {
    metadata_names <- names(item$metadata) %||% character()
    suspicious_hold_names <- metadata_names[
      grepl("hold.*node.*id", metadata_names) &
        metadata_names != "hold_node_ids"
    ]
    if (length(suspicious_hold_names) > 0L) {
      cli::cli_abort(c(
        "Canonical obligation metadata has an unrecognized hold field: {.field {suspicious_hold_names}}.",
        "i" = "Use {.field hold_node_ids}; misspelling it would silently disable workflow holds."
      ))
    }
    if (
      !is.null(item$metadata$hold_node_ids) &&
        !is.character(item$metadata$hold_node_ids)
    ) {
      cli::cli_abort(
        "Canonical obligation {.field metadata$hold_node_ids} must be a character vector."
      )
    }
  }
  if (
    identical(type, "obligation") &&
      !item$severity %in% c("advisory", "warning", "blocking")
  ) {
    cli::cli_abort(
      "Canonical obligation has invalid severity {.val {item$severity}}."
    )
  }
  invisible(TRUE)
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
bg_collect_workflow_contexts <- function(
  project,
  resolved_scope,
  plan = NULL,
  state = NULL,
  graph = NULL
) {
  raw_graph <- graph
  graph <- bg_active_graph(project, graph = graph)
  predicted_fingerprints <- plan$metadata$fingerprints %||% NULL
  artifact_index <- plan$metadata$artifact_index %||% NULL
  all_summaries <- if (!is.null(state)) {
    state$summaries
  } else {
    bg_read_summaries(
      project,
      include_stale = TRUE,
      include_inactive = FALSE,
      predicted_fingerprints = predicted_fingerprints,
      artifact_index = artifact_index
    )
  }
  all_decisions <- if (!is.null(state)) {
    state$decisions
  } else {
    bg_read_decisions(project)
  }

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
        plan = plan,
        state = state,
        graph = raw_graph
      )
    )))
  }

  branch_ids <- sort(names(
    bg_read_branch_registry(project)$branches %||% list()
  ))
  branch_ids <- intersect(branch_ids, bg_active_branch_ids(project))
  project_context <- enrich_context(
    bg_build_workflow_context_impl(
      project,
      scope = "project",
      plan = plan,
      state = state,
      graph = raw_graph
    )
  )
  branch_contexts <- lapply(branch_ids, function(branch_id) {
    enrich_context(bg_build_workflow_context_impl(
      project,
      scope = branch_id,
      plan = plan,
      state = state,
      graph = raw_graph
    ))
  })

  c(list(project_context), branch_contexts)
}
