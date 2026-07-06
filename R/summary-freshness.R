# Summary persistence, validation, freshness, and reading. Pure moves
# from workflow-context.R (M9 item 1).

# --- Summary Persistence and Reading ---

#' Persist executor summaries
#'
#' @param project A `bg_handle`.
#' @param node_id Node id for the execution.
#' @param artifact_ref Persisted artifact ref.
#' @param execution_fingerprint Predicted execution fingerprint.
#' @param summaries List of emitted summary payloads.
#'
#' @return Named list of persisted summary entries.
#' @export
bg_write_summaries <- function(
  project,
  node_id,
  artifact_ref,
  execution_fingerprint,
  summaries
) {
  summaries <- summaries %||% list()
  if (length(summaries) == 0) {
    return(list())
  }

  scope <- bg_resolve_node_scope(project, node_id)
  created_at <- bg_now_timestamp()
  known_kinds <- bg_known_summary_kinds(project)
  persisted <- list()

  for (summary in summaries) {
    bg_validate_summary(summary, node_id, known_kinds)

    summary_id <- sprintf(
      "sum_%s",
      digest::digest(
        paste(
          node_id,
          execution_fingerprint,
          summary$summary_kind,
          runif(1),
          sep = "|"
        ),
        algo = "xxhash32"
      )
    )

    entry <- list(
      schema_name = "bg_summary_entry",
      schema_version = 2L,
      summary_id = summary_id,
      project_id = project@project_id,
      node_id = node_id,
      artifact_ref = artifact_ref,
      execution_fingerprint = execution_fingerprint,
      summary_kind = summary$summary_kind,
      passed = summary$passed %||% NULL,
      severity = summary$severity %||% "ok",
      metrics = summary$metrics %||% list(),
      scope = scope,
      created_at = created_at,
      metadata = summary$metadata %||% list()
    )

    bg_append_jsonl(bg_summary_log_path(project), entry)
    persisted[[summary_id]] <- entry
  }

  persisted
}

#' Validate a summary record before writing.
#'
#' Aborts on malformed structure (missing/non-string summary_kind, bad severity,
#' non-logical passed, non-named-list metrics). Warns — but still allows the
#' write — when the summary_kind is not in the vocabulary, naming the nearest
#' known kind as a typo suggestion.
#' @keywords internal
#' @noRd
bg_validate_summary <- function(summary, node_id, known_kinds) {
  if (!is.list(summary)) {
    cli::cli_abort(
      "Summary for node {.val {node_id}} must be a list, not {.obj_type_friendly {summary}}."
    )
  }

  kind <- summary$summary_kind %||% NULL
  if (!is.character(kind) || length(kind) != 1 || !nzchar(kind)) {
    cli::cli_abort(
      "Summary for node {.val {node_id}} must have a single non-empty {.field summary_kind}."
    )
  }

  severity <- summary$severity %||% "ok"
  valid_severities <- c("ok", "warning", "error")
  if (!severity %in% valid_severities) {
    cli::cli_abort(
      paste0(
        "Summary {.val {kind}} for node {.val {node_id}} has invalid ",
        "{.field severity} {.val {severity}} ",
        "(must be one of {.val {valid_severities}})."
      )
    )
  }

  passed <- summary$passed %||% NULL
  if (!is.null(passed) && !is.logical(passed)) {
    cli::cli_abort(
      "Summary {.val {kind}} for node {.val {node_id}} has non-logical {.field passed}."
    )
  }

  metrics <- summary$metrics %||% NULL
  if (!is.null(metrics) && !is.list(metrics)) {
    cli::cli_abort(
      "Summary {.val {kind}} for node {.val {node_id}} has non-list {.field metrics}."
    )
  }

  if (length(metrics) > 0 && is.null(names(metrics))) {
    cli::cli_abort(
      "Summary {.val {kind}} for node {.val {node_id}} has unnamed {.field metrics}."
    )
  }

  if (!kind %in% known_kinds) {
    suggestion <- bg_nearest_summary_kind(kind, known_kinds)
    cli::cli_warn(c(
      "Unknown summary kind {.val {kind}} for node {.val {node_id}}.",
      "i" = if (!is.null(suggestion)) {
        sprintf("Did you mean {.val %s}?", suggestion)
      } else {
        "Register it with {.fn bg_register_summary_kind} to silence this warning."
      }
    ))
  }

  invisible(TRUE)
}

#' Determine whether a summary is fresh
#'
#' @param project A `bg_handle`.
#' @param summary A persisted summary entry.
#' @param predicted_fingerprints Optional named fingerprint map.
#' @param artifact_index Optional normalized artifact index.
#'
#' @return Logical scalar.
#' @noRd
bg_summary_is_fresh <- function(
  project,
  summary,
  predicted_fingerprints = NULL,
  artifact_index = NULL
) {
  predicted_fingerprints <- predicted_fingerprints %||%
    bg_predicted_fingerprints(project)
  artifact_index <- artifact_index %||% bg_read_artifact_index(project)

  predicted <- predicted_fingerprints[[summary$node_id]] %||% NULL
  if (
    is.null(predicted) || !identical(summary$execution_fingerprint, predicted)
  ) {
    return(FALSE)
  }

  binding <- bg_artifact_binding(
    artifact_index,
    summary$execution_fingerprint,
    summary$node_id
  )

  !is.null(binding) && identical(binding$status, "active")
}

#' Read persisted summaries
#'
#' @param project A `bg_handle`.
#' @param scope Optional scope filter.
#' @param include_stale Whether to keep stale entries.
#' @param include_inactive Whether to keep summaries from retired or disabled
#'   nodes. Defaults to `TRUE`.
#' @param predicted_fingerprints Optional named fingerprint map.
#' @param artifact_index Optional artifact index.
#'
#' @return Named list of summary entries annotated with `is_fresh` and `is_stale`.
#' @export
bg_read_summaries <- function(
  project,
  scope = NULL,
  include_stale = TRUE,
  include_inactive = TRUE,
  predicted_fingerprints = NULL,
  artifact_index = NULL
) {
  path <- bg_summary_log_path(project)
  if (!file.exists(path)) {
    return(list())
  }

  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) {
    return(list())
  }

  predicted_fingerprints <- predicted_fingerprints %||%
    bg_predicted_fingerprints(project, include_inactive = include_inactive)
  artifact_index <- artifact_index %||% bg_read_artifact_index(project)
  inactive_node_ids <- if (isTRUE(include_inactive)) {
    character()
  } else {
    bg_inactive_node_ids(project)
  }

  summaries <- list()
  for (line in lines) {
    if (trimws(line) == "") {
      next
    }

    entry <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    entry$is_fresh <- bg_summary_is_fresh(
      project = project,
      summary = entry,
      predicted_fingerprints = predicted_fingerprints,
      artifact_index = artifact_index
    )
    entry$is_stale <- !entry$is_fresh

    if (entry$node_id %in% inactive_node_ids) {
      next
    }

    if (
      !is.null(scope) && !bg_summary_scope_matches(project, scope, entry$scope)
    ) {
      next
    }

    if (!include_stale && isTRUE(entry$is_stale)) {
      next
    }

    summaries[[entry$summary_id]] <- entry
  }

  summaries
}
