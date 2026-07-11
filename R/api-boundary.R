#' @title bayesgrove API Boundary Registry
#' @description Internal registry of all exported `bg_*` functions with their
#' classification.
#' @name api_boundary_registry_block
#' @keywords internal
NULL

#' Build the API boundary registry
#'
#' Returns a data frame containing all exported `bg_*` functions with their
#' classification.
#'
#' @return A data.frame with columns: `fn` (function name) and `classification`
#'   (`stable`, `experimental`, `internal`, or `deprecated`).
#'
#' @keywords internal
bg_build_api_boundary_registry <- function() {
  registry <- list(
    # === Stable: Core Project Lifecycle ===
    list(fn = "bg_handle", classification = "stable"),
    list(fn = "bg_init", classification = "stable"),
    list(fn = "bg_open", classification = "stable"),
    list(fn = "bg_close", classification = "stable"),
    list(fn = "bg_use_default_workflow", classification = "stable"),
    list(fn = "bg_use_workflow_packs", classification = "stable"),

    # === Stable: Graph Editing ===
    list(fn = "bg_add_node", classification = "stable"),
    list(fn = "bg_connect", classification = "stable"),
    list(fn = "bg_update_node", classification = "stable"),
    list(fn = "bg_set_node_data", classification = "experimental"),
    list(fn = "bg_remove_node", classification = "stable"),
    list(fn = "bg_read_graph", classification = "stable"),
    list(fn = "bg_commit_graph", classification = "internal"),

    # === Stable: Branching ===
    list(fn = "bg_branch", classification = "stable"),
    list(fn = "bg_branch_with_continuation", classification = "deprecated"),
    list(fn = "bg_branch_lineage", classification = "stable"),
    list(fn = "bg_list_branches", classification = "stable"),
    list(fn = "bg_retire_node", classification = "stable"),
    list(fn = "bg_retire_branch", classification = "stable"),
    list(fn = "bg_invalidate", classification = "stable"),

    # === Stable: Decisions ===
    list(fn = "bg_add_gate", classification = "stable"),
    list(fn = "bg_answer_gate", classification = "stable"),
    list(fn = "bg_pending_gates", classification = "stable"),
    list(fn = "bg_record_decision", classification = "stable"),
    list(fn = "bg_read_decisions", classification = "stable"),
    list(fn = "bg_extension_registry", classification = "experimental"),
    list(fn = "bg_execute_action", classification = "experimental"),

    # === Stable: Execution Control ===
    list(fn = "bg_plan", classification = "stable"),
    list(fn = "bg_run", classification = "stable"),
    list(fn = "bg_pause", classification = "experimental"),
    list(fn = "bg_resume", classification = "experimental"),
    list(fn = "bg_status", classification = "stable"),
    list(fn = "bg_result", classification = "stable"),
    list(fn = "bg_jobs", classification = "stable"),
    list(fn = "bg_compact_jobs", classification = "experimental"),
    list(fn = "bg_fit_stan", classification = "experimental"),
    list(fn = "bg_fit_brms", classification = "experimental"),
    list(fn = "bg_graph_mermaid", classification = "stable"),
    list(fn = "bg_plot", classification = "experimental"),

    # === Stable: Registries and Summaries ===
    list(fn = "bg_read_branch_registry", classification = "stable"),
    list(fn = "bg_read_goal_registry", classification = "stable"),
    list(fn = "bg_read_summaries", classification = "stable"),
    list(fn = "bg_write_summaries", classification = "stable"),
    list(fn = "bg_summary_vocabulary", classification = "experimental"),
    list(fn = "bg_register_summary_kind", classification = "experimental"),
    list(fn = "bg_set_goal", classification = "stable"),
    list(fn = "bg_get_goal", classification = "stable"),
    list(fn = "bg_scope_label", classification = "stable"),
    list(fn = "bg_compute_fingerprint", classification = "stable"),

    # === Stable: Snapshots and Bundling ===
    list(fn = "bg_snapshot", classification = "stable"),
    list(fn = "bg_bundle", classification = "stable"),
    list(fn = "bg_export_report", classification = "stable"),

    # === Experimental: Workflow Protocol ===
    list(fn = "bg_workflow_packs", classification = "experimental"),
    list(fn = "bg_build_workflow_context", classification = "experimental"),
    list(fn = "bg_next_actions", classification = "stable"),
    list(
      fn = "bg_partition_protocol_by_scope",
      classification = "experimental"
    ),

    # === Experimental: Templates ===
    list(fn = "bg_list_templates", classification = "experimental"),

    # === Experimental: REPL ===
    list(fn = "bg_repl", classification = "experimental"),

    # === Internal Exported: Backend Registration ===
    list(fn = "bg_register_node_kind", classification = "internal"),

    # === Experimental: Executor trust model ===
    list(fn = "bg_restore_executors", classification = "experimental"),

    # === Stable: Built-in Backends ===
    list(fn = "bg_use_cmdstanr", classification = "stable"),
    list(fn = "bg_use_brms", classification = "stable"),
    list(fn = "bg_hmc_severity", classification = "stable"),

    # === Stable: API Boundary Introspection ===
    list(fn = "bg_api_boundary", classification = "stable")
  )

  df <- data.frame(
    fn = vapply(registry, `[[`, character(1), "fn"),
    classification = vapply(registry, `[[`, character(1), "classification"),
    stringsAsFactors = FALSE
  )
  df[order(df$fn), ]
}

#' Query the bayesgrove API boundary
#'
#' Returns the API classification registry for all exported `bg_*` functions.
#' Each function is classified as `stable`, `experimental`, `internal`, or
#' `deprecated`.
#'
#' @param fn Optional function name to filter by. If `NULL`, returns all entries.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{fn}{Function name (character)}
#'   \item{classification}{One of `stable`, `experimental`, `internal`, or `deprecated`}
#' }
#'
#' @details
#' ## Classification Meanings
#'
#' \itemize{
#'   \item `stable`: Core public API expected to remain backward-compatible.
#'     Includes project lifecycle, graph editing, branching, decisions,
#'     execution control, status/results, and established workflow entry points.
#'   \item `experimental`: Newer or provisional APIs that may evolve.
#'     Includes workflow protocol/context/template surfaces, REPL, and
#'     practitioner sugar helpers.
#'   \item `internal`: Technically exported for worker/process reasons
#'     but not intended as ergonomic user-facing API. Includes artifact helpers,
#'     job primitives, and backend registration.
#'   \item `deprecated`: Kept for backward compatibility for one release;
#'     warns on use and names its replacement. Currently
#'     `bg_branch_with_continuation()` (use `bg_branch(continue = )`).
#' }
#'
#' @export
#' @examples
#' # Get all API boundary entries
#' bg_api_boundary()
#'
#' # Filter to a specific function
#' bg_api_boundary("bg_run")
#'
#' # Filter to experimental functions
#' subset(bg_api_boundary(), classification == "experimental")
bg_api_boundary <- function(fn = NULL) {
  registry <- bg_build_api_boundary_registry()

  if (!is.null(fn)) {
    if (!is.character(fn) || length(fn) != 1) {
      cli::cli_abort("{.arg fn} must be a single character string or NULL.")
    }
    registry <- registry[registry$fn == fn, , drop = FALSE]
    if (nrow(registry) == 0) {
      cli::cli_warn("Function {.val {fn}} not found in API boundary registry.")
    }
  }

  registry
}

#' Get valid classification values
#'
#' Returns the allowed classification values for the API boundary registry.
#'
#' @return Character vector of valid classifications.
#' @keywords internal
bg_valid_classifications <- function() {
  c("stable", "experimental", "internal", "deprecated")
}

#' Validate API boundary registry integrity
#'
#' Internal helper to validate that the registry is well-formed.
#' Used by tests to catch drift.
#'
#' @return `TRUE` if valid, otherwise raises an error.
#' @keywords internal
bg_validate_api_boundary_registry <- function() {
  registry <- bg_build_api_boundary_registry()

  required_cols <- c("fn", "classification")
  missing_cols <- setdiff(required_cols, names(registry))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Registry missing required columns: {.val {missing_cols}}")
  }

  fn_counts <- table(registry$fn)
  duplicates <- names(fn_counts[fn_counts > 1])
  if (length(duplicates) > 0) {
    cli::cli_abort(
      "Duplicate function entries in registry: {.val {duplicates}}"
    )
  }

  valid_class <- bg_valid_classifications()
  invalid_class <- setdiff(registry$classification, valid_class)
  if (length(invalid_class) > 0) {
    cli::cli_abort(
      "Invalid classifications in registry: {.val {unique(invalid_class)}}"
    )
  }

  invisible(TRUE)
}
