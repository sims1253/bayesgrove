#' @title bayesgrove API Boundary Registry
#' @description Internal registry of all exported `bg_*` functions with their
#' classification, short note, and `remote_accessible` flags.
#' @name api_boundary_registry_block
#' @keywords internal
NULL

#' Build the API boundary registry
#'
#' Returns a data frame containing all exported `bg_*` functions with their
#' classification, notes, and remote accessibility flags.
#'
#' @return A data.frame with columns: `fn` (function name), `classification`
#'   (`stable`, `experimental`, or `internal_exported`), `note` (description),
#'   and `remote_accessible` (logical).
#'
#' @keywords internal
bg_build_api_boundary_registry <- function() {
  registry <- list(
    # === Stable: Core Project Lifecycle ===
    list(
      fn = "bg_handle",
      classification = "stable",
      note = "S7 class for project handles",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_init",
      classification = "stable",
      note = "Initialize a new project",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_open",
      classification = "stable",
      note = "Open an existing project",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_close",
      classification = "stable",
      note = "Close a project handle",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_use_default_workflow",
      classification = "experimental",
      note = "Activate the built-in starter workflow packs and node kinds",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_use_workflow_packs",
      classification = "experimental",
      note = "Persist additional workflow-pack activations in project config",
      remote_accessible = TRUE
    ),

    # === Stable: Graph Editing ===
    list(
      fn = "bg_add_node",
      classification = "stable",
      note = "Add a node to the graph",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_connect",
      classification = "stable",
      note = "Connect two nodes",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_update_node",
      classification = "stable",
      note = "Update node properties",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_remove_node",
      classification = "stable",
      note = "Remove a node from the graph",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_read_graph",
      classification = "stable",
      note = "Read the project graph",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_commit_graph",
      classification = "internal_exported",
      note = "Internal graph persistence helper",
      remote_accessible = FALSE
    ),

    # === Stable: Branching ===
    list(
      fn = "bg_branch",
      classification = "stable",
      note = "Create a branch from a node",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_branch_with_continuation",
      classification = "experimental",
      note = "Branch with downstream continuation (newer)",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_branch_lineage",
      classification = "stable",
      note = "Get ancestor branch lineage",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_list_branches",
      classification = "stable",
      note = "List all branches",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_retire_node",
      classification = "stable",
      note = "Retire a node from planning",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_retire_branch",
      classification = "stable",
      note = "Retire a branch from planning",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_invalidate",
      classification = "stable",
      note = "Invalidate cached results",
      remote_accessible = FALSE
    ),

    # === Stable: Decisions ===
    list(
      fn = "bg_add_gate",
      classification = "stable",
      note = "Add a decision gate",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_answer_gate",
      classification = "stable",
      note = "Answer a decision gate",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_pending_gates",
      classification = "stable",
      note = "List pending gates",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_record_decision",
      classification = "stable",
      note = "Record an explicit decision",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_extension_registry",
      classification = "experimental",
      note = "Return the descriptive extension registry",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_execute_action",
      classification = "experimental",
      note = "Execute a protocol action inside bayesgrove",
      remote_accessible = TRUE
    ),

    # === Stable: Execution Control ===
    list(
      fn = "bg_plan",
      classification = "stable",
      note = "Create an execution plan",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_run",
      classification = "stable",
      note = "Run workflow synchronously",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_submit",
      classification = "stable",
      note = "Submit workflow asynchronously",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_wait",
      classification = "stable",
      note = "Wait for async jobs",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_cancel",
      classification = "stable",
      note = "Cancel an async run",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_pause",
      classification = "stable",
      note = "Pause workflow execution",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_resume",
      classification = "stable",
      note = "Resume workflow execution",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_status",
      classification = "stable",
      note = "Get workflow status",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_result",
      classification = "stable",
      note = "Retrieve a node result",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_jobs",
      classification = "stable",
      note = "List job records",
      remote_accessible = FALSE
    ),

    # === Stable: Registries and Summaries ===
    list(
      fn = "bg_read_branch_registry",
      classification = "stable",
      note = "Read branch registry",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_read_goal_registry",
      classification = "stable",
      note = "Read goal registry",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_read_summaries",
      classification = "stable",
      note = "Read persisted summaries",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_write_summaries",
      classification = "stable",
      note = "Write executor summaries",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_summary_is_fresh",
      classification = "stable",
      note = "Check summary freshness",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_set_goal",
      classification = "stable",
      note = "Set branch inferential goal",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_get_goal",
      classification = "stable",
      note = "Get branch goal",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_resolve_node_scope",
      classification = "stable",
      note = "Resolve node workflow scope",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_scope_label",
      classification = "stable",
      note = "Get display label for scope",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_compute_fingerprint",
      classification = "stable",
      note = "Compute cache fingerprint",
      remote_accessible = FALSE
    ),

    # === Stable: Snapshots and Bundling ===
    list(
      fn = "bg_snapshot",
      classification = "stable",
      note = "Get project snapshot",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_bundle",
      classification = "stable",
      note = "Bundle project for handoff",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_export_report",
      classification = "stable",
      note = "Export workflow report",
      remote_accessible = FALSE
    ),

    # === Stable: Backend Plugins ===
    list(
      fn = "bg_cmdstanr_plugin",
      classification = "stable",
      note = "cmdstanr backend plugin",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_brms_plugin",
      classification = "stable",
      note = "brms backend plugin",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_diagnostics_plugin",
      classification = "stable",
      note = "Diagnostics plugin",
      remote_accessible = FALSE
    ),

    # === Experimental: Workflow Protocol ===
    list(
      fn = "bg_workflow_packs",
      classification = "experimental",
      note = "List active workflow packs",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_workflow_context",
      classification = "experimental",
      note = "Build workflow context for protocol",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_build_workflow_context",
      classification = "experimental",
      note = "Build workflow context (lower-level)",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_next_actions",
      classification = "experimental",
      note = "Compute next workflow actions",
      remote_accessible = TRUE
    ),
    list(
      fn = "bg_partition_protocol_by_scope",
      classification = "experimental",
      note = "Partition protocol results by scope",
      remote_accessible = FALSE
    ),

    # === Experimental: Templates ===
    list(
      fn = "bg_list_templates",
      classification = "experimental",
      note = "List built-in templates",
      remote_accessible = FALSE
    ),

    # === Experimental: REPL ===
    list(
      fn = "bg_repl",
      classification = "experimental",
      note = "Interactive REPL",
      remote_accessible = FALSE
    ),

    # === Internal Exported: Artifact Helpers ===
    list(
      fn = "bg_check_artifact",
      classification = "internal_exported",
      note = "Check artifact cache (worker-facing)",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_store_artifact",
      classification = "internal_exported",
      note = "Store artifact in cache (worker-facing)",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_fetch_artifact",
      classification = "internal_exported",
      note = "Fetch artifact from cache (worker-facing)",
      remote_accessible = FALSE
    ),

    # === Internal Exported: Job Primitives ===
    list(
      fn = "bg_log_job",
      classification = "internal_exported",
      note = "Log job entry (worker-facing)",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_create_job",
      classification = "internal_exported",
      note = "Create job record (worker-facing)",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_update_job",
      classification = "internal_exported",
      note = "Update job record (worker-facing)",
      remote_accessible = FALSE
    ),

    # === Internal Exported: Backend Registration ===
    list(
      fn = "bg_register_backend",
      classification = "internal_exported",
      note = "Register backend implementation",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_register_node_kind",
      classification = "internal_exported",
      note = "Register node kind executor",
      remote_accessible = FALSE
    ),

    # === Internal Exported: Daemon Reconciliation ===
    list(
      fn = "bg_reconcile_daemon_jobs",
      classification = "internal_exported",
      note = "Reconcile background daemon jobs",
      remote_accessible = FALSE
    ),

    # === Internal Exported: Worker Process ===
    list(
      fn = "bg_worker_process",
      classification = "internal_exported",
      note = "Worker process entry point (cross-process)",
      remote_accessible = FALSE
    ),

    # === Stable: API Boundary Introspection ===
    list(
      fn = "bg_api_boundary",
      classification = "stable",
      note = "Query API classification registry",
      remote_accessible = FALSE
    ),
    list(
      fn = "bg_serve",
      classification = "experimental",
      note = "Serve the local experimental IPC protocol",
      remote_accessible = FALSE
    )
  )

  # Convert to data.frame and sort by function name
  df <- do.call(
    rbind,
    lapply(registry, function(row) {
      data.frame(
        fn = row$fn,
        classification = row$classification,
        note = row$note,
        remote_accessible = row$remote_accessible,
        stringsAsFactors = FALSE
      )
    })
  )
  df[order(df$fn), ]
}

#' Query the bayesgrove API boundary
#'
#' Returns the API classification registry for all exported `bg_*` functions.
#' Each function is classified as `stable`, `experimental`, or `internal_exported`,
#' and carries a `remote_accessible` flag indicating whether it is part of the
#' remote protocol contract.
#'
#' @param fn Optional function name to filter by. If `NULL`, returns all entries.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{fn}{Function name (character)}
#'   \item{classification}{One of `stable`, `experimental`, or `internal_exported`}
#'   \item{note}{Short description of the function's role}
#'   \item{remote_accessible}{Logical indicating if exposed via remote protocol}
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
#'     newer branch continuation helpers.
#'   \item `internal_exported`: Technically exported for worker/process reasons
#'     but not intended as ergonomic user-facing API. Includes artifact helpers,
#'     job primitives, backend registration, and daemon reconciliation.
#' }
#'
#' ## Remote Accessibility
#'
#' The `remote_accessible` column indicates whether a function is part of the
#' IPC contract exposed via remote protocol (e.g., WebSocket). Functions marked
#' `TRUE` form a deliberate public contract for external clients. The
#' remote-accessible set is intentionally smaller than the full in-process R API
#' and backs the experimental `bg_serve()` protocol boundary.
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
  c("stable", "experimental", "internal_exported")
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

  # Check required columns
  required_cols <- c("fn", "classification", "note", "remote_accessible")
  missing_cols <- setdiff(required_cols, names(registry))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Registry missing required columns: {.val {missing_cols}}")
  }

  # Check no duplicate function names
  fn_counts <- table(registry$fn)
  duplicates <- names(fn_counts[fn_counts > 1])
  if (length(duplicates) > 0) {
    cli::cli_abort(
      "Duplicate function entries in registry: {.val {duplicates}}"
    )
  }

  # Check valid classifications
  valid_class <- bg_valid_classifications()
  invalid_class <- setdiff(registry$classification, valid_class)
  if (length(invalid_class) > 0) {
    cli::cli_abort(
      "Invalid classifications in registry: {.val {unique(invalid_class)}}"
    )
  }

  # Check remote_accessible is logical
  if (!is.logical(registry$remote_accessible)) {
    cli::cli_abort("{.col remote_accessible} must be logical.")
  }
  if (anyNA(registry$remote_accessible)) {
    cli::cli_abort("{.col remote_accessible} must not contain NA values.")
  }

  # Check notes are non-empty
  empty_notes <- registry$fn[!nzchar(trimws(registry$note))]
  if (length(empty_notes) > 0) {
    cli::cli_abort("Empty notes for functions: {.val {empty_notes}}")
  }

  invisible(TRUE)
}
