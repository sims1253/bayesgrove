# Registry persistence: branch registry, goal registry, lifecycle state
# helpers, branch records, and branch listing / scope labels.

# --- Workflow Directory and Registry Paths ---

#' @keywords internal
bg_workflow_dir <- function(project) {
  path <- bg_storage_path(project, "workflow")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' @keywords internal
bg_branch_registry_path <- function(project) {
  file.path(bg_workflow_dir(project), "branches.json")
}

#' @keywords internal
bg_goal_registry_path <- function(project) {
  file.path(bg_workflow_dir(project), "goals.json")
}

#' @keywords internal
bg_summary_log_path <- function(project) {
  file.path(bg_workflow_dir(project), "summaries.jsonl")
}

#' @keywords internal
bg_empty_branch_registry <- function(project) {
  list(
    schema_name = "bg_branch_registry",
    schema_version = 1L,
    project_id = project@project_id,
    branches = list()
  )
}

#' @keywords internal
bg_empty_goal_registry <- function(project) {
  list(
    schema_name = "bg_goal_registry",
    schema_version = 1L,
    project_id = project@project_id,
    branch_goals = list()
  )
}

# --- Branch Registry IO ---

#' Read the persisted branch registry
#'
#' @param project A `bg_handle`.
#'
#' @return A plain-data branch registry document.
#' @export
bg_read_branch_registry <- function(project) {
  path <- bg_branch_registry_path(project)
  if (!file.exists(path)) {
    return(bg_empty_branch_registry(project))
  }

  registry <- jsonlite::read_json(path, simplifyVector = FALSE)
  registry$branches <- registry$branches %||% list()
  registry
}

#' @keywords internal
bg_write_branch_registry <- function(project, registry) {
  bg_write_json_atomic(bg_branch_registry_path(project), registry)
}

#' @keywords internal
bg_update_branch_metadata <- function(project, branch_id, metadata = list()) {
  registry <- bg_read_branch_registry(project)
  record <- registry$branches[[branch_id]] %||% NULL

  if (is.null(record)) {
    cli::cli_abort("Branch {.val {branch_id}} not found in branch registry.")
  }

  record$metadata <- utils::modifyList(record$metadata %||% list(), metadata)
  registry$branches[[branch_id]] <- record
  bg_write_branch_registry(project, registry)

  invisible(record)
}

# --- Lifecycle State Helpers ---

#' @keywords internal
bg_lifecycle_state <- function(metadata = list()) {
  metadata$lifecycle %||% "active"
}

#' @keywords internal
bg_is_active_lifecycle <- function(state) {
  identical(state %||% "active", "active")
}

#' @keywords internal
bg_node_lifecycle <- function(node) {
  bg_lifecycle_state(node$metadata %||% list())
}

#' @keywords internal
bg_branch_lifecycle <- function(branch) {
  bg_lifecycle_state(branch$metadata %||% list())
}

#' @keywords internal
bg_active_branch_ids <- function(project) {
  branches <- bg_read_branch_registry(project)$branches %||% list()
  Filter(
    function(branch_id) {
      bg_is_active_lifecycle(
        bg_branch_lifecycle(branches[[branch_id]])
      )
    },
    names(branches)
  )
}

#' @keywords internal
bg_inactive_node_ids <- function(project, graph = NULL) {
  graph <- graph %||% bg_read_graph(project)
  node_ids <- names(graph$nodes %||% list())
  if (length(node_ids) == 0) {
    return(character())
  }

  active_branch_ids <- bg_active_branch_ids(project)
  inactive <- character()

  for (node_id in node_ids) {
    node <- graph$nodes[[node_id]] %||% list()
    if (!bg_is_active_lifecycle(bg_node_lifecycle(node))) {
      inactive <- c(inactive, node_id)
      next
    }

    node_scope <- bg_resolve_node_scope(project, node_id)
    if (
      startsWith(node_scope, "branch:") &&
        !node_scope %in% active_branch_ids
    ) {
      inactive <- c(inactive, node_id)
    }
  }

  sort(unique(inactive))
}

#' @keywords internal
bg_active_graph <- function(project, graph = NULL) {
  graph <- graph %||% bg_read_graph(project)
  inactive_node_ids <- bg_inactive_node_ids(project, graph = graph)
  active_node_ids <- setdiff(names(graph$nodes %||% list()), inactive_node_ids)

  graph$nodes <- graph$nodes[active_node_ids]
  graph$edges <- Filter(
    function(edge) {
      edge$from %in% active_node_ids && edge$to %in% active_node_ids
    },
    graph$edges %||% list()
  )

  active_edge_ids <- names(graph$edges %||% list())
  if (!is.null(graph$gates)) {
    graph$gates <- Filter(
      function(gate) {
        (gate$edge_id %||% NULL) %in% active_edge_ids
      },
      graph$gates
    )
  }

  graph
}

# --- Branch Record Creation and Registration ---

#' @keywords internal
bg_new_branch_record <- function(
  project,
  root_node_id,
  source_node_id,
  label = NULL,
  metadata = list()
) {
  created_at <- bg_now_timestamp()
  branch_id <- sprintf(
    "branch:%s",
    digest::digest(
      paste(root_node_id, source_node_id, created_at, sep = "|"),
      algo = "xxhash32"
    )
  )

  list(
    branch_id = branch_id,
    root_node_id = root_node_id,
    source_node_id = source_node_id,
    label = label,
    created_at = created_at,
    metadata = metadata %||% list()
  )
}

#' @keywords internal
bg_register_branch <- function(
  project,
  root_node_id,
  source_node_id,
  label = NULL,
  metadata = list()
) {
  registry <- bg_read_branch_registry(project)
  record <- bg_new_branch_record(
    project = project,
    root_node_id = root_node_id,
    source_node_id = source_node_id,
    label = label,
    metadata = metadata
  )
  registry$branches[[record$branch_id]] <- record
  bg_write_branch_registry(project, registry)
  record
}

# --- Goal Registry IO ---

#' Read the persisted goal registry
#'
#' @param project A `bg_handle`.
#'
#' @return A plain-data goal registry document.
#' @export
bg_read_goal_registry <- function(project) {
  path <- bg_goal_registry_path(project)
  if (!file.exists(path)) {
    return(bg_empty_goal_registry(project))
  }

  registry <- jsonlite::read_json(path, simplifyVector = FALSE)
  registry$branch_goals <- registry$branch_goals %||% list()
  registry
}

#' @keywords internal
bg_write_goal_registry <- function(project, registry) {
  bg_write_json_atomic(bg_goal_registry_path(project), registry)
}

#' @keywords internal
bg_goal_from_decision <- function(decision) {
  if (!identical(decision$kind, "goal_update")) {
    return(NULL)
  }

  goal <- decision$metadata$goal %||% list()
  goal_kind <- goal$kind %||% decision$choice
  if (is.null(goal_kind) || !startsWith(decision$scope, "branch:")) {
    return(NULL)
  }

  list(
    goal_id = goal$goal_id %||%
      sprintf(
        "goal_%s",
        digest::digest(
          paste(decision$decision_id, goal_kind, sep = "|"),
          algo = "xxhash32"
        )
      ),
    kind = goal_kind,
    scope = decision$scope,
    label = goal$label %||% decision$choice,
    decision_id = decision$decision_id,
    updated_at = decision$created_at,
    metadata = goal$metadata %||% list()
  )
}

#' @keywords internal
bg_update_goal_registry_from_decision <- function(project, decision) {
  goal <- bg_goal_from_decision(decision)
  if (is.null(goal)) {
    return(invisible(NULL))
  }

  registry <- bg_read_goal_registry(project)
  registry$branch_goals[[goal$scope]] <- goal
  bg_write_goal_registry(project, registry)
  invisible(goal)
}

#' Set the active inferential goal for a branch
#'
#' @param project A `bg_handle`.
#' @param branch_id A branch scope id such as `branch:...`.
#' @param kind Goal kind string.
#' @param label Optional user-facing label.
#' @param rationale Required rationale string.
#' @param metadata Optional metadata.
#' @param goal_id Optional goal id.
#'
#' @return The recorded decision.
#' @export
bg_set_goal <- function(
  project,
  branch_id,
  kind,
  label = NULL,
  rationale,
  metadata = list(),
  goal_id = NULL
) {
  if (!startsWith(branch_id, "branch:")) {
    cli::cli_abort("Goals must be recorded against a branch scope.")
  }

  bg_record_decision(
    project = project,
    scope = branch_id,
    kind = "goal_update",
    prompt = "Set inferential goal",
    choice = label %||% kind,
    rationale = rationale,
    metadata = list(
      goal = list(
        goal_id = goal_id,
        kind = kind,
        label = label %||% kind,
        metadata = metadata %||% list()
      )
    )
  )
}

#' Get the active inferential goal for a scope
#'
#' @param project A `bg_handle`.
#' @param scope A scope string.
#'
#' @return A goal record or `NULL`.
#' @export
bg_get_goal <- function(project, scope) {
  if (!startsWith(scope, "branch:")) {
    return(NULL)
  }

  bg_read_goal_registry(project)$branch_goals[[scope]] %||% NULL
}

# --- Branch Listing and Scope Labels ---

#' List all branches with their metadata
#'
#' Returns a data-frame-like list of all registered branches with their
#' identifying information and optional display labels.
#'
#' @param project A `bg_handle`.
#'
#' @return A named list of branch records, each containing `branch_id`,
#'   `label`, `root_node_id`, `source_node_id`, `created_at`, and `has_goal`.
#' @export
bg_list_branches <- function(project) {
  S7::check_is_S7(project, bg_handle)

  branches <- bg_read_branch_registry(project)$branches %||% list()
  if (length(branches) == 0) {
    return(list())
  }

  goals <- bg_read_goal_registry(project)$branch_goals %||% list()

  lapply(branches, function(record) {
    list(
      branch_id = record$branch_id,
      label = record$label %||% "",
      root_node_id = record$root_node_id,
      source_node_id = record$source_node_id,
      created_at = record$created_at,
      has_goal = !is.null(goals[[record$branch_id]]),
      lifecycle = bg_branch_lifecycle(record)
    )
  })
}

#' Get a display label for a scope
#'
#' Returns a human-readable label for a scope string. For project scope,
#' returns "Project". For branch scopes, returns the branch label or a
#' truncated branch id.
#'
#' @param project A `bg_handle`.
#' @param scope A scope string like "project" or "branch:...".
#'
#' @return A character string suitable for display.
#' @export
bg_scope_label <- function(project, scope) {
  S7::check_is_S7(project, bg_handle)

  if (identical(scope, "project")) {
    return("Project")
  }

  if (!startsWith(scope, "branch:")) {
    return(scope)
  }

  branches <- bg_read_branch_registry(project)$branches
  record <- branches[[scope]] %||% NULL

  if (!is.null(record) && !is.null(record$label) && nzchar(record$label)) {
    return(record$label)
  }

  # Fall back to truncated branch id
  sprintf("Branch %s", substr(scope, 8, 14))
}

#' @keywords internal
bg_scope_node_ids <- function(
  project,
  scope,
  include_inactive = FALSE,
  graph = NULL
) {
  graph <- graph %||% bg_read_graph(project)
  node_ids <- names(graph$nodes)
  scope_resolution <- bg_node_scope_resolution(project, graph = graph)
  node_scopes <- stats::setNames(
    vapply(
      node_ids,
      function(node_id) {
        bg_resolve_node_scope(
          project,
          node_id,
          scope_resolution = scope_resolution
        )
      },
      character(1)
    ),
    node_ids
  )

  scope_node_ids <- node_ids[node_scopes == scope]
  if (isTRUE(include_inactive)) {
    return(scope_node_ids)
  }

  setdiff(scope_node_ids, bg_inactive_node_ids(project, graph = graph))
}
