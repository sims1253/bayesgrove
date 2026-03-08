#' @keywords internal
bg_now_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' @keywords internal
bg_storage_path <- function(project, ...) {
  file.path(project@path, ".bayesgrove", ...)
}

#' @keywords internal
bg_sort_persisted_value <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(x)
  }

  if (is.list(x)) {
    if (!is.null(names(x))) {
      nm <- names(x)
      ord <- order(nm)
      x <- x[ord]
      names(x) <- nm[ord]
    }

    return(lapply(x, bg_sort_persisted_value))
  }

  x
}

#' @keywords internal
bg_write_json_atomic <- function(path, data, sort_keys = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  payload <- if (isTRUE(sort_keys)) bg_sort_persisted_value(data) else data

  jsonlite::write_json(
    payload,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    force = TRUE
  )

  file.rename(tmp, path)
  invisible(TRUE)
}

#' @keywords internal
bg_with_file_lock <- function(lock_path, code, timeout = 10, poll = 0.05) {
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  acquired <- FALSE
  start <- Sys.time()

  while (!acquired) {
    acquired <- dir.create(lock_path, recursive = FALSE, showWarnings = FALSE)
    if (acquired) {
      break
    }

    if (as.numeric(Sys.time() - start, units = "secs") >= timeout) {
      cli::cli_abort(
        "Timed out waiting for project lock at {.path {lock_path}}."
      )
    }

    Sys.sleep(poll)
  }

  on.exit(unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
  eval.parent(substitute(code))
}

#' @keywords internal
bg_append_jsonl <- function(path, record) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock_path <- paste0(path, ".lock")

  bg_with_file_lock(lock_path, {
    json_line <- jsonlite::toJSON(
      bg_sort_persisted_value(record),
      auto_unbox = TRUE,
      null = "null"
    )
    cat(paste0(json_line, "\n"), file = path, append = TRUE)
    invisible(TRUE)
  })
}

#' @keywords internal
bg_artifact_index_path <- function(project) {
  file.path(project@path, ".bayesgrove", "cache", "index.json")
}

#' @keywords internal
bg_modify_artifact_index <- function(project, code, timeout = 10, poll = 0.05) {
  bg_with_file_lock(
    paste0(bg_artifact_index_path(project), ".lock"),
    code,
    timeout = timeout,
    poll = poll
  )
}

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

#' @keywords internal
bg_normalize_artifact_entry <- function(fingerprint, entry) {
  entry <- entry %||% list()
  bindings <- entry$bindings %||% list()

  if (length(bindings) == 0 && !is.null(entry$node_id)) {
    bindings[[entry$node_id]] <- list(
      node_id = entry$node_id,
      status = entry$status %||% "active",
      updated_at = entry$updated_at %||% entry$created_at %||% NULL
    )
  }

  if (length(bindings) > 0) {
    for (node_id in names(bindings)) {
      bindings[[node_id]] <- utils::modifyList(
        list(
          node_id = node_id,
          status = "active",
          updated_at = entry$updated_at %||% entry$created_at %||% NULL
        ),
        bindings[[node_id]]
      )
    }
  }

  active_bindings <- names(Filter(
    function(x) identical(x$status, "active"),
    bindings
  ))
  first_active <- if (length(active_bindings) > 0) {
    active_bindings[[1]]
  } else {
    NULL
  }
  first_binding <- if (length(bindings) > 0) names(bindings)[[1]] else NULL
  entry$status <- entry$status %||%
    if (length(active_bindings) > 0) "active" else "superseded"
  entry$node_id <- entry$node_id %||% first_active %||% first_binding %||% NULL
  entry$artifact_ref <- entry$artifact_ref %||% NULL
  entry$created_at <- entry$created_at %||% NULL
  entry$updated_at <- entry$updated_at %||% entry$created_at %||% NULL
  entry$bindings <- bindings
  entry$fingerprint <- fingerprint
  entry
}

#' @keywords internal
bg_normalize_artifact_index <- function(index) {
  index <- index %||% list()
  if (
    identical(index$schema_name %||% NULL, "bg_artifact_index") &&
      !is.null(index$entries)
  ) {
    entries <- index$entries %||% list()
    normalized <- list()

    for (node_id in names(entries)) {
      node_entries <- entries[[node_id]] %||% list()
      for (fingerprint in names(node_entries)) {
        leaf <- node_entries[[fingerprint]] %||% list()
        entry <- normalized[[fingerprint]] %||%
          list(
            artifact_ref = leaf$artifact_ref %||% NULL,
            created_at = leaf$created_at %||% NULL,
            updated_at = leaf$updated_at %||% leaf$created_at %||% NULL,
            metadata = leaf$metadata %||% list(),
            bindings = list()
          )
        entry$artifact_ref <- entry$artifact_ref %||%
          leaf$artifact_ref %||%
          NULL
        entry$created_at <- entry$created_at %||% leaf$created_at %||% NULL
        entry$updated_at <- leaf$updated_at %||%
          leaf$created_at %||%
          entry$updated_at %||%
          NULL
        entry$metadata <- utils::modifyList(
          entry$metadata %||% list(),
          leaf$metadata %||% list()
        )
        entry$bindings[[node_id]] <- list(
          node_id = node_id,
          status = leaf$status %||% "active",
          updated_at = leaf$updated_at %||% leaf$created_at %||% NULL
        )
        normalized[[fingerprint]] <- entry
      }
    }

    index <- normalized
  }

  stats::setNames(
    lapply(names(index), function(fingerprint) {
      bg_normalize_artifact_entry(fingerprint, index[[fingerprint]])
    }),
    names(index)
  )
}

#' @keywords internal
bg_write_artifact_index <- function(project, index) {
  entries <- list()

  for (fingerprint in names(index)) {
    entry <- index[[fingerprint]]
    bindings <- entry$bindings %||% list()
    for (node_id in names(bindings)) {
      binding <- bindings[[node_id]]
      entries[[node_id]] <- entries[[node_id]] %||% list()
      entries[[node_id]][[fingerprint]] <- list(
        artifact_ref = entry$artifact_ref %||% NULL,
        status = binding$status %||% "active",
        created_at = entry$created_at %||% NULL,
        updated_at = binding$updated_at %||% entry$updated_at %||% NULL,
        metadata = entry$metadata %||% list()
      )
    }
  }

  persisted <- list(
    schema_name = "bg_artifact_index",
    schema_version = 1L,
    project_id = project@project_id,
    entries = entries
  )

  bg_write_json_atomic(
    bg_storage_path(project, "cache", "index.json"),
    persisted
  )
}

#' @keywords internal
bg_artifact_binding <- function(index, fingerprint, node_id) {
  entry <- index[[fingerprint]]
  if (is.null(entry)) {
    return(NULL)
  }
  entry$bindings[[node_id]] %||% NULL
}

#' @keywords internal
bg_any_active_artifact_binding <- function(entry) {
  any(vapply(
    entry$bindings %||% list(),
    function(binding) identical(binding$status, "active"),
    logical(1)
  ))
}

#' @keywords internal
bg_refresh_artifact_entry <- function(entry) {
  active_nodes <- names(Filter(
    function(binding) identical(binding$status, "active"),
    entry$bindings %||% list()
  ))
  first_active <- if (length(active_nodes) > 0) active_nodes[[1]] else NULL
  first_binding <- if (length(entry$bindings) > 0) {
    names(entry$bindings)[[1]]
  } else {
    NULL
  }

  entry$status <- if (length(active_nodes) > 0) "active" else "superseded"
  entry$node_id <- first_active %||% first_binding %||% entry$node_id %||% NULL
  entry$updated_at <- bg_now_timestamp()
  entry
}

#' @keywords internal
bg_supersede_artifacts_for_node <- function(
  index,
  node_id,
  except_fingerprint = NULL
) {
  changed <- FALSE
  count <- 0L

  for (fingerprint in names(index)) {
    if (
      !is.null(except_fingerprint) && identical(fingerprint, except_fingerprint)
    ) {
      next
    }

    binding <- index[[fingerprint]]$bindings[[node_id]] %||% NULL
    if (!is.null(binding) && identical(binding$status, "active")) {
      index[[fingerprint]]$bindings[[node_id]]$status <- "superseded"
      index[[fingerprint]]$bindings[[node_id]]$updated_at <- bg_now_timestamp()
      index[[fingerprint]] <- bg_refresh_artifact_entry(index[[fingerprint]])
      changed <- TRUE
      count <- count + 1L
    }
  }

  list(index = index, changed = changed, count = count)
}

#' @keywords internal
bg_reverse_edge_map <- function(graph) {
  incoming <- stats::setNames(
    vector("list", length(graph$nodes)),
    names(graph$nodes)
  )
  for (edge in graph$edges) {
    incoming[[edge$to]] <- c(incoming[[edge$to]], edge$from)
  }
  incoming
}

#' @keywords internal
bg_node_scope_resolution <- function(project, graph = NULL, branches = NULL) {
  graph <- graph %||% bg_read_graph(project)
  branches <- branches %||% bg_read_branch_registry(project)$branches %||% list()
  branch_ids <- names(branches) %||% character(length(branches))
  roots <- if (length(branches) == 0) {
    character()
  } else {
    vapply(branches, `[[`, character(1), "root_node_id")
  }

  list(
    graph = graph,
    branches = branches,
    incoming = bg_reverse_edge_map(graph),
    root_to_branch = stats::setNames(branch_ids, roots)
  )
}

#' Resolve the workflow scope for a node
#'
#' @param project A `bg_handle`.
#' @param node_id Node id.
#' @param graph Optional pre-read graph.
#' @param branches Optional pre-read branch registry entries.
#' @param scope_resolution Optional scope-resolution cache created by
#'   `bg_node_scope_resolution()`.
#'
#' @return A scope string, either `project` or a branch id.
#' @export
bg_resolve_node_scope <- function(
  project,
  node_id,
  graph = NULL,
  branches = NULL,
  scope_resolution = NULL
) {
  scope_resolution <- scope_resolution %||%
    bg_node_scope_resolution(project, graph = graph, branches = branches)
  graph <- scope_resolution$graph
  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  branches <- scope_resolution$branches
  if (length(branches) == 0) {
    return("project")
  }

  root_to_branch <- scope_resolution$root_to_branch

  if (node_id %in% names(root_to_branch)) {
    return(root_to_branch[[node_id]])
  }

  incoming <- scope_resolution$incoming
  frontier <- node_id
  visited <- stats::setNames(0L, node_id)
  distance <- 0L

  repeat {
    hits <- frontier[frontier %in% names(root_to_branch)]
    if (length(hits) > 0) {
      branch_ids <- unique(unname(root_to_branch[hits]))
      if (length(branch_ids) == 1) {
        return(branch_ids[[1]])
      }
      return("project")
    }

    next_frontier <- character(0)
    for (current in frontier) {
      parents <- incoming[[current]] %||% character(0)
      for (parent in parents) {
        if (!parent %in% names(visited)) {
          visited[[parent]] <- distance + 1L
          next_frontier <- c(next_frontier, parent)
        }
      }
    }

    if (length(next_frontier) == 0) {
      return("project")
    }

    frontier <- unique(next_frontier)
    distance <- distance + 1L
  }
}

#' Get the ancestor branch lineage for a branch
#'
#' @param project A `bg_handle`.
#' @param branch_id A branch id.
#'
#' @return Character vector of ancestor branch ids.
#' @export
bg_branch_lineage <- function(project, branch_id) {
  scope_resolution <- bg_node_scope_resolution(project)
  branches <- scope_resolution$branches
  record <- branches[[branch_id]] %||% NULL
  if (is.null(record)) {
    return(character())
  }

  lineage <- character()
  seen <- branch_id
  source_node_id <- record$source_node_id

  repeat {
    parent_scope <- bg_resolve_node_scope(
      project,
      source_node_id,
      scope_resolution = scope_resolution
    )
    if (
      !startsWith(parent_scope, "branch:") ||
        parent_scope %in% seen ||
        is.null(branches[[parent_scope]])
    ) {
      break
    }

    lineage <- c(lineage, parent_scope)
    seen <- c(seen, parent_scope)
    source_node_id <- branches[[parent_scope]]$source_node_id
  }

  lineage
}

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

#' @keywords internal
bg_predicted_fingerprints <- function(project, include_inactive = TRUE) {
  bg_plan(
    project,
    mode = "sync",
    include_inactive = include_inactive
  )$metadata$fingerprints %||%
    list()
}

#' @keywords internal
bg_scope_supports_branch_lineage <- function(scope) {
  startsWith(scope, "branch:")
}

#' @keywords internal
bg_scope_matches <- function(project, scope, candidate_scope) {
  if (identical(scope, "project")) {
    return(identical(candidate_scope, "project"))
  }

  valid_scopes <- c("project", scope)
  if (bg_scope_supports_branch_lineage(scope)) {
    valid_scopes <- c(valid_scopes, bg_branch_lineage(project, scope))
  }

  candidate_scope %in% valid_scopes
}

#' @keywords internal
bg_summary_scope_matches <- function(project, scope, candidate_scope) {
  bg_scope_matches(project, scope, candidate_scope)
}

#' @keywords internal
bg_decision_scope_for_record <- function(project, decision) {
  scope <- decision$scope %||% "project"

  if (startsWith(scope, "node:")) {
    return(bg_resolve_node_scope(project, sub("^node:", "", scope)))
  }

  if (startsWith(scope, "edge:")) {
    graph <- bg_read_graph(project)
    scope_resolution <- bg_node_scope_resolution(project, graph = graph)
    edge_id <- sub("^edge:", "", scope)
    edge <- graph$edges[[edge_id]] %||% NULL
    if (is.null(edge)) {
      return("project")
    }
    from_scope <- bg_resolve_node_scope(
      project,
      edge$from,
      scope_resolution = scope_resolution
    )
    to_scope <- bg_resolve_node_scope(
      project,
      edge$to,
      scope_resolution = scope_resolution
    )
    if (identical(from_scope, to_scope)) {
      return(from_scope)
    }
    return("project")
  }

  if (startsWith(scope, "gate:")) {
    graph <- bg_read_graph(project)
    scope_resolution <- bg_node_scope_resolution(project, graph = graph)
    gate_id <- sub("^gate:", "", scope)
    gate <- graph$gates[[gate_id]] %||% NULL
    if (is.null(gate)) {
      return("project")
    }
    edge <- graph$edges[[gate$edge_id]] %||% NULL
    if (is.null(edge)) {
      return("project")
    }
    from_scope <- bg_resolve_node_scope(
      project,
      edge$from,
      scope_resolution = scope_resolution
    )
    to_scope <- bg_resolve_node_scope(
      project,
      edge$to,
      scope_resolution = scope_resolution
    )
    if (identical(from_scope, to_scope)) {
      return(from_scope)
    }
    return("project")
  }

  scope
}

#' @keywords internal
bg_scope_decisions <- function(project, scope) {
  decisions <- bg_read_decisions(project)
  Filter(
    function(decision) {
      bg_scope_matches(
        project,
        scope,
        bg_decision_scope_for_record(project, decision)
      )
    },
    decisions
  )
}

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
  persisted <- list()

  for (summary in summaries) {
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
      schema_version = 1L,
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

#' Determine whether a summary is fresh
#'
#' @param project A `bg_handle`.
#' @param summary A persisted summary entry.
#' @param predicted_fingerprints Optional named fingerprint map.
#' @param artifact_index Optional normalized artifact index.
#'
#' @return Logical scalar.
#' @export
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

#' Build the workflow context for a given scope
#'
#' @param project A `bg_handle`.
#' @param scope Scope string. Defaults to `project`.
#'
#' @return A workflow context list partitioned into structural, execution, and evidence.
#' @export
bg_build_workflow_context <- function(project, scope = "project") {
  bg_build_workflow_context_impl(project, scope = scope)
}

#' @keywords internal
bg_build_workflow_context_impl <- function(
  project,
  scope = "project",
  plan = NULL
) {
  S7::check_is_S7(project, bg_handle)

  full_graph <- bg_read_graph(project)
  graph <- bg_active_graph(project, graph = full_graph)
  plan <- plan %||% bg_plan(project, mode = "sync")
  artifact_index <- plan$metadata$artifact_index %||% bg_read_artifact_index(
    project
  )
  predicted_fingerprints <- plan$metadata$fingerprints %||% list()
  scope_node_ids <- bg_scope_node_ids(project, scope, graph = full_graph)

  node_status <- stats::setNames(
    vector("list", length(scope_node_ids)),
    scope_node_ids
  )
  available_artifacts <- list()
  missing_artifacts <- character()
  for (node_id in scope_node_ids) {
    fp <- predicted_fingerprints[[node_id]] %||% NULL
    binding <- if (!is.null(fp)) {
      bg_check_artifact(
        project,
        fp,
        node_id = node_id,
        artifact_index = artifact_index
      )
    } else {
      NULL
    }
    node_status[[node_id]] <- list(
      predicted_fingerprint = fp,
      has_active_artifact = !is.null(binding),
      state = graph$nodes[[node_id]]$state
    )
    if (!is.null(binding)) {
      available_artifacts[[node_id]] <- list(
        node_id = node_id,
        artifact_ref = binding,
        execution_fingerprint = fp
      )
    } else if (!is.null(fp)) {
      missing_artifacts <- c(missing_artifacts, node_id)
    }
  }

  summaries <- bg_read_summaries(
    project = project,
    scope = scope,
    include_stale = TRUE,
    include_inactive = FALSE,
    predicted_fingerprints = predicted_fingerprints,
    artifact_index = artifact_index
  )
  decisions <- bg_scope_decisions(project, scope)
  failed_nodes <- unique(vapply(
    Filter(function(job) identical(job$status, "failed"), bg_jobs(project)),
    `[[`,
    character(1),
    "node_id"
  ))
  failed_nodes <- intersect(failed_nodes, scope_node_ids)

  ready_nodes <- intersect(plan$eligible, scope_node_ids)
  blocked_nodes <- intersect(names(plan$blocked), scope_node_ids)
  stale_nodes <- unique(vapply(
    Filter(function(x) isTRUE(x$is_stale), summaries),
    `[[`,
    character(1),
    "node_id"
  ))

  edge_ids <- names(Filter(
    function(edge) edge$to %in% scope_node_ids,
    graph$edges
  ))
  active_packs <- lapply(bg_workflow_packs(project), function(pack) {
    list(
      pack_id = pack$pack_id,
      version = pack$version
    )
  })
  branch_record <- if (startsWith(scope, "branch:")) {
    bg_read_branch_registry(project)$branches[[scope]] %||% NULL
  } else {
    NULL
  }

  list(
    project_id = project@project_id,
    scope = scope,
    active_packs = active_packs,
    inferential_goal = bg_get_goal(project, scope),
    structural = list(
      nodes = graph$nodes[scope_node_ids],
      edges = graph$edges[edge_ids],
      ready_nodes = ready_nodes,
      blocked_nodes = blocked_nodes
    ),
    execution = list(
      node_status = node_status,
      stale_nodes = stale_nodes,
      failed_nodes = failed_nodes
    ),
    evidence = list(
      artifacts = list(
        available = available_artifacts,
        missing = unique(missing_artifacts)
      ),
      summaries = summaries,
      decisions = decisions
    ),
    scope_context = list(
      branch_root = branch_record$root_node_id %||% NULL,
      branch_metadata = branch_record$metadata %||% list(),
      branch_lineage = if (startsWith(scope, "branch:")) {
        bg_branch_lineage(project, scope)
      } else {
        character()
      }
    ),
    metadata = list(
      artifact_index = artifact_index,
      predicted_fingerprints = predicted_fingerprints
    )
  )
}
