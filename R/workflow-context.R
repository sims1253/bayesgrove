# Workflow context building: scope resolution, scope matching, decision
# resolution, and the context assembly consumed by the protocol engine.

# --- Scope Resolution ---

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
  branches <- branches %||%
    bg_read_branch_registry(project)$branches %||%
    list()
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
#' @noRd
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
  visited <- node_id

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
        if (!parent %in% visited) {
          visited <- c(visited, parent)
          next_frontier <- c(next_frontier, parent)
        }
      }
    }

    if (length(next_frontier) == 0) {
      return("project")
    }

    frontier <- next_frontier
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

# --- Scope Matching and Decision Resolution ---

#' @keywords internal
bg_predicted_fingerprints <- function(project, include_inactive = TRUE) {
  bg_plan(
    project,
    include_inactive = include_inactive
  )$metadata$fingerprints %||%
    list()
}

#' @keywords internal
bg_scope_matches <- function(project, scope, candidate_scope) {
  if (identical(scope, "project")) {
    return(identical(candidate_scope, "project"))
  }

  valid_scopes <- c("project", scope)
  if (startsWith(scope, "branch:")) {
    valid_scopes <- c(valid_scopes, bg_branch_lineage(project, scope))
  }

  candidate_scope %in% valid_scopes
}

#' @keywords internal
bg_decision_scope_for_record <- function(project, decision) {
  scope <- decision$scope %||% "project"

  if (startsWith(scope, "node:")) {
    return(bg_resolve_node_scope(project, sub("^node:", "", scope)))
  }

  if (startsWith(scope, "edge:") || startsWith(scope, "gate:")) {
    graph <- bg_read_graph(project)
    scope_resolution <- bg_node_scope_resolution(project, graph = graph)
    edge_id <- if (startsWith(scope, "gate:")) {
      gate <- graph$gates[[sub("^gate:", "", scope)]] %||% NULL
      if (is.null(gate)) {
        return("project")
      }
      gate$edge_id
    } else {
      sub("^edge:", "", scope)
    }
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

# --- Workflow Context Building ---

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
  plan = NULL,
  state = NULL,
  graph = NULL
) {
  S7::check_is_S7(project, bg_handle)

  full_graph <- graph %||% bg_read_graph(project)
  graph <- bg_active_graph(project, graph = full_graph)
  plan <- plan %||% bg_plan(project)
  artifact_index <- plan$metadata$artifact_index %||%
    bg_read_artifact_index(
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

  # When a per-run state object is supplied, use its cached
  # summaries/decisions/jobs instead of re-reading from disk on every node.
  summaries <- if (!is.null(state)) {
    state$summaries
  } else {
    bg_read_summaries(
      project = project,
      scope = scope,
      include_stale = TRUE,
      include_inactive = FALSE,
      predicted_fingerprints = predicted_fingerprints,
      artifact_index = artifact_index
    )
  }
  decisions <- if (!is.null(state)) {
    state$decisions
  } else {
    bg_scope_decisions(project, scope)
  }
  failed_nodes <- unique(vapply(
    Filter(
      function(job) identical(job$status, "failed"),
      if (!is.null(state)) state$jobs else bg_jobs(project)
    ),
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
