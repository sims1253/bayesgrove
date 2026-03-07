#' Branch a node in the BayesGrove project graph
#'
#' Clones an existing node and its upstream dependencies (edges) to create a new branch.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to branch.
#' @param label Optional label for the new branched node.
#' @param copy_params Whether to copy the parameters of the branched node (default: TRUE).
#'
#' @return A `bg_branch_record`.
#' @export
bg_branch <- function(project, node_id, label = NULL, copy_params = TRUE) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)

  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  old_node <- graph$nodes[[node_id]]
  new_id <- sprintf("node_%s", digest::digest(runif(1), algo = "xxhash32"))

  new_label <- label %||% paste0(old_node$label %||% old_node$kind, " (branch)")
  new_params <- if (copy_params) old_node$params else list()

  # Create the new node
  graph <- bg_dagri_add_node(
    graph = graph,
    id = new_id,
    kind = old_node$kind,
    label = new_label,
    params = new_params,
    metadata = old_node$metadata
  )

  # Find upstream edges and duplicate them for the new node
  upstream_edges <- bg_dagri_incoming_edges(graph, node_id)

  for (e in upstream_edges) {
    new_edge_id <- sprintf(
      "edge_%s",
      digest::digest(runif(1), algo = "xxhash32")
    )
    graph <- bg_dagri_add_edge(
      graph = graph,
      from = e$from,
      to = new_id,
      type = e$type,
      id = new_edge_id,
      metadata = e$metadata
    )
  }

  bg_commit_graph(project, graph)
  record <- bg_register_branch(
    project = project,
    root_node_id = new_id,
    source_node_id = node_id,
    label = new_label,
    metadata = list(copy_params = isTRUE(copy_params))
  )

  record
}

#' Branch a node with downstream continuation
#'
#' Creates a branch from a node and optionally clones immediate downstream
#' nodes to establish a continuation path. This is a narrowly scoped helper
#' for guided workflows where a branched fit should flow into downstream
#' diagnostic or comparison nodes.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to branch.
#' @param label Optional label for the new branched node.
#' @param copy_params Whether to copy the parameters of the branched node (default: TRUE).
#' @param continuation_kinds Character vector of node kinds to clone downstream.
#'   If NULL (default), clones all immediate children. If empty, behaves like
#'   `bg_branch()` with no continuation.
#' @param continuation_depth How many levels of downstream nodes to clone.
#'   Only 1 (immediate children) is supported in v1.
#'
#' @return A list containing the `branch` record and `continuation_nodes`
#'   mapping source node IDs to their cloned counterparts.
#' @export
bg_branch_with_continuation <- function(
  project,
  node_id,
  label = NULL,
  copy_params = TRUE,
  continuation_kinds = NULL,
  continuation_depth = 1L
) {
  S7::check_is_S7(project, bg_handle)

  if (!identical(continuation_depth, 1L)) {
    cli::cli_abort(
      "Only {.val continuation_depth = 1} is supported in the current version."
    )
  }

  # Create the base branch first
  branch <- bg_branch(
    project = project,
    node_id = node_id,
    label = label,
    copy_params = copy_params
  )

  graph <- bg_read_graph(project)
  continuation_nodes <- list()

  # Find direct downstream edges from the source node
  downstream_edges <- bg_dagri_outgoing_edges(graph, node_id)

  if (length(downstream_edges) == 0) {
    return(list(
      branch = branch,
      continuation_nodes = continuation_nodes
    ))
  }

  # Group edges by target node to handle multiple edges to the same node
  targets <- unique(vapply(downstream_edges, `[[`, character(1), "to"))

  # Filter by continuation_kinds if specified
  if (!is.null(continuation_kinds) && length(continuation_kinds) > 0) {
    targets <- Filter(
      function(target_id) {
        target_node <- graph$nodes[[target_id]]
        !is.null(target_node) && target_node$kind %in% continuation_kinds
      },
      targets
    )
  }

  if (length(targets) == 0) {
    return(list(
      branch = branch,
      continuation_nodes = continuation_nodes
    ))
  }

  # Build a mapping from source node IDs to their cloned counterparts
  node_mapping <- stats::setNames(
    branch$root_node_id,
    node_id
  )

  # Clone each downstream target
  for (target_id in targets) {
    target_node <- graph$nodes[[target_id]]
    if (is.null(target_node)) {
      next
    }

    new_target_id <- sprintf(
      "node_%s",
      digest::digest(runif(1), algo = "xxhash32")
    )

    # Find all edges into this target and remap inputs
    incoming_edges <- bg_dagri_incoming_edges(graph, target_id)

    # Determine new inputs: remap from branch source, keep others as-is
    new_inputs <- character()
    for (e in incoming_edges) {
      if (e$from %in% names(node_mapping)) {
        # This input comes from the branch lineage, remap to clone
        new_inputs <- c(new_inputs, node_mapping[[e$from]])
      } else {
        # This input is external to the branch, keep it
        new_inputs <- c(new_inputs, e$from)
      }
    }

    # Create the cloned downstream node
    new_label <- paste0(
      target_node$label %||% target_node$kind,
      " (from branch)"
    )

    graph <- bg_dagri_add_node(
      graph = graph,
      id = new_target_id,
      kind = target_node$kind,
      label = new_label,
      params = target_node$params %||% list(),
      metadata = utils::modifyList(
        target_node$metadata %||% list(),
        list(
          branched_from = target_id,
          branch_id = branch$branch_id
        )
      )
    )

    # Add edges from the remapped inputs
    for (i in seq_along(new_inputs)) {
      new_edge_id <- sprintf(
        "edge_%s",
        digest::digest(runif(1), algo = "xxhash32")
      )
      graph <- bg_dagri_add_edge(
        graph = graph,
        from = new_inputs[[i]],
        to = new_target_id,
        type = incoming_edges[[i]]$type %||% "default",
        id = new_edge_id,
        metadata = incoming_edges[[i]]$metadata %||% list()
      )
    }

    # Track the mapping for provenance
    node_mapping[[target_id]] <- new_target_id
    continuation_nodes[[target_id]] <- list(
      source_id = target_id,
      clone_id = new_target_id,
      kind = target_node$kind,
      label = new_label
    )
  }

  bg_commit_graph(project, graph)

  list(
    branch = branch,
    continuation_nodes = continuation_nodes
  )
}

#' Invalidate a node's result
#'
#' Invalidation is an explicit escape hatch for intentional recomputation.
#' It does not alter the structural graph state.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to invalidate.
#' @param recursive Whether to recursively invalidate downstream nodes (default: TRUE).
#'
#' @export
bg_invalidate <- function(project, node_id, recursive = TRUE) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)
  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  nodes_to_invalidate <- c(node_id)

  if (recursive) {
    descendants <- bg_dagri_descendants(graph, node_id)
    nodes_to_invalidate <- unique(c(nodes_to_invalidate, descendants))
  } else {
    cli::cli_warn(
      paste0(
        "Non-recursive invalidation leaves downstream cache entries untouched. ",
        "Use {.code recursive = TRUE} for the consistency-preserving path."
      )
    )
  }

  changed <- FALSE
  superseded_count <- 0L

  bg_modify_artifact_index(project, {
    index <- bg_read_artifact_index(project)

    for (target_id in nodes_to_invalidate) {
      updated <- bg_supersede_artifacts_for_node(index, target_id)
      index <- updated$index
      changed <- changed || updated$changed
      superseded_count <- superseded_count + updated$count
    }

    if (changed) {
      bg_write_artifact_index(project, index)
    }
  })

  cli::cli_inform(
    "Invalidated {length(nodes_to_invalidate)} node{?s}; superseded {superseded_count} cache binding{?s}."
  )

  invisible(TRUE)
}

#' @keywords internal
bg_set_node_lifecycle <- function(
  project,
  node_ids,
  lifecycle = c("active", "retired", "disabled"),
  reason = NULL
) {
  lifecycle <- match.arg(lifecycle)
  node_ids <- sort(unique(as.character(node_ids)))
  if (length(node_ids) == 0) {
    return(invisible(character()))
  }

  graph <- bg_read_graph(project)
  missing <- setdiff(node_ids, names(graph$nodes %||% list()))
  if (length(missing) > 0) {
    cli::cli_abort("Node{?s} not found in graph: {.val {missing}}")
  }

  for (node_id in node_ids) {
    node <- graph$nodes[[node_id]]
    metadata <- node$metadata %||% list()
    metadata$lifecycle <- lifecycle
    metadata$lifecycle_updated_at <- bg_now_timestamp()
    if (!is.null(reason) && nzchar(trimws(reason))) {
      metadata$lifecycle_reason <- trimws(reason)
    }
    graph$nodes[[node_id]]$metadata <- metadata
  }
  graph$version <- (graph$version %||% 0L) + 1L

  bg_commit_graph(project, graph)
  invisible(node_ids)
}

#' @keywords internal
bg_set_branch_lifecycle <- function(
  project,
  branch_id,
  lifecycle = c("active", "retired", "disabled"),
  reason = NULL
) {
  lifecycle <- match.arg(lifecycle)

  metadata <- list(
    lifecycle = lifecycle,
    lifecycle_updated_at = bg_now_timestamp()
  )
  if (!is.null(reason) && nzchar(trimws(reason))) {
    metadata$lifecycle_reason <- trimws(reason)
  }

  bg_update_branch_metadata(project, branch_id, metadata)
}

#' Retire a node and downstream lineage
#'
#' Retirement removes a node path from future planning and workflow guidance
#' while preserving its structural provenance and cached artifacts.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to retire.
#' @param recursive Whether to recursively retire downstream nodes (default: TRUE).
#' @param reason Optional rationale stored in node metadata.
#'
#' @return Invisibly returns the retired node ids.
#' @export
bg_retire_node <- function(project, node_id, recursive = TRUE, reason = NULL) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)
  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  node_ids <- c(node_id)
  if (isTRUE(recursive)) {
    node_ids <- unique(c(
      node_ids,
      bg_dagri_descendants(graph, node_id)
    ))
  }

  bg_set_node_lifecycle(
    project = project,
    node_ids = node_ids,
    lifecycle = "retired",
    reason = reason
  )

  branches <- bg_read_branch_registry(project)$branches %||% list()
  for (branch_id in names(branches)) {
    if (identical(branches[[branch_id]]$root_node_id %||% NULL, node_id)) {
      bg_set_branch_lifecycle(
        project = project,
        branch_id = branch_id,
        lifecycle = "retired",
        reason = reason
      )
    }
  }

  cli::cli_inform(
    "Retired {length(node_ids)} node{?s} from future planning."
  )

  invisible(node_ids)
}

#' Retire an entire branch
#'
#' Retirement removes the branch from future planning and workflow guidance
#' while preserving its provenance and cached artifacts.
#'
#' @param project A `bg_handle`.
#' @param branch_id The branch id to retire.
#' @param reason Optional rationale stored in branch and node metadata.
#'
#' @return Invisibly returns the retired branch node ids.
#' @export
bg_retire_branch <- function(project, branch_id, reason = NULL) {
  S7::check_is_S7(project, bg_handle)

  branches <- bg_read_branch_registry(project)$branches %||% list()
  branch <- branches[[branch_id]] %||% NULL
  if (is.null(branch)) {
    cli::cli_abort("Branch {.val {branch_id}} not found in branch registry.")
  }

  node_ids <- bg_scope_node_ids(project, branch_id, include_inactive = TRUE)
  bg_set_node_lifecycle(
    project = project,
    node_ids = node_ids,
    lifecycle = "retired",
    reason = reason
  )
  bg_set_branch_lifecycle(
    project = project,
    branch_id = branch_id,
    lifecycle = "retired",
    reason = reason
  )

  cli::cli_inform(
    "Retired branch {.val {branch_id}} with {length(node_ids)} node{?s}."
  )

  invisible(node_ids)
}

#' Compute parameter suggestions from a modification hint
#'
#' Given a modification hint (e.g., "reparametrize", "adjust_tolerances") and
#' current node parameters, returns a list of suggested parameter changes.
#'
#' @param hint Modification hint string.
#' @param current_params Current node parameters as a named list.
#'
#' @return Named list of suggested parameter changes.
#' @noRd
bg_parameter_suggestions_from_hint <- function(hint, current_params = list()) {
  if (is.null(hint) || !nzchar(hint)) {
    return(list())
  }

  suggestions <- switch(
    hint,
    "reparametrize" = {
      if ("parametrization" %in% names(current_params)) {
        current <- current_params$parametrization
        if (identical(current, "centered")) {
          list(parametrization = "non-centered")
        } else if (identical(current, "non-centered")) {
          list(parametrization = "centered")
        } else {
          list(parametrization = "non-centered")
        }
      } else {
        list()
      }
    },
    "adjust_tolerances" = {
      if ("tolerance" %in% names(current_params)) {
        current_tol <- current_params$tolerance
        if (is.numeric(current_tol) && current_tol < 1e-4) {
          list(tolerance = 1e-4)
        } else {
          list(tolerance = 1e-3)
        }
      } else if ("adapt_delta" %in% names(current_params)) {
        current_delta <- current_params$adapt_delta
        if (is.numeric(current_delta) && current_delta < 0.95) {
          list(adapt_delta = 0.95)
        } else {
          list(adapt_delta = 0.99)
        }
      } else {
        list()
      }
    },
    "increase_iterations" = {
      if ("iter" %in% names(current_params)) {
        current_iter <- current_params$iter
        if (is.numeric(current_iter)) {
          list(iter = as.integer(current_iter * 2))
        } else {
          list()
        }
      } else if ("iterations" %in% names(current_params)) {
        current_iter <- current_params$iterations
        if (is.numeric(current_iter)) {
          list(iterations = as.integer(current_iter * 2))
        } else {
          list()
        }
      } else {
        list()
      }
    },
    list()
  )

  suggestions
}
