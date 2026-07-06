#' Branch a node in the bayesgrove project graph
#'
#' Clones an existing node and its upstream dependencies (edges) to create a
#' new branch. With `continue`, immediate downstream nodes are cloned too, so
#' a branched fit flows into fresh diagnostic or comparison nodes instead of
#' the originals.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to branch.
#' @param label Optional label for the new branched node.
#' @param copy_params Whether to copy the parameters of the branched node (default: TRUE).
#' @param continue Downstream continuation: `TRUE` clones all immediate
#'   children of `node_id` onto the branch (their inputs remapped to the
#'   branch root), a character vector clones only children of those node
#'   kinds (e.g. `c("check", "ppc")`), and the default `character()` (or
#'   `FALSE`) clones nothing. Only immediate children are cloned.
#'
#' @return A `bg_branch_record` list. It always carries a
#'   `$continuation_nodes` entry: a named list (keyed by source node id) of
#'   `list(source_id, clone_id, kind, label)` records for each cloned child,
#'   empty when `continue` requested none.
#' @export
bg_branch <- function(
  project,
  node_id,
  label = NULL,
  copy_params = TRUE,
  continue = character()
) {
  S7::check_is_S7(project, bg_handle)
  continuation <- bg_branch_resolve_continue(continue)

  graph <- bg_read_graph(project)

  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  old_node <- graph$nodes[[node_id]]
  new_id <- bg_new_id("node")

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
    new_edge_id <- bg_new_id("edge")
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

  record$continuation_nodes <- if (continuation$clone) {
    bg_branch_continuation_clones(
      project,
      branch = record,
      node_id = node_id,
      continuation_kinds = continuation$kinds
    )
  } else {
    list()
  }

  record
}

#' Normalize the `continue` argument of [bg_branch].
#'
#' @return `list(clone = <logical>, kinds = NULL | character())`; `kinds =
#'   NULL` means "all immediate children".
#' @keywords internal
#' @noRd
bg_branch_resolve_continue <- function(continue) {
  if (isTRUE(continue)) {
    return(list(clone = TRUE, kinds = NULL))
  }
  if (is.null(continue) || isFALSE(continue)) {
    return(list(clone = FALSE, kinds = NULL))
  }
  if (is.character(continue)) {
    if (length(continue) == 0) {
      return(list(clone = FALSE, kinds = NULL))
    }
    return(list(clone = TRUE, kinds = continue))
  }
  cli::cli_abort(
    "{.arg continue} must be TRUE, FALSE, or a character vector of node kinds."
  )
}

#' Clone immediate downstream nodes of a freshly created branch.
#'
#' The continuation half of [bg_branch]: clones the immediate children of the
#' branched source node, remapping their inputs from the source lineage to
#' the branch root (inputs external to the branch are kept as-is).
#'
#' @param project A `bg_handle`.
#' @param branch The just-registered branch record (root + branch id).
#' @param node_id The source node the branch was created from.
#' @param continuation_kinds Character vector of node kinds to clone, or NULL
#'   to clone all immediate children.
#' @return Named list (by source node id) of continuation records:
#'   `list(source_id, clone_id, kind, label)`.
#' @keywords internal
#' @noRd
bg_branch_continuation_clones <- function(
  project,
  branch,
  node_id,
  continuation_kinds = NULL
) {
  graph <- bg_read_graph(project)
  continuation_nodes <- list()

  # Find direct downstream edges from the source node
  downstream_edges <- bg_dagri_outgoing_edges(graph, node_id)

  if (length(downstream_edges) == 0) {
    return(continuation_nodes)
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
    return(continuation_nodes)
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

    new_target_id <- bg_new_id("node")

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
      new_edge_id <- bg_new_id("edge")
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

  continuation_nodes
}

#' Branch a node with downstream continuation (deprecated)
#'
#' Deprecated: use [bg_branch()] with the `continue` argument instead —
#' `continue = TRUE` clones all immediate children, a character vector clones
#' only children of those kinds. This wrapper delegates to [bg_branch()] and
#' returns the historical `list(branch, continuation_nodes)` shape; it warns
#' once per session and will be removed in a future release.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to branch.
#' @param label Optional label for the new branched node.
#' @param copy_params Whether to copy the parameters of the branched node (default: TRUE).
#' @param continuation_kinds Character vector of node kinds to clone downstream.
#'   If NULL (default), clones all immediate children. If empty, behaves like
#'   `bg_branch()` with no continuation.
#' @param continuation_depth How many levels of downstream nodes to clone.
#'   Only 1 (immediate children) is supported.
#'
#' @return A list containing the `branch` record and `continuation_nodes`
#'   mapping source node IDs to their cloned counterparts.
#' @seealso [bg_branch()]
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

  cli::cli_warn(
    c(
      "!" = "{.fn bg_branch_with_continuation} is deprecated as of bayesgrove 0.7.0.",
      "i" = paste0(
        "Use {.code bg_branch(project, node_id, continue = )} instead: ",
        "{.code continue = TRUE} clones all immediate children, a ",
        "character vector clones matching kinds."
      )
    ),
    .frequency = "once",
    .frequency_id = "bg_branch_with_continuation-deprecated"
  )

  record <- bg_branch(
    project = project,
    node_id = node_id,
    label = label,
    copy_params = copy_params,
    continue = continuation_kinds %||% TRUE
  )

  continuation_nodes <- record$continuation_nodes %||% list()
  record$continuation_nodes <- NULL

  list(
    branch = record,
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

  bg_cli_inform(
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

  bg_cli_inform(
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

  bg_cli_inform(
    "Retired branch {.val {branch_id}} with {length(node_ids)} node{?s}."
  )

  invisible(node_ids)
}
